package jd.controlling.downloadcontroller;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import jd.config.Property;
import jd.controlling.AccountController;
import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.downloadcontroller.AccountCache.CachedAccount;
import jd.controlling.downloadcontroller.event.DownloadWatchdogEvent;
import jd.controlling.packagecontroller.PackageControllerQueue.ReadOnlyQueueAction;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountTrafficView;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.PluginForHost;
import jd.plugins.download.raf.FileBytesCache;
import jd.utils.JDUtilities;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.Queue.QueuePriority;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.controlling.hosterrule.AccountGroup.Rules;
import org.jdownloader.controlling.hosterrule.CachedAccountGroup;
import org.jdownloader.controlling.hosterrule.HosterRuleController;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.controller.PluginClassLoader;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.IfFileExistsAction;
import org.jdownloader.settings.IfFilenameTooLongAction;

public class DownloadSession extends Property {
    public static enum STOPMARK {
        /* no stopmark is set */
        NONE,
        /*
         * stopmark is set but no longer visible, eg link/package removed from list
         */
        HIDDEN,
        /* to set a random stopmark */
        RANDOM;
    }

    public static enum SessionState {
        NORMAL,
        RECONNECT_REQUESTED,
        RECONNECT_RUNNING;
    }

    /* non shared between DownloadSessions */
    private static final FileAccessManager FILE_ACCESS_MANAGER = new FileAccessManager();
    private static final DiskSpaceManager  DISK_SPACE_MANAGER  = new DiskSpaceManager();

    public DiskSpaceManager getDiskSpaceManager() {
        return DISK_SPACE_MANAGER;
    }

    private final NullsafeAtomicReference<SessionState>                   sessionState               = new NullsafeAtomicReference<SessionState>(SessionState.NORMAL);
    private final HashMap<String, AccountCache>                           accountCache               = new HashMap<String, AccountCache>();
    private final WeakHashMap<DownloadLink, DownloadLinkCandidateHistory> candidateHistory           = new WeakHashMap<DownloadLink, DownloadLinkCandidateHistory>();
    private final WeakHashMap<UniqueAlltimeID, IfFileExistsAction>        fileExistsActions          = new WeakHashMap<UniqueAlltimeID, IfFileExistsAction>();
    private final WeakHashMap<UniqueAlltimeID, IfFilenameTooLongAction>   fileFilenameTooLongActions = new WeakHashMap<UniqueAlltimeID, IfFilenameTooLongAction>();
    private final AtomicInteger                                           downloadsStarted           = new AtomicInteger(0);
    private final AtomicInteger                                           skipCounter                = new AtomicInteger(0);
    private final NullsafeAtomicReference<Integer>                        speedLimitBeforePause      = new NullsafeAtomicReference<Integer>(null);
    private final NullsafeAtomicReference<Boolean>                        speedLimitedBeforePause    = new NullsafeAtomicReference<Boolean>(null);
    private volatile List<DownloadLink>                                   forcedLinks                = new CopyOnWriteArrayList<DownloadLink>();
    private volatile List<DownloadLink>                                   activationRequests         = new CopyOnWriteArrayList<DownloadLink>();
    private final WeakHashMap<PluginForHost, PluginClassLoaderChild>      activationPluginCache      = new WeakHashMap<PluginForHost, PluginClassLoaderChild>();
    private final AtomicBoolean                                           refreshCandidates          = new AtomicBoolean(false);
    private final AtomicBoolean                                           activateForcedOnly         = new AtomicBoolean(false);
    private AtomicLong                                                    activatorRebuildRequest    = new AtomicLong(1);
    private NullsafeAtomicReference<CaptchaSettings.MODE>                 captchaMode                = new NullsafeAtomicReference<CaptchaSettings.MODE>(CaptchaSettings.MODE.NORMAL);
    private final static Comparator<CachedAccount>                        EXPIRE_DATE_SORTER         = new Comparator<CachedAccount>() {
        public final int compare(long x, long y) {
            return (x < y) ? -1 : ((x == y) ? 0 : 1);
        }

        public final long getExpireDate(Account account) {
            if (account == null) {
                return Long.MAX_VALUE;
            } else {
                final long validUntil = account.getValidPremiumUntil();
                if (validUntil <= 0) {
                    return Long.MAX_VALUE;
                } else {
                    return validUntil;
                }
            }
        }

        @Override
        public int compare(CachedAccount account1, CachedAccount account2) {
            final long expireDate1 = getExpireDate(account1.getAccount());
            final long expireDate2 = getExpireDate(account2.getAccount());
            return compare(expireDate1, expireDate2);
        }
    };
    private final static Comparator<CachedAccount>                        TRAFFIC_LEFT_SORTER        = new Comparator<CachedAccount>() {
        public final int compare(long x, long y) {
            return (x < y) ? -1 : ((x == y) ? 0 : 1);
        }

        public final long getTrafficLeft(Account account) {
            if (account == null) {
                return Long.MAX_VALUE;
            } else {
                final AccountTrafficView accountTrafficView = account.getAccountTrafficView();
                if (accountTrafficView == null || accountTrafficView.isUnlimitedTraffic()) {
                    return Long.MAX_VALUE;
                } else {
                    return accountTrafficView.getTrafficLeft();
                }
            }
        }

        @Override
        public int compare(CachedAccount account1, CachedAccount account2) {
            final long trafficLeft1 = getTrafficLeft(account1.getAccount());
            final long trafficLeft2 = getTrafficLeft(account2.getAccount());
            return compare(trafficLeft1, trafficLeft2);
        }
    };
    private final static Comparator<CachedAccount>                        ACCOUNT_SORTER             = new Comparator<CachedAccount>() {
        @Override
        public int compare(CachedAccount o1, CachedAccount o2) {
            final int ret = EXPIRE_DATE_SORTER.compare(o1, o2);
            if (ret == 0) {
                return TRAFFIC_LEFT_SORTER.compare(o1, o2);
            } else {
                return ret;
            }
        }
    };

    public CaptchaSettings.MODE getCaptchaMode() {
        return captchaMode.get();
    }

    protected void setCaptchaMode(CaptchaSettings.MODE captchaMode) {
        if (captchaMode == null) {
            captchaMode = CaptchaSettings.MODE.NORMAL;
        }
        this.captchaMode.set(captchaMode);
    }

    public long getActivatorRebuildRequest() {
        return activatorRebuildRequest.get();
    }

    protected void incrementActivatorRebuildRequest() {
        activatorRebuildRequest.incrementAndGet();
    }

    /* shared between DownloadSessions */
    private final ProxyInfoHistory                proxyInfoHistory;
    private final AtomicInteger                   maxConcurrentDownloadsPerHost = new AtomicInteger(Integer.MAX_VALUE);
    private final NullsafeAtomicReference<Object> stopMark                      = new NullsafeAtomicReference<Object>(STOPMARK.NONE);
    private final AtomicBoolean                   useAccounts                   = new AtomicBoolean(true);
    private final AtomicBoolean                   mirrorManagement              = new AtomicBoolean(true);

    public boolean isCandidatesRefreshRequired() {
        return refreshCandidates.get();
    }

    public boolean isUseAccountsEnabled() {
        return useAccounts.get();
    }

    protected void setUseAccountsEnabled(boolean b) {
        useAccounts.set(b);
    }

    public boolean isMirrorManagementEnabled() {
        return mirrorManagement.get();
    }

    protected void setMirrorManagementEnabled(boolean b) {
        mirrorManagement.set(b);
    }

    public int getMaxConcurrentDownloadsPerHost() {
        return maxConcurrentDownloadsPerHost.get();
    }

    protected void setMaxConcurrentDownloadsPerHost(int max) {
        if (max <= 0) {
            maxConcurrentDownloadsPerHost.set(Integer.MAX_VALUE);
        } else {
            maxConcurrentDownloadsPerHost.set(max);
        }
    }

    public boolean isForcedOnlyModeEnabled() {
        return activateForcedOnly.get();
    }

    public int getSkipCounter() {
        return skipCounter.get();
    }

    public int getSpeedLimitBeforePause() {
        Integer ret = speedLimitBeforePause.get();
        if (ret == null) {
            return -1;
        }
        return Math.max(-1, ret);
    }

    public Boolean isSpeedWasLimitedBeforePauseEnabled() {
        return speedLimitedBeforePause.get();
    }

    protected void setActivationRequests(List<DownloadLink> activationRequests) {
        if (isCandidatesRefreshRequired() == false) {
            if (!activationRequests.equals(this.activationRequests)) {
                refreshCandidates();
            }
        }
        this.activationRequests = activationRequests;
    }

    public void refreshCandidates() {
        refreshCandidates.set(true);
    }

    protected void setForcedLinks(CopyOnWriteArrayList<DownloadLink> forcedLinks) {
        if (isCandidatesRefreshRequired() == false) {
            if (!forcedLinks.equals(this.forcedLinks)) {
                refreshCandidates();
            }
        }
        this.forcedLinks = forcedLinks;
    }

    protected boolean removeActivationRequest(DownloadLink link) {
        if (link == null) {
            return false;
        }
        boolean removed = activationRequests.remove(link);
        removed = forcedLinks.remove(link) || removed;
        if (removed) {
            refreshCandidates();
        }
        return removed;
    }

    protected boolean removeActivationRequests(List<DownloadLink> links) {
        if (links == null) {
            return false;
        }
        boolean removed = activationRequests.remove(links);
        removed = forcedLinks.remove(links) || removed;
        if (removed) {
            refreshCandidates();
        }
        return removed;
    }

    private Map<PluginForHost, PluginClassLoaderChild> getActivationPluginCache() {
        return activationPluginCache;
    }

    private final CopyOnWriteArraySet<SingleDownloadController> controllers        = new CopyOnWriteArraySet<SingleDownloadController>() {
        /**
         *
         */
         private static final long serialVersionUID = -3897088297641777499L;

        public boolean add(SingleDownloadController e) {
            downloadsStarted.incrementAndGet();
            e.getDownloadLinkCandidate().getLink().setDownloadLinkController(e);
            return super.add(e);
        };

        @Override
        public boolean remove(Object e) {
            final boolean ret = super.remove(e);
            if (ret) {
                try {
                    getDiskSpaceManager().freeAllReservationsBy(e);
                } catch (final Throwable ignore) {
                }
                try {
                    getFileAccessManager().unlockAllHeldby(e);
                } finally {
                    if (e instanceof SingleDownloadController) {
                        ((SingleDownloadController) e).getDownloadLinkCandidate().getLink().setDownloadLinkController(null);
                    }
                }
            }
            return ret;
        };
    };
    private final long                                          createTime;
    private static volatile WeakReference<FileBytesCache>       downloadWriteCache = null;

    public static synchronized FileBytesCache getDownloadWriteCache() {
        FileBytesCache ret = null;
        if (downloadWriteCache != null && (ret = downloadWriteCache.get()) != null) {
            return ret;
        }
        ret = new FileBytesCache(JsonConfig.create(GeneralSettings.class).getMaxBufferSize() * 1024, JsonConfig.create(GeneralSettings.class).getFlushBufferTimeout());
        downloadWriteCache = new WeakReference<FileBytesCache>(ret);
        return ret;
    }

    public ProxyInfoHistory getProxyInfoHistory() {
        return proxyInfoHistory;
    }

    public synchronized PluginForHost getPlugin(String host) {
        if (StringUtils.isEmpty(host)) {
            return null;
        }
        final Iterator<Entry<PluginForHost, PluginClassLoaderChild>> it = getActivationPluginCache().entrySet().iterator();
        while (it.hasNext()) {
            final Entry<PluginForHost, PluginClassLoaderChild> next = it.next();
            final PluginForHost plugin = next.getKey();
            if (plugin != null && StringUtils.equalsIgnoreCase(host, plugin.getHost())) {
                return plugin;
            }
        }
        PluginForHost plugin = JDUtilities.getPluginForHost(host);
        if (plugin == null) {
            final LazyHostPlugin fallBackPlugin = HostPluginController.getInstance().getFallBackPlugin();
            if (fallBackPlugin != null) {
                try {
                    plugin = fallBackPlugin.getPrototype(PluginClassLoader.getThreadPluginClassLoaderChild());
                } catch (Throwable e) {
                    LogController.CL().log(e);
                }
            }
        }
        getActivationPluginCache().put(plugin, PluginClassLoader.getSharedChild(plugin));
        return plugin;
    }

    public synchronized PluginClassLoaderChild getPluginClassLoaderChild(final PluginForHost plugin) {
        if (plugin == null) {
            return null;
        }
        final Iterator<Entry<PluginForHost, PluginClassLoaderChild>> it = getActivationPluginCache().entrySet().iterator();
        while (it.hasNext()) {
            final Entry<PluginForHost, PluginClassLoaderChild> next = it.next();
            final PluginForHost entry = next.getKey();
            if (entry == plugin) {
                return next.getValue();
            }
        }
        return null;
    }

    public List<DownloadLink> getForcedLinks() {
        return forcedLinks;
    }

    public void toggleStopMark(Object entry) {
        final Object stopMark = getStopMark();
        if (entry == STOPMARK.RANDOM) {
            if (stopMark != STOPMARK.NONE) {
                entry = null;
            }
        }
        if (entry == null || stopMark == entry || entry == STOPMARK.NONE) {
            /* no stopmark OR toggle current set stopmark */
            setStopMark(STOPMARK.NONE);
        } else {
            /* set new stopmark */
            setStopMark(entry);
        }
    }

    public boolean isForcedLinksWaiting() {
        return forcedLinks.size() > 0;
    }

    public boolean isActivationRequestsWaiting() {
        if (activateForcedOnly.get()) {
            return forcedLinks.size() > 0;
        } else {
            return forcedLinks.size() > 0 || activationRequests.size() > 0;
        }
    }

    public DownloadSession() {
        this(null);
    }

    protected DownloadSession(DownloadSession previousSession) {
        createTime = System.currentTimeMillis();
        if (previousSession == null) {
            proxyInfoHistory = new ProxyInfoHistory();
        } else {
            if (previousSession.getControllers().size() > 0) {
                throw new IllegalArgumentException("previousSession contains active controllers!");
            }
            proxyInfoHistory = previousSession.getProxyInfoHistory();
            if (previousSession.isStopMarkSet() && previousSession.isStopMarkReached() == false) {
                setStopMark(previousSession.getStopMark());
            }
            setCaptchaMode(previousSession.getCaptchaMode());
            setMaxConcurrentDownloadsPerHost(previousSession.getMaxConcurrentDownloadsPerHost());
            setUseAccountsEnabled(previousSession.isUseAccountsEnabled());
            setMirrorManagementEnabled(previousSession.isMirrorManagementEnabled());
        }
    }

    public long getCreateTime() {
        return createTime;
    }

    protected DownloadLinkCandidateHistory getHistory(DownloadLink downloadLink) {
        return candidateHistory.get(downloadLink);
    }

    protected Collection<DownloadLinkCandidateHistory> getHistories() {
        return candidateHistory.values();
    }

    public DownloadLinkCandidateHistory buildHistory(DownloadLink downloadLink) {
        DownloadLinkCandidateHistory ret = candidateHistory.get(downloadLink);
        if (ret == null) {
            ret = new DownloadLinkCandidateHistory();
            candidateHistory.put(downloadLink, ret);
        }
        return ret;
    }

    protected DownloadLinkCandidateHistory removeHistory(DownloadLink downloadLink) {
        if (downloadLink == null) {
            candidateHistory.clear();
            return null;
        } else {
            return candidateHistory.remove(downloadLink);
        }
    }

    protected void removeAccountCache(String host) {
        refreshCandidates.set(true);
        synchronized (accountCache) {
            if (StringUtils.isEmpty(host)) {
                accountCache.clear();
            } else {
                accountCache.remove(host.toLowerCase(Locale.ENGLISH));
            }
        }
    }

    public AccountCache getAccountCache(final DownloadLink link) {
        final String host = link.getHost();
        if (StringUtils.isEmpty(host)) {
            return AccountCache.NA;
        }
        AccountCache ret = null;
        synchronized (accountCache) {
            if (accountCache.containsKey(host)) {
                ret = accountCache.get(host);
                if (ret == null) {
                    return AccountCache.NA;
                } else {
                    return ret;
                }
            }
        }
        final PluginForHost defaulPlugin = getPlugin(host);
        final List<CachedAccountGroup> cachedGroups = new ArrayList<CachedAccountGroup>();
        if (isUseAccountsEnabled() == false) {
            /* Accounts disabled -> free only */
            final CachedAccountGroup freeGroup = new CachedAccountGroup(Rules.ORDER);
            freeGroup.add(new CachedAccount(host, null, defaulPlugin));
            cachedGroups.add(freeGroup);
            ret = new AccountCache(cachedGroups);
        } else {
            ret = HosterRuleController.getInstance().getAccountCache(host, this);
            if (ret == null) {
                /* No usage rules available: Use premium account -> Free Account -> Non account */
                boolean removeNoAccountWithCaptcha = false;
                int numberofPremiumAccounts = 0;
                final CachedAccountGroup hosterPremiumGroup = new CachedAccountGroup(Rules.ORDER);
                final CachedAccountGroup hosterFreeGroup = new CachedAccountGroup(Rules.RANDOM);
                for (final Account acc : AccountController.getInstance().list(host)) {
                    if (!acc.isEnabled()) {
                        /* Ignore disabled accounts */
                        continue;
                    } else if (!acc.isValid()) {
                        /* Ignore invalid/expired accounts */
                        continue;
                    }
                    final AccountInfo ai = acc.getAccountInfo();
                    if (ai == null || ai.isSpecialTraffic() || ai.isUnlimitedTraffic() || ai.isTrafficRefill()) {
                        removeNoAccountWithCaptcha = true;
                    }
                    final CachedAccount cachedAccount = new CachedAccount(host, acc, defaulPlugin);
                    switch (acc.getType()) {
                    case LIFETIME:
                    case PREMIUM:
                        hosterPremiumGroup.add(cachedAccount);
                        break;
                    default:
                        hosterFreeGroup.add(cachedAccount);
                        break;
                    }
                }
                if (hosterPremiumGroup.size() > 0) {
                    numberofPremiumAccounts += hosterPremiumGroup.size();
                    try {
                        Collections.sort(hosterPremiumGroup, ACCOUNT_SORTER);
                    } catch (final Throwable e) {
                        LogController.CL().log(e);
                    }
                    cachedGroups.add(hosterPremiumGroup);
                }
                {
                    /* multihoster */
                    final List<Account> multiHosts = AccountController.getInstance().getMultiHostAccounts(host);
                    if (multiHosts != null) {
                        final CachedAccountGroup multiHosterGroup = new CachedAccountGroup(Rules.ORDER);
                        for (final Account acc : multiHosts) {
                            if (!acc.isEnabled()) {
                                /* Ignore disabled accounts */
                                continue;
                            } else if (!acc.isValid()) {
                                /* Ignore invalid/expired accounts */
                                continue;
                            }
                            multiHosterGroup.add(new CachedAccount(host, acc, getPlugin(acc.getHoster())));
                        }
                        if (multiHosterGroup.size() > 0) {
                            numberofPremiumAccounts += multiHosterGroup.size();
                            try {
                                Collections.sort(multiHosterGroup, ACCOUNT_SORTER);
                            } catch (final Throwable e) {
                                LogController.CL().log(e);
                            }
                            cachedGroups.add(multiHosterGroup);
                        }
                    }
                }
                if (hosterFreeGroup.size() > 0) {
                    cachedGroups.add(hosterFreeGroup);
                }
                /* free(no account) */
                CachedAccount noAccount = new CachedAccount(host, null, defaulPlugin);
                final boolean addNoAccountGroup;
                if (numberofPremiumAccounts > 0) {
                    /* At least one premium account is available -> Do not allow non-account download. */
                    addNoAccountGroup = false;
                } else if (removeNoAccountWithCaptcha && noAccount.hasCaptcha(link)) {
                    /* Do not allow non-account download with captcha */
                    addNoAccountGroup = false;
                } else {
                    /* Allow download without account */
                    addNoAccountGroup = true;
                }
                if (!addNoAccountGroup) {
                    noAccount = new CachedAccount(host, null, defaulPlugin) {
                        final List<CachedAccountGroup> finalCachedGroups = new ArrayList<CachedAccountGroup>(cachedGroups);

                        @Override
                        public boolean canHandle(DownloadLink link) throws Exception {
                            try {
                                for (final CachedAccountGroup cachedGroup : finalCachedGroups) {
                                    for (final CachedAccount cachedAccount : cachedGroup) {
                                        if (cachedAccount.canHandle(link)) {
                                            return false;
                                        }
                                    }
                                }
                            } catch (final Exception ignore) {
                                return false;
                            }
                            return super.canHandle(link);
                        }
                    };
                }
                final CachedAccountGroup freeGroup = new CachedAccountGroup(Rules.ORDER);
                freeGroup.add(noAccount);
                cachedGroups.add(freeGroup);
                ret = new AccountCache(cachedGroups);
            }
        }
        synchronized (accountCache) {
            if (!accountCache.containsKey(host)) {
                accountCache.put(host, ret);
                return ret;
            } else {
                ret = accountCache.get(host);
                if (ret == null) {
                    return AccountCache.NA;
                } else {
                    return ret;
                }
            }
        }
    }

    public boolean isStopMark(final Object item) {
        final Object stopMark = this.stopMark.get();
        if (stopMark instanceof STOPMARK) {
            return false;
        } else if (stopMark == item) {
            return true;
        } else if (stopMark instanceof DownloadLink) {
            return item == ((DownloadLink) stopMark).getFilePackage();
        }
        return false;
    }

    public boolean isStopMarkSet() {
        return stopMark.get() != STOPMARK.NONE;
    }

    public boolean isStopMarkReached(final DownloadLink link, final boolean checkRemoved) {
        if (!link.isEnabled() || link.isSkipped() || link.getFinalLinkState() != null) {
            return true;
        }
        final DownloadLinkCandidateHistory history = getHistory(link);
        if (history != null) {
            final List<DownloadLinkCandidateResult> results = DownloadLinkCandidateHistory.selectResults(history, null);
            if (results.size() > 0) {
                final DownloadLinkCandidateResult last = results.get(results.size() - 1);
                if (DownloadLinkCandidateResult.RESULT.IP_BLOCKED.equals(last.getResult()) && (System.currentTimeMillis() - last.getTimeStamp()) < 60 * 1000l) {
                    return false;
                }
            }
            return true;
        }
        if (checkRemoved) {
            return Boolean.TRUE.equals(DownloadController.getInstance().getQueue().addWait(new ReadOnlyQueueAction<Boolean, RuntimeException>(QueuePriority.HIGH) {
                @Override
                protected Boolean run() throws RuntimeException {
                    final FilePackage fp = link.getParentNode();
                    return FilePackage.isDefaultFilePackage(fp) || fp == null || DownloadController.getInstance() != fp.getControlledBy();
                }
            }));
        } else {
            return false;
        }
    }

    protected boolean isStopMarkReached() {
        final Object stop = stopMark.get();
        if (stop == STOPMARK.NONE) {
            return false;
        } else if (stop == STOPMARK.HIDDEN) {
            return true;
        } else if (stop instanceof DownloadLink) {
            final DownloadLink link = (DownloadLink) stop;
            return isStopMarkReached(link, true);
        } else if (stop instanceof FilePackage) {
            final FilePackage fp = (FilePackage) stop;
            final Boolean fpResult = DownloadController.getInstance().getQueue().addWait(new ReadOnlyQueueAction<Boolean, RuntimeException>(QueuePriority.HIGH) {
                @Override
                protected Boolean run() throws RuntimeException {
                    return FilePackage.isDefaultFilePackage(fp) || fp == null || DownloadController.getInstance() != fp.getControlledBy();
                }
            });
            if (Boolean.FALSE.equals(fpResult)) {
                final boolean readL = fp.getModifyLock().readLock();
                try {
                    for (final DownloadLink link : fp.getChildren()) {
                        if (isStopMarkReached(link, false)) {
                            continue;
                        } else {
                            return false;
                        }
                    }
                } finally {
                    fp.getModifyLock().readUnlock(readL);
                }
            }
            return true;
        }
        return false;
    }

    protected void setStopMark(final Object stopEntry) {
        Object entry = stopEntry;
        if (entry == null || entry == STOPMARK.NONE) {
            entry = STOPMARK.NONE;
        }
        if (entry == STOPMARK.RANDOM) {
            /* user wants to set a random stopmark */
            while (true) {
                try {
                    final Iterator<SingleDownloadController> it = controllers.iterator();
                    if (it.hasNext()) {
                        entry = it.next().getDownloadLink();
                    } else {
                        entry = STOPMARK.HIDDEN;
                    }
                    break;
                } catch (final Throwable e) {
                }
            }
        }
        stopMark.set(entry);
        DownloadWatchDog.getInstance().getEventSender().fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.PROPERTY_CHANGE, new DownloadWatchDogProperty(DownloadWatchDogProperty.Property.STOPSIGN, entry)));
    }

    public IfFileExistsAction getOnFileExistsAction(FilePackage filePackage) {
        return fileExistsActions.get(filePackage.getUniqueID());
    }

    public void setOnFileExistsAction(FilePackage filePackage, IfFileExistsAction action) {
        if (filePackage == null) {
            fileExistsActions.clear();
            return;
        }
        if (action == null) {
            fileExistsActions.remove(filePackage.getUniqueID());
        } else {
            fileExistsActions.put(filePackage.getUniqueID(), action);
        }
    }

    public IfFilenameTooLongAction getOnFilenameTooLongAction(final FilePackage filePackage) {
        return fileFilenameTooLongActions.get(filePackage.getUniqueID());
    }

    public void setOnFileFilenameTooLongAction(FilePackage filePackage, IfFilenameTooLongAction action) {
        if (filePackage == null) {
            fileFilenameTooLongActions.clear();
            return;
        }
        if (action == null) {
            fileFilenameTooLongActions.remove(filePackage.getUniqueID());
        } else {
            fileFilenameTooLongActions.put(filePackage.getUniqueID(), action);
        }
    }

    public FileAccessManager getFileAccessManager() {
        return FILE_ACCESS_MANAGER;
    }

    public Object getStopMark() {
        return stopMark.get();
    }

    /**
     * @return the downloadsStarted
     */
    public int getDownloadsStarted() {
        return downloadsStarted.get();
    }

    /**
     * @return the controllers
     */
    protected Set<SingleDownloadController> getControllers() {
        return controllers;
    }

    /**
     * @return the activationLinks
     */
    protected List<DownloadLink> getActivationRequests() {
        return activationRequests;
    }

    /**
     * @return the sessionState
     */
    public SessionState getSessionState() {
        return sessionState.get();
    }

    public void setForcedOnlyModeEnabled(boolean b) {
        activateForcedOnly.set(b);
    }

    public boolean setCandidatesRefreshRequired(boolean b) {
        return refreshCandidates.getAndSet(b);
    }

    public void setSpeedLimitBeforePause(int downloadSpeedLimit) {
        speedLimitBeforePause.set(downloadSpeedLimit);
    }

    public void setSpeedWasLimitedBeforePauseEnabled(boolean b) {
        speedLimitedBeforePause.set(b);
    }

    public boolean compareAndSetSessionState(SessionState expect, SessionState update) {
        return sessionState.compareAndSet(expect, update);
    }

    protected void setSessionState(SessionState state) {
        sessionState.set(state);
    }

    protected void setSkipCounter(int i) {
        skipCounter.set(i);
    }

    public synchronized void clearPluginCache() {
        getActivationPluginCache().clear();
    }
}
