package org.jdownloader.gui.sponsor;

import java.awt.Cursor;
import java.awt.Dialog.ModalityType;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.txtresource.TranslationFactory;
import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.event.queue.Queue;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.images.Interpolation;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.download.DownloadControllerListener;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.myjdownloader.client.json.AvailableLinkState;
import org.jdownloader.premium.OpenURLAction;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.SecondLevelLaunch;
import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.controlling.AccountFilter;
import jd.controlling.AccountUpOrDowngradeEvent;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadLinkCandidate;
import jd.controlling.downloadcontroller.DownloadLinkCandidateResult;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogProperty;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.downloadcontroller.event.DownloadWatchdogListener;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollectorCrawler;
import jd.controlling.linkcollector.LinkCollectorEvent;
import jd.controlling.linkcollector.LinkCollectorListener;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractNodeVisitor;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.DownloadLinkProperty;
import jd.plugins.FilePackage;
import jd.plugins.FilePackageProperty;
import jd.plugins.PluginForHost;

public class BannerRotation implements Sponsor, AccountControllerListener {
    private final List<AvailableBanner> allBanners = new CopyOnWriteArrayList<AvailableBanner>();
    private final Queue                 queue      = new Queue("Banner") {
                                                       public void killQueue() {
                                                       };
                                                   };

    private class AvailableBanner implements DownloadControllerListener, LinkCollectorListener, DownloadWatchdogListener, AccountControllerListener {
        private volatile boolean    hasDownloadLinks        = false;
        private volatile boolean    hasCrawledLinks         = false;
        private volatile boolean    hasLinks                = false;
        private volatile boolean    hasAccounts             = false;
        private volatile long       lastFreeDownloadSeen    = -1;
        private volatile long       lastPremiumDownloadSeen = -1;
        private volatile long       lastMultiDownloadSeen   = -1;
        private volatile long       lastAccountChangeSeen   = -1;
        private final AtomicBoolean hasChanges              = new AtomicBoolean(false);
        private long                lastUpdateTimestamp     = -1;
        private final DomainInfo    domainInfo;

        protected AvailableBanner(final String domain) {
            this.domainInfo = DomainInfo.getInstance(domain);
            DownloadController.getInstance().getEventSender().addListener(this, true);
            LinkCollector.getInstance().getEventsender().addListener(this, true);
            DownloadWatchDog.getInstance().getEventSender().addListener(this, true);
            AccountController.getInstance().getEventSender().addListener(this, true);
        }

        public boolean hasRunningDownloads() {
            for (final SingleDownloadController singleDownloadController : DownloadWatchDog.getInstance().getRunningDownloadLinks()) {
                if (StringUtils.equalsIgnoreCase(getHost(), singleDownloadController.getDownloadLink().getHost())) {
                    return true;
                }
            }
            return false;
        }

        public String getHost() {
            return domainInfo.getTld();
        }

        protected String getIconHost() {
            return getHost();
        }

        public Icon getIcon() {
            return getIcon(TranslationFactory.getDesiredLanguage());
        }

        public Icon getIcon(String lng) {
            if (StringUtils.isEmpty(lng)) {
                /* No language given: this is the English fallback, there is nothing else to try */
                return loadIcon("banner/" + getIconHost() + "_en");
            }
            final Icon ret = loadIcon("banner/" + getIconHost() + "_" + lng);
            if (ret != null) {
                /* Localized icon exists, no need for the English fallback */
                return ret;
            }
            return getIcon(null);
        }

        public String getURL() {
            return getHost();
        }

        protected Icon loadIcon(final String relPath) {
            if (!NewTheme.getInstance().hasIcon(relPath)) {
                /* The theme has no such icon (e.g. banner not available in this language) */
                return null;
            }
            try {
                return NewTheme.getInstance().getIcon(relPath, -1, 25);
            } catch (final Throwable e) {
                LogController.CL().log(e);
                return null;
            }
        }

        public DomainInfo getDomainInfo() {
            return domainInfo;
        }

        public void onClick(final MouseEvent e) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    // marker see BannerRotation.paint method
                    new OpenURLAction(getDomainInfo(), "Banner12072017").actionPerformed(new ActionEvent(e.getSource(), e.getID(), e.paramString()));
                }
            };
        }

        public String getPreSelectedInAddAccountDialog() {
            return getHost();
        }

        private boolean updateData() {
            if (Time.systemIndependentCurrentJVMTimeMillis() - lastUpdateTimestamp <= TimeUnit.MINUTES.toMillis(1)) {
                /* Throttle: the data is recalculated at most once per minute, otherwise report "no change" */
                return false;
            }
            final DomainInfo domainInfo = getDomainInfo();
            final boolean hasDownloadLinks = this.hasDownloadLinks;
            final boolean hasCrawledLinks = this.hasCrawledLinks;
            final boolean hasLinks = this.hasLinks;
            boolean hasRunningDownloadWithAccount = false;
            if (!hasDownloadLinks || lastDownloadLinkEnabled < lastUpdateTimestamp) {
                boolean hasRunningDownload = false;
                for (final SingleDownloadController singleDownloadController : DownloadWatchDog.getInstance().getRunningDownloadLinks()) {
                    final DownloadLinkCandidate candidate = singleDownloadController.getDownloadLinkCandidate();
                    if (domainInfo != candidate.getLink().getDomainInfo()) {
                        /* Only running downloads of this banner's domain are relevant */
                        continue;
                    }
                    hasRunningDownload = true;
                    switch (candidate.getCachedAccount().getType()) {
                    case ORIGINAL:
                        switch (candidate.getCachedAccount().getAccount().getType()) {
                        case LIFETIME:
                        case PREMIUM:
                            hasRunningDownloadWithAccount = true;
                            break;
                        default:
                            break;
                        }
                        break;
                    default:
                        break;
                    }
                }
                if (hasRunningDownload) {
                    this.hasDownloadLinks = true;
                } else {
                    this.hasDownloadLinks = contains(DownloadController.getInstance(), domainInfo, Boolean.TRUE);
                }
            }
            if (!hasCrawledLinks || lastCrawledLinkEnabled < lastUpdateTimestamp) {
                this.hasCrawledLinks = contains(LinkCollector.getInstance(), domainInfo, Boolean.TRUE);
            }
            if (this.hasDownloadLinks || this.hasCrawledLinks) {
                this.hasLinks = true;
            } else if (!hasLinks || lastDownloadLinkDisabled < lastUpdateTimestamp || lastCrawledLinkDisabled < lastUpdateTimestamp) {
                this.hasLinks = contains(DownloadController.getInstance(), domainInfo, null) || contains(LinkCollector.getInstance(), domainInfo, null);
            }
            final boolean hasAccounts = this.hasAccounts;
            if (hasRunningDownloadWithAccount) {
                this.hasAccounts = true;
            } else if (!hasAccounts || lastAccountChangeSeen < lastUpdateTimestamp) {
                this.hasAccounts = false;
                final ArrayList<Account> accounts = AccountController.getInstance().getValidAccounts(getHost());
                if (accounts != null) {
                    for (final Account account : accounts) {
                        switch (account.getType()) {
                        case PREMIUM:
                        case LIFETIME:
                            this.hasAccounts = true;
                            break;
                        default:
                            break;
                        }
                    }
                }
            }
            lastUpdateTimestamp = Time.systemIndependentCurrentJVMTimeMillis();
            return hasAccounts != this.hasAccounts || hasLinks != this.hasLinks || hasDownloadLinks != this.hasDownloadLinks || hasCrawledLinks != this.hasCrawledLinks;
        }

        private volatile long lastDownloadLinkEnabled  = -1;
        private volatile long lastDownloadLinkDisabled = -1;

        @Override
        public void onDownloadControllerAddedPackage(final FilePackage pkg) {
            final boolean readL = pkg.getModifyLock().readLock();
            try {
                for (final DownloadLink link : pkg.getChildren()) {
                    if (link == null || AvailableStatus.FALSE.equals(link.getAvailableStatus()) || getDomainInfo() != link.getDomainInfo()) {
                        /* Offline links and links of other domains do not make this banner relevant */
                        continue;
                    }
                    queue.add(new QueueAction<Void, RuntimeException>() {
                        @Override
                        protected Void run() throws RuntimeException {
                            hasLinks = true;
                            if (link.isEnabled()) {
                                hasDownloadLinks = true;
                                lastDownloadLinkEnabled = Time.systemIndependentCurrentJVMTimeMillis();
                            } else {
                                lastDownloadLinkDisabled = Time.systemIndependentCurrentJVMTimeMillis();
                            }
                            return null;
                        }
                    });
                    onChange();
                    break;
                }
            } finally {
                pkg.getModifyLock().readUnlock(readL);
            }
        }

        @Override
        public void onDownloadControllerStructureRefresh(FilePackage pkg) {
        }

        @Override
        public void onDownloadControllerStructureRefresh() {
        }

        @Override
        public void onDownloadControllerStructureRefresh(AbstractNode node, Object param) {
        }

        @Override
        public void onDownloadControllerRemovedPackage(FilePackage pkg) {
        }

        @Override
        public void onDownloadControllerRemovedLinklist(List<DownloadLink> list) {
        }

        @Override
        public void onDownloadControllerUpdatedData(final DownloadLink link, final DownloadLinkProperty property) {
            if (link == null || AvailableStatus.FALSE.equals(link.getAvailableStatus()) || getDomainInfo() != link.getDomainInfo()) {
                /* Offline links and links of other domains do not make this banner relevant */
                return;
            }
            queue.add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    hasLinks = true;
                    if (link.isEnabled()) {
                        hasDownloadLinks = true;
                        lastDownloadLinkEnabled = Time.systemIndependentCurrentJVMTimeMillis();
                    } else {
                        lastDownloadLinkDisabled = Time.systemIndependentCurrentJVMTimeMillis();
                    }
                    return null;
                }
            });
            onChange();
        }

        @Override
        public void onDownloadControllerUpdatedData(FilePackage pkg, FilePackageProperty property) {
        }

        @Override
        public void onDownloadControllerUpdatedData(DownloadLink downloadlink) {
        }

        @Override
        public void onDownloadControllerUpdatedData(FilePackage pkg) {
        }

        @Override
        public void onLinkCollectorAbort(LinkCollectorEvent event) {
        }

        @Override
        public void onLinkCollectorFilteredLinksAvailable(LinkCollectorEvent event) {
        }

        @Override
        public void onLinkCollectorFilteredLinksEmpty(LinkCollectorEvent event) {
        }

        @Override
        public void onLinkCollectorDataRefresh(final LinkCollectorEvent event) {
            if (event == null || !(event.getParameter() instanceof CrawledLink)) {
                /* Only refresh events for a single CrawledLink are relevant */
                return;
            }
            final CrawledLink link = (CrawledLink) event.getParameter();
            if (link == null || AvailableLinkState.OFFLINE.equals(link.getLinkState()) || getDomainInfo() != link.getDomainInfo()) {
                /* Offline links and links of other domains do not make this banner relevant */
                return;
            }
            queue.add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    hasLinks = true;
                    if (link.isEnabled()) {
                        hasCrawledLinks = true;
                        lastCrawledLinkEnabled = Time.systemIndependentCurrentJVMTimeMillis();
                    } else {
                        lastCrawledLinkDisabled = Time.systemIndependentCurrentJVMTimeMillis();
                    }
                    return null;
                }
            });
            onChange();
        }

        @Override
        public void onLinkCollectorStructureRefresh(LinkCollectorEvent event) {
        }

        @Override
        public void onLinkCollectorContentRemoved(LinkCollectorEvent event) {
        }

        @Override
        public void onLinkCollectorContentAdded(LinkCollectorEvent event) {
        }

        private volatile long lastCrawledLinkEnabled  = -1;
        private volatile long lastCrawledLinkDisabled = -1;

        @Override
        public void onLinkCollectorLinkAdded(final LinkCollectorEvent event, final CrawledLink link) {
            if (link == null || AvailableLinkState.OFFLINE.equals(link.getLinkState()) || getDomainInfo() != link.getDomainInfo()) {
                /* Offline links and links of other domains do not make this banner relevant */
                return;
            }
            queue.add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    hasLinks = true;
                    if (link.isEnabled()) {
                        hasCrawledLinks = true;
                        lastCrawledLinkEnabled = Time.systemIndependentCurrentJVMTimeMillis();
                    } else {
                        lastCrawledLinkDisabled = Time.systemIndependentCurrentJVMTimeMillis();
                    }
                    return null;
                }
            });
            onChange();
        }

        @Override
        public void onLinkCollectorDupeAdded(final LinkCollectorEvent event, final CrawledLink link) {
            if (link == null || AvailableLinkState.OFFLINE.equals(link.getLinkState()) || getDomainInfo() != link.getDomainInfo()) {
                /* Offline links and links of other domains do not make this banner relevant */
                return;
            }
            queue.add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    hasLinks = true;
                    if (link.isEnabled()) {
                        hasCrawledLinks = true;
                        lastCrawledLinkEnabled = Time.systemIndependentCurrentJVMTimeMillis();
                    } else {
                        lastCrawledLinkDisabled = Time.systemIndependentCurrentJVMTimeMillis();
                    }
                    return null;
                }
            });
            onChange();
        }

        @Override
        public void onLinkCrawlerAdded(LinkCollectorCrawler crawler) {
        }

        @Override
        public void onLinkCrawlerStarted(LinkCollectorCrawler crawler) {
        }

        @Override
        public void onLinkCrawlerStopped(LinkCollectorCrawler crawler) {
        }

        @Override
        public void onLinkCrawlerFinished() {
        }

        @Override
        public void onLinkCrawlerNewJob(LinkCollectingJob job) {
        }

        @Override
        public void onDownloadWatchdogDataUpdate() {
        }

        @Override
        public void onDownloadWatchdogStateIsIdle() {
        }

        @Override
        public void onDownloadWatchdogStateIsPause() {
        }

        @Override
        public void onDownloadWatchdogStateIsRunning() {
        }

        @Override
        public void onDownloadWatchdogStateIsStopped() {
        }

        @Override
        public void onDownloadWatchdogStateIsStopping() {
        }

        @Override
        public void onDownloadControllerStart(final SingleDownloadController downloadController, final DownloadLinkCandidate candidate) {
            if (getDomainInfo() != candidate.getLink().getDomainInfo()) {
                /* The started download belongs to another domain */
                return;
            }
            queue.add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    hasDownloadLinks = true;
                    hasLinks = true;
                    switch (candidate.getCachedAccount().getType()) {
                    case NONE:
                        lastFreeDownloadSeen = Time.systemIndependentCurrentJVMTimeMillis();
                        break;
                    case ORIGINAL:
                        switch (candidate.getCachedAccount().getAccount().getType()) {
                        case FREE:
                        case UNKNOWN:
                            lastFreeDownloadSeen = Time.systemIndependentCurrentJVMTimeMillis();
                            break;
                        default:
                            lastPremiumDownloadSeen = Time.systemIndependentCurrentJVMTimeMillis();
                            break;
                        }
                        break;
                    case MULTI:
                        lastMultiDownloadSeen = Time.systemIndependentCurrentJVMTimeMillis();
                        break;
                    default:
                        break;
                    }
                    return null;
                }
            });
            onChange();
        }

        @Override
        public void onDownloadControllerStopped(SingleDownloadController downloadController, DownloadLinkCandidate candidate, DownloadLinkCandidateResult result) {
        }

        @Override
        public void onDownloadWatchDogPropertyChange(DownloadWatchDogProperty propertyChange) {
        }

        @Override
        public void onAccountControllerEvent(final AccountControllerEvent event) {
            final Account acc = event.getAccount();
            if (acc == null || !StringUtils.equalsIgnoreCase(getDomainInfo().getTld(), acc.getHoster())) {
                /* The account belongs to another hoster */
                return;
            }
            switch (event.getType()) {
            case ADDED:
            case ACCOUNT_UP_OR_DOWNGRADE:
            case ACCOUNT_CHECKED:
                break;
            default:
                /* Other event types (e.g. REMOVED) never add a premium account */
                return;
            }
            switch (acc.getType()) {
            case LIFETIME:
            case PREMIUM:
                break;
            default:
                /* Only premium/lifetime accounts make the banner obsolete */
                return;
            }
            if (!acc.isEnabled() || !acc.isValid()) {
                /* Disabled or invalid accounts cannot be used for downloading */
                return;
            }
            queue.add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    hasAccounts = true;
                    lastAccountChangeSeen = Time.systemIndependentCurrentJVMTimeMillis();
                    return null;
                }
            });
            onChange();
        }

        public boolean hasChanges() {
            return queue.addWait(new QueueAction<Boolean, RuntimeException>() {
                @Override
                protected Boolean run() throws RuntimeException {
                    return updateData() || hasChanges.compareAndSet(true, false);
                }
            });
        }

        protected void onChange() {
            hasChanges.set(true);
            BannerRotation.this.onChange(this);
        }
    }

    private class Banner {
        private final Icon            icon;
        private final AvailableBanner banner;
        private final long            timeStamp = Time.systemIndependentCurrentJVMTimeMillis();

        private Banner(AvailableBanner banner) {
            this.icon = banner.getIcon();
            this.banner = banner;
        }

        private final long getTimestamp() {
            return timeStamp;
        }

        @Override
        public int hashCode() {
            return getBanner().getHost().hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                /* Nothing is equal to null */
                return false;
            }
            if (obj == this) {
                /* Same instance, no need to compare the hosts */
                return true;
            }
            if (!(obj instanceof Banner)) {
                /* Different type */
                return false;
            }
            return StringUtils.equals(getBanner().getHost(), ((Banner) obj).getBanner().getHost());
        }

        private final Icon getIcon() {
            return icon;
        }

        private final AvailableBanner getBanner() {
            return banner;
        }
    }

    private final MainTabbedPane pane;
    private volatile Banner      current = null;

    private void rotateBanner(List<AvailableBanner> rotateBanners) {
        final Banner old = current;
        try {
            if (!isEnabled()) {
                /* Banners are disabled: a rotation that was scheduled before must not bring the banner back */
                current = null;
                return;
            }
            if (rotateBanners != null && rotateBanners.size() == 0) {
                /* The caller explicitly passed an empty rotation: no banner shall be shown */
                current = null;
                return;
            }
            if (rotateBanners == null) {
                rotateBanners = getAllBanners();
            }
            if (rotateBanners == null || rotateBanners.size() == 0) {
                /* Nothing to rotate (no rotation passed and no banners available) */
                current = null;
                return;
            }
            if (old == null) {
                /* First banner: start with the first one of the rotation */
                current = new Banner(rotateBanners.get(0));
                return;
            }
            final int index = rotateBanners.indexOf(old.getBanner());
            if (index == -1) {
                /*
                 * Current banner is not part of the rotation (anymore), e.g. because the user just added a premium account for this hoster:
                 * replace it right away, the minimum showtime does not apply.
                 */
                current = new Banner(rotateBanners.get(0));
                return;
            }
            if (Time.systemIndependentCurrentJVMTimeMillis() - old.getTimestamp() <= bannerMinimumShowtime) {
                /* The current banner has not been shown long enough yet, keep it */
                return;
            }
            if (index + 1 == rotateBanners.size() || rotateBanners.size() == 1) {
                /* Current banner was the last one: wrap around to the first */
                current = new Banner(rotateBanners.get(0));
                return;
            }
            current = new Banner(rotateBanners.get(index + 1));
        } finally {
            if (old != current) {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        pane.repaint();
                    }
                };
            }
        }
    }

    private final AtomicBoolean   isBannerEnabled       = new AtomicBoolean(false);
    private final long            refreshTimeout        = TimeUnit.MINUTES.toMillis(1);
    private final long            bannerMinimumShowtime = TimeUnit.MINUTES.toMillis(5);
    private final DelayedRunnable updateDelayer;

    private List<AvailableBanner> updateRotation() {
        final ArrayList<AvailableBanner> rotation = new ArrayList<AvailableBanner>();
        final ArrayList<AvailableBanner> fallback = new ArrayList<AvailableBanner>();
        for (final AvailableBanner banner : getAllBanners()) {
            if (banner.getIcon() == null) {
                // cannot be shown, skip
                continue;
            }
            if (banner.hasChanges()) {
                // required to process changes
            }
            if (banner.hasAccounts) {
                /* The user already has a premium account for this hoster, do not advertise it */
                continue;
            }
            if (banner.hasDownloadLinks || banner.hasRunningDownloads()) {
                rotation.add(0, banner);
            } else if (banner.hasLinks) {
                rotation.add(banner);
            } else {
                fallback.add(banner);
            }
        }
        if (rotation.size() > 0) {
            /* Banners of hosters the user actually uses (links/downloads) take precedence */
            return rotation;
        }
        return fallback;
    }

    private List<AvailableBanner> getAllBanners() {
        return allBanners;
    }

    public BannerRotation() {
        pane = MainTabbedPane.getInstance();
        updateDelayer = new DelayedRunnable(1000, 15000) {
            @Override
            public void delayedrun() {
                queue.addWait(new QueueAction<Void, RuntimeException>() {
                    @Override
                    protected Void run() throws RuntimeException {
                        rotateBanner(updateRotation());
                        return null;
                    }
                });
            }
        };
        final Thread refreshThread = new Thread("BannerRotation") {
            {
                setDaemon(true);
            }

            public void run() {
                while (true) {
                    try {
                        Thread.sleep(refreshTimeout);
                    } catch (InterruptedException e) {
                    }
                    onChange(null);
                }
            };
        };
        SecondLevelLaunch.INIT_COMPLETE.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                new Thread() {
                    @Override
                    public void run() {
                        SecondLevelLaunch.ACCOUNTLIST_LOADED.executeWhenReached(new Runnable() {
                            @Override
                            public void run() {
                                AccountController.getInstance().getEventSender().addListener(BannerRotation.this, true);
                            }
                        });
                        CFG_GUI.BANNER_ENABLED.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
                            @Override
                            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                                final boolean isEnabled = Boolean.TRUE.equals(newValue);
                                if (isBannerEnabled.getAndSet(isEnabled) == isEnabled) {
                                    /* The enabled state did not actually change */
                                    return;
                                }
                                /* Stored wall-clock timestamp: System.currentTimeMillis() is intended here */
                                JsonConfig.create(GraphicalUserInterfaceSettings.class).setBannerChangeTimestamp(System.currentTimeMillis());
                                if (isEnabled) {
                                    /* Banners were just enabled: start the rotation, there is nothing to hide */
                                    updateDelayer.resetAndStart();
                                    return;
                                }
                                new EDTRunner() {
                                    @Override
                                    protected void runInEDT() {
                                        current = null;
                                        pane.repaint();
                                    }
                                };
                            }

                            @Override
                            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
                            }
                        });
                        isBannerEnabled.set(CFG_GUI.BANNER_ENABLED.isEnabled());
                        getAllBanners().add(new AvailableBanner("rapidgator.net"));
                        getAllBanners().add(new AvailableBanner("k2s.cc") {
                            @Override
                            protected String getIconHost() {
                                return "keep2share.cc";
                            };
                        });
                        getAllBanners().add(new AvailableBanner("ddownload.com"));
                        getAllBanners().add(new AvailableBanner("filejoker.net"));
                        getAllBanners().add(new AvailableBanner("filer.net"));
                        updateDelayer.resetAndStart();
                        refreshThread.start();
                    }
                }.start();
            }
        });
    }

    private void onChange(AvailableBanner banner) {
        if (!isEnabled()) {
            /* Banners are disabled, no need to recalculate the rotation */
            return;
        }
        updateDelayer.resetAndStart();
    }

    @Override
    public void onAccountControllerEvent(AccountControllerEvent event) {
        try {
            if (event == null || !AccountControllerEvent.Types.ACCOUNT_UP_OR_DOWNGRADE.equals(event.getType()) || !CFG_GUI.CFG.isPremiumExpireWarningEnabled()) {
                /* Only up-/downgrade events are relevant, and only if the user did not disable the expiry warnings */
                return;
            }
            final AccountUpOrDowngradeEvent accountEvent = (AccountUpOrDowngradeEvent) event;
            final long DAY = TimeUnit.DAYS.toMillis(1);
            final Account account = event.getAccount();
            final String hoster = account.getHoster();
            if (accountEvent.isPremiumAccount() && !accountEvent.isPremiumExpired()) {
                /*
                 * Premium account that is still valid: warn if it expires within one day. Intentionally does not check for another valid
                 * premium account of the same hoster (unlike the downgrade case below): the warning is only a hint and shown at most once
                 * per hoster every 30 days.
                 */
                final long expireTimeStamp = accountEvent.getExpireTimeStamp();
                /* The expire date is a wall-clock date: compare it with System.currentTimeMillis() */
                if (expireTimeStamp <= 0 || expireTimeStamp - System.currentTimeMillis() >= DAY) {
                    /* No known expire date (e.g. lifetime) or it is more than one day away: too early to warn */
                    return;
                }
                if (!markNotified("warn_" + hoster, TimeUnit.DAYS.toMillis(30))) {
                    /* Already warned for this hoster within the last 30 days */
                    return;
                }
                notify(account, _GUI.T.OboomController_onAccountControllerEvent_premiumexpire_warn_still_premium_title(hoster), _GUI.T.OboomController_onAccountControllerEvent_premiumexpire_warn_still_premium_msg(account.getUser(), hoster));
                return;
            }
            if (!accountEvent.isPremiumDowngraded()) {
                /* Neither a valid premium account nor a downgrade: nothing to tell the user */
                return;
            }
            /* Only ask if there is no other valid premium account for this hoster */
            if (!AccountController.getInstance().listAccounts(new AccountFilter(hoster).setEnabled(true).setValid(true).setAccountTypes(AccountType.PREMIUM, AccountType.LIFETIME).setExpired(false).setMaxResultsNum(1)).isEmpty()) {
                /* The user still has another usable premium account for this hoster */
                return;
            }
            /* Ask at max once a month */
            if (!markNotified("expired_" + hoster, TimeUnit.DAYS.toMillis(30))) {
                /* Already asked for this hoster within the last 30 days */
                return;
            }
            notify(account, _GUI.T.OboomController_onAccountControllerEvent_premiumexpire_warn_expired_premium_title(hoster), _GUI.T.OboomController_onAccountControllerEvent_premiumexpire_warn_expired_premium_msg(account.getUser(), hoster));
        } catch (final Throwable e) {
            LogController.CL().log(e);
        }
    }

    /**
     * Checks whether the notification with the given ID may be shown (not shown within the last <code>minInterval</code> milliseconds) and
     * if so, remembers the current time for it.
     *
     * @return true if the notification may be shown now
     */
    private synchronized boolean markNotified(final String id, final long minInterval) {
        Map<String, Long> expireNotifies = CFG_GUI.CFG.getPremiumExpireWarningMapV2();
        final Long lastNotify = expireNotifies != null ? expireNotifies.get(id) : null;
        /* Persisted wall-clock timestamp: System.currentTimeMillis() is intended here */
        final long now = System.currentTimeMillis();
        if (lastNotify != null && now - lastNotify <= minInterval) {
            /* Notified too recently */
            return false;
        }
        if (expireNotifies == null) {
            expireNotifies = new HashMap<String, Long>();
        }
        expireNotifies.put(id, now);
        CFG_GUI.CFG.setPremiumExpireWarningMapV2(expireNotifies);
        return true;
    }

    private void notify(final Account account, String title, String msg) {
        /* May be null: the dialog is still shown, only the afflink cannot be opened then */
        final PluginForHost plugin = account.getPlugin();
        final Icon fav = account.getDomainInfo().getFavIcon(false);
        final ExtMergedIcon hosterIcon = new ExtMergedIcon(new AbstractIcon(IconKey.ICON_REFRESH, 32)).add(fav, 32 - fav.getIconWidth(), 32 - fav.getIconHeight());
        final ConfirmDialog d = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, title, msg, hosterIcon, _GUI.T.lit_continue(), _GUI.T.lit_close()) {
            @Override
            public ModalityType getModalityType() {
                return ModalityType.MODELESS;
            }

            @Override
            protected void addButtons(JPanel buttonBar) {
                /* The ad disclosure is intentionally always shown here. */
                final JLabel adDisclosureText = AdDisclosureHelper.createAdDisclosureLabel(null, SwingConstants.BOTTOM);
                buttonBar.add(adDisclosureText, "aligny bottom, gapbottom 2", 0);
            };

            @Override
            public String getDontShowAgainKey() {
                return "expireRenewNotification_" + account.getHoster();
            }

            @Override
            public long getCountdown() {
                return TimeUnit.MINUTES.toMillis(5);
            }
        };
        try {
            /*
             * TODO: If the user ticked "do not show again" and clicked OK once, showDialog returns the remembered OK immediately in the
             * future and the afflink below is opened without any dialog (LOGIC_DONT_SHOW_AGAIN_IGNORES_OK is not set). Also, the key
             * "expireRenewNotification_<hoster>" is shared by the "expires soon" and the "expired" notification, so ticking it for one
             * suppresses the other.
             */
            Dialog.getInstance().showDialog(d);
            if (plugin != null) {
                AccountController.openAfflink(plugin.getLazyP(), plugin, "expireRenewNotification");
            }
        } catch (DialogNoAnswerException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Rectangle paint(Graphics2D g) {
        final Banner banner = this.current;
        final Icon icon = banner != null ? banner.icon : null;
        if (!isVisible() || !isEnabled() || icon == null) {
            /* No banner to draw (nothing selected, banners disabled or no icon): nothing painted, no area to report */
            return null;
        }
        /* Gap between ad label and icon */
        final int gap = 3;
        /* Visible area (set by the caller). The ad label is only clickable/hoverable inside of it. */
        final Rectangle visibleArea = g.getClipBounds();
        /* 1. Determine font (JLabel default) and metrics */
        Font baseFont = UIManager.getFont("Label.font");
        if (baseFont == null) {
            baseFont = new JLabel().getFont();
        }
        final Font adFont = baseFont.deriveFont(8f);
        final FontMetrics fm = g.getFontMetrics(adFont);
        final String adLabel = AdDisclosureHelper.AD_LABEL_SHORT;
        final int textWidth = fm.stringWidth(adLabel);
        /* 2. Position of the icon */
        final int iconX = pane.getWidth() - icon.getIconWidth() - 2;
        final int iconY = 25 - icon.getIconHeight();
        /* 3. Draw the label left of the icon (vertically aligned with the icon) */
        final Graphics2D gText = (Graphics2D) g.create();
        try {
            gText.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            gText.setFont(adFont);
            /* Adapts to the dark/light theme automatically */
            gText.setColor(UIManager.getColor("Label.foreground"));
            final int textX = iconX - gap - textWidth;
            final int textY = iconY + fm.getAscent();
            gText.drawString(adLabel, textX, textY);
        } finally {
            gText.dispose();
        }
        /* 4. Draw the icon */
        final Rectangle bannerRectangle = new Rectangle(iconX, iconY, icon.getIconWidth(), icon.getIconHeight() + 2);
        g = (Graphics2D) g.create(bannerRectangle.x, bannerRectangle.y, bannerRectangle.width, bannerRectangle.height);
        try {
            if (g.getTransform().getScaleX() != 1 || g.getTransform().getScaleY() != 1) {
                /* At least use interpolation for HIGHDPI */
                /* TODO: Use banner images with higher resolution */
                g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, Interpolation.BILINEAR.getHint());
            }
            icon.paintIcon(pane, g, 0, 0);
            /* Whole area (label + icon), reduced to the part that is actually visible (the label may be clipped). */
            final Rectangle ret = new Rectangle(iconX - gap - textWidth, bannerRectangle.y, textWidth + gap + bannerRectangle.width, bannerRectangle.height);
            return visibleArea != null ? ret.intersection(visibleArea) : ret;
        } finally {
            g.dispose();
        }
    }

    private boolean isEnabled() {
        return isBannerEnabled.get();
    }

    @Override
    public boolean isVisible() {
        return this.current != null;
    }

    @Override
    public void onMouseOver(MouseEvent e) {
        if (!isEnabled()) {
            /* Banners are disabled: no clickable banner, so no hand cursor */
            return;
        }
        pane.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }

    @Override
    public void onMouseOut(MouseEvent e) {
        pane.setCursor(null);
    }

    @Override
    public String getPreSelectedInAddAccountDialog() {
        final Banner banner = this.current;
        if (banner == null) {
            /* No banner is shown right now: use the default hoster */
            return "rapidgator.net";
        }
        return banner.getBanner().getPreSelectedInAddAccountDialog();
    }

    @Override
    public void onClicked(MouseEvent e) {
        final Banner banner = this.current;
        if (banner == null || !isEnabled()) {
            /* Nothing is shown or banners are disabled: the click does not hit a banner */
            return;
        }
        banner.getBanner().onClick(e);
    }

    private static <P extends AbstractPackageNode<C, P>, C extends AbstractPackageChildrenNode<P>> boolean contains(PackageController<P, C> controller, final DomainInfo domainInfo, final Boolean enabledFlag) {
        final AtomicBoolean ret = new AtomicBoolean(false);
        controller.visitNodes(new AbstractNodeVisitor<C, P>() {
            @Override
            public Boolean visitPackageNode(P pkg) {
                if (ret.get()) {
                    /* A match was already found: stop visiting */
                    return null;
                }
                if (enabledFlag == null || enabledFlag.booleanValue() == pkg.isEnabled()) {
                    return Boolean.TRUE;
                }
                /* Package has the wrong enabled state: skip its children */
                return Boolean.FALSE;
            }

            @Override
            public Boolean visitChildrenNode(C node) {
                if ((enabledFlag == null || node.isEnabled() == enabledFlag.booleanValue()) && node.getDomainInfo() == domainInfo) {
                    /* Match found: remember it and stop visiting */
                    ret.set(true);
                    return null;
                }
                return Boolean.TRUE;
            }
        }, true);
        return ret.get();
    }
}
