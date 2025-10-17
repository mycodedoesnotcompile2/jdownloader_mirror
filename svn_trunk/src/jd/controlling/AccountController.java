//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.controlling;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.TimeUnit;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.Eventsender;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.plugins.controller.PluginClassLoader;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;
import org.jdownloader.plugins.controller.UpdateRequiredClassNotFoundException;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;
import org.jdownloader.settings.AccountData;
import org.jdownloader.settings.AccountSettings;

import jd.controlling.accountchecker.AccountChecker;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;
import jd.http.Browser;
import jd.http.BrowserSettingsThread;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountError;
import jd.plugins.Account.AccountPropertyChangeHandler;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountProperty;
import jd.plugins.AccountUnavailableException;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.Plugin;
import jd.plugins.Plugin.PluginEnvironment;
import jd.plugins.PluginForHost;

public class AccountController implements AccountControllerListener, AccountPropertyChangeHandler {
    private static final long                                                    serialVersionUID = -7560087582989096645L;
    private final HashMap<String, List<Account>>                                 ACCOUNTS;
    @Deprecated
    private final HashMap<String, Map<Account, Object>>                          MULTIHOSTER_ACCOUNTS;
    private static AccountController                                             INSTANCE         = new AccountController();
    private final Eventsender<AccountControllerListener, AccountControllerEvent> broadcaster      = new Eventsender<AccountControllerListener, AccountControllerEvent>() {
                                                                                                      @Override
                                                                                                      protected void fireEvent(final AccountControllerListener listener, final AccountControllerEvent event) {
                                                                                                          listener.onAccountControllerEvent(event);
                                                                                                      }
                                                                                                  };

    public Eventsender<AccountControllerListener, AccountControllerEvent> getEventSender() {
        return broadcaster;
    }

    private AccountSettings config;
    private DelayedRunnable delayedSaver;

    private AccountController() {
        super();
        config = JsonConfig.create(AccountSettings.class);
        if (config.getListID() <= 0) {
            config.setListID(System.currentTimeMillis());
        }
        ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
            @Override
            public void onShutdown(final ShutdownRequest shutdownRequest) {
                save();
            }

            @Override
            public long getMaxDuration() {
                return 0;
            }

            @Override
            public String toString() {
                return "ShutdownEvent: Save AccountController";
            }
        });
        ACCOUNTS = loadAccounts(config);
        MULTIHOSTER_ACCOUNTS = new HashMap<String, Map<Account, Object>>();
        delayedSaver = new DelayedRunnable(5000, 30000) {
            @Override
            public String getID() {
                return "AccountController";
            }

            @Override
            public void delayedrun() {
                save();
            }
        };
        final Collection<List<Account>> accsc = ACCOUNTS.values();
        for (final List<Account> accs : accsc) {
            for (final Account acc : accs) {
                acc.setAccountController(this);
                if (acc.getPlugin() != null) {
                    updateInternalMultiHosterMap(acc, acc.getAccountInfo());
                }
            }
        }
        broadcaster.addListener(this);
        delayedSaver.getService().scheduleWithFixedDelay(new Runnable() {
            public void run() {
                if (JsonConfig.create(AccountSettings.class).isAutoAccountRefreshEnabled()) {
                    /*
                     * this scheduleritem checks all enabled accounts every 5 mins
                     */
                    try {
                        refreshAccountStats();
                    } catch (Throwable e) {
                        LogController.CL().log(e);
                    }
                }
            }
        }, 1, 5, TimeUnit.MINUTES);
        org.jdownloader.settings.staticreferences.CFG_GENERAL.USE_AVAILABLE_ACCOUNTS.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                if (JDGui.bugme(WarnLevel.SEVERE)) {
                    if (!newValue) {
                        final ConfirmDialog d = new ConfirmDialog(0 | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, _GUI.T.lit_are_you_sure(), _GUI.T.are_you_sure_disabled_premium(), new AbstractIcon(IconKey.ICON_QUESTION, 32), _GUI.T.lit_continue(), null);
                        try {
                            UIOManager.I().show(ConfirmDialogInterface.class, d).throwCloseExceptions();
                            return;
                        } catch (DialogClosedException e) {
                        } catch (DialogCanceledException e) {
                        }
                        // we need a new thread to throw correct change events
                        new Thread("") {
                            public void run() {
                                org.jdownloader.settings.staticreferences.CFG_GENERAL.USE_AVAILABLE_ACCOUNTS.setValue(true);
                            };
                        }.start();
                    }
                } else {
                    return;
                }
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }
        });
    }

    protected void save() {
        final HashMap<String, ArrayList<AccountData>> ret = new HashMap<String, ArrayList<AccountData>>();
        synchronized (AccountController.this) {
            for (final Iterator<Entry<String, List<Account>>> it = ACCOUNTS.entrySet().iterator(); it.hasNext();) {
                final Entry<String, List<Account>> next = it.next();
                if (next.getValue().size() == 0) {
                    continue;
                }
                final ArrayList<AccountData> list = new ArrayList<AccountData>(next.getValue().size());
                ret.put(next.getKey(), list);
                for (final Account a : next.getValue()) {
                    list.add(AccountData.create(a));
                }
            }
        }
        config.setListVersion(System.currentTimeMillis());
        config.setAccounts(ret);
    }

    private void updateInternalMultiHosterMap(Account account, AccountInfo ai) {
        synchronized (AccountController.this) {
            Iterator<Entry<String, Map<Account, Object>>> it = MULTIHOSTER_ACCOUNTS.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Map<Account, Object>> next = it.next();
                final Map<Account, Object> accs = next.getValue();
                if (accs.remove(account) != null && accs.size() == 0) {
                    it.remove();
                }
            }
            boolean isMulti = false;
            if (ai != null) {
                final List<MultiHostHost> multiHostSupport = ai.getMultiHostSupportV2();
                if (multiHostSupport != null) {
                    for (final MultiHostHost support : multiHostSupport) {
                        isMulti = true;
                        final MultihosterHostStatus status = support.getStatus();
                        switch (status) {
                        case WORKING:
                        case WORKING_UNSTABLE:
                            break;
                        default:
                            continue;
                        }
                        final String host = support.getDomain();
                        Map<Account, Object> accs = MULTIHOSTER_ACCOUNTS.get(host);
                        if (accs == null) {
                            accs = new WeakHashMap<Account, Object>();
                            MULTIHOSTER_ACCOUNTS.put(host, accs);
                        }
                        accs.put(account, this);
                    }
                }
            }
            account.setProperty(Account.IS_MULTI_HOSTER_ACCOUNT, isMulti);
        }
    }

    public AccountInfo updateAccountInfo(final Account account, final boolean forceupdate) {
        AccountInfo ai = account.getAccountInfo();
        if (account.getPlugin() == null) {
            return ai;
        }
        final AccountError errorBefore = account.getError();
        double balanceBefore = -1;
        if (ai != null) {
            balanceBefore = ai.getAccountBalance();
        }
        PluginForHost plugin = null;
        final HashMap<AccountProperty.Property, AccountProperty> propertyChanges = new HashMap<AccountProperty.Property, AccountProperty>();
        try {
            final AccountPropertyChangeHandler handler = new AccountPropertyChangeHandler() {
                @Override
                public boolean fireAccountPropertyChange(AccountProperty property) {
                    if (property != null) {
                        synchronized (propertyChanges) {
                            propertyChanges.put(property.getProperty(), property);
                        }
                    }
                    return false;
                }
            };
            account.setNotifyHandler(handler);
            account.setChecking(true);
            if (!forceupdate) {
                if (account.lastUpdateTime() != 0) {
                    if (ai != null && ai.isExpired()) {
                        account.setError(AccountError.EXPIRED, -1, null);
                        /* account is expired, no need to update */
                        return ai;
                    } else if (!account.isValid()) {
                        account.setError(AccountError.INVALID, -1, null);
                        /* account is invalid, no need to update */
                        return ai;
                    }
                }
                if ((System.currentTimeMillis() - account.lastUpdateTime()) < account.getRefreshTimeout()) {
                    /*
                     * account was checked before, timeout for recheck not reached, no need to update
                     */
                    return ai;
                }
            }
            final PluginClassLoaderChild cl = PluginClassLoader.getSharedChild(account.getPlugin());
            PluginClassLoader.setThreadPluginClassLoaderChild(cl, null);
            try {
                plugin = account.getPlugin().getLazyP().newInstance(cl);
                if (plugin == null) {
                    LogController.CL().severe("AccountCheck: Failed because plugin " + account.getHoster() + " is missing!");
                    account.setError(AccountError.PLUGIN_ERROR, -1, null);
                    return null;
                }
            } catch (final Throwable e) {
                LogController.CL().log(e);
                account.setError(AccountError.PLUGIN_ERROR, -1, e.getMessage());
                return null;
            }
            final String whoAmI = account.getUser() + "->" + account.getHoster();
            LogSource logger = LogController.getFastPluginLogger("accountCheck:" + plugin.getHost() + "_" + plugin.getLazyP().getClassName());
            logger.info("Account Update: " + whoAmI + "(" + plugin.getLazyP().getClassName() + "|" + plugin.getVersion() + ")");
            plugin.setLogger(logger);
            Thread currentThread = Thread.currentThread();
            BrowserSettingsThread bThread = null;
            LogInterface oldLogger = null;
            if (currentThread instanceof BrowserSettingsThread) {
                bThread = (BrowserSettingsThread) currentThread;
            }
            if (bThread != null) {
                /* set logger to browserSettingsThread */
                oldLogger = bThread.getLogger();
                bThread.setLogger(logger);
            }
            try {
                final Browser br = plugin.createNewBrowserInstance();
                br.setLogger(logger);
                plugin.setBrowser(br);
                plugin.setDownloadLink(plugin.buildAccountCheckDownloadLink(account));
                plugin.init();
                /* not every plugin sets this info correctly */
                account.setError(null, -1, null);
                /* get previous account info and resets info for new update */
                ai = account.getAccountInfo();
                if (ai != null) {
                    /* reset expired and setValid */
                    ai.setLastValidUntil(ai.getValidUntil());
                    ai.setExpired(false);
                }
                long tempDisabledCounterBefore = account.getTmpDisabledTimeout();
                try {
                    plugin.validateLogins(account);
                    ai = plugin.fetchAccountInfo(account);
                    plugin.validateLastChallengeResponse();
                    account.setAccountInfo(ai);
                } finally {
                    account.setUpdateTime(System.currentTimeMillis());
                }
                if (account.isValid() == false) {
                    /* account is invalid */
                    logger.info("Account:" + whoAmI + "|Invalid!");
                    account.setError(AccountError.INVALID, -1, null);
                    return ai;
                }
                if (plugin.hasFeature(FEATURE.CAPTCHA_SOLVER)) {
                    ai.setStatus("Balance: " + ai.getAccountBalanceFormatted());
                    if (ai.getAccountBalance() <= 0) {
                        account.setError(AccountError.INVALID, -1, "Zero balance");
                    }
                    /* Warn on low balance */
                    final double lowBalanceThreshold = 1.0;
                    if (balanceBefore >= lowBalanceThreshold && ai.getAccountBalance() < lowBalanceThreshold) {
                        // TODO: Display notification on low balance
                    }
                }
                /* Account check was successful. Account can still get an error status down below if it is expired or out of traffic. */
                account.setLastValidTimestamp(System.currentTimeMillis());
                if (ai != null && ai.isExpired()) {
                    /* expired account */
                    logger.clear();
                    logger.info("Account:" + whoAmI + "|Expired!");
                    account.setError(AccountError.EXPIRED, -1, null);
                    return ai;
                } else if (ai != null && !ai.isUnlimitedTraffic() && !ai.isSpecialTraffic() && ai.getTrafficLeft() == 0) {
                    throw new AccountUnavailableException(_GUI.T.account_error_no_traffic_left(), 5 * 60 * 1000);
                }
                if (tempDisabledCounterBefore > 0 && account.getTmpDisabledTimeout() == tempDisabledCounterBefore) {
                    /* Reset temp disabled information */
                    logger.info("no longer temp disabled!");
                    if (AccountError.TEMP_DISABLED.equals(account.getError())) {
                        account.setError(null, -1, null);
                    }
                }
                logger.clear();
            } catch (final Throwable e) {
                return plugin.handleAccountException(account, logger, e);
            } finally {
                try {
                    if (plugin != null) {
                        plugin.invalidateLastChallengeResponse();
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
                if (plugin != null) {
                    try {
                        plugin.clean();
                    } catch (final Throwable e) {
                        logger.log(e);
                    }
                }
                logger.close();
                if (bThread != null) {
                    /* remove logger from browserSettingsThread */
                    bThread.setLogger(oldLogger);
                }
            }
            return ai;
        } finally {
            PluginClassLoader.setThreadPluginClassLoaderChild(null, null);
            account.setNotifyHandler(null);
            account.setChecking(false);
            getEventSender().fireEvent(new AccountControllerEvent(this, AccountControllerEvent.Types.ACCOUNT_CHECKED, account));
            checkAccountUpOrDowngrade(account);
            final AccountError errorNow = account.getError();
            if (errorBefore != errorNow) {
                AccountProperty latestChangeEvent = null;
                synchronized (propertyChanges) {
                    latestChangeEvent = propertyChanges.get(AccountProperty.Property.ERROR);
                }
                if (latestChangeEvent != null) {
                    getEventSender().fireEvent(new AccountPropertyChangedEvent(latestChangeEvent.getAccount(), latestChangeEvent));
                }
            }
        }
    }

    private final String lastKnownAccountTypeProperty         = "lastKnownAccountType";
    private final String lastKnownValidUntilTimeStampProperty = "lastKnownValidUntilTimeStamp";
    private final long   minimumExtendTime                    = 24 * 60 * 60 * 1000l;

    private void checkAccountUpOrDowngrade(final Account account) {
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return;
        } else if (account.getError() != null) {
            return;
        } else if (account.getLastValidTimestamp() <= 0) {
            return;
        } else if (account.getPlugin() == null) {
            return;
        }
        final AccountType currentAccountType = account.getType();
        if (AccountType.UNKNOWN.equals(currentAccountType)) {
            // unknown account type should not trigger and upOrDowngrade events
            return;
        }
        final String lastKnownAccountType = account.getStringProperty(lastKnownAccountTypeProperty, currentAccountType.name());
        account.setProperty(lastKnownAccountTypeProperty, currentAccountType.name());
        final long currentValidUntilTimeStamp = account.getValidPremiumUntil();
        final boolean hasLastKnownPremiumValidUntilTimeStamp = account.hasProperty(lastKnownValidUntilTimeStampProperty);
        final long lastKnownPremiumValidUntilTimeStamp = account.getLongProperty(lastKnownValidUntilTimeStampProperty, currentValidUntilTimeStamp);
        final boolean isPremiumAccount = AccountType.PREMIUM.equals(currentAccountType) || AccountType.LIFETIME.equals(currentAccountType);
        final boolean wasPremiumAccount = AccountType.PREMIUM.name().equals(lastKnownAccountType) || AccountType.LIFETIME.name().equals(lastKnownAccountType);
        final boolean isPremiumUpgraded = isPremiumAccount && !wasPremiumAccount;
        final boolean isPremiumDowngraded = !isPremiumAccount && wasPremiumAccount;
        final boolean isLimitedRenewal = (currentValidUntilTimeStamp > lastKnownPremiumValidUntilTimeStamp && (currentValidUntilTimeStamp - lastKnownPremiumValidUntilTimeStamp) > minimumExtendTime);
        final boolean isPremiumLimitedRenewal = isPremiumAccount && isLimitedRenewal;
        final boolean isUnlimitedRenewal = currentValidUntilTimeStamp != lastKnownPremiumValidUntilTimeStamp && currentValidUntilTimeStamp == -1;
        final boolean isPremiumUnlimitedRenewal = isPremiumAccount && isUnlimitedRenewal;
        if (isPremiumLimitedRenewal || isPremiumUnlimitedRenewal) {
            account.setProperty(lastKnownValidUntilTimeStampProperty, currentValidUntilTimeStamp);
        } else if (isPremiumAccount && !hasLastKnownPremiumValidUntilTimeStamp) {
            account.setProperty(lastKnownValidUntilTimeStampProperty, currentValidUntilTimeStamp);
        }
        final long renewalDuration;
        if (currentValidUntilTimeStamp > 0) {
            if (lastKnownPremiumValidUntilTimeStamp > 0 && (lastKnownPremiumValidUntilTimeStamp - System.currentTimeMillis() > 0)) {
                renewalDuration = currentValidUntilTimeStamp - lastKnownPremiumValidUntilTimeStamp;
            } else {
                renewalDuration = currentValidUntilTimeStamp - System.currentTimeMillis();
            }
        } else {
            renewalDuration = 0;
        }
        if (isPremiumDowngraded || isPremiumUpgraded || isPremiumLimitedRenewal || isPremiumUnlimitedRenewal) {
            getEventSender().fireEvent(new AccountUpOrDowngradeEvent(this, account) {
                @Override
                public boolean isPremiumAccount() {
                    return isPremiumAccount;
                }

                @Override
                public boolean isPremiumUpgraded() {
                    return isPremiumUpgraded;
                }

                @Override
                public boolean isPremiumDowngraded() {
                    return isPremiumDowngraded;
                }

                @Override
                public boolean isPremiumLimitedRenewal() {
                    return isPremiumLimitedRenewal;
                }

                @Override
                public boolean isPremiumUnlimitedRenewal() {
                    return isPremiumUnlimitedRenewal;
                }

                @Override
                public long getPremiumRenewalDuration() {
                    return renewalDuration;
                }

                @Override
                public long getExpireTimeStamp() {
                    return currentValidUntilTimeStamp;
                }
            });
        }
    }

    public void checkPluginUpdates() {
        /**
         * TODO: assignPlugin(see loadAccounts)
         */
        final PluginFinder pluginFinder = new PluginFinder();
        for (final Account account : list(null)) {
            final AccountInfo accountInfo = account.getAccountInfo();
            if (account.getPlugin() != null && account.isMultiHost() && accountInfo != null) {
                try {
                    accountInfo.setMultiHostSupportV2(account.getPlugin(), accountInfo.getMultiHostSupportV2(), pluginFinder);
                    getEventSender().fireEvent(new AccountControllerEvent(this, AccountControllerEvent.Types.ACCOUNT_CHECKED, account));
                } catch (final Throwable e) {
                    LogController.CL().log(e);
                }
            }
        }
    }

    public static AccountController getInstance() {
        return INSTANCE;
    }

    private synchronized HashMap<String, List<Account>> loadAccounts(AccountSettings config) {
        HashMap<String, ArrayList<AccountData>> dat = config.getAccounts();
        if (dat == null) {
            dat = new HashMap<String, ArrayList<AccountData>>();
        }
        final PluginFinder pluginFinder = new PluginFinder();
        final HashMap<String, List<Account>> ret = new HashMap<String, List<Account>>();
        for (Iterator<Entry<String, ArrayList<AccountData>>> it = dat.entrySet().iterator(); it.hasNext();) {
            final Entry<String, ArrayList<AccountData>> next = it.next();
            if (next.getValue().size() > 0) {
                final String nextHost = next.getKey().toLowerCase(Locale.ENGLISH);
                for (final AccountData ad : next.getValue()) {
                    final Account acc = ad.toAccount();
                    acc.setHoster(nextHost);
                    final PluginForHost plugin = pluginFinder.assignPlugin(acc, true);
                    final String accountHost;
                    if (plugin != null) {
                        accountHost = plugin.getHost();
                        acc.setPlugin(plugin);
                    } else {
                        accountHost = nextHost;
                        acc.setPlugin(null);
                    }
                    final AccountInfo ai = acc.getAccountInfo();
                    if (ai != null) {
                        if (plugin != null) {
                            try {
                                ai.setMultiHostSupportV2(plugin, ai.getMultiHostSupportV2(), pluginFinder);
                            } catch (final Throwable e) {
                                LogController.CL().log(e);
                            }
                        } else {
                            ai.setMultiHostSupportV2(null, null, null);
                        }
                    }
                    List<Account> accs = ret.get(accountHost);
                    if (accs == null) {
                        accs = new ArrayList<Account>();
                        ret.put(accountHost, accs);
                    }
                    accs.add(acc);
                }
            }
        }
        return ret;
    }

    @Deprecated
    public void addAccount(final PluginForHost pluginForHost, final Account account) {
        account.setHoster(pluginForHost.getHost());
        addAccount(account);
    }

    /* returns a list of all available accounts for given host */
    public ArrayList<Account> list(String host) {
        if (host == null) {
            return listAccounts(null);
        } else {
            return listAccounts(new AccountFilter(host));
        }
    }

    public ArrayList<Account> listAccounts(final AccountFilter filter) {
        final ArrayList<Account> ret = new ArrayList<Account>();
        synchronized (AccountController.this) {
            final Integer maxResults = filter != null ? filter.getMaxResultsNum() : null;
            Set<String> hostPreFilter = null;
            if (filter != null) {
                hostPreFilter = new HashSet<String>();
                final List<String> hosts = filter.getHosts();
                if (hosts != null) {
                    for (String host : hosts) {
                        hostPreFilter.add(StringUtils.toLowerCaseOrNull(host));
                    }
                }
                if (hostPreFilter.size() == 0) {
                    hostPreFilter = null;
                }
            }
            accountsLoop: for (final Map.Entry<String, List<Account>> accounts : ACCOUNTS.entrySet()) {
                if (accounts.getValue() == null) {
                    continue;
                }
                if (hostPreFilter != null && !hostPreFilter.contains(accounts.getKey())) {
                    continue;
                }
                for (final Account account : accounts.getValue()) {
                    if (account.getPlugin() == null) {
                        continue;
                    }
                    if (filter != null && !filter.matches(account)) {
                        continue;
                    }
                    ret.add(account);
                    if (maxResults != null && ret.size() >= maxResults.intValue()) {
                        break accountsLoop;
                    }
                }
            }
        }
        return ret;
    }

    /* returns a list of all available accounts */
    public List<Account> list() {
        return listAccounts(null);
    }

    public int getAccountsSize(final String host) {
        if (host == null) {
            return 0;
        }
        synchronized (AccountController.this) {
            final List<Account> ret = ACCOUNTS.get(host.toLowerCase(Locale.ENGLISH));
            if (ret != null && ret.size() > 0) {
                return ret.size();
            }
            return 0;
        }
    }

    public void addAccount(final Account account) {
        addAccount(account, true);
    }

    /**
     * Adds account to list of accounts. If account already exists, it will be enabled (and checked) in case it is currently disabled. </br>
     * If password of new account differs from existing accounts' password, existing accounts' password will be updated.
     */
    public Account addAccount(final Account account, final boolean forceAccountCheckOnPropertyChange) {
        if (account == null) {
            return null;
        } else if (account.getPlugin() == null) {
            new PluginFinder().assignPlugin(account, true);
        } else if (account.getHoster() == null) {
            return null;
        }
        Account existingAccount = null;
        synchronized (AccountController.this) {
            final String host = account.getHoster().toLowerCase(Locale.ENGLISH);
            List<Account> accs = ACCOUNTS.get(host);
            if (accs == null) {
                accs = new ArrayList<Account>();
                ACCOUNTS.put(host, accs);
            }
            final String username = StringUtils.isNotEmpty(account.getUser()) ? account.getUser() : null;
            for (final Account acc : accs) {
                if (acc == account) {
                    existingAccount = acc;
                    break;
                } else if (username != null && StringUtils.equals(acc.getUser(), username) && StringUtils.equals(acc.getPass(), account.getPass())) {
                    /* Check for same user + pw. */
                    existingAccount = acc;
                    break;
                }
            }
            if (existingAccount == null && username != null) {
                /*
                 * Check for same username only --> Username is supposed to be unique so this should also be a safe method of finding an
                 * existing same account.
                 */
                for (final Account acc : accs) {
                    if (StringUtils.equals(acc.getUser(), username)) {
                        existingAccount = acc;
                        break;
                    }
                }
            }
            if (existingAccount == null) {
                /* No existing account found -> Add this account to list of accounts. */
                account.setAccountController(this);
                accs.add(account);
            }
        }
        if (existingAccount != null) {
            final AccountError newError = account.getError();
            final AccountInfo newAccountInfo = account.getAccountInfo();
            /* Existing account found -> Update password of existing account */
            if (!StringUtils.equals(existingAccount.getPass(), account.getPass())) {
                // at this stage the logins of account are correct
                existingAccount.setPass(account.getPass(), false);
            }
            if (newError == null) {
                // no error -> replace
                existingAccount.setError(null, -1, null, false);
                existingAccount.setAccountInfo(newAccountInfo);
                existingAccount.setProperties(account.getProperties());
            } else {
                // error -> merge
                existingAccount.setError(newError, -1, account.getErrorString(), false);
                existingAccount.setTmpDisabledTimeout(account.getTmpDisabledTimeout());
                if (newAccountInfo != null) {
                    final AccountInfo oldAccountInfo = existingAccount.getAccountInfo();
                    if (oldAccountInfo != null) {
                        for (Map.Entry<String, Object> entry : oldAccountInfo.getProperties().entrySet()) {
                            newAccountInfo.setProperty(entry.getKey(), entry.getValue());
                        }
                    }
                    existingAccount.setAccountInfo(newAccountInfo);
                }
                for (Map.Entry<String, Object> entry : account.getProperties().entrySet()) {
                    existingAccount.setProperty(entry.getKey(), entry.getValue());
                }
            }
            existingAccount.setEnabled(true, forceAccountCheckOnPropertyChange);
            getEventSender().fireEvent(new AccountControllerEvent(this, AccountControllerEvent.Types.ACCOUNT_CHECKED, existingAccount));
            return existingAccount;
        } else {
            getEventSender().fireEvent(new AccountControllerEvent(this, AccountControllerEvent.Types.ADDED, account));
            return account;
        }
    }

    public boolean removeAccount(final Account account) {
        if (account == null) {
            return false;
        }
        /* remove reference to AccountController */
        account.setAccountController(null);
        synchronized (AccountController.this) {
            final String host = account.getHoster().toLowerCase(Locale.ENGLISH);
            final List<Account> accs = ACCOUNTS.get(host);
            if (accs == null || !accs.remove(account)) {
                return false;
            }
            if (accs.size() == 0) {
                ACCOUNTS.remove(host);
            }
        }
        this.broadcaster.fireEvent(new AccountControllerEvent(this, AccountControllerEvent.Types.REMOVED, account));
        return true;
    }

    public void onAccountControllerEvent(final AccountControllerEvent event) {
        final Account acc = event.getAccount();
        delayedSaver.resetAndStart();
        final PluginEnvironment pluginEnvironment = Plugin.PluginEnvironment.getPluginEnvironment();
        boolean forceRecheck = false;
        switch (event.getType()) {
        case ADDED:
            org.jdownloader.settings.staticreferences.CFG_GENERAL.USE_AVAILABLE_ACCOUNTS.setValue(true);
            updateInternalMultiHosterMap(acc, acc.getAccountInfo());
            break;
        case ACCOUNT_PROPERTY_UPDATE:
            final AccountProperty propertyChange = ((AccountPropertyChangedEvent) event).getProperty();
            switch (propertyChange.getProperty()) {
            case ENABLED:
                if (Boolean.FALSE.equals(propertyChange.getValue())) {
                    return;
                }
                final AccountInfo ai = acc.getAccountInfo();
                if (!acc.isValid() || (ai != null && ai.isExpired())) {
                    acc.setUpdateTime(0);
                }
                forceRecheck = true;
                break;
            case ERROR:
                if (propertyChange.getValue() != null) {
                    return;
                }
                forceRecheck = true;
                break;
            case PASSWORD:
            case USERNAME:
                if (PluginEnvironment.UNKNOWN.equals(pluginEnvironment)) {
                    // username/password has changed outside of a Plugin
                    // *reset* Account and dump all previous stored information (AccountInfo, Properties...)
                    acc.setAccountInfo(null);
                    acc.setProperties(null);
                    acc.setUpdateTime(0);
                    forceRecheck = true;
                }
                break;
            }
            break;
        case ACCOUNT_CHECKED:
            updateInternalMultiHosterMap(acc, acc.getAccountInfo());
            return;
        case REMOVED:
            updateInternalMultiHosterMap(acc, null);
            return;
        }
        if (acc == null || acc.isEnabled() == false) {
            return;
        } else if (PluginEnvironment.ACCOUNT_CHECK.equals(pluginEnvironment)) {
            return;
        }
        AccountChecker.getInstance().check(acc, forceRecheck);
    }

    private void refreshAccountStats() {
        synchronized (AccountController.this) {
            for (final List<Account> accounts : ACCOUNTS.values()) {
                if (accounts == null) {
                    return;
                }
                for (final Account acc : accounts) {
                    if (acc.getPlugin() != null && acc.isEnabled() && acc.isValid() && acc.refreshTimeoutReached()) {
                        /*
                         * we do not force update here, the internal timeout will make sure accounts get fresh checked from time to time
                         */
                        AccountChecker.getInstance().check(acc, false);
                    }
                }
            }
        }
    }

    public Account getValidAccount(final PluginForHost pluginForHost) {
        return getValidAccount(pluginForHost.getHost());
    }

    /** Returns first account that is enabled and valid. */
    public Account getValidAccount(final String host) {
        final List<Account> ret = getValidAccounts(host);
        if (ret != null && ret.size() > 0) {
            return ret.get(0);
        } else {
            return null;
        }
    }

    public ArrayList<Account> getValidAccounts(final String host) {
        if (StringUtils.isEmpty(host)) {
            return null;
        }
        final Thread currentThread = Thread.currentThread();
        if (currentThread instanceof SingleDownloadController) {
            // requestFileInformation must use the account from DownloadLinkCandidate of SingleDownloadController
            final SingleDownloadController controller = (SingleDownloadController) currentThread;
            final Account acc = controller.getAccount();
            if (acc == null) {
                return null;
            } else if (StringUtils.equals(acc.getHosterByPlugin(), host)) {
                final ArrayList<Account> ret = new ArrayList<Account>();
                ret.add(acc);
                return ret;
            }
        }
        final ArrayList<Account> ret = listAccounts(new AccountFilter(host).setEnabled(true).setValid(true).setTemporarilyDisabled(false));
        if (ret == null || ret.size() == 0) {
            return null;
        } else {
            return ret;
        }
    }

    @Deprecated
    public List<Account> getMultiHostAccounts(final String host) {
        if (host == null) {
            return null;
        }
        synchronized (AccountController.this) {
            final Map<Account, Object> list = MULTIHOSTER_ACCOUNTS.get(host.toLowerCase(Locale.ENGLISH));
            if (list != null && list.size() > 0) {
                return new ArrayList<Account>(list.keySet());
            }
        }
        return null;
    }

    public static String createFullBuyPremiumUrl(String buyPremiumUrl, String id) {
        final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("ddMMyyyy'_'HHmm");
        id = id + "/" + simpleDateFormat.format(new Date());
        return "https://update3.jdownloader.org/jdserv/BuyPremiumInterface/redirect?" + Encoding.urlEncode(buyPremiumUrl) + "&" + Encoding.urlEncode(id);
    }

    public static String buildAfflink(final LazyHostPlugin lazyHostPlugin, PluginForHost plugin, final String source) {
        String ret = null;
        if (plugin != null) {
            ret = plugin.getBuyPremiumUrl();
        } else if (lazyHostPlugin != null) {
            try {
                plugin = lazyHostPlugin.getPrototype(null);
                ret = plugin.getBuyPremiumUrl();
            } catch (UpdateRequiredClassNotFoundException ignore) {
            }
        }
        try {
            URLHelper.verifyURL(new URL(ret));
        } catch (MalformedURLException e) {
            ret = null;
        }
        if (StringUtils.isEmpty(ret) && lazyHostPlugin != null && lazyHostPlugin.getHost().contains(".")) {
            ret = "https://" + lazyHostPlugin.getHost();
        }
        if (StringUtils.isEmpty(ret)) {
            return null;
        } else {
            return AccountController.createFullBuyPremiumUrl(ret, source);
        }
    }

    public static void openAfflink(final LazyHostPlugin lazyHostPlugin, final PluginForHost plugin, final String source) {
        final String refURL = buildAfflink(lazyHostPlugin, plugin, source);
        if (refURL == null) {
            return;
        }
        CrossSystem.openURL(refURL);
    }

    @Override
    public boolean fireAccountPropertyChange(jd.plugins.AccountProperty propertyChange) {
        if (propertyChange.getAccount().isChecking()) {
            return false;
        }
        getEventSender().fireEvent(new AccountPropertyChangedEvent(propertyChange.getAccount(), propertyChange));
        return true;
    }

    private final String SLV                = "slv";
    private final String SLID               = "slid";
    private final String IMPORTED_TIMESTAMP = "im";

    public List<Account> importAccounts(File f) {
        /* TODO: add cleanup to avoid memleak */
        final AccountSettings cfg = JsonConfig.create(new File(f.getParent(), "org.jdownloader.settings.AccountSettings"), AccountSettings.class);
        final long timeStamp = System.currentTimeMillis();
        final HashMap<String, List<Account>> accounts = loadAccounts(cfg);
        final ArrayList<Account> added = new ArrayList<Account>();
        for (final Entry<String, List<Account>> es : accounts.entrySet()) {
            for (final Account ad : es.getValue()) {
                final Account acc = new Account(ad.getUser(), ad.getPass());
                acc.setHoster(ad.getHoster());
                acc.setProperty(IMPORTED_TIMESTAMP, timeStamp);
                acc.setProperty(SLID, cfg.getListID());
                acc.setProperty(SLV, cfg.getListVersion());
                addAccount(acc);
                added.add(ad);
            }
        }
        return added;
    }
}