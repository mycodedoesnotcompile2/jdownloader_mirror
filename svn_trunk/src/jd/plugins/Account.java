//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
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
package jd.plugins;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import jd.config.Property;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.translate._JDT;

public class Account extends Property {
    private static final String VALID_UNTIL              = "VALID_UNTIL";
    private static final String ACCOUNT_TYPE             = "ACCOUNT_TYPE";
    private static final String LATEST_VALID_TIMESTAMP   = "LATEST_VALID_TIMESTAMP";
    public static final String  IS_MULTI_HOSTER_ACCOUNT  = "IS_MULTI_HOSTER_ACCOUNT";
    private static final long   serialVersionUID         = -7578649066389032068L;
    private String              user;
    private String              pass;
    private boolean             enabled                  = true;
    private boolean             concurrentUsePossible    = true;
    public static final String  PROPERTY_REFRESH_TIMEOUT = "PROPERTY_REFRESH_TIMEOUT";
    private static final String COOKIE_STORAGE           = "COOKIE_STORAGE";

    public boolean isConcurrentUsePossible() {
        return concurrentUsePossible;
    }

    private static final String OBJECT_STORAGE = "OBJECT_STORAGE";

    /** Returns unique hash generated via username + password. */
    public String getAccountFootprintString() {
        return Hash.getSHA256(getUser() + ":" + getPass());
    }

    public synchronized void storeObject(final String storageID, final Object object) {
        setProperty(OBJECT_STORAGE + ".validation." + storageID, getAccountFootprintString());
        setProperty(OBJECT_STORAGE + ".object." + storageID, JSonStorage.serializeToJson(object));
        setProperty(OBJECT_STORAGE + ".ts." + storageID, System.currentTimeMillis());
    }

    public synchronized void clearObject(final String storageID) {
        removeProperty(OBJECT_STORAGE + ".validation." + storageID);
        removeProperty(OBJECT_STORAGE + ".object." + storageID);
        removeProperty(OBJECT_STORAGE + ".ts." + storageID);
    }

    public synchronized <T> T restoreObject(final String storageID, final TypeRef<T> type) {
        final boolean containsObject = hasProperty(OBJECT_STORAGE + ".object." + storageID);
        if (containsObject) {
            if (StringUtils.equals(getStringProperty(OBJECT_STORAGE + ".validation." + storageID), getAccountFootprintString())) {
                return JSonStorage.restoreFromString(getStringProperty(OBJECT_STORAGE + ".object." + storageID), type, null);
            } else {
                clearObject(storageID);
            }
        }
        return null;
    }

    @Deprecated
    // will be removed by Jiaz
    public void setTempDisabled(final boolean tempDisabled) {
        if (tempDisabled) {
            setError(AccountError.TEMP_DISABLED, -1, null);
        } else {
            if (AccountError.TEMP_DISABLED.equals(getError())) {
                setError(null, -1, null);
            }
        }
    }

    private DomainInfo domainInfo;

    public DomainInfo getDomainInfo() {
        DomainInfo domainInfo = this.domainInfo;
        if (domainInfo == null) {
            domainInfo = DomainInfo.getInstance(getHosterByPlugin(true));
            this.domainInfo = domainInfo;
        }
        return domainInfo;
    }

    public final AccountTrafficView getAccountTrafficView() {
        final PluginForHost plugin = getPlugin();
        if (plugin == null) {
            final AccountInfo accountInfo = getAccountInfo();
            if (accountInfo != null) {
                return accountInfo;
            } else {
                return null;
            }
        } else {
            return plugin.getAccountTrafficView(this);
        }
    }

    public synchronized long getObjectTimeStamp(final String storageID) {
        return getLongProperty(OBJECT_STORAGE + ".ts." + storageID, -1);
    }

    public synchronized void saveCookies(final Cookies cookies, final String ID) {
        final String validation = getAccountFootprintString();
        /*
         * Do not cache antiddos cookies, this is job of the antiddos module, otherwise it can and will cause conflicts!
         */
        /* TODO: Maybe always save ALL cookies but add ability to NOT load Cloudflare cookies - this might be the easier way ... */
        final List<CookieStorable> cookieStorables = getListOfCookieStorablesWithoutAntiDdosCookies(cookies);
        setProperty(COOKIE_STORAGE, validation);
        final String COOKIE_STORAGE_ID = COOKIE_STORAGE + ":" + ID;
        setProperty(COOKIE_STORAGE_ID, new SimpleMapper().setPrettyPrintEnabled(false).objectToString(cookieStorables));
        final String COOKIE_STORAGE_TIMESTAMP_ID = COOKIE_STORAGE + ":TS:" + ID;
        final long ret = System.currentTimeMillis();
        setProperty(COOKIE_STORAGE_TIMESTAMP_ID, ret);
        // TODO: add support for UserAgent which can be given e.g. in cookies exported via browser addon "FlagCookies"
    }

    /** TODO: This might not be the right place for this satatic method! */
    public static List<CookieStorable> getListOfCookieStorables(final Cookies cookies) {
        if (cookies == null) {
            return null;
        }
        final List<CookieStorable> cookieStorables = new ArrayList<CookieStorable>();
        for (final Cookie cookie : cookies.getCookies()) {
            if (cookie.getKey() != null && !cookie.isExpired()) {
                cookieStorables.add(new CookieStorable(cookie));
            }
        }
        return cookieStorables;
    }

    /**
     * Returns Array List of CookieStorable of all cookies that do NOT match
     * org.jdownloader.plugins.components.antiDDoSForHost.antiDDoSCookiePattern!
     */
    /** TODO: This might not be the right place for this static method! */
    public static List<CookieStorable> getListOfCookieStorablesWithoutAntiDdosCookies(final Cookies cookies) {
        if (cookies == null) {
            return null;
        }
        final List<CookieStorable> cookieStorables = new ArrayList<CookieStorable>();
        final String antiddosCookies = org.jdownloader.plugins.components.antiDDoSForHost.antiDDoSCookiePattern;
        for (final Cookie cookie : cookies.getCookies()) {
            if (cookie.getKey() != null && !cookie.getKey().matches(antiddosCookies) && !cookie.isExpired()) {
                cookieStorables.add(new CookieStorable(cookie));
            }
        }
        return cookieStorables;
    }

    /** Deletes cookies and related information for given ID. */
    public synchronized void clearCookies(final String ID) {
        final String COOKIE_STORAGE_ID = COOKIE_STORAGE + ":" + ID;
        removeProperty(COOKIE_STORAGE_ID);
        final String COOKIE_STORAGE_TIMESTAMP_ID = COOKIE_STORAGE + ":TS:" + ID;
        removeProperty(COOKIE_STORAGE_TIMESTAMP_ID); // TODO: add support for UserAgent, eg dropbox/filestore.to
    }

    public synchronized long getCookiesTimeStamp(final String ID) {
        final String COOKIE_STORAGE_TIMESTAMP_ID = COOKIE_STORAGE + ":TS:" + ID;
        return getLongProperty(COOKIE_STORAGE_TIMESTAMP_ID, -1);
    }

    /** Returns list of stored cookies WITHOUT additional information such as User-Agent header. */
    public synchronized Cookies loadCookies(final String ID) {
        final String validation = getAccountFootprintString();
        if (StringUtils.equals(getStringProperty(COOKIE_STORAGE), validation)) {
            final String COOKIE_STORAGE_ID = COOKIE_STORAGE + ":" + ID;
            // TODO: add support for UserAgent, eg dropbox/filestore.to
            final String cookieStorables = getStringProperty(COOKIE_STORAGE_ID);
            if (StringUtils.isNotEmpty(cookieStorables)) {
                try {
                    final List<CookieStorable> cookies = JSonStorage.restoreFromString(cookieStorables, new TypeRef<ArrayList<CookieStorable>>() {
                    }, null);
                    final Cookies ret = new Cookies();
                    for (final CookieStorable storable : cookies) {
                        final Cookie cookie = storable._restore();
                        if (!cookie.isExpired()) {
                            ret.add(cookie);
                        }
                    }
                    return ret;
                } catch (Throwable e) {
                    LogController.CL().log(e);
                }
            }
        }
        clearCookies(ID);
        return null;
    }

    /** Returns cookies provided by user, typically put into the "Password" field when adding an account. */
    public Cookies loadUserCookies() {
        return loadUserCookies(true);
    }

    public Cookies loadUserCookies(final boolean loadUserAgent) {
        final Cookies ret = Cookies.parseCookiesFromJsonString(this.getPass(), LogController.getRebirthLogger());
        if (ret != null && !loadUserAgent) {
            ret.setUserAgent(null);
        }
        return ret;
    }

    /**
     * true = Multiple accounts of this host can be used at the same time <br/>
     * false = Multiple accounts of this host can NOT be used at the same time. <br/>
     * For most hosts, this should be set to false for free accounts as the traffic limit of those sits on current IP + account which means
     * that if a traffic-limit of one free account is reached, all accounts will be out of traffic!
     */
    public void setConcurrentUsePossible(boolean concurrentUsePossible) {
        this.concurrentUsePossible = concurrentUsePossible;
    }

    private final AtomicLong tmpDisabledTimeout = new AtomicLong(-1);

    public long getTmpDisabledTimeout() {
        return Math.max(-1, tmpDisabledTimeout.get());
    }

    private transient UniqueAlltimeID id            = new UniqueAlltimeID();
    /* keep for comp. reasons */
    private String                    hoster        = null;
    private List<String>              hosterHistory = null;

    public List<String> getHosterHistory() {
        return hosterHistory;
    }

    public void setHosterHistory(List<String> hosterHistory) {
        if (hosterHistory != null && hosterHistory.size() > 0) {
            this.hosterHistory = new CopyOnWriteArrayList<String>(hosterHistory);
        } else {
            this.hosterHistory = null;
        }
    }

    private AccountInfo                     accinfo               = null;
    private long                            updatetime            = 0;
    private int                             maxDownloads          = 0;
    private transient AccountController     ac                    = null;
    private transient PluginForHost         plugin                = null;
    private transient boolean               isMulti               = false;
    private transient boolean               isMultiPlugin         = false;
    private transient boolean               isCaptchaSolverPlugin = false;
    private transient volatile AccountError error;
    private transient volatile String       errorString;

    public PluginForHost getPlugin() {
        return plugin;
    }

    public void setPlugin(final PluginForHost plugin) {
        this.plugin = plugin;
        if (plugin != null) {
            isMultiPlugin = plugin.isHandlingMultipleHosts();
            isCaptchaSolverPlugin = plugin.hasFeature(FEATURE.CAPTCHA_SOLVER);
        } else {
            isMultiPlugin = false;
        }
    }

    public boolean isMultiPlugin() {
        return isMultiPlugin;
    }

    public boolean isCaptchaSolverPlugin() {
        return isCaptchaSolverPlugin;
    }

    /**
     * Set this to true to indicate that changing the IP address will also reset this accounts' limits. </br> Most of all services will
     * store the limits on account (+ IP) but some will only rely on the IP thus allowing users to reset account limits by changing their
     * IP.
     */
    public void setAllowReconnectToResetLimits(final boolean b) {
        /* 2022-07-19: TODO: Dummy function, see: https://svn.jdownloader.org/issues/87351 */
    }

    /**
     *
     * @param string
     * @return
     */
    private static final String trim(final String string) {
        // NOTE: don't use String.trim() because users might want to use spaces at the beginning/end for passwords!
        return (string == null) ? null : StringUtils.removeBOM(string);
    }

    public void setAccountController(AccountController ac) {
        this.ac = ac;
    }

    public AccountController getAccountController() {
        return ac;
    }

    /**
     *
     * @param user
     * @param pass
     */
    public Account(final String user, final String pass) {
        this.user = trim(user);
        this.pass = trim(pass);
    }

    public int getMaxSimultanDownloads() {
        return maxDownloads;
    }

    /**
     * -1 = unlimited, 0 = use deprecated getMaxSimultanPremiumDownloadNum/getMaxSimultanFreeDownloadNum,>1 = use this
     *
     * @since JD2
     */
    public void setMaxSimultanDownloads(int max) {
        if (max < 0) {
            maxDownloads = -1;
        } else {
            maxDownloads = max;
        }
    }

    public String getPass() {
        return pass = updateAccountPassword(pass);
    }

    protected String updateAccountPassword(final String password) {
        final PluginForHost plugin = getPlugin();
        if (plugin != null && password != null) {
            return plugin.updateAccountPassword(this, password);
        } else {
            return password;
        }
    }

    public boolean isValid() {
        final AccountError lerror = getError();
        return lerror == null || AccountError.TEMP_DISABLED.equals(lerror);
    }

    public long getLastValidTimestamp() {
        return getLongProperty(LATEST_VALID_TIMESTAMP, -1);
    }

    /**
     * @Deprecated Use setError
     * @param b
     */
    @Deprecated
    public void setValid(final boolean b) {
        if (b) {
            if (getError() == AccountError.INVALID) {
                setError(null, -1, null);
            }
        } else {
            setError(AccountError.INVALID, -1, null);
        }
    }

    /** Returns last update time for current JD runtime session. */
    public long lastUpdateTime() {
        return updatetime;
    }

    public void setUpdateTime(final long l) {
        updatetime = l;
    }

    public String getHoster() {
        return hoster;
    }

    public void setHoster(final String h) {
        hoster = h;
    }

    public String getHosterByPlugin() {
        return getHosterByPlugin(false);
    }

    public String getHosterByPlugin(final boolean includeSubdomain) {
        final PluginForHost plugin = this.getPlugin();
        if (plugin != null && isMultiPlugin()) {
            final String ret = plugin.getHost(null, this, includeSubdomain);
            if (ret != null) {
                return ret;
            }
        }
        return hoster;
    }

    private final AtomicBoolean checking = new AtomicBoolean(false);

    public void setChecking(boolean b) {
        checking.set(b);
    }

    public boolean isChecking() {
        return checking.get();
    }

    public AccountInfo getAccountInfo() {
        return accinfo;
    }

    public void setAccountInfo(final AccountInfo info) {
        accinfo = info;
        if (info == null) {
            return;
        }
        info.setAccount(this);
        if (AccountType.PREMIUM.equals(getType()) && !info.isExpired() && info.getValidUntil() > 0) {
            setValidPremiumUntil(info.getValidUntil());
        }
        // this sets default status message based on account type
        // TODO: translation?
        if (getType() != null && info.getStatus() == null) {
            String output = getType().toString();
            output = output.substring(0, 1) + output.substring(1).toLowerCase(Locale.ENGLISH) + " Account";
            info.setStatus(output);
        }
    }

    /**
     * The expire Date of an premium account. if the account is not a premium account any more, this timestamp points to the last valid
     * expire date. it can be used to check when an account has expired
     *
     * @param validUntil
     */
    private void setValidPremiumUntil(long validUntil) {
        setProperty(VALID_UNTIL, validUntil);
    }

    /**
     * this method returns for how long this account will be (or has been) a premium account
     *
     * The expire Date of an premium account. if the account is not a premium account any more, this timestamp points to the last valid
     * expire date. it can be used to check when an account has expired
     *
     * @param validUntil
     *
     * @return
     */
    public long getValidPremiumUntil() {
        final AccountInfo info = getAccountInfo();
        long ret = -1;
        if (info != null) {
            if (AccountType.PREMIUM.equals(getType()) && !info.isExpired()) {
                ret = info.getValidUntil();
            }
        }
        if (ret <= 0) {
            ret = getLongProperty(VALID_UNTIL, 0);
        }
        return ret;
    }

    public String getUser() {
        return user;
    }

    public boolean isEnabled() {
        return enabled;
    }

    private void readObject(final java.io.ObjectInputStream stream) throws java.io.IOException, ClassNotFoundException {
        /* nach dem deserialisieren sollen die transienten neu geholt werden */
        stream.defaultReadObject();
        setTmpDisabledTimeout(-1);
        isMulti = false;
        id = new UniqueAlltimeID();
        isMultiPlugin = false;
    }

    public UniqueAlltimeID getId() {
        return id;
    }

    public void setId(long id) {
        if (id > 0) {
            this.id = new UniqueAlltimeID(id);
        }
    }

    public boolean isTempDisabled() {
        return AccountError.TEMP_DISABLED.equals(getError());
    }

    public static enum AccountError {
        TEMP_DISABLED,
        EXPIRED,
        INVALID,
        PLUGIN_ERROR;
    }

    public void setError(final AccountError error, final long setTimeout, String errorString) {
        setError(error, setTimeout, errorString, true);
    }

    public void setError(final AccountError error, final long setTimeout, String errorString, final boolean forceAccountCheckOnChange) {
        if (error == null) {
            errorString = null;
        }
        if (getError() != error || !StringUtils.equals(this.getErrorString(), errorString)) {
            if (AccountError.TEMP_DISABLED.equals(error)) {
                final long timeout;
                if (setTimeout <= 0) {
                    timeout = System.currentTimeMillis() + CFG_GENERAL.CFG.getAccountTemporarilyDisabledDefaultTimeout();
                } else {
                    timeout = System.currentTimeMillis() + setTimeout;
                }
                setTmpDisabledTimeout(timeout);
            } else {
                setTmpDisabledTimeout(-1);
            }
            this.error = error;
            this.errorString = errorString;
            if (forceAccountCheckOnChange) {
                notifyUpdate(AccountProperty.Property.ERROR, error);
            }
        }
    }

    /**
     * @author raztoki
     * @since JD2
     * @see findAndSetNextDayAsTimeOut
     * @param br
     */
    public final void setNextDayAsTempTimeout(final Browser br) {
        setNextDayAsTempTimeout(br, "EEE, dd MMM yyyy HH:mm:ss z", -1, null);
    }

    /**
     * @author raztoki
     * @since JD2
     * @see findAndSetNextDayAsTimeOut
     * @param br
     * @param message
     */
    public final void setNextDayAsTempTimeout(final Browser br, final String message) {
        setNextDayAsTempTimeout(br, "EEE, dd MMM yyyy HH:mm:ss z", -1, message);
    }

    /**
     * @author raztoki
     * @since JD2
     * @see findAndSetNextDayAsTimeOut
     * @param br
     * @param failOverTime
     */
    public final void setNextDayAsTempTimeout(final Browser br, final long failOverTime) {
        setNextDayAsTempTimeout(br, "EEE, dd MMM yyyy HH:mm:ss z", failOverTime, null);
    }

    /**
     * When sites don't tell you when the daily traffic reset is, we can assume that it is on a new day. We can use server time date stamp
     * to determine this!, on the assumption that is when they do the reset! This method is required because, some sites do not have traffic
     * left statistics within fetchAccountInfo, which then re-enables download and continue this cycle.
     *
     * @author raztoki
     * @since JD2
     * @param br
     * @param formatter
     * @param failOverTime
     * @param errorString
     */
    public final void setNextDayAsTempTimeout(final Browser br, final String formatter, long failOverTime, final String errorString) {
        long result = -1;
        if (failOverTime <= 0) {
            // 1 hour default.
            failOverTime = 60 * 60 * 1000l;
        }
        if (br != null && br.getHttpConnection() != null) {
            long serverTime = -1;
            // lets use server time to determine time out value; we then need to adjust timeformatter reference +- time against server time
            final String dateString = br.getHttpConnection().getHeaderField("Date");
            if (dateString != null) {
                if (StringUtils.isNotEmpty(formatter)) {
                    serverTime = TimeFormatter.getMilliSeconds(dateString, formatter, Locale.ENGLISH);
                } else {
                    final Date date = TimeFormatter.parseDateString(dateString);
                    if (date != null) {
                        serverTime = date.getTime();
                    }
                }
                // server time.. is generally in GMT! @see http://tools.ietf.org/html/rfc2616#section-3.3
                final Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                c.setTime(new Date(serverTime));
                // plus one day!
                c.set(c.DAY_OF_YEAR, c.get(c.DAY_OF_YEAR) + 1);
                c.set(c.HOUR_OF_DAY, 0);
                // offset before check account can trigger fetch account info.
                c.set(c.MINUTE, 1);
                c.set(c.SECOND, 0);
                final long stTomorrow = c.getTimeInMillis();
                // difference in server time
                final long stDifference = stTomorrow - serverTime;
                // user time
                final long ut = System.currentTimeMillis();
                // adjustment to user time!
                result = ut + stDifference;
            }
        }
        final long timeout = (result > 0 ? result : failOverTime);
        setError(AccountError.TEMP_DISABLED, timeout, errorString);
    }

    public void setTmpDisabledTimeout(long tmpDisabledTimeout) {
        this.tmpDisabledTimeout.set(tmpDisabledTimeout);
    }

    public AccountError getError() {
        final AccountError error = this.error;
        if (AccountError.TEMP_DISABLED.equals(error)) {
            final Long disabledTimeStamp = getTmpDisabledTimeout();
            if (disabledTimeStamp.longValue() < 0) {
                return null;
            } else if (System.currentTimeMillis() >= disabledTimeStamp.longValue()) {
                if (tmpDisabledTimeout.compareAndSet(disabledTimeStamp, -1)) {
                    // workaround to avoid deadlock, get method should not cause change of internal state
                    new Thread("notifyUpdate:" + getUser()) {
                        {
                            setDaemon(true);
                        }

                        public void run() {
                            notifyUpdate(AccountProperty.Property.ERROR, null);
                        };
                    }.start();
                }
                return null;
            }
        }
        return error;
    }

    public String getErrorString() {
        return errorString;
    }

    /** Enabled/disables account and checks account-status if new enabled status != old enabled status. */
    public void setEnabled(final boolean enabled) {
        setEnabled(enabled, true);
    }

    public void setEnabled(final boolean enabled, final boolean forceAccountCheckOnChange) {
        if (this.enabled != enabled) {
            this.enabled = enabled;
            if (forceAccountCheckOnChange) {
                notifyUpdate(AccountProperty.Property.ENABLED, enabled);
            }
        }
    }

    private static final long DEFAULT_REFRESH_TIMEOUT = 30 * 60 * 1000l;
    private static final long MIN_REFRESH_TIMEOUT     = 5 * 60 * 1000l;

    /**
     * In which interval (milliseconds) will this account get checked? Min. = 5 minutes, default = 30 minutes.
     */
    public long getRefreshTimeout() {
        // Get the configured timeout, defaulting to DEFAULT_REFRESH_TIMEOUT if null or invalid
        long timeout = this.getLongProperty(PROPERTY_REFRESH_TIMEOUT, DEFAULT_REFRESH_TIMEOUT);
        // Ensure timeout is at least MIN_REFRESH_TIMEOUT
        timeout = Math.max(MIN_REFRESH_TIMEOUT, timeout <= 0 ? DEFAULT_REFRESH_TIMEOUT : timeout);
        final AccountInfo ai = this.getAccountInfo();
        if (ai == null) {
            return timeout;
        } else if (ai.isExpired()) {
            return timeout;
        }
        final long validUntil = ai.getValidUntil();
        if (validUntil < 0) {
            return timeout;
        }
        // Calculate remaining valid time
        final long timeValid = validUntil - System.currentTimeMillis();
        // Return the minimum of timeout and time valid
        return Math.min(timeout, Math.max(timeValid, 0));
    }

    /** In which interval (milliseconds) will this account get checked? Min. = 5 minutes, default = 30 minutes. */
    public void setRefreshTimeout(long refresh_timeout) {
        this.setProperty(PROPERTY_REFRESH_TIMEOUT, refresh_timeout);
    }

    public boolean refreshTimeoutReached() {
        if (updatetime <= 0) {
            /* Account has never been checked so far. */
            return true;
        }
        return System.currentTimeMillis() - updatetime >= getRefreshTimeout();
    }

    public static interface AccountPropertyChangeHandler {
        boolean fireAccountPropertyChange(AccountProperty property);
    }

    private transient volatile AccountPropertyChangeHandler notifyHandler = null;

    public void setNotifyHandler(AccountPropertyChangeHandler notifyHandler) {
        this.notifyHandler = notifyHandler;
    }

    private void notifyUpdate(AccountProperty.Property property, Object value) {
        AccountPropertyChangeHandler notify = notifyHandler;
        boolean notifyController = true;
        AccountProperty event = null;
        if (notify != null) {
            event = new AccountProperty(this, property, value);
            notifyController = notify.fireAccountPropertyChange(event);
        }
        notify = getAccountController();
        if (notify != null && notifyController) {
            if (event == null) {
                event = new AccountProperty(this, property, value);
            }
            notifyController = notify.fireAccountPropertyChange(event);
        }
    }

    /** Sets new password and force checks account if password differs from previously set password. */
    public void setPass(final String newPass) {
        setPass(newPass, true);
    }

    public void setPass(String newPass, final boolean forceAccountCheckOnChange) {
        newPass = updateAccountPassword(trim(newPass));
        if (!StringUtils.equals(this.pass, newPass)) {
            this.pass = newPass;
            if (forceAccountCheckOnChange) {
                notifyUpdate(AccountProperty.Property.PASSWORD, newPass);
            }
        }
    }

    public void setUser(String newUser) {
        newUser = trim(newUser);
        if (!StringUtils.equals(this.user, newUser)) {
            this.user = newUser;
            notifyUpdate(AccountProperty.Property.USERNAME, newUser);
        }
    }

    public String toString() {
        final AccountInfo ai = this.accinfo;
        if (ai != null) {
            return user + ":" + pass + "@" + hoster + "=" + enabled + " " + super.toString() + " AccInfo: " + ai.toString();
        } else {
            return user + ":" + pass + "@" + hoster + "=" + enabled + " " + super.toString();
        }
    }

    public boolean equals(final Account account) {
        if (account == null) {
            return false;
        } else if (account == this) {
            return true;
        } else if (!StringUtils.equals(getHoster(), account.getHoster())) {
            // different hoster
            return false;
        } else if (!StringUtils.equals(getUser(), account.getUser())) {
            // different user names
            return false;
        } else if (StringUtils.isNotEmpty(getUser())) {
            // same none null username
            return true;
        } else if (!StringUtils.equals(getPass(), account.getPass())) {
            // different passwords
            return false;
        } else {
            return true;
        }
    }

    public boolean isMultiHost() {
        return isMulti;
    }

    /** Returns true if this account was ever checked before without errors. */
    public boolean hasEverBeenValid() {
        if (this.getLastValidTimestamp() == -1) {
            return false;
        } else {
            return true;
        }
    }

    public void setLastValidTimestamp(long currentTimeMillis) {
        setProperty(LATEST_VALID_TIMESTAMP, currentTimeMillis);
    }

    public static enum AccountType implements LabelInterface {
        FREE {
            @Override
            public String getLabel() {
                return _JDT.T.AccountType_free();
            }
        },
        PREMIUM {

            @Override
            public boolean is(Account account) {
                return super.is(account) || LIFETIME.is(account);
            }

            @Override
            public String getLabel() {
                return _JDT.T.AccountType_premium();
            }
        },
        LIFETIME {
            @Override
            public String getLabel() {
                return _JDT.T.AccountType_lifetime();
            }
        },
        UNKNOWN {

            @Override
            public boolean is(Account account) {
                return account == null || account.getType() == null || super.is(account);
            }

            @Override
            public String getLabel() {
                return _JDT.T.AccountType_unknown();
            }
        };

        public boolean is(Account account) {
            return account != null && (this == account.getType());
        }
    }

    /**
     * JD2 Code!
     *
     * @since JD2
     */
    public void setType(AccountType type) {
        if (type == null) {
            super.setProperty(ACCOUNT_TYPE, Property.NULL);
        } else {
            super.setProperty(ACCOUNT_TYPE, type.name());
        }
    }

    @Override
    public boolean setProperty(String key, Object value) {
        if (IS_MULTI_HOSTER_ACCOUNT.equalsIgnoreCase(key)) {
            isMulti = value != null && Boolean.TRUE.equals(value);
        }
        return super.setProperty(key, value);
    }

    private final static String REGTS = "regts";

    public long getRegisterTimeStamp() {
        return getLongProperty(REGTS, -1);
    }

    public void setRegisterTimeStamp(long ts) {
        if (ts < 0) {
            removeProperty(REGTS);
        } else {
            setProperty(REGTS, ts);
        }
    }

    public AccountType getType() {
        final String v = getStringProperty(ACCOUNT_TYPE, null);
        if (v != null) {
            try {
                return AccountType.valueOf(v);
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
        return AccountType.PREMIUM;
    }

    /** Returns date format string to be used for account expire dates anywhere within GUI. */
    public static String getExpireDateFormatString(final Object requestor) {
        final String custom = CFG_GUI.CFG.getDateTimeFormatAccountManagerExpireDateColumn();
        if (StringUtils.isNotEmpty(custom)) {
            /* User defined format */
            return custom;
        } else {
            final DateFormat sd = SimpleDateFormat.getDateTimeInstance();
            if (sd instanceof SimpleDateFormat) {
                return ((SimpleDateFormat) sd).toPattern();
            } else {
                /* Localized default format */
                return _GUI.T.PremiumAccountTableModel_getDateFormatString_();
            }
        }
    }
}
