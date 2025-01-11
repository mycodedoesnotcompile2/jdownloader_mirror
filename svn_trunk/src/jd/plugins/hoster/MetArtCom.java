package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.MetartConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50416 $", interfaceVersion = 2, names = {}, urls = {})
public class MetArtCom extends PluginForHost {
    public MetArtCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://join." + getHost() + "/?campaign=header-bar");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        /* Similar websites can be found here for logged in users: https://account-new.metartnetwork.com/ */
        ret.add(new String[] { "metartnetwork.com" });
        ret.add(new String[] { "metart.com" });
        ret.add(new String[] { "sexart.com" });
        ret.add(new String[] { "alsscan.com" });
        ret.add(new String[] { "domai.com" });
        ret.add(new String[] { "eroticbeauty.com" });
        ret.add(new String[] { "errotica-archives.com" });
        ret.add(new String[] { "eternaldesire.com" });
        ret.add(new String[] { "goddessnudes.com" });
        ret.add(new String[] { "lovehairy.com" });
        ret.add(new String[] { "metartx.com" });
        ret.add(new String[] { "rylskyart.com" });
        ret.add(new String[] { "stunning18.com" });
        ret.add(new String[] { "thelifeerotic.com" });
        ret.add(new String[] { "vivthomas.com" });
        ret.add(new String[] { "straplez.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/api/download-media/[A-F0-9]{32}.+");
        }
        return ret.toArray(new String[0]);
    }

    public static final String PROPERTY_UUID         = "uuid";
    public static final String PROPERTY_QUALITY      = "quality";
    public static final String COOKIES_METARTNETWORK = "metartnetwork";

    @Override
    public String getLinkID(final DownloadLink link) {
        final String uuid = link.getStringProperty(PROPERTY_UUID);
        if (uuid != null) {
            final String linkid_without_quality_identifier = this.getHost() + "://" + uuid;
            if (link.hasProperty(PROPERTY_QUALITY)) {
                return linkid_without_quality_identifier + link.getStringProperty(PROPERTY_QUALITY);
            } else {
                return linkid_without_quality_identifier;
            }
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getMirrorID(final DownloadLink link) {
        return this.getLinkID(link);
    }

    @Override
    public String getAGBLink() {
        return "https://help.metart.network/policies-rules/terms-conditions";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        /* URLs are added via crawler and will get checked there already. */
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        /* Free downloads are not possible! */
        throw new AccountRequiredException();
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            return false;
        } else {
            return super.canHandle(link, account);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        this.setBrowserExclusive();
        final AccountInfo ai = this.login(account, true);
        findMoreInformation: if (account.getType() == AccountType.PREMIUM && ai.getValidUntil() == -1) {
            /* Try to find the expire-date the hard way... */
            if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                /* 2025-01-09: This is debug mode only code! */
                break findMoreInformation;
            }
            logger.info("Trying to find premium expire date the hard way...");
            final Cookies requiredCookies = account.loadCookies(COOKIES_METARTNETWORK);
            if (requiredCookies == null || requiredCookies.isEmpty()) {
                /* E.g. if user used cookie login method we won't have these cookies! */
                logger.warning("Failed to find detailed account information because: Required cookies are missing");
                break findMoreInformation;
            }
            try {
                final Browser brc = br.cloneBrowser();
                brc.setCookies(requiredCookies);
                /* Not required but maybe useful in the future */
                // final String metartnetworkAppdataURL = "https://account-new.metartnetwork.com/api/app-data";
                // brc.getPage(metartnetworkAppdataURL);
                // final Map<String, Object> entries = restoreFromString(brc.toString(), TypeRef.MAP);
                // String thisMetartSiteUUID = null;
                // final List<Map<String, Object>> metartSites = (List<Map<String, Object>>) entries.get("sites");
                // for (final Map<String, Object> metartSite : metartSites) {
                // final String domain = metartSite.get("domain").toString();
                // if (domain.equalsIgnoreCase(this.getHost())) {
                // thisMetartSiteUUID = metartSite.get("UUID").toString();
                // break;
                // }
                // }
                final String metartnetworkSubscriptionsURL = "https://account-new.metartnetwork.com/api/subscriptions";
                brc.getPage(metartnetworkSubscriptionsURL);
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Map<String, Object>> subscriptions = (List<Map<String, Object>>) entries.get("subscriptions");
                processSubscriptions(account, ai, subscriptions);
            } catch (final Throwable ignore) {
                logger.log(ignore);
                logger.info("Failed to find detailed account information because: Exception occured");
            }
        }
        return ai;
    }

    public AccountInfo login(final Account account, final boolean verifyCredentials) throws Exception {
        final Cookies cookies = account.loadCookies("");
        /* User cookie login is possible as an alternative way to login */
        final Cookies userCookies = account.loadUserCookies();
        if (cookies != null || userCookies != null) {
            if (userCookies != null) {
                logger.info("Attempting user cookie login");
                setCookies(br, userCookies);
            } else if (cookies != null) { // no need to check for this because it can never be null?!
                logger.info("Attempting cookie login");
                setCookies(br, cookies);
            }
            if (!verifyCredentials) {
                /* Do not verify credentials. */
                return null;
            }
            try {
                final AccountInfo ai = fetchAndSetUserData(account);
                account.setAccountInfo(ai);
                logger.info("Cookie login successful");
                account.saveCookies(br.getCookies(br.getHost()), "");
                return ai;
            } catch (final Exception e) {
                /* Not logged in = Different json -> Exception */
                logger.info("Cookie login failed");
                if (userCookies != null) {
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                }
            }
            br.clearCookies(null);
        }
        logger.info("Performing full login");
        /* Redirect to: https://sso.metartnetwork.com/login */
        br.getPage("https://" + this.getHost() + "/login");
        /* Very important step to obtain 2nd csrf token as cookie! */
        final Browser brb = br.cloneBrowser();
        brb.getPage("/cm?");
        final String _csrfToken = brb.getCookie(brb.getHost(), "_csrfToken", Cookies.NOTDELETEDPATTERN);
        if (StringUtils.isEmpty(_csrfToken)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Browser brc = brb.cloneBrowser();
        brc.setAllowedResponseCodes(401);
        brc.getHeaders().put("Content-Type", "application/json");
        brc.getPage("/api/app-data");
        Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        /* Handle login captcha */
        final String reCaptchaSiteKey = entries.get("googleReCaptchaKey").toString();
        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaSiteKey) {
            @Override
            public TYPE getType() {
                return TYPE.INVISIBLE;
            }
        }.getToken();
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("login", account.getUser());
        postData.put("password", account.getPass());
        postData.put("rememberMe", true);
        postData.put("g-recaptcha-response", recaptchaV2Response);
        /* Very important */
        brc.getHeaders().put("csrf-token", _csrfToken);
        brc.postPageRaw("/api/login", JSonStorage.serializeToJson(postData));
        if (brc.getHttpConnection().getResponseCode() == 401) {
            throw new AccountInvalidException();
        } else if (brc.getHttpConnection().getResponseCode() == 403) {
            throw new AccountInvalidException("Account banned");
        }
        /* Login successful: Access special URL that will lead to multiple redirects and provide required cookies. */
        entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final String redirectURL = entries.get("redirectTo").toString();
        br.getPage(redirectURL);
        final Cookies freshCookies = brc.getCookies(brc.getHost());
        this.setCookies(br, freshCookies);
        account.saveCookies(freshCookies, COOKIES_METARTNETWORK);
        account.saveCookies(br.getCookies(br.getHost()), "");
        if (verifyCredentials) {
            return fetchAndSetUserData(account);
        } else {
            return null;
        }
    }

    private AccountInfo fetchAndSetUserData(final Account account) throws AccountUnavailableException, IOException {
        br.getPage("https://www." + this.getHost() + "/api/user-data");
        final AccountInfo ai = new AccountInfo();
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> user = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "initialState/auth/user");
        if (Boolean.TRUE.equals(user.get("accountNeedsAgeVerification"))) {
            throw new AccountUnavailableException("Age verification needed", 1 * 60 * 60 * 1000);
        }
        if (Boolean.TRUE.equals(user.get("hasAnyValidSubscription"))) {
            account.setType(AccountType.PREMIUM);
            final List<Map<String, Object>> subscriptions = (List<Map<String, Object>>) user.get("subscriptions");
            processSubscriptions(account, ai, subscriptions);
        } else {
            account.setType(AccountType.FREE);
        }
        /* This plugin supports cookie login --> Make sure that usernames are unique! */
        final String email = (String) user.get("email");
        if (!StringUtils.isEmpty(email)) {
            account.setUser(email);
        }
        if (Boolean.TRUE.equals(user.get("isDownloadsBlocked"))) {
            ai.setStatus(ai.getStatus() + " | Downloads blocked!");
        }
        return ai;
    }

    private void processSubscriptions(final Account account, final AccountInfo ai, final List<Map<String, Object>> subscriptions) {
        if (subscriptions == null || subscriptions.isEmpty()) {
            ai.setExpired(true);
            return;
        }
        long highestExpireDate = -1;
        int numberofLifetimeSubscriptions = 0;
        for (final Map<String, Object> subscription : subscriptions) {
            final String expireDateStr = subscription.get("expireDate").toString();
            final long expireDateTimestamp = TimeFormatter.getMilliSeconds(expireDateStr, "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.ENGLISH);
            if (expireDateTimestamp > highestExpireDate) {
                highestExpireDate = expireDateTimestamp;
            }
            if (Boolean.TRUE.equals(subscription.get("isLifetime"))) {
                numberofLifetimeSubscriptions += 1;
            }
        }
        if (numberofLifetimeSubscriptions == subscriptions.size()) {
            logger.info("All subscriptions are lifetime subscriptions -> Account is a lifetime premium account");
            account.setType(AccountType.LIFETIME);
        } else if (highestExpireDate > System.currentTimeMillis()) {
            ai.setValidUntil(highestExpireDate);
        } else {
            logger.info("Failed to determine reasonable expire-date for premium account");
        }
        ai.setStatus(account.getType().getLabel() + " | Paid Packages: " + subscriptions.size());
    }

    private void setCookies(final Browser br, final Cookies cookies) {
        br.setCookies(cookies);
        /* Set cookies on all domains we support. */
        for (final String[] domains : getPluginDomains()) {
            for (final String domain : domains) {
                br.setCookies(domain, cookies);
            }
        }
    }

    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.setBrowserExclusive();
        this.login(account, false);
        String directurl = link.getPluginPatternMatcher();
        /* Access main page to find out if a captcha is required for downloading. */
        br.getPage("https://www." + getHost());
        if (br.containsHTML("\"enableReCaptchaDownload\":\\s*true")) {
            final String rcKey = br.getRegex("\"googleReCaptchaEnterprise\":\\{\"key\":\"([^\"]+)").getMatch(0);
            if (rcKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, rcKey) {
                protected boolean isEnterprise(final String source) {
                    return true;
                }

                protected TYPE getType(String source) {
                    return TYPE.INVISIBLE;
                }
            }.getToken();
            final UrlQuery query = UrlQuery.parse(directurl);
            query.addAndReplace("recaptchaToken", Encoding.urlEncode(recaptchaV2Response));
            directurl = URLHelper.getUrlWithoutParams(directurl) + "?" + query.toString();
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 401) {
                throw new AccountUnavailableException("Session expired?", 5 * 60 * 1000l);
            } else if (br.getHttpConnection().getResponseCode() == 404) {
                /* Should never happen as their URLs are static */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                /* Check for json response with error */
                Map<String, Object> entries = null;
                try {
                    entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                } catch (final Throwable ignore) {
                }
                if (entries != null) {
                    final String errorprefix = "API error: ";
                    final String error = (String) entries.get("error");
                    if (error != null) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorprefix + error);
                    }
                    final List<String> errorcodes = (List<String>) entries.get("error-codes");
                    if (errorcodes != null && errorcodes.size() > 0) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorprefix + errorcodes.get(0));
                    }
                }
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Directurl expired or media content is broken");
            }
        }
        dl.startDownload();
    }

    @Override
    public Class<? extends MetartConfig> getConfigInterface() {
        return MetartConfig.class;
    }
}
