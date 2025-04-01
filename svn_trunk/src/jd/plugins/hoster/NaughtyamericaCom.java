//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.NaughtyamericaConfig;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.NaughtyamericaComCrawler;

@HostPlugin(revision = "$Revision: 50893 $", interfaceVersion = 2, names = { "naughtyamerica.com" }, urls = { "http://naughtyamericadecrypted.+" })
public class NaughtyamericaCom extends PluginForHost {
    public NaughtyamericaCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://natour." + getHost() + "/signup/signup.php");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        // br.addAllowedResponseCodes(new int[] { 456 });
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        if (allowCookieLoginOnly) {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
        } else {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
        }
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms-of-service.html";
    }

    /* Connection stuff */
    private static final boolean allowCookieLoginOnly      = false;
    private static final int     FREE_MAXCHUNKS            = 1;
    /*
     * 2017-01-23: Max 100 connections tital seems to be a stable value - I'd not recommend allowing more as this will most likely cause
     * failing downloads which start over and over.
     */
    private static final int     ACCOUNT_PREMIUM_MAXCHUNKS = -5;
    private final String         type_pic                  = ".+\\.jpg.*?";
    private String               dllink                    = null;
    public static final String   PROPERTY_CONTENT_ID       = "fid";
    public static final String   PROPERTY_VIDEO_QUALITY    = "quality";
    public static final String   PROPERTY_URL_SLUG         = "filename_url";
    public static final String   PROPERTY_PICTURE_NUMBER   = "picnumber";
    public static final String   PROPERTY_CRAWLER_FILENAME = "crawler_filename";
    public static final String   PROPERTY_MAINLINK         = "mainlink";
    public static final String   PROPERTY_DESCRIPTION      = "description";

    public void correctDownloadLink(final DownloadLink link) {
        link.setPluginPatternMatcher(link.getPluginPatternMatcher().replaceAll("(?i)https?://naughtyamericadecrypted", "https://"));
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.PREMIUM.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this);
        return requestFileInformation(link, account);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        /* 2022-03-17: This property should always exist but to keep compatibility to older plugin revisions we're not yet enforcing it. */
        final String crawlerForcedFilename = link.getStringProperty(PROPERTY_CRAWLER_FILENAME);
        if (crawlerForcedFilename != null) {
            link.setFinalFileName(crawlerForcedFilename);
        }
        if (account != null) {
            login(account, false);
            dllink = link.getPluginPatternMatcher();
        } else {
            logger.info("No account available, checking trailer download");
            final String urlSlug = link.getStringProperty("filename_url");
            if (urlSlug == null) {
                /* This should never happen! */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String main_video_url_free = NaughtyamericaComCrawler.getVideoUrlFree(urlSlug);
            br.getPage(main_video_url_free);
            if (NaughtyamericaComCrawler.isOffline(br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            /* 2019-01-18: Trailer is only available in a single quality */
            dllink = br.getRegex("file\\s*?:\\s*?\"(https?[^<>\"]+)\"").getMatch(0);
            if (StringUtils.isEmpty(dllink)) {
                logger.info("Trailer not found -> Premium account needed to download this item");
                return AvailableStatus.TRUE;
            }
        }
        boolean isFreshDirectURL = false;
        if (isTrailer(link) && account != null) {
            dllink = refreshDirecturl(account, link);
            isFreshDirectURL = true;
        }
        URLConnectionAdapter con = null;
        try {
            Browser brc = br.cloneBrowser();
            con = brc.openHeadConnection(dllink);
            if (!looksLikeDownloadableContent(con)) {
                logger.info("Final downloadurl seems to be expired");
                try {
                    brc.followConnection(true);
                } catch (final IOException e) {
                    if (isFreshDirectURL) {
                        throw e;
                    } else {
                        logger.log(e);
                    }
                }
                final String directURL = refreshDirecturl(account, link);
                brc = br.cloneBrowser();
                con = brc.openHeadConnection(directURL);
                if (!looksLikeDownloadableContent(con)) {
                    brc.followConnection(true);
                    errorNoFile();
                }
                /* Save new directURL for next usage. */
                link.setPluginPatternMatcher(directURL);
                dllink = directURL;
            }
            if (con.getCompleteContentLength() > 0) {
                if (con.isContentDecoded()) {
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
            final String filenameFromConnection = getFileNameFromConnection(con);
            if (link.getFinalFileName() == null && filenameFromConnection != null) {
                link.setFinalFileName(Encoding.htmlDecode(filenameFromConnection).trim());
            }
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
        return AvailableStatus.TRUE;
    }

    private String refreshDirecturl(final Account account, final DownloadLink link) throws Exception {
        final String mainlink = link.getStringProperty(PROPERTY_MAINLINK);
        if (mainlink == null) {
            /* This should never happen! Can happen for URLs added via revision 45663 and before. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Let crawler run once again. */
        final PluginForDecrypt crawler = this.getNewPluginForDecryptInstance(this.getHost());
        /* Find expected result. */
        final CryptedLink forCrawler = new CryptedLink(mainlink, link);
        final ArrayList<DownloadLink> results = ((jd.plugins.decrypter.NaughtyamericaComCrawler) crawler).crawlContent(forCrawler, true);
        DownloadLink fresh = null;
        if (link.hasProperty(PROPERTY_PICTURE_NUMBER)) {
            /* Image */
            for (final DownloadLink tmp : results) {
                if (StringUtils.equals(tmp.getStringProperty(PROPERTY_PICTURE_NUMBER), link.getStringProperty(PROPERTY_PICTURE_NUMBER))) {
                    fresh = tmp;
                    break;
                }
            }
        } else {
            /* Video */
            DownloadLink best = null;
            for (final DownloadLink result : results) {
                if (StringUtils.equals(this.getLinkID(result), this.getLinkID(link))) {
                    fresh = result;
                    break;
                }
                if (result.hasProperty("best")) {
                    best = result;
                }
            }
            if (fresh == null && isTrailer(link)) {
                /* Single item which was added before a premium account was available -> Auto select best */
                logger.info("Fallback to best item");
                fresh = best;
            }
        }
        if (fresh == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to refresh directurl");
        }
        this.correctDownloadLink(fresh);
        final String directURL = fresh.getPluginPatternMatcher();
        return directURL;
    }

    private boolean isTrailer(final DownloadLink link) {
        if (StringUtils.containsIgnoreCase(link.getLinkID(), "trailer")) {
            return true;
        } else {
            return false;
        }
    }

    private String getFID(final DownloadLink dl) {
        return dl.getStringProperty("fid");
    }

    private String getUrlSlug(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_URL_SLUG);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(null, link);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    public void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            /* For developers: Disable this Boolean if normal login process breaks down and you're unable or too lazy to fix it! */
            final Cookies userCookies = account.loadUserCookies();
            final Cookies cookies = account.loadCookies("");
            if (cookies != null || userCookies != null) {
                /*
                 * Try to avoid login captcha by re-using cookies.
                 */
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!force) {
                    return;
                }
                br.getPage("https://" + NaughtyamericaComCrawler.DOMAIN_PREFIX_PREMIUM + account.getHoster());
                if (isLoggedIN(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(account.getHoster()), "");
                    return;
                }
                logger.info("Cookie login failed");
                account.clearCookies("");
                if (userCookies != null) {
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                } else {
                    /* Full login required */
                    br.clearAll();
                }
            }
            if (allowCookieLoginOnly) {
                showCookieLoginInfo();
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_required());
            }
            logger.info("Performing full login");
            br.getPage("https://" + NaughtyamericaComCrawler.DOMAIN_PREFIX_PREMIUM + account.getHoster() + "/");
            br.getPage("https://" + NaughtyamericaComCrawler.DOMAIN_PREFIX_PREMIUM + account.getHoster() + "/login");
            final Regex httpRedirect = br.getRegex("http-equiv=\"refresh\" content=\"(\\d+);\\s*url=(/[^<>\"]+)\"");
            if (httpRedirect.patternFind()) {
                /* 2019-01-21: Hmm leads to HTTP/1.1 405 Not Allowed */
                /*
                 * <meta http-equiv="refresh" content="10; url=/distil_r_captcha.html?requestId=<requestId>c&httpReferrer=%2Flogin" />
                 */
                final String waitStr = httpRedirect.getMatch(0);
                final String redirectURL = httpRedirect.getMatch(1);
                int wait = 10;
                if (waitStr != null) {
                    wait = Integer.parseInt(waitStr);
                }
                Thread.sleep(wait * 1001l);
                br.getPage(redirectURL);
            }
            Form loginform = br.getFormbyKey("username");
            if (loginform == null) {
                loginform = br.getForm(0);
            }
            if (loginform == null) {
                if (br.containsHTML("AwsWafIntegration")) {
                    throw new AccountInvalidException("Anti bot protection blocked login, try cookie login");
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            loginform.put("dest", "");
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            /* Handle login captcha if required */
            if (CaptchaHelperHostPluginRecaptchaV2.containsRecaptchaV2Class(loginform)) {
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                loginform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            }
            final Request request = br.createFormRequest(loginform);
            request.getHeaders().put("Origin", "https://members.naughtyamerica.com");
            br.getPage(request);
            final Form continueform = br.getFormbyKey("response");
            if (continueform != null) {
                /* Redirect from probiller.com to main website --> Login complete */
                br.submitForm(continueform);
            }
            if (br.getURL().contains("/postLogin")) {
                br.getPage("//" + NaughtyamericaComCrawler.DOMAIN_PREFIX_PREMIUM + br.getHost());
            }
            if (br.getURL().contains("beta.") || br.getURL().contains("/login")) {
                /* 2016-12-12: Redirects to their beta-page might happen --> Go back to the old/stable version of their webpage. */
                br.getPage("https://" + NaughtyamericaComCrawler.DOMAIN_PREFIX_PREMIUM + account.getHoster());
            }
            final String loginCookie = br.getCookie(br.getHost(), "nrc", Cookies.NOTDELETEDPATTERN);
            if (!isLoggedIN(br) && loginCookie == null) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    public static boolean isLoggedIN(final Browser br) {
        if (br.containsHTML("/logout\"")) {
            return true;
        } else {
            return false;
        }
    }

    /***
     * 2022-03-17: Not much account information for us to crawl. User purchases can be found on website but expire-date or next bill date is
     * nowhere to be found: </br>
     * https://members.naughtyamerica.com/account/purchases
     */
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.PREMIUM);
        account.setConcurrentUsePossible(true);
        /* 2022-03-17: Their cookies are not valid for a long time -> Be sure to keep them active */
        account.setRefreshTimeout(5 * 60 * 1000l);
        br.getPage("/account/profile");
        String username = null;
        final Form profileform = br.getFormbyActionRegex(".*/account/profile");
        if (profileform != null) {
            final InputField ifield = profileform.getInputField("display_name");
            if (ifield != null) {
                username = ifield.getValue();
            }
        }
        if (username == null) {
            logger.warning("Failed to find real username inside html code");
        } else {
            username = Encoding.htmlDecode(username).trim();
        }
        if (account.loadUserCookies() != null && username != null) {
            /*
             * Try to use unique usernames even if user did use cookie login as in theory he can enter whatever he wants into that username
             * field when using cookie login.
             */
            account.setUser(username);
        }
        return ai;
    }

    private void handleDownload(final Account account, final DownloadLink link) throws Exception {
        if (StringUtils.isEmpty(dllink)) {
            /* Usually only happens in free mode e.g. trailer download --> But no trailer is available */
            throw new AccountRequiredException();
        }
        if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), ACCOUNT_PREMIUM_MAXCHUNKS);
        } else {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), FREE_MAXCHUNKS);
        }
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("The final dllink seems not to be a file!");
            br.followConnection(true);
            errorNoFile();
        }
        dl.startDownload();
    }

    private void errorNoFile() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_FATAL, "Final downloadurl did not return file-content");
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account);
        handleDownload(account, link);
    }

    public boolean allowHandle(final DownloadLink link, final PluginForHost plugin) {
        final boolean is_this_plugin = link.getHost().equalsIgnoreCase(plugin.getHost());
        if (is_this_plugin) {
            /* The original plugin is always allowed to download. */
            return true;
        } else if (!link.isEnabled() && "".equals(link.getPluginPatternMatcher())) {
            /*
             * setMultiHostSupport uses a dummy DownloadLink, with isEnabled == false. we must set to true for the host to be added to the
             * supported host array.
             */
            return true;
        } else {
            /* Multihosts should not be tried for picture-downloads! */
            return !link.getDownloadURL().matches(type_pic);
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* free accounts also have captchas */
            return true;
        } else {
            return false;
        }
    }

    @Override
    public String buildExternalDownloadURL(final DownloadLink link, final PluginForHost buildForThisPlugin) {
        if (StringUtils.equals("premiumize.me", buildForThisPlugin.getHost())) {
            return jd.plugins.decrypter.NaughtyamericaComCrawler.getVideoUrlFree(getUrlSlug(link));
        } else {
            return super.buildExternalDownloadURL(link, buildForThisPlugin);
        }
    }

    @Override
    public String getDescription() {
        return "Download videos- and pictures with the naughtyamerica.com plugin.";
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return NaughtyamericaConfig.class;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}