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
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.AbstractCloudflareTurnstileCaptcha;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52239 $", interfaceVersion = 3, names = {}, urls = {})
public class AlfafileNet extends PluginForHost {
    public AlfafileNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(this.getHost(), "lang", "en");
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "alfafile.net" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/file/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    /* Connection stuff */
    private static final boolean FREE_RESUME               = false;
    private static final int     FREE_MAXCHUNKS            = 1;
    private static final boolean ACCOUNT_FREE_RESUME       = false;
    private static final int     ACCOUNT_FREE_MAXCHUNKS    = 1;
    private static final boolean ACCOUNT_PREMIUM_RESUME    = true;
    private static final int     ACCOUNT_PREMIUM_MAXCHUNKS = -5;
    /* don't touch the following! */
    private boolean              isDirecturl               = false;
    /*
     * TODO: Use API for linkchecking whenever an account is added to JD. This will ensure that the plugin will always work, at least for
     * premium users. Status 2015-08-03: Filecheck API does not seem to work --> Disabled it - reported API issues to jiaz.
     */
    private static final boolean prefer_api_linkcheck      = false;

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link);
    }

    @SuppressWarnings("deprecation")
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        isDirecturl = false;
        this.setBrowserExclusive();
        String filename = null;
        String filesize = null;
        String md5 = null;
        String api_token = null;
        Map<String, Object> api_resp = null;
        Number filesize_bytes = null;
        if (prefer_api_linkcheck) {
            final Account account = AccountController.getInstance().getValidAccount(this.getHost());
            if (account != null) {
                api_token = getLoginToken(account);
            }
            if (api_token != null) {
                br.getPage(API_BASE + "/file/info?file_id=" + getFileID(link) + "&token=" + api_token);
                api_resp = this.handleAPIErrors(link, account);
            }
        }
        if (api_resp != null) {
            Map<String, Object> fileinfo = (Map<String, Object>) api_resp.get("file");
            filename = fileinfo.get("name").toString();
            filesize_bytes = (Number) fileinfo.get("size");
            md5 = (String) fileinfo.get("hash");
        } else {
            URLConnectionAdapter con = null;
            try {
                con = br.openGetConnection(link.getDownloadURL());
                if (this.looksLikeDownloadableContent(con)) {
                    logger.info("This url is a directurl");
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                    link.setFinalFileName(getFileNameFromConnection(con));
                    isDirecturl = true;
                    return AvailableStatus.TRUE;
                } else {
                    br.followConnection();
                }
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
            if (this.br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            filename = br.getRegex("id=\"st_file_name\" title=\"([^<>\"]*?)\"").getMatch(0);
            filesize = br.getRegex("<span class=\"size\">([^<>\"]*?)</span>").getMatch(0);
        }
        if (filename == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setFinalFileName(Encoding.htmlDecode(filename).trim());
        if (filesize_bytes != null) {
            link.setVerifiedFileSize(filesize_bytes.longValue());
        } else if (StringUtils.isNotEmpty(filesize)) {
            link.setDownloadSize(SizeFormatter.getSize(filesize.contains(".") && filesize.contains(",") ? filesize.replace(",", "") : filesize));
        }
        if (md5 != null) {
            /* TODO: Check if their API actually returns valid md5 hashes */
            link.setMD5Hash(md5);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link, FREE_RESUME, FREE_MAXCHUNKS, "free_directlink");
    }

    private void doFree(final DownloadLink link, final boolean resumable, final int maxchunks, final String directlinkproperty) throws Exception, PluginException {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        String dllink = checkDirectLink(link, directlinkproperty);
        if (dllink == null) {
            if (isDirecturl) {
                dllink = link.getDownloadURL();
            } else {
                final String fid = getFileID(link);
                br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
                br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                br.getPage("/download/start_timer/" + fid);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                /* Small hack: Extract html code from json, then set it as request-html code. */
                final String html = entries.get("html").toString();
                br.getRequest().setHtmlCode(html);
                final String reconnect_wait = br.getRegex("Try again in (\\d+) minutes").getMatch(0);
                if (br.containsHTML(">\\s*This file can be downloaded by premium users only|>You can download files up to")) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
                } else if (reconnect_wait != null) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Long.parseLong(reconnect_wait) * 60 * 1001l);
                } else if (br.containsHTML("You can't download not more than \\d+ file at a time")) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Too many max sim dls", 20 * 60 * 1000l);
                } else if (br.containsHTML("You have reached your daily downloads limit. Please try again later\\.")) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "You have reached your daily download limit.", 3 * 60 * 60 * 1000l);
                }
                final String wait_str = br.getRegex(">(\\d+)\\s*<span>\\s*s").getMatch(0);
                final String redirect_url = entries.get("redirect_url").toString();
                if (wait_str == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else if (redirect_url == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final int wait = (int) ReflectionUtils.cast(entries.get("timer"), Integer.class);
                this.sleep(wait * 1001l, link);
                br.getPage(redirect_url);
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
                }
                final Form dlform = br.getFormBySubmitvalue("send");
                if (CaptchaHelperHostPluginRecaptchaV2.containsRecaptchaV2Class(br)) {
                    logger.info("Detected captcha method \"reCaptchaV2\" for this host");
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, this.br).getToken();
                    dlform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                    br.submitForm(dlform);
                    logger.info("Submitted DLForm");
                } else if (AbstractCloudflareTurnstileCaptcha.containsCloudflareTurnstileClass(br)) {
                    final String captchaResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br).getToken();
                    dlform.put("cf-turnstile-response", Encoding.urlEncode(captchaResponse));
                } else {
                    /* No captcha? */
                    logger.info("No captcha required?");
                }
                br.submitForm(dlform);
                if (CaptchaHelperHostPluginRecaptchaV2.containsRecaptchaV2Class(br) || AbstractCloudflareTurnstileCaptcha.containsCloudflareTurnstileClass(br)) {
                    /* This should be a rare case */
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                dllink = br.getRegex("href=\"(https://[^<>\"]*?)\" class=\"big_button\"><span>\\s*Download</span>").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("\"(https?://[a-z0-9\\-]+\\.alfafile\\.net/dl/[^<>\"]*?)\"").getMatch(0);
                }
                if (dllink == null) {
                    /* 2020-04-14 */
                    dllink = br.getRegex("\"(https?://[a-z0-9\\-]+\\.alfafile\\.[a-z]+/download/[^<>\"]*?)\"").getMatch(0);
                }
                if (dllink == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resumable, maxchunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        link.setProperty(directlinkproperty, dllink);
        dl.startDownload();
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    if (con.getCompleteContentLength() > 0) {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                    return dllink;
                } else {
                    throw new IOException();
                }
            } catch (final Exception e) {
                logger.log(e);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    /** https://alfafile.net/api/doc */
    private static final String API_BASE = "https://alfafile.net/api/v1";

    private Map<String, Object> login(final Account account, final boolean validateLoginToken) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            String token = getLoginToken(account);
            if (cookies != null && token != null) {
                /* We do not really need the cookies but we need the timstamp! */
                br.setCookies(cookies);
                if (!validateLoginToken) {
                    return null;
                }
                logger.info("Checking token");
                br.postPage(API_BASE + "/user/info", "token=" + Encoding.urlEncode(token));
                try {
                    final Map<String, Object> entries = this.handleAPIErrors(null, account);
                    logger.info("Token login successful");
                    return entries;
                } catch (final Exception e) {
                    logger.log(e);
                    logger.info("Token login failed");
                }
            }
            logger.info("Performing full login");
            /*
             * Using the same API as rapidgator.net (alfafile uses "/v1" in baseURL, rapidgator uses "v2" but responses are the same.)
             */
            br.getPage(API_BASE + "/user/login?login=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            final Map<String, Object> entries = this.handleAPIErrors(null, account);
            token = entries.get("token").toString();
            if (StringUtils.isEmpty(token)) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            account.saveCookies(br.getCookies(br.getURL()), "");
            account.setProperty("token", token);
            return entries;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> entries = login(account, true);
        final Map<String, Object> user = (Map<String, Object>) entries.get("user");
        final Map<String, Object> traffic = (Map<String, Object>) user.get("traffic");
        final Map<String, Object> storage = (Map<String, Object>) user.get("storage");
        final long traffic_total = ((Number) traffic.get("total")).longValue();
        final long traffic_left = ((Number) traffic.get("left")).longValue();
        if (Boolean.TRUE.equals(user.get("is_premium"))) {
            final String expire = user.get("premium_end_time").toString();
            ai.setValidUntil(Long.parseLong(expire) * 1000);
            account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
            account.setType(AccountType.PREMIUM);
            account.setConcurrentUsePossible(true);
        } else {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(this.getMaxSimultanFreeDownloadNum());
            account.setConcurrentUsePossible(false);
        }
        ai.setTrafficLeft(traffic_left);
        ai.setTrafficMax(traffic_total);
        try {
            final long storage_total = ((Number) storage.get("total")).longValue();
            final long storage_left = ((Number) storage.get("left")).longValue();
            ai.setUsedSpace(storage_total - storage_left);
        } catch (final Exception e) {
            logger.log(e);
            logger.info("Failed to parse storage information");
        }
        return ai;
    }

    @SuppressWarnings("deprecation")
    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        login(account, false);
        if (account.getType() == AccountType.FREE) {
            /*
             * No API --> We're actually not downloading via free account but it doesnt matter as there are no known free account advantages
             * compared to unregistered mode.
             */
            br.getPage(link.getDownloadURL());
            doFree(link, ACCOUNT_FREE_RESUME, ACCOUNT_FREE_MAXCHUNKS, "account_free_directlink");
        } else {
            String dllink = this.checkDirectLink(link, "premium_directlink");
            if (dllink == null) {
                final String fid = getFileID(link);
                this.br.getPage(API_BASE + "/file/download?file_id=" + fid + "&token=" + getLoginToken(account));
                Map<String, Object> entries = handleAPIErrors(link, account);
                dllink = entries.get("download_url").toString();
                if (dllink == null) {
                    logger.warning("Final downloadlink (String is \"dllink\") regex didn't match!");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, ACCOUNT_PREMIUM_RESUME, ACCOUNT_PREMIUM_MAXCHUNKS);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            link.setProperty("premium_directlink", dllink);
            dl.startDownload();
        }
    }

    private Map<String, Object> handleAPIErrors(final DownloadLink link, final Account account) throws PluginException {
        Map<String, Object> entries = null;
        try {
            /* 2024-11-21: Hotfix for API returning invalid json: "1{"val" (string starts with "1" and not with "{". */
            final String json = br.getRegex("(\\{.+)").getMatch(0);
            entries = restoreFromString(json, TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, wait);
            } else {
                throw new AccountUnavailableException(msg, wait);
            }
        }
        final int status = ((Number) entries.get("status")).intValue();
        String errormessage = (String) entries.get("details");
        if (status == 200) {
            /* No error */
            Map<String, Object> response = (Map<String, Object>) entries.get("response");
            return response;
        }
        if (status == 409 && StringUtils.containsIgnoreCase(errormessage, "File temporarily unavailable")) {
            /*
             * {"response":null,"status":409,"details":"Conflict. File temporarily unavailable."}
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormessage, 30 * 60 * 1000l);
        }
        if (status == 401) {
            /* This can sometimes happen in premium mode */
            /* {"response":null,"status":401,"details":"Unauthorized. Token doesn't exist"} */
            if (account != null) {
                throw new AccountInvalidException(errormessage);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormessage, 5 * 60 * 1000l);
            }
        } else if (status == 404) {
            /*
             * E.g. detailed errormessages: "details":"File with file_id: '1234567' doesn't exist"
             */
            if (errormessage == null) {
                errormessage = "File does not exist according to API";
            }
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (status == 409) {
            /*
             * E.g. detailed errormessages:
             *
             * Conflict. Delay between downloads must be not less than 60 minutes. Try again in 51 minutes.
             *
             * Conflict. DOWNLOAD::ERROR::You can't download not more than 1 file at a time in free mode.
             */
            String minutes_regexed = null;
            int minutes = 60;
            if (errormessage != null) {
                minutes_regexed = new Regex(errormessage, "again in (\\d+) minutes?").getMatch(0);
                if (minutes_regexed != null) {
                    minutes = Integer.parseInt(minutes_regexed);
                }
            }
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, minutes * 60 * 1001l);
        } else {
            final String msg = "Error " + status + " | " + errormessage;
            if (link == null) {
                throw new AccountUnavailableException(msg, 3 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, msg);
            }
        }
    }

    @SuppressWarnings("deprecation")
    private String getFileID(final DownloadLink dl) {
        return new Regex(dl.getDownloadURL(), "([A-Za-z0-9]+)$").getMatch(0);
    }

    private String getLoginToken(final Account acc) {
        return acc.getStringProperty("token", null);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        if (acc != null && AccountType.PREMIUM.equals(acc.getType())) {
            return false;
        } else {
            return true;
        }
    }
}