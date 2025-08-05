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
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;

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
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.SuicidegirlsComCrawler;

@HostPlugin(revision = "$Revision: 51299 $", interfaceVersion = 3, names = { "suicidegirls.com" }, urls = { "https?://(?:www\\.)?suicidegirls\\.com/videos/\\d+/[A-Za-z0-9\\-_]+/" })
public class SuicidegirlsCom extends PluginForHost {
    public SuicidegirlsCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/shop/");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        prepBR(br);
        return br;
    }

    public Browser prepBR(final Browser br) {
        br.setFollowRedirects(true);
        br.setCookie(getHost(), "burlesque_ad_closed", "True");
        br.setCookie(getHost(), "django_language", "en");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/legal/";
    }

    private static final Pattern TYPE_VIDEO                      = Pattern.compile("https?://(?:www\\.)?suicidegirls\\.com/videos/(\\d+)/[A-Za-z0-9\\-_]+/", Pattern.CASE_INSENSITIVE);
    /* Properties */
    public static final String   PROPERTY_DIRECTURL              = "directlink";
    public static final String   PROPERTY_IMAGE_NAME             = "imageName";
    public static final String   PROPERTY_IMAGE_INDEX            = "image_index";
    public static final String   PROPERTY_MAIN_LINK              = "mainlink";
    private static final String  PROPERTY_FRESH_DIRECTURL_NEEDED = "fresh_directurl_needed";
    private String               dllink                          = null;

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getMirrorID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        if (link == null || link.getPluginPatternMatcher() == null) {
            return null;
        }
        final Regex regex_video = new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO);
        if (regex_video.patternFind()) {
            return regex_video.getMatch(0);
        } else {
            /* Image */
            final String mainlink = link.getStringProperty(PROPERTY_DIRECTURL);
            if (mainlink != null) {
                return mainlink + "_" + link.getStringProperty(PROPERTY_IMAGE_INDEX);
            } else {
                /* Legacy / older items */
                return link.getStringProperty(PROPERTY_IMAGE_NAME);
            }
        }
    }

    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(getHost());
        return requestFileInformation(link, account, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        if (account != null) {
            this.login(account, false);
        }
        String filename = null;
        final Regex videourl = new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO);
        String extDefault = null;
        if (videourl.patternFind()) {
            extDefault = ".mp4";
            br.getPage(link.getPluginPatternMatcher());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            dllink = br.getRegex("<source src=\"(http[^<>\"]*?)\" type=\\'video/mp4\\'").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("\"(https?://[^/]+/videos/[^/]+\\.(?:mp4|m3u8))\"").getMatch(0);
                if (dllink == null) {
                    /* 2025-08-04 */
                    dllink = br.getRegex("<source src=\"(https://[^\"]+)\" type=\"application/x-mpegURL\"").getMatch(0);
                }
            }
            filename = br.getRegex("<h2 class=\"title\">(?:SuicideGirls:\\s*)?([^<>\"]*?)</h2>").getMatch(0);
            if (filename == null) {
                /* Fallback: Take file name from URL */
                filename = new Regex(link.getPluginPatternMatcher(), "(?i)/videos/\\d+/([A-Za-z0-9\\-_]+)/").getMatch(0);
            }
            filename = Encoding.htmlDecode(filename).trim();
            filename += extDefault;
        } else {
            extDefault = ".jpg";
            filename = link.getStringProperty(PROPERTY_IMAGE_NAME);
            if (link.hasProperty(PROPERTY_FRESH_DIRECTURL_NEEDED)) {
                /* Direct url expired -> Find fresh one */
                final String contenturl = link.getStringProperty(PROPERTY_MAIN_LINK);
                if (contenturl == null) {
                    /* Older links */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final SuicidegirlsComCrawler crawler = (SuicidegirlsComCrawler) this.getNewPluginForDecryptInstance(this.getHost());
                final CryptedLink cl = new CryptedLink(contenturl);
                final List<DownloadLink> results = crawler.crawl(cl, account);
                DownloadLink fresh = null;
                final String thisLinkID = this.getLinkID(link);
                for (final DownloadLink result : results) {
                    if (StringUtils.equals(thisLinkID, this.getLinkID(result))) {
                        fresh = result;
                        break;
                    }
                }
                if (fresh == null) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Failed to refresh directurl");
                }
                link.removeProperty(PROPERTY_FRESH_DIRECTURL_NEEDED);
                link.setProperty(PROPERTY_DIRECTURL, fresh.getStringProperty(PROPERTY_DIRECTURL));
            }
            dllink = getStoredDirecturl(link);
        }
        if (dllink != null) {
            dllink = Encoding.htmlOnlyDecode(dllink);
        }
        link.setName(filename);
        if (dllink != null && !isHLS(dllink)) {
            final Browser brc = br.cloneBrowser();
            basicLinkCheck(brc, brc.createHeadRequest(dllink), link, filename, extDefault);
            this.handleConnectionErrors(link, brc, brc.getHttpConnection());
            link.setProperty(PROPERTY_DIRECTURL, dllink);
        }
        if (link.getFinalFileName() == null && filename != null) {
            link.setFinalFileName(correctOrApplyFileNameExtension(filename, extDefault, null));
        }
        return AvailableStatus.TRUE;
    }

    protected void handleConnectionErrors(final DownloadLink link, final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        super.handleConnectionErrors(br, con);
        if (StringUtils.containsIgnoreCase(con.getURL().toExternalForm(), "signature-expired")) {
            this.getDownloadLink().setProperty(PROPERTY_FRESH_DIRECTURL_NEEDED, true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Expired directurl", 5 * 60 * 1000l);
        } else if (StringUtils.containsIgnoreCase(con.getURL().toExternalForm(), "ph-join-sg.jpg")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid media: Got advertising image instead of expected file content");
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link, null, true);
        throw new AccountRequiredException();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public void login(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(this.getHost(), cookies);
                if (!validateCookies) {
                    logger.info("Trust cookies without checking");
                    return;
                }
                logger.info("Checking login cookies");
                // do a test
                br.getPage("https://www." + this.getHost() + "/member/account/");
                if (isLoggedin(br)) {
                    logger.info("Successfully logged in via cookies");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + this.getHost());
            final Form loginform = br.getFormbyProperty("id", "login-form");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* login can require recaptchav2 */
            if (loginform.containsHTML("g-recaptcha") && loginform.containsHTML("data-sitekey")) {
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br) {
                    @Override
                    public String getSiteKey() {
                        return getSiteKey(loginform.getHtmlCode());
                    };
                }.getToken();
                loginform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            }
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            final Browser brc = br.cloneBrowser();
            /* 400 = invalid credentials, 429 = too many bad login attempts in a short time */
            brc.setAllowedResponseCodes(400, 429);
            brc.getHeaders().put("Accept", "*/*");
            brc.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            brc.getHeaders().put("Origin", "https://www." + br.getHost());
            final String specialToken = br.getCookie(br.getHost(), "sgcsrftoken", Cookies.NOTDELETEDPATTERN);
            if (specialToken != null) {
                brc.getHeaders().put("X-Csrftoken", specialToken);
            }
            brc.submitForm(loginform);
            /* We only get a json response if there was a problem. */
            Map<String, Object> entries = null;
            try {
                entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            } catch (final Throwable ignore) {
            }
            if (entries != null) {
                final String errormessage = (String) entries.get("message");
                if (!StringUtils.isEmpty(errormessage)) {
                    throw new AccountInvalidException(errormessage);
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML(">\\s*Log Out\\s*<");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        if (!br.getURL().contains("/member/account")) {
            br.getPage("/member/account/");
        }
        ai.setUnlimitedTraffic();
        final Regex info = br.getRegex("YOUR ACCOUNT IS CLOSING IN\\s*<a>(\\d+)\\s*weeks?\\s*,\\s*(\\d+) days?");
        long expire_long = -1;
        if (info.patternFind()) {
            final String weeks = info.getMatch(0);
            final String days = info.getMatch(1);
            final long days_total = Long.parseLong(weeks) * 7 * +Long.parseLong(days);
            expire_long = System.currentTimeMillis() + days_total * 24 * 60 * 60 * 1000l;
        }
        if (expire_long == -1) {
            final String expireStr = br.getRegex("Your account will be cancelled on (\\w+ \\d+, \\d{4})").getMatch(0);
            if (expireStr != null) {
                expire_long = TimeFormatter.getMilliSeconds(expireStr, "MMMM dd, yyyy", Locale.ENGLISH);
            }
        }
        if (expire_long > -1) {
            ai.setValidUntil(expire_long, br);
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(true);
        } else {
            // free account
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultanFreeDownloadNum());
            account.setConcurrentUsePossible(false);
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.requestFileInformation(link, account, true);
        if (dllink == null) {
            logger.warning("Failed to find final downloadurl");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (isHLS(dllink)) {
            /* 2020-03-02: New: HLS streams */
            br.getPage(dllink);
            final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(this.br));
            if (hlsbest == null) {
                /* No content available --> Probably the user wants to download hasn't aired yet --> Wait and retry later! */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Sendung wurde noch nicht ausgestrahlt", 60 * 60 * 1000l);
            }
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, hlsbest.getDownloadurl());
            dl.startDownload();
        } else {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), 0);
            handleConnectionErrors(link, br, dl.getConnection());
            link.setProperty(PROPERTY_DIRECTURL, dl.getConnection().getURL().toExternalForm());
            dl.startDownload();
        }
    }

    private boolean isHLS(final String url) {
        if (StringUtils.containsIgnoreCase(url, ".m3u8")) {
            return true;
        } else {
            return false;
        }
    }

    private String getStoredDirecturl(final DownloadLink link) {
        String url = link.getStringProperty(PROPERTY_DIRECTURL);
        if (url != null) {
            /* 2024-03-12: Fix older items which were stored with html entities. */
            url = Encoding.htmlOnlyDecode(url);
            return url;
        }
        return null;
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