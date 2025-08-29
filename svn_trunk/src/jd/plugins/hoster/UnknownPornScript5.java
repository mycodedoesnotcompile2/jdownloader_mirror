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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.HTMLSearch;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.SiteType.SiteTemplate;

@HostPlugin(revision = "$Revision: 51396 $", interfaceVersion = 3, names = { "boyfriendtv.com", "ashemaletube.com", "pornoxo.com", "worldsex.com", "bigcamtube.com" }, urls = { "https?://(?:\\w+\\.)?boyfriendtv\\.com/videos/\\d+/[a-z0-9\\-_]+/", "https?://(?:\\w+\\.)?ashemaletube\\.com/videos/\\d+/[a-z0-9\\-_]+/", "https?://(?:\\w+\\.)?pornoxo\\.com/videos/\\d+/[a-z0-9\\-_]+/", "https?://(?:\\w+\\.)?worldsex\\.com/videos/[a-z0-9\\-_]+\\-\\d+(?:\\.html|/)?", "https?://(?:\\w+\\.)?bigcamtube\\.com/videos/[a-z0-9\\-_]+/" })
public class UnknownPornScript5 extends PluginForHost {
    public UnknownPornScript5(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/registration/");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        /* 2025-08-28: worldsex.com */
        br.setCookie(getHost(), "age_verification", "1");
        /* 2025-08-28: worldsex.com */
        br.setCookie(getHost(), "wspop", "1");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* DEV NOTES */
    /* Porn_plugin */
    /* V0.1 */
    // other: Should work for all (porn) sites that use the "jwplayer" with http URLs: http://www.jwplayer.com/
    private static final String type_allow_title_as_filename = ".+FOR_WEBSITES_FOR_WHICH_HTML_TITLE_TAG_CONTAINS_GOOD_FILENAME.+";
    private static final String default_Extension            = ".mp4";
    private List<String>        directurls                   = new ArrayList<String>();

    @Override
    public void resetPluginGlobals() {
        this.directurls.clear();
    }

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/tos.html";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, AccountController.getInstance().getValidAccount(this.getHost()), false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        this.directurls.clear();
        final String contenturl = link.getPluginPatternMatcher();
        if (!link.isNameSet()) {
            /* Find and set weak-filename */
            /* Let's find the url_slug as a fallback in case we cannot find the filename inside the html code. */
            String url_slug = null;
            final String[] urlparts = new Regex(link.getPluginPatternMatcher(), "https?://[^/]+/[^/]+/(.+)").getMatch(0).split("/");
            String url_id = null;
            for (String urlpart : urlparts) {
                if (urlpart.matches("\\d+")) {
                    url_id = urlpart;
                } else {
                    url_slug = urlpart;
                    break;
                }
            }
            if (url_slug != null) {
                url_slug = url_slug.replace(".html", "");
                if (url_id == null) {
                    /* In case we have an ID, it might be in the url_filename --> Find it */
                    /* First check if we find it at the beginning. */
                    url_id = new Regex(url_slug, "^(\\d+\\-).+").getMatch(0);
                    if (url_id == null) {
                        /* Secondly check if we find it at the end. */
                        url_id = new Regex(url_slug, ".+(\\-\\d+)$").getMatch(0);
                    }
                }
                if (url_id != null) {
                    /* Remove url_id from url_filename */
                    url_slug = url_slug.replace(url_id, "");
                }
            } else {
                url_slug = url_id;
            }
            /* Make it look nicer! */
            url_slug = url_slug.replace("-", " ");
            link.setName(url_slug + default_Extension);
        }
        br.setAllowedResponseCodes(new int[] { 410 });
        if (StringUtils.containsIgnoreCase(contenturl, "bigcamtube.com")) {
            br.setCookie("www.bigcamtube.com", "age_verify", "1");
            br.addAllowedResponseCodes(500);
        }
        if (account != null) {
            this.login(account, false);
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">Sorry, we couldn't find")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = regexStandardTitleWithHost(this.getHost());
        if (title == null) {
            title = HTMLSearch.searchMetaTag(br, "og:title");
        }
        if (title == null) {
            /* Works e.g. for: boyfriendtv.com, ashemaletube.com, pornoxo.com */
            title = br.getRegex("<div id=\"maincolumn2\">\\s*?<h1>([^<>]*?)</h1>").getMatch(0);
        }
        if (title == null && contenturl.matches(type_allow_title_as_filename)) {
            title = br.getRegex("<title>([^<>]*?)</title>").getMatch(0);
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            title = applyFilenameExtension(title, default_Extension);
            link.setFinalFileName(title);
        }
        getDllink();
        return AvailableStatus.TRUE;
    }

    private boolean isHLS(final String str) {
        /* 2024-10-22: pornoxo.com */
        if (StringUtils.contains(str, "media=hls")) {
            return true;
        } else if (StringUtils.contains(str, ".m3u8")) {
            /* HLS master e.g. bigcamtube.com */
            return true;
        } else {
            return false;
        }
    }

    private void getDllink() throws Exception {
        /* Find correct js-source, then find dllink inside of it. */
        final String[] scripts = br.getRegex("<script[^>]*?>(.*?)</script>").getColumn(0);
        for (final String script : scripts) {
            String dllink = searchDllinkInsideJWPLAYERSource(script);
            if (dllink != null) {
                this.directurls.add(dllink);
                break;
            }
        }
        final String[] videolinks = br.getRegex("<(?:source|video)[^<>]*? src=(?:'|\")([^<>'\"]+)(?:'|\")").getColumn(0);
        if (videolinks != null && videolinks.length != 0) {
            for (final String videolink : videolinks) {
                if (this.directurls.contains(videolink)) {
                    continue;
                }
                this.directurls.add(videolink);
            }
        }
        if (this.directurls.isEmpty() && !requiresAccount(br)) {
            /*
             * No player found --> Chances are high that there is no playable content --> Video offline
             *
             * This can also be seen as a "last chance offline" errorhandling for websites for which the above offline-errorhandling doesn't
             * work!
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    private String searchDllinkInsideJWPLAYERSource(final String jwplayer_source) {
        /* Source #1 */
        String dllink = new Regex(jwplayer_source, "('|\")file\\1:\\s*('|\")(http.*?)\\2").getMatch(2);
        if (inValidateDllink(dllink)) {
            /* E.g. worldsex.com */
            dllink = new Regex(jwplayer_source, "file[\t\n\r ]*?:[\t\n\r ]*?('|\")(http.*?)\\1").getMatch(1);
        }
        if (inValidateDllink(dllink)) {
            /*
             * E.g. kt_player + jwplayer (can also be KernelVideoSharingCom), example: xfig.net[dedicated plugin], cliplips.com)<br />
             * Important: Do not pickup the slash at the end!
             */
            dllink = new Regex(jwplayer_source, "var videoFile=\"(http[^<>\"]*?)/?\";").getMatch(0);
        }
        if (inValidateDllink(dllink)) {
            /* Check for multiple videoqualities --> Find highest quality */
            final String sources_source = new Regex(jwplayer_source, "sources:\\s*\"?\\s*(\\[[^\\]]+\\])").getMatch(0);
            if (sources_source != null) {
                logger.info("Found video json source");
                /*
                 * Different services store the values we want under different names. E.g. vidoza.net uses 'res', most providers use
                 * 'label'.
                 */
                final String[] possibleQualityObjectNames = new String[] { "desc", "label", "res" };
                /*
                 * Different services store the values we want under different names. E.g. vidoza.net uses 'src', most providers use 'file'.
                 */
                final String[] possibleStreamURLObjectNames = new String[] { "file", "src" };
                try {
                    /*
                     * Important: Default is -1 so that even if only one quality is available without quality-identifier, it will be used!
                     */
                    long quality_picked = -1;
                    String dllink_temp = null;
                    /*
                     * Important: Do not use "Plugin.restoreFromString" here as the input of this can also be js structure and not only
                     * json!!
                     */
                    final List<Object> ressourcelist = (List<Object>) JavaScriptEngineFactory.jsonToJavaObject(sources_source);
                    final boolean onlyOneQualityAvailable = ressourcelist.size() == 1;
                    int userSelectedQuality = -1;
                    if (false) {
                        // avoid eclipse error due to comparing identical constants
                        userSelectedQuality = 0;
                    }
                    if (userSelectedQuality == -1) {
                        logger.info("Looking for BEST video stream");
                    } else {
                        logger.info("Looking for user selected video stream quality: " + userSelectedQuality);
                    }
                    boolean foundUserSelectedQuality = false;
                    for (final Object videoo : ressourcelist) {
                        /* Check for single URL without any quality information e.g. uqload.com */
                        if (videoo instanceof String && onlyOneQualityAvailable) {
                            logger.info("Only one quality available --> Returning that");
                            dllink_temp = (String) videoo;
                            if (dllink_temp.startsWith("http")) {
                                dllink = dllink_temp;
                                break;
                            }
                        }
                        final Map<String, Object> entries;
                        if (videoo instanceof Map) {
                            entries = (Map<String, Object>) videoo;
                            for (final String possibleStreamURLObjectName : possibleStreamURLObjectNames) {
                                if (entries.containsKey(possibleStreamURLObjectName)) {
                                    dllink_temp = (String) entries.get(possibleStreamURLObjectName);
                                    break;
                                }
                            }
                        } else {
                            entries = null;
                        }
                        if (StringUtils.isEmpty(dllink_temp)) {
                            /* No downloadurl found --> Continue */
                            continue;
                        } else if (dllink_temp.contains(".mpd")) {
                            /* 2020-05-20: This plugin cannot yet handle DASH stream downloads */
                            logger.info("Skipping DASH stream: " + dllink_temp);
                            continue;
                        }
                        /* Find quality + downloadurl */
                        long quality_temp = 0;
                        for (final String possibleQualityObjectName : possibleQualityObjectNames) {
                            try {
                                final Object quality_temp_o = entries.get(possibleQualityObjectName);
                                if (quality_temp_o != null && quality_temp_o instanceof Number) {
                                    quality_temp = ((Number) quality_temp_o).intValue();
                                } else if (quality_temp_o != null && quality_temp_o instanceof String) {
                                    /* E.g. '360p' */
                                    final String res = new Regex((String) quality_temp_o, "(\\d+)p?$").getMatch(0);
                                    if (res != null) {
                                        quality_temp = (int) Long.parseLong(res);
                                    }
                                }
                                if (quality_temp > 0) {
                                    break;
                                }
                            } catch (final Throwable e) {
                                /* This should never happen */
                                logger.log(e);
                                logger.info("Failed to find quality via key '" + possibleQualityObjectName + "' for current downloadurl candidate: " + dllink_temp);
                                if (!onlyOneQualityAvailable) {
                                    continue;
                                }
                            }
                        }
                        if (StringUtils.isEmpty(dllink_temp)) {
                            continue;
                        } else if (quality_temp == userSelectedQuality) {
                            /* Found user selected quality */
                            logger.info("Found user selected quality: " + userSelectedQuality);
                            foundUserSelectedQuality = true;
                            quality_picked = quality_temp;
                            dllink = dllink_temp;
                            break;
                        } else {
                            /* Look for best quality */
                            if (quality_temp > quality_picked) {
                                quality_picked = quality_temp;
                                dllink = dllink_temp;
                            }
                        }
                    }
                    if (!StringUtils.isEmpty(dllink)) {
                        logger.info("Quality handling for multiple video stream sources succeeded - picked quality is: " + quality_picked);
                        if (foundUserSelectedQuality) {
                            logger.info("Successfully found user selected quality: " + userSelectedQuality);
                        } else {
                            logger.info("Successfully found BEST quality: " + quality_picked);
                        }
                    } else {
                        logger.info("Failed to find any stream downloadurl");
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                    logger.info("BEST handling for multiple video source failed");
                }
            }
        }
        if (dllink != null) {
            dllink = dllink.replace("\\", "");
        }
        return dllink;
    }

    public static boolean inValidateDllink(final String dllink) {
        if (dllink == null) {
            return true;
        } else if (dllink.endsWith(".vtt")) {
            /* We picked up the subtitle url instead of the video downloadurl! */
            return true;
        } else {
            return false;
        }
    }

    private boolean requiresAccount(final Browser br) {
        if (br.containsHTML(">\\s*To watch this video please")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account, true);
        if (requiresAccount(br)) {
            throw new AccountRequiredException();
        } else if (this.directurls.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        int index = 0;
        findWorkingDirecturl: for (final String directurl : this.directurls) {
            logger.info("Checking item " + (index + 1) + "/" + this.directurls.size() + " -> " + directurl);
            final boolean isLastItem = index == this.directurls.size() - 1;
            try {
                if (this.isHLS(directurl)) { // bigcamtube.com
                    /* hls download */
                    final String hlsurl;
                    if (directurl.contains(".m3u8")) {
                        /* Access hls master. */
                        br.getPage(directurl);
                        if (br.getHttpConnection().getResponseCode() == 403) {
                            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                        } else if (br.getHttpConnection().getResponseCode() == 404) {
                            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                        }
                        final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(br));
                        if (hlsbest == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                        hlsurl = hlsbest.getDownloadurl();
                    } else {
                        hlsurl = directurl;
                    }
                    checkFFmpeg(link, "Download a HLS Stream");
                    dl = new HLSDownloader(link, br, hlsurl);
                    break;
                } else {
                    dl = new jd.plugins.BrowserAdapter().openDownload(br, link, directurl, true, 0);
                    handleConnectionErrors(br, dl.getConnection());
                    break findWorkingDirecturl;
                }
            } catch (final InterruptedException ie) {
                throw ie;
            } catch (final Exception e) {
                if (isLastItem) {
                    throw e;
                }
                logger.info("Skipping invalid/broken stream: " + directurl);
            }
            index++;
        }
        dl.startDownload();
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("/logout");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        return ai;
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                this.br.setCookies(this.getHost(), cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost() + "/");
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(this.br.getCookies(this.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    account.clearCookies("");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/login.php");
            final Form loginform = br.getFormbyProperty("name", "loginForm");
            if (loginform == null) {
                logger.warning("Failed to find loginform");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (CaptchaHelperHostPluginRecaptchaV2.containsRecaptchaV2Class(loginform)) {
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                loginform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            }
            loginform.put("login", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            loginform.put("rememberMe", "1");
            final Request req = br.createFormRequest(loginform);
            req.getHeaders().put("x-requested-with", "XMLHttpRequest");
            br.getPage(req);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if ((Boolean) entries.get("SUCCESS") != Boolean.TRUE) {
                throw new AccountInvalidException();
            }
            /* Double-check */
            br.getPage(entries.get("URL").toString());
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(this.br.getCookies(this.getHost()), "");
            return true;
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    private String regexStandardTitleWithHost(final String host) {
        final String[] hostparts = host.split("\\.");
        final String host_relevant_part = hostparts[0];
        String site_title = br.getRegex(Pattern.compile("<title>([^<>\"]*?) \\- " + Pattern.quote(host) + "</title>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL)).getMatch(0);
        if (site_title == null) {
            site_title = br.getRegex(Pattern.compile("<title>([^<>\"]*?) at " + Pattern.quote(host) + "</title>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL)).getMatch(0);
        }
        if (site_title == null) {
            site_title = br.getRegex(Pattern.compile("<title>([^<>\"]*?) at " + host_relevant_part + "</title>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL)).getMatch(0);
        }
        return site_title;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.UnknownPornScript5;
    }
}
