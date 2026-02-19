//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.VoeSxCrawler;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigVideo;
import org.jdownloader.plugins.components.config.XFSConfigVideo.DownloadMode;
import org.jdownloader.plugins.components.config.XFSConfigVideo.PreferredDownloadQuality;
import org.jdownloader.plugins.components.config.XFSConfigVideoVoeSx;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@HostPlugin(revision = "$Revision: 52336 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { VoeSxCrawler.class })
public class VoeSx extends XFileSharingProBasic {
    public VoeSx(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    public String getPurchasePremiumURL() {
        return this.getMainPage() + "/register";
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2020-11-27: Premium untested, set FREE limits <br />
     * captchatype-info: 2020-08-19: null<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        return VoeSxCrawler.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return VoeSxCrawler.getAnnotationUrls();
    }

    @Override
    public String getFUIDFromURL(final DownloadLink dl) {
        try {
            final String result = new Regex(new URL(dl.getPluginPatternMatcher()).getPath(), "/(?:embed-|e/)?([a-z0-9]{12})").getMatch(0);
            return result;
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return null;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 0;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        /* 2020-08-19 */
        return true;
    }

    @Override
    protected boolean supportsAPIMassLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean supportsAPISingleLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean isVideohosterEmbed() {
        /* 2021-03-09 */
        return true;
    }

    private static Map<String, String> LOADER_CACHE   = new HashMap<String, String>();
    static {
        synchronized (LOADER_CACHE) {
            LOADER_CACHE.put("/js/loader.c7381b2.js", "['@$','^^','~@','%?','*~','!!','#&']");
        }
    }
    protected String                   javaScriptJSON = null;

    public String getJavaScriptJSON() {
        return javaScriptJSON;
    }

    public String getDllinkVideohostJavaScript(DownloadLink link, Account account, Browser br, final String src) {
        final ScriptEngineManager manager = JavaScriptEngineFactory.getScriptEngineManager(this);
        final ScriptEngine engine = manager.getEngineByName("javascript");
        try {
            String input = br.getRegex("MKGMa\\s*=\\s*(\"|')(.*?)\\1").getMatch(1);
            if (input == null) {
                input = br.getRegex("<script[^>]*type\\s*=\\s*\"application/json\\s*\"[^>]*>\\s*\\[\\s*\"(.*?)\"").getMatch(0);
            }
            if (input == null) {
                return null;
            }
            String replace = br.getRegex("=\\s*(\\[\\s*(?:'[^']{2,}'\\s*,?){5,8}\\])\\s*,").getMatch(0);
            if (replace == null) {
                final String loader = br.getRegex("<script[^>]*src\\s*=\\s*\"(/js/loader.*?)\"").getMatch(0);
                if (loader != null) {
                    synchronized (LOADER_CACHE) {
                        replace = LOADER_CACHE.get(loader);
                        if (replace == null) {
                            // avoid potential DDOS-GUARD blocking
                            final Browser brc = br.cloneBrowser();
                            brc.getPage(loader);
                            replace = brc.getRegex("=\\s*(\\[\\s*(?:'[^']{2,}'\\s*,?){5,8}\\])\\s*,").getMatch(0);
                            LOADER_CACHE.put(loader, replace);
                        }
                    }
                }
            }
            if (replace == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            String js = new String(
                    HexFormatter
                    .hexToByteArray("2020202020202066756e6374696f6e205f3078333734616131285f307834376365313129207b0a2020202020202009766172205f3078343466643663203d2027273b0a2020202020202009666f722028766172205f3078343730306232203d203078303b205f3078343730306232203c205f30783437636531312e6c656e6774683b205f30783437303062322b2b29207b0a202020202020200909766172205f3078313262623462203d205f30783437636531312e63686172436f64654174285f3078343730306232293b0a202020202020200909696620285f3078313262623462203e3d2030783431202626205f3078313262623462203c3d203078356129207b0a202020202020200909095f3078313262623462203d20285f3078313262623462202d2030783431202b203078642920252030783161202b20307834313b0a2020202020202009097d20656c7365207b0a20202020202020090909696620285f3078313262623462203e3d2030783631202626205f3078313262623462203c3d203078376129207b0a20202020202020090909095f3078313262623462203d20285f3078313262623462202d2030783631202b203078642920252030783161202b20307836313b0a202020202020200909097d0a2020202020202009097d0a2020202020202009095f3078343466643663202b3d20537472696e672e66726f6d43686172436f6465285f3078313262623462293b0a20202020202020097d0a202020202020200972657475726e205f30783434666436633b0a202020202020207d0a0a2020202020202066756e6374696f6e205f3078326539633565285f307835366237613229207b0a2020202020202009766172205f3078363138663136203d205b272358272c20272551272c20222a414243222c20277e5a272c20273f3f272c20272140272c20275e26275d3b0a2020202020202009766172205f3078343235623566203d205f30783536623761323b0a2020202020202009666f722028766172205f3078353463623566203d203078303b205f3078353463623566203c205f30783631386631362e6c656e6774683b205f30783534636235662b2b29207b0a202020202020200909766172205f3078333063346332203d205f30783631386631365b5f30783534636235665d3b0a202020202020200909766172205f3078326331626364203d206e657720526567457870285f30783330633463322e7265706c616365282f5b2e2a2b3f5e247b7d28297c5b5c5d5c5c5d2f672c20225c5c242622292c20276727293b0a2020202020202009095f3078343235623566203d205f30783432356235662e7265706c616365285f30783263316263642c20275f27293b0a20202020202020097d0a202020202020200972657475726e205f30783432356235663b0a202020202020207d0a0a2020202020202066756e6374696f6e205f3078353333653061285f30783432343933362c205f307831346439313029207b0a2020202020202009766172205f3078316230376164203d205b5d3b0a2020202020202009666f722028766172205f3078316333353531203d203078303b205f3078316333353531203c205f30783432343933362e6c656e6774683b205f30783163333535312b2b29207b0a2020202020202009095f30783162303761642e7075736828537472696e672e66726f6d43686172436f6465285f30783432343933362e63686172436f64654174285f307831633335353129202d205f307831346439313029293b0a20202020202020097d0a202020202020200972657475726e205f30783162303761642e6a6f696e282727293b0a202020202020207d0a0a2020202020202066756e6374696f6e205f3078343335353163285f307834323937323529207b0a2020202020202009766172205f3078353032333433203d205f3078333734616131285f3078343239373235293b0a2020202020202009766172205f3078356538363061203d205f3078326539633565285f3078353032333433293b0a2020202020202009766172205f3078333162323939203d205f30783565383630612e73706c697428275f27292e6a6f696e282727293b0a2020202020202009766172205f3078343835396438203d2061746f62285f3078333162323939293b0a2020202020202009766172205f3078343431323835203d205f3078353333653061285f30783438353964382c20307833293b0a2020202020202009766172205f3078373530666162203d205f30783434313238352e73706c6974282727292e7265766572736528292e6a6f696e282727293b0a2020202020202009766172205f3078333032346139203d2061746f62285f3078373530666162293b0a2020202020202009766172205f30783362383839333b0a2020202020202009747279207b0a2020202020202009095f3078336238383933203d204a534f4e2e7061727365285f3078333032346139293b0a20202020202020097d20636174636820285f307831306430323729207b0a2020202020202009095f3078336238383933203d207b7d3b0a20202020202020097d0a202020202020200972657475726e205f30783362383839333b0a202020202020207d0a0a2020202020202066756e6374696f6e2061746f62286629207b0a20202020202020097661722067203d207b7d2c0a20202020202020090962203d2036352c0a20202020202020090964203d20302c0a202020202020200909612c2063203d20302c0a202020202020200909682c2065203d2027272c0a2020202020202009096b203d20537472696e672e66726f6d43686172436f64652c0a2020202020202009096c203d20662e6c656e6774683b0a2020202020202009666f72202861203d2027273b203931203e20623b292061202b3d206b28622b2b293b0a202020202020200961202b3d20612e746f4c6f776572436173652829202b2027303132333435363738392b2f273b0a2020202020202009666f72202862203d20303b203634203e20623b20622b2b2920675b612e6368617241742862295d203d20623b0a2020202020202009666f72202861203d20303b2061203c206c3b20612b2b290a202020202020200909666f72202862203d20675b662e6368617241742861295d2c2064203d202864203c3c203629202b20622c2063202b3d20363b2038203c3d20633b29282868203d2064203e3e3e202863202d3d20382920262032353529207c7c2061203c206c202d203229202626202865202b3d206b286829293b0a202020202020200972657475726e20650a202020202020207d3b0a"),
                    "UTF-8");
            js = js.replace("['#X', '%Q', \"*ABC\", '~Z', '??', '!@', '^&']", replace);
            engine.eval(js);
            engine.eval("var result = _0x43551c(\"" + input + "\");");
            final Object result = engine.get("result");
            javaScriptJSON = JSonStorage.toString(result);
            return parseMap(javaScriptJSON);
        } catch (Exception e) {
            logger.log(e);
            return null;
        }
    }

    @Override
    public void clean() {
        javaScriptJSON = null;
        super.clean();
    }

    private String parseMap(String jsonMap) {
        final Map<String, Object> entries = restoreFromString(jsonMap, TypeRef.MAP);
        final List<Map<String, Object>> fallbacks = (List<Map<String, Object>>) entries.get("fallbacks");
        if (fallbacks != null && fallbacks.size() == 1) {
            final String mp4 = (String) fallbacks.get(0).get("file");
            if (mp4 != null) {
                // return mp4;
            }
        }
        String hlsMaster = (String) entries.get("file");
        if (hlsMaster == null) {
            hlsMaster = (String) entries.get("source");
        }
        return hlsMaster;
    }

    @Override
    protected String getDllinkVideohost(DownloadLink link, Account account, Browser br, final String src) {
        {
            final String jsResult = getDllinkVideohostJavaScript(link, account, br, src);
            if (jsResult != null) {
                return jsResult;
            }
        }
        {
            final String mp4Master = new Regex(src, "(?i)(\"|')mp4\\1\\s*:\\s*(\"|')(https?://[^\"']+)").getMatch(2);
            if (mp4Master != null) {
                return mp4Master;
            }
        }
        {
            String hlsMaster = new Regex(src, "(?i)(\"|')hls\\1\\s*:\\s*(\"|')(https?://[^\"']+)").getMatch(2);
            if (hlsMaster == null) {
                /* 2023-11-21 */
                hlsMaster = new Regex(src, "(?i)\"(https?://[^/]+/engine/hls[^\"]+)").getMatch(0);
            }
            if (hlsMaster != null) {
                return hlsMaster;
            }
        }
        {
            final String hlsMasterB64 = br.getRegex("'hls'\\s*:\\s*'(aHR0[^']+)").getMatch(0);
            if (hlsMasterB64 != null) {
                final String ret = Encoding.Base64Decode(hlsMasterB64);
                return ret;
            }
        }
        {
            String altSourceB64 = br.getRegex("(?:var|let|const)\\s*wc0\\s*=\\s*'([^\\']+)").getMatch(0);
            if (altSourceB64 == null) {
                /* 2024-02-23 */
                altSourceB64 = br.getRegex("(?:var|let|const)\\s*[^=]+\\s*=\\s*'(ey[^\\']+)").getMatch(0);
                if (altSourceB64 == null) {
                    /* 2024-02-26 */
                    altSourceB64 = br.getRegex("(?:var|let|const)\\s*[a-f0-9]+\\s*=\\s*'([^\\']+)").getMatch(0);
                    if (altSourceB64 == null) {
                        /* 2024-11-29 */
                        altSourceB64 = br.getRegex("(?i)(\"|')hls\\1\\s*:\\s*(\"|')([^\"']+)").getMatch(2);
                    }
                }
            }
            if (altSourceB64 != null) {
                String input = Encoding.Base64Decode(altSourceB64);
                if (input.startsWith("}")) {
                    /* 2024-02-26 */
                    input = new StringBuilder(input).reverse().toString();
                }
                if (StringUtils.startsWithCaseInsensitive(input, "http")) {
                    /* Result is url */
                    return input;
                } else {
                    /* Assume that result is json */
                    try {
                        final String ret = parseMap(input);
                        if (ret != null) {
                            return ret;
                        }
                    } catch (Exception e) {
                        logger.log(e);
                    }
                }
            }
        }
        /* Fallback */
        return super.getDllinkVideohost(link, account, br, src);
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account);
        // if (br.containsHTML(">\\s*This video can be watched as embed only")) {
        // throw new PluginException(LinkStatus.ERROR_FATAL, "This video can be watched as embed only");
        // }
        if (br.containsHTML(">\\s*Server overloaded, download temporary disabled|The server of this file is currently over")) {
            /* 2023-10-26 */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server overloaded");
        } else if (br.containsHTML(">\\s*Access to this file has been temporarily restricted")) {
            /* 2023-11-29 */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Access to this file has been temporarily restricted");
        }
        final String encodingError1 = br.getRegex(">\\s*(File is in the encoding queue\\. Current position: #\\d+)<").getMatch(0);
        if (encodingError1 != null) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, encodingError1, 10 * 60 * 1000);
        }
        if (br.containsHTML(">\\s*Encoding in progress, please wait") || br.containsHTML("class=\"progress-bar encoding-process\"")) {
            String encodingError2 = "Encoding in progress, please wait";
            long waitMillis = 5 * 60 * 1000;
            try {
                final Browser brc = br.cloneBrowser();
                brc.getPage("/engine/status?fileCode=" + this.getFUIDFromURL(link));
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final int waitSeconds = ((Number) entries.get("encodingWaitingSeconds")).intValue();
                if (waitSeconds > 0) {
                    waitMillis = waitSeconds * 1000;
                }
                final String state = entries.get("state").toString();
                if (!StringUtils.isEmpty(state)) {
                    /*
                     * e.g. Das Video wird jetzt verarbeitet. Live-Codierungsstatus: <b>74 / 100%</b> - geschätzte Wartezeit: <b>3 Minuten
                     * 40 Sekunden</b>
                     */
                    encodingError2 = state;
                }
            } catch (final Throwable e) {
                logger.log(e);
                logger.info("Failed to find detailed encoding in progress status info");
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, encodingError2, waitMillis);
        }
    }

    @Override
    protected boolean isServerUnderMaintenance(final Browser br) {
        if (br.containsHTML("<title>\\s*Maintenance Mode\\s*</title>")) {
            return true;
        } else {
            return super.isServerUnderMaintenance(br);
        }
    }

    @Override
    public AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account) throws Exception {
        if (link.getPluginPatternMatcher().matches("(?i)https?://[^/]+/(e/|embed-).+")) {
            /* 2021-03-09: Special: New browser required else they won't let us stream some videos at all! */
            final boolean embedOnly = br.containsHTML(">\\s*This video can be watched as embed only");
            br.setFollowRedirects(true);
            boolean fallBackFileName = true;
            getPage(this.getMainPage(link) + "/e/" + this.getFUIDFromURL(link));
            if (this.isOffline(link, br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String[] fileInfo = internal_getFileInfoArray();
            scanInfo(br.getRequest().getHtmlCode(), fileInfo);
            processFileInfo(fileInfo, br, link);
            if (!StringUtils.isEmpty(fileInfo[0])) {
                /* Correct- and set filename */
                setFilename(fileInfo[0], link, br);
                fallBackFileName = false;
            } else {
                /*
                 * Fallback. Do this again as now we got the html code available so we can e.g. know if this is a video-filehoster or not.
                 */
                fallBackFileName = true;
            }
            final String dllink = getDllinkVideohost(link, account, br, br.getRequest().getHtmlCode());
            if (StringUtils.isEmpty(dllink) && embedOnly) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "This video can be watched as embed only");
            }
            return AvailableStatus.TRUE;
        } else {
            return super.requestFileInformationWebsite(link, account);
        }
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        String betterTitle = new Regex(html, "class=\"player-title\"[^>]*>([^<]+)").getMatch(0);
        if (betterTitle == null) {
            /* 2024-01-03 */
            betterTitle = new Regex(html, "name=\"og:title\" content=\"([^\"]+)").getMatch(0);
        }
        if (betterTitle != null) {
            fileInfo[0] = betterTitle;
        }
        return fileInfo;
    }

    @Override
    protected void processFileInfo(String[] fileInfo, Browser altbr, DownloadLink link) {
        if (fileInfo != null && fileInfo[0] != null) {
            fileInfo[0] = fileInfo[0].replaceFirst("(\\s*-\\s*VOE\\s*\\|\\s*Content\\s*Delivery\\s*Network\\s*\\(CDN\\)\\s*&\\s*Video\\s*Cloud)", "");
        }
        super.processFileInfo(fileInfo, altbr, link);
    }

    @Override
    protected String getDllinkViaOfficialVideoDownload(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            return null;
        }
        logger.info("[DownloadMode] Trying to find official video downloads");
        String continueLink = br.getRegex("(?:\"|')(/dl\\?op=download_orig[^\"\\']+)").getMatch(0);
        if (continueLink == null) {
            /* 2023-10-07 */
            continueLink = br.getRegex("(?:\"|')((https?://[^/]+)?/[a-z0-9]{12}/download)").getMatch(0);
        }
        if (continueLink == null) {
            /* No official download available */
            logger.info("Failed to find any official video downloads");
            return null;
        }
        final String streamDownloadlink = getDllinkVideohost(link, account, br, br.getRequest().getHtmlCode());
        final DownloadMode mode = this.getPreferredDownloadModeFromConfig();
        if (streamDownloadlink != null && mode == DownloadMode.STREAM) {
            return null;
        } else if (streamDownloadlink != null && mode == DownloadMode.AUTO && Boolean.TRUE.equals(requiresCaptchaForOfficialVideoDownload()) && (account == null || !AccountType.PREMIUM.equals(account.getType()))) {
            /*
             * User wants to download stream in free- or free account mode. Obtaining an official downloadlink would require the user to
             * enter a captcha -> Skip that.
             */
            return null;
        }
        if (br.containsHTML("&embed=&adb=")) {
            /* 2022-08-24: This might give us more download-speed, not sure though. */
            continueLink += "&embed=&adb=0";
        }
        getPage(br, continueLink);
        final Form dlform = br.getFormbyActionRegex("(?i).+/download$");
        if (dlform != null) {
            try {
                reCaptchaSiteurlWorkaround = br.getURL();
                this.handleCaptcha(link, br, dlform);
            } finally {
                reCaptchaSiteurlWorkaround = null;
            }
            this.submitForm(br, dlform);
        }
        final String[] directurls = br.getRegex("\"(https?://[^/]+/engine/download/[^\"]+)\"").getColumn(0);
        if (directurls == null || directurls.length == 0) {
            logger.warning("Failed to find dllink via official video download");
            return null;
        }
        String dllink = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
        final String userSelectedQualityValue = getPreferredDownloadQualityStr();
        if (userSelectedQualityValue != null) {
            /* Try to find user preferred quality */
            for (final String directurl : directurls) {
                if (StringUtils.containsIgnoreCase(directurl, "_" + userSelectedQualityValue + ".mp4")) {
                    dllink = directurl;
                    break;
                }
            }
            if (dllink == null) {
                logger.info("Failed to find user preferred quality: " + userSelectedQualityValue);
            } else {
                logger.info("Found user preferred quality: " + userSelectedQualityValue);
            }
        }
        if (dllink == null) {
            /* Fallback/best */
            dllink = directurls[directurls.length - 1];
        }
        logger.info("Successfully found dllink via official video download");
        dllink = Encoding.htmlOnlyDecode(dllink);
        final String filesizeBytesStr = br.getRegex("File Size \\(bytes\\)</td>\\s*<td>\\s*(\\d+)\\s*<").getMatch(0);
        if (filesizeBytesStr != null) {
            link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
        }
        return dllink;
    }

    protected String getPreferredDownloadQualityHeightStr() {
        final Class<? extends XFSConfigVideo> cfgO = getVideoConfigInterface();
        if (cfgO == null) {
            return null;
        }
        final XFSConfigVideo cfg = PluginJsonConfig.get(cfgO);
        final PreferredDownloadQuality quality = cfg.getPreferredDownloadQuality();
        switch (quality) {
        case HIGH:
            return "1080p";
        case NORMAL:
            return "720p";
        case LOW:
            return "480p";
        case BEST:
        default:
            return null;
        }
    }

    @Override
    public Form findLoginform(final Browser br) {
        final Form loginform = br.getFormbyActionRegex(".*/login$");
        if (loginform != null) {
            return loginform;
        } else {
            return super.findLoginform(br);
        }
    }

    @Override
    public boolean isLoggedin(final Browser brc) {
        final boolean logout = brc.containsHTML("/logout");
        boolean cookie1 = false;
        boolean cookie2 = false;
        for (final Cookies cookies : brc.getCookies().values()) {
            for (final Cookie cookie : cookies.getCookies()) {
                if (cookie.getKey().matches("remember_web_.+")) {
                    cookie1 = true;
                } else if (cookie.getKey().equalsIgnoreCase("voe_session")) {
                    cookie2 = true;
                }
            }
        }
        logger.info("cookie1:" + cookie1);
        logger.info("cookie2:" + cookie2);
        logger.info("logout:" + logout);
        if (logout && (cookie1 || cookie2)) {
            return true;
        } else {
            return super.isLoggedin(brc);
        }
    }

    private String reCaptchaSiteurlWorkaround = null;

    @Override
    protected CaptchaHelperHostPluginRecaptchaV2 getCaptchaHelperHostPluginRecaptchaV2(PluginForHost plugin, Browser br) throws PluginException {
        return new CaptchaHelperHostPluginRecaptchaV2(this, br) {
            @Override
            protected String getSiteUrl() {
                if (reCaptchaSiteurlWorkaround != null) {
                    return reCaptchaSiteurlWorkaround;
                } else {
                    return super.getSiteUrl();
                }
            }
        };
    }

    @Override
    protected void runPostRequestTask(final Browser ibr) throws Exception {
        super.runPostRequestTask(ibr);
        String redirect = ibr.getRegex("else \\{\\s*window\\.location\\.href = '(https?://[^\"\\']+)';").getMatch(0);
        if (redirect == null) {
            return;
        }
        /*
         * 2025-03-14: Workaround for server side bug where links ending with "/download" will do endless redirects on themselves, ending in
         * http response 429 too many requests.
         */
        redirect = redirect.replaceFirst("/download$", "");
        final String fuid = this.getFUIDFromURL(this.getDownloadLink());
        if (canHandle(redirect) || (fuid != null && redirect.endsWith("/" + fuid))) {
            logger.info("Handle special js redirect: " + redirect);
            getPage(ibr, redirect);
        } else {
            logger.info("Unuspported domain for special js redirect: " + redirect);
        }
    }

    @Override
    protected Long findExpireTimestamp(final Account account, final Browser br, AtomicBoolean isPreciseTimestampFlag) throws Exception {
        final String[] expiredates = br.getRegex("(\\d{1,2}/\\d{1,2}/\\d{4} \\d{2}:\\d{2})").getColumn(0);
        if (expiredates == null || expiredates.length == 0) {
            /* Fallback */
            return super.findExpireTimestamp(account, br, isPreciseTimestampFlag);
        }
        /* User can own multiple premium packages at the same time -> Return highest expire date value */
        long highestExpireTimestamp = -1;
        for (final String expiredate : expiredates) {
            final long timestamp = TimeFormatter.getMilliSeconds(expiredate, "dd/MM/yyyy HH:mm", Locale.ENGLISH);
            if (timestamp > highestExpireTimestamp) {
                highestExpireTimestamp = timestamp;
            }
        }
        isPreciseTimestampFlag.set(true);
        return highestExpireTimestamp;
    }

    @Override
    protected AccountInfo fetchAccountInfoAPI(final Browser br, final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> entries = loginAPI(br, account);
        /** 2019-07-31: Better compare expire-date against their serverside time if possible! */
        final String server_timeStr = (String) entries.get("server_time");
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        long expire_milliseconds_precise_to_the_second = 0;
        final long currentTime;
        if (server_timeStr != null && server_timeStr.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            currentTime = TimeFormatter.getMilliSeconds(server_timeStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        } else {
            /* Fallback */
            currentTime = br.getCurrentServerTime(System.currentTimeMillis());
        }
        String expireStr = (String) result.get("premium_expire");
        if (StringUtils.isEmpty(expireStr)) {
            /* 2024-12-16: voe.sx custom field. */
            expireStr = (String) result.get("premium_until");
        }
        if (expireStr != null && expireStr.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            expire_milliseconds_precise_to_the_second = TimeFormatter.getMilliSeconds(expireStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        }
        ai.setUnlimitedTraffic();
        final long premiumDurationMilliseconds = expire_milliseconds_precise_to_the_second - currentTime;
        if (premiumDurationMilliseconds <= 0) {
            /* Expired premium or no expire date given --> Free Account */
            setAccountLimitsByType(account, AccountType.FREE);
        } else {
            /* Expire date is in the future --> Premium account */
            ai.setValidUntil(System.currentTimeMillis() + premiumDurationMilliseconds);
            setAccountLimitsByType(account, AccountType.PREMIUM);
        }
        final String premium_bandwidthBytesStr = (String) result.get("premium_bandwidth"); // Double as string
        final String traffic_leftBytesStr = (String) result.get("traffic_left");
        if (premium_bandwidthBytesStr != null) {
            ai.setTrafficLeft(SizeFormatter.getSize(premium_bandwidthBytesStr));
        } else if (traffic_leftBytesStr != null) {
            ai.setTrafficLeft(SizeFormatter.getSize(traffic_leftBytesStr));
        }
        {
            /* Now set less relevant account information */
            final Object balanceO = result.get("balance"); // Double returned as string
            if (balanceO != null) {
                final String balanceStr = balanceO.toString();
                if (balanceStr.matches("[0-9.]+")) {
                    ai.setAccountBalance(Double.parseDouble(balanceStr), Currency.getInstance("USD"));
                }
            }
            /* 2019-07-26: values can also be "inf" for "Unlimited": "storage_left":"inf" */
            // final long storage_left = JavaScriptEngineFactory.toLong(entries.get("storage_left"), 0);
            final Object storage_usedO = result.get("storage_used");
            if (storage_usedO != null) {
                ai.setUsedSpace(SizeFormatter.getSize(storage_usedO.toString()));
            }
        }
        final Object files_totalO = result.get("files_total");
        if (files_totalO instanceof Number) {
            ai.setFilesNum(((Number) files_totalO).intValue());
        }
        final String email = (String) result.get("email");
        if (this.enableAccountApiOnlyMode() && !StringUtils.isEmpty(email)) {
            /*
             * Each account is unique. Do not care what the user entered - trust what API returns! <br> This is not really important - more
             * visually so that something that makes sense is displayed to the user in his account managers' "Username" column!
             */
            account.setUser(email);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Devs only */
            String accStatus;
            if (ai.getStatus() != null && !ai.getStatus().startsWith("[API] ")) {
                accStatus = ai.getStatus();
            } else {
                accStatus = account.getType().toString();
            }
            ai.setStatus("[API] | DLs: " + allowAPIDownloadIfApikeyIsAvailable(null, account) + " | " + accStatus);
        }
        return ai;
    }

    @Override
    public boolean massLinkcheckerAPI(final DownloadLink[] urls, final String apikey) {
        if (urls == null || urls.length == 0) {
            return false;
        } else if (!this.looksLikeValidAPIKey(apikey)) {
            throw new IllegalArgumentException("apikey cannot be null");
        }
        boolean linkcheckerHasFailed = false;
        final String preferredQualityHeightStr = getPreferredDownloadQualityHeightStr();
        try {
            final Browser br = createNewBrowserInstance();
            this.prepBrowser(br, getMainPage());
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /*
                     * We test max 50 links at once. 2020-05-29: XFS default API linkcheck limit is exactly 50 items. If you check more than
                     * 50 items, it will only return results for the first 50 items.
                     */
                    if (index == urls.length || links.size() == 50) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                final List<DownloadLink> apiLinkcheckLinks = new ArrayList<DownloadLink>();
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    try {
                        resolveShortURL(br.cloneBrowser(), link, null);
                    } catch (final PluginException e) {
                        logger.log(e);
                        if (e.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                            link.setAvailableStatus(AvailableStatus.FALSE);
                        } else if (e.getLinkStatus() == LinkStatus.ERROR_IP_BLOCKED) {
                            link.setAvailableStatus(AvailableStatus.TRUE);
                        } else {
                            link.setAvailableStatus(AvailableStatus.UNCHECKABLE);
                        }
                        /*
                         * We cannot check shortLinks via API so if we're unable to convert them to TYPE_NORMAL we basically already checked
                         * them here. Also we have to avoid sending wrong fileIDs to the API otherwise linkcheck WILL fail!
                         */
                        continue;
                    }
                    sb.append(this.getFUIDFromURL(link));
                    sb.append("%2C");
                    apiLinkcheckLinks.add(link);
                }
                getPage(br, getAPIBase() + "/file/info?key=" + apikey + "&file_code=" + sb.toString());
                Map<String, Object> entries = null;
                try {
                    entries = this.checkErrorsAPI(br, links.get(0), null);
                } catch (final Throwable e) {
                    logger.log(e);
                    /* E.g. invalid apikey, broken serverside API, developer mistake (e.g. sent fileIDs in invalid format) */
                    logger.info("Fatal failure");
                    return false;
                }
                final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("result");
                for (final DownloadLink link : apiLinkcheckLinks) {
                    Map<String, Object> fileInfo = null;
                    final String thisFUID = this.getFUIDFromURL(link);
                    for (final Map<String, Object> fileInfoTmp : ressourcelist) {
                        String fuid_temp = (String) fileInfoTmp.get("filecode");
                        if (StringUtils.isEmpty(fuid_temp)) {
                            /* 2022-08-09 */
                            fuid_temp = (String) fileInfoTmp.get("file_code");
                            if (StringUtils.isEmpty(fuid_temp)) {
                                /* 2024-12-16: Special voe.sx */
                                fuid_temp = (String) fileInfoTmp.get("fileCode");
                            }
                        }
                        if (StringUtils.equals(fuid_temp, thisFUID)) {
                            fileInfo = fileInfoTmp;
                            break;
                        }
                    }
                    if (fileInfo == null) {
                        /**
                         * This should never happen. Possible reasons: <br>
                         * - Wrong APIKey <br>
                         * - We tried to check too many items at once <br>
                         * - API only allows users to check self-uploaded content --> Disable API linkchecking in plugin! <br>
                         * - API does not not allow linkchecking at all --> Disable API linkchecking in plugin! <br>
                         */
                        logger.warning("WTF failed to find information for fuid: " + this.getFUIDFromURL(link));
                        linkcheckerHasFailed = true;
                        continue;
                    }
                    /* E.g. check for "result":[{"status":404,"filecode":"xxxxxxyyyyyy"}] */
                    final int status = ((Number) fileInfo.get("status")).intValue();
                    String filename = null;
                    boolean isVideohost = false;
                    if (status != 200) {
                        link.setAvailable(false);
                    } else {
                        link.setAvailable(true);
                        filename = (String) fileInfo.get("name");
                        if (StringUtils.isEmpty(filename)) {
                            filename = (String) fileInfo.get("file_title");
                        }
                        Number filesize = null;
                        if (preferredQualityHeightStr != null) {
                            filesize = (Number) fileInfo.get("size_" + preferredQualityHeightStr);
                        }
                        if (filesize == null) {
                            /* Fallback/default */
                            /* Look for 1080p first since they put the middle quality size (720p) in the field "file_size". */
                            filesize = (Number) fileInfo.get("size_1080p");
                            if (filesize == null) {
                                filesize = (Number) fileInfo.get("file_size");
                            }
                        }
                        final Object canplay = fileInfo.get("canplay");
                        final Object views_started = fileInfo.get("views_started");
                        final Object views = fileInfo.get("views");
                        final Object length = fileInfo.get("length");
                        isVideohost = canplay != null || views_started != null || views != null || length != null;
                        /* Filesize is not always given especially not for videohosts. */
                        if (filesize != null) {
                            link.setDownloadSize(filesize.longValue());
                        }
                    }
                    if (!isVideohost) {
                        isVideohost = this.internal_isVideohoster_enforce_video_filename(link, null);
                    }
                    if (!StringUtils.isEmpty(filename)) {
                        /*
                         * At least for videohosts, filenames from json would often not contain a file extension!
                         */
                        if (Encoding.isHtmlEntityCoded(filename)) {
                            filename = Encoding.htmlDecode(filename).trim();
                        }
                        if (isVideohost) {
                            filename = this.applyFilenameExtension(filename, ".mp4");
                        }
                        /* Trust API filenames -> Set as final filename. */
                        link.setFinalFileName(filename);
                    } else {
                        /* Use cached name */
                        final String name = link.getName();
                        if (name != null && isVideohost) {
                            link.setName(this.applyFilenameExtension(filename, ".mp4"));
                        }
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        } finally {
            if (linkcheckerHasFailed) {
                logger.info("Seems like massLinkcheckerAPI availablecheck is not supported by this host or currently broken");
            }
        }
        if (linkcheckerHasFailed) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public Class<? extends XFSConfigVideoVoeSx> getConfigInterface() {
        return XFSConfigVideoVoeSx.class;
    }

    @Override
    protected Boolean requiresCaptchaForOfficialVideoDownload() {
        // TODO: Add override annotation once this gets added to XFS core
        return Boolean.TRUE;
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        // 2024-07-04
        return false;
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        // 2024-07-04
        return false;
    }

    @Override
    protected String getRelativeAccountInfoURL() {
        return "/settings";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("^[A-Za-z0-9]{64}$")) {
            return true;
        } else {
            return false;
        }
    }
}