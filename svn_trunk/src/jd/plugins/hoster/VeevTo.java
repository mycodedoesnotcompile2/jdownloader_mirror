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
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 53337 $", interfaceVersion = 3, names = {}, urls = {})
public class VeevTo extends XFileSharingProBasic {
    public VeevTo(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2024-04-10: reCaptchaV2 <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "veev.to", "doods.to" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        /**
         * 2025-03-20: This domain is not dead but such links will fail even in browser due to wrong domain for their Cloudflare Turnstile
         * captcha. <br>
         * Marking it as dead here will work around this problem.
         */
        deadDomains.add("doods.to");
        return deadDomains;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return VeevTo.buildAnnotationUrlsVeevToSpecial(getPluginDomains());
    }

    private static final Pattern PATTERN_SPECIAL = Pattern.compile("/(d|e)/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrlsVeevToSpecial(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + PATTERN_SPECIAL.pattern());
        }
        return ret.toArray(new String[0]);
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
    protected String getDllinkViaOfficialVideoDownloadNew(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            logger.info("[FilesizeMode] Trying to find official video downloads");
            return null;
        }
        final Form dlform = br.getFormbyActionRegex(".*/dl");
        if (dlform == null) {
            // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            /* 2024-07-08 */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Uploader has disabled downloads for this file");
        }
        final Form preForm = new Form();
        preForm.setMethod(MethodType.POST);
        preForm.put("op", "ae");
        preForm.put("cmd", "aet");
        preForm.put("t", "1");
        preForm.put("e", "1");
        // preForm.put("kp38653", "");
        preForm.put("n", "propellerads");
        preForm.put("wc", "0");
        preForm.put("h", "");
        preForm.put("u", "25");
        final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
        final String turnstileSiteKey = br.getRegex("sitekey\\s*:\\s*\"([^\"]+)").getMatch(0);
        String cfTurnstileResponse = null;
        if (turnstileSiteKey != null) {
            final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, turnstileSiteKey);
            logger.info("Detected captcha method \"CloudflareTurnstileCaptcha\" for this host");
            cfTurnstileResponse = ts.getToken();
            preForm.put("cf-turnstile-response", Encoding.urlEncode(cfTurnstileResponse));
            preForm.put("g-recaptcha-response", Encoding.urlEncode(cfTurnstileResponse));
        }
        this.waitTime(link, timeBefore);
        final Browser br2 = br.cloneBrowser();
        submitForm(br2, preForm);
        final Map<String, Object> entries = restoreFromString(br2.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"success".equals(entries.get("status"))) {
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        if (cfTurnstileResponse != null) {
            dlform.put("cf-turnstile-response", Encoding.urlEncode(cfTurnstileResponse));
            dlform.put("g-recaptcha-response", Encoding.urlEncode(cfTurnstileResponse));
        }
        br2.setFollowRedirects(false);
        submitForm(br2, dlform);
        final String dllink = br2.getRedirectLocation();
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
        } else {
            logger.info("Successfully found dllink via official video download");
        }
        return dllink;
    }

    /**
     * 2025-09-04: Resolves the streaming direct-URL from the embed page (/e/<fuid>). <br>
     * The player obfuscates both the API token and the source URL with an LZW variant (see {@link #lzwDecode(String)}). Flow: <br>
     * 1. Load /e/<fuid> and grab the real, LZW-encoded "fc" token (there are multiple ASCII decoy "fc" values; the real one is set via a
     * bracket-assignment on window._vvto and contains non-ASCII codepoints). <br>
     * 2. LZW-decode "fc" -> signed "ch" token. <br>
     * 3. GET /dl?op=player_api&cmd=gi&...&ch=<ch> -> JSON containing file.dv[0] with obfuscated source fields (this endpoint needs neither
     * captcha nor adscore token). <br>
     * 4. Decode file.dv[0].s (fallback t/sz) -> direct .mp4 URL (see {@link #decodeVeevSource(String)}).
     */
    @SuppressWarnings("unchecked")
    @Override
    protected String getEmbedDllink(final Browser br, final String embedURL, final DownloadLink link, final Account account) throws Exception, PluginException {
        final Browser brc = br.cloneBrowser();
        getPage(brc, embedURL);
        final String encodedFc = brc.getRegex("window\\._vvto\\s*\\[\\s*\\w+\\s*\\]\\s*=\\s*\"([^\"]+)\"").getMatch(0);
        if (encodedFc == null) {
            logger.info("Failed to find encoded player 'fc' token -> Falling back to default embed handling");
            return super.getEmbedDllink(br, embedURL, link, account);
        }
        final String fileCode = new Regex(embedURL, "(?i)/e/([A-Za-z0-9]+)").getMatch(0);
        /* Decode the API token. */
        final String ch = lzwDecode(encodedFc);
        /* Ask the player API for the sources. brc is still on the embed page so the Referer is set correctly. */
        final Browser brc2 = brc.cloneBrowser();
        brc2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        getPage(brc2, "/dl?op=player_api&cmd=gi&file_code=" + fileCode + "&r=&ch=" + Encoding.urlEncode(ch) + "&ie=1");
        final Map<String, Object> entries = restoreFromString(brc2.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"success".equals(entries.get("status"))) {
            logger.info("player_api 'gi' did not return status 'success' -> Falling back to default embed handling");
            return super.getEmbedDllink(br, embedURL, link, account);
        }
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* TODO: 2026-09-07: Code down below does not work */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> file = (Map<String, Object>) entries.get("file");
        final List<Map<String, Object>> dv = (List<Map<String, Object>>) file.get("dv");
        final Map<String, Object> dv0 = dv.get(0);
        /*
         * The direct video URL is normally in field "s"; "t"/"sz" carry timeslide/sprite metadata. Try all known fields so a response that
         * ever moves the URL still works.
         */
        final String[] fieldOrder = new String[] { "s", "t", "sz" };
        String directurl = null;
        for (final String field : fieldOrder) {
            final Object encodedSource = dv0.get(field);
            if (encodedSource == null) {
                continue;
            }
            directurl = decodeVeevSource(encodedSource.toString());
            if (directurl != null) {
                break;
            }
        }
        if (directurl == null) {
            /* Unexpected format -> signals a site change. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        logger.info("Successfully resolved streaming direct-URL via player API");
        return directurl;
    }

    /**
     * Decodes one LZW-encoded player-API source field (file.dv[].s|t|sz) to a plain URL, or null if it does not contain one. <br>
     * Pipeline: LZW-decompress -> (optionally reverse) -> hex-decode, repeatedly stripping the constant "dXRmOA==" (Base64 of "utf8")
     * marker that prefixes each nested layer, until a plain http(s) URL appears. Both the number of nested hex layers and the orientation
     * (forward/reversed) vary per response.
     */
    private final String decodeVeevSource(final String encoded) throws PluginException {
        final String lzw = lzwDecode(encoded);
        String url = veevMarkerLoop(lzw);
        if (url == null) {
            url = veevMarkerLoop(new StringBuilder(lzw).reverse().toString());
        }
        return url;
    }

    /**
     * Hex-decodes {@code start} layer by layer, stripping the leading "dXRmOA==" marker each round, and returns the first plain http(s)
     * URL.
     */
    private final String veevMarkerLoop(final String start) {
        final String marker = "dXRmOA==";
        String t = hexDecode(start);
        int guard = 0;
        while (t != null) {
            if (t.startsWith(marker)) {
                t = t.substring(marker.length());
            }
            if (t.startsWith("https://") || t.startsWith("http://")) {
                return t;
            }
            t = hexDecode(t);
            if (++guard > 20) {
                return null;
            }
        }
        return null;
    }

    /** Decompresses a classic LZW stream where each entry is a single UTF-16 codepoint (literals 0-255, dictionary entries 256+). */
    private final String lzwDecode(final String data) throws PluginException {
        if (data == null || data.length() == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<String> dict = new ArrayList<String>();
        for (int i = 0; i < 256; i++) {
            dict.add(String.valueOf((char) i));
        }
        String prev = dict.get(data.charAt(0));
        final StringBuilder out = new StringBuilder();
        out.append(prev);
        for (int i = 1; i < data.length(); i++) {
            final int code = data.charAt(i);
            final String entry;
            if (code < dict.size()) {
                entry = dict.get(code);
            } else if (code == dict.size()) {
                entry = prev + prev.charAt(0);
            } else {
                /* Malformed LZW stream. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            out.append(entry);
            dict.add(prev + entry.charAt(0));
            prev = entry;
        }
        return out.toString();
    }

    /**
     * Decodes a hex string into the string built from the resulting bytes (one char per byte); returns null on odd length or non-hex input.
     */
    private final String hexDecode(final String hex) {
        if (hex == null || (hex.length() % 2) != 0) {
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < hex.length(); i += 2) {
            final char c1 = hex.charAt(i);
            final char c2 = hex.charAt(i + 1);
            final int hi = Character.digit(c1, 16);
            final int lo = Character.digit(c2, 16);
            if (hi < 0 || lo < 0) {
                return null;
            }
            sb.append((char) ((hi << 4) | lo));
        }
        return sb.toString();
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        final String betterFilename = new Regex(html, "<h4>([^<]+)</h4>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        final String betterFilesize = new Regex(html, "<i class=\"fa-solid fa-floppy-disk[^\"]*\"></i>([^<]+)<").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        return fileInfo;
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("<title>Watch video - Veev\\.to</title>")) {
            /* 2024-07-08: Offline embed item without further error message. */
            return true;
        } else if (br.containsHTML(">\\s*File not found")) {
            /* 2024-07-08: Offline embed item without further error message. */
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    protected URL_TYPE getURLType(final String url) {
        if (url == null) {
            return null;
        }
        final Regex regex_pattern_special = new Regex(url, PATTERN_SPECIAL);
        if (regex_pattern_special.patternFind()) {
            return super.getURLType(url);
        }
        final String typeString = regex_pattern_special.getMatch(0);
        if (typeString.equalsIgnoreCase("e")) {
            return URL_TYPE.EMBED_VIDEO_2;
        } else {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        }
    }

    @Override
    protected String getFUID(final String url, URL_TYPE type) {
        if (url == null || type == null) {
            return null;
        }
        try {
            if (type == URL_TYPE.EMBED_VIDEO_2) {
                return new Regex(new URL(url).getPath(), "(?i)/e/([A-Za-z0-9]+)").getMatch(0);
            } else if (type == URL_TYPE.EMBED_VIDEO_2) {
                return new Regex(new URL(url).getPath(), "(?i)/d/([A-Za-z0-9]+)").getMatch(0);
            }
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return super.getFUID(url, type);
    }
}