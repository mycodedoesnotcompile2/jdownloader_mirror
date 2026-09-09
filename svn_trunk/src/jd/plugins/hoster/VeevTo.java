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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
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

@HostPlugin(revision = "$Revision: 53356 $", interfaceVersion = 3, names = {}, urls = {})
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

    private static int jsInt(char ch) {
        return Character.getNumericValue(ch);
    }

    public static List<List<Integer>> buildArray(String encodedString) {
        final List<List<Integer>> d = new ArrayList<List<Integer>>();
        // LinkedList erlaubt effizientes FIFO-Verhalten (pop(0) / removeFirst)
        final LinkedList<Character> c = new LinkedList<Character>();
        for (char ch : encodedString.toCharArray()) {
            c.add(ch);
        }
        int count = jsInt(c.removeFirst());
        while (count > 0) {
            List<Integer> currentArray = new ArrayList<Integer>();
            for (int i = 0; i < count; i++) {
                // insert(0, ...) schiebt Elemente an den Anfang der Liste
                currentArray.add(0, jsInt(c.removeFirst()));
            }
            d.add(currentArray);
            count = jsInt(c.removeFirst());
        }
        return d;
    }

    // https://github.com/skoruppa/docchi-players/blob/main/veev.py
    // https://static.veevcdn.co/assets/videoplayer/434b479.js?v4
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
        final String ch = R(encodedFc);
        List<List<Integer>> arr = buildArray(ch);
        /* Ask the player API for the sources. brc is still on the embed page so the Referer is set correctly. */
        final Browser brc2 = brc.cloneBrowser();
        brc2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        getPage(brc2, "/dl?op=player_api&cmd=gi&file_code=" + fileCode + "&r=&ch=" + Encoding.urlEncode(ch) + "&ie=1");
        final Map<String, Object> entries = restoreFromString(brc2.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"success".equals(entries.get("status"))) {
            logger.info("player_api 'gi' did not return status 'success' -> Falling back to default embed handling");
            return super.getEmbedDllink(br, embedURL, link, account);
        }
        final Map<String, Object> file = (Map<String, Object>) entries.get("file");
        final List<Map<String, Object>> dv = (List<Map<String, Object>>) file.get("dv");
        final Map<String, Object> dv0 = dv.get(0);
        final String source = L(R(dv0.get("s").toString()), arr.get(0));
        // final String resolution = L(R(dv0.get("sz").toString()), arr.get(2));
        if (source == null) {
            /* Unexpected format -> signals a site change. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        logger.info("Successfully resolved streaming direct-URL via player API");
        return source;
    }

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

    // 1:1 Nachbildung von JS: a = function(j)
    public static String a(Object[] j) {
        StringBuilder k = new StringBuilder();
        for (Object item : j) {
            if (item instanceof String && item.equals("NaN")) {
                k.append("%0NaN");
            } else {
                int i = (Integer) item;
                String d = Integer.toHexString(i);
                if (i < 16) {
                    k.append("%0").append(d);
                } else {
                    k.append("%").append(d);
                }
            }
        }
        // Simuliert JS decodeURIComponent(k)
        return jsDecodeURIComponent(k.toString());
    }

    // 1:1 Nachbildung von JS: p = function(j)
    public static Object[] p(String j) {
        List<Object> result = new ArrayList<Object>();
        for (int d = 0; d < j.length(); d += 2) {
            int end = Math.min(d + 2, j.length());
            String sub = j.substring(d, end);
            // Simuliert JS parseInt(sub, 16)
            Integer parsed = jsParseIntHex(sub);
            if (parsed == null) {
                result.add("NaN"); // JS gibt NaN zurück
            } else {
                result.add(parsed);
            }
        }
        return result.toArray();
    }

    // Emuliert exakt das JS parseInt(str, 16) Abbrech-Verhalten
    private static Integer jsParseIntHex(String s) {
        int result = 0;
        boolean hasDigit = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            final int digit = Character.digit(c, 16);
            if (digit != -1) {
                result = result * 16 + digit;
                hasDigit = true;
            } else {
                break; // JS stoppt beim ersten Nicht-Hex-Zeichen!
            }
        }
        return hasDigit ? result : null; // null = NaN
    }

    // Simuliert JS decodeURIComponent, indem fehlerhafte Sequenzen (wie %0NaN) unberührt bleiben
    private static String jsDecodeURIComponent(String encoded) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < encoded.length(); i++) {
            final char c = encoded.charAt(i);
            if (c == '%' && i + 2 < encoded.length()) {
                final String hex = encoded.substring(i + 1, i + 3);
                try {
                    final int code = Integer.parseInt(hex, 16);
                    sb.append((char) code);
                    i += 2;
                    continue;
                } catch (NumberFormatException e) {
                    // Falls z.B. %0N vorkommt, lässt JS es stehen
                }
            }
            sb.append(c);
        }
        return sb.toString();
    }

    // 1:1 Nachbildung von JS: T = function(j)
    public static String T(String j) {
        return new StringBuilder(j).reverse().toString();
    }

    // 1:1 Nachbildung von JS: L = function(j, I)
    public static String L(String j, List<Integer> I) {
        String D = j;
        for (int k = 0; k < I.size(); k++) {
            if (1 == I.get(k)) {
                D = T(D);
            }
            D = a(p(D)).replace("dXRmOA==", "");
        }
        return D;
    }

    public static String R(String j) {
        if (j == null || j.isEmpty()) {
            return "";
        }
        char[] k = j.toCharArray();
        String C = String.valueOf(k[0]);
        String M = C;
        final List<String> U = new ArrayList<String>();
        U.add(C);
        // In JS: var D = { y: M + C }, y = 256
        // 'y' war in JS ein Name/Literal, keine Variable!
        final Map<Object, String> D = new HashMap<Object, String>();
        D.put("y", M + C); // Key ist der String "y", NICHT 256!
        int y = 256;
        for (int G = 1; G < k.length; G++) {
            int Y = k[G]; // JS: charCodeAt(0)
            String I;
            if (Y < 256) {
                I = String.valueOf(k[G]);
            } else if (D.containsKey(Y)) {
                // Y ist ein Integer (z.B. 268) -> matcht NIE gegen den String-Key "y"!
                I = D.get(Y);
            } else {
                // Deshalb sprang JS IMMER hierhin wenn Y >= 256 war!
                I = M + C;
            }
            U.add(I);
            C = String.valueOf(I.charAt(0));
            // Erst hier wird der numerische Key 256, 257, ... gesetzt:
            D.put(y, M + C);
            y++;
            M = I;
        }
        final StringBuilder result = new StringBuilder();
        for (String s : U) {
            result.append(s);
        }
        return result.toString();
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