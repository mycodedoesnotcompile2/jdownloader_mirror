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
package jd.plugins.decrypter;

import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.HanimeTvConfig;
import org.jdownloader.plugins.components.config.HanimeTvConfig.QUALITY;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.GenericM3u8;
import jd.plugins.hoster.HanimeTv;

/**
 * Crawler for hanime.tv (Astro page + Vue player, reworked 2026-11). <br>
 * Flow (reverse engineered from the player JS and the WASM module in hanime-cdn.com/js/vendor.&lt;hash&gt;.min.js):
 * <ol>
 * <li>GET https://ct.hanime.tv/csrf-token -> {"csrf_token": "..."} (also sets cookie "htv_csrf_proof").</li>
 * <li>POST https://auth.hanime.tv/api/v11/handshake with JSON {"token": &lt;encrypted blob&gt;} and the headers x-signature-version: web2,
 * x-signature, x-time, x-csrf-token.</li>
 * <li>Plaintext of the token: {"timestamp_unix": &lt;unix seconds&gt;, "directive": "htv_player_handshake", "slug": "..."}.</li>
 * <li>Encryption ("htv-insecure" scheme): AES-256-GCM, key = SHA-256("htv-insecure-handshake-v1"), random 12 byte IV, AAD =
 * "htv-insecure-v1", 128 bit tag. Output = base64url(JSON {"v":1,"alg":"AES-256-GCM","iv":b64url,"tag":b64url,"data":b64url}), "data" is
 * the ciphertext without the tag.</li>
 * <li>The response header "x-token" is encrypted the same way. Decrypted it is JSON containing "sources": a list of HLS media playlists
 * with relative URLs (resolved against https://hanime.tv). Items with an empty "src" are premium-only promotions. The playlists are AES-128
 * encrypted HLS streams and are short-lived, so they must be handed over to the HLS plugin right away.</li>
 * </ol>
 * x-signature / x-time (computed by the WASM module): x-time = current unix time in seconds (t), x-signature = hex(SHA-256(t + ",Xkdi29," +
 * window.top.location.origin + ",mn2," + t)). Verified against the original WASM implementation (executed via wasmtime) and against the
 * live server.
 */
@DecrypterPlugin(revision = "$Revision: 54023 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { HanimeTv.class })
public class HanimeTvCrawler extends PluginForDecrypt {
    private static final String HANDSHAKE_KEY_SEED  = "htv-insecure-handshake-v1";
    private static final String HANDSHAKE_AAD       = "htv-insecure-v1";
    private static final String HANDSHAKE_DIRECTIVE = "htv_player_handshake";
    private static final String URL_HANDSHAKE       = "https://auth.hanime.tv/api/v11/handshake";

    public HanimeTvCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return HanimeTv.getPluginDomains();
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

    private static final Pattern PATTERN_DEFAULT = Pattern.compile("/videos/hentai/([a-z0-9\\-_]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_DEFAULT.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String slug = new Regex(contenturl, PATTERN_DEFAULT).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("\"@type\":\"VideoObject\",\"name\":\"([^\"]+)\"").getMatch(0);
        if (title == null) {
            title = slug.replace("-", " ");
        }
        title = Encoding.htmlDecode(title).trim();
        final String origin = HanimeTv.ORIGIN;
        final String videoID = br.getRegex("data-video-id=\"(\\d+)\"\\s+data-video-slug=\"" + Pattern.quote(slug) + "\"").getMatch(0);
        if (videoID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find video ID");
        }
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        /* Optional login: Logged in users also get direct downloadlinks. */
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account != null) {
            final HanimeTv hosterplugin = (HanimeTv) this.getNewPluginForHostInstance(this.getHost());
            hosterplugin.login(br, account, false);
        }
        final Browser brc = br.cloneBrowser();
        final String csrfToken = HanimeTv.fetchCsrfToken(brc);
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey("hanimetv://video/" + videoID);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /*
         * Step 1: Direct downloadlinks (external filehoster, e.g. pixeldrain). The response body contains an encrypted "token":
         * {"is_premium_user":false,"download_links":[{"label":"720p","height":720,"width":1280,"url":"https://pixeldrain.com/d/..."}]} This
         * also works for guests. Logged in premium users probably get more qualities.
         */
        final Map<String, Object> downloadsPlain = new LinkedHashMap<String, Object>();
        downloadsPlain.put("directive", "get_download_links");
        downloadsPlain.put("slug", slug);
        downloadsPlain.put("hentai_video_id", Long.parseLong(videoID));
        final Map<String, Object> downloadsResponse = postHandshake(brc, csrfToken, downloadsPlain);
        final HanimeTvConfig cfg = get(getConfigInterface());
        final String downloadsToken = (String) downloadsResponse.get("token");
        if (downloadsToken != null) {
            final Map<String, Object> downloads = restoreFromString(decryptToken(downloadsToken), TypeRef.MAP);
            final List<Map<String, Object>> downloadMaps = (List<Map<String, Object>>) downloads.get("download_links");
            final Map<String, Object> downloadMap = selectQuality(downloadMaps, cfg.getPreferredDownloadQuality());
            if (downloadMap != null) {
                final DownloadLink link = this.createDownloadlink(downloadMap.get("url").toString());
                link.setReferrerUrl(contenturl);
                link.setName(title + "_" + downloadMap.get("label") + ".mp4");
                link._setFilePackage(fp);
                ret.add(link);
            }
            logger.info("Found " + downloadMaps.size() + " direct downloadlinks | Selected: " + ret.size() + " | is_premium_user: " + downloads.get("is_premium_user"));
            if (!ret.isEmpty()) {
                /* Direct downloadlinks are preferred over the (encrypted) HLS streams. */
                return ret;
            }
        }
        /* Step 2: No direct downloadlinks available -> Fallback to HLS streams (player handshake) */
        final Map<String, Object> playerPlain = new LinkedHashMap<String, Object>();
        playerPlain.put("directive", HANDSHAKE_DIRECTIVE);
        playerPlain.put("slug", slug);
        postHandshake(brc, csrfToken, playerPlain);
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        final String xToken = brc.getRequest().getResponseHeader("x-token");
        final Map<String, Object> handshakeResult = restoreFromString(decryptToken(xToken), TypeRef.MAP);
        final List<Map<String, Object>> sources = (List<Map<String, Object>>) handshakeResult.get("sources");
        final List<Map<String, Object>> availableSources = new ArrayList<Map<String, Object>>();
        for (final Map<String, Object> source : sources) {
            if (StringUtils.isEmpty(source.get("src").toString())) {
                /* Premium only quality without URL */
                logger.info("Skipping source without URL: " + source.get("label"));
                continue;
            }
            availableSources.add(source);
        }
        /* Find user preferred quality source */
        final Map<String, Object> source = selectQuality(availableSources, cfg.getPreferredStreamQuality());
        if (source != null) {
            final String src = source.get("src").toString();
            final String url = src.startsWith("/") ? origin + src : src;
            final DownloadLink link = this.createDownloadlink(GenericM3u8.createURLForThisPlugin(url));
            link.setAvailable(true);
            link.setReferrerUrl(contenturl);
            link.setProperty(GenericM3u8.PROPERTY_HEIGHT, ((Number) source.get("height")).intValue());
            link.setProperty(GenericM3u8.PROPERTY_WIDTH, ((Number) source.get("width")).intValue());
            link.setFinalFileName(title + "_" + source.get("label") + ".mp4");
            link._setFilePackage(fp);
            ret.add(link);
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "No playable sources");
        }
        return ret;
    }

    /**
     * Returns the item matching the preferred quality. If the preferred quality is BEST or not available, the best available item is
     * returned (fallback). Returns null if the list is empty.
     */
    private static Map<String, Object> selectQuality(final List<Map<String, Object>> items, final QUALITY preferred) {
        Map<String, Object> best = null;
        int bestHeight = -1;
        for (final Map<String, Object> item : items) {
            final int height = ((Number) item.get("height")).intValue();
            if (preferred.getHeight() == height) {
                return item;
            } else if (best == null || height > bestHeight) {
                best = item;
                bestHeight = height;
            }
        }
        return best;
    }

    @Override
    public Class<? extends HanimeTvConfig> getConfigInterface() {
        return HanimeTvConfig.class;
    }

    /**
     * Sends a signed handshake request. "timestamp_unix" is added to the given plaintext. Returns the parsed response body. The response
     * header "x-token" can be read from the browser afterwards.
     */
    private Map<String, Object> postHandshake(final Browser brc, final String csrfToken, final Map<String, Object> plaintext) throws Exception {
        final long timestampUnix = HanimeTv.setSignedApiHeaders(brc, csrfToken);
        final Map<String, Object> handshakePlain = new LinkedHashMap<String, Object>();
        handshakePlain.put("timestamp_unix", timestampUnix);
        handshakePlain.putAll(plaintext);
        final Map<String, Object> handshakeBody = new LinkedHashMap<String, Object>();
        handshakeBody.put("token", encryptToken(JSonStorage.serializeToJson(handshakePlain)));
        brc.postPageRaw(URL_HANDSHAKE, JSonStorage.serializeToJson(handshakeBody));
        if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (brc.getHttpConnection().getResponseCode() != 200) {
            /* E.g. 401 {"error_id":"UA1"} = invalid x-signature / x-time */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Handshake failed: HTTP " + brc.getHttpConnection().getResponseCode());
        }
        return restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
    }

    private static SecretKeySpec getHandshakeKey() throws Exception {
        final MessageDigest md = MessageDigest.getInstance("SHA-256");
        return new SecretKeySpec(md.digest(HANDSHAKE_KEY_SEED.getBytes("UTF-8")), "AES");
    }

    /** Encrypts the plaintext with the "htv-insecure" scheme, see class comment. */
    private static String encryptToken(final String plaintext) throws Exception {
        final byte[] iv = new byte[12];
        new SecureRandom().nextBytes(iv);
        final Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
        cipher.init(Cipher.ENCRYPT_MODE, getHandshakeKey(), new GCMParameterSpec(128, iv));
        cipher.updateAAD(HANDSHAKE_AAD.getBytes("UTF-8"));
        final byte[] encrypted = cipher.doFinal(plaintext.getBytes("UTF-8"));
        /* Java appends the 16 byte tag to the ciphertext */
        final byte[] data = new byte[encrypted.length - 16];
        final byte[] tag = new byte[16];
        System.arraycopy(encrypted, 0, data, 0, data.length);
        System.arraycopy(encrypted, data.length, tag, 0, 16);
        final Base64.Encoder enc = Base64.getUrlEncoder().withoutPadding();
        final Map<String, Object> envelope = new LinkedHashMap<String, Object>();
        envelope.put("v", 1);
        envelope.put("alg", "AES-256-GCM");
        envelope.put("iv", enc.encodeToString(iv));
        envelope.put("tag", enc.encodeToString(tag));
        envelope.put("data", enc.encodeToString(data));
        return enc.encodeToString(JSonStorage.serializeToJson(envelope).getBytes("UTF-8"));
    }

    /** Decrypts a token (e.g. the x-token response header) with the "htv-insecure" scheme, see class comment. */
    private String decryptToken(final String token) throws Exception {
        final Base64.Decoder dec = Base64.getUrlDecoder();
        final Map<String, Object> envelope = restoreFromString(new String(dec.decode(token), "UTF-8"), TypeRef.MAP);
        final byte[] iv = dec.decode(envelope.get("iv").toString());
        final byte[] tag = dec.decode(envelope.get("tag").toString());
        final byte[] data = dec.decode(envelope.get("data").toString());
        final byte[] encrypted = new byte[data.length + tag.length];
        System.arraycopy(data, 0, encrypted, 0, data.length);
        System.arraycopy(tag, 0, encrypted, data.length, tag.length);
        final Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
        cipher.init(Cipher.DECRYPT_MODE, getHandshakeKey(), new GCMParameterSpec(128, iv));
        cipher.updateAAD(HANDSHAKE_AAD.getBytes("UTF-8"));
        return new String(cipher.doFinal(encrypted), Charset.forName("UTF-8"));
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}
