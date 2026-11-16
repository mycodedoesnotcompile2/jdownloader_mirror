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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.HanimeTv;

/**
 * <b>STATUS (2026-11): DEFECT - website was completely rebuilt, this crawler is outdated and needs a rewrite.</b> <br>
 * Findings so far:
 * <ul>
 * <li>The website is now an Astro page with a Vue island (HTVPlayer). <code>window.__NUXT__</code> no longer exists. The HTML only contains
 * title/slug (ld+json VideoObject, <code>data-video-id</code>, astro-island props "slug"). No streams/manifest/description in the HTML.</li>
 * <li>The old flow (/downloads/&lt;base64 slug&gt; + reCAPTCHA v2 + h.freeanimehentai.net/rapi/v7/downloads) is no longer referenced by the
 * player JS. The page now uses a Turnstile site key (0x4AAAAAABsjKGKilqEiCHWo).</li>
 * <li>New flow according to the player JS (/_astro/BLboX1K_.js):
 * <ol>
 * <li>GET https://ct.hanime.tv/csrf-token -> {"csrf_token": "...", "csrf_token_expires_at": ...} and sets cookie "htv_csrf_proof".</li>
 * <li>POST https://auth.hanime.tv/api/v11/handshake with JSON body {"token": &lt;encrypted blob&gt;} and headers content-type/accept
 * application/json, x-signature-version: web2, x-signature, x-time, x-csrf-token.</li>
 * <li>Plaintext of the token: {"timestamp_unix": &lt;unix seconds&gt;, "directive": "htv_player_handshake", "slug": "...", "playlist_slug":
 * optional}.</li>
 * <li>Encryption ("htv-insecure" scheme, /_astro/CwylGxhk2.js): AES-256-GCM, key = SHA-256("htv-insecure-handshake-v1"), random 12 byte IV,
 * AAD = "htv-insecure-v1", 128 bit tag. Output = base64url(JSON {"v":1,"alg":"AES-256-GCM","iv":b64url,"tag":b64url,"data":b64url}), data
 * = ciphertext without tag.</li>
 * <li>The response header "x-token" is encrypted the same way. Decrypted it is JSON containing "sources" (+ preroll ad info).</li>
 * </ol>
 * </li>
 * <li><b>Blocker:</b> x-signature and x-time come from an Emscripten WASM module (embedded as base64 in
 * hanime-cdn.com/js/vendor.&lt;hash&gt;.min.js, exports window.ssignature / window.stime, recalculated on every "e" window event). The server
 * validates the signature: a random 32 char hex value results in HTTP 401 {"error_id":"UA1","message":"Unauthorized"}. The WASM contains
 * SHA-256 constants and the strings ",Xkdi29," and ",mn2,", so the signature is most likely a SHA-256 over a string built with these
 * salts, the unix time and probably location/origin. The exact formula is not reversed yet.</li>
 * <li>Unknown: whether "sources" contains direct download URLs or only HLS streams, and whether Cloudflare/Turnstile is required.</li>
 * </ul>
 */
@DecrypterPlugin(revision = "$Revision: 53999 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { HanimeTv.class })
public class HanimeTvCrawler extends PluginForDecrypt {
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            // See: https://svn.jdownloader.org/issues/89919
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/videos/hentai/([a-z0-9\\-]+)");
            } else {
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/videos/hentai_TODO_UNFINISHED_PLUGIN/([a-z0-9\\-]+)");
            }
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* TODO: OUTDATED - window.__NUXT__ no longer exists, see class comment for the new website structure. */
        final String json = br.getRegex("window\\.__NUXT__=(.*?);</script>").getMatch(0);
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        final Map<String, Object> video = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "state/data/video");
        final Map<String, Object> hentai_video = (Map<String, Object>) video.get("hentai_video");
        final String title = hentai_video.get("name").toString();
        final String slug = hentai_video.get("slug").toString();
        final String slug_base64_encoded = Encoding.Base64Encode(slug);
        final String description = hentai_video.get("description").toString();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(title).trim());
        fp.setComment(description);
        final String downloadlink = "/downloads/" + slug_base64_encoded;
        br.getPage(downloadlink);
        final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, "6Lcs0SUUAAAAALB-B0yj9Bw0DQFSn0te4l72YrtV").getToken();
        /* TODO: Implement signature verification */
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("id", slug_base64_encoded);
        query.appendEncoded("kind", "requestlinks");
        query.appendEncoded("captcha_kind", "recaptcha");
        query.appendEncoded("captcha_token", recaptchaV2Response);
        query.appendEncoded("captcha_expires", "");
        query.appendEncoded("loc", "https://" + getHost());
        br.getHeaders().put("Accept", "application/json, text/plain, */*");
        br.getHeaders().put("Origin", "https://" + br.getHost());
        /* Session token only exists for logged in users */
        br.getHeaders().put("x-session-token", "");
        br.getHeaders().put("x-signature", generateXSignature()); // TODO: Fix this
        br.getHeaders().put("x-signature-version", "web2");
        br.getHeaders().put("x-time", Long.toString(System.currentTimeMillis() / 1000));
        br.getHeaders().put("x-token", "null");
        br.getPage("https://h.freeanimehentai.net/rapi/v7/downloads?" + query.toString());
        final Map<String, Object> entries2 = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (entries2.get("is_downloads_enabled") != Boolean.TRUE) {
            throw new AccountRequiredException();
        }
        boolean hasFreeAccount = false;
        /*
         * TODO: Add proper handling for case that user owns premium account -> Captcha should be skippable and 1080p downloads should be
         * possible.
         */
        boolean hasPremiumAccount = false;
        Account premiumAccount = null;
        final ArrayList<Account> accounts = AccountController.getInstance().listAccounts(new AccountFilter("hanime.tv"));
        for (final Account account : accounts) {
            if (!hasFreeAccount && account.getType() == AccountType.FREE) {
                hasFreeAccount = true;
            } else if (!hasPremiumAccount && account.getType() == AccountType.PREMIUM) {
                hasPremiumAccount = true;
                premiumAccount = account;
            }
            if (hasFreeAccount && hasPremiumAccount) {
                break;
            }
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int numberofDownloadableItemsGuest = 0;
        // int numberofDownloadableItemsMember = 0;
        final Map<String, Object> videos_manifest = (Map<String, Object>) entries2.get("videos_manifest");
        final List<Map<String, Object>> servers = (List<Map<String, Object>>) videos_manifest.get("servers");
        for (final Map<String, Object> server : servers) {
            final List<Map<String, Object>> streams = (List<Map<String, Object>>) server.get("streams");
            for (final Map<String, Object> stream : streams) {
                if (stream.get("is_downloadable") != Boolean.TRUE) {
                    logger.info("Skipping undownloadable item: " + stream.get("id"));
                    continue;
                } else if (stream.get("is_guest_allowed") != Boolean.TRUE) {
                    logger.info("Skipping undownloadable item: " + stream.get("id"));
                    continue;
                }
                numberofDownloadableItemsGuest++;
                final String url = stream.get("url").toString();
                final String filename = stream.get("filename").toString();
                final long filesize_mbs = ((Number) stream.get("filesize_mbs")).longValue();
                final DownloadLink link = this.createDownloadlink(url);
                link.setName(filename);
                link.setDownloadSize(filesize_mbs * 1024 * 1024 * 1024);
                if (StringUtils.containsIgnoreCase(url, "highwinds-cdn.com")) {
                    /* Direct downloadable URL */
                    link.setAvailable(true);
                }
                link._setFilePackage(fp);
                ret.add(link);
            }
        }
        if (numberofDownloadableItemsGuest == 0) {
            throw new AccountRequiredException();
        }
        return ret;
    }

    /**
     * Generates a random 32-character hexadecimal string for x-signature <br>
     * This might have worked for their apiv6 but not for v7(?) <br>
     * Verified 2026-11: A random signature is rejected by the new API (auth.hanime.tv/api/v11/handshake) with HTTP 401 "UA1". The real
     * signature is calculated by a WASM module, see class comment.
     */
    public static String generateXSignature() {
        Random random = new Random();
        StringBuilder signature = new StringBuilder(32);
        for (int i = 0; i < 32; i++) {
            int randomHex = random.nextInt(16); // 0-15
            signature.append(Integer.toHexString(randomHex));
        }
        return signature.toString();
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return true;
    }
}
