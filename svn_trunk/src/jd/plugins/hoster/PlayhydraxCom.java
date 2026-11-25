//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.jdownloader.downloader.segment.Segment;
import org.jdownloader.downloader.segment.SegmentDownloader;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/**
 * Plugin for the video player "Hydrax" / "Abyss" which is embedded by various sites (e.g. tube.perverzija.com).
 *
 * <pre>
 * How it works:
 * 1. The embed page (playhydrax.com/?v=SLUG) contains the variable "datas": base64 of a JSON object with slug, md5_id, user_id and "media".
 * 2. "media" is AES-256-CTR encrypted. Decrypted it contains the available qualities (size, res_id, sub) and the CDN domains.
 * 3. The video file is not available as one file. The CDN delivers it in chunks of 2 MiB (CHUNK_SIZE). Every chunk has its own URL
 *    https://DOMAIN/sora/SIZE/TOKEN where the TOKEN is the (AES encrypted) path of the chunk, see {@link #buildChunkUrl}.
 * 4. The chunks are downloaded one after another by the {@link SegmentDownloader} (every chunk is one {@link Segment}) and are written
 *    into one file.
 * </pre>
 */
@HostPlugin(revision = "$Revision: 54173 $", interfaceVersion = 3, names = {}, urls = {})
public class PlayhydraxCom extends PluginForHost {
    public PlayhydraxCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* DEV NOTES */
    /* Tags: Porn plugin, Hydrax, Abyss */
    /* other: The player redirects to abyss.to if it is not displayed inside of an iframe */
    /* The CDN only delivers the chunks if the request looks like it was sent by the player. */
    private static final String PLAYER_REFERER = "https://playhydrax.com/";
    private static final String PLAYER_ORIGIN  = "https://playhydrax.com";
    /* Size of the chunks the CDN delivers (the player requests the file in pieces of 2 MiB). */
    private static final long   CHUNK_SIZE     = 2097152;

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain */
        ret.add(new String[] { "playhydrax.com", "abyss.to", "abysscdn.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    /**
     * Supported are embed URLs like https://playhydrax.com/?v=B38BKfKVGg. The only capture group is the video ID ("slug").
     */
    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/\\?v=([A-Za-z0-9_-]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost();
    }

    /**
     * The same video is available on multiple domains (playhydrax.com, abyss.to, ...), therefore the link ID is independent of the domain.
     */
    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "playhydrax://" + fid;
        }
        return super.getLinkID(link);
    }

    /** Returns the video ID ("slug") of the given link. */
    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    /** Filename which is displayed until the real filename (title of the embed page) is known. */
    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link) + ".mp4";
    }

    /** The {@link SegmentDownloader} resumes behind the last completely loaded chunk. */
    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    /* No @Override */
    public int getMaxChunks(final Account account) {
        /* The SegmentDownloader loads the chunks of the CDN one after another. */
        return 1;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    /** Information about the video file which will be downloaded. */
    private class MediaInfo {
        /* Title of the embed page, can be null. */
        private String title;
        /* CDN domain, e.g. yurawzq6o26.sssrr.org */
        private String domain;
        /* ID of the video in the database of the service, part of the path which is signed in the chunk token. */
        private String md5Id;
        /* ID of the quality, part of the path which is signed in the chunk token. */
        private long resId;
        /* Size of the complete video file in bytes. */
        private long size;
    }

    /**
     * Sets filename and filesize. The size is known exactly because the media information contains the size of every quality.
     */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final MediaInfo info = loadMediaInfo(link);
        if (info.title != null) {
            link.setFinalFileName(this.correctOrApplyFileNameExtension(info.title, ".mp4", null));
        }
        link.setVerifiedFileSize(info.size);
        return AvailableStatus.TRUE;
    }

    /**
     * Loads the embed page, decrypts the media information and picks the best quality.
     *
     * <pre>
     * Structure of the decrypted media information (only the used fields):
     * {"mp4": {"sources": [{"label":"720p", "res_id":4, "size":302231752, "status":true, "sub":"yurawzq6o26"}, ...],
     *          "domains": ["i34mbcqo17.sssrr.org", "yurawzq6o26.sssrr.org"]}}
     * </pre>
     */
    private MediaInfo loadMediaInfo(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        /* The embed page can be opened with the referrer of the site which embeds it. */
        final String referer = link.getReferrerUrl();
        if (referer != null) {
            br.setCurrentURL(referer);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Step 1: "datas" = base64 of a JSON object. */
        final String datasBase64 = br.getRegex("const datas = \"([^\"]+)\"").getMatch(0);
        if (datasBase64 == null) {
            /*
             * Error page of the service: <h1>204</h1><h2>Notification</h2><p>No playable sources found</p>. The message is the reason why
             * the video cannot be played.
             */
            final String errorCode = br.getRegex("<h1>\\s*(\\d{3})\\s*</h1>").getMatch(0);
            final String errorMessage = br.getRegex("<h1>\\s*\\d{3}\\s*</h1>\\s*<h2>[^<]*</h2>\\s*<p>([^<]+)</p>").getMatch(0);
            if (errorMessage != null) {
                final String message = Encoding.htmlDecode(errorMessage).trim();
                /* Example no playable sources: tube.per.../teenfidelity-athena-rayneon-point-pussy/ */
                if (StringUtils.equalsIgnoreCase(message, "No playable sources found") || br.getHttpConnection().getResponseCode() == 404) {
                    /* Video is not (or no longer) available, e.g. deleted or still being processed */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Error " + errorCode + ": " + message);
            }
            if (br.containsHTML("not found|deleted|removed")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find media information");
        }
        /* The JSON contains raw bytes (as ISO-8859-1 characters) in the field "media". */
        final Map<String, Object> datas = restoreFromString(new String(Base64.decode(datasBase64), StandardCharsets.ISO_8859_1), TypeRef.MAP);
        final String md5Id = String.valueOf(((Number) datas.get("md5_id")).longValue());
        /*
         * Step 2: Decrypt "media". The key is the md5 of "user_id:slug:md5_id", see aesCtr. Result is a JSON object with the qualities and
         * the CDN domains.
         */
        final String seed = ((Number) datas.get("user_id")).longValue() + ":" + datas.get("slug") + ":" + md5Id;
        final byte[] mediaEncrypted = ((String) datas.get("media")).getBytes(StandardCharsets.ISO_8859_1);
        final Map<String, Object> media = restoreFromString(new String(aesCtr(mediaEncrypted, md5Hex(seed.getBytes(StandardCharsets.UTF_8))), StandardCharsets.UTF_8), TypeRef.MAP);
        final Map<String, Object> mp4 = (Map<String, Object>) media.get("mp4");
        final List<Map<String, Object>> sources = (List<Map<String, Object>>) mp4.get("sources");
        final List<String> domains = (List<String>) mp4.get("domains");
        /* Step 3: Pick the best available quality = the biggest file which is marked as playable. */
        Map<String, Object> bestSource = null;
        for (final Map<String, Object> source : sources) {
            if (!Boolean.TRUE.equals(source.get("status"))) {
                continue;
            }
            if (bestSource == null || ((Number) source.get("size")).longValue() > ((Number) bestSource.get("size")).longValue()) {
                bestSource = source;
            }
        }
        if (bestSource == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "No playable sources");
        }
        final MediaInfo info = new MediaInfo();
        info.md5Id = md5Id;
        info.resId = ((Number) bestSource.get("res_id")).longValue();
        info.size = ((Number) bestSource.get("size")).longValue();
        /*
         * Step 4: Every quality is stored on a specific server: The "sub" of the quality is the subdomain of one of the domains. Same
         * fallback as in the player: Pick a domain by the size of the file.
         */
        final String sub = (String) bestSource.get("sub");
        for (final String domain : domains) {
            if (domain.contains(sub)) {
                info.domain = domain;
                break;
            }
        }
        if (info.domain == null) {
            info.domain = domains.get((int) (info.size % domains.size()));
        }
        final String title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        if (title != null) {
            info.title = Encoding.htmlDecode(title).trim();
        }
        return info;
    }

    /**
     * Starts the download: Every chunk of the CDN is one {@link Segment}. The {@link SegmentDownloader} loads them one after another and
     * appends them to the output file.
     */
    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        final MediaInfo info = loadMediaInfo(link);
        link.setVerifiedFileSize(info.size);
        /* The CDN only delivers the chunks if the request looks like it was sent by the player. The downloader clones this browser. */
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, PLAYER_REFERER);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, PLAYER_ORIGIN);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
        final long chunkCount = (info.size + CHUNK_SIZE - 1) / CHUNK_SIZE;
        final List<Segment> segments = new ArrayList<Segment>();
        for (long chunkIndex = 0; chunkIndex < chunkCount; chunkIndex++) {
            /* All chunks have the same size except the last one. */
            final long expectedLength = Math.min(CHUNK_SIZE, info.size - chunkIndex * CHUNK_SIZE);
            segments.add(new Segment(buildChunkUrl(info, chunkIndex), expectedLength));
        }
        dl = new SegmentDownloader(this, link, null, br, segments);
        dl.startDownload();
    }

    /**
     * Builds the URL of the given chunk: https://DOMAIN/sora/SIZE/TOKEN
     *
     * <pre>
     * TOKEN = base64(base64(AES-CTR(PATH)) without "=") without "=" with
     * PATH  = /mp4/MD5_ID/RES_ID/SIZE/CHUNK_SIZE/CHUNK_INDEX    e.g. /mp4/10349385/4/302231752/2097152/0
     * </pre>
     *
     * @param chunkIndex
     *            Index of the chunk, starting with 0. The chunk contains the bytes [index * CHUNK_SIZE, (index + 1) * CHUNK_SIZE - 1]. The
     *            last chunk is smaller.
     */
    private static String buildChunkUrl(final MediaInfo info, final long chunkIndex) throws Exception {
        final String path = "/mp4/" + info.md5Id + "/" + info.resId + "/" + info.size + "/" + CHUNK_SIZE + "/" + chunkIndex;
        /*
         * The key is the md5 of the size, but the player passes the size as number (not as String) which makes the md5 function it uses
         * hash the digit VALUES (1,2,3,...) instead of the character codes ('1' = 49, ...). That is why the digits are converted here.
         */
        final String sizeString = Long.toString(info.size);
        final byte[] digits = new byte[sizeString.length()];
        for (int i = 0; i < digits.length; i++) {
            digits[i] = (byte) (sizeString.charAt(i) - '0');
        }
        final byte[] encrypted = aesCtr(path.getBytes(StandardCharsets.UTF_8), md5Hex(digits));
        /* Base64 is applied twice and the padding ("=") is removed each time. */
        final String inner = Base64.encodeToString(encrypted).replace("=", "");
        final String token = Base64.encodeToString(inner.getBytes(StandardCharsets.US_ASCII)).replace("=", "");
        return "https://" + info.domain + "/sora/" + info.size + "/" + token;
    }

    /** Returns the md5 hash of the given bytes as lowercase hex string (32 characters). */
    private static String md5Hex(final byte[] input) throws Exception {
        final byte[] digest = MessageDigest.getInstance("MD5").digest(input);
        final StringBuilder sb = new StringBuilder(32);
        for (final byte b : digest) {
            sb.append(Character.forDigit((b >> 4) & 0xF, 16)).append(Character.forDigit(b & 0xF, 16));
        }
        return sb.toString();
    }

    /**
     * AES-256-CTR: The key are the 32 ASCII characters of the hex string (not the bytes of the hash!), the IV are its first 16 characters.
     * CTR is symmetric: This method encrypts and decrypts.
     */
    private static byte[] aesCtr(final byte[] input, final String keyHex) throws Exception {
        final byte[] key = keyHex.getBytes(StandardCharsets.US_ASCII);
        final byte[] iv = new byte[16];
        System.arraycopy(key, 0, iv, 0, 16);
        final Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding");
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"), new IvParameterSpec(iv));
        return cipher.doFinal(input);
    }
}
