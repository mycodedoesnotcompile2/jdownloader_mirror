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
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.antiDDoSForHost;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.config.Property;
import jd.config.SubConfiguration;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 54102 $", interfaceVersion = 3, names = { "highporn.net" }, urls = { "highporndecrypted://(.+)" })
public class HighpornNet extends antiDDoSForHost {
    @Override
    public String[] siteSupportedNames() {
        return new String[] { "highporn.net", "tanix.net" };
    }

    public HighpornNet(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* DEV NOTES */
    // Tags:
    // protocol: no https
    // other:
    /* Extension which will be used if no correct extension is found */
    private static final String default_extension = ".mp4";
    /* Property keys (DownloadLink properties set by the crawler and the hoster) */
    public static final String PROPERTY_MAINLINK              = "mainlink";
    public static final String PROPERTY_SINGLEVIDEO           = "singlevideo";
    public static final String PROPERTY_DECRYPTERNAME         = "decryptername";
    public static final String PROPERTY_DIRECTLINK            = "directlink";
    /* Set by the crawler for javhub.net links: Final URL has to be obtained via /playapi. */
    public static final String PROPERTY_PLAYAPI               = "playapi";
    /* Property keys (plugin config) */
    public static final String PROPERTY_ALLOW_RESUME          = "Allow_resume";
    public static final String PROPERTY_ALLOW_FILESIZE_CHECK  = "Allow_filesize_check";
    /* Connection stuff */
    private final int           free_maxchunks    = 1;
    private String              dllink            = null;
    boolean                     isSingleVideo     = false;
    private SubConfiguration    cfg               = getPluginConfig();

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/static/terms";
    }

    /* Custom Base58 alphabet used by the website player (play.js) to encode the response of /playurl (highporn.net family). */
    private static final String PLAYURL_BASE58_ALPHABET        = "789ABCDEFGHJKLNMPQRSTUVWX23456YZabcdefghijkmnopqrstuvwxyz1";
    /* javhub.net: alphabet used for data-src attributes and the response of /playapi. */
    private static final String PLAYAPI_BASE58_ALPHABET_RESPONSE = "efghijkmnopqrstuvwxyz1789ABCDEFGHJKLNMPQRSTUVWX23456YZabcd";
    /* javhub.net: alphabet used for the "data" parameter of the /playapi request. */
    private static final String PLAYAPI_BASE58_ALPHABET_REQUEST  = "JKLNMPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz123456789ABCDEFGH";

    private static byte[] base58Decode(final String input, final String alphabet) throws PluginException {
        BigInteger value = BigInteger.ZERO;
        final BigInteger base = BigInteger.valueOf(alphabet.length());
        for (int i = 0; i < input.length(); i++) {
            final int digit = alphabet.indexOf(input.charAt(i));
            if (digit == -1) {
                /* Not an encoded string (e.g. error message or format change) */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected encoded response");
            }
            value = value.multiply(base).add(BigInteger.valueOf(digit));
        }
        final byte[] bytes = value.toByteArray();
        /* BigInteger may add a leading sign byte (0x00) */
        int start = 0;
        while (start < bytes.length - 1 && bytes[start] == 0) {
            start++;
        }
        final byte[] ret = new byte[bytes.length - start];
        System.arraycopy(bytes, start, ret, 0, ret.length);
        return ret;
    }

    private static String base58Encode(final byte[] data, final String alphabet) {
        BigInteger value = new BigInteger(1, data);
        final BigInteger base = BigInteger.valueOf(alphabet.length());
        final StringBuilder sb = new StringBuilder();
        while (value.signum() > 0) {
            final BigInteger[] divRem = value.divideAndRemainder(base);
            sb.append(alphabet.charAt(divRem[1].intValue()));
            value = divRem[0];
        }
        return sb.reverse().toString();
    }

    private static String decodeUrl(final String encoded, final String alphabet) throws PluginException {
        final String ret = new String(base58Decode(encoded.trim(), alphabet), Charset.forName("UTF-8"));
        if (!ret.startsWith("http")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected playurl response");
        }
        return ret;
    }

    /**
     * Decodes the response of /playurl. The website encodes the final video URL with Base58 and a custom alphabet.
     */
    public static String decodePlayUrlResponse(final String encoded) throws PluginException {
        return decodeUrl(encoded, PLAYURL_BASE58_ALPHABET);
    }

    /** Requests and decodes the final video URL for the given video ID. */
    public static String requestPlayUrl(final String sourceurl, final Browser br, final String videoID) throws Exception {
        final PostRequest postRequest = new PostRequest("https://play.openhub.tv/playurl?random=" + (new Date().getTime() / 1000));
        postRequest.getHeaders().put("Referer", sourceurl);
        postRequest.setContentType("application/x-www-form-urlencoded");
        postRequest.addVariable("v", videoID);
        postRequest.addVariable("s", "highporn");
        final Browser brc = br.cloneBrowser();
        return decodePlayUrlResponse(brc.getPage(postRequest));
    }

    /**
     * javhub.net: Requests the final (signed) video URL. The "videoID" is the (encoded) data-src attribute of the play button. It is sent
     * to /playapi re-encoded with a different alphabet.
     */
    private String requestPlayApiUrl(final String sourceurl, final String videoID) throws Exception {
        if (br.getURL() == null) {
            /* Needed for cookies (e.g. Cloudflare) */
            getPage(sourceurl);
        }
        final byte[] payload = base58Decode(videoID, PLAYAPI_BASE58_ALPHABET_RESPONSE);
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        brc.getHeaders().put("Referer", sourceurl);
        postPage(brc, "https://" + Browser.getHost(sourceurl) + "/playapi", "data=" + Encoding.urlEncode(base58Encode(payload, PLAYAPI_BASE58_ALPHABET_REQUEST)));
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        return decodeUrl((String) entries.get("playurl"), PLAYAPI_BASE58_ALPHABET_RESPONSE);
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public String getDefaultFileName(final DownloadLink link) {
        final String decryptername = link.getStringProperty(PROPERTY_DECRYPTERNAME);
        if (decryptername != null) {
            return decryptername;
        }
        final String fid = getFID(link);
        if (fid == null) {
            return null;
        }
        return fid + default_extension;
    }

    private String getSourceurl(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_MAINLINK);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        isSingleVideo = link.getBooleanProperty(PROPERTY_SINGLEVIDEO, false);
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String url_source = getSourceurl(link);
        if (url_source == null) {
            /* Should never happen */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        getPage(url_source);
        final boolean isPlayApi = link.getBooleanProperty(PROPERTY_PLAYAPI, false);
        if (isPlayApi) {
            /* data-src attributes on this website are encoded or contain previews, not the video URL. */
            dllink = null;
        } else {
            dllink = br.getRegex("data-src\\s*=\\s*\"(https?[^<>\"]+)\"").getMatch(0); // If single link, no videoID
        }
        if (jd.plugins.decrypter.HighpornNetCrawler.isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String fid = getFID(link);
        String filename = link.getStringProperty(PROPERTY_DECRYPTERNAME);
        if (filename != null) {
            /* Final filename as set by the crawler */
            link.setFinalFileName(filename);
        } else {
            /* Fallback */
            filename = fid;
        }
        if (filename == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        filename = Encoding.htmlDecode(filename);
        filename = filename.trim();
        final String ext;
        if (dllink != null) {
            ext = getFileNameExtensionFromString(dllink, default_extension);
        } else {
            ext = default_extension;
        }
        if (!filename.endsWith(ext)) {
            filename += ext;
        }
        /*
         * 2017-06-01: Disabled filesize check as this could lead to download issues as this host only allows a total of 1
         * stream-connection.
         */
        boolean allow_filesize_check = cfg.getBooleanProperty(PROPERTY_ALLOW_FILESIZE_CHECK, false) && !link.isSizeSet();
        logger.info("Allow_filesize_check: " + allow_filesize_check);
        if (dllink != null && allow_filesize_check && PluginEnvironment.LINK_CHECK.isCurrentPluginEnvironment()) {
            dllink = Encoding.htmlDecode(dllink);
            if (link.getFinalFileName() == null) {
                link.setFinalFileName(filename);
            }
            if (allow_filesize_check) {
                URLConnectionAdapter con = null;
                try {
                    final Browser br2 = br.cloneBrowser();
                    con = br2.openHeadConnection(dllink);
                    if (!this.looksLikeDownloadableContent(con)) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 10 * 60 * 1000l);
                    }
                    link.setDownloadSize(con.getCompleteContentLength());
                    link.setProperty(PROPERTY_DIRECTLINK, dllink);
                } finally {
                    try {
                        con.disconnect();
                    } catch (final Throwable e) {
                    }
                }
            }
        }
        if (!isSingleVideo && allow_filesize_check && PluginEnvironment.LINK_CHECK.isCurrentPluginEnvironment()) {
            final String file;
            if (isPlayApi) {
                file = requestPlayApiUrl(br.getURL(), fid);
            } else {
                file = requestPlayUrl(br.getURL(), br, fid);
            }
            final URLConnectionAdapter con = br.cloneBrowser().openHeadConnection(file);
            try {// referer check
                if (con.getResponseCode() == 200 && con.getLongContentLength() > 0 && !StringUtils.contains(con.getContentType(), "text")) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            } finally {
                con.disconnect();
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        isSingleVideo = link.getBooleanProperty(PROPERTY_SINGLEVIDEO, false);
        final boolean resumes = cfg.getBooleanProperty(PROPERTY_ALLOW_RESUME, true);
        logger.info("resumes: " + resumes);
        dllink = link.getStringProperty(PROPERTY_DIRECTLINK);
        final String fid = getFID(link);
        if (link.getBooleanProperty(PROPERTY_PLAYAPI, false)) {
            dllink = requestPlayApiUrl(this.getSourceurl(link), fid);
        } else if (!isSingleVideo) {
            dllink = requestPlayUrl(this.getSourceurl(link), br, fid);
        }
        if (dllink != null) {
            // cached downloadlink doesn't have a browser session, which leads to 403.
            br.getHeaders().put("Referer", getSourceurl(link));
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resumes, free_maxchunks);
            if (dl.getConnection().getContentType().contains("html") || dl.getConnection().getResponseCode() == 403 || dl.getConnection().getLongContentLength() == -1 || (dl.getConnection().getLongContentLength() < 10 && dl.getConnection().getContentType().equals("application/octet-stream"))) {
                link.setProperty(PROPERTY_DIRECTLINK, Property.NULL);
                dllink = null;
                try {
                    dl.getConnection().disconnect();
                } catch (final Throwable e) {
                }
                dl = null;
                br = createNewBrowserInstance();
            }
        }
        if (dllink == null) {
            requestFileInformation(link);
            if (dllink == null && link.getBooleanProperty(PROPERTY_PLAYAPI, false)) {
                /* javhub.net: The highporn.net fallback (/play.php) does not exist there. */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to obtain final video URL", 5 * 60 * 1000l);
            } else if (dllink == null) {
                final Browser br = this.br.cloneBrowser();
                br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                postPage(br, "/play.php", "v=" + fid);
                handleResponsecodeErrors(br.getHttpConnection().getResponseCode());
                if (StringUtils.equalsIgnoreCase(br.getRequest().getHtmlCode(), "fail")) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 10 * 60 * 1000l);
                }
                dllink = br.toString();
            }
        }
        if (dllink == null || !StringUtils.startsWithCaseInsensitive(dllink, "http")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (dl == null) {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resumes, free_maxchunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                try {
                    dl.getConnection().setAllowedResponseCodes(new int[] { dl.getConnection().getResponseCode() });
                    br.followConnection();
                } catch (final IOException e) {
                    logger.log(e);
                }
                handleResponsecodeErrors(dl.getConnection().getResponseCode());
                try {
                    dl.getConnection().disconnect();
                } catch (final Throwable e) {
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        link.setProperty(PROPERTY_DIRECTLINK, dllink);
        dl.startDownload();
    }

    private void handleResponsecodeErrors(final int responsecode) throws PluginException {
        switch (responsecode) {
        case 403:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
        case 404:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
        default:
            break;
        }
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "", JDL.L("plugins.hoster.HighpornNet.Allow_resume", "Allow resume")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), PROPERTY_ALLOW_FILESIZE_CHECK, JDL.L("plugins.hoster.HighpornNet.Allow_filesize_check", "Allow filesize check")).setDefaultValue(false));
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /*
         * 2019-08-08: More than one is possible but will cause a lot of connection issues. Also a lot of videos will not play via browser
         * and need multiple restarts to complete, even with only 1 simultaneous download!
         */
        return 1;
    }

    public void getPage(final String page) throws Exception {
        super.getPage(page);
    }
}
