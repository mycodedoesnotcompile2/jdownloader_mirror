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
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLSearch;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54111 $", interfaceVersion = 3, names = {}, urls = {})
public class YoupicCom extends PluginForHost {
    public YoupicCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST };
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    private String       dllink             = null;
    private final String PROPERTY_TITLE     = "title";
    private final String PROPERTY_FULL_NAME = "full_name";
    private final String PROPERTY_USERNAME  = "user";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "youpic.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    /* Image size suffix: Flags (1 = full size) + max. size 2500px */
    private static final String  SIZE_SUFFIX_FULL = "001271";
    private static final String  EXT_DEFAULT      = ".jpg";
    private static final Pattern PATTERN_OLD      = Pattern.compile("image/(\\d+)(/([\\w\\-]+))?");
    private static final Pattern PATTERN_NEW      = Pattern.compile("([\\w\\-]+)/([0-9]{15,})/([0-9]{15,})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_OLD + "|" + PATTERN_NEW + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/termconditions";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_NEW).getMatch(2);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_OLD).getMatch(0);
        return fid;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        final String urlSlug = new Regex(link.getPluginPatternMatcher(), PATTERN_OLD).getMatch(2);
        if (urlSlug != null) {
            return urlSlug.replace("-", " ").trim() + EXT_DEFAULT;
        } else {
            return this.getFID(link) + EXT_DEFAULT;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        final String extDefault = EXT_DEFAULT;
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = HTMLSearch.searchMetaTag(br, "og:title");
        if (StringUtils.isEmpty(title)) {
            title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        }
        dllink = br.getRegex("\"(https?://cdn\\.[^/]+/large/[^\"]+)\"").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title);
            title = title.replaceFirst("(?i)\\s*on YouPic", "");
            title = title.replaceFirst("(?i) — YouPic$", "");
            final Regex moreinfo = new Regex(title, " by (.+)");
            final String fullName = moreinfo.getMatch(0);
            if (fullName != null) {
                link.setProperty(PROPERTY_FULL_NAME, fullName);
                title = title.replace(moreinfo.getMatch(-1), "");
            } else {
                logger.warning("Failed to find fullName in title");
            }
            title = title.trim();
            link.setProperty(PROPERTY_TITLE, title);
            link.setFinalFileName(this.applyFilenameExtension(title, extDefault));
        } else {
            logger.warning("Failed to find image title");
        }
        final String username = br.getRegex("/photographer/([\\w\\-]+)").getMatch(0);
        if (username != null) {
            link.setProperty(PROPERTY_USERNAME, username);
        } else {
            logger.warning("Failed to find username");
        }
        final String appB64 = br.getRegex("\"app\"\\), \"([^\"]+)").getMatch(0);
        if (appB64 != null) {
            /*
             * The page redirects old "/image/<id>" URLs to "/<user>/<userid>/<photoid>" so use the final URL to get the real photo ID. The
             * blob contains many photos (the whole gallery) so the correct one must be identified by this ID.
             */
            String photoId = new Regex(br.getURL(), PATTERN_NEW).getMatch(2);
            if (photoId == null) {
                photoId = this.getFID(link);
            }
            final String imageKey = findImageKey(appB64, photoId);
            if (imageKey != null) {
                this.dllink = "https://youpic.com/i/" + imageKey + SIZE_SUFFIX_FULL;
            } else {
                logger.warning("Failed to find image key for photoID: " + photoId);
            }
        }
        if (dllink == null) {
            /*
             * TODO: This is not the image we want as it contains a "Twitter frame". This is just to provide the user something to download
             * vs just failing!
             */
            dllink = br.getRegex("(/card/message/\\d+/\\d+)").getMatch(0);
            if (dllink != null) {
                logger.warning("Fallback to low quality image active");
            }
        }
        if (!StringUtils.isEmpty(dllink)) {
            dllink = dllink.replaceFirst("(?i)/large/", "/huge/"); // Higher image quality
            if (!link.isSizeSet() && PluginEnvironment.LINK_CHECK.isCurrentPluginEnvironment()) {
                basicLinkCheck(br.cloneBrowser(), br.createGetRequest(dllink), link, title, extDefault);
            }
        }
        return AvailableStatus.TRUE;
    }

    /**
     * Finds the hex key of the full size image of the given photo in the binary "app" blob of the page. <br>
     * Blob layout: The record of a photo starts with its ID as LEB128 varint. Further behind come two keys (file, thumbnail), each one a
     * length byte 0x20 followed by 32 bytes (16 bytes hash, 8 bytes id, 4 bytes 0x00, 3 bytes color, 0x00). The "file" key (original)
     * returns 404, the "thumbnail" key returns the image in full size (max. 2500px).
     *
     * @return key as hex string or null if not found
     */
    private String findImageKey(final String appB64, final String photoId) {
        if (photoId == null || !photoId.matches("\\d+")) {
            return null;
        }
        final byte[] blob = Base64.decode(appB64);
        final String blobStr = new String(blob, StandardCharsets.ISO_8859_1);
        /* Encode the photo ID as LEB128 varint */
        long id = Long.parseLong(photoId);
        final StringBuilder idBytes = new StringBuilder();
        while (id >= 0x80) {
            idBytes.append((char) ((id & 0x7f) | 0x80));
            id >>>= 7;
        }
        idBytes.append((char) id);
        final int recordStart = blobStr.indexOf(idBytes.toString());
        if (recordStart == -1) {
            return null;
        }
        final Matcher keyMatcher = Pattern.compile("\\x20.{24}\\x00{4}.{3}\\x00\\x20(.{24}\\x00{4}.{3}\\x00)", Pattern.DOTALL).matcher(blobStr);
        if (!keyMatcher.find(recordStart)) {
            return null;
        }
        final String key = keyMatcher.group(1);
        /*
         * Behind the thumbnail key: length-prefixed string (file name), then the varints size (bytes of the ORIGINAL file, not of the
         * downloadable image), width, height. Old entries have no size: Their first varint is 0.
         */
        int pos = keyMatcher.end(1);
        pos += 1 + blobStr.charAt(pos);
        long sizeVarint = 0;
        int shift = 0;
        while (pos < blobStr.length() && shift < 63) {
            final int cur = blobStr.charAt(pos++);
            sizeVarint |= (long) (cur & 0x7f) << shift;
            if ((cur & 0x80) == 0) {
                break;
            }
            shift += 7;
        }
        if (sizeVarint > 0) {
            logger.info("Original file size (not downloadable): " + SizeFormatter.formatBytes(sizeVarint) + " (" + sizeVarint + " bytes)");
        } else {
            logger.info("Original file size not found in blob (old entry without size)");
        }
        final StringBuilder hex = new StringBuilder();
        for (int i = 0; i < key.length(); i++) {
            hex.append(String.format("%02x", key.charAt(i) & 0xff));
        }
        return hex.toString();
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    protected boolean looksLikeDownloadableContent(final URLConnectionAdapter con) {
        /* Special: For working image direct-URLs they return nearly no headers at all. Also no content-type header. */
        final String contentType = con.getContentType();
        if (con.isOK() && !StringUtils.containsIgnoreCase(contentType, "html") && con.getCompleteContentLength() > 0) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}