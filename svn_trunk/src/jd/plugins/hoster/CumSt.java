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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.downloader.text.TextDownloader;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.CumStCrawler;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;

@HostPlugin(revision = "$Revision: 53176 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { CumStCrawler.class })
public class CumSt extends PluginForHost {
    public CumSt(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* Raw post text/caption content */
    public static final String   PROPERTY_POST_TEXT          = "post_text";
    /* Content of a text/caption .txt file */
    public static final String   PROPERTY_TEXT               = "text";
    public static final String   PROPERTY_BETTER_FILENAME    = "better_filename";
    public static final String   PROPERTY_SERVICE            = "service";
    public static final String   PROPERTY_CREATOR_ID         = "creator_id";
    public static final String   PROPERTY_CREATOR_NAME       = "creator_name";
    public static final String   PROPERTY_POST_ID            = "post_id";
    /* Content type of the item: "post" or "dm" */
    public static final String   PROPERTY_CONTENT_TYPE       = "content_type";
    /* Published date as unix timestamp in seconds */
    public static final String   PROPERTY_DATE               = "date";
    public static final String   PROPERTY_POST_CONTENT_INDEX = "postContentIndex";
    public static final String   UNIQUE_ID_PREFIX            = "cumst://";
    private static final Pattern HASH_PATTERN                = Pattern.compile("/([a-fA-F0-9]{64})");

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/contact";
    }

    private static List<String[]> getPluginDomains() {
        return CumStCrawler.getPluginDomains();
    }

    @Override
    public String rewriteHost(final String host) {
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/creators/([^/]+)/(\\d+)/(?:post/(\\d+)|dm/([\\w\\-]+))");
        }
        return ret.toArray(new String[0]);
    }

    private static final String PROPERTY_LAST_KNOWN_FID = "last_known_fid";

    @Override
    public String getLinkID(final DownloadLink link) {
        try {
            String fid = link.getStringProperty(PROPERTY_LAST_KNOWN_FID);
            if (fid != null) {
                return fid;
            }
            final String service = link.getStringProperty(PROPERTY_SERVICE);
            final String creatorID = link.getStringProperty(PROPERTY_CREATOR_ID);
            final String postID = link.getStringProperty(PROPERTY_POST_ID);
            final String contentType = link.getStringProperty(PROPERTY_CONTENT_TYPE, "post");
            if (this.isTextFile(link)) {
                fid = UNIQUE_ID_PREFIX + "textfile/service/" + service + "/creator/" + creatorID + "/" + contentType + "/" + postID;
            } else {
                final String path = new URL(link.getPluginPatternMatcher()).getPath();
                final String sha256Hash = getSha256HashFromPath(path);
                if (sha256Hash != null) {
                    fid = UNIQUE_ID_PREFIX + "filehash_sha256/" + sha256Hash;
                } else {
                    fid = UNIQUE_ID_PREFIX + "path/" + path;
                }
            }
            link.setProperty(PROPERTY_LAST_KNOWN_FID, fid);
            return fid;
        } catch (final Exception ignore) {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getMirrorID(final DownloadLink link) {
        return this.getLinkID(link);
    }

    private String getFID(final DownloadLink link) {
        final String service = link.getStringProperty(PROPERTY_SERVICE);
        final String creatorID = link.getStringProperty(PROPERTY_CREATOR_ID);
        final String postID = link.getStringProperty(PROPERTY_POST_ID);
        final String contentType = link.getStringProperty(PROPERTY_CONTENT_TYPE, "post");
        final int index = link.getIntegerProperty(PROPERTY_POST_CONTENT_INDEX, -1);
        if (service != null && creatorID != null && postID != null && index != -1) {
            /* Media/Files */
            return service + "_" + creatorID + "_" + contentType + "_" + postID + "_index_" + index;
        } else if (service != null && creatorID != null && postID != null) {
            /* Raw text content */
            return service + "_" + creatorID + "_" + contentType + "_" + postID;
        } else {
            return null;
        }
    }

    private boolean isTextFile(final DownloadLink link) {
        return link.hasProperty(PROPERTY_TEXT);
    }

    /** Returns sha256 hash if it is present in the given url. */
    public static String getSha256HashFromURL(final String url) {
        try {
            return getSha256HashFromPath(new URL(url).getPath());
        } catch (final MalformedURLException ignore) {
            ignore.printStackTrace();
            return null;
        }
    }

    public static String getSha256HashFromPath(final String path) {
        return new Regex(path, HASH_PATTERN).getMatch(0);
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        if (this.isTextFile(link)) {
            return this.getFID(link) + ".txt";
        }
        return super.getDefaultFileName(link);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (isTextFile(link)) {
            final String textContent = link.getStringProperty(PROPERTY_TEXT);
            if (StringUtils.isEmpty(textContent)) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            link.setDownloadSize(textContent.getBytes("UTF-8").length);
        } else {
            /* Media file */
            final String contenturl = link.getPluginPatternMatcher();
            /* The file hash can be used for CRC check as media is content-addressed by sha256. */
            final String sha256 = getSha256HashFromURL(contenturl);
            if (sha256 != null) {
                link.setSha256Hash(sha256);
            }
            String betterFilename = link.getStringProperty(PROPERTY_BETTER_FILENAME);
            if (betterFilename != null) {
                link.setFinalFileName(betterFilename);
            }
            if (this.getPluginEnvironment() != PluginEnvironment.DOWNLOAD) {
                final Browser brc = br.cloneBrowser();
                brc.setFollowRedirects(true);
                basicLinkCheck(brc, brc.createHeadRequest(contenturl), link, betterFilename, null, FILENAME_SOURCE.prefer(FILENAME_SOURCE.values(), FILENAME_SOURCE.FORCED, FILENAME_SOURCE.CUSTOM));
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    protected String correctOrApplyFileNameExtension(FILENAME_SOURCE source, DownloadLink link, String fileName, URLConnectionAdapter con, String... customValues) {
        String extensionFromMimeType = null;
        if (CompiledFiletypeFilter.ImageExtensions.JPG.isSameExtensionGroup(CompiledFiletypeFilter.getExtensionsFilterInterface(Files.getExtension(fileName, true))) && CompiledFiletypeFilter.ImageExtensions.JPG.isSameExtensionGroup(CompiledFiletypeFilter.getExtensionsFilterInterface(extensionFromMimeType = getExtensionFromMimeType(con)))) {
            // fileName and mimeType have ImageExtensions, so we prefer the extension from mimeType
            return super.correctOrApplyFileNameExtension(fileName, extensionFromMimeType, null);
        }
        return super.correctOrApplyFileNameExtension(source, link, fileName, con, customValues);
    }

    @Override
    protected boolean looksLikeDownloadableContent(URLConnectionAdapter urlConnection) {
        if ((urlConnection.getResponseCode() == 200 || urlConnection.getResponseCode() == 206) && getFileNameFromDispositionHeader(urlConnection) != null) {
            return true;
        } else {
            return super.looksLikeDownloadableContent(urlConnection);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        if (this.isTextFile(link)) {
            /* Write text to file */
            final String text = link.getStringProperty(PROPERTY_TEXT);
            if (StringUtils.isEmpty(text)) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            dl = new TextDownloader(this, link, text);
            dl.startDownload();
        } else {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, link.getPluginPatternMatcher(), this.isResumeable(link, null), this.getMaxChunks(null));
            handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        }
    }

    @Override
    public Downloadable newDownloadable(final DownloadLink link, Browser br) {
        return new DownloadLinkDownloadable(link, br) {
            @Override
            public long getLastModifiedTimestamp() {
                /**
                 * We return the date when the content was published instead of the import date.
                 */
                final String publishedDateStr = getDownloadLink().getStringProperty(PROPERTY_DATE);
                if (publishedDateStr == null || !publishedDateStr.matches("\\d+")) {
                    /* Missing/invalid property */
                    return super.getLastModifiedTimestamp();
                }
                /* Stored as unix timestamp in seconds */
                return Long.parseLong(publishedDateStr) * 1000;
            }
        };
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        /* Media is served content-addressed and supports range requests. */
        return true;
    }

    public int getMaxChunks(final Account account) {
        /* 0 = maximum number of chunks */
        return 0;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean allowHandle(final DownloadLink link, final PluginForHost plugin) {
        /* Do not allow multihost plugins to handle items from this plugin. */
        return link.getHost().equalsIgnoreCase(plugin.getHost());
    }
}
