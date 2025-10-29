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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

@HostPlugin(revision = "$Revision: 51763 $", interfaceVersion = 3, names = {}, urls = {})
public class GigafileNu extends PluginForHost {
    public GigafileNu(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static final String PROPERTY_FILE_ID                = "file_id";
    public static final String PROPERTY_FILE_NAME_FROM_CRAWLER = "filename_from_crawler";

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/privacy.php";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "gigafile.nu" });
        return ret;
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
            /* Liks are added via crawler plugin */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_FILE_ID);
    }

    public static final String REPORT_WORKAROUND_PROPERTY = "use_report_website";

    public static boolean isFilename(final String filename) {
        if (StringUtils.isEmpty(filename)) {
            return false;
        }
        if (filename.contains("ファイル名が置換されました※DLしたファイルは、原題まま表示されます")) {
            return false;
        }
        return true;
    }

    private AvailableStatus requestReportFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        final String host = Browser.getHost(link.getPluginPatternMatcher(), true);
        if (host != null && fid != null) {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            brc.getPage("https://" + host + "/report.php?host=" + host + "&uri=" + fid);
            if (!StringUtils.containsIgnoreCase(brc.getURL(), "/report.php")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String filename = brc.getRegex(">\\s*対象ファイル名\\s*</td>\\s*(?:<td[^>]*>)?\\s*<p[^>]*>\\s*(.*?)\\s*</p>").getMatch(0);
            if (isFilename(filename)) {
                filename = Encoding.htmlDecode(filename);
                link.setFinalFileName(filename);
                link.setProperty(PROPERTY_FILE_NAME_FROM_CRAWLER, filename);
                return AvailableStatus.TRUE;
            }
        }
        return null;
    }

    private AvailableStatus requestAPIFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        final String host = Browser.getHost(link.getPluginPatternMatcher(), true);
        if (host != null && fid != null) {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            brc.getPage("https://" + host + "/get_uploaded_file_name_jx.php?file=" + fid + "&_=" + System.currentTimeMillis());
            /* e.g. {"status":0,"file_status":2,"filename":"filename.ext"} */
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!"0".equals(entries.get("status").toString())) {
                if (false) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                return null;
            }
            final String filename = entries.get("filename").toString();
            if (isFilename(filename)) {
                link.setFinalFileName(filename);
                link.setProperty(PROPERTY_FILE_NAME_FROM_CRAWLER, filename);
            }
            if ("2".equals(StringUtils.valueOfOrNull(entries.get("file_status")))) {
                return AvailableStatus.TRUE;
            }
        }
        return null;
    }

    private String findDirectURL(final Browser br, final DownloadLink link) throws PluginException {
        final String fileIDForDownload = link.getStringProperty(PROPERTY_FILE_ID);
        if (fileIDForDownload == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (isSingleZipDownload(br, fileIDForDownload)) {
            return "/dl_zip.php?file=" + fileIDForDownload;
        } else {
            return "/download.php?file=" + fileIDForDownload;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(fid);
        }
        this.setBrowserExclusive();
        if (link.getFinalFileName() == null && link.hasProperty(REPORT_WORKAROUND_PROPERTY)) {
            final AvailableStatus ret = requestReportFileInformation(link);
            if (ret != null) {
                if (!PluginEnvironment.DOWNLOAD.isCurrentPluginEnvironment()) {
                    if (AvailableStatus.FALSE.equals(ret)) {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    } else {
                        return ret;
                    }
                }
            }
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final PluginForDecrypt crawlerplugin = this.getNewPluginForDecryptInstance(this.getHost());
        if (!crawlerplugin.canHandle(br.getURL())) {
            /* E.g. redirect to main page */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filenameFromCrawler = link.getStringProperty(PROPERTY_FILE_NAME_FROM_CRAWLER);
        if (filenameFromCrawler != null) {
            link.setFinalFileName(filenameFromCrawler);
        } else if (link.getFinalFileName() == null) {
            final AvailableStatus ret = requestReportFileInformation(link);
            if (ret != null) {
                if (AvailableStatus.FALSE.equals(ret)) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    return ret;
                }
            }
            if (link.getFinalFileName() == null && !PluginEnvironment.DOWNLOAD.isCurrentPluginEnvironment()) {
                final String dllink = findDirectURL(br, link);
                try {
                    final Browser brc = br.cloneBrowser();
                    brc.setFollowRedirects(true);
                    final URLConnectionAdapter con = basicLinkCheck(brc, brc.createHeadRequest(dllink), link, null, null);
                    link.setProperty(PROPERTY_FILE_NAME_FROM_CRAWLER, link.getFinalFileName());
                    link.setProperty("free_directlink", con.getURL().toExternalForm());
                } catch (final IOException e) {
                    logger.log(e);
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        final String directlinkproperty = "free_directlink";
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty, this.isResumeable(link, null), this.getMaxChunks(link, null))) {
            requestFileInformation(link);
            final String fileIDForDownload = link.getStringProperty(PROPERTY_FILE_ID);
            if (fileIDForDownload == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String dllink = findDirectURL(br, link);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
                } else if (br.containsHTML("(?i)alert\\(\"ダウンロードキーが異なります")) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Password protected files are not yet supported");
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    public static boolean isSingleZipDownload(final Browser br, final String file_id) {
        return br.containsHTML("download_zip\\('" + Pattern.quote(file_id));
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty, final boolean resumable, final int maxchunks) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, resumable, maxchunks);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}