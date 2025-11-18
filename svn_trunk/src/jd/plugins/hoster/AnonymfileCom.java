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
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51841 $", interfaceVersion = 3, names = {}, urls = {})
public class AnonymfileCom extends PluginForHost {
    public AnonymfileCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private final int FREE_MAXCHUNKS = 1;

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/help/privacy";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "anonfilesnew.com" });
        ret.add(new String[] { "filefa.st", "anonymfile.com", "anonfiles.me" });
        return ret;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9\\-_]+)(/([^/#\\?]+))?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        final String filenameFromURL = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
        if (filenameFromURL != null) {
            return Encoding.htmlDecode(filenameFromURL).trim();
        } else {
            return this.getFID(link);
        }
    }

    private boolean useLinkcheckAPI() {
        return true;
    }

    private String getAPIBase() {
        if (this.getHost().equals("anonfilesnew.com")) {
            return "https://api.anonfilesnew.com/v3";
        } else {
            /* e.g. filefa.st */
            return "https://" + getHost() + "/api/v1";
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (this.useLinkcheckAPI()) {
            return requestFileInformationAPI(link);
        } else {
            return requestFileInformationWebsite(link);
        }
    }

    private AvailableStatus requestFileInformationWebsite(final DownloadLink link) throws Exception {
        final String fid = this.getFID(link);
        if (fid.toLowerCase(Locale.ENGLISH).equals(fid)) {
            /* Example: https://anonymfile.com/about */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invlid fileID");
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("<title>\\s*File Not Found") || br.containsHTML(">\\s*Sorry, File does not exist on this server")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Requested file might be deleted")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        boolean safeFilename = false;
        String fileName = br.getRegex("download\\s*=\\s*\"(.*?)\"").getMatch(0);
        if (fileName == null) {
            fileName = br.getRegex("fileName.\"\\s*:\\s*.\"(.*?).\"\\s*,").getMatch(0);
            fileName = fileName == null ? null : (String) JavaScriptEngineFactory.jsonToJavaObject("\"" + fileName + "\"");
            safeFilename = fileName != null;
        }
        if (fileName == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (safeFilename) {
            link.setFinalFileName(fileName);
        } else {
            link.setName(fileName);
        }
        String fileSizeBytes = br.getRegex("bytes.\"\\s*:\\s*(\\d+)").getMatch(0);
        if (fileSizeBytes != null) {
            link.setVerifiedFileSize(Long.parseLong(fileSizeBytes));
        } else {
            String fileSize = br.getRegex(">\\s*Download\\s*\\((.*?)\\)").getMatch(0);
            if (fileSize == null) {
                fileSize = br.getRegex("readable.\"\\s*:\\s*.\"(.*?).\"\\s*").getMatch(0);
            }
            if (fileSize != null) {
                link.setDownloadSize(SizeFormatter.getSize(fileSize));
            }
        }
        return AvailableStatus.TRUE;
    }

    /**
     * Docs: https://filefa.st/docs/api </br>
     * Docs2: https://anonfilesnew.com/docs/api </br>
     * Old API that also existed back in 2024: </br>
     * https://svn.jdownloader.org/projects/jd/repository/revisions/48795/entry/trunk/src/jd/plugins/hoster/AnonfilesMe.java
     */
    private AvailableStatus requestFileInformationAPI(final DownloadLink link) throws Exception {
        final String fid = this.getFID(link);
        if (fid.toLowerCase(Locale.ENGLISH).equals(fid)) {
            /* Example: https://anonymfile.com/about */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invlid fileID");
        }
        this.setBrowserExclusive();
        br.getPage(getAPIBase() + "/file/" + fid + "/info");
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* Plaintext response "Not found -> anonfilesnew.com */
            /* json response: {"status":false,"errors":{"file":"The file you are looking for does not exist."}} -> filefa.st */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> metadata = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/file/metadata");
        final Map<String, Object> sizemap = (Map<String, Object>) metadata.get("size");
        link.setFinalFileName(metadata.get("name").toString());
        /* Can be either a number or a string. Number: anonfilesnew.com String: filefa.st */
        final String filesizeBytesStr = sizemap.get("bytes").toString();
        link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, "free_directlink");
    }

    private void handleDownload(final DownloadLink link, final String directlinkproperty) throws Exception, PluginException {
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            requestFileInformationWebsite(link);
            String dllink = br.getRegex("\"(https[^\"]+)\"\\s*download\\s*=\\s*").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("(/f/[a-f0-9\\-]+)").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("fileUrl.\"\\s*:\\s*.\"(.*?).\"\\s*,").getMatch(0);
                    dllink = dllink == null ? null : (String) JavaScriptEngineFactory.jsonToJavaObject("\"" + dllink + "\"");
                }
            }
            if (StringUtils.isEmpty(dllink)) {
                logger.warning("Failed to find final downloadurl");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), FREE_MAXCHUNKS);
            handleConnectionErrors(br, dl.getConnection());
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, null), FREE_MAXCHUNKS);
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