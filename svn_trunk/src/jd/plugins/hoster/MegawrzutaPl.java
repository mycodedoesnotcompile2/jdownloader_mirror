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
import java.util.regex.Pattern;

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51139 $", interfaceVersion = 3, names = {}, urls = {})
public class MegawrzutaPl extends PluginForHost {
    public MegawrzutaPl(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookie(getHost(), "popupConsent1", "true");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "megawrzuta.pl" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/download/([a-f0-9]{32})\\.html", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_SHORT  = Pattern.compile("/(?:[a-z]{2}/)?([a-z0-9]{8,})", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_NORMAL.pattern() + "|" + PATTERN_SHORT.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
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
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_NORMAL).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_SHORT).getMatch(0);
        return fid;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"title-download-bold wow fadeIn\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Wrzuta, której szukasz, wygasła lub została usunięta")) {
            /* "Upload expired" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int numberofFiles = 1;
        final String numberofFilesStr = br.getRegex("<span>(\\d+)</span>\\s*pliki").getMatch(0);
        if (numberofFilesStr != null) {
            numberofFiles = Integer.parseInt(numberofFilesStr);
            if (numberofFiles == 0) {
                /* Not sure if this can happen */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Empty folder");
            }
        }
        /* Folders can contain multiple files but user cannot download individual files, only if the folder contains only a single file. */
        final String[] filenames = br.getRegex("<span class=\"ml-2\"[^>]*>([^<]+)</span>").getColumn(0);
        String folder_title = br.getRegex("<h1 class=\"h3\">Pobierz - ([^<]+)</h1>").getMatch(0);
        if (folder_title != null) {
            folder_title = Encoding.htmlDecode(folder_title).trim();
        }
        if (numberofFiles == 1) {
            /* Single file -> Can have varying file extension */
            if (filenames != null && filenames.length > 0) {
                final String filename = Encoding.htmlDecode(filenames[0]).trim();
                link.setFinalFileName(filename);
            } else {
                logger.warning("Failed to find filename of single file");
                /* Fallback */
                if (folder_title != null) {
                    link.setName(folder_title);
                }
            }
        } else {
            /* Multiple files -> Always .zip extension */
            if (folder_title != null) {
                link.setName(this.correctOrApplyFileNameExtension(folder_title, ".zip", null));
            } else {
                logger.warning("Failed to find folder title");
                /* Fallback */
                link.setName(fid + ".zip");
            }
        }
        final String[] filesizes = br.getRegex("<span class=\"text-muted\"[^>]*>(\\d+[^<]+)</span>").getColumn(0);
        if (filesizes != null && filesizes.length > 0) {
            /* First file size = Size of all files -> Total file size of folder */
            link.setDownloadSize(SizeFormatter.getSize(filesizes[0]));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final Form dlform = br.getForm(0);
        if (dlform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
        // link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}