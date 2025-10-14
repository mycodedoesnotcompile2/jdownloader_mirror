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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51662 $", interfaceVersion = 3, names = { "sendgb.com" }, urls = { "https?://(?:www\\.)?sendgb\\.com/(?:upload/\\?utm_source=)?([A-Za-z0-9]+)" })
public class SendgbCom extends PluginForHost {
    public SendgbCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/en/terms-of-use.html";
    }

    /* Connection stuff */
    private final boolean FREE_RESUME    = false;
    private final int     FREE_MAXCHUNKS = 1;

    @Override
    public Browser createNewBrowserInstance() {
        final Browser ret = super.createNewBrowserInstance();
        ret.setCookie(getHost(), "l_code_3", "en");
        return ret;
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isSizeSet()) {
            /* Set default filename. Mimic names that user would get when downloading .zip files via browser. */
            link.setName("sendgb-" + fid + ".zip");
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"boo\\-wrapper\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(fid)) {
            /* 2020-12-08: E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String downloadData[] = br.getRegex("\"submitdownload\"\\s*data-id\\s*=\\s*\"(.*?)\"\\s*\\s*data-sc\\s*=\\s*\"(.*?)\"\\s*data-file\\s*=\\s*\"(.*?)\"\\s*data-private_id\\s*=\\s*\"(.*?)\"").getRow(0);
        if (downloadData != null) {
            // single file downloads
            link.setFinalFileName(Encoding.htmlDecode(downloadData[2].trim()));
        }
        final String filesize = br.getRegex("Total files / Total size[^<]*</div>\\s*\\d+\\s*/\\s*(.*?)\\s*<").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link, FREE_RESUME, FREE_MAXCHUNKS);
    }

    private void doFree(final DownloadLink link, final boolean resumable, final int maxchunks) throws Exception, PluginException {
        startDownload: {
            final String downloadData[] = br.getRegex("\"submitdownload\"\\s*data-id\\s*=\\s*\"(.*?)\"\\s*\\s*data-sc\\s*=\\s*\"(.*?)\"\\s*data-file\\s*=\\s*\"(.*?)\"\\s*data-private_id\\s*=\\s*\"(.*?)\"").getRow(0);
            if (downloadData != null) {
                // single file downloads
                final Browser brc = br.cloneBrowser();
                brc.getPage("/src/download_one.php?uploadId=" + downloadData[0] + "&sc=" + downloadData[1] + "&file=" + downloadData[2] + "&private_id=" + downloadData[3]);
                final Map<String, Object> response = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                if (!Boolean.TRUE.equals(response.get("success"))) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String url = response.get("url").toString();
                dl = jd.plugins.BrowserAdapter.openDownload(br, link, url, resumable, maxchunks);
                break startDownload;
            }
            final Form downloadForm = br.getFormbyActionRegex(".*/download/.*");
            if (downloadForm != null) {
                // multiple files download as a zip
                dl = jd.plugins.BrowserAdapter.openDownload(br, link, downloadForm, false, 1);
                break startDownload;
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (dl == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            handleConnectionErrors(br, dl.getConnection());
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}