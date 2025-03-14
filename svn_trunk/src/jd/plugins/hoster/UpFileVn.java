//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
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

import org.appwork.utils.Hash;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.antiDDoSForHost;

import jd.PluginWrapper;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

@HostPlugin(revision = "$Revision: 50777 $", interfaceVersion = 2, names = { "upfile.vn" }, urls = { "http://(www\\.)?upfile\\.vn/(?!faq|register|login|terms|report_file)[a-z0-9~]+(?:/.*?\\.html)?" })
public class UpFileVn extends antiDDoSForHost {
    public UpFileVn(PluginWrapper wrapper) {
        super(wrapper);
    }
    // YetiShareBasic Version 0.1.7-psp
    // mods: fInfo regex, many changes, DO NOT UPGRADE!
    // non account: chunks * maxdls
    // premium account: chunks * maxdls
    // protocol: no https
    // captchatype: null

    @Override
    public String getAGBLink() {
        return MAINPAGE + "/terms." + TYPE;
    }

    private final String        MAINPAGE                 = "http://upfile.vn";
    private final String        TYPE                     = "html";
    private final boolean       RESUME                   = true;
    private final int           MAXCHUNKS                = 1;
    private static final String SIMULTANDLSLIMIT         = "?e=You+have+reached+the+maximum+concurrent+downloads";
    private static final String SIMULTANDLSLIMITUSERTEXT = "Max. simultan downloads limit reached, wait to start more downloads from this host";
    private static final String SERVERERROR              = "e=Error%3A+Could+not+open+file+for+reading.";
    private static final String SERVERERRORUSERTEXT      = "Server error";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getDownloadURL());
        if (br.getURL().contains("/error." + TYPE) || br.getURL().contains("/index." + TYPE) || !br.containsHTML("class=\"downloadPageTable(V2)?\"|<div class='Download'>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (br.getURL().contains(SIMULTANDLSLIMIT)) {
            link.setName(new Regex(link.getDownloadURL(), "([A-Za-z0-9]+)$").getMatch(0));
            link.getLinkStatus().setStatusText(SIMULTANDLSLIMITUSERTEXT);
            return AvailableStatus.TRUE;
        } else if (br.getURL().contains(SERVERERROR)) {
            link.setName(new Regex(link.getDownloadURL(), "([A-Za-z0-9]+)$").getMatch(0));
            link.getLinkStatus().setStatusText(SERVERERRORUSERTEXT);
            return AvailableStatus.TRUE;
        }
        // Regex fInfo =
        // br.getRegex("<th class=\"descr\"([^<>]*?)?>[\t\n\r ]+<h1>([^<>\"]*?) \\((\\d+(,\\d+)?(\\.\\d+)? (KB|MB|GB))\\)<br/>");
        // String filename = fInfo.getMatch(1);
        // String filesize = fInfo.getMatch(2);
        // if (filename == null) {
        final Regex fInfo = br.getRegex("<h1>(.*?) \\((\\d+([\\.,]\\d+)?\\s*(KB|MB|GB))\\)</h1>");
        final String filename = fInfo.getMatch(0);
        final String filesize = fInfo.getMatch(1);
        if (filename == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setName(Encoding.htmlDecode(filename.trim()));
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize.replace(",", "")));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception, PluginException {
        requestFileInformation(downloadLink);
        if (br.getURL().contains(SIMULTANDLSLIMIT)) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, SIMULTANDLSLIMITUSERTEXT, 1 * 60 * 1000l);
        } else if (br.getURL().contains(SERVERERROR)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, SERVERERRORUSERTEXT, 5 * 60 * 1000l);
        }
        final String uid = new Regex(downloadLink.getDownloadURL(), "upfile\\.vn/([^/]+)").getMatch(0);
        final String hash = Hash.getSHA256(uid + 7891).toUpperCase();
        br.postPage(br.getURL(), "Token=" + hash);
        final String dllink = PluginJSonUtils.getJsonValue(br, "Link");
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        // 3 sec wait
        sleep(4 * 1001l, downloadLink);
        dl = jd.plugins.BrowserAdapter.openDownload(br, downloadLink, dllink, RESUME, MAXCHUNKS);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            if (br.getURL().contains(SERVERERROR)) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, SERVERERRORUSERTEXT, 5 * 60 * 1000l);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    /* NO OVERRIDE!! We need to stay 0.9*compatible */
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return true;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.MFScripts_YetiShare;
    }
}