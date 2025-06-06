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

import java.util.Random;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51002 $", interfaceVersion = 2, names = { "otr-files.de" }, urls = { "https?://(www\\.)?otr\\-files\\.de/(index\\.php\\?option=com_content\\&task=view\\&id=\\d+\\&Itemid=\\d+\\&server=\\d+\\&f=[^<>\"\\']+\\.otrkey|\\?file=[^<>\"\\']+\\.otrkey)" })
public class OtrFilesDe extends PluginForHost {
    public OtrFilesDe(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String LIMITREACHED = "(>Die maximale Anzahl Download Links pro Stunde|Versuche es in einer Stunde nochmal oder Spende dann kannst Du soviele Downloads|Limit erreicht<)";
    private static final String NOSLOTS      = "(Server ausgelastet,|>versuche es in ein paar Minuten noch einmal|Server voll)";

    @Override
    public String getAGBLink() {
        return "https://www.otr-files.de/";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        /* Offline links should also have nice filenames */
        link.setName(new Regex(link.getDownloadURL(), "file=(.+)").getMatch(0));
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getDownloadURL());
        if (!br.getURL().contains("?otr-files.de/index.php?option=")) {
            br.getPage(getOptionsLink());
        }
        if (!br.containsHTML("> Verf\\&uuml;gbare Formate auf otr\\-files") && !br.containsHTML(LIMITREACHED) && !br.containsHTML(NOSLOTS)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setFinalFileName(Encoding.htmlDecode(new Regex(link.getDownloadURL(), "\\&f=(.+\\.otrkey)$").getMatch(0)));
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception, PluginException {
        requestFileInformation(downloadLink);
        String dllink = downloadLink.getStringProperty("freelink");
        if (dllink != null) {
            try {
                Browser br2 = br.cloneBrowser();
                URLConnectionAdapter con = br2.openGetConnection(dllink);
                if (con.getContentType().contains("html") || con.getLongContentLength() == -1) {
                    downloadLink.setProperty("freelink", Property.NULL);
                    dllink = null;
                }
                con.disconnect();
            } catch (Exception e) {
                downloadLink.setProperty("freelink", Property.NULL);
                dllink = null;
            }
        }
        if (dllink == null) {
            if (br.containsHTML(NOSLOTS)) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Keine freien Slots verfügbar!", (1 + new Random().nextInt(7)) * 60 * 1000l);
            }
            if (br.containsHTML(LIMITREACHED)) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, 60 * 60 * 1000l);
            }
            dllink = br.getRegex("\"(https?://otr\\-files\\.de/dl\\-slot/\\d+/[a-z0-9]+/[^<>\"\\']+\\.otrkey)\"").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("<br><br><a href=\"(https?://[^<>\"\\']+)\"").getMatch(0);
            }
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, downloadLink, dllink, true, 1);
        if (dl.getConnection().getContentType().contains("html")) {
            br.followConnection();
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        downloadLink.setProperty("freelink", dllink);
        dl.startDownload();
    }

    private String getOptionsLink() throws Exception {
        String optlink = br.getRegex("\"(https?://(www\\.)?otr-files\\.de/index\\.php\\?option=com_content(?:&amp;|&)task=view(?:&amp;|&)id=\\d+(?:&amp;|&)Itemid=\\d+(?:&amp;|&)server=[a-z0-9]*(?:&amp;|&)f=[^<>\"\\']+\\.otrkey)\"").getMatch(0);
        if (optlink == null) {
            optlink = br.getRegex("\"(\\.?/index\\.php\\?option=com_content(?:&amp;|&)task=view(?:&amp;|&)id=\\d+(?:&amp;|&)Itemid=\\d+(?:&amp;|&)server=[a-z0-9]*(?:&amp;|&)f=[^<>\"\\']+\\.otrkey)\"").getMatch(0);
            if (optlink != null) {
                optlink = br.getURL(optlink).toString();
            }
        }
        if (optlink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return Encoding.htmlDecode(optlink);
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}