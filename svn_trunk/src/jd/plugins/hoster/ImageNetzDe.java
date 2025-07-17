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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.ImagenetzDeCrawler;

@HostPlugin(revision = "$Revision: 51193 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { ImagenetzDeCrawler.class })
public class ImageNetzDe extends PluginForHost {
    public ImageNetzDe(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/agb.php";
    }

    private static List<String[]> getPluginDomains() {
        return ImagenetzDeCrawler.getPluginDomains();
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
            /* URLs get added solely via crawler-plugin. */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
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
        return new Regex(link.getPluginPatternMatcher(), "(?i)https?://[^/]+/(.+)").getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        ImagenetzDeCrawler.checkOffline(br, this.getFID(link));
        return AvailableStatus.TRUE;
    }

    public static void parseFileInfo(final Browser br, final DownloadLink link) throws PluginException {
        final String description = br.getRegex("<strong>\\s*Beschreibung:\\s*</strong>\\s*([^<>\"]+)\\s*<").getMatch(0);
        String filename = br.getRegex("class='dfname'>([^<>\"]+)<").getMatch(0);
        if (filename == null) {
            filename = br.getRegex("data-title=\"([^<>\"]+)\"").getMatch(0);
        }
        if (filename == null && !br.containsHTML("/abuse\\.php\\?sk=")) {
            /* E.g. https://www.imagenetz.de/contact */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filesize = br.getRegex("<small>(\\d+([\\.,0-9]+)? MB)</small>").getMatch(0);
        if (filename != null) {
            link.setName(filename.trim());
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        if (description != null && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String dllink = br.getRegex("(/files[^<>\"\\']+)").getMatch(0);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        long passedMillis = 0;
        if (br.containsHTML("pwd-protected")) {
            final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
            link.setPasswordProtected(true);
            String passCode = link.getDownloadPassword();
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            final UrlQuery pwquery = new UrlQuery();
            pwquery.appendEncoded("action", "verifyPwd");
            pwquery.appendEncoded("downloadLink", dllink);
            pwquery.appendEncoded("pwd", passCode);
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
            brc.postPage("/functions.ajax.php", pwquery);
            final Map<String, Object> entries = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (entries.get("verified").toString().equals("0")) {
                /* e.g. {"error":"Das Kennwort stimmt nicht.","verified":0} */
                link.setDownloadPassword(null);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            }
            /* Success e.g. {"error":false,"verified":1} */
            link.setDownloadPassword(passCode);
            passedMillis = Time.systemIndependentCurrentJVMTimeMillis() - timeBefore;
        } else {
            link.setPasswordProtected(false);
        }
        final String waitSecondsStr = br.getRegex("d='dlCD'><span>(\\d+)<").getMatch(0);
        final int waitSeconds;
        if (waitSecondsStr != null) {
            waitSeconds = Integer.parseInt(waitSecondsStr);
        } else {
            /* 2020-09-09: Static pre-download-waittime */
            waitSeconds = 3;
            logger.warning("Failed to extract pre download wait time from html code -> Fallback to default value: " + waitSeconds);
        }
        /* Subtract the time user eventually needed to enter download password. */
        final long waitMillis = waitSeconds * 1001 - passedMillis;
        if (waitMillis > 0) {
            this.sleep(waitMillis, link);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.setAllowFilenameFromURL(true);
        dl.startDownload();
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}