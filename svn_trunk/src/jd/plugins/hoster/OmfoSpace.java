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

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

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

@HostPlugin(revision = "$Revision: 51914 $", interfaceVersion = 3, names = {}, urls = {})
public class OmfoSpace extends PluginForHost {
    public OmfoSpace(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(getHost(), "adb", "0");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "omfo.space", "365file.me", "365file.net" });
        /* Similar but different: bestfile.io */
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        /* 2025-12-01: This domain is not dead but file links won't work with it. */
        deadDomains.add("365file.net");
        return deadDomains;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9]{10,})");
        }
        return ret.toArray(new String[0]);
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
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        final String contenturl = link.getPluginPatternMatcher();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("<span class=\"text-break h3\">([^<]+)</span>").getMatch(0);
        String filesize = br.getRegex("<span>\\((\\d+[^<]+)\\)\\s*</span>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
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
        final String fileID = this.getFID(link);
        final Form dlform = br.getFormByInputFieldKeyValue("method", "free");
        if (dlform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getHeaders().put("Origin", "https://" + br.getHost(false));
        br.submitForm(dlform);
        final Form dlform2 = br.getFormbyProperty("id", "down_2Form");
        if (dlform2 == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.submitForm(dlform2);
        final String csrftoken = br.getRegex("name=\"csrf-token\" content=\"([^\"]+)\"").getMatch(0);
        if (csrftoken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final boolean skipWaitTime = true;
        if (!skipWaitTime) {
            final String waitSecondsStr = br.getRegex("(\\d{1,2})\\s*</span>\\s*Seconds").getMatch(0);
            if (waitSecondsStr != null) {
                this.sleep(Long.parseLong(waitSecondsStr) * 1000, link);
            }
        }
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Origin", "https://" + br.getHost());
        brc.getHeaders().put("X-Csrf-Token", csrftoken);
        brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        /* Super important cookie! */
        brc.setCookie(br.getHost(), "rqf", fileID);
        brc.postPage("/" + fileID + "/file/generate", new UrlQuery());
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final String error = (String) entries.get("error");
        if (error != null) {
            throw new PluginException(LinkStatus.ERROR_FATAL, error);
        }
        final String dllink = entries.get("download_link").toString();
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(brc, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}