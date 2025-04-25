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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.HTMLParser;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51011 $", interfaceVersion = 2, names = { "cnet.com" }, urls = { "https?://(?:www\\.)?download\\.cnet\\.com/([A-Za-z0-9\\-_]+)/([A-Za-z0-9\\-_]+)-(\\d+)\\.html" })
public class CnetCom extends PluginForHost {
    public CnetCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://download.cnet.com/";
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        final String titleFromURL = urlinfo.getMatch(0);
        final String subTitleFromURL = urlinfo.getMatch(1);
        link.setName(titleFromURL.replace("-", " ").trim() + " - " + subTitleFromURL.replace("-", " ").trim());
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(500);
        br.getPage(link.getPluginPatternMatcher());
        if (br.containsHTML("(>Whoops\\! You broke the Internet\\!<|>No, really,  it looks like you clicked on a borked link)") || br.getHttpConnection().getResponseCode() == 404 || br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /* 2024-04-26: Disabled API */
        final boolean useAPI = false;
        String dllink = null;
        if (useAPI) {
            /* 2021-10-06: See https://download.cnet.com/a/neutron/7dbdf09.modern.js */
            String apikey = br.getRegex("apikey=([A-Za-z0-9\\-_]+)").getMatch(0);
            if (apikey == null) {
                /* 2023-06-29 */
                apikey = "ZHqYnHs4B8F0aQnLAwylfp2rWfPhBfqC";
            }
            final Browser brc = br.cloneBrowser();
            brc.getPage("https://cmg-prod.apigee.net/v1/xapi/products/signedurl/download/" + this.getFID(link) + "/web?apiKey=" + apikey);
            if (brc.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Not downloadable (external download, see browser)");
            }
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            dllink = JavaScriptEngineFactory.walkJson(entries, "data/item/url").toString();
        } else {
            /* Try to get installer without adware */
            final String step1 = br.getRegex("(/download/[^/]+/[\\w-]+\\.html)\"").getMatch(0);
            if (step1 != null) {
                br.getPage(step1);
            }
            String step2 = null;
            String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            for (final String url : urls) {
                if (url.contains("internalDownload")) {
                    step2 = url;
                    break;
                }
            }
            if (step2 == null) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Not downloadable (external download, see browser)");
            }
            br.getPage(step2);
            urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            for (final String url : urls) {
                if ((StringUtils.containsIgnoreCase(url, "internalDownload") || StringUtils.containsIgnoreCase(url, "download-launch")) && StringUtils.containsIgnoreCase(url, "token=")) {
                    dllink = url;
                    break;
                }
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (br.containsHTML("File not found")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final String serverFilename = getFileNameFromConnection(dl.getConnection());
        if (!StringUtils.isEmpty(serverFilename)) {
            link.setFinalFileName(Encoding.htmlDecode(serverFilename).trim());
        }
        dl.startDownload();
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}