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
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PutRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51799 $", interfaceVersion = 3, names = {}, urls = {})
public class PicdropCom extends PluginForHost {
    public PicdropCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookie(getHost(), "wp-wpml_current_language", "en");
        br.setCookie(getHost(), "consent", "{%22version%22:%221.7%22%2C%22entries%22:{%22essential%22:false%2C%22gtm%22:false%2C%22google%22:false%2C%22googleads%22:false%2C%22facebook%22:false%2C%22satismeter%22:false%2C%22vwo%22:false%2C%22rewardful%22:false}}");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "picdrop.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([\\w-]+)/([a-zA-Z0-9]+)\\?file=([a-f0-9]{32})");
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
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        final String album_key = urlinfo.getMatch(1);
        final String file_hash = urlinfo.getMatch(2);
        return album_key + "_" + file_hash;
    }

    private String getAlbumKey(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        return urlinfo.getMatch(1);
    }

    private String getFileKey(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        return urlinfo.getMatch(2);
    }

    private String getUsername(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        return urlinfo.getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return getUsername(link) + "_" + this.getFID(link) + ".jpg";
    }

    private String dllink = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.dllink = null;
        this.setBrowserExclusive();
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Accept", "application/json, text/plain, */*");
        brc.getHeaders().put("Content-Type", "application/json");
        final String username = this.getUsername(link);
        final String album_key = getAlbumKey(link);
        final String file_key = this.getFileKey(link);
        final PutRequest req = new PutRequest("https://www." + getHost() + "/api/download/" + username + ":" + album_key);
        final String reqData = "{\"file\":{\"key\":\"" + file_key + "\",\"type\":\"attachment\"}}";
        req.setPostBytes(reqData.getBytes());
        brc.getHeaders().put("Referer", link.getPluginPatternMatcher());
        brc.getPage(req);
        if (brc.getHttpConnection().getResponseCode() == 404) {
            /* response 404 with plaintext response "not found" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final String status = entries.get("status").toString();
        if (!"ready".equalsIgnoreCase(status)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dllink = entries.get("downloadUrl").toString();
        final UrlQuery query = UrlQuery.parse(dllink);
        String content_disposition_string = query.get("response-content-disposition");
        if (content_disposition_string != null) {
            content_disposition_string = Encoding.htmlDecode(content_disposition_string);
            final String filename = HTTPConnectionUtils.getFileNameFromDispositionHeader(content_disposition_string);
            if (filename != null) {
                link.setFinalFileName(filename);
            } else {
                logger.warning("Failed to find filename via response-content-disposition");
            }
        } else {
            logger.warning("Failed to find response-content-disposition string");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
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