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

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51338 $", interfaceVersion = 3, names = {}, urls = {})
public class SubsourceNet extends PluginForHost {
    public SubsourceNet(PluginWrapper wrapper) {
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
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "subsource.net" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/subtitle/([\\w\\-]+)/([\\w\\-]+)/(\\d+)");
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
        final String titleSlug = urlinfo.getMatch(0);
        final String languageSlug = urlinfo.getMatch(1);
        final String subtitleID = urlinfo.getMatch(2);
        return titleSlug + "_" + languageSlug + "_" + subtitleID;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    private String dllink = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException, InterruptedException {
        dllink = null;
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        final String titleSlug = urlinfo.getMatch(0);
        final String languageSlug = urlinfo.getMatch(1);
        final String subtitleID = urlinfo.getMatch(2);
        if (!link.isNameSet()) {
            /* Fallback: Try to set filenames resembling the serverside final filenames */
            link.setName(titleSlug + "-" + subtitleID + ".zip");
        }
        this.setBrowserExclusive();
        // postRequest.getHeaders().put("Accept", "application/json, text/plain, */*");
        // postRequest.getHeaders().put("Content-Type", "application/json");
        final GetRequest request = new GetRequest("https://api.subsource.net/v1/subtitle/" + titleSlug + "/" + languageSlug + "/" + subtitleID);
        request.getHeaders().put("Origin", "https://" + getHost());
        request.getHeaders().put("Priority", "u=1, i");
        request.getHeaders().put("Referer", "https://" + getHost() + "/");
        getPage(br, request);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> sub = (Map<String, Object>) entries.get("subtitle");
        final String comment = (String) sub.get("commentary");
        /* 0 = unknown filesize */
        final Number filesize = ((Number) sub.get("size"));
        if (filesize != null && filesize.longValue() > 0) {
            link.setDownloadSize(filesize.longValue());
        }
        if (!StringUtils.isEmpty(comment) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(comment);
        }
        dllink = br.getURL("/v1/subtitle/download/" + sub.get("download_token").toString()).toExternalForm();
        if (link.getVerifiedFileSize() == -1 || link.getFinalFileName() == null) {
            basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, null, null);
        }
        return AvailableStatus.TRUE;
    }

    private void getPage(final Browser br, final Request req) throws IOException, InterruptedException, PluginException {
        final URLConnectionAdapter con = br.openRequestConnection(req);
        try {
            final String ratelimitRemainingStr = con.getRequest().getResponseHeader("x-ratelimit-remaining");
            final String retryInSecondsStr = con.getRequest().getResponseHeader(HTTPConstants.HEADER_RESPONSE_RETRY_AFTER);
            logger.info("API Limits: Remaining: " + ratelimitRemainingStr + " | Reset in seconds: " + retryInSecondsStr);
            if (con.getResponseCode() == 429) {
                br.followConnection(true);
                logger.info("Waiting seconds to resolve rate-limit: " + retryInSecondsStr);
                final int retryInSeconds;
                if (retryInSecondsStr != null && retryInSecondsStr.matches("\\d+")) {
                    retryInSeconds = Integer.parseInt(retryInSecondsStr);
                } else {
                    /* Fallback */
                    retryInSeconds = 30;
                }
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate-Limit reached", retryInSeconds * 1000l);
            } else {
                br.followConnection();
            }
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find final downloadurl");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /* Try to avoid running into rate-limit */
        return 1;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}