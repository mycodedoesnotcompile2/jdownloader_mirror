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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52960 $", interfaceVersion = 3, names = {}, urls = {})
public class FirestreamTo extends PluginForHost {
    public FirestreamTo(final PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "firestream.to" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/e/([A-Za-z0-9]+)");
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
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link) + ".mp4";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        final String fid = this.getFID(link);
        br.getPage("https://" + this.getHost() + "/e/" + fid);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String videoDataJson = br.getRegex("<script id=\"video-data\" type=\"application/json\">(.*?)</script>").getMatch(0);
        if (videoDataJson == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> root = restoreFromString(videoDataJson, TypeRef.MAP);
        final Map<String, Object> video = (Map<String, Object>) root.get("video");
        if (video == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String encodingStatus = (String) video.get("encodingStatus");
        if (!"completed".equals(encodingStatus)) {
            /* Video is still being encoded/transferred by the hoster -> retry later */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Video is still being processed by the hoster |status: " + encodingStatus, 5 * 60 * 1000l);
        }
        String filename = (String) video.get("originalName");
        if (StringUtils.isEmpty(filename)) {
            filename = (String) video.get("title");
        }
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(this.correctOrApplyFileNameExtension(filename, ".mp4", null));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        this.requestFileInformation(link);
        final String fid = this.getFID(link);
        final String blob = br.getRegex("<script id=\"token-blob\" type=\"text/plain\">([^<]*)</script>").getMatch(0);
        if (StringUtils.isEmpty(blob)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find video token");
        }
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("blob", blob.trim());
        final PostRequest request = br.createJSonPostRequest("https://" + this.getHost() + "/api/videos/" + Encoding.urlEncode(fid) + "/resolve", postdata);
        br.getPage(request);
        final Map<String, Object> resolveResponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String signedVideoUrl = resolveResponse == null ? null : (String) resolveResponse.get("signedVideoUrl");
        if (StringUtils.isEmpty(signedVideoUrl)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to resolve video url", 5 * 60 * 1000l);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, signedVideoUrl, this.isResumeable(link, null), this.getMaxChunks(link, null));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Broken file?");
        }
        dl.startDownload();
    }
}
