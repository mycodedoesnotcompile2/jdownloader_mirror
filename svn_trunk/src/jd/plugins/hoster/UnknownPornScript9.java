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

import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

@HostPlugin(revision = "$Revision: 51517 $", interfaceVersion = 2, names = { "winporn.com", "proporn.com", "vivatube.com", "tubeon.com", "viptube.com", "hd21.com", "iceporn.com", "nuvid.com", "yeptube.com" }, urls = { "https?://(?:(?:www|m)\\.)?winporn\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?proporn\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?vivatube\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?tubeon\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?viptube\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?hd21\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?iceporn\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?", "https?://(?:(?:www|m)\\.)?nuvid\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?",
        "https?://(?:(?:www|m)\\.)?yeptube\\.com/(?:[a-z]{2}/)?video/\\d+(?:/[a-z0-9\\-]+)?" })
public class UnknownPornScript9 extends PluginForHost {
    public UnknownPornScript9(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* Similar sites but they use a different 'player_config' URL: drtuber.com, viptube.com */
    /* Connection stuff */
    private final boolean free_resume    = true;
    private int           free_maxchunks = 0;
    private String        dllink         = null;

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/static/terms";
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
        return new Regex(link.getPluginPatternMatcher(), "/video/(\\d+)").getMatch(0);
    }

    /** Items with different FUIDs but same filenames should not get treated as mirrors! */
    @Override
    public String getMirrorID(DownloadLink link) {
        final String fid = getFID(link);
        if (link != null && StringUtils.equals(getHost(), link.getHost()) && fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getMirrorID(link);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        final String extDefault = ".mp4";
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid + extDefault);
        }
        this.setBrowserExclusive();
        /* Avoid mobile subdomain since official video download is sometimes broken on mobile website version e.g. winporn.com */
        final String contenturl = link.getPluginPatternMatcher().replaceFirst("^https?://m\\.", "https://");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"notifications__item notifications__item-error\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String url_title = new Regex(br.getURL(), "/([a-z0-9_-]+)$").getMatch(0);
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
        final boolean fetchDirecturlOnlyOnDownload = true;
        String title = null;
        if (!fetchDirecturlOnlyOnDownload || isDownload) {
            if (br.containsHTML("/video/download/" + fid)) {
                /* e.g. winporn.com, hd21.com */
                dllink = "/video/download/save/" + fid;
            } else {
                /* e.g. vivatube.com */
                /* Access player json */
                final String videoid = PluginJSonUtils.getJson(br, "vid");
                if (StringUtils.isEmpty(videoid)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
                br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                String config_url = br.getRegex("config_url\\s*:\\s*'(.*?)'").getMatch(0);
                String embed = br.getRegex("embed\\s*:\\s*(\\d+)").getMatch(0);
                if (config_url == null) {
                    /* Fallback */
                    config_url = "/player_config_json/";
                } else {
                    config_url = config_url.replace("\\", "");
                }
                if (embed == null) {
                    embed = "0";
                }
                br.getPage(String.format("%s?vid=%s&aid=&domain_id=&embed=%s&ref=&check_speed=0", config_url, videoid, embed));
                final Map<String, Object> map = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                // final long has_hq = JavaScriptEngineFactory.toLong(map.get("has_hq"), 1);
                /* Most reliable way to find filename */
                String filename = (String) map.get("title");
                if (filename == null) {
                    filename = url_title;
                }
                /* Prefer hq */
                dllink = (String) JavaScriptEngineFactory.walkJson(map, "files/hq");
                if (dllink == null) {
                    dllink = (String) JavaScriptEngineFactory.walkJson(map, "files/lq");
                }
            }
        }
        if (title == null) {
            title = url_title.replace("-", " ").trim();
        }
        final String ext = getFileNameExtensionFromString(dllink, extDefault);
        final String filename = Encoding.htmlDecode(title).trim() + ext;
        link.setFinalFileName(Encoding.htmlDecode(title).trim() + ext);
        if (!isDownload && dllink != null && !link.isSizeSet()) {
            this.basicLinkCheck(br, br.createGetRequest(this.dllink), link, filename, ext);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (link.getDownloadURL().contains("iceporn") || link.getDownloadURL().contains("viptube")) {
            free_maxchunks = 1; // https://svn.jdownloader.org/issues/84428, /84735
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume, free_maxchunks);
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.UnknownPornScript9;
    }
}
