//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
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
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLSearch;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51579 $", interfaceVersion = 3, names = {}, urls = {})
public class TokyvideoCom extends PluginForHost {
    public TokyvideoCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        /* Prefer English language */
        br.setCookie(getHost(), "lang", "en_US");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    private String dllink = null;

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "tokyvideo.com" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL    = Pattern.compile("/video/([\\w-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_EMBED     = Pattern.compile("/embed/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final String  PROPERTY_VIDEO_ID = "video_id";

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_NORMAL.pattern() + "|" + PATTERN_EMBED.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/tos";
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
        String fid = link.getStringProperty(PROPERTY_VIDEO_ID);
        if (fid != null) {
            /* Return internal video_id stored as plugin property. */
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_EMBED).getMatch(0);
        return fid;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        final String extDefault = ".mp4";
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + extDefault);
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title;
        if (new Regex(br._getURL().getPath(), PATTERN_NORMAL).patternFind()) {
            title = HTMLSearch.searchMetaTag(br, "og:title");
            if (title == null) {
                /* Fallback */
                title = br._getURL().getPath();
            }
            if (!link.hasProperty(PROPERTY_VIDEO_ID)) {
                /* Find internal video_id for better duplicate matching. */
                final String video_id = br.getRegex("/embed/(\\d+)").getMatch(0);
                if (video_id != null) {
                    link.setProperty(PROPERTY_VIDEO_ID, video_id);
                } else {
                    logger.warning("Failed to find video_id");
                }
            }
        } else {
            /* PATTERN_EMBED */
            title = br.getRegex("global_tokyvideo_endcard_title\\s*= \"([^\"]+)\";").getMatch(0);
        }
        dllink = br.getRegex("<source src=\"(https://[^\"]+)\"[^>]*type=\"video/mp4\">").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title);
            title = title.trim();
            link.setFinalFileName(title + extDefault);
        }
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
        if (!isDownload && !link.isSizeSet() && !StringUtils.isEmpty(dllink)) {
            this.basicLinkCheck(br, br.createHeadRequest(dllink), link, title, extDefault);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            if (con.getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Video broken?");
            }
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetPluginGlobals() {
        this.dllink = null;
    }
}