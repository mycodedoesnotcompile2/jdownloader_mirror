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
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
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

@HostPlugin(revision = "$Revision: 51083 $", interfaceVersion = 3, names = {}, urls = {})
public class HotscopeTv extends PluginForHost {
    public HotscopeTv(PluginWrapper wrapper) {
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

    /* Connection stuff */
    private static final boolean free_resume    = true;
    private static final int     free_maxchunks = 0;
    private String               dllink         = null;
    private static final Pattern TYPE_NEW       = Pattern.compile("/video/([a-zA-Z0-9]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_OLD       = Pattern.compile("/(\\w+)/(\\d+)", Pattern.CASE_INSENSITIVE);

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "hotscope.tv" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_NEW.pattern() + "|" + TYPE_OLD.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
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
        final String video_id_new = new Regex(link.getPluginPatternMatcher(), TYPE_NEW).getMatch(0);
        if (video_id_new != null) {
            return video_id_new;
        } else {
            final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), TYPE_OLD);
            return urlinfo.getMatch(0) + "_" + urlinfo.getMatch(1);
        }
    }

    private final String PROPERTY_DATE               = "date";
    private final String PROPERTY_USERNAME           = "user";
    private final String PROPERTY_VIEWS              = "views";
    private final String PROPERTY_PERCENTAGE_UPVOTES = "percentage_upvotes";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String ext_default = ".mp4";
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + ext_default);
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("\"name\"\\s*:\\s*\"([^\"]+)").getMatch(0);
        dllink = br.getRegex("\"contentUrl\"\\s*:\\s*\"(http[^\"]+)").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title);
            title = title.trim();
            link.setFinalFileName(title + ext_default);
        }
        /* Packagizer properties */
        final String username = br.getRegex("<a title=\"([^\"]+)\" href=\"/channel/\\d+\"").getMatch(0);
        if (username != null) {
            link.setProperty(PROPERTY_USERNAME, Encoding.htmlDecode(username).trim());
        }
        final String numberofViews = br.getRegex("\"views\"\\s*:\\s*(\\d+)").getMatch(0);
        if (numberofViews != null) {
            setViewCount(link, Integer.parseInt(numberofViews));
        }
        final String upvotesPercentage = br.getRegex("class=\"MuiTypography-root MuiTypography-body1 MuiTypography-colorTextSecondary\">(\\d+)<\\!-- -->%").getMatch(0);
        if (upvotesPercentage != null) {
            link.setProperty(PROPERTY_PERCENTAGE_UPVOTES, upvotesPercentage);
        }
        final String uploadDate = br.getRegex("\"uploadDate\"\\s*:\\s*\"([^\"]+)").getMatch(0);
        if (PROPERTY_DATE != null) {
            link.setProperty(PROPERTY_DATE, uploadDate);
        }
        final PluginEnvironment env = this.getPluginEnvironment();
        if (!StringUtils.isEmpty(dllink) && !StringUtils.endsWithCaseInsensitive(dllink, ".m3u8") && !link.isSizeSet() && env != PluginEnvironment.DOWNLOAD) {
            URLConnectionAdapter con = null;
            try {
                con = br.openHeadConnection(this.dllink);
                handleConnectionErrors(con);
                if (con.getCompleteContentLength() > 0) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    private final void setViewCount(final DownloadLink link, final Number number) {
        link.setProperty(PROPERTY_VIEWS + "_string", TiktokCom.toHumanReadableNumber(number));
        link.setProperty(PROPERTY_VIEWS, number.longValue());
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (StringUtils.endsWithCaseInsensitive(dllink, ".m3u8")) {
            br.getPage(dllink);
            final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(this.br));
            if (hlsbest == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dllink = hlsbest.getDownloadurl();
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, dllink);
            dl.startDownload();
        } else {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume, free_maxchunks);
            handleConnectionErrors(dl.getConnection());
            dl.startDownload();
        }
    }

    private void handleConnectionErrors(final URLConnectionAdapter con) throws PluginException {
        if (!this.looksLikeDownloadableContent(con)) {
            if (con.getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            }
            try {
                br.followConnection(true);
            } catch (final IOException e) {
                logger.log(e);
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Video broken?");
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}