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
import java.util.HashSet;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51239 $", interfaceVersion = 3, names = {}, urls = {})
public class SuperpornCom extends PluginForHost {
    public SuperpornCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookie(this.getHost(), "lang", "en_US");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
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
        ret.add(new String[] { "superporn.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern TYPE_NORMAL       = Pattern.compile("/(?:[a-z]{2}/)?video/([\\w-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_EMBED        = Pattern.compile("/embed/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final String  INTERNAL_VIDEO_ID = "internal_video_id";

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_EMBED.pattern() + "|" + TYPE_NORMAL.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/tos";
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
        final String storedFid = link.getStringProperty(INTERNAL_VIDEO_ID);
        if (storedFid != null) {
            return storedFid;
        }
        String fid = new Regex(link.getPluginPatternMatcher(), TYPE_EMBED).getMatch(0);
        if (fid == null) {
            fid = new Regex(link.getPluginPatternMatcher(), TYPE_NORMAL).getMatch(0);
        }
        return fid;
    }

    private String getUrlSlug(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), TYPE_NORMAL).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        dllink = null;
        final String extDefault = ".mp4";
        final String fid = this.getFID(link);
        String urlSlug = getUrlSlug(link);
        final Regex embed = new Regex(link.getPluginPatternMatcher(), TYPE_EMBED);
        if (!link.isNameSet()) {
            if (urlSlug != null) {
                link.setName(urlSlug + extDefault);
            } else {
                link.setName(fid + extDefault);
            }
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (embed.patternFind()) {
            urlSlug = br.getRegex("global_url_video\\s*=\\s*.https?://[^/]+/video/([\\w-]+)").getMatch(0);
        } else {
            if (!link.hasProperty(INTERNAL_VIDEO_ID)) {
                /* Search for internal video ID for more reliable duplicate detection. It should be contained in html exactly once. */
                final String[] embedIDs = br.getRegex("/embed/(\\d+)").getColumn(0);
                if (embedIDs != null && embedIDs.length > 0) {
                    final HashSet<String> dupes = new HashSet<String>();
                    for (final String embedID : embedIDs) {
                        dupes.add(embedID);
                    }
                    if (dupes.size() == 1) {
                        link.setProperty(INTERNAL_VIDEO_ID, embedIDs[0]);
                    } else {
                        logger.warning("Failed to find internalVideoID");
                    }
                }
            }
        }
        String filename = null;
        if (urlSlug != null) {
            urlSlug = urlSlug.replace("-", " ").trim();
            filename = urlSlug + extDefault;
        }
        dllink = br.getRegex("<source src=\"(https?://[^\"]+)\" type=\"video/mp4\">").getMatch(0);
        if (!StringUtils.isEmpty(dllink) && !isDownload) {
            this.basicLinkCheck(br, br.createHeadRequest(this.dllink), link, filename, null);
        } else {
            link.setFinalFileName(filename);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
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
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}