//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

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
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52917 $", interfaceVersion = 2, names = {}, urls = {})
public class PornHostCom extends PluginForHost {
    private String ending = null;
    private String dllink = null;

    public PornHostCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "pornhost.com" });
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

    private static final Pattern PATTERN_FILE  = Pattern.compile("/([0-9]+(?:/[0-9]+\\.html)?)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_EMBED = Pattern.compile("/embed/(\\d+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:" + PATTERN_FILE.pattern().substring(1) + "|" + PATTERN_EMBED.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_EMBED).getMatch(0);
        if (fid == null) {
            fid = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
        }
        return fid;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        }
        return super.getLinkID(link);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    @Override
    public String getAGBLink() {
        return "http://www." + getHost() + "/tos.html";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link) + ".mp4";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String contenturl = link.getPluginPatternMatcher().replace("/embed/", "/");
        br.getPage(contenturl);
        if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        boolean isVideo = true;
        if (link.getPluginPatternMatcher().contains(".html")) {
            dllink = br.getRegex("class=\"image\"[^>]*>\\s*<img src=\"(http[^\"]+)\"").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("\"(https?://file[0-9]+\\.pornhost\\.com/[0-9]+/.*?)\"").getMatch(0);
            }
        } else {
            dllink = br.getRegex("\"(https?://cdn\\d+\\.dl\\.pornhost\\.com/[^<>\"]*?)\"").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("file: \"(.*?)\"").getMatch(0);
            }
            if (dllink == null) {
                /* 2020-04-30 */
                dllink = br.getRegex("class=\"download button\" target=\"blank\" href=\"(https[^<>\"]+)").getMatch(0);
            }
            if (dllink == null) {
                /* 2020-04-30 */
                dllink = br.getRegex("<source src=\"(http[^<>\"]+)\" type=\"video/mp4\">").getMatch(0);
            }
        }
        // Maybe we have a picture
        if (dllink == null) {
            dllink = br.getRegex("<div class=\"image\" style=\"width: \\d+px; height: \\d+px\">[\t\n\r ]+<img src=\"(http://[^<>\"]*?)\"").getMatch(0);
            isVideo = false;
        }
        final String extDefault;
        if (isVideo) {
            extDefault = ".mp4";
        } else {
            extDefault = ".jpg";
        }
        String title = br.getRegex("class=\"video-title\"[^>]*>([^<]+)</h1>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            link.setName(title + extDefault);
        }
        if (dllink != null) {
            dllink = Encoding.htmlOnlyDecode(dllink);
            URLConnectionAdapter con = null;
            try {
                final Browser brc = br.cloneBrowser();
                con = brc.openHeadConnection(dllink);
                if (con.getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Final downloadurl is offline -> Content looks to be offline");
                }
                if (!looksLikeDownloadableContent(con)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
            } finally {
                try {
                    con.disconnect();
                } catch (Throwable e) {
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    public static boolean isOffline(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 404) {
            return true;
        } else if (br.containsHTML("gallery not found") || br.containsHTML("You will be redirected to")) {
            return true;
        } else if (!br.containsHTML("class=\"report button\"")) {
            /* Offline without error message e.g. /2435978716 */
            return true;
        }
        return false;
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (br.containsHTML(">\\s*The movie needs to be converted first")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "The movie needs to be converted first", 30 * 60 * 1000l);
        } else if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        try {
            dl.setAllowFilenameFromURL(false);
            String name = Plugin.getFileNameFromConnection(dl.getConnection());
            if (ending != null && ending.length() <= 1) {
                String name2 = link.getName();
                name = new Regex(name, ".+?(\\..{1,4})").getMatch(0);
                if (name != null && !name2.endsWith(name)) {
                    name2 = name2 + name;
                    link.setFinalFileName(name2);
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        dl.startDownload();
    }
}
