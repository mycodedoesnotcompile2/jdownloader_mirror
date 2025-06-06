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
import java.util.List;
import java.util.Map;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51000 $", interfaceVersion = 3, names = { "hqwo.cc" }, urls = { "https?://(?:www\\.)?hqwo\\.cc/player/([a-f0-9]{32})" })
public class HqwoCc extends PluginForHost {
    public HqwoCc(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* DEV NOTES */
    // Tags: Porn plugin
    // protocol: no https
    // other:
    /* Connection stuff */
    private static final boolean free_resume    = true;
    private static final int     free_maxchunks = 0;
    private String               dllink         = null;

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        dllink = null;
        /* E.g. items from hqporner.com */
        final String title = link.getStringProperty("title");
        final String filename;
        final String extDefault = ".mp4";
        if (title != null) {
            filename = title + extDefault;
        } else {
            filename = getFID(link) + extDefault;
        }
        if (!link.isNameSet()) {
            link.setName(filename);
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String continue_url = br.getRegex("\\'(https?://[^/]+/runplayer/[^<>\"\\']+)\\'").getMatch(0);
        if (continue_url != null) {
            br.getPage(continue_url);
        }
        /* RegExes sometimes used for streaming */
        final String jssource = br.getRegex("play\\((\\[.*?\\])").getMatch(0);
        if (jssource != null) {
            try {
                Map<String, Object> entries = null;
                Object quality_temp_o = null;
                long quality_temp = 0;
                String quality_temp_str = null;
                long quality_best = 0;
                String dllink_temp = null;
                final List<Object> ressourcelist = (List) JavaScriptEngineFactory.jsonToJavaObject(jssource);
                for (final Object videoo : ressourcelist) {
                    entries = (Map<String, Object>) videoo;
                    dllink_temp = (String) entries.get("file");
                    quality_temp_o = entries.get("label");
                    if (quality_temp_o != null && quality_temp_o instanceof Number) {
                        quality_temp = JavaScriptEngineFactory.toLong(quality_temp_o, 0);
                    } else if (quality_temp_o != null && quality_temp_o instanceof String) {
                        quality_temp_str = (String) quality_temp_o;
                        if (quality_temp_str.matches("\\d+p.*?")) {
                            /* E.g. '360p' */
                            quality_temp = Long.parseLong(new Regex(quality_temp_str, "(\\d+)p").getMatch(0));
                        } else {
                            /* Bad / Unsupported format */
                            continue;
                        }
                    }
                    if (StringUtils.isEmpty(dllink_temp) || quality_temp == 0) {
                        continue;
                    } else if (dllink_temp.contains(".m3u8")) {
                        /* Skip hls */
                        continue;
                    }
                    if (quality_temp > quality_best) {
                        quality_best = quality_temp;
                        dllink = dllink_temp;
                    }
                }
                if (!StringUtils.isEmpty(dllink)) {
                    logger.info("BEST handling for multiple video source succeeded");
                }
            } catch (final Throwable e) {
                logger.info("BEST handling for multiple video source failed");
            }
        }
        final String[] qualities = br.getRegex("(//[^\"]+/\\d+\\.mp4)").getColumn(0);
        int bestQualityHeight = 0;
        for (final String quality : qualities) {
            final int height = Integer.parseInt(new Regex(quality, "(\\d+)\\.mp4$").getMatch(0));
            if (height > bestQualityHeight) {
                bestQualityHeight = height;
                dllink = quality;
            }
        }
        if (this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD) {
            /* Do not check for filesize as download will be started soon */
            return AvailableStatus.TRUE;
        }
        this.basicLinkCheck(br, br.createHeadRequest(this.dllink), link, filename, ".mp4");
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume, free_maxchunks);
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
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
