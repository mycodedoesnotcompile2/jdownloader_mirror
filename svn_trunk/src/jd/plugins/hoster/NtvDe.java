//jDownloader - Downloadmanager
//Copyright (C) 2012  JD-Team support@jdownloader.org
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
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.downloader.hls.HLSDownloader;

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

@HostPlugin(revision = "$Revision: 51865 $", interfaceVersion = 2, names = { "n-tv.de" }, urls = { "https?://(?:www\\.)?n\\-tv\\.de/mediathek/(?:videos|sendungen|magazine)/([^/]+/[^/]+)\\.html" })
public class NtvDe extends PluginForHost {
    public NtvDe(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://www.n-tv.de/ntvintern/n-tv-Webseiten-article495196.html";
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    private static final String host_hls = "http://video.n-tv.de";

    // private static final String host_http = "http://video.n-tv.de";
    /* Possible http url (low quality for old mobile phones): http://video.n-tv.de/mobile/ContentPoolSchiesspolizist_1506251137.mp4 */
    /* Available streaming types (best to worst): hls, http */
    @SuppressWarnings("deprecation")
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + ".mp4");
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getDownloadURL());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /*
         * 2022-09-13: Some URLs redirect to tvnow.de which is using DRM for all of their content e.g.
         * https://www.n-tv.de/mediathek/sendungen/RTLplus/Queen-Elizabeth-Eine-Familiengeschichte-article20787496.html
         */
        final boolean looksLikeDRMProtected = br.containsHTML("sec-mediathek_sendungen_tvnow") && getStreamURL(br) == null;
        String dateString = br.getRegex("publishedDateAsUnixTimeStamp:\\s*?\"(\\d+)\"").getMatch(0);
        if (dateString == null) {
            // publishedAt":"2025-10-31T16:43"
            final long date = TimeFormatter.getMilliSeconds(br.getRegex("\"publishedAt\"\\s*:\\s*\"([0-9T:-]+)").getMatch(0), "yyyy-MM-dd", Locale.GERMANY);
            if (date != -1) {
                dateString = Long.toString(date / 1000);
            }
        }
        String title = br.getRegex("headline:\\s*\"(.*?)\",\\s").getMatch(0);
        if (title != null) {
            title = Encoding.unicodeDecode(title);
        } else {
            // new json
            title = br.getRegex("\"headline\"\\s*:\\s*\"(.*?)\"\\s*,\\s*\"").getMatch(0);
            if (title != null) {
                title = restoreFromString("\"" + title + "\"", TypeRef.STRING);
            }
        }
        if (title != null) {
            title = title.trim();
            String filename = "";
            if (dateString != null) {
                filename = formatDate(dateString) + "_";
            }
            filename += "_n-tv_" + title + ".mp4";
            if (looksLikeDRMProtected) {
                filename = "[DRM]" + filename;
                link.setFinalFileName(filename);
            } else {
                /*
                 * Example normal non-DRM clip:
                 * https://www.n-tv.de/mediathek/videos/politik/Schande-Moskauer-verurteilen-Krieg-ploetzlich-offen-article23584866.html
                 */
                link.setFinalFileName(filename);
            }
        }
        if (looksLikeDRMProtected) {
            logger.info("This item looks to be DRM protected -> Treating it as offline");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String streamURL = getStreamURL(br);
        if (streamURL == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (streamURL.contains(".m3u8")) {
            /* HLS */
            /* Access index */
            br.getPage(org.appwork.utils.net.URLHelper.parseLocation(new URL(host_hls), streamURL));
            /* Grab highest quality possible - always located at the beginning of the index file (for this host). */
            final String best = br.getRegex("([^<>\"/\r\n\t]+\\.m3u8)\n").getMatch(0);
            if (best == null) {
                logger.warning("Failed to find HLS quality");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String url = host_hls + "/apple/" + best;
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, this.br, url);
            dl.startDownload();
        } else {
            /* http stream */
            final String filter = new Regex(streamURL, "(\\?filter=.+)").getMatch(0);
            final String url;
            if (filter != null) {
                url = streamURL.replace(filter, "");
            } else {
                url = streamURL;
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, url, true, 0);
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Geo blocked!");
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error");
                }
            }
            dl.startDownload();
        }
    }

    private String getStreamURL(final Browser br) {
        String progressive = br.getRegex("progressive\\s*?:\\s*?\"(https?[^\"]+)\"").getMatch(0);
        if (progressive == null) {
            // new json
            progressive = br.getRegex("\"web-prog\"\\s*:\\s*\"(https?.*?)\"\\s*,\\s*\"").getMatch(0);
            if (progressive != null) {
                progressive = restoreFromString("\"" + progressive + "\"", TypeRef.STRING);
            }
        }
        if (progressive != null) {
            return progressive;
        }
        String m3u8 = br.getRegex("videoM3u8\\s*:\\s*\"(/apple/[^<>\"/]+\\.m3u8)\"").getMatch(0);
        if (m3u8 == null) {
            // new json, has split video/audio
            m3u8 = br.getRegex("\"web-hls\"\\s*:\\s*\"(https?.*?)\"\\s*,\\s*\"").getMatch(0);
            if (m3u8 != null) {
                m3u8 = restoreFromString("\"" + m3u8 + "\"", TypeRef.STRING);
            }
        }
        return m3u8;
    }

    public String formatDate(final String input) {
        String formattedDate = null;
        final String targetFormat = "yyyy-MM-dd";
        final Date theDate = new Date(Long.parseLong(input) * 1000);
        try {
            final SimpleDateFormat formatter = new SimpleDateFormat(targetFormat);
            formattedDate = formatter.format(theDate);
        } catch (Exception e) {
            /* prevent input error killing plugin */
            formattedDate = input;
        }
        return formattedDate;
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}