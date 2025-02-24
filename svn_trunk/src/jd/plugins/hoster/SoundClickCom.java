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

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.HashInfo;
import jd.plugins.download.raf.HTTPDownloader;

@HostPlugin(revision = "$Revision: 49207 $", interfaceVersion = 2, names = {}, urls = {})
public class SoundClickCom extends PluginForHost {
    public SoundClickCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.AUDIO_STREAMING };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "soundclick.com" });
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:bands/page_songInfo|html5/v4/player|music/songInfo)\\.cfm\\?(?:bandID=\\d+\\&)?songID=(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    private String dllink = null;

    @Override
    public String getAGBLink() {
        return "https://www.soundclick.com/docs/legal.cfm";
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
        return requestFileInformation(link, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException {
        final String songID = getFID(link);
        final String extDefault = ".mp3";
        if (!link.isNameSet()) {
            /* Offline links should also have nice filenames */
            link.setName(songID + extDefault);
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String songURL = "https://www." + this.getHost() + "/music/songInfo.cfm?songID=" + songID + "&popup=true";
        br.getPage(songURL);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(songID)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("(?i)<title>Error</title>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<div id=\"sclkArtist_songInfo_title\"[^>]*>([^<]+)</div>").getMatch(0);
        if (title != null) {
            link.setFinalFileName(Encoding.htmlDecode(title).trim() + extDefault);
        }
        String filesizeFromHTML = null;
        final String[][] songInfoMetaBlocks = br.getRegex("<div class=\"songinfo_metaBlock_labels\"[^>]*>([^<]+)</div>\\s*<div>([^<]+)</div>").getMatches();
        if (songInfoMetaBlocks != null) {
            for (final String[] songInfoMetaBlock : songInfoMetaBlocks) {
                final String format = songInfoMetaBlock[0];
                final String metadata = songInfoMetaBlock[1];
                if (StringUtils.equalsIgnoreCase(format, "MP3")) {
                    filesizeFromHTML = new Regex(metadata, "(?i)MP3 (\\d+(?:\\.\\d{1,2})? [A-Za-z]+)").getMatch(0);
                    logger.info("Successfully found filesize in html: " + filesizeFromHTML);
                    break;
                }
            }
        }
        if (filesizeFromHTML != null) {
            /* Use filesize found in html code */
            link.setDownloadSize(SizeFormatter.getSize(filesizeFromHTML));
        }
        return AvailableStatus.TRUE;
    }

    private void connectionErrorhandling(final URLConnectionAdapter con) throws IOException, PluginException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Broken audio file?");
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        final String songID = getFID(link);
        /* 2020-05-06 */
        final String officialDownloadurl = "https://www." + this.getHost() + "/utils_download/download_song.cfm?ID=" + getFID(link);
        final String streamLink = "https://www." + getHost() + "/playerV5/panels/audioStream.cfm?songID=" + songID + "&r=0." + System.currentTimeMillis();
        // br.getPage(officialDownloadurl);
        br.getHeaders().put("Referer", officialDownloadurl);
        final boolean preferStreamDownload = false;
        if (preferStreamDownload) {
            /* 2023-05-22: This seems to do the same but we will not get a header containing the md5 hash. */
            dllink = streamLink;
        } else {
            dllink = String.format("/utils_download/download_songDeliver.cfm?songID=%s&ppID=0&selectLevel=160", this.getFID(link));
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection()) && !preferStreamDownload) {
            logger.info("Attempting fallback to stream download");
            try {
                dl.getConnection().disconnect();
            } catch (Throwable e) {
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, streamLink, true, 1);
        }
        this.connectionErrorhandling(dl.getConnection());
        final HashInfo hashInfo = HTTPDownloader.parseAmazonHash(getLogger(), this.dl.getConnection());
        String etag = this.br.getRequest().getResponseHeader("ETag");
        if (hashInfo != null) {
            link.setHashInfo(hashInfo);
        } else if (etag != null) {
            /* chip.de servers will often | always return md5 hash via headers! */
            try {
                etag = etag.replace("\"", "");
                final String[] etagInfo = etag.split(":");
                final String md5 = etagInfo[0];
                if (md5.matches("[A-Fa-f0-9]{32}")) {
                    link.setMD5Hash(md5);
                }
            } catch (final Throwable ignore) {
            }
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
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
