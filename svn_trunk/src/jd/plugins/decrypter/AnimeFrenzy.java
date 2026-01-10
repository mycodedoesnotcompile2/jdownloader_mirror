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
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 52075 $", interfaceVersion = 2, names = {}, urls = {})
public class AnimeFrenzy extends antiDDoSForDecrypt {
    public AnimeFrenzy(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* 2025-11-27: All domains except animefrenzy.cc are dead */
        ret.add(new String[] { "animefrenzy.cc", "animefrenzy.vip", "animefrenzy.net", "animefrenzy.org" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/((?:anime|cartoon|watch|stream)/[^/]+|(?!player)[\\w\\-]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>(?:Watch\\s+)?([^<]+)\\s+(?:- Watch Anime Online|English\\s+[SD]ub\\s+)").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        } else {
            fp.setName(br._getURL().getPath());
        }
        fp.addLinks(ret);
        final ArrayList<String> links = new ArrayList<String>();
        Collections.addAll(links, br.getRegex("<li[^>]*>\\s*<a[^>]+href\\s*=\\s*[\"']([^\"']+/watch/[^\"']+)[\"']").getColumn(0));
        Collections.addAll(links, br.getRegex("<a[^>]+class\\s*=\\s*[\"']noepia[\"'][^>]+href\\s*=\\s*[\"']([^\"']+)[\"']").getColumn(0));
        String[][] hostLinks = br.getRegex("\\\"(?:host|id)\\\"\\s*:\\s*\\\"([^\\\"]+)\\\"[^\\}]+\\\"(?:host|id)\\\"\\s*:\\s*\\\"([^\\\"]+)\\\"[^\\}]+\\\"type\\\"\\s*:\\s*\\\"(?:subbed|cartoon)\\\"").getMatches();
        for (String[] hostLink : hostLinks) {
            links.add(buildEmbedURL(hostLink[0], hostLink[1]));
            links.add(buildEmbedURL(hostLink[1], hostLink[0]));
        }
        for (String link : links) {
            if (link != null) {
                link = br.getURL(link).toString();
                ret.add(createDownloadlink(link));
            }
        }
        final String embedURL = br.getRegex("(/player/v\\d+[^\"']+)").getMatch(0);
        if (embedURL != null) {
            /* 2022-10-13: Look for selfhosted content */
            this.getPage(embedURL);
            final String hlsmaster = br.getRegex("file\\s*:\\s*\"(https?://[^\"]+\\.m3u8)\"").getMatch(0);
            if (hlsmaster != null) {
                ret.add(this.createDownloadlink(hlsmaster));
            }
        }
        final String api_base = "https://api.hianimeto.site/";
        final String api_token = "YopgjtY0CA0q6a7NX1Oe";
        final Regex animeurlRegex = new Regex(br.getURL(), "(?i)https?://[^/]+/anime/([\\w\\-]+)$");
        if (ret.isEmpty() && animeurlRegex.patternFind()) {
            /* Crawl all episodes from a series */
            final String animeSlug = animeurlRegex.getMatch(0);
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Origin", "https://" + br.getHost());
            brc.getPage(api_base + "/anime/slug/" + animeSlug + "?token=" + api_token);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (Boolean.TRUE.equals(entries.get("error"))) {
                /* E.g. {"message":"Nothing to see here move along...","error":true} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            final List<Map<String, Object>> episodes = (List<Map<String, Object>>) data.get("episodes");
            for (final Map<String, Object> episode : episodes) {
                final List<Map<String, Object>> videos = (List<Map<String, Object>>) episode.get("videos");
                ret.addAll(crawlVideos(videos));
            }
        }
        /* 2023-08-17 */
        final String slugFromURL = new Regex(br.getURL(), "https?://[^/]+/stream/([^/]+)").getMatch(0);
        if (ret.isEmpty() && slugFromURL != null) {
            /* 2023-08-17: Token is from: https://animefrenzy.org/static/js/main.60f4e127.chunk.js */
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Origin", "https://" + br.getHost());
            brc.getPage(api_base + "/anime-episode/slug/" + slugFromURL + "?token=" + api_token);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (Boolean.TRUE.equals(entries.get("error"))) {
                /* E.g. {"message":"Nothing to see here move along...","error":true} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            title = data.get("name").toString();
            final List<Map<String, Object>> videos = (List<Map<String, Object>>) data.get("videos");
            ret.addAll(crawlVideos(videos));
        }
        final String episodeID = UrlQuery.parse(br.getURL()).get("ep");
        // TODO: Crawl all episodes of a series if there is no single episode id given inside URL
        final String seriesID = new Regex(br.getURL(), "-(\\d+)$").getMatch(0);
        if (episodeID != null) {
            /* 2026-01-09 */
            final Browser brc = br.cloneBrowser();
            brc.getPage("https://nine.mewcdn.online/ajax/episode/servers?episodeId=" + episodeID);
            final Map<String, Object> epinfo = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(epinfo.get("status"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String html = epinfo.get("html").toString();
            br.getRequest().setHtmlCode(html);
            final String[] mirror_ids = br.getRegex("data-id=\"(\\d+)").getColumn(0);
            for (final String mirror_id : mirror_ids) {
                brc.getPage("/ajax/episode/sources?id=" + mirror_id);
                final Map<String, Object> mirrorinfo = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final String url = mirrorinfo.get("link").toString();
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlVideos(final List<Map<String, Object>> videos) {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final Map<String, Object> video : videos) {
            final DownloadLink link = crawlVideo(video);
            if (link != null) {
                ret.add(link);
            }
        }
        return ret;
    }

    private DownloadLink crawlVideo(final Map<String, Object> video) {
        final String host = video.get("host").toString();
        final String video_id = video.get("video_id").toString();
        final String url = buildEmbedURL(host, video_id);
        if (url == null) {
            logger.warning("Failed to generate URL for host: " + host + " | video_id: " + video_id);
            return null;
        }
        return this.createDownloadlink(url);
    }

    private String buildEmbedURL(final String host, final String id) {
        final String result;
        if (host.equals("trollvid")) {
            result = "https//trollvid.net/embed/" + id;
        } else if (host.equals("mp4.sh")) {
            result = "https://trollvid.net/embedc/" + id;
        } else if (host.equals("mp4upload")) {
            result = "//www.mp4upload.com/embed-" + id + ".html";
        } else if (host.equals("xstreamcdn")) {
            result = "https://www.xstreamcdn.com/v/" + id;
        } else if (host.equals("yare.wtf")) {
            result = "https://yare.wtf/vidstreaming/download/" + id;
        } else if (host.equals("facebook")) {
            result = "https://www.facebook.com/plugins/video.php?href=https%3A%2F%2Fwww.facebook.com%2Flayfon.alseif.16%2Fvideos%2F" + id + "%2F";
        } else if (host.equals("upload2")) {
            result = "https//upload2.com/embed/" + id;
        } else {
            logger.info("Unknown host: " + host);
            result = null;
        }
        return result;
    }
}