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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 53132 $", interfaceVersion = 3, names = {}, urls = {})
public class ArdaudiothekDe extends PluginForDecrypt {
    public ArdaudiothekDe(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "ardaudiothek.de", "ardsounds.de" });
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

    /*
     * Matches both the old numeric URL layouts and the new ardsounds.de URN based ones. Group 1 = content ID (numeric or URN):</br>
     * /sendung/<slug>/<numeric-id|urn:ard:show:...></br>
     * /episode/[<slug>/...]<numeric-id|urn:ard:episode|section|extra:...>
     */
    private static final Pattern PATTERN_CONTENT = Pattern.compile("/(?:sendung|episode)/(?:[\\w\\-]+/)*(\\d+|urn:ard:[a-z]+:[a-fA-F0-9]+)/?");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_CONTENT.pattern());
        }
        return ret.toArray(new String[0]);
    }

    /** GraphQL API endpoint, see https://api.ardaudiothek.de/docs/ */
    private static final String                 API_BASE       = "https://api.ardaudiothek.de/graphql";
    private static final String                 QUERY_EPISODE  = "query($id:ID!){item(id:$id){id title description duration startDate episodeNumber image{url1X1} show{id title} programSet{id publicationService{organizationName}} audioList{href distributionType audioBitrate audioCodec}}}";
    private static final String                 QUERY_SHOW     = "query($id:ID!,$count:Int!,$after:Cursor){show(id:$id){id title description publicationService{organizationName} items(first:$count after:$after filter:{isPublished:{equalTo:true}}){pageInfo{hasNextPage endCursor} nodes{id url title description duration startDate episodeNumber image{url1X1} audioList{href distributionType audioBitrate audioCodec}}}}}";
    private static final int                    ITEMS_PER_PAGE = 24;

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contentURL = param.getCryptedUrl();
        final String id = new Regex(contentURL, PATTERN_CONTENT).getMatch(0);
        if (id == null) {
            /* Should never happen as this is enforced by the plugin pattern. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final boolean isShow;
        if (id.startsWith("urn:")) {
            /* The type is encoded in the URN itself. */
            isShow = id.startsWith("urn:ard:show:");
        } else {
            /* Old numeric layout: only the path tells us show vs. single episode. */
            isShow = contentURL.contains("/sendung/");
        }
        if (isShow) {
            return crawlShow(id);
        } else {
            return crawlEpisode(id, contentURL);
        }
    }

    /** Crawls a single episode/section/extra via item(id: ...). */
    private ArrayList<DownloadLink> crawlEpisode(final String id, final String contentURL) throws Exception {
        final Map<String, Object> data = callGraphql(QUERY_EPISODE, "{\"id\":\"" + id + "\"}");
        final Map<String, Object> item = data != null ? (Map<String, Object>) data.get("item") : null;
        if (item == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String station = (String) JavaScriptEngineFactory.walkJson(item, "programSet/publicationService/organizationName");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        ret.add(buildEpisodeLink(item, contentURL, station, 0, 1, true));
        final Map<String, Object> show = (Map<String, Object>) item.get("show");
        final String showTitle = show != null ? (String) show.get("title") : null;
        if (showTitle != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(showTitle);
            final Object showID = JavaScriptEngineFactory.walkJson(item, "programSet/id");
            if (showID != null) {
                fp.setPackageKey("ardaudiothek://show/" + showID);
            }
            fp.addLinks(ret);
        }
        return ret;
    }

    /** Crawls a complete show (all published episodes) via show(id: ...). */
    private ArrayList<DownloadLink> crawlShow(final String id) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final List<Map<String, Object>> allNodes = new ArrayList<Map<String, Object>>();
        String showTitle = null;
        String showDescription = null;
        String showID = null;
        String station = null;
        String after = null;
        int page = 0;
        do {
            page++;
            final String variables = "{\"id\":\"" + id + "\",\"count\":" + ITEMS_PER_PAGE + (after != null ? ",\"after\":\"" + after + "\"" : "") + "}";
            final Map<String, Object> data = callGraphql(QUERY_SHOW, variables);
            final Map<String, Object> show = data != null ? (Map<String, Object>) data.get("show") : null;
            if (show == null) {
                if (page == 1) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    break;
                }
            }
            if (showTitle == null) {
                showTitle = (String) show.get("title");
                showDescription = (String) show.get("description");
                showID = show.get("id") != null ? show.get("id").toString() : null;
                station = (String) JavaScriptEngineFactory.walkJson(show, "publicationService/organizationName");
            }
            final Map<String, Object> items = (Map<String, Object>) show.get("items");
            final Map<String, Object> pageInfo = (Map<String, Object>) items.get("pageInfo");
            final List<Map<String, Object>> nodes = (List<Map<String, Object>>) items.get("nodes");
            if (nodes != null) {
                allNodes.addAll(nodes);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + allNodes.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (!Boolean.TRUE.equals(pageInfo.get("hasNextPage"))) {
                logger.info("Stopping because: Reached last page");
                break;
            }
            after = (String) pageInfo.get("endCursor");
        } while (after != null);
        if (allNodes.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(showTitle);
        if (!StringUtils.isEmpty(showDescription)) {
            fp.setComment(showDescription);
        }
        if (showID != null) {
            fp.setPackageKey("ardaudiothek://show/" + showID);
        }
        final int padLength = StringUtils.getPadLength(allNodes.size());
        final boolean single = allNodes.size() == 1;
        int position = 1;
        for (final Map<String, Object> node : allNodes) {
            final String episodeURL = (String) node.get("url");
            ret.add(buildEpisodeLink(node, episodeURL, station, padLength, position, single));
            position++;
        }
        fp.addLinks(ret);
        return ret;
    }

    /** Builds one DownloadLink from an episode item/node using its best available audio format. */
    private DownloadLink buildEpisodeLink(final Map<String, Object> episode, final String contentURL, final String station, final int padLength, final int position, final boolean single) throws PluginException {
        final List<Map<String, Object>> audios = (List<Map<String, Object>>) episode.get("audioList");
        final Map<String, Object> best = pickBestAudio(audios);
        if (best == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String href = best.get("href").toString();
        if (href.startsWith("//")) {
            href = "https:" + href;
        }
        final DownloadLink link = this.createDownloadlink(href);
        final String title = (String) episode.get("title");
        String ext = new Regex(href, "\\.([A-Za-z0-9]{2,4})(?:\\?.*)?$").getMatch(0);
        if (ext == null) {
            ext = "mp3";
        }
        final String filename;
        if (single) {
            filename = title + "." + ext;
        } else {
            filename = StringUtils.formatByPadLength(padLength, position) + " - " + title + "." + ext;
        }
        link.setFinalFileName(filename);
        link.setAvailable(true);
        final String description = (String) episode.get("description");
        if (!StringUtils.isEmpty(description)) {
            link.setComment(description);
        }
        /* Estimate filesize based on the bitrate of the chosen audio format. */
        final Number durationSeconds = (Number) episode.get("duration");
        final Number bitrate = (Number) best.get("audioBitrate");
        if (durationSeconds != null && bitrate != null) {
            link.setDownloadSize((durationSeconds.longValue() * bitrate.longValue() * 1000) / 8);
        }
        if (!StringUtils.isEmpty(contentURL)) {
            link.setContentUrl(contentURL);
        }
        /* Preserve additional metadata. */
        final Object episodeNumber = episode.get("episodeNumber");
        if (episodeNumber != null) {
            link.setProperty("episodeNumber", episodeNumber);
        }
        final String startDate = (String) episode.get("startDate");
        if (!StringUtils.isEmpty(startDate)) {
            link.setProperty("date", startDate);
        }
        if (!StringUtils.isEmpty(station)) {
            link.setProperty("station", station);
        }
        final String thumbnail = (String) JavaScriptEngineFactory.walkJson(episode, "image/url1X1");
        if (!StringUtils.isEmpty(thumbnail)) {
            link.setProperty("thumbnailurl", thumbnail.replace("{width}", "448"));
        }
        return link;
    }

    /** Selects the best audio format: prefers real downloads and then the highest bitrate. */
    private Map<String, Object> pickBestAudio(final List<Map<String, Object>> audios) {
        if (audios == null || audios.isEmpty()) {
            return null;
        }
        Map<String, Object> best = null;
        long bestScore = -1;
        for (final Map<String, Object> audio : audios) {
            if (StringUtils.isEmpty((String) audio.get("href"))) {
                continue;
            }
            final Number bitrate = (Number) audio.get("audioBitrate");
            long score = bitrate != null ? bitrate.longValue() : 0;
            if ("download".equals(audio.get("distributionType"))) {
                score += 1000000;
            }
            if (score > bestScore) {
                best = audio;
                bestScore = score;
            }
        }
        return best;
    }

    /** Performs a GraphQL POST request and returns the "data" object of the response. */
    private Map<String, Object> callGraphql(final String query, final String variables) throws IOException, PluginException {
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Content-Type", "application/json");
        brc.getHeaders().put("Accept", "application/json");
        final String body = "{\"query\":\"" + query + "\",\"variables\":" + variables + "}";
        brc.postPageRaw(API_BASE, body);
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (entries == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Object errors = entries.get("errors");
        if (errors != null) {
            logger.warning("GraphQL returned errors: " + errors);
        }
        return (Map<String, Object>) entries.get("data");
    }
}
