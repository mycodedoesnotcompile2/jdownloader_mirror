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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.scripting.JavaScriptEngineFactory;

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
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.BbcCom;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52341 $", interfaceVersion = 3, names = { "bbc.com" }, urls = { "https?://(?:www\\.)?(?:bbc\\.com|bbc\\.co\\.uk)/.+" })
public class BbcComDecrypter extends PluginForDecrypt {
    public BbcComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private final Pattern         TYPE_EMBED      = Pattern.compile("/[^/]+/av-embeds/.+", Pattern.CASE_INSENSITIVE);
    private final Pattern         TYPE_PROGRAMMES = Pattern.compile("/programmes/([^/]+)$", Pattern.CASE_INSENSITIVE);
    private final HashSet<String> globaldupes     = new HashSet<String>();

    @SuppressWarnings("unchecked")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        globaldupes.clear();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("This programme is not currently available on BBC iPlayer")) {
            /* Content is online but not streamable at the moment */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (new Regex(br.getURL(), TYPE_EMBED).patternFind()) {
            return this.crawlEmbed(param);
        }
        String url_name = null;
        if (ret.size() == 0 && param.getCryptedUrl().matches(".+/video/[^/]+/.+")) {
            url_name = new Regex(param.getCryptedUrl(), "/video/[^/]+/(.+)").getMatch(0);
        }
        String pageTitle = br.getRegex("property=\"og:title\" content=\"([^\"]+)\"").getMatch(0);
        if (pageTitle == null) {
            pageTitle = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        }
        crawlLegacyJsonBlocks(ret, param);
        /* 2022-01-17: New handling */
        final String jsonMorphSingle = br.getRegex("Morph\\.setPayload\\('[^\\']+', (\\{.*?\\})\\);").getMatch(0);
        if (jsonMorphSingle != null) {
            final Map<String, Object> root = restoreFromString(jsonMorphSingle, TypeRef.MAP);
            final Map<String, Object> body = (Map<String, Object>) root.get("body");
            pageTitle = (String) body.get("pageTitle");
            final List<Map<String, Object>> videos = (List<Map<String, Object>>) body.get("videos");
            if (videos != null) {
                for (final Map<String, Object> video : videos) {
                    final String vpid = (String) video.get("versionPid");
                    if (StringUtils.isEmpty(vpid) || !globaldupes.add(vpid)) {
                        continue;
                    }
                    final String title = (String) video.get("title");
                    final String description = (String) video.get("description");
                    final String tv_brand = (String) video.get("masterBrand");
                    final String date = (String) video.get("createdDateTime");
                    final DownloadLink media = generateDownloadlink(vpid);
                    if (!StringUtils.isEmpty(title)) {
                        media.setProperty(BbcCom.PROPERTY_TITLE, title);
                        media.setProperty(BbcCom.PROPERTY_DATE, new Regex(date, "(\\d{4}-\\d{2}-\\d{2})").getMatch(0));
                        media.setProperty(BbcCom.PROPERTY_TV_BRAND, tv_brand);
                        media.setContentUrl(param.getCryptedUrl());
                        if (!StringUtils.isEmpty(description)) {
                            media.setComment(description);
                        }
                        media.setName(BbcCom.getFilename(media));
                        ret.add(media);
                    }
                }
            }
        }
        /* 2022-03-04 e.g. https://www.bbc.com/news/av/world-europe-60608706 */
        final String json2022 = br.getRegex("window\\.__INITIAL_DATA__=\"(.*?\\})\"").getMatch(0);
        if (json2022 != null) {
            final String json2022_fixed = PluginJSonUtils.unescape(json2022);
            final Object videoO = findVideoMap(JavaScriptEngineFactory.jsonToJavaObject(json2022_fixed));
            if (videoO != null) {
                final Map<String, Object> video = (Map<String, Object>) videoO;
                final Map<String, Object> structuredData = (Map<String, Object>) video.get("structuredData");
                final String description = (String) structuredData.get("description");
                final String dateFormatted = new Regex(structuredData.get("uploadDate").toString(), "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
                final String embedUrl = (String) structuredData.get("embedUrl");
                final Map<String, Object> videoInfo = (Map<String, Object>) JavaScriptEngineFactory.walkJson(video, "mediaItem/media/items/{0}");
                final String vpid = videoInfo.get("id").toString();
                if (globaldupes.add(vpid)) {
                    final String title = videoInfo.get("title").toString();
                    final DownloadLink media = this.generateDownloadlink(vpid);
                    if (!StringUtils.isEmpty(embedUrl)) {
                        media.setContentUrl(embedUrl);
                    } else {
                        media.setContentUrl(br.getURL());
                    }
                    if (!StringUtils.isEmpty(description)) {
                        media.setComment(description);
                    }
                    media.setProperty(BbcCom.PROPERTY_DATE, dateFormatted);
                    media.setProperty(BbcCom.PROPERTY_TITLE, title);
                    media.setName(BbcCom.getFilename(media));
                    ret.add(media);
                }
            }
        }
        final String json2023_07 = br.getRegex("window\\.SIMORGH_DATA=(\\{.*?\\})</script>").getMatch(0);
        if (json2023_07 != null) {
            /* E.g. https://www.bbc.com/tigrinya/news-54078571 */
            final Map<String, Object> map202307 = restoreFromString(json2023_07, TypeRef.MAP);
            if (map202307 != null) {
                final Map<String, Object> media_map = (Map<String, Object>) findVideoMap202307MediaMap(map202307);
                final String description = (String) media_map.get("caption");
                final String title = media_map.get("title").toString();
                final Map<String, Object> videoInfo = (Map<String, Object>) JavaScriptEngineFactory.walkJson(media_map, "versions/{0}");
                final String vpid = videoInfo.get("versionId").toString();
                if (globaldupes.add(vpid)) {
                    final Number availableFrom = (Number) videoInfo.get("availableFrom");
                    final DownloadLink media = this.generateDownloadlink(vpid);
                    media.setContentUrl(br.getURL());
                    if (!StringUtils.isEmpty(description)) {
                        media.setComment(description);
                    }
                    if (availableFrom != null) {
                        final SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
                        media.setProperty(BbcCom.PROPERTY_DATE, df.format(new Date(availableFrom.longValue())));
                    }
                    media.setProperty(BbcCom.PROPERTY_TITLE, title);
                    media.setName(BbcCom.getFilename(media));
                    ret.add(media);
                }
            }
        }
        final String json2023_08 = br.getRegex("page: (\\{.*?\\}),\\s+").getMatch(0);
        if (json2023_08 != null) {
            /* E.g. https://www.bbc.com/historyofthebbc/100-voices/ww2/country-at-war/ */
            final Map<String, Object> map202308 = restoreFromString(json2023_08, TypeRef.MAP);
            if (map202308 != null) {
                final ArrayList<Map<String, Object>> hits = new ArrayList<Map<String, Object>>();
                findVideoMapsList202308(hits, map202308);
                logger.info("Number of detected embedded items: " + hits.size());
                for (final Map<String, Object> hit : hits) {
                    final String vpid = hit.get("video").toString();
                    if (!globaldupes.add(vpid)) {
                        continue;
                    }
                    final String description = (String) hit.get("description");
                    final String title = hit.get("name").toString();
                    final DownloadLink media = this.generateDownloadlink(vpid);
                    media.setContentUrl(br.getURL());
                    if (!StringUtils.isEmpty(description)) {
                        media.setComment(description);
                    }
                    media.setProperty(BbcCom.PROPERTY_TITLE, title);
                    media.setName(BbcCom.getFilename(media));
                    ret.add(media);
                }
            }
        }
        /* 2024-02-12: works for e.g. https://www.bbc.co.uk/archive/the-great-egg-race--eggmobiles/zbrvmfr */
        final HashSet<String> jsons_all = new HashSet<String>();
        final String[] jsons2024_02_12 = br.getRegex("Morph\\.setPayload\\('([^']+)', \\(\\{\"\\);").getColumn(0);
        jsons_all.addAll(Arrays.asList(jsons2024_02_12));
        final String[] jsons_2024_12_17 = br.getRegex("type=\"application/json\">(.*?)</script>").getColumn(0);
        jsons_all.addAll(Arrays.asList(jsons_2024_12_17));
        for (final String json : jsons_all) {
            final Object object = restoreFromString(json, TypeRef.OBJECT);
            findVideoMapsDownloadLinkList20240212(ret, object);
        }
        if (new Regex(br.getURL(), TYPE_PROGRAMMES).patternFind()) {
            if (ret.isEmpty()) {
                ret.addAll(crawlProgrammes(br.getURL()));
            }
        } else {
            if (ret.isEmpty()) {
                /* E.g. bbc.co.uk/programmes/blabla/clips --> Look for clips */
                ret.addAll(lookForProgrammesURLs(param));
            }
        }
        ret.addAll(crawl_2026_02_19());
        if (ret.isEmpty()) {
            logger.info("Failed to find any playable content --> Probably only irrelevant photo content or no content at all --> Adding offline url");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (pageTitle != null) {
            pageTitle = Encoding.htmlDecode(pageTitle).trim();
        }
        if (ret.size() == 1 && pageTitle != null) {
            /* Single item -> Page title is a better source than anything else. */
            final DownloadLink singlevideo = ret.get(0);
            singlevideo.setProperty(BbcCom.PROPERTY_TITLE, pageTitle);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (pageTitle != null) {
            fp.setName(pageTitle);
        } else {
            /* Fallback */
            fp.setName(br._getURL().toExternalForm());
        }
        fp.addLinks(ret);
        return ret;
    }

    @SuppressWarnings("unchecked")
    private void crawlLegacyJsonBlocks(final List<DownloadLink> ret, final CryptedLink param) {
        String[] jsons = this.br.getRegex("data\\-playable=\"(.*?)\">").getColumn(0);
        if (jsons != null && jsons.length != 0) {
            jsons[0] = Encoding.htmlDecode(jsons[0]);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 1 */
            jsons = this.br.getRegex("data\\-playable=\\'(.*?)\\'>").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 2 */
            jsons = this.br.getRegex("playlistObject\\s*?:\\s*?(\\{.*?\\}),[\n]+").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 3 */
            jsons = this.br.getRegex("_exposedData=(\\{.+),").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 4 OR 5 */
            jsons = this.br.getRegex("mediator\\.bind\\((\\{.*?\\}),\\s*?document\\.").getColumn(0);
            if (jsons == null || jsons.length == 0) {
                /* 2017-12-05 */
                jsons = this.br.getRegex("\\(\"tviplayer\"\\),(\\{.*?\\})\\);").getColumn(0);
            }
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 6 */
            /* 2018-11-15 */
            jsons = this.br.getRegex("window\\.mediatorDefer=page\\(document\\.getElementById\\(\"tviplayer\"\\),(\\{.*?\\}\\}\\})\\);").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 7 - Radio */
            /* 2018-11-15 */
            jsons = this.br.getRegex("window\\.__PRELOADED_STATE__ = (\\{.*?\\});").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 8 */
            /* 2018-12-07 */
            jsons = this.br.getRegex("<script id=\"initial\\-data\" type=\"text/plain\" data\\-json=\\'([^<>\"\\']+)\\'").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* Type 9 (similar to 5) */
            /* 2019-04-04 */
            jsons = this.br.getRegex("window\\.__IPLAYER_REDUX_STATE__ = (\\{.*?\\});").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /* 2021-08-05: bbc.co.uk/archive/.* */
            jsons = this.br.getRegex("(\\{\"meta\".*?\\})\\);\\s*\\}\\);</script>").getColumn(0);
        }
        if (jsons == null || jsons.length == 0) {
            /*
             * 2023-03-01: e.g.
             * https://www.bbc.co.uk/reel/video/p0dztrm5/is-separation-marriage-key-to-a-healthy-relationship-?ocid=ww.social.link.twitter
             */
            jsons = this.br.getRegex("data-json=\"([^\"]+)\"").getColumn(0);
        }
        for (String json : jsons) {
            if (json.contains("{&quot;")) {
                json = Encoding.htmlDecode(json);
            }
            final Map<String, Object> entries;
            try {
                entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(json);
            } catch (final Throwable ignore) {
                logger.info("Unsupported json (parser failure): " + json);
                continue;
            }
            final Object o_story = entries.get("story");
            final Object o_player = entries.get("player");
            final Object o_episode = entries.get("episode");
            final Object o_versions = entries.get("versions");
            final Object o_appStoreState = entries.get("appStoreState");
            final Object o_programmes = entries.get("programmes");
            final Object o_body_video = JavaScriptEngineFactory.walkJson(entries, "body/video");
            String title = null;
            String description = null;
            String tv_brand = null;
            String date = null;
            String vpid = null;
            Map<String, Object> block = entries;
            Map<String, Object> entries2 = null;
            if (o_story != null) {
                block = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "story/Content/AssetVideoIb2/{0}");
                if (block == null) {
                    logger.info("Failed to find video content");
                    break;
                }
                title = (String) block.get("Title");
                vpid = (String) block.get("Vpid");
            } else if (o_player != null && ((Map<String, Object>) o_player).containsKey("title")) {
                /* Type 4 */
                entries2 = (Map<String, Object>) o_episode;
                block = (Map<String, Object>) o_player;
                title = (String) block.get("title");
                vpid = (String) block.get("vpid");
                tv_brand = (String) block.get("masterbrand");
                date = (String) entries2.get("release_date_time");
                description = (String) JavaScriptEngineFactory.walkJson(block, "synopses/large");
            } else if (o_episode != null && o_versions != null) {
                /* Type 9 - similar to type 5 */
                block = (Map<String, Object>) o_episode;
                title = (String) block.get("title");
                vpid = (String) JavaScriptEngineFactory.walkJson(o_versions, "{0}/id");
                tv_brand = (String) JavaScriptEngineFactory.walkJson(block, "master_brand/id");
                date = (String) block.get("release_date_time");
                description = (String) JavaScriptEngineFactory.walkJson(block, "synopses/large");
            } else if (o_episode != null) {
                /* Type 5 */
                block = (Map<String, Object>) o_episode;
                title = (String) block.get("title");
                vpid = (String) JavaScriptEngineFactory.walkJson(block, "versions/{0}/id");
                tv_brand = (String) JavaScriptEngineFactory.walkJson(block, "master_brand/id");
                date = (String) block.get("release_date_time");
                description = (String) JavaScriptEngineFactory.walkJson(block, "synopses/large");
            } else if (o_appStoreState != null) {
                /* Type 6 */
                block = (Map<String, Object>) o_appStoreState;
                vpid = (String) JavaScriptEngineFactory.walkJson(block, "versions/{0}/id");
                date = (String) JavaScriptEngineFactory.walkJson(block, "versions/{0}/firstBroadcast");
                block = (Map<String, Object>) block.get("episode");
                title = (String) block.get("title");
                tv_brand = (String) JavaScriptEngineFactory.walkJson(block, "masterBrand/id");
                description = (String) JavaScriptEngineFactory.walkJson(block, "synopses/large");
            } else if (o_programmes != null) {
                /* Type 7 - Audio */
                block = (Map<String, Object>) o_programmes;
                block = (Map<String, Object>) block.get("current");
                vpid = (String) block.get("id");
                title = (String) JavaScriptEngineFactory.walkJson(block, "titles/primary");
                description = (String) JavaScriptEngineFactory.walkJson(block, "titles/secondary");
                tv_brand = (String) JavaScriptEngineFactory.walkJson(block, "network/id");
                date = (String) JavaScriptEngineFactory.walkJson(block, "availability/from");
            } else if (entries.containsKey("initData")) {
                /* Type 8 */
                block = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "initData/items/{0}/smpData/items/{0}");
                vpid = (String) block.get("versionID");
            } else if (o_body_video != null) {
                /* 2021-08-05: bbc.co.uk/archive/.* */
                block = (Map<String, Object>) o_body_video;
                vpid = (String) block.get("vpid");
                title = (String) block.get("title");
            } else {
                /* Hopefully type 1 */
                Object sourcemapo = JavaScriptEngineFactory.walkJson(entries, "settings/playlistObject");
                if (sourcemapo == null) {
                    /* Type 2 */
                    sourcemapo = JavaScriptEngineFactory.walkJson(entries, "allAvailableVersions/{0}/smpConfig");
                }
                if (sourcemapo == null) {
                    logger.info("Incompatible json: " + json);
                    continue;
                }
                block = (Map<String, Object>) sourcemapo;
                title = (String) block.get("title");
                description = (String) block.get("summary");
                block = (Map<String, Object>) JavaScriptEngineFactory.walkJson(block, "items/{0}");
                vpid = (String) block.get("vpid");
            }
            if (StringUtils.isEmpty(vpid) || !globaldupes.add(vpid)) {
                continue;
            }
            final DownloadLink media = generateDownloadlink(vpid);
            if (!StringUtils.isEmpty(title)) {
                media.setProperty(BbcCom.PROPERTY_TITLE, title);
            }
            media.setProperty(BbcCom.PROPERTY_TV_BRAND, tv_brand);
            final String date_formatted = formatDate(date);
            if (date_formatted != null) {
                media.setProperty(BbcCom.PROPERTY_DATE, date_formatted);
            }
            media.setName(BbcCom.getFilename(media));
            media.setContentUrl(param.getCryptedUrl());
            if (!StringUtils.isEmpty(description)) {
                media.setComment(description);
            }
            ret.add(media);
        }
    }

    /** Recursive function to find specific map in json */
    private Object findVideoMap(final Object o) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            final Map<String, Object> safeVideoItem = (Map<String, Object>) entrymap.get("initialItem");
            if (safeVideoItem != null) {
                return safeVideoItem;
            }
            for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    final Object hit = findVideoMap(value);
                    if (hit != null) {
                        return hit;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object pico = findVideoMap(arrayo);
                    if (pico != null) {
                        return pico;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private Object findVideoMap202307MediaMap(final Object o) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            if (entrymap.containsKey("imageUrl") && entrymap.containsKey("versions")) {
                return entrymap;
            }
            for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    final Object hit = findVideoMap202307MediaMap(value);
                    if (hit != null) {
                        return hit;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object hit = findVideoMap202307MediaMap(arrayo);
                    if (hit != null) {
                        return hit;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private void findVideoMapsList202308(final ArrayList<Map<String, Object>> hits, final Object o) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            if (entrymap.containsKey("pid") && entrymap.containsKey("video") && entrymap.containsKey("videoDuration")) {
                hits.add(entrymap);
            } else {
                for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                    final Object value = entry.getValue();
                    if (value instanceof List || value instanceof Map) {
                        findVideoMapsList202308(hits, value);
                    }
                }
            }
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    findVideoMapsList202308(hits, arrayo);
                }
            }
        }
    }

    private void findVideoMapsDownloadLinkList20240212(final List<DownloadLink> results, final Object o) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            Object duration = entrymap.get("duration");
            if (duration == null) {
                duration = JavaScriptEngineFactory.walkJson(entrymap, "versions/{0}/duration");
            }
            final String title = (String) entrymap.get("title");
            String vpid = (String) entrymap.get("vpid");
            if (vpid == null) {
                vpid = (String) JavaScriptEngineFactory.walkJson(entrymap, "versions/{0}/versionId");
            }
            if (duration != null && title != null && vpid != null && globaldupes.add(vpid)) {
                /* Hit :) */
                final DownloadLink media = this.generateDownloadlink(vpid);
                media.setContentUrl(br.getURL());
                media.setProperty(BbcCom.PROPERTY_TITLE, title);
                media.setName(BbcCom.getFilename(media));
                results.add(media);
            } else {
                for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                    final Object value = entry.getValue();
                    if (value instanceof List || value instanceof Map) {
                        findVideoMapsDownloadLinkList20240212(results, value);
                    }
                }
            }
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    findVideoMapsDownloadLinkList20240212(results, arrayo);
                }
            }
        }
    }

    private ArrayList<DownloadLink> lookForProgrammesURLs(final CryptedLink param) throws PluginException {
        if (new Regex(param.getCryptedUrl(), TYPE_PROGRAMMES).patternFind()) {
            /* Developer mistake! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* These ones will go back into this crawler */
        final String[] urls = br.getRegex("\"(https?://[^/]+/programmes/[a-z0-9]+)\"").getColumn(0);
        for (final String url : urls) {
            ret.add(this.createDownloadlink(url));
        }
        return ret;
    }

    /**
     * Crawls single 'programmes' clips. </br>
     * Typically such a link will lead to a single /iplayer/episode/... link.
     *
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    private ArrayList<DownloadLink> crawlProgrammes(final String url) throws PluginException, IOException {
        final Regex urlInfo = new Regex(url, TYPE_PROGRAMMES);
        if (!urlInfo.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contentID = urlInfo.getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean tryAPI = true;
        if (tryAPI) {
            /* Thx to: https://gist.github.com/werid/19d4197147617fbe8e7a439ff9fab885#file-bbc-py-L467 */
            try {
                final Browser brc = br.cloneBrowser();
                brc.getPage("https://www.bbc.co.uk/programmes/" + contentID + "/playlist.json");
                if (brc.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Map<String, Object> root = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Map<String, Object>> allAvailableVersions = (List<Map<String, Object>>) root.get("allAvailableVersions");
                String rootCoverImagURL = (String) root.get("holdingImage");
                if (allAvailableVersions.isEmpty() && StringUtils.isEmpty(rootCoverImagURL)) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                if (allAvailableVersions.size() > 0) {
                    for (final Map<String, Object> version : allAvailableVersions) {
                        final Map<String, Object> smpConfig = (Map<String, Object>) version.get("smpConfig");
                        final String title = smpConfig.get("title").toString();
                        final String summary = (String) smpConfig.get("summary");
                        String coverImageURL = (String) smpConfig.get("holdingImageURL");
                        final List<Map<String, Object>> items = (List<Map<String, Object>>) smpConfig.get("items");
                        for (final Map<String, Object> item : items) {
                            final String kind = item.get("kind").toString();
                            if (!kind.matches("programme|radioProgramme")) {
                                logger.info("Skipping invalid kind: " + kind);
                                continue;
                            }
                            final String vpid = item.get("vpid").toString();
                            if (!globaldupes.add(vpid)) {
                                continue;
                            }
                            final DownloadLink media = this.generateDownloadlink(vpid);
                            media.setProperty(BbcCom.PROPERTY_TITLE, title);
                            media.setProperty(BbcCom.PROPERTY_TV_BRAND, smpConfig.get("masterBrandName"));
                            if (!StringUtils.isEmpty(summary)) {
                                media.setComment(summary);
                            }
                            ret.add(media);
                        }
                        if (!StringUtils.isEmpty(coverImageURL)) {
                            coverImageURL = brc.getURL(coverImageURL).toExternalForm();
                            coverImageURL = coverImageURL.replace("$recipe", "976x549");
                            final DownloadLink cover = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(coverImageURL));
                            cover.setAvailable(true);
                            ret.add(cover);
                        }
                    }
                } else {
                    /* Content not available anymore but cover still available. */
                    rootCoverImagURL = brc.getURL(rootCoverImagURL).toExternalForm();
                    rootCoverImagURL = rootCoverImagURL.replace("$recipe", "976x549");
                    final DownloadLink cover = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(rootCoverImagURL));
                    cover.setAvailable(true);
                    ret.add(cover);
                }
                return ret;
            } catch (final Exception e) {
                logger.log(e);
                logger.info("json crawler failed due to exception -> Returning website crawler results");
            }
        }
        /* Check if specific episode/expected content is offline */
        if (br.containsHTML(">\\s*Sorry, this episode is not currently available")) {
            /* Episode offline. Only cover may be downloadable. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String specificIplayerEpisode = br.getRegex("(/iplayer/episode/" + Pattern.quote(contentID) + ")").getMatch(0);
        if (specificIplayerEpisode != null) {
            ret.add(this.createDownloadlink(br.getURL(specificIplayerEpisode).toString()));
            return ret;
        } else {
            logger.info("Failed to find specific episode -> Possible crawler failure -> Jumping into old handling/fallback");
        }
        final String[] programmeJsons = br.getRegex("(\\{\"container\":\"#playout-" + contentID + ".*?\\})\\);").getColumn(0);
        final String date = br.getRegex("class=\"details__streamablefrom\" datetime=\"(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        if (programmeJsons.length > 0) {
            String playlistTitle = null;
            for (final String programmeJson : programmeJsons) {
                final Map<String, Object> root = restoreFromString(programmeJson, TypeRef.MAP);
                final Map<String, Object> smpSettings = (Map<String, Object>) root.get("smpSettings");
                final Map<String, Object> playlistObject = (Map<String, Object>) smpSettings.get("playlistObject");
                playlistTitle = (String) playlistObject.get("title");
                final List<Map<String, Object>> videos = (List<Map<String, Object>>) playlistObject.get("items");
                for (final Map<String, Object> video : videos) {
                    final String vpid = (String) video.get("vpid");
                    if (StringUtils.isEmpty(vpid) || !globaldupes.add(vpid)) {
                        continue;
                    }
                    final DownloadLink media = this.generateDownloadlink(vpid);
                    ret.add(media);
                }
            }
            if (ret.size() == 1) {
                /* We got only 1 result --> Set metadata on it */
                for (final DownloadLink media : ret) {
                    if (date != null) {
                        media.setProperty(BbcCom.PROPERTY_DATE, date);
                    }
                    if (playlistTitle != null) {
                        media.setProperty(BbcCom.PROPERTY_TITLE, playlistTitle);
                    }
                }
            }
        }
        if (ret.isEmpty()) {
            /* Old fallback from 2017 */
            final String[] videoIDs = this.br.getRegex("episode_id=([pbm][a-z0-9]{7})").getColumn(0);
            for (final String vpid : videoIDs) {
                ret.add(createDownloadlink(br.getURL("/iplayer/episode/" + vpid).toString()));
            }
        }
        if (ret.isEmpty()) {
            /* Final fallback */
            final String[] episodeURLs = this.br.getRegex("(/iplayer/episode/[a-z0-9]+)").getColumn(0);
            for (final String episodeURL : episodeURLs) {
                ret.add(createDownloadlink(br.getURL(episodeURL).toString()));
            }
        }
        return ret;
    }

    /**
     * Crawls URLs like: https://www.bbc.com/news/av-embeds/60608706 </br>
     * Be sure to access them in beforehand using the global Browser instance.
     */
    private ArrayList<DownloadLink> crawlEmbed(final CryptedLink param) throws PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String title = br.getRegex("title:\"([^\"]+)\"").getMatch(0);
        final String vpid = br.getRegex("versionID:\\s*\"([a-z0-9]+)\"").getMatch(0);
        if (vpid != null && globaldupes.add(vpid)) {
            final DownloadLink media = this.generateDownloadlink(vpid);
            media.setContentUrl(br.getURL());
            if (title != null) {
                media.setProperty(BbcCom.PROPERTY_TITLE, Encoding.htmlDecode(title).trim());
            }
            media.setName(BbcCom.getFilename(media));
            ret.add(media);
        }
        return ret;
    }

    /*
     * 2026-02-19 e.g. /sounds/play/m002qqsb
     */
    private ArrayList<DownloadLink> crawl_2026_02_19() throws PluginException, IOException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String json = br.getRegex("id=\"__NEXT_DATA__\" type=\"application/json\">(\\{.*?\\})</script>").getMatch(0);
        if (json == null) {
            return ret;
        }
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        crawl_2026_02_19_recursive(ret, entries);
        return ret;
    }

    @SuppressWarnings("unchecked")
    private void crawl_2026_02_19_recursive(final List<DownloadLink> results, final Object o) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            final Object type = entrymap.get("type");
            if (type instanceof String && ((String) type).equalsIgnoreCase("playable_item")) {
                /* Hit :) */
                final String vpid = entrymap.get("id").toString();
                if (!globaldupes.add(vpid)) {
                    logger.info("Avoiding duplicate: " + vpid);
                    return;
                }
                final String description = JavaScriptEngineFactory.walkJson(entrymap, "synopses/long").toString();
                final String title = JavaScriptEngineFactory.walkJson(entrymap, "titles/entity_title").toString();
                final String releasedate = (String) JavaScriptEngineFactory.walkJson(entrymap, "release/date");
                final DownloadLink media = this.generateDownloadlink(vpid);
                media.setContentUrl(br.getURL());
                media.setProperty(BbcCom.PROPERTY_TITLE, title);
                if (releasedate != null) {
                    final String date = new Regex(releasedate, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
                    if (date != null) {
                        media.setProperty(BbcCom.PROPERTY_DATE, date);
                    } else {
                        logger.info("Unparseable date format: " + releasedate);
                    }
                }
                media.setName(BbcCom.getFilename(media));
                if (!StringUtils.isEmpty(description)) {
                    media.setComment(description);
                }
                final Map<String, Object> quality_variants = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entrymap, "download/quality_variants");
                if (quality_variants != null) {
                    /* Find estimated file size */
                    long max_size = -1;
                    for (final Object quality_o : quality_variants.values()) {
                        final Map<String, Object> quality_info = (Map<String, Object>) quality_o;
                        final long size = ((Number) quality_info.get("file_size")).longValue();
                        if (size > max_size) {
                            max_size = size;
                        }
                    }
                    if (max_size != -1) {
                        media.setDownloadSize(max_size);
                    }
                }
                results.add(media);
            } else {
                for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                    final Object value = entry.getValue();
                    if (value instanceof List || value instanceof Map) {
                        crawl_2026_02_19_recursive(results, value);
                    }
                }
            }
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    crawl_2026_02_19_recursive(results, arrayo);
                }
            }
        }
    }

    private DownloadLink generateDownloadlink(final String vpid) {
        final DownloadLink media = createDownloadlink(generateInternalVideoURL(vpid));
        media.setName(vpid + ".mp4");
        return media;
    }

    public static String generateInternalVideoURL(final String videoid) {
        return "https://" + BbcComiPlayerCrawler.getPluginDomains().get(0)[0] + "/video/" + videoid;
    }

    public static String formatDate(final String input) {
        if (input == null) {
            return null;
        }
        String dateformat = null;
        if (input.matches("\\d{4}\\-\\d{2}\\-\\d{2}")) {
            dateformat = "yyyy-MM-dd";
        } else if (input.matches("\\d{1,2}/\\d{1,2}/\\d{4}")) {
            dateformat = "dd/MM/yyyy";
        } else if (input.matches("\\d{1,2} [A-Z][a-z]+ \\d{4}")) {
            dateformat = "dd MMM yyyy";
        }
        if (dateformat == null) {
            return input;
        }
        final long date = TimeFormatter.getMilliSeconds(input, dateformat, Locale.ENGLISH);
        String formattedDate = null;
        final String targetFormat = "yyyy-MM-dd";
        Date theDate = new Date(date);
        try {
            final SimpleDateFormat formatter = new SimpleDateFormat(targetFormat);
            formattedDate = formatter.format(theDate);
        } catch (Exception e) {
            /* prevent input error killing plugin */
            formattedDate = input;
        }
        return formattedDate;
    }
}
