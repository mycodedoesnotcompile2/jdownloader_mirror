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

import java.net.MalformedURLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.LinkCrawlerThread;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.parser.html.HTMLSearch;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.FaceBookComVideos;

@SuppressWarnings("deprecation")
@DecrypterPlugin(revision = "$Revision: 50442 $", interfaceVersion = 3, names = {}, urls = {})
public class FaceBookComGallery extends PluginForDecrypt {
    public FaceBookComGallery(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        prepBR(br);
        return br;
    }

    public static void prepBR(final Browser br) {
        br.getHeaders().put("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8");
        br.getHeaders().put("Accept-Language", "en-gb, en;q=0.9");
        br.getHeaders().put("Accept-Encoding", "gzip, deflate");
        br.getHeaders().put("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7");
        br.setCookie(getAnnotationNames()[0], "locale", "en_GB");
        br.setFollowRedirects(true);
    }

    public static enum URL_TYPE {
        VIDEO_GENERIC,
        PHOTO,
        PHOTO_OR_PHOTO_ALBUM;
    }

    private String getVideoidFromURL(final String url) throws MalformedURLException {
        final UrlQuery query = UrlQuery.parse(url);
        String videoid = query.get("v");
        if (videoid == null) {
            videoid = query.get("video_id");
        }
        if (videoid != null && videoid.matches("\\d+")) {
            return videoid;
        }
        videoid = new Regex(url, "(?i)/videos/(\\d+)").getMatch(0);
        if (videoid == null) {
            videoid = new Regex(url, "(?i)/reel/(\\d+)").getMatch(0);
        }
        return videoid;
    }

    private String getPhotoidFromURL(final String url) throws MalformedURLException {
        String photoid = new Regex(url, "(?i)/photos/[^/]+/(\\d+)").getMatch(0);
        if (photoid == null) {
            /* Single photo as part of photo album */
            photoid = new Regex(url, "(?i)/photo/.+fbid=(\\d+)").getMatch(0);
        }
        return photoid;
    }

    /**
     * Returns type of URL. </br>
     * Important: This is just a hint. URLs which look like a photo can also lead to video content!
     */
    private URL_TYPE getUrlType(final String url) throws MalformedURLException {
        if (url == null) {
            return null;
        } else if (!this.canHandle(url)) {
            /*
             * Important check! Link cannot be handled by this plugin. Links from other websites could look like Facebook links e.g.
             * https://www.youtube.com/watch?v=XXXXXXyyyyyy
             */
            return null;
        }
        if (getVideoidFromURL(url) != null) {
            return URL_TYPE.VIDEO_GENERIC;
        } else if (getPhotoidFromURL(url) != null) {
            return URL_TYPE.PHOTO;
        }
        // if (url.matches("(?i)https?://[^/]+/watch/\\?v=\\d+")) {
        // return URL_TYPE.VIDEO_1_current;
        // } else if (url.matches("(?i)https?://[^/]+/video/video\\.php\\?v=\\d+")) {
        // return URL_TYPE.VIDEO_2;
        // } else if (url.matches("(?i)https?://[^/]+/video/embed\\?video_id=\\d+")) {
        // return URL_TYPE.VIDEO_2;
        // }
        return null;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 2;
    }

    public static String[] getAnnotationNames() {
        return new String[] { "facebook.com" };
    }

    public static String[] getAnnotationUrls() {
        return new String[] { "https?://(?:(m|www)\\.)?facebook\\.com/.+" };
    }

    private final Pattern PATTERN_FBSHORTLINK             = Pattern.compile("https?://(?:www\\.)?on\\.fb\\.me/[A-Za-z0-9]+\\+?", Pattern.CASE_INSENSITIVE);
    private final Pattern TYPE_FB_REDIRECT_TO_EXTERN_SITE = Pattern.compile("https?://l\\.facebook\\.com/(?:l/[^/]+/.+|l\\.php\\?u=.+)", Pattern.CASE_INSENSITIVE);
    // private final String TYPE_SINGLE_PHOTO = "https?://(?:www\\.)?facebook\\.com/photo\\.php\\?fbid=\\d+.*?";
    // private final String TYPE_SET_LINK_PHOTO = "(?i)https?://[^/]+/(media/set/\\?set=|media_set\\?set=)o?a[0-9\\.]+(&type=\\d+)?";
    // private final String TYPE_SET_LINK_VIDEO = "(?i)https?://[^/]+/(media/set/\\?set=|media_set\\?set=)vb\\.\\d+.*?";
    // private final String TYPE_PHOTOS_ALBUMS_LINK = "(?i)https?://[^/]+/.+photos_albums";
    // private final String TYPE_PHOTOS_OF_LINK = "(?i)https?://[^/]+/[A-Za-z0-9\\.]+/photos_of.*";
    // private final String TYPE_PHOTOS_ALL_LINK = "(?i)https?://[^/]+/[A-Za-z0-9\\.]+/photos_all.*";
    // private final String TYPE_PHOTOS_STREAM_LINK = "(?i)https?://[^/]+/[^/]+/photos_stream.*";
    // private final String TYPE_PHOTOS_STREAM_LINK_2 = "https?://(?:www\\.)?facebook\\.com/pages/[^/]+/\\d+\\?sk=photos_stream&tab=.*";
    // private final String TYPE_PHOTOS_LINK_2 = "https?://(?:www\\.)?facebook\\.com/pg/" + COMPONENT_USERNAME + "/photos.*";
    // private final String TYPE_GROUPS_PHOTOS = "https?://(?:www\\.)?facebook\\.com/groups/\\d+/photos/";
    // private final String TYPE_GROUPS_FILES = "https?://(?:www\\.)?facebook\\.com/groups/\\d+/files/";
    // private final String TYPE_PROFILE_PHOTOS =
    // "^https?://(?:www\\.)?facebook\\.com/profile\\.php\\?id=\\d+&sk=photos&collection_token=\\d+(?:%3A|:)\\d+(?:%3A|:)5$";
    // private final String TYPE_PROFILE_ALBUMS =
    // "^https?://(?:www\\.)?facebook\\.com/profile\\.php\\?id=\\d+&sk=photos&collection_token=\\d+(?:%3A|:)\\d+(?:%3A|:)6$";
    // private final String TYPE_NOTES = "(?i)https?://[^/]+/(notes/|note\\.php\\?note_id=).+";
    // private final String TYPE_MESSAGE = "(?i)https?://[^/]+/messages/.+";
    private boolean       debug                           = false;

    @Deprecated
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        skippedLivestreams = 0;
        if (debug && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* For debugging as facebook websites and resulting log outputs are huge */
            disableLogger();
        }
        if (new Regex(param.getCryptedUrl(), PATTERN_FBSHORTLINK).patternFind()) {
            return handleRedirectToExternalSite(param.getCryptedUrl());
        } else {
            final Account account = AccountController.getInstance().getValidAccount(this.getHost());
            return crawl(param, account);
        }
    }

    FaceBookComVideos hosterplugin = null;

    public ArrayList<DownloadLink> crawl(final CryptedLink param, final Account account) throws Exception {
        hosterplugin = (FaceBookComVideos) this.getNewPluginForHostInstance(this.getHost());
        if (account != null) {
            hosterplugin.login(account, false);
        }
        /* Do some minor corrections of added link. */
        /* Remove m.facebook.com as our crawler can't cope with those (old?) facebook website versions for mobile devices. */
        String addedurl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://").replace("https://m.", "https://www.");
        final String videoIDFRomEmbedURL = new Regex(addedurl, "(?i)https?://[^/]+/video/embed\\?video_id=(\\d+)").getMatch(0);
        // final String mobileSubdomain = new Regex(url, "(?i)https?:/(m\\.[^/]+)/.+").getMatch(0);
        if (videoIDFRomEmbedURL != null) {
            /* Small workaround for embedded videourls */
            addedurl = "https://www." + this.getHost() + "/watch/?v=" + videoIDFRomEmbedURL;
        }
        // if (mobileSubdomain != null) {
        // /* Remove mobile subdomain */
        // url = url.replaceFirst(Pattern.quote(mobileSubdomain), this.getHost());
        // }
        br.getPage(addedurl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Different sources to parse their json. */
        final List<String> jsonRegExes = new ArrayList<String>();
        /* 2021-03-19: E.g. when user is loggedIN */
        jsonRegExes.add(Pattern.quote("(new ServerJS()).handleWithCustomApplyEach(ScheduledApplyEach,") + "(\\{.*?\\})" + Pattern.quote(");});});</script>"));
        /* Same as previous RegEx but lazier. */
        jsonRegExes.add(Pattern.quote("(new ServerJS()).handleWithCustomApplyEach(ScheduledApplyEach,") + "(\\{.*?\\})" + Pattern.quote(");"));
        /* 2022-08-01: Lazier attempt: On RegEx which is simply supposed to find all jsons on the current page. */
        jsonRegExes.add("<script type=\"application/json\"[^>]*>(\\{.*?)</script>");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        // final ArrayList<DownloadLink> videoPermalinks = new ArrayList<DownloadLink>();
        final HashSet<String> processedJsonStrings = new HashSet<String>();
        final List<Object> parsedJsons = new ArrayList<Object>();
        for (final String jsonRegEx : jsonRegExes) {
            final String[] jsons = br.getRegex(jsonRegEx).getColumn(0);
            for (final String json : jsons) {
                /* Do not process/parse same json multiple times. */
                if (!processedJsonStrings.add(json)) {
                    continue;
                }
                try {
                    /* 2021-03-23: Use JavaScriptEngineFactory as they can also have json without quotes around the keys. */
                    // final Object jsonO = restoreFromString(json, TypeRef.OBJECT);
                    final Object jsonO = JavaScriptEngineFactory.jsonToJavaMap(json);
                    parsedJsons.add(jsonO);
                    final ArrayList<DownloadLink> videos = new ArrayList<DownloadLink>();
                    this.crawlVideos(jsonO, videos);
                    ret.addAll(videos);
                    final ArrayList<DownloadLink> photos = new ArrayList<DownloadLink>();
                    this.crawlPhotos(jsonO, photos);
                    ret.addAll(photos);
                } catch (final Throwable ignore) {
                    /* Do not log to avoid log spam */
                    // logger.log(ignore);
                }
            }
        }
        if (processedJsonStrings.isEmpty()) {
            logger.warning("Failed to find any jsons to process");
        }
        if (ret.isEmpty() && this.skippedLivestreams > 0) {
            logger.info("Livestreams are not supported");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (processedJsonStrings.isEmpty()) {
            logger.info("Failed to find any jsons --> Probably unsupported URL");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (ret.isEmpty()) {
            logger.info("Found nothing -> Going through fallback handling");
            /*
             * It is really hard to find out why specific Facebook content is offline (permission issue or offline content) so this is a
             * last ditch effort.
             */
            /* Look for any errors. */
            for (final Object parsedJson : parsedJsons) {
                final Map<String, Object> videoErrormap = (Map<String, Object>) websiteFindVideoErrorMap(parsedJson, null);
                if (videoErrormap != null) {
                    logger.info("Offline reason: " + videoErrormap.get("title") + " | Offline Map: " + videoErrormap);
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
            /* No errors found -> Last resort: Look for any URLs which look like links to other single images or videos. */
            final String[] allurls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            final HashSet<String> dupes = new HashSet<String>();
            for (final String thisurl : allurls) {
                String contentID = getVideoidFromURL(thisurl);
                if (contentID == null) {
                    contentID = getPhotoidFromURL(thisurl);
                }
                if (contentID == null) {
                    /* Unsupported URL */
                    continue;
                } else if (!dupes.add(contentID)) {
                    /* Skip duplicates */
                    continue;
                } else if (thisurl.equals(br.getURL())) {
                    continue;
                } else if (br.getURL().contains(contentID)) {
                    /* Same ID inside URL which we are currently processing -> Skip */
                    continue;
                } else if (addedurl.contains(contentID)) {
                    /* Same ID inside URL which we are currently processing -> Skip */
                    continue;
                } else if (!this.canHandle(thisurl)) {
                    /* Skip links which this plugin cannot handle */
                    continue;
                }
                logger.info("Adding last resort URL: " + thisurl);
                ret.add(this.createDownloadlink(thisurl));
            }
            if (ret.isEmpty()) {
                logger.warning("Unsupported link or broken plugin -> Returning empty array");
            }
            return ret;
        }
        final Map<String, List<DownloadLink>> contentIDPackages = new HashMap<String, List<DownloadLink>>();
        final Map<String, List<DownloadLink>> videoPackages = new HashMap<String, List<DownloadLink>>();
        for (final DownloadLink result : ret) {
            final String contentID = result.getStringProperty(FaceBookComVideos.PROPERTY_CONTENT_ID);
            if (contentID == null) {
                continue;
            }
            if (contentIDPackages.containsKey(contentID)) {
                contentIDPackages.get(contentID).add(result);
            } else {
                final List<DownloadLink> contentIDPackage = new ArrayList<DownloadLink>();
                contentIDPackage.add(result);
                contentIDPackages.put(contentID, contentIDPackage);
            }
        }
        /**
         * Check if user only wanted to have a single video crawled. </br>
         * If so, filter out all other stuff we might have picked up.
         */
        String contentIDOfSingleDesiredVideo = null;
        String contentIDOfSingleDesiredPhoto = null;
        DownloadLink singleDesiredPhoto = null;
        for (final DownloadLink result : ret) {
            final String contentID = result.getStringProperty(FaceBookComVideos.PROPERTY_CONTENT_ID);
            if (contentID == null) {
                continue;
            }
            if (FaceBookComVideos.isVideo(result)) {
                videoPackages.put(contentID, contentIDPackages.get(contentID));
                if (addedurl.contains(contentID) || br.getURL().contains(contentID)) {
                    contentIDOfSingleDesiredVideo = contentID;
                }
            } else if (FaceBookComVideos.isPhoto(result)) {
                if (addedurl.contains(contentID) || br.getURL().contains(contentID)) {
                    contentIDOfSingleDesiredPhoto = contentID;
                    singleDesiredPhoto = result;
                }
            }
        }
        String titleHTML = HTMLSearch.searchMetaTag(br, "og:title");
        if (titleHTML != null) {
            titleHTML = Encoding.htmlDecode(titleHTML);
            titleHTML = titleHTML.replaceFirst("(?i)( \\| By.+)", "");
            titleHTML = titleHTML.replaceAll("\\.\\.\\.$", "");
            titleHTML = titleHTML.trim();
        }
        String slugOfVideoTitle = br.getRegex("/videos/([^/]+)/" + contentIDOfSingleDesiredVideo).getMatch(0);
        if (slugOfVideoTitle != null) {
            slugOfVideoTitle = Encoding.htmlDecode(slugOfVideoTitle).trim();
        }
        for (final Entry<String, List<DownloadLink>> entry : videoPackages.entrySet()) {
            // final String videoID = entry.getKey();
            final List<DownloadLink> results = entry.getValue();
            final Map<String, Object> videoExtraInfoMap = this.findVideoExtraInfoMap(parsedJsons, contentIDOfSingleDesiredVideo);
            final Map<String, Object> videoExtraInfoMap2 = this.findVideoExtraInfoMap2(parsedJsons, contentIDOfSingleDesiredVideo);
            addExtraInfo1: if (titleHTML != null || videoExtraInfoMap != null) {
                boolean updatedMetadata = false;
                String videoTitleFromExtraMap = null;
                String uploaderNameFromMap = null;
                if (videoExtraInfoMap != null) {
                    final Map<String, Object> titlemap = (Map<String, Object>) videoExtraInfoMap.get("title");
                    videoTitleFromExtraMap = (String) titlemap.get("text");
                    if (videoTitleFromExtraMap != null) {
                        videoTitleFromExtraMap = videoTitleFromExtraMap.trim();
                    }
                    final Map<String, Object> ownermap = (Map<String, Object>) videoExtraInfoMap.get("owner");
                    if (ownermap != null) {
                        uploaderNameFromMap = (String) ownermap.get("name");
                        if (uploaderNameFromMap != null) {
                            uploaderNameFromMap = uploaderNameFromMap.trim();
                        }
                    }
                }
                for (final DownloadLink result : results) {
                    final String preFetchedTitle = result.getStringProperty(FaceBookComVideos.PROPERTY_TITLE);
                    if (preFetchedTitle == null) {
                        if (!StringUtils.isEmpty(videoTitleFromExtraMap)) {
                            result.setProperty(FaceBookComVideos.PROPERTY_TITLE, videoTitleFromExtraMap);
                        } else {
                            result.setProperty(FaceBookComVideos.PROPERTY_TITLE, titleHTML);
                        }
                        updatedMetadata = true;
                    }
                    final String preFetchedUploaderName = result.getStringProperty(FaceBookComVideos.PROPERTY_UPLOADER);
                    if (preFetchedUploaderName == null && !StringUtils.isEmpty(uploaderNameFromMap)) {
                        result.setProperty(FaceBookComVideos.PROPERTY_UPLOADER, uploaderNameFromMap);
                        updatedMetadata = true;
                    }
                }
                if (updatedMetadata) {
                    logger.info("Successfully updated video metadata");
                }
            }
            addExtraInfo2: if (true) {
                if (videoExtraInfoMap2 == null) {
                    logger.warning("videoExtraInfoMap2: invalid result");
                    break addExtraInfo2;
                }
                final String trackingJson = (String) videoExtraInfoMap2.get("tracking");
                if (trackingJson != null) {
                    final Map<String, Object> entries = restoreFromString(trackingJson, TypeRef.MAP);
                    final Number publish_time = (Number) JavaScriptEngineFactory.walkJson(entries, "page_insights/{0}/post_context/publish_time");
                    if (publish_time == null) {
                        logger.warning("videoExtraInfoMap2: Failed to find publish_time");
                        break addExtraInfo2;
                    }
                    final String publishDateFormatted = getFormattedPublishTime(publish_time.longValue());
                    for (final DownloadLink result : results) {
                        result.setProperty(FaceBookComVideos.PROPERTY_DATE_FORMATTED, publishDateFormatted);
                    }
                }
            }
            if (videoPackages.size() == 1 && titleHTML != null) {
                /* Only one video item -> We can set the title found in html as video title if a better title hasn't already been found. */
                for (final DownloadLink result : results) {
                    if (result.hasProperty(FaceBookComVideos.PROPERTY_TITLE)) {
                        continue;
                    }
                    result.setProperty(FaceBookComVideos.PROPERTY_TITLE, titleHTML);
                }
            }
        }
        if (contentIDOfSingleDesiredVideo != null) {
            logger.info("Returning only results for content: " + contentIDOfSingleDesiredVideo);
            final List<DownloadLink> resultsForOneDesiredVideo = videoPackages.get(contentIDOfSingleDesiredVideo);
            ret.clear();
            ret.addAll(resultsForOneDesiredVideo);
            /* Do not return here since filenames will be set down below! */
        }
        if (singleDesiredPhoto != null) {
            /* User wants single photo -> Try to find more metadata e.g. name of the uploader */
            final Map<String, Object> photoExtraInfoMap = this.findPhotoExtraInfoMap(parsedJsons, contentIDOfSingleDesiredPhoto);
            if (photoExtraInfoMap != null) {
                final Number created_time = (Number) photoExtraInfoMap.get("created_time");
                if (created_time != null) {
                    final SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
                    singleDesiredPhoto.setProperty(FaceBookComVideos.PROPERTY_DATE_FORMATTED, df.format(new Date(created_time.longValue() * 1000)));
                }
                final Map<String, Object> ownermap = (Map<String, Object>) photoExtraInfoMap.get("owner");
                String uploaderNameFromMap = (String) ownermap.get("name");
                if (uploaderNameFromMap != null) {
                    uploaderNameFromMap = uploaderNameFromMap.trim();
                    singleDesiredPhoto.setProperty(FaceBookComVideos.PROPERTY_UPLOADER, uploaderNameFromMap);
                    logger.info("Found extra information for photo: " + contentIDOfSingleDesiredPhoto);
                } else {
                    logger.warning("Found wrong ownermap");
                }
            } else {
                logger.info("Failed to find any extra information for photo: " + contentIDOfSingleDesiredPhoto);
            }
        }
        /**
         * Set filenames and package names. Put each video into a different package to be able to group video + thumbnail or other items
         * which belong to one video.
         */
        final Map<String, FilePackage> packages = new HashMap<String, FilePackage>();
        for (final DownloadLink result : ret) {
            FaceBookComVideos.setFilename(result);
            final String videoID = result.getStringProperty(FaceBookComVideos.PROPERTY_CONTENT_ID);
            if (videoID == null) {
                /* Other/external item */
                continue;
            }
            final String description = result.getStringProperty(FaceBookComVideos.PROPERTY_DESCRIPTION);
            FilePackage fp = packages.get(videoID);
            if (fp == null) {
                fp = FilePackage.getInstance();
                final String title = result.getStringProperty(FaceBookComVideos.PROPERTY_TITLE);
                final String uploaderNameForPackage = FaceBookComVideos.getUploaderNameAny(result);
                if (uploaderNameForPackage != null && title != null) {
                    fp.setName(uploaderNameForPackage + " - " + title + " - " + videoID);
                } else if (uploaderNameForPackage != null) {
                    fp.setName(uploaderNameForPackage + " - " + videoID);
                } else if (title != null) {
                    fp.setName(title + " - " + videoID);
                } else {
                    fp.setName(videoID);
                }
                if (!StringUtils.isEmpty(description)) {
                    fp.setComment(description);
                }
                packages.put(videoID, fp);
            }
            result._setFilePackage(fp);
            result.setContainerUrl(br.getURL());
        }
        return ret;
    }

    private Object websiteFindVideoErrorMap(final Object o, final String videoid) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                final String key = entry.getKey();
                final Object value = entry.getValue();
                if (key.equals("rootView") && value instanceof Map && entrymap.containsKey("tracePolicy")) {
                    final String tracePolicy = (String) entrymap.get("tracePolicy");
                    final String videoidTmp = (String) JavaScriptEngineFactory.walkJson(entrymap, "params/video_id");
                    if ((StringUtils.equalsIgnoreCase(tracePolicy, "comet.error") || StringUtils.equalsIgnoreCase(tracePolicy, "comet.watch.video.not.found")) && (videoid == null || StringUtils.equals(videoidTmp, videoid))) {
                        return o;
                    }
                } else if (value instanceof List || value instanceof Map) {
                    final Object pico = websiteFindVideoErrorMap(value, videoid);
                    if (pico != null) {
                        return pico;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object ret = websiteFindVideoErrorMap(arrayo, videoid);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private ArrayList<DownloadLink> handleRedirectToExternalSite(final String url) throws DecrypterException, PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String external_url = new Regex(url, "/l\\.php\\?u=([^&]+)").getMatch(0);
        if (StringUtils.isNotEmpty(external_url)) {
            external_url = Encoding.urlDecode(external_url, false);
            ret.add(this.createDownloadlink(external_url));
            return ret;
        }
        external_url = new Regex(url, "facebook\\.com/l/[^/]+/(.+)").getMatch(0);
        if (StringUtils.isNotEmpty(external_url)) {
            ret.add(this.createDownloadlink("https://" + external_url));
            return ret;
        }
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private int skippedLivestreams = 0;

    private void crawlVideos(final Object o, final List<DownloadLink> results) throws PluginException {
        if (o instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) o;
            final Map<String, Object> videoDeliveryLegacyFields = (Map<String, Object>) map.get("videoDeliveryLegacyFields");
            final List<Map<String, Object>> progressive_urls = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(map, "videoDeliveryResponseFragment/videoDeliveryResponseResult/progressive_urls");
            // final Map<String, Object> owner = (Map<String, Object>) map.get("owner");
            isVideo: if (videoDeliveryLegacyFields != null || progressive_urls != null) {
                final String id = map.get("id").toString();
                final Boolean is_live_streaming = (Boolean) map.get("is_live_streaming");
                if (Boolean.TRUE.equals(is_live_streaming)) {
                    /* Livestreams are not supported */
                    logger.info("Skipping livestream: " + id);
                    skippedLivestreams++;
                    break isVideo;
                }
                final Map<String, Object> video_owner = (Map<String, Object>) map.get("video_owner");
                final String videoContentURL = (String) map.get("permalink_url");
                final String thumbnailDirectURL = (String) JavaScriptEngineFactory.walkJson(map, "preferred_thumbnail/image/uri");
                final DownloadLink video = new DownloadLink(this.hosterplugin, this.getHost(), videoContentURL, true);
                final Object playable_duration_in_ms = map.get("playable_duration_in_ms");
                final Object length_in_second = map.get("length_in_second");
                if (playable_duration_in_ms instanceof Number) {
                    video.setProperty(FaceBookComVideos.PROPERTY_RUNTIME_MILLISECONDS, ((Number) playable_duration_in_ms).longValue());
                } else if (length_in_second != null) {
                    /* New 2024-10-28 */
                    video.setProperty(FaceBookComVideos.PROPERTY_RUNTIME_MILLISECONDS, ((Number) length_in_second).longValue() * 1000);
                }
                final String title = (String) map.get("name");
                final String uploader = video_owner != null ? (String) video_owner.get("name") : null;
                String publishDateFormatted = null;
                final Object publish_timeO = map.get("publish_time");
                if (publish_timeO instanceof Number) {
                    publishDateFormatted = getFormattedPublishTime(((Number) publish_timeO).longValue());
                }
                final String description = (String) JavaScriptEngineFactory.walkJson(map, "savable_description/text");
                String uploaderNameFromURL = FaceBookComVideos.getUploaderNameFromVideoURL(videoContentURL);
                if (uploaderNameFromURL == null && video_owner != null) {
                    final String url = (String) video_owner.get("url");
                    if (url != null) {
                        uploaderNameFromURL = new Regex(url, "https?://[^/]+/([^/]+)$").getMatch(0);
                    }
                }
                String urlLow = null;
                String urlHigh = null;
                if (videoDeliveryLegacyFields != null) {
                    urlLow = (String) videoDeliveryLegacyFields.get("playable_url");
                    if (StringUtils.isEmpty(urlLow)) {
                        /* 2023-07-13 */
                        urlLow = (String) videoDeliveryLegacyFields.get("browser_native_sd_url");
                    }
                    urlHigh = (String) videoDeliveryLegacyFields.get("playable_url_quality_hd");
                    if (StringUtils.isEmpty(urlHigh)) {
                        /* 2023-07-13 */
                        urlHigh = (String) videoDeliveryLegacyFields.get("browser_native_hd_url");
                    }
                }
                if (progressive_urls != null) {
                    /* 2024-11-11: For users who are logged in */
                    for (final Map<String, Object> progressive_url_map : progressive_urls) {
                        final String progressive_url = progressive_url_map.get("progressive_url").toString();
                        final Map<String, Object> metadata = (Map<String, Object>) progressive_url_map.get("metadata");
                        final String quality = metadata.get("quality").toString();
                        if (quality.equalsIgnoreCase("HD")) {
                            urlHigh = progressive_url;
                        } else {
                            urlLow = progressive_url;
                        }
                    }
                }
                if (urlHigh == null && urlLow == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (!StringUtils.isEmpty(urlHigh)) {
                    video.setProperty(FaceBookComVideos.PROPERTY_DIRECTURL_HD, urlHigh);
                }
                if (!StringUtils.isEmpty(urlLow)) {
                    video.setProperty(FaceBookComVideos.PROPERTY_DIRECTURL_LOW, urlLow);
                }
                video.setProperty(FaceBookComVideos.PROPERTY_TYPE, FaceBookComVideos.TYPE_VIDEO);
                final ArrayList<DownloadLink> thisResults = new ArrayList<DownloadLink>();
                thisResults.add(video);
                if (!StringUtils.isEmpty(thumbnailDirectURL)) {
                    /* Not all videos have thumbnails! Alternatively we could check for field "has_preview_thumbnails". */
                    final DownloadLink thumbnail = new DownloadLink(this.hosterplugin, this.getHost(), thumbnailDirectURL, true);
                    thumbnail.setProperty(FaceBookComVideos.PROPERTY_TYPE, FaceBookComVideos.TYPE_THUMBNAIL);
                    thumbnail.setProperty(FaceBookComVideos.PROPERTY_DIRECTURL_LAST, thumbnailDirectURL);
                    thisResults.add(thumbnail);
                }
                /* Add properties to result */
                for (final DownloadLink thisResult : thisResults) {
                    thisResult.setProperty(FaceBookComVideos.PROPERTY_CONTENT_ID, id);
                    if (uploaderNameFromURL != null) {
                        thisResult.setProperty(FaceBookComVideos.PROPERTY_UPLOADER_URL, uploaderNameFromURL);
                    }
                    if (!StringUtils.isEmpty(title)) {
                        thisResult.setProperty(FaceBookComVideos.PROPERTY_TITLE, title);
                    }
                    if (!StringUtils.isEmpty(uploader)) {
                        thisResult.setProperty(FaceBookComVideos.PROPERTY_UPLOADER, uploader);
                    }
                    if (publishDateFormatted != null) {
                        thisResult.setProperty(FaceBookComVideos.PROPERTY_DATE_FORMATTED, publishDateFormatted);
                    }
                    if (description != null) {
                        thisResult.setProperty(FaceBookComVideos.PROPERTY_DESCRIPTION, description);
                    }
                    thisResult.setAvailable(true);
                }
                results.addAll(thisResults);
                return;
            }
            for (final Map.Entry<String, Object> entry : map.entrySet()) {
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    crawlVideos(value, results);
                }
            }
            return;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    crawlVideos(arrayo, results);
                }
            }
            return;
        } else {
            return;
        }
    }

    private String getFormattedPublishTime(final long timestampSeconds) {
        final Date date = new Date(timestampSeconds * 1000);
        final String publishDateFormatted = new SimpleDateFormat("yyyy-MM-dd").format(date);
        return publishDateFormatted;
    }

    /**
     * Tries to find map containing extra video metadata. </br>
     * This does NOT work for reel videos -> facebook.com/reels/...
     */
    private Map<String, Object> findVideoExtraInfoMap(final List<Object> parsedJsons, final String videoid) {
        for (final Object parsedJsonO : parsedJsons) {
            final Object mapO = findVideoExtraInfoMapRecursive(parsedJsonO, videoid);
            if (mapO != null) {
                return (Map<String, Object>) mapO;
            }
        }
        return null;
    }

    private Object findVideoExtraInfoMapRecursive(final Object o, final String videoid) {
        if (videoid == null) {
            return null;
        }
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                final String __typename = (String) entrymap.get("__typename");
                final String id = (String) entrymap.get("id");
                final Object titlemap = entrymap.get("title");
                if (StringUtils.equalsIgnoreCase(__typename, "video") && StringUtils.equals(id, videoid) && titlemap instanceof Map) {
                    return entrymap;
                } else if (value instanceof List || value instanceof Map) {
                    final Object ret = findVideoExtraInfoMapRecursive(value, videoid);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object res = findVideoExtraInfoMapRecursive(arrayo, videoid);
                    if (res != null) {
                        return res;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private Map<String, Object> findVideoExtraInfoMap2(final List<Object> parsedJsons, final String videoid) {
        for (final Object parsedJsonO : parsedJsons) {
            final Object mapO = findVideoExtraInfoMapRecursive2(parsedJsonO, videoid);
            if (mapO != null) {
                return (Map<String, Object>) mapO;
            }
        }
        return null;
    }

    private Object findVideoExtraInfoMapRecursive2(final Object o, final String videoid) {
        if (videoid == null) {
            return null;
        }
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            final String id = (String) entrymap.get("id");
            final Object creation_story = entrymap.get("creation_story");
            if (StringUtils.equals(id, videoid) && creation_story != null) {
                return creation_story;
            }
            for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    final Object ret = findVideoExtraInfoMapRecursive2(value, videoid);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object res = findVideoExtraInfoMapRecursive2(arrayo, videoid);
                    if (res != null) {
                        return res;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private Map<String, Object> findPhotoExtraInfoMap(final List<Object> parsedJsons, final String photoid) {
        for (final Object parsedJsonO : parsedJsons) {
            final Object mapO = findPhotoExtraInfoMapRecursive(parsedJsonO, photoid);
            if (mapO != null) {
                return (Map<String, Object>) mapO;
            }
        }
        return null;
    }

    private Object findPhotoExtraInfoMapRecursive(final Object o, final String photoid) {
        if (photoid == null) {
            return null;
        }
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                final String __isMedia = (String) entrymap.get("__isMedia");
                final String id = (String) entrymap.get("id");
                if (StringUtils.equalsIgnoreCase(__isMedia, "Photo") && StringUtils.equals(id, photoid) && entrymap.containsKey("owner") && entrymap.containsKey("created_time") && entrymap.containsKey("message_preferred_body")) {
                    return entrymap;
                } else if (value instanceof List || value instanceof Map) {
                    final Object ret = findPhotoExtraInfoMapRecursive(value, photoid);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object res = findPhotoExtraInfoMapRecursive(arrayo, photoid);
                    if (res != null) {
                        return res;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private void crawlPhotos(final Object o, final ArrayList<DownloadLink> results) {
        if (o instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) o;
            for (final Map.Entry<String, Object> entry : map.entrySet()) {
                final String key = entry.getKey();
                final Object value = entry.getValue();
                if (key.equals("id") && value instanceof String) {
                    if (map.containsKey("__isMedia") && map.containsKey("image")) {
                        final String directurl = JavaScriptEngineFactory.walkJson(map, "image/uri").toString();
                        final DownloadLink photo = this.createDownloadlink(directurl);
                        photo.setProperty(FaceBookComVideos.PROPERTY_CONTENT_ID, value);
                        photo.setProperty(FaceBookComVideos.PROPERTY_TYPE, FaceBookComVideos.TYPE_PHOTO);
                        photo.setProperty(FaceBookComVideos.PROPERTY_DIRECTURL_LAST, directurl);
                        photo.setAvailable(true);
                        results.add(photo);
                        break;
                    } else {
                        continue;
                    }
                } else if (value instanceof List || value instanceof Map) {
                    crawlPhotos(value, results);
                }
            }
            return;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    crawlPhotos(arrayo, results);
                }
            }
            return;
        } else {
            return;
        }
    }

    @Deprecated
    private String getProfileID() {
        String profileid = br.getRegex("data-gt=\"\\&#123;\\&quot;profile_owner\\&quot;:\\&quot;(\\d+)\\&quot;").getMatch(0);
        if (profileid == null) {
            profileid = br.getRegex("PageHeaderPageletController_(\\d+)\"").getMatch(0);
            if (profileid == null) {
                profileid = br.getRegex("data-profileid=\"(\\d+)\"").getMatch(0);
                if (profileid == null) {
                    profileid = br.getRegex("\\\\\"profile_id\\\\\":(\\d+)").getMatch(0);
                }
            }
        }
        return profileid;
    }

    public static String getDyn() {
        return "7xeXxmdwgp8fqwOyax68xfLFwgoqwgEoyUnwgU6C7QdwPwDyUG4UeUuwh8eUny8lwIwHwJwr9U";
    }

    public static String get_fb_dtsg(final Browser br) {
        final String fb_dtsg = br.getRegex("name=\\\\\"fb_dtsg\\\\\" value=\\\\\"([^<>\"]*?)\\\\\"").getMatch(0);
        return fb_dtsg;
    }

    public static String getUser(final Browser br) {
        String user = br.getRegex("\"user\":\"(\\d+)\"").getMatch(0);
        if (user == null) {
            user = br.getRegex("detect_broken_proxy_cache\\(\"(\\d+)\", \"c_user\"\\)").getMatch(0);
        }
        // regex verified: 10.2.2014
        if (user == null) {
            user = br.getRegex("\\[(\\d+)\\,\"c_user\"").getMatch(0);
        }
        return user;
    }

    public static final String get_ttstamp() {
        return Long.toString(System.currentTimeMillis());
    }

    @Override
    public boolean isProxyRotationEnabledForLinkCrawler() {
        return false;
    }

    /**
     * Code below = prevents Eclipse from freezing as it removes the log output of this thread!
     **/
    private void disableLogger() {
        final LogInterface logger = new LogInterface() {
            @Override
            public void warning(String msg) {
            }

            @Override
            public void severe(String msg) {
            }

            @Override
            public void log(Throwable e) {
            }

            @Override
            public void info(String msg) {
            }

            @Override
            public void finest(String msg) {
            }

            @Override
            public void finer(String msg) {
            }

            @Override
            public void exception(String msg, Throwable e) {
            }

            @Override
            public void fine(String msg) {
            }
        };
        this.setLogger(logger);
        ((LinkCrawlerThread) Thread.currentThread()).setLogger(logger);
    }

    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}