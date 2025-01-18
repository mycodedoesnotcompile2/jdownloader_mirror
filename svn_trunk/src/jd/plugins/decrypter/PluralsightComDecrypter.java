package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;
import org.jdownloader.plugins.components.config.PluralsightComConfig;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.PluralsightCom;

@DecrypterPlugin(revision = "$Revision: 50463 $", interfaceVersion = 1, names = { "pluralsight.com" }, urls = { "https?://(?:app|www)?\\.pluralsight\\.com/.+" })
public class PluralsightComDecrypter extends antiDDoSForDecrypt {
    public PluralsightComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return PluralsightComConfig.class;
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        return crawl(param);
    }

    @Override
    public void sendRequest(Browser ibr, Request request) throws Exception {
        super.sendRequest(ibr, request);
    }

    private ArrayList<DownloadLink> crawl(final CryptedLink param) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean accountRequiredForCrawling = false;
        Account account = null;
        final PluginForHost hosterPlugin = this.getNewPluginForHostInstance(this.getHost());
        if (!accountRequiredForCrawling) {
            logger.info("No account used because: Not required");
        } else if ((account = AccountController.getInstance().getValidAccount(getHost())) != null) {
            ((jd.plugins.hoster.PluralsightCom) hosterPlugin).login(account, false);
            logger.info("Account - Mode:" + account.getUser());
        } else {
            logger.info("No account used because: No account available");
        }
        PluralsightCom.prepBR(this.br);
        final UrlQuery query = UrlQuery.parse(param.getCryptedUrl());
        String courseId = query.get("courseId");
        if (courseId == null) {
            courseId = new Regex(param.getCryptedUrl(), "(?i)/video-courses/([a-f0-9\\-]{4,})").getMatch(0);
        }
        String clipIdByAddedURL = query.get("clipId");
        if (clipIdByAddedURL == null) {
            clipIdByAddedURL = new Regex(param.getCryptedUrl(), "(?i)/clips?/([a-f0-9\\-]+)").getMatch(0);
        }
        final String courseSlug = new Regex(param.getCryptedUrl(), "(?i)https?://(?:app|www)?\\.pluralsight\\.com(?:\\/library)?\\/courses\\/([^/]+)").getMatch(0);
        final String baseURL = "https://app.pluralsight.com";
        if (courseId == null && clipIdByAddedURL == null) {
            /* Find courseId */
            if (courseSlug == null) {
                /* Invalid URL or developer mistake. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            getPage(baseURL + "/learner/content/courses/" + courseSlug);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            courseId = entries.get("courseId").toString();
        }
        if (courseId != null) {
            /* Get table of contents via courseId */
            getPage(baseURL + "/course-player/api/v1/table-of-contents/course/" + courseId);
        } else if (clipIdByAddedURL != null) {
            /* Get table of contents via clipId */
            getPage(baseURL + "/course-player/api/v1/table-of-contents/clip/" + clipIdByAddedURL);
        } else {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* Invalid courseId */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> course = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        // final String courseSlug = course.get("deprecatedCourseId").toString();
        final String courseTitle = (String) course.get("title");
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(courseTitle);
        final List<Map<String, Object>> modules = (List<Map<String, Object>>) course.get("modules");
        int moduleIndex = 0;
        int totalNumberofClips = 0;
        final PluralsightComConfig cfg = PluginJsonConfig.get(PluralsightComConfig.class);
        final ArrayList<DownloadLink> specificClipIDResults = new ArrayList<DownloadLink>();
        for (final Map<String, Object> module : modules) {
            final String moduleTitle = module.get("title").toString().trim();
            final List<Map<String, Object>> clips = (List<Map<String, Object>>) module.get("contentItems");
            int clipIndex = 0;
            for (final Map<String, Object> clip : clips) {
                totalNumberofClips++;
                final String title = (String) clip.get("title");
                final String type = StringUtils.valueOfOrNull(clip.get("type"));
                final String clipID = StringUtils.valueOfOrNull(clip.get("id"));
                final DownloadLink link;
                final String extension;
                boolean isDirecthttpURL = false;
                if (StringUtils.equalsIgnoreCase(type, "link")) {
                    final String url = (String) clip.get("url");
                    if (StringUtils.isEmpty(url)) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    link = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                    extension = Plugin.getFileNameExtensionFromURL(url, ".pdf");
                    isDirecthttpURL = true;
                } else if (StringUtils.equalsIgnoreCase(type, "clip")) {
                    link = new DownloadLink(hosterPlugin, null, this.getHost(), createContentURL(clipID), true);
                    extension = ".mp4";
                    final Number durationSeconds = (Number) clip.get("duration");
                    if (durationSeconds != null) {
                        link.setProperty(PluralsightCom.PROPERTY_DURATION_SECONDS, durationSeconds);
                    }
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unknown item type:" + type);
                }
                final List<DownloadLink> thisClipResults = new ArrayList<DownloadLink>();
                if (isDirecthttpURL) {
                    /* Misc files such as .pdf files. */
                    // TODO: 2023-04-19: Check filenames of such items.
                    link.setAvailable(true);
                    ret.add(link);
                    thisClipResults.add(link);
                } else {
                    /* Video/Subtitle */
                    final String clipVersion = (String) clip.get("version");
                    link.setProperty(PluralsightCom.PROPERTY_TYPE, extension.substring(1));
                    thisClipResults.add(link);
                    if (clipVersion != null && cfg.isCrawlSubtitles()) {
                        final String subtitleURL = baseURL + "/transcript/api/v1/caption/webvtt/" + clipID + "/" + clipVersion + "/en/";
                        final DownloadLink subtitle = new DownloadLink(hosterPlugin, null, this.getHost(), subtitleURL, true);
                        subtitle.setProperty(PluralsightCom.PROPERTY_DIRECTURL, subtitleURL);
                        subtitle.setProperty(PluralsightCom.PROPERTY_TYPE, PluralsightCom.TYPE_SUBTITLE);
                        thisClipResults.add(subtitle);
                    }
                    for (final DownloadLink thisClipResult : thisClipResults) {
                        thisClipResult.setAvailable(true);
                        thisClipResult.setProperty(PluralsightCom.PROPERTY_CLIP_ID, clipID);
                        if (clipVersion != null) {
                            thisClipResult.setProperty(PluralsightCom.PROPERTY_CLIP_VERSION, clipVersion);
                        }
                        thisClipResult.setProperty(PluralsightCom.PROPERTY_MODULE_ORDER_ID, moduleIndex + 1);
                        thisClipResult.setProperty(PluralsightCom.PROPERTY_MODULE_TITLE, moduleTitle);
                        thisClipResult.setProperty(PluralsightCom.PROPERTY_MODULE_CLIP_TITLE, title);
                        thisClipResult.setProperty(PluralsightCom.PROPERTY_CLIP_ORDER_ID, clipIndex + 1);
                        PluralsightCom.setFinalFilename(thisClipResult);
                    }
                }
                for (final DownloadLink thisClipResult : thisClipResults) {
                    thisClipResult._setFilePackage(fp);
                    ret.add(thisClipResult);
                }
                ret.addAll(thisClipResults);
                if (StringUtils.equals(clipID, clipIdByAddedURL)) {
                    specificClipIDResults.addAll(thisClipResults);
                }
                /* Continue to next item */
                clipIndex++;
            }
            moduleIndex++;
        }
        logger.info("Total number of clips crawled: " + totalNumberofClips);
        final boolean addOnlySpecificClipResultsIfSpecificClipIdIsAvailable = false;
        if (addOnlySpecificClipResultsIfSpecificClipIdIsAvailable && specificClipIDResults.size() > 0) {
            logger.info("Returning only specific clipId results as clipId was given inside user added URL | Returning " + specificClipIDResults.size() + "/" + ret.size() + " total results");
            return specificClipIDResults;
        } else {
            return ret;
        }
    }

    private static String createContentURL(final String clipID) {
        return "https://app.pluralsight.com/course-player?clipId=" + clipID;
    }
}
