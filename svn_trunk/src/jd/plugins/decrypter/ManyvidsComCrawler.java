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
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.ManyvidsCom;

@DecrypterPlugin(revision = "$Revision: 51098 $", interfaceVersion = 3, names = {}, urls = {})
public class ManyvidsComCrawler extends PluginForDecrypt {
    public ManyvidsComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "manyvids.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(Profile|Video)/(\\d+)(/([^/]+))?");
        }
        return ret.toArray(new String[0]);
    }
    // public static final String PATTERN_VIDEO = "/Video/(\\d+)(/([^/]+))?";
    // public static final String PATTERN_PROFILE = "/Profile/(\\d+)(/([^/]+))?";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        final Account account = AccountController.getInstance().getValidAccount(getHost());
        boolean hasMultiplePaidVideosPages = false;
        final HashSet<String> paidVideoIDs = new HashSet<String>();
        if (account != null) {
            final ManyvidsCom hosterplugin = (ManyvidsCom) this.getNewPluginForHostInstance(getHost());
            hosterplugin.login(account, false);
            /* 2025-05-28: We could use some caching here but for now I'll leave it as it is. */
            logger.info("Obtaining list of videoIDs which the user has bought to determine how to crawl them down below.");
            final Browser br2 = br.cloneBrowser();
            br2.getPage("https://www." + getHost() + "/View-my-history/1/");
            if (br2.containsHTML("View-my-history/2")) {
                // TODO: Add pagination support
                logger.info("User has multiple pages of paid videos available -> Crawl all videos separately to ensure that for paid videos, the full length videos get crawled");
                hasMultiplePaidVideosPages = true;
                logger.info("TODO: DEV: Add support for user purched videos pagination");
            }
            final String[] paidVideoidsArray = br2.getRegex("/Video/(\\d+)").getColumn(0);
            if (paidVideoidsArray != null && paidVideoidsArray.length > 0) {
                paidVideoIDs.addAll(Arrays.asList(paidVideoidsArray));
            }
            logger.info("Number of paid videos this user owns: " + paidVideoIDs.size());
        }
        final String manyvidsKeyPrefix = "manyvids_com://";
        String title = null;
        final boolean useWebsiteHandling = false;
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        if (!urlinfo.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contentType = urlinfo.getMatch(0);
        final String contentID = urlinfo.getMatch(1);
        String urlSlug = urlinfo.getMatch(3);
        if (contentType.equalsIgnoreCase("profile")) {
            /* Crawl user profile (crawls preview videos if user is not logged in) */
            // br.getPage("https://www." + getHost() + "/Profile/" + contentID + "/" + urlSlug + "/");
            // if (br.getHttpConnection().getResponseCode() == 404) {
            // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            // } else if (!br.getURL().contains(contentID)) {
            // /* E.g. redirect to mainpage */
            // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            // }
            br.getPage("https://www." + getHost() + "/Profile/" + contentID + "/" + urlSlug + "/Store/Videos/");
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!br.getURL().contains(contentID)) {
                /* E.g. redirect to mainpage */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Accept", "application/json, text/plain, */*");
            brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            int page = 1;
            int offset = 0;
            final HashSet<String> dupes = new HashSet<String>();
            final FilePackage fp = FilePackage.getInstance();
            if (urlSlug != null) {
                fp.setName(urlSlug.replace("-", " ").trim());
            } else {
                /* Fallback */
                fp.setName(contentID);
            }
            fp.setPackageKey(manyvidsKeyPrefix + contentType + "/" + contentID);
            final boolean crawlFreeVideosOneByOne = true;
            do {
                brc.getPage("/bff/store/videos/" + contentID + "/?page=" + page);
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final Map<String, Object> pagination = (Map<String, Object>) entries.get("pagination");
                final int totalNumberofItems = ((Number) pagination.get("total")).intValue();
                final Number nextPage = (Number) pagination.get("nextPage");
                if (totalNumberofItems == 0) {
                    throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE);
                }
                final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("data");
                int numberofNewItems = 0;
                for (final Map<String, Object> item : items) {
                    final Map<String, Object> creator = (Map<String, Object>) item.get("creator");
                    final Map<String, Object> thumbnail = (Map<String, Object>) item.get("thumbnail");
                    final Map<String, Object> preview = (Map<String, Object>) item.get("preview");
                    final Map<String, Object> price = (Map<String, Object>) item.get("price");
                    final String video_id = item.get("id").toString();
                    final String video_title = item.get("title").toString();
                    final String path = preview.get("url").toString();
                    if (!dupes.add(video_id)) {
                        continue;
                    }
                    numberofNewItems++;
                    final String content_url = "https://www." + getHost() + "/Video/" + video_id + "/" + item.get("slug");
                    final String previewSuffix;
                    boolean crawlThisVideoSeparately = paidVideoIDs.contains(video_id) || hasMultiplePaidVideosPages;
                    if (Boolean.TRUE.equals(price.get("free"))) {
                        if (crawlFreeVideosOneByOne) {
                            crawlThisVideoSeparately = true;
                        }
                        /* Free video */
                        previewSuffix = "";
                    } else {
                        /* Preview video */
                        previewSuffix = "_PREVIEW";
                    }
                    if (crawlThisVideoSeparately) {
                        /**
                         * This item will go back into our crawler to find the other/official streaming qualities. <br>
                         * This is important so that: <br>
                         * - Free full videos are crawled correctly <br>
                         * - Videos which the user has bought separately are crawled correctly
                         */
                        final DownloadLink video = this.createDownloadlink(content_url);
                        ret.add(video);
                        distribute(video);
                        continue;
                    }
                    if (thumbnail != null) {
                        final DownloadLink thumb = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(thumbnail.get("url").toString()));
                        thumb.setAvailable(true);
                        ret.add(thumb);
                    }
                    final String fullVideoURL = br.getURL(path).toExternalForm();
                    final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(fullVideoURL));
                    video.setContentUrl(content_url);
                    video.setAvailable(true);
                    video._setFilePackage(fp);
                    video.setFinalFileName(creator.get("slug") + "_" + video_title + previewSuffix + ".mp4");
                    video.setLinkID(manyvidsKeyPrefix + "video/" + video_id + "/quality/preview");
                    ret.add(video);
                    distribute(video);
                }
                logger.info("Crawled page: " + page + "/" + pagination.get("totalPages") + " | Offset: " + offset + " |  Number of new items on this page: " + numberofNewItems + " | Total found so far: " + ret.size() + "/" + totalNumberofItems);
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break;
                } else if (numberofNewItems == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page");
                    break;
                } else if (nextPage == null || nextPage.intValue() <= page) {
                    logger.info("Stopping because: Reached end");
                    break;
                } else {
                    /* Continue to next page */
                    offset += numberofNewItems;
                    page++;
                }
            } while (true);
        } else {
            /* Crawl single video */
            if (useWebsiteHandling) {
                /* Deprecated code */
                br.getPage(param.getCryptedUrl());
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                if (this.isAbort()) {
                    logger.info("Aborted by user");
                    return ret;
                }
                /* Search video title */
                if (urlSlug == null) {
                    urlSlug = br.getRegex(contentID + "/" + "([^\"/]+)/").getMatch(0);
                }
                if (urlSlug != null) {
                    title = urlSlug.replace("-", " ").trim();
                } else {
                    /* Fallback */
                    title = contentID;
                }
            }
            /* Find video metadata + screenshots */
            logger.info("Crawling metadata and images");
            final Browser brc = br.cloneBrowser();
            brc.getPage("https://video-player-bff.estore.kiwi.manyvids.com/vercel/videos/" + contentID);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final int statusCode = ((Number) entries.get("statusCode")).intValue();
            if (statusCode != 200) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            final Map<String, Object> model = (Map<String, Object>) data.get("model");
            title = data.get("title").toString();
            title = Encoding.htmlDecode(title).trim();
            final String filesizeStr = (String) data.get("size");
            final String description = (String) data.get("description");
            // final Boolean isFree = (Boolean) data.get("isFree");
            final String urlThumbnail = data.get("thumbnail").toString();
            ret.add(this.createDownloadlink(DirectHTTP.createURLForThisPlugin(urlThumbnail)));
            final String urlScreenshot = data.get("screenshot").toString();
            ret.add(this.createDownloadlink(DirectHTTP.createURLForThisPlugin(urlScreenshot)));
            if (this.isAbort()) {
                logger.info("Aborted by user");
                return ret;
            }
            /* Find stream-downloadurls */
            brc.getPage("/vercel/videos/" + contentID + "/private");
            final Map<String, Object> entries2 = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> data2 = (Map<String, Object>) entries2.get("data");
            boolean crawlVideoStreams = true;
            final String filename_base = model.get("displayName") + "_" + title;
            if (Boolean.TRUE.equals(data2.get("isDownloadable"))) {
                /**
                 * Crawl official video downloads </br>
                 * Users can download videos if they bought them (or if they are premium/flatrate users??)
                 */
                logger.info("Crawling official downloadlinks");
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("id", contentID);
                query.appendEncoded("etag", "KIWI_video_player");
                br.postPage("https://www." + getHost() + "/download.php?" + query.toString(), query);
                final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final Object errorO = resp.get("error");
                if (errorO != null) {
                    /* This should never happen */
                    logger.info("Failed to obtain official downloadlinks because: " + errorO);
                } else {
                    /* Crawl downloadlinks */
                    final ArrayList<DownloadLink> officialVideoDownloads = new ArrayList<DownloadLink>();
                    final Iterator<Entry<String, Object>> iterator = resp.entrySet().iterator();
                    while (iterator.hasNext()) {
                        final Entry<String, Object> entry = iterator.next();
                        final String videoQualityLabel = entry.getKey();
                        final Map<String, Object> qualitymap = (Map<String, Object>) entry.getValue();
                        final String file_url = qualitymap.get("file_url").toString();
                        final String officialVideoDownloadFilesizeStr = qualitymap.get("file_size").toString();
                        /* Don't do this, it will fuckup URL-encoding. */
                        // final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(file_url));
                        final DownloadLink video = this.createDownloadlink(file_url);
                        video.setFinalFileName(filename_base + "_" + videoQualityLabel + ".mp4");
                        video.setDownloadSize(SizeFormatter.getSize(officialVideoDownloadFilesizeStr));
                        video.setLinkID(manyvidsKeyPrefix + "video/" + contentID + "/quality/" + videoQualityLabel);
                        officialVideoDownloads.add(video);
                        if (videoQualityLabel.equalsIgnoreCase("original")) {
                            /* Best possible quality -> Only take this if available */
                            officialVideoDownloads.clear();
                            officialVideoDownloads.add(video);
                            break;
                        }
                    }
                    ret.addAll(officialVideoDownloads);
                    crawlVideoStreams = false;
                }
            }
            if (crawlVideoStreams) {
                logger.info("Crawling stream downloadurls");
                final String videourl1 = (String) data2.get("filepath");
                boolean originalVideoAvailable = false;
                if (!StringUtils.isEmpty(videourl1)) {
                    /* Original video -> We may know the filesize of this item. */
                    final DownloadLink video1 = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(videourl1));
                    if (!StringUtils.isEmpty(filesizeStr)) {
                        video1.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                    }
                    video1.setFinalFileName(filename_base + ".mp4");
                    ret.add(video1);
                    originalVideoAvailable = true;
                }
                if (!originalVideoAvailable) {
                    final String videourl2 = (String) data2.get("transcodedFilepath");
                    if (!StringUtils.isEmpty(videourl2)) {
                        /* Transcoded video -> We do not know the filesize of this item. */
                        final DownloadLink video2 = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(videourl2));
                        video2.setFinalFileName(filename_base + ".mp4");
                        ret.add(video2);
                    }
                    final Map<String, Object> teasermap = (Map<String, Object>) data2.get("teaser");
                    if (teasermap != null && StringUtils.isEmpty(videourl2)) {
                        final String videourlTeaser = (String) teasermap.get("filepath");
                        if (!StringUtils.isEmpty(videourlTeaser)) {
                            final DownloadLink video3 = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(videourlTeaser));
                            video3.setFinalFileName(filename_base + "_PREVIEW.mp4");
                            ret.add(video3);
                        }
                    }
                }
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            if (!StringUtils.isEmpty(description)) {
                fp.setComment(description);
            }
            fp.setPackageKey(manyvidsKeyPrefix + contentType + "/" + contentID);
            /* Set additional properties */
            for (final DownloadLink result : ret) {
                result._setFilePackage(fp);
                /* We know that all of our results are online and downloadable. */
                result.setAvailable(true);
            }
        }
        return ret;
    }
}
