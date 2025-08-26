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

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.StreamrecorderIo;

@DecrypterPlugin(revision = "$Revision: 51365 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { StreamrecorderIo.class })
public class StreamrecorderIoCrawler extends PluginForDecrypt {
    public StreamrecorderIoCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return StreamrecorderIo.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(/userrecordings(/(recording|playlist)/\\d+)?|/clip/[^/\\?#]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return crawl(param, account);
    }

    private StreamrecorderIo hosterplugin = null;

    @Override
    public void clean() {
        hosterplugin = null;
        super.clean();
    }

    public ArrayList<DownloadLink> crawl(final CryptedLink param, final Account account) throws Exception {
        if (account == null) {
            throw new AccountRequiredException();
        }
        hosterplugin = (StreamrecorderIo) this.getNewPluginForHostInstance(this.getHost());
        hosterplugin.login(account, false);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (param.getCryptedUrl().matches("(?i).*/clip/.+")) {
            br.getPage(param.getCryptedUrl());
            if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("<title>\\s*Error 404")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String directurl = br.getRegex("<meta property=\"og:video\" content=\"(.*?)\"").getMatch(0);
            final String video_Resolution_height = br.getRegex("<meta property=\"og:video:height\" content=\"(\\d+)\"").getMatch(0);
            final String recordingID = br.getRegex("/recording/(\\d+)").getMatch(0);
            final String username = br.getRegex("<meta property=\"og:video:tag\" content=\"(.*?) (clip|vod|stream|download)\">").getMatch(0);
            final String clipID = new Regex(param.getCryptedUrl(), "/clip/([^/\\?#]+)").getMatch(0);
            final String userID = br.getRegex("/img/profile_image/(\\d+)").getMatch(0);
            final DownloadLink video = this.createDownloadlink(directurl);
            video.setDefaultPlugin(hosterplugin);
            video.setHost(getHost());
            video.setProperty(StreamrecorderIo.PROPERTY_user_id, userID);
            video.setProperty(StreamrecorderIo.PROPERTY_username, username);
            video.setProperty(StreamrecorderIo.PROPERTY_record_id, recordingID);
            video.setProperty(StreamrecorderIo.PROPERTY_clip_id, clipID);
            video.setProperty(StreamrecorderIo.PROPERTY_video_resolution_height, video_Resolution_height);
            video.setProperty(StreamrecorderIo.PROPERTY_directurl, directurl);
            video.setProperty(StreamrecorderIo.PROPERTY_type, StreamrecorderIo.PROPERTY_TYPE_CLIP);
            video.setProperty(StreamrecorderIo.PROPERTY_downloadable_via_account_id, account.getUser());
            final String filename = getFileNameFromURL(new URL(directurl));
            if (filename != null) {
                video.setFinalFileName(filename.replace("+", " "));
            }
            video.setAvailable(true);
            ret.add(video);
            return ret;
        }
        if (param.getCryptedUrl().matches("(?i).*/playlist/\\d+")) {
            br.getPage(param.getCryptedUrl());
            if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("<title>\\s*Error 404")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String recordingIDs[] = br.getRegex("loadModalPlayer\\((\\d+)").getColumn(0);
            final String displayName = br.getRegex("<span class\\s*=\\s*\"user-name[^\"]*\"\\s*>\\s*<span>\\s*(.*?)\\s*</span>").getMatch(0);
            if (displayName != null) {
                String title = br.getRegex("</span>\\s*([^>]*?\\s*Stream\\s*from\\s*<span sr-date-time.*?</span>\\s*to\\s*<span sr-date-time[^<]*</span>)\\s*</?span").getMatch(0);
                final List<DownloadLink> results = findUserRecordings(account, br.cloneBrowser(), displayName, new HashSet<String>(Arrays.asList(recordingIDs)));
                if (results.size() > 0) {
                    ret.addAll(results);
                    if (title != null) {
                        title = title.replaceAll("<span[^>]*>", "").replaceAll("</span>", "");
                        final FilePackage fp = FilePackage.getInstance();
                        fp.setName(title);
                        fp.addLinks(ret);
                    }
                    return ret;
                }
            }
            for (String recordingID : recordingIDs) {
                ret.add(createDownloadlink("https://streamrecorder.io/userrecordings/recording/" + recordingID));
            }
            return ret;
        }
        if (param.getCryptedUrl().matches("(?i).*/recording/\\d+")) {
            final String recordingID = new Regex(param.getCryptedUrl(), "/recording/(\\d+)").getMatch(0);
            br.getPage("https://streamrecorder.io/recordings/stream/" + recordingID);
            final Map<String, Object> recordingEntry = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if ("no access".equals(recordingEntry.get("error"))) {
                throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND);
            }
            final String displayName = (String) recordingEntry.get("displayname");
            List<DownloadLink> results = null;
            if (displayName != null) {
                results = findUserRecordings(account, br.cloneBrowser(), displayName, new HashSet<String>(Arrays.asList(recordingID)));
            }
            if (results == null || results.size() == 0) {
                results = parseRecordingEntry(account, null, displayName, recordingID, recordingEntry);
            }
            final String title = (String) recordingEntry.get("title");
            ret.addAll(results);
            if (title != null) {
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(title);
                fp.addLinks(ret);
            }
            return ret;
        }
        ret.addAll(findUserRecordings(account, br, null, null));
        return ret;
    }

    private List<DownloadLink> findUserRecordings(final Account account, final Browser br, final String displayName, final Set<String> recordingIDs) throws Exception {
        final List<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage("https://" + getHost() + "/userrecordings");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String error_msg_no_recordings = "User has zero recordings";
        final String[] user_ids = br.getRegex("content-(\\d+)").getColumn(0);
        if (user_ids == null || user_ids.length == 0) {
            final String totalNumberofRecordingsStr = br.getRegex("class=\"count\">(\\d+)</div>\\s*<div class=\"title\"[^^>]*>\\s*Total recordings\\s*</div>").getMatch(0);
            if (totalNumberofRecordingsStr != null && totalNumberofRecordingsStr.equals("0")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, error_msg_no_recordings);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final HashSet<String> dupes_user_ids = new HashSet<String>();
        int numberOfUsersWithZeroRecordings = 0;
        userloop: for (final String user_id : user_ids) {
            if (!dupes_user_ids.add(user_id)) {
                /* Prevent crawling the same user_id multiple times. */
                continue userloop;
            }
            String username = null;
            int offset = 0;
            final int maxItemsPerPage = 20;
            FilePackage fp = null;
            fetchUserVideoLoop: do {
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("targetid", user_id);
                query.appendEncoded("offset", Integer.toString(offset));
                query.appendEncoded("limit", Integer.toString(maxItemsPerPage));
                final Browser brc = br.cloneBrowser();
                brc.getPage("/api/user/recordingsv2?" + query.toString());
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final String errorStr = (String) entries.get("error");
                if (errorStr != null) {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, errorStr);
                }
                final int totalNumberofRecordings = ((Number) entries.get("count")).intValue();
                if (totalNumberofRecordings == 0) {
                    numberOfUsersWithZeroRecordings++;
                    continue;
                }
                final List<Map<String, Object>> recordings = (List<Map<String, Object>>) entries.get("data");
                int newItemsThisPage = 0;
                final ArrayList<DownloadLink> thisresults = new ArrayList<DownloadLink>();
                recordingsLoop: for (final Map<String, Object> recording : recordings) {
                    if (username == null) {
                        /* First loop */
                        username = recording.get("target_name").toString();
                        fp = FilePackage.getInstance();
                        fp.setName(username);
                    }
                    if (recordingIDs != null) {
                        if (!recordingIDs.remove(StringUtils.valueOfOrNull(recording.get("id")))) {
                            continue recordingsLoop;
                        }
                    }
                    final List<DownloadLink> results = parseRecordingEntry(account, user_id, username, null, recording);
                    // parentID -> another recording that belongs to same "recording" session, see /playlist/ or playListMode
                    newItemsThisPage += results.size();
                    thisresults.addAll(results);
                }
                for (final DownloadLink thisresult : thisresults) {
                    ret.add(thisresult);
                    if (displayName == null) {
                        thisresult._setFilePackage(fp);
                        distribute(thisresult);
                    }
                }
                logger.info("UserID" + user_id + " | Crawled page: " + "" + " | Offset: " + offset + "/" + totalNumberofRecordings + " | Found items so far: " + ret.size());
                offset += recordings.size();
                if (recordingIDs != null && recordingIDs.isEmpty()) {
                    logger.info("UserID " + user_id + ": Stopping because: recordingIDs array is empty");
                    break fetchUserVideoLoop;
                }
                if (newItemsThisPage == 0 && recordingIDs == null) {
                    logger.info("UserID " + user_id + ": Stopping because: Reached end?");
                    break fetchUserVideoLoop;
                } else if (this.isAbort()) {
                    /* Aborted by user */
                    throw new InterruptedException();
                } else if (offset >= totalNumberofRecordings) {
                    logger.info("UserID " + user_id + ": Stopping because: Reached end");
                    break fetchUserVideoLoop;
                }
            } while (!this.isAbort());
        }
        if (ret.isEmpty() && numberOfUsersWithZeroRecordings > 0) {
            /* Edge case: User is following some users but none of them have any recorded videos available. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, error_msg_no_recordings);
        }
        return ret;
    }

    private List<DownloadLink> parseRecordingEntry(final Account account, String userID, String username, String recordingID, Map<String, Object> recording) throws Exception {
        List<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (recordingID == null) {
            recordingID = recording.get("id").toString();
        }
        String title = (String) recording.get("streamtitle");
        if (title == null) {
            // https://streamrecorder.io/recordings/stream/
            title = (String) recording.get("title");
            if (title == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (userID == null) {
            // https://streamrecorder.io/recordings/stream/
            userID = new Regex(StringUtils.valueOfOrNull(recording.get("profile_image_url")), "/img/profile_image/(\\d+)").getMatch(0);
        }
        final String status = recording.get("status").toString();
        if (username == null) {
            // https://streamrecorder.io/recordings/stream/
            username = (String) recording.get("displayname");
        }
        if (Boolean.TRUE.equals(recording.get("premium_required"))) {
            logger.info("Skipping recordingID " + recordingID + " | Reason: Premium-only");
            return ret;
        } else if (!StringUtils.equalsIgnoreCase(status, "finished")) {
            logger.info("Skipping recordingID " + recordingID + " | Reason: status != 'finished'");
            return ret;
        }
        final List<Map<String, Object>> sources = (List<Map<String, Object>>) recording.get("sources");
        for (final Map<String, Object> source : sources) {
            String directurl = (String) source.get("downloadlink");
            if (directurl == null) {
                // https://streamrecorder.io/recordings/stream/
                directurl = (String) source.get("src");
                if (directurl == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            String video_Resolution_height = StringUtils.valueOfOrNull(source.get("resolution"));
            if (video_Resolution_height == null) {
                // https://streamrecorder.io/recordings/stream/
                video_Resolution_height = StringUtils.valueOfOrNull(source.get("size"));
                if (video_Resolution_height == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            final DownloadLink video = this.createDownloadlink(directurl);
            video.setDefaultPlugin(hosterplugin);
            video.setHost(getHost());
            video.setProperty(StreamrecorderIo.PROPERTY_streamcategory, recording.get("streamcategory"));
            video.setProperty(StreamrecorderIo.PROPERTY_recorded_at, recording.get("recorded_at"));
            video.setProperty(StreamrecorderIo.PROPERTY_downloaded_according_to_api, recording.get("downloaded"));
            video.setProperty(StreamrecorderIo.PROPERTY_duration_seconds, recording.get("duration"));
            video.setProperty(StreamrecorderIo.PROPERTY_title, title);
            video.setProperty(StreamrecorderIo.PROPERTY_user_id, userID);
            video.setProperty(StreamrecorderIo.PROPERTY_username, username);
            video.setProperty(StreamrecorderIo.PROPERTY_record_id, recordingID);
            video.setProperty(StreamrecorderIo.PROPERTY_video_resolution_height, video_Resolution_height);
            video.setProperty(StreamrecorderIo.PROPERTY_directurl, directurl);
            video.setProperty(StreamrecorderIo.PROPERTY_type, StreamrecorderIo.PROPERTY_TYPE_RECORDING);
            video.setProperty(StreamrecorderIo.PROPERTY_downloadable_via_account_id, account.getUser());
            video.setFinalFileName(username + "_" + title + "_" + video_Resolution_height + ".mp4");
            video.setVerifiedFileSize(((Number) source.get("filesize")).longValue());
            video.setAvailable(true);
            ret.add(video);
        }
        String url_thumb_large = (String) recording.get("thumb_large");
        if (url_thumb_large == null) {
            // https://streamrecorder.io/recordings/stream/
            url_thumb_large = (String) recording.get("poster");
        }
        if (!StringUtils.isEmpty(url_thumb_large)) {
            final DownloadLink thumbnail = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url_thumb_large));
            thumbnail.setProperty(DirectHTTP.PROPERTY_CUSTOM_HOST, getHost());
            thumbnail.setFinalFileName(title + "_thumb_large.jpg");
            thumbnail.setAvailable(true);
            ret.add(thumbnail);
        }
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
