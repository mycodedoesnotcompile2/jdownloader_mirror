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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.WeTransferCom;
import jd.plugins.hoster.WeTransferCom.WetransferConfig;
import jd.plugins.hoster.WeTransferCom.WetransferConfig.CrawlMode;

@DecrypterPlugin(revision = "$Revision: 51712 $", interfaceVersion = 3, names = { "wetransfer.com" }, urls = { WeTransferComFolder.patternShort + "|" + WeTransferComFolder.patternNormal + "|" + WeTransferComFolder.patternCollection })
public class WeTransferComFolder extends PluginForDecrypt {
    public WeTransferComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    protected static final String patternShort                 = "https?://(?:we\\.tl|shorturls\\.wetransfer\\.com|go\\.wetransfer\\.com)/([\\w\\-]+)";
    protected static final String patternNormal                = "https?://(?:[\\w+\\-]+\\.)?wetransfer\\.com/downloads/(?:[a-f0-9]{46}/[a-f0-9]{46}/[a-f0-9]{4,12}|[a-f0-9]{46}/[a-f0-9]{4,12})";
    protected static final String patternCollection            = "https?://(?:boards|collect)\\.wetransfer\\.com/board/([a-z0-9]+)";
    private static final Pattern  PATTERN_COLLECTION           = Pattern.compile("(?i)https?://(boards|collect)\\.wetransfer\\.com/board/([a-z0-9]+)");
    private static Object         COLLECTION_OBTAIN_TOKEN_LOCK = new Object();

    public static String getAPIToken(final Browser br) throws IOException, PluginException {
        WeTransferCom.prepBRAPI(br);
        /*
         * 2019-09-30: E.g. {"device_token": "wt-android-<hash-length-8>-<hash-length-4>-<hash-length-4>-<hash-length-4>-<hash-length-12>"}
         */
        br.postPageRaw(WeTransferCom.API_BASE_AUTH + "/authorize", "{\"device_token\":\"wt-android-\"}");
        final Map<String, Object> tokenResp = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String token = tokenResp.get("token").toString();
        if (StringUtils.isEmpty(token)) {
            /* This should never happen. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return tokenResp.get("token").toString();
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String contenturl = param.getCryptedUrl();
        final Regex collection = new Regex(contenturl, patternCollection);
        if (collection.patternFind()) {
            final String collectionID = collection.getMatch(0);
            String token = null;
            boolean authed = false;
            int counter = 0;
            synchronized (COLLECTION_OBTAIN_TOKEN_LOCK) {
                token = this.getPluginConfig().getStringProperty("api_token");
                do {
                    /* Clear old headers and cookies each loop */
                    counter += 1;
                    WeTransferCom.prepBRAPI(br);
                    if (token == null) {
                        /* Only generate new token if needed */
                        token = getAPIToken(br);
                        /* Save token for to eventually re-use it later */
                        this.getPluginConfig().setProperty("api_token", token);
                    }
                    br.getHeaders().put("Authorization", "Bearer " + token);
                    br.getPage(WeTransferCom.API_BASE_NORMAL + "/mobile/collections/" + collectionID);
                    if (counter >= 2) {
                        logger.info("Stopping because: Too many failed auth attempts");
                        break;
                    } else if (br.getHttpConnection().getResponseCode() == 401) {
                        /* 2019-09-30 {"error":"Invalid JWT token : Signature Verification Error"} */
                        logger.info("Continue on invalid token");
                        token = null;
                        br.clearCookies(null);
                        continue;
                    }
                    authed = true;
                    break;
                } while (!this.isAbort());
            }
            if (!authed) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Authorization error");
            } else if (br.getHttpConnection().getResponseCode() == 404) {
                /* 2019-09-30 e.g. {"success":false,"message":"This collection does not exist","error_key":"board_deleted"} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final int maxItemsPerPage = 20;
            final int total_items = ((Number) entries.get("total_items")).intValue();
            final String name = entries.get("name").toString();
            final String description = (String) entries.get("description");
            if (total_items == 0) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, name);
            }
            final WeTransferCom hosterplugin = (WeTransferCom) this.getNewPluginForHostInstance(this.getHost());
            List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("items");
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(name);
            if (description != null) {
                fp.setComment(description);
            }
            fp.setPackageKey("wetransfer://collection/" + collectionID);
            int offset = 0;
            final HashSet<String> dupes = new HashSet<String>();
            do {
                int numberofNewItems = 0;
                for (final Map<String, Object> resource : ressourcelist) {
                    final String fileID = resource.get("id").toString();
                    if (!dupes.add(fileID)) {
                        continue;
                    }
                    final long filesize = ((Number) resource.get("size")).longValue();
                    final DownloadLink link = this.createDownloadlink("https://collect.wetransfer.com/board/" + collectionID + "/" + fileID);
                    link.setDefaultPlugin(hosterplugin);
                    link.setHost(hosterplugin.getHost());
                    link.setFinalFileName(resource.get("name").toString());
                    link.setVerifiedFileSize(filesize);
                    link.setContentUrl(contenturl);
                    link.setAvailable(true);
                    link._setFilePackage(fp);
                    link.setProperty(WeTransferCom.PROPERTY_DIRECT_LINK, resource.get("download_url"));
                    link.setProperty(WeTransferCom.PROPERTY_DIRECT_LINK_EXPIRES_AT, resource.get("download_url_expires_at"));
                    link.setProperty(WeTransferCom.PROPERTY_COLLECTION_ID, resource.get("collection_id"));
                    link.setProperty(WeTransferCom.PROPERTY_COLLECTION_FILE_ID, resource.get("id"));
                    ret.add(link);
                    distribute(link);
                    offset += 1;
                    numberofNewItems++;
                }
                logger.info("Crawled offset " + offset + " | Found new items on this page: " + numberofNewItems + " | Found items so far: " + ret.size() + "/" + total_items);
                if (ret.size() == total_items) {
                    logger.info("Stopping because: Reached last page");
                    break;
                } else if (numberofNewItems < maxItemsPerPage) {
                    logger.info("Stopping because: Current page contains less items than we expect to be on a full page");
                    break;
                } else if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break;
                } else {
                    /* Continue to next page */
                    br.getPage("/v2/mobile/collections/" + collectionID + "/items?offset=" + offset);
                    ressourcelist = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
                }
            } while (true);
        } else {
            WeTransferCom.prepBRWebsite(this.br);
            String shortID = new Regex(contenturl, patternShort).getMatch(0);
            final boolean accessURL;
            if (shortID != null) {
                /* Short link */
                br.getPage(contenturl);
                /* Redirects to somewhere */
                if (!this.canHandle(br.getURL())) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                contenturl = br.getURL();
                accessURL = false;
            } else {
                accessURL = true;
            }
            final Regex urlregex = new Regex(contenturl, "(?i)/downloads/([a-f0-9]+)/([a-f0-9]{46})?/?([a-f0-9]+)");
            final String folder_id = urlregex.getMatch(0);
            final String recipient_id = urlregex.getMatch(1);
            final String security_hash = urlregex.getMatch(2);
            if (security_hash == null || folder_id == null) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (accessURL) {
                br.getPage(contenturl);
                if (br.getHttpConnection().getResponseCode() == 410 || br.getHttpConnection().getResponseCode() == 503) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
            final String csrfToken = br.getRegex("name\\s*=\\s*\"csrf-token\"\\s*content\\s*=\\s*\"(.*?)\"").getMatch(0);
            // final String domain_user_id = br.getRegex("user\\s*:\\s*\\{\\s*\"key\"\\s*:\\s*\"(.*?)\"").getMatch(0);
            final Map<String, Object> jsonMap = new HashMap<String, Object>();
            jsonMap.put("security_hash", security_hash);
            if (recipient_id != null) {
                jsonMap.put("recipient_id", recipient_id);
            }
            final String refererValue = br.getURL();
            Map<String, Object> entries = null;
            String passCode = param.getDecrypterPassword();
            boolean passwordSuccess = false;
            boolean isPasswordProtected = false;
            handle_download_password: {
                int i = 0;
                while (!this.isAbort() && i <= 3) {
                    if (i > 0) {
                        passCode = getUserInput("Password?", param);
                    }
                    final PostRequest post = new PostRequest(br.getURL(("/api/v4/transfers/" + folder_id + "/prepare-download")));
                    post.getHeaders().put("Accept", "application/json");
                    post.getHeaders().put("Content-Type", "application/json");
                    post.getHeaders().put("Origin", "https://wetransfer.com");
                    post.getHeaders().put("X-Requested-With", " XMLHttpRequest");
                    if (csrfToken != null) {
                        post.getHeaders().put("X-CSRF-Token", csrfToken);
                    }
                    if (passCode != null) {
                        jsonMap.put("password", passCode);
                    }
                    post.setPostDataString(JSonStorage.serializeToJson(jsonMap));
                    br.getPage(post);
                    if (br.getHttpConnection().getResponseCode() == 404) {
                        /* {"message":"Transfer not found"} */
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                    entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    if (i == 0) {
                        isPasswordProtected = Boolean.TRUE.equals(entries.get("password_protected"));
                        if (!isPasswordProtected) {
                            /* No password required -> Exit loop */
                            passwordSuccess = true;
                            break handle_download_password;
                        }
                        logger.info("Password required");
                    } else {
                        /**
                         * Check if correct password has been entered <br>
                         * Response on invalid password: {"message":"invalid_transfer_password"}
                         */
                        if (br.getHttpConnection().getResponseCode() == 200) {
                            passwordSuccess = true;
                            break handle_download_password;
                        }
                        logger.info("User has entered wrong password: " + passCode);
                    }
                    /* Try [again] */
                    i++;
                }
                ;
                if (isPasswordProtected && !passwordSuccess) {
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
            }
            /* Download password has been entered successfully if needed -> Process item */
            final String internal_folder_id = entries.get("id").toString();
            final String state = (String) entries.get("state");
            /* Possible values: anonymous, tracking, more to be found */
            final String downloader_email_verification = (String) entries.get("downloader_email_verification");
            if (!"downloadable".equals(state)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            /* This is often the name of the first file in that folder. */
            final String thisFolderName = (String) entries.get("display_name");
            /* description field contains the "message" that the uploader of the file(s) can customize. */
            final String description = (String) entries.get("description");
            if (shortID == null) {
                /* Fallback */
                final String shortened_url = (String) entries.get("shortened_url");
                if (shortened_url != null && shortened_url.matches(patternShort)) {
                    shortID = new Regex(shortened_url, patternShort).getMatch(0);
                }
            }
            final WetransferConfig cfg = PluginJsonConfig.get(WetransferConfig.class);
            CrawlMode mode = cfg.getCrawlMode2();
            if (mode == null || CrawlMode.DEFAULT.equals(mode)) {
                mode = WetransferConfig.DEFAULT_MODE;
            }
            /* Use dummy file_id for the .zip download item. */
            final DownloadLink zip = this.createDownloadlink(createDummylinkForHosterplugin(folder_id, security_hash, "ffffffffffffffffffffffffffffffffffffffffffffff"));
            final String zipFilename = (String) entries.get("recommended_filename");
            if (zipFilename != null) {
                zip.setName(zipFilename);
            }
            zip.setDownloadSize(((Number) entries.get("size")).longValue());
            zip.setAvailable(true);
            zip.setProperty(WeTransferCom.PROPERTY_SINGLE_ZIP, true);
            final FilePackage rootPackage = FilePackage.getInstance();
            if (thisFolderName != null) {
                rootPackage.setName(thisFolderName);
            } else if (zipFilename != null) {
                rootPackage.setName(zipFilename);
            } else if (shortID != null) {
                /* Fallback 1 */
                rootPackage.setName(shortID);
            } else {
                /* Fallback 2 */
                rootPackage.setName(internal_folder_id);
            }
            if (description != null) {
                rootPackage.setComment(description);
            }
            rootPackage.setPackageKey(this.getHost() + "://folder/" + internal_folder_id);
            zip._setFilePackage(rootPackage);
            if (mode == CrawlMode.ZIP) {
                /* Return zip only */
                ret.add(zip);
            } else {
                final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("items");
                /* TODO: Handle this case */
                // final boolean per_file_download_available = map.containsKey("per_file_download_available") &&
                // Boolean.TRUE.equals(map.get("per_file_download_available"));
                /* E.g. okay would be "downloadable" */
                // final String state = (String) map.get("state");
                final Map<String, FilePackage> packagemap = new HashMap<String, FilePackage>();
                for (final Map<String, Object> resource : ressourcelist) {
                    final String file_id = resource.get("id").toString();
                    final String absolutePath = (String) resource.get("name");
                    final long filesize = ((Number) resource.get("size")).longValue();
                    final DownloadLink link = this.createDownloadlink(createDummylinkForHosterplugin(folder_id, security_hash, file_id));
                    String filename = null;
                    /*
                     * Add folderID as root of the path because otherwise files could be mixed up - there is no real "base folder name"
                     * given!
                     */
                    String pathForPlugin = null;
                    if (absolutePath.contains("/")) {
                        /* Looks like we got a subfolder-structure -> Build path */
                        if (shortID != null) {
                            pathForPlugin = shortID + "/";
                        } else {
                            /* Fallback */
                            pathForPlugin = folder_id + "/";
                        }
                        final String[] urlSegments = absolutePath.split("/");
                        filename = urlSegments[urlSegments.length - 1];
                        pathForPlugin += absolutePath.substring(0, absolutePath.lastIndexOf("/"));
                    } else {
                        /* In this case given name/path really is only the filename without path -> File of root folder */
                        filename = absolutePath;
                        /* Path == root */
                    }
                    if (pathForPlugin != null) {
                        link.setRelativeDownloadFolderPath(pathForPlugin);
                    }
                    link.setFinalFileName(filename);
                    link.setVerifiedFileSize(filesize);
                    link.setContentUrl(contenturl);
                    link.setAvailable(true);
                    ret.add(link);
                    /* Set individual packagename per URL because every item can have a totally different file-structure! */
                    if (pathForPlugin != null) {
                        FilePackage fp = packagemap.get(pathForPlugin);
                        if (fp == null) {
                            fp = FilePackage.getInstance();
                            fp.setName(pathForPlugin);
                            if (description != null) {
                                fp.setComment(description);
                            }
                            fp.setAllowMerge(true);
                            packagemap.put(pathForPlugin, fp);
                        }
                        link._setFilePackage(fp);
                    } else {
                        /* Assume that there is no subfolder and this file simply belongs to the root of this item. */
                        link._setFilePackage(rootPackage);
                    }
                }
                if (mode == CrawlMode.ALL) {
                    /* Add single .zip */
                    ret.add(zip);
                }
            }
            /* Add some properties */
            for (final DownloadLink result : ret) {
                result.setReferrerUrl(refererValue);
                if (isPasswordProtected) {
                    result.setPasswordProtected(true);
                    result.setDownloadPassword(passCode);
                }
                result.setProperty(WeTransferCom.PROPERTY_DOWNLOADER_EMAIL_VERIFICATION, downloader_email_verification);
            }
        }
        return ret;
    }

    private String createDummylinkForHosterplugin(final String folder_id, final String security_hash, final String file_id) {
        return "http://wetransferdecrypted/" + folder_id + "/" + security_hash + "/" + file_id;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
