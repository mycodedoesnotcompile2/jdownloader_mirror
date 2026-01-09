package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.parser.UrlQuery;

import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.requests.GetRequest;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
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
import jd.plugins.hoster.GofileIo;

@DecrypterPlugin(revision = "$Revision: 52067 $", interfaceVersion = 3, names = { "gofile.io" }, urls = { "https?://(?:www\\.)?gofile\\.io/(?:#download#|\\?c=|d/)([A-Za-z0-9\\-]+)" })
public class GoFileIoCrawler extends PluginForDecrypt {
    @Override
    public void init() {
        Browser.setRequestIntervalLimitGlobal(getHost(), 350);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        return decryptIt(param, progress, null);
    }

    protected static AtomicReference<String> WEBSITE_TOKEN           = new AtomicReference<String>();
    protected static AtomicLong              WEBSITE_TOKEN_TIMESTAMP = new AtomicLong(-1);
    protected final static long              WEBSITE_TOKEN_EXPIRE    = 30 * 60 * 1000l;

    public String getWebsiteToken(final Browser br) throws Exception {
        synchronized (WEBSITE_TOKEN) {
            String token = WEBSITE_TOKEN.get();
            if (!StringUtils.isEmpty(token) && Time.systemIndependentCurrentJVMTimeMillis() - WEBSITE_TOKEN_TIMESTAMP.get() < WEBSITE_TOKEN_EXPIRE) {
                return token;
            } else {
                final Browser brc = br.cloneBrowser();
                final GetRequest req = brc.createGetRequest("https://" + getHost() + "/dist/js/config.js");
                GofileIo.getPage(this, brc, req);
                token = brc.getRegex("websiteToken\\s*(?::|=)\\s*\"(.*?)\"").getMatch(0);
                if (token == null) {
                    /* 2024-01-26 / 2024-12-03 */
                    token = brc.getRegex("(?:fetchData|appdata)\\.wt\\s*(?::|=)\\s*\"(.*?)\"").getMatch(0);
                    if (token == null) {
                        /* 2024-03-11 */
                        token = brc.getRegex("wt\\s*:\\s*\"([^\"]+)").getMatch(0);
                    }
                }
                if (StringUtils.isEmpty(token)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                WEBSITE_TOKEN.set(token);
                WEBSITE_TOKEN_TIMESTAMP.set(Time.systemIndependentCurrentJVMTimeMillis());
                return token;
            }
        }
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress, Account account) throws Exception {
        final Browser brc = br.cloneBrowser();
        final GofileIo hosterplugin = (GofileIo) this.getNewPluginForHostInstance(this.getHost());
        final String token = hosterplugin.getAndSetToken(this, brc, account);
        final String folderID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("contentId", folderID);
        final String X_Website_Token = getWebsiteToken(br);
        String passCode = param.getDecrypterPassword();
        boolean passwordCorrect = true;
        boolean passwordRequired = false;
        int attempt = 0;
        Map<String, Object> response = null;
        Map<String, Object> response_data = null;
        passwordHandling: do {
            if (passwordRequired || passCode != null) {
                /* Pre-given password was wrong or this is the 2nd+ attempt -> Ask user for password */
                if (attempt > 0 || passCode == null) {
                    passCode = getUserInput("Password?", param);
                }
                query.addAndReplace("password", Hash.getSHA256(passCode));
            }
            final GetRequest req = br.createGetRequest("https://api." + this.getHost() + "/contents/" + folderID + "?" + query.toString());
            if (token != null) {
                req.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + token);
            }
            if (X_Website_Token != null) {
                req.getHeaders().put("X-Website-Token", X_Website_Token);
            }
            req.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://" + this.getHost()));
            req.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_REFERER, "https://" + this.getHost()));
            GofileIo.getPage(this, brc, req);
            response = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            response_data = (Map<String, Object>) response.get("data");
            final String passwordStatus = (String) response_data.get("passwordStatus");
            if (passwordStatus != null && (passwordStatus.equalsIgnoreCase("passwordRequired") || passwordStatus.equalsIgnoreCase("passwordWrong"))) {
                passwordRequired = true;
                passwordCorrect = false;
                passCode = null;
                attempt += 1;
                if (attempt >= 3) {
                    break passwordHandling;
                } else {
                    continue passwordHandling;
                }
            } else {
                passwordCorrect = true;
                break passwordHandling;
            }
        } while (!this.isAbort());
        if (passwordRequired && !passwordCorrect) {
            throw new DecrypterException(DecrypterException.PASSWORD);
        }
        if (!"ok".equals(response.get("status"))) {
            final String statustext = (String) response.get("status");
            if ("error-notPremium".equals(statustext)) {
                // {"status":"error-notPremium","data":{}}
                throw new AccountRequiredException("Premium account required to access this link");
            } else {
                /* Assume that folder is offline. */
                /* E.g. {"status":"error-notFound","data":{}} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        } else if (!Boolean.TRUE.equals(response_data.get("canAccess"))) {
            throw new AccountRequiredException("Private link");
        }
        final Map<String, Map<String, Object>> children = (Map<String, Map<String, Object>>) response_data.get("children");
        if (children.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        final String parentFolderShortID = response_data.get("code").toString();
        String lastItemFilename = null;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int numberofFiles = 0;
        int numberofFolders = 0;
        for (final Entry<String, Map<String, Object>> item : children.entrySet()) {
            final Map<String, Object> entry = item.getValue();
            final String type = (String) entry.get("type");
            if (type.equalsIgnoreCase("file")) {
                final String fileID = item.getKey();
                final String url = "https://" + getHost() + "/?c=" + folderID + "#file=" + fileID;
                final DownloadLink file = new DownloadLink(hosterplugin, null, this.getHost(), url, true);
                hosterplugin.parseFileInfo(file, account, entry);
                file.setProperty(GofileIo.PROPERTY_PARENT_FOLDER_SHORT_ID, parentFolderShortID);
                file.setAvailable(true);
                ret.add(file);
                lastItemFilename = file.getName();
                numberofFiles++;
            } else if (type.equalsIgnoreCase("folder")) {
                /* Subfolder containing more files/folders */
                final DownloadLink folder = this.createDownloadlink("https://" + this.getHost() + "/d/" + entry.get("code"));
                ret.add(folder);
                numberofFolders++;
            } else {
                /* This should never happen */
                logger.warning("Unsupported resource type: " + type);
                continue;
            }
        }
        final String folderDescription = (String) response_data.get("description");
        String currentFolderName = response_data.get("name").toString();
        if (StringUtils.isEmpty(currentFolderName)) {
            /* Fallback which should never be needed */
            currentFolderName = folderID;
        }
        logger.info("FolderID: " + folderID + " | files: " + numberofFiles + " | folders: " + numberofFolders);
        final String pathToParentFolder = this.getAdoptedCloudFolderStructure();
        /*
         * File is single loose file if we do not have a path of a previous folder structure && current folder contains exactly a single
         * file item.
         */
        String path = pathToParentFolder;
        final boolean isSingleLooseFile = pathToParentFolder == null && numberofFiles == 1 && numberofFolders == 0;
        if (path != null) {
            /*
             * We are inside a subfolder of another folder -> Append name of current folder to path. Ignore "invalid" folder names otherwise
             * our folder path would become invalid.
             */
            path += "/" + currentFolderName;
        } else {
            /* This is the root folder -> Set name of current folder as path */
            path = currentFolderName;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(path);
        if (!StringUtils.isEmpty(folderDescription)) {
            fp.setComment(folderDescription);
        }
        /* Set additional properties */
        for (final DownloadLink result : ret) {
            if (passCode != null) {
                result.setDownloadPassword(passCode);
            }
            if (isSingleLooseFile) {
                /*
                 * Single file
                 */
                /*
                 * Set folder description as single file comment since we do not set a FilePackage in this case so otherwise this
                 * description would be lost.
                 */
                if (!StringUtils.isEmpty(folderDescription)) {
                    result.setComment(folderDescription);
                }
            } else {
                /* We got multiple files which might be a part of a larger file/subfolder structure. */
                if (path != null) {
                    result.setRelativeDownloadFolderPath(path);
                }
                result._setFilePackage(fp);
            }
        }
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2024-01-26: Try to prevent running into rate-limit. */
        return 1;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
