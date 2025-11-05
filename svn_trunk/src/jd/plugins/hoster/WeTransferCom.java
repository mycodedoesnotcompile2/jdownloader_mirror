//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.shutdown.ShutdownVetoException;
import org.appwork.shutdown.ShutdownVetoListener;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.DispositionHeader;
import org.jdownloader.controlling.FileStateManager;
import org.jdownloader.controlling.FileStateManager.FILESTATE;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.WeTransferComFolder;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import jd.plugins.download.HashInfo;

@HostPlugin(revision = "$Revision: 51791 $", interfaceVersion = 2, names = { "wetransfer.com" }, urls = { "https?://wetransferdecrypted/[a-f0-9]{46}/[a-f0-9]{4,12}/[a-f0-9]{46}" })
public class WeTransferCom extends PluginForHost {
    public WeTransferCom(final PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium("https://auth." + getHost() + "/signup");
        }
    }
    // @Override
    // public LazyPlugin.FEATURE[] getFeatures() {
    // return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
    // }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/en-US/explore/legal/terms";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public static Browser prepBRWebsite(final Browser br) {
        br.addAllowedResponseCodes(new int[] { 410, 503 });
        br.setCookie("wetransfer.com", "wt_tandc", "20240117%3A1");
        br.setCookie("wetransfer.com", "wt_lang", "en");
        return br;
    }

    public static Browser prepBRAPI(final Browser br) {
        br.getHeaders().put("User-Agent", "okhttp/3.12.0");
        br.getHeaders().put("Accept", "application/json, text/plain, */*");
        br.setAllowedResponseCodes(new int[] { 401 });
        return br;
    }

    /* 2019-09-30: https://play.google.com/store/apps/details?id=com.wetransfer.app.live */
    public static final String   API_BASE_AUTH                                  = "https://api.wetransfermobile.com/v1";
    public static final String   API_BASE_NORMAL                                = "https://api.wetransfermobile.com/v2";
    public static final String   API_BASE_LOGIN                                 = "https://wetransfer.com/adroit/api";
    private static final Pattern TYPE_DOWNLOAD                                  = Pattern.compile("https?://wetransferdecrypted/([a-f0-9]{46})/([a-f0-9]{4,12})/([a-f0-9]{46})");
    public static final String   PROPERTY_DIRECT_LINK                           = "direct_link";
    public static final String   PROPERTY_DIRECT_LINK_EXPIRES_AT                = "direct_link_expires_at";
    public static final String   PROPERTY_SINGLE_ZIP                            = "single_zip";
    public static final String   PROPERTY_COLLECTION_ID                         = "collection_id";
    public static final String   PROPERTY_COLLECTION_FILE_ID                    = "collection_file_id";
    public static final String   PROPERTY_DOWNLOADER_EMAIL_VERIFICATION         = "downloader_email_verification";
    /* Account properties */
    public static final String   PROPERTY_ACCOUNT_ACCESS_TOKEN                  = "access_token";
    public static final String   PROPERTY_ACCOUNT_ACCESS_TOKEN_EXPIRE_TIMESTAMP = "access_token_expire_timestamp";
    public static final String   PROPERTY_ACCOUNT_REFRESH_TOKEN                 = "refresh_token";

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isSingleZip(link)) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        setBrowserExclusive();
        prepBRWebsite(br);
        final String directurl = link.getStringProperty(PROPERTY_DIRECT_LINK);
        final long directurlExpiresTimestamp = getStoredDirecturlValidityTimestamp(link);
        if (directurl != null && directurlExpiresTimestamp > System.currentTimeMillis()) {
            /* Trust direct-URL to still be usable so item is online. */
            return AvailableStatus.TRUE;
        }
        final String error_text_account_required = "Free account required to download this file";
        if (account == null && this.isAccountRequired(link)) {
            throw new AccountRequiredException(error_text_account_required);
        }
        if (account != null) {
            this.login(account, false);
        }
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), TYPE_DOWNLOAD);
        if (urlinfo.patternFind()) {
            final String folder_id = urlinfo.getMatch(0);
            final String security_hash = urlinfo.getMatch(1);
            final String file_id = urlinfo.getMatch(2);
            if (security_hash == null || folder_id == null || file_id == null) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "One or multiple plugin properties are missing");
            }
            final String refererurl = link.getReferrerUrl();
            if (refererurl == null) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Referer property is missing");
            }
            br.getPage(refererurl);
            final String[] recipient_id = refererurl.replaceFirst("https?://[^/]+/+", "").split("/");
            if (recipient_id == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String domain_user_id = br.getRegex("user\\s*:\\s*\\{\\s*\"key\"\\s*:\\s*\"(.*?)\"").getMatch(0);
            final String csrfToken = br.getRegex("name\\s*=\\s*\"csrf-token\"\\s*content\\s*=\\s*\"(.*?)\"").getMatch(0);
            String passCode = link.getDownloadPassword();
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("security_hash", security_hash);
            if (this.isSingleZip(link)) {
                postdata.put("intent", "entire_transfer");
            } else {
                postdata.put("intent", "single_file");
                postdata.put("file_ids", Arrays.asList(new String[] { file_id }));
            }
            if (recipient_id.length == 4) {
                postdata.put("recipient_id", recipient_id[2]);
            }
            if (domain_user_id != null) {
                postdata.put("domain_user_id", domain_user_id);
            }
            if (link.isPasswordProtected()) {
                if (passCode == null) {
                    passCode = getUserInput("Password?", link);
                }
                postdata.put("password", passCode);
            }
            final PostRequest post = new PostRequest(br.getURL(("/api/v4/transfers/" + folder_id + "/download")));
            post.getHeaders().put("Accept", "application/json");
            post.getHeaders().put("Content-Type", "application/json");
            post.getHeaders().put("Origin", "https://" + br.getHost());
            post.getHeaders().put("X-Requested-With", " XMLHttpRequest");
            if (csrfToken != null) {
                post.getHeaders().put("X-CSRF-Token", csrfToken);
            }
            post.setPostDataString(JSonStorage.serializeToJson(postdata));
            br.getPage(post);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (br.getHttpConnection().getResponseCode() == 403) {
                final String message = (String) entries.get("message");
                if ("invalid_transfer_password".equalsIgnoreCase(message)) {
                    link.setPasswordProtected(true);
                    link.setDownloadPassword(null);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered or password required");
                } else if ("No download access to this Transfer".equalsIgnoreCase(message)) {
                    /* Set property so such items will be instantly skipped when no account is available */
                    if (!link.hasProperty(PROPERTY_DOWNLOADER_EMAIL_VERIFICATION)) {
                        link.setProperty(PROPERTY_DOWNLOADER_EMAIL_VERIFICATION, "tracking");
                    }
                    throw new AccountRequiredException(error_text_account_required);
                } else if (message != null) {
                    /* Unknown error */
                    throw new PluginException(LinkStatus.ERROR_FATAL, message);
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Error 403");
                }
            } else if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String error = (String) entries.get("error");
            if (error != null) {
                if (error.equalsIgnoreCase("invalid_transfer")) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, error);
                }
            }
            final String direct_link = (String) entries.get("direct_link");
            if (!StringUtils.isEmpty(direct_link)) {
                link.setProperty(PROPERTY_DIRECT_LINK, direct_link);
            }
            /* Save valid download password if we know it. */
            if (link.isPasswordProtected()) {
                link.setDownloadPassword(passCode);
            }
        } else {
            final String collectionID = link.getStringProperty(PROPERTY_COLLECTION_ID);
            final String fileID = link.getStringProperty(PROPERTY_COLLECTION_FILE_ID);
            if (collectionID == null || fileID == null) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Required plugin property is missing");
            }
            final Browser brc = br.cloneBrowser();
            /* TODO: Maybe try to re-use cached token */
            final String token = WeTransferComFolder.getAPIToken(brc);
            brc.getHeaders().put("Authorization", "Bearer " + token);
            brc.postPageRaw(API_BASE_NORMAL + "/web/downloads/" + collectionID + "/public", "{\"file_ids\":[\"" + fileID + "\"]}");
            if (brc.getHttpConnection().getResponseCode() == 404) {
                /* E.g. {"success":false,"message":"Collection does not contain requested file(s)"} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            // brc.postPageRaw(API_BASE_NORMAL + "/mobile/downloads/" + collectionID + "/private", "{\"file_ids\":[\"" + fileID + "\"]}");
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final String error = (String) entries.get("error");
            if (error != null) {
                throw new PluginException(LinkStatus.ERROR_FATAL, error);
            }
            final String direct_link = (String) entries.get("download_url");
            if (!StringUtils.isEmpty(direct_link)) {
                link.setProperty(PROPERTY_DIRECT_LINK, direct_link);
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        String direct_link = link.getStringProperty(PROPERTY_DIRECT_LINK);
        final boolean stored_direct_link = direct_link != null;
        if (direct_link != null) {
            logger.info("Trying to re-use stored directurl: " + direct_link);
        } else {
            requestFileInformation(link, account);
            direct_link = link.getStringProperty(PROPERTY_DIRECT_LINK);
        }
        final boolean isSingleZip = this.isSingleZip(link);
        final int maxChunks;
        if (isSingleZip) {
            maxChunks = 1;
        } else {
            maxChunks = 1;
        }
        try {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, direct_link, this.isResumeable(link, null), maxChunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 10 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (stored_direct_link) {
                final long directurlExpiresTimestamp = getStoredDirecturlValidityTimestamp(link);
                final long timeDirecturlStillValid = directurlExpiresTimestamp - System.currentTimeMillis();
                if (timeDirecturlStillValid > 5 * 60 * 1000) {
                    /* Try again later with the same URL. */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Stored directurl did not lead to downloadable file", e);
                } else {
                    link.removeProperty(PROPERTY_DIRECT_LINK);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
                }
            } else {
                throw e;
            }
        }
        dl.startDownload();
        /**
         * 2024-02-27: This website delivers single files as .zip files without .zip file-extension while all of them contain exactly one
         * file. </br>
         * The special handling down below corrects this by extracting such files.
         */
        if (!isSingleZip && link.getLinkStatus().hasStatus(LinkStatus.FINISHED) && link.getDownloadCurrent() > 0) {
            extract(link);
        }
    }

    private boolean isAccountRequired(final DownloadLink link) {
        return StringUtils.equalsIgnoreCase(link.getStringProperty(PROPERTY_DOWNLOADER_EMAIL_VERIFICATION), "tracking");
    }

    private long getStoredDirecturlValidityTimestamp(final DownloadLink link) {
        return link.getLongProperty(PROPERTY_DIRECT_LINK_EXPIRES_AT, 0) * 1000;
    }

    @Override
    public List<File> listProcessFiles(DownloadLink link) {
        final List<File> ret = super.listProcessFiles(link);
        if (!this.isSingleZip(link)) {
            final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), TYPE_DOWNLOAD);
            final String file_id = urlinfo.getMatch(2);
            if (file_id != null) {
                final File extractedFile = new File(new File(link.getFileOutput()).getParent(), file_id + ".extracted");
                ret.add(extractedFile);
            }
        }
        return ret;
    }

    @Override
    public Downloadable newDownloadable(DownloadLink downloadLink, Browser br) {
        return new DownloadLinkDownloadable(downloadLink) {
            @Override
            public void setFinalFileName(final String newfinalFileName) {
                // filename header contains full path including parent directories -> only take filename part without parent directories
                final String fileNameOnly = newfinalFileName == null ? null : newfinalFileName.replaceFirst("^(.+/)", "");
                super.setFinalFileName(fileNameOnly);
            }

            @Override
            public boolean isHashCheckEnabled() {
                return false;
            }
        };
    }

    private void extract(final DownloadLink link) throws Exception {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), TYPE_DOWNLOAD);
        final String file_id = urlinfo.getMatch(2);
        if (file_id == null) {
            return;
        } else if (isSingleZip(link)) {
            return;
        }
        final File srcDst = new File(link.getFileOutput());
        ZipFile zipFile = null;
        try {
            final FileInputStream fis = new FileInputStream(srcDst);
            try {
                if (fis.read() != 0x50 || fis.read() != 0x4B) {
                    // no zip magic found
                    return;
                }
            } finally {
                fis.close();
            }
            zipFile = new ZipFile(srcDst);
        } catch (IOException e) {
            logger.log(e);
            return;
        }
        try {
            ZipEntry zipEntry = null;
            final Enumeration<? extends ZipEntry> zipEntries = zipFile.entries();
            while (zipEntries.hasMoreElements()) {
                if (zipEntry != null) {
                    logger.info("Skip extract as we found multiple ZipEntries!");
                    return;
                } else {
                    zipEntry = zipEntries.nextElement();
                }
            }
            String fileName = null;
            final DispositionHeader dispositionHeader = getDispositionHeader(dl.getConnection());
            if (dispositionHeader != null) {
                fileName = dispositionHeader.getFilename();
            }
            if (fileName == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (zipEntry == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "no entry in zip file!");
            } else {
                ZipEntry checkZipEntry = zipFile.getEntry(fileName);
                if (checkZipEntry != null) {
                    logger.info("Extract!Full matching ZipEntry found:" + checkZipEntry.getName());
                    zipEntry = checkZipEntry;
                } else if ((checkZipEntry = zipFile.getEntry(fileName.replaceFirst("^(.+/)", ""))) != null) {
                    logger.info("Extract!Filename only matching ZipEntry found:" + checkZipEntry.getName());
                    zipEntry = checkZipEntry;
                } else {
                    logger.info("Skip!No matching ZipEntry found:" + zipEntry.getName());
                    return;
                }
            }
            final ShutdownVetoListener vetoListener = new ShutdownVetoListener() {
                @Override
                public void onShutdownVetoRequest(ShutdownRequest request) throws ShutdownVetoException {
                    throw new ShutdownVetoException(getHost() + " extraction in progress:" + link.getName(), this);
                }

                @Override
                public void onShutdownVeto(ShutdownRequest request) {
                }

                @Override
                public void onShutdown(ShutdownRequest request) {
                }

                @Override
                public long getShutdownVetoPriority() {
                    return 0;
                }
            };
            File extractedFile = new File(srcDst.getParent(), file_id + ".extracted");
            if (!extractedFile.delete() && extractedFile.exists()) {
                throw new IOException("Could not delete:" + extractedFile);
            }
            FileStateManager.getInstance().requestFileState(extractedFile, FILESTATE.WRITE_EXCLUSIVE, this);
            ShutdownController.getInstance().addShutdownVetoListener(vetoListener);
            try {
                synchronized (getExtractionLock(link)) {
                    DigestInputStream dis = null;
                    final InputStream zis = zipFile.getInputStream(zipEntry);
                    try {
                        final FileOutputStream fos = new FileOutputStream(extractedFile);
                        try {
                            dis = new DigestInputStream(zis, MessageDigest.getInstance("SHA-256"));
                            final byte[] buffer = new byte[2048 * 1024];
                            while (true) {
                                final int read = dis.read(buffer);
                                if (read == -1) {
                                    break;
                                } else if (read > 0) {
                                    fos.write(buffer, 0, read);
                                }
                            }
                        } finally {
                            fos.close();
                        }
                    } finally {
                        zis.close();
                    }
                    zipFile.close();
                    zipFile = null;
                    if (!srcDst.delete() && srcDst.exists()) {
                        throw new IOException("Could not delete:" + srcDst);
                    } else {
                        if (extractedFile.renameTo(srcDst)) {
                            extractedFile = null;
                            link.setVerifiedFileSize(srcDst.length());
                            link.setHashInfo(HashInfo.parse(HexFormatter.byteArrayToHex(dis.getMessageDigest().digest()), true, true));
                        } else {
                            throw new IOException("Could not rename:" + extractedFile + " -> " + srcDst);
                        }
                    }
                }
            } finally {
                ShutdownController.getInstance().removeShutdownVetoListener(vetoListener);
                if (extractedFile != null) {
                    extractedFile.delete();
                }
                FileStateManager.getInstance().releaseFileState(extractedFile, this);
            }
        } finally {
            if (zipFile != null) {
                zipFile.close();
            }
        }
    }

    private static Object GLOBAL_EXTRACTION_LOCK = new Object();

    private Object getExtractionLock(final DownloadLink link) {
        return GLOBAL_EXTRACTION_LOCK;
    }

    /**
     * Returns true if this file is a single .zip file containing all items of a wetransfer.com item. In most cases that .zip file will
     * contain subfolders and files or at least 2 files.
     */
    private boolean isSingleZip(final DownloadLink link) {
        return link.getBooleanProperty(PROPERTY_SINGLE_ZIP, false);
    }

    private Map<String, Object> login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            logger.info("Attempting cookie/token login");
            String access_token = account.getStringProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN);
            final boolean allowOnlyLocalStorageStringAsPassword = true;
            Number access_token_expire_timestamp = this.getTokenExpireTimestamp(account);
            final String error_invalid_local_storage_login_input = "Invalid password syntax: Enter exported LocalStorage token string in password field";
            final String error_login_token_expired = "Login token expired";
            Map<String, Object> parsedJson = null;
            try {
                parsedJson = restoreFromString(account.getPass(), TypeRef.MAP);
            } catch (final JSonMapperException jme) {
                if (allowOnlyLocalStorageStringAsPassword) {
                    throw new AccountInvalidException(error_invalid_local_storage_login_input);
                }
            }
            if (parsedJson != null) {
                final Map<String, Object> data = (Map<String, Object>) parsedJson.get("data");
                if (data == null) {
                    throw new AccountInvalidException(error_invalid_local_storage_login_input);
                }
                access_token = (String) data.get("access_token");
                access_token_expire_timestamp = (Number) data.get("expiresAt");
                if (StringUtils.isEmpty(access_token)) {
                    throw new AccountInvalidException(error_invalid_local_storage_login_input);
                } else if (access_token_expire_timestamp == null) {
                    throw new AccountInvalidException(error_invalid_local_storage_login_input);
                }
                account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN, access_token);
                account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_EXPIRE_TIMESTAMP, access_token_expire_timestamp);
            }
            if (access_token != null) {
                if (access_token_expire_timestamp.longValue() <= System.currentTimeMillis()) {
                    /*
                     * 2025-11-04: Looks like the "expiresAt" timestamp from LocalStorage token string does not represent the expire date of
                     * that login token.
                     */
                    logger.info("Login token expired according to expire timestamp");
                    throw new AccountInvalidException(error_login_token_expired);
                }
                br.getHeaders().put("Authorization", "Bearer " + access_token);
                br.getHeaders().put("x-app-origin", "decoupled"); // optional
                // br.getHeaders().put("x-current-team", "TODO_not_needed_??");
                // br.getHeaders().put("x-local-storage-id", "TODO_not_needed_??");
                if (!force) {
                    /* Don't validate logins */
                    return null;
                }
                br.getPage(API_BASE_LOGIN + "/v1/users/me");
                if (br.getHttpConnection().getResponseCode() == 200) {
                    logger.info("Token login successful");
                    return (Map<String, Object>) this.checkErrorsAPI(br);
                }
                logger.info("Token login failed");
                /* Remove auth header */
                // br.getHeaders().put("Authorization", "");
                // if (account.hasEverBeenValid()) {
                // throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                // } else {
                // throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                // }
                throw new AccountInvalidException(error_login_token_expired);
            }
            if (allowOnlyLocalStorageStringAsPassword || !DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                throw new AccountInvalidException(error_invalid_local_storage_login_input);
            }
            final Cookies userCookies = account.loadCookies("");
            if (userCookies == null) {
                throw new AccountInvalidException(_GUI.T.accountdialog_LoginValidationErrorCookieLoginMandatoryButNoCookiesGiven());
            }
            br.setCookies(userCookies);
            // TODO: Fix this -> Obtain auth_token via cookies and the request down below
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("client_id", "TODO");
            postdata.put("code", "TODO");
            postdata.put("code_verifier", "TODO");
            postdata.put("grant_type", "authorization_code");
            postdata.put("redirect_uri", "https://wetransfer.com/account/callback?finalizeSSOAuth=1&login=1");
            br.postPageRaw("https://auth.wetransfer.com/oauth/token", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            access_token = resp.get("access_token").toString();
            account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN, access_token);
            // TODO: Fix this: Either set authorization header or add handling that creates fresh auth header via cookies
            br.getHeaders().put("Authorization", "Bearer " + access_token);
            br.getPage(API_BASE_LOGIN + "/v1/users/me");
            return (Map<String, Object>) this.checkErrorsAPI(br);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> user = login(account, true);
        if (Boolean.TRUE.equals(user.get("blocked"))) {
            /* This should be a super rare case! */
            throw new AccountInvalidException("Your account is banned/blocked");
        }
        final AccountInfo ai = new AccountInfo();
        /*
         * Plugin supports special login types only -> User could enter anything into username field -> Set username here so we can be sure
         * to have an unique username.
         */
        final String email = (String) user.get("email");
        if (!StringUtils.isEmpty(email)) {
            account.setUser(email);
        }
        final String date_created_at = user.get("created_at").toString();
        final String normalizedDate = date_created_at.replace("Z", "+0000");
        final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        ai.setCreateTime(sdf.parse(normalizedDate).getTime());
        ai.setUnlimitedTraffic();
        int activePremiumPackages = 0;
        int activeFreePackages = 0;
        int activeMiscPackages = 0;
        final List<Map<String, Object>> subscriptions = (List<Map<String, Object>>) user.get("subscriptions");
        for (Map<String, Object> subscription : subscriptions) {
            subscription = (Map<String, Object>) subscription.get("subscription");
            final String status = subscription.get("status").toString();
            if (!"active".equalsIgnoreCase(status)) {
                continue;
            }
            // TODO: Add check for premium packages
            final String tier = subscription.get("tier").toString();
            if (tier.equalsIgnoreCase("free")) {
                activeFreePackages += 1;
            } else {
                activeMiscPackages += 1;
            }
        }
        if (activePremiumPackages > 0) {
            // TODO: Set expire date (premium account needed for testing)
            account.setType(AccountType.PREMIUM);
        } else if (activeFreePackages > 0) {
            account.setType(AccountType.FREE);
        } else {
            account.setType(AccountType.UNKNOWN);
        }
        final long tokenExpireTimestamp = getTokenExpireTimestamp(account);
        if (tokenExpireTimestamp != -1) {
            ai.setStatus(account.getType().getLabel() + " | TokenValidity: " + TimeFormatter.formatMilliSeconds(tokenExpireTimestamp - System.currentTimeMillis(), 1));
        }
        return ai;
    }

    private long getTokenExpireTimestamp(final Account account) {
        return account.getLongProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_EXPIRE_TIMESTAMP, -1);
    }

    private Object checkErrorsAPI(final Browser br) throws PluginException {
        final Object object = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (!(object instanceof Map)) {
            return object;
        }
        final Map<String, Object> map = (Map<String, Object>) object;
        final String error = (String) map.get("error");
        if (error == null) {
            /* No error */
            return map;
        }
        // TODO: Add better errorhandling
        final int statusCode = ((Number) map.get("statusCode")).intValue();
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "ErrorCode " + statusCode);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null && this.isAccountRequired(link)) {
            /* Without account it's not possible to download this link. */
            return false;
        }
        return super.canHandle(link, account);
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return WetransferConfig.class;
    }

    public static interface WetransferConfig extends PluginConfigInterface {
        public static final TRANSLATION TRANSLATION  = new TRANSLATION();
        public static final CrawlMode   DEFAULT_MODE = CrawlMode.FILES_FOLDERS;

        public static class TRANSLATION {
            public String getCrawlMode2_label() {
                return "Crawl mode";
            }
        }

        public static enum CrawlMode implements LabelInterface {
            ZIP {
                @Override
                public String getLabel() {
                    return "Add .zip only";
                }
            },
            FILES_FOLDERS {
                @Override
                public String getLabel() {
                    return "Add individual files & folders";
                }
            },
            ALL {
                @Override
                public String getLabel() {
                    return "Add individual files & folders AND .zip with all items";
                }
            },
            DEFAULT {
                @Override
                public String getLabel() {
                    return "Default: " + DEFAULT_MODE.getLabel();
                }
            };
        }

        @AboutConfig
        @DefaultEnumValue("DEFAULT")
        @Order(10)
        @DefaultOnNull
        CrawlMode getCrawlMode2();

        void setCrawlMode2(final CrawlMode mode);
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        link.removeProperty(PROPERTY_DIRECT_LINK);
    }
    // @Override
    // protected boolean looksLikeValidAPIKey(final String str) {
    // if (str == null) {
    // return false;
    // }
    // /* Very basic validation of json string */
    // if (!str.startsWith("{")) {
    // return false;
    // } else if (!str.contains("\"access_token\"")) {
    // return false;
    // }
    // return true;
    // }
}