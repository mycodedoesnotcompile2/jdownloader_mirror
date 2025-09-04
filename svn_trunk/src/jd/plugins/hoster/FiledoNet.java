package jd.plugins.hoster;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.requests.GetRequest;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginBrowser;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class FiledoNet extends PluginForHost {
    public FiledoNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(getPurchasePremiumURL());
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = new PluginBrowser<FiledoNet>(this) {
            {
                getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
                setFollowRedirects(true);
            }
        };
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "filedo.net" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/d/([A-Fa-f0-9\\-]{32,}).*");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return getFID(link.getPluginPatternMatcher());
    }

    private String getFID(final String url) {
        return new Regex(url, this.getSupportedLinks()).getMatch(0);
    }

    private final String API_BASE = "https://api.filedo.net";

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/tos";
    }

    public String getPurchasePremiumURL() {
        return "https://" + getHost() + "/pricing";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        return checkLinks(urls, null);
    }

    /** Using API: https://api.filedo.net/doc/index.html#/File/post_File_Multiple */
    private boolean checkLinks(final DownloadLink[] urls, final Account account) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        try {
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final Map<String, DownloadLink> links = new HashMap<String, DownloadLink>();
            int index = 0;
            while (index < urls.length) {
                links.clear();
                while (true) {
                    if (index == urls.length || links.size() == 50) {
                        break;
                    } else {
                        final DownloadLink link = urls[index++];
                        final String fileID = getFID(link);
                        if (!link.isNameSet()) {
                            link.setName(fileID);
                        }
                        links.put(fileID, link);
                    }
                }
                sb.delete(0, sb.capacity());
                final PostRequest req = br.createJSonPostRequest(API_BASE + "/file/multiple", JSonStorage.serializeToJson(links.keySet()));
                final List<Map<String, Object>> responselist = (List<Map<String, Object>>) this.callAPI(req, account, links.get(0));
                for (final Map<String, Object> resp : responselist) {
                    final String fileID = resp.get("fileId").toString();
                    final DownloadLink link = links.remove(fileID);
                    if (link == null) {
                        logger.info("No response for: " + fileID);
                        continue;
                    }
                    try {
                        final int fileStatus = ((Number) resp.get("fileStatus")).intValue();
                        if (fileStatus != 1) {
                            link.setAvailable(false);
                            continue;
                        }
                        final String cryptedFileName = resp.get("fileName").toString();
                        final Number fileSize = (Number) resp.get("fileSize");
                        final String fileHash = resp.get("fileHash").toString();
                        final String downloadUrl = resp.get("downloadUrl").toString();
                        final Boolean hasCaptcha = (Boolean) resp.get("hasCaptcha");
                        if (fileSize != null) {
                            link.setVerifiedFileSize(fileSize.longValue());
                        }
                        /* Try to set final filename here because Content-Disposition header contains crypted filename. */
                        /* Set final filename here because Content-Disposition header contains crypted filename. */
                        try {
                            final String decryptedFileName = decryptFileName(cryptedFileName, link.getPluginPatternMatcher());
                            link.setFinalFileName(decryptedFileName);
                            link.setProperty("decryptedFileName", decryptedFileName);
                        } catch (final Exception e) {
                            logger.info("Invalid key");
                            link.setName(cryptedFileName);
                        }
                        link.setMD5Hash(fileHash);
                        link.setProperty("dl3", downloadUrl);
                        // link.setProperty("hashedFileName", fileName);
                        if (Boolean.TRUE.equals(resp.get("hasCaptcha"))) {
                            link.setProperty("hasCatpcha", hasCaptcha);
                        } else {
                            link.removeProperty("hasCatpcha");
                        }
                        link.setAvailable(true);
                    } catch (final Exception e) {
                        logger.log(e);
                    }
                }
                /** All items which were not included in the API response shall be offline. */
                for (final DownloadLink link : links.values()) {
                    link.setAvailable(false);
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        final String fileId = this.getFID(link);
        if (!link.isNameSet()) {
            /* Set weak-filename */
            link.setName(fileId);
        }
        final boolean useMassLinkchecker = true;
        if (useMassLinkchecker) {
            return requestFileInformationSingleViaMassLinkcheck(link, account);
        } else {
            return requestFileInformationAPISingle(link, account);
        }
    }

    @Deprecated
    private AvailableStatus requestFileInformationAPISingle(final DownloadLink link, final Account account) throws Exception {
        final String fileId = this.getFID(link);
        final GetRequest req = br.createGetRequest(API_BASE + "/file?fileId=" + fileId);
        final Map<String, Object> resp = (Map<String, Object>) this.callAPI(req, null, link);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String cryptedFileName = resp.get("fileName").toString();
        final String fileHash = resp.get("fileHash").toString();
        final String downloadUrl = resp.get("downloadUrl").toString();
        link.setVerifiedFileSize(((Number) resp.get("fileSize")).longValue() * 1024);
        /* Set final filename here because Content-Disposition header contains crypted filename. */
        try {
            final String decryptedFileName = decryptFileName(cryptedFileName, link.getPluginPatternMatcher());
            link.setFinalFileName(decryptedFileName);
            link.setProperty("decryptedFileName", decryptedFileName);
        } catch (final Exception e) {
            logger.info("Invalid key");
            link.setName(cryptedFileName);
        }
        link.setMD5Hash(fileHash);
        link.setProperty("dl3", downloadUrl);
        // link.setProperty("hashedFileName", fileName);
        if (Boolean.TRUE.equals(resp.get("hasCaptcha"))) {
            link.setProperty("hasCatpcha", true);
        } else {
            link.removeProperty("hasCatpcha");
        }
        return AvailableStatus.TRUE;
    }

    private AvailableStatus requestFileInformationSingleViaMassLinkcheck(final DownloadLink link, final Account account) throws Exception {
        checkLinks(new DownloadLink[] { link });
        if (!link.isAvailabilityStatusChecked()) {
            return AvailableStatus.UNCHECKED;
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    private boolean isValidFileID(final String str) {
        if (str.replace("-", "").matches("[a-f0-9]{32}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account);
        String dlUrl = link.getProperty("dl3").toString();
        dlUrl = URLHelper.parseLocation(new URL(dlUrl), "&secret=" + Encoding.urlEncode(this.getApikey(account)));
        handleDownload(link, account, dlUrl);
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog();
        }
        requestFileInformation(link);
        final String dlUrl = link.getStringProperty("dl3");
        handleDownload(link, null, dlUrl);
    }

    private String decryptFileName(String encryptedFileName, final String sourcelink) throws IllegalBlockSizeException, BadPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, NoSuchAlgorithmException, NoSuchPaddingException, UnsupportedEncodingException, MalformedURLException {
        final UrlQuery query = UrlQuery.parse(sourcelink);
        final String key = query.get("key");
        final String counterFileName = query.get("counterFileName");
        final byte[] decodedFileNameIv = Base64.decode(URLEncode.decodeURIComponent(counterFileName));
        final byte[] decoded = Base64.decode(URLEncode.decodeURIComponent(key));
        final String decryptedFileName = decryptFileName(encryptedFileName, decoded, decodedFileNameIv);
        return decryptedFileName;
    }

    private String decryptFileName(String encryptedFileName, byte[] keyData, byte[] iv) throws IllegalBlockSizeException, BadPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, NoSuchAlgorithmException, NoSuchPaddingException, UnsupportedEncodingException {
        SecretKeySpec secretKeySpec = new SecretKeySpec(keyData, "AES");
        IvParameterSpec ivParameterSpec = new IvParameterSpec(iv);
        Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding");
        cipher.init(Cipher.DECRYPT_MODE, secretKeySpec, ivParameterSpec);
        byte[] decryptedBytes = cipher.doFinal(Base64.decode(encryptedFileName));
        return new String(decryptedBytes, "UTF-8");
    }

    private void decryptFile(String encryptedFilePath, String decryptedFilePath, byte[] keyData, ArrayList<byte[]> iv, String decryptedFileName) throws Exception {
        FileInputStream encryptedFileInputStream = new FileInputStream(encryptedFilePath);
        FileOutputStream decryptedFileOutputStream = new FileOutputStream(decryptedFilePath);
        SecretKey secretKey = new SecretKeySpec(keyData, "AES");
        for (int i = 0; i < iv.size(); i++) {
            Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding");
            IvParameterSpec ivSpec = new IvParameterSpec(iv.get(i));
            cipher.init(Cipher.DECRYPT_MODE, secretKey, ivSpec);
            byte[] buffer = new byte[10485760]; // 10mb
            int bytesRead;
            int count = 0;
            while ((bytesRead = encryptedFileInputStream.read(buffer)) != -1) {
                byte[] decryptedBytes = cipher.update(buffer, 0, bytesRead);
                decryptedFileOutputStream.write(decryptedBytes);
                if (count == 9) {
                    break;
                }
                count++;
            }
            byte[] finalDecryptedBytes = cipher.doFinal();
            decryptedFileOutputStream.write(finalDecryptedBytes);
        }
        encryptedFileInputStream.close();
        decryptedFileOutputStream.close();
    }

    private void handleDownload(final DownloadLink link, final Account account, final String dlUrl) throws Exception, PluginException {
        if (StringUtils.isEmpty(dlUrl)) {
            /* Programmer mistake */
            throw new IllegalArgumentException();
        }
        final String decryptedFilename = link.getStringProperty("decryptedFileName");
        if (decryptedFilename == null) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Decryption key invalid/missing");
        }
        if (account != null) {
            br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, this.getApikey(account));
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dlUrl, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Got unexpected non-file content");
        }
        if (dl.startDownload()) {
            /* Decrypt downloaded file if download was successful. */
            logger.info("Decrypting file...");
            // TODO: Add try-catch, progress and errorhandling
            final String key = UrlQuery.parse(link.getPluginPatternMatcher()).get("key");
            String downloadPath = dl.getDownloadable().getFileOutput();
            String destinationPath = (new File(downloadPath)).getParent();
            destinationPath = destinationPath + "\\" + decryptedFilename;
            final UrlQuery query = UrlQuery.parse(link.getPluginPatternMatcher());
            final String countersString = Encoding.htmlDecode(query.get("counters"));
            String[] test = countersString.split(",");
            ArrayList<byte[]> decodedIvs = new ArrayList<byte[]>();
            for (int i = 0; i < test.length; i++) {
                decodedIvs.add(Base64.decode(test[i]));
            }
            byte[] decoded = Base64.decode(URLEncode.decodeURIComponent(key));
            decryptFile(downloadPath, destinationPath, decoded, decodedIvs, link.getStringProperty("decryptedFileName"));
            final File tempFile = new File(downloadPath);
            tempFile.delete();
        }
    }

    private Map<String, Object> loginAPI(final Account account, boolean verifySession) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final String apikey = account.getPass();
            if (StringUtils.isEmpty(apikey)) {
                throw new AccountInvalidException();
            } else if (!verifySession) {
                logger.info("Trust apikey without verification");
                return null;
            } else {
                logger.info("Performing full login");
                br.getPage(API_BASE + "/user");
                return (Map<String, Object>) this.callAPI(br.createGetRequest(API_BASE + "/user"), account, null);
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> resp = loginAPI(account, true);
        final AccountInfo ai = new AccountInfo();
        final String premiumUntilStr = PluginJSonUtils.getJson(br, "premiumUntil");
        long premiumUntil = -1;
        if (premiumUntilStr != null) {
            premiumUntil = TimeFormatter.getMilliSeconds(premiumUntilStr, "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.ENGLISH);
        }
        if (premiumUntil == -1 || premiumUntil < System.currentTimeMillis()) {
            ai.setExpired(true);
            return ai;
        }
        ai.setTrafficLeft(((Number) resp.get("dailyDownloadLimit")).longValue() * 1024);
        ai.setTrafficMax(((Number) resp.get("maxDailyDownloadLimit")).longValue() * 1024);
        account.setType(AccountType.PREMIUM);
        ai.setValidUntil(premiumUntil, br);
        final String user = (String) resp.get("secret");
        if (!StringUtils.isEmpty(user)) {
            /* User only enters API key so our "primary key" username is missing -> Set it. */
            account.setUser(user);
        }
        account.setConcurrentUsePossible(true);
        return ai;
    }

    /* API docs: https://api.filedo.net/doc/index.html */
    private Object callAPI(final Request req, final Account account, final DownloadLink link) throws IOException, PluginException, InterruptedException {
        if (account != null) {
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, getApikey(account));
        }
        br.getPage(req);
        if (br.getHttpConnection().getResponseCode() == 401) {
            throw new AccountInvalidException();
        } else {
            return checkErrors(br, account, link);
        }
    }

    private Object checkErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        try {
            final Object respO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (!(respO instanceof Map)) {
                return respO;
            }
            final Map<String, Object> resp = (Map<String, Object>) respO;
            final String errormsg = (String) resp.get("message");
            if (errormsg == null) {
                /* No error */
                return resp;
            } else if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg);
            } else {
                throw new AccountUnavailableException(errormsg, 1 * 60 * 1000l);
            }
        } catch (final JSonMapperException jme) {
            final String errortext = "Bad API response";
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errortext, jme);
            } else {
                throw new AccountUnavailableException(jme, errortext, 1 * 60 * 1000l);
            }
        }
    }

    private String getApikey(final Account account) {
        return account.getPass();
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/user-info";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.replace("-", "").trim().matches("[a-f0-9]{32}")) {
            // TODO: Add proper validation
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 6;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        if (acc != null && AccountType.PREMIUM.equals(acc.getType())) {
            /* Premium accounts don't have captchas */
            return false;
        } else {
            return link.getBooleanProperty("hasCaptcha", false);
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}