//    jDownloader - Downloadmanager
//    Copyright (C) 2013  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.util.Map;
import java.util.Random;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.ChoMikujPlFolder;

@HostPlugin(revision = "$Revision: 51111 $", interfaceVersion = 3, names = { "chomikuj.pl" }, urls = { "https?://chomikujdecrypted\\.pl/.*?,\\d+$" })
public class ChoMikujPl extends PluginForHost {
    /* Plugin settings */
    public static final String   CRAWL_SUBFOLDERS                                             = "CRAWL_SUBFOLDERS";
    public static final boolean  default_CRAWL_SUBFOLDERS                                     = true;
    private static final String  ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES         = "ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES";
    private static final boolean default_ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES = false;
    private static final String  ALLOW_STREAM_DOWNLOAD_AS_FALLBACK                            = "ALLOW_STREAM_DOWNLOAD_AS_FALLBACK";
    private static final boolean default_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK                    = true;
    private static final String  IGNORE_TRAFFIC_LIMIT                                         = "IGNORE_TRAFFIC_LIMIT";
    private static final boolean default_IGNORE_TRAFFIC_LIMIT                                 = false;
    /* DownloadLink properties */
    private final String         PROPERTY_DOWNLOADLINK_ADULT_CONTENT                          = "adult_content";
    private final String         PROPERTY_DOWNLOADLINK_OWNED_BY_USERNAME                      = "owned_by_username";
    private final String         PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE                 = "stream_download_active";

    public ChoMikujPl(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/Create.aspx");
        setConfigElements();
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/Regulamin.aspx";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setAllowedResponseCodes(new int[] { 500 });
        br.setFollowRedirects(true);
        return br;
    }

    @SuppressWarnings("deprecation")
    public void correctDownloadLink(DownloadLink link) {
        link.setUrlDownload(link.getDownloadURL().replace("chomikujdecrypted.pl/", "chomikuj.pl/"));
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

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null) {
            /* Free Account */
            return true;
        } else if (link.hasProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE)) {
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (account != null) {
            return 0;
        } else if (link.hasProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE)) {
            /* No chunk limit for stream download */
            return 0;
        } else {
            return 1;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        /*
         * Always try to login even if this is not called from handlePremium as some content is only available when user is logged in which
         * he usually isn't during availablecheck and we do not want to have that content displayed as offline!
         */
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account);
    }

    private String getMainlink(final DownloadLink link) {
        final String mainlink = link.getStringProperty(ChoMikujPlFolder.PROPERTY_MAINLINK);
        if (mainlink != null) {
            return mainlink;
        } else {
            return link.getContentUrl();
        }
    }

    @Override
    public String buildExternalDownloadURL(final DownloadLink link, final PluginForHost buildForThisPlugin) {
        final String mainlink = getMainlink(link);
        if (mainlink != null) {
            return mainlink;
        } else {
            return super.buildExternalDownloadURL(link, buildForThisPlugin);
        }
    }

    /** Returns true if we know that the given file is owned by the given account. */
    private boolean isOwnedBy(final DownloadLink link, final Account account) {
        if (account == null) {
            return false;
        }
        final String ownedByUsername = link.getStringProperty(PROPERTY_DOWNLOADLINK_OWNED_BY_USERNAME);
        if (ownedByUsername == null) {
            return false;
        }
        if (ownedByUsername.equals(account.getUser())) {
            return true;
        }
        return false;
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        final String fid = getFID(link);
        final String mainlink = getMainlink(link);
        if (fid == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (account != null) {
            this.login(account, false);
        }
        if (mainlink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Try to find better filename - usually only needed for single links. */
        br.getPage(mainlink);
        if (this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD) {
            this.passwordHandling(link, account);
        }
        if (br.getHttpConnection().getResponseCode() == 400) {
            // TODO: Implement workaround
            logger.warning("Server side error 400 happened -> Attempting workaround");
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                return AvailableStatus.UNCHECKABLE;
            }
        }
        if (br.getHttpConnection() != null && br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (requiresPasswordPrompt(br)) {
            logger.info("Cannot fetch file information because password is required");
            return AvailableStatus.TRUE;
        }
        if (!br.containsHTML(fid)) {
            /* html must contain fileid - if not, content is assumed to be offline (e.g. redirect to upper folder or errorpage) */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filesize = br.getRegex("<p class=\"fileSize\">([^<>\"]*?)</p>").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize.replace(",", ".")));
        }
        String filename = br.getRegex("property=\"og:title\" content=\"([^<>\"]*?)\"").getMatch(0);
        if (filename != null) {
            logger.info("Found html filename for single link");
            filename = Encoding.htmlDecode(filename).trim();
            link.setFinalFileName(filename);
        } else {
            logger.info("Failed to find html filename for single link");
        }
        return AvailableStatus.TRUE;
    }

    /**
     * Alternative way to check files from chomikuj.pl. <br>
     * Account required for this to also fine the file size. <br>
     * This does not find the file name!!
     */
    private AvailableStatus requestFileInformation2(final DownloadLink link, final Account account) throws Exception {
        final String fid = this.getFID(link);
        br.getPage("https://" + getHost() + "/action/fileDetails/Index/" + fid);
        final String filesize = br.getRegex("<p class=\"fileSize\">([^<>\"]*?)</p>").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize.replace(",", ".")));
        } else {
            logger.warning("Failed to find filesize");
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "fileDetails/Unavailable")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "/Error403.aspx")) {
            logger.info("File is online but only downloadable via account");
            if (this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD) {
                throw new AccountRequiredException();
            }
            // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        final String remainingTraffic = br.getRegex("<strong>([^<>\"]*?)</strong>\\s*transferu").getMatch(0);
        if (remainingTraffic != null) {
            if (this.getPluginConfig().getBooleanProperty(ChoMikujPl.IGNORE_TRAFFIC_LIMIT, default_IGNORE_TRAFFIC_LIMIT) || this.getPluginConfig().getBooleanProperty(ChoMikujPl.ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES, default_ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES)) {
                /*
                 * Uploaders can always download their OWN files no matter how much traffic they have left and downloading streams does not
                 * use up any traffic.
                 */
                ai.setSpecialTraffic(true);
                ai.setStatus("Account without traffic limitation (limitation disabled by user)");
            } else {
                ai.setSpecialTraffic(false);
                ai.setStatus("Account with traffic limitation");
            }
            final long hardcodedDailyFreeLimit = SizeFormatter.getSize("50MB");
            ai.setTrafficLeft(SizeFormatter.getSize(remainingTraffic.replace(",", ".")));
            /*
             * Most users will use free accounts with a daily limit of max 50 MB so let's just display that as max. traffic for all users
             * who have less- exactly 50MB left.
             */
            if (ai.getTrafficLeft() <= hardcodedDailyFreeLimit) {
                ai.setTrafficMax(hardcodedDailyFreeLimit);
            }
        } else {
            /*
             * 2019-07-16: Not sure if that is a good idea but at the moment we're handling all accounts as premium and set unlimited
             * traffic if we don't find any ...
             */
            ai.setStatus("Account without traffic limitation");
            ai.setUnlimitedTraffic();
        }
        account.setType(AccountType.PREMIUM);
        /* 2019-07-16: Points can be converted to traffic but for us they're not important */
        final String collectedPointsStr = br.getRegex("title=\"Punkty\"[^<>]*?><strong>\\s*(\\d+)\\s*</strong>").getMatch(0);
        if (collectedPointsStr != null) {
            ai.setPremiumPoints(collectedPointsStr);
        }
        return ai;
    }

    private static Object PWLOCK = new Object();

    private void passwordHandling(final DownloadLink link, final Account account) throws Exception {
        final Object thislock;
        if (account != null) {
            thislock = account;
        } else {
            thislock = PWLOCK;
        }
        final PluginForDecrypt plg = this.getNewPluginForDecryptInstance(this.getHost());
        synchronized (thislock) {
            ((ChoMikujPlFolder) plg).passwordHandling(link);
        }
    }

    /** Returns true if some kind of folder password needs to be entered into a Form according to given browsers' HTML code. */
    private static boolean requiresPasswordPrompt(final Browser br) {
        if (ChoMikujPlFolder.isFolderPasswordProtected(br)) {
            return true;
        } else if (ChoMikujPlFolder.isSpecialUserPasswordProtected(br)) {
            return true;
        } else {
            return false;
        }
    }

    private boolean looksLikeAudio(final DownloadLink link) {
        return StringUtils.endsWithCaseInsensitive(link.getName(), ".mp3");
    }

    private boolean looksLikeVideo(final DownloadLink link) {
        final String filename = link.getName();
        if (filename == null) {
            return false;
        } else if (!filename.contains(".")) {
            return false;
        }
        final String[] video_exts = new String[] { "avi", "flv", "mp4", "mpg", "rmvb", "divx", "wmv", "mkv" };
        for (final String video_ext : video_exts) {
            if (StringUtils.endsWithCaseInsensitive(filename, "." + video_ext)) {
                return true;
            }
        }
        return false;
    }

    /** Returns true if we can expect this file to be streamable via audio-/video player. */
    private boolean looksLikeStreamableFile(final DownloadLink link) {
        return looksLikeAudio(link) || looksLikeVideo(link);
    }

    private boolean allowStreamDownloadFallback() {
        return this.getPluginConfig().getBooleanProperty(ALLOW_STREAM_DOWNLOAD_AS_FALLBACK, default_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK);
    }

    /**
     * Unified method to get download link for both free and premium users
     *
     * @param link
     *            The DownloadLink to process
     * @param account
     *            The account to use (can be null for free users)
     * @return The download link
     * @throws Exception
     *             If an error occurs
     */
    private String getDllink(final DownloadLink link, final Account account) throws Exception {
        link.removeProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE);
        final boolean isAudio = looksLikeAudio(link);
        final boolean isVideo = looksLikeVideo(link);
        final boolean adultContentBlocked = br.containsHTML("\"FormAdultViewAccepted\"");
        final boolean allowStreamDownloadFallback = this.allowStreamDownloadFallback();
        if (account == null) {
            if (adultContentBlocked) {
                link.setProperty(PROPERTY_DOWNLOADLINK_ADULT_CONTENT, true);
            } else {
                /* Account required for original file download of adult content but stream can be downloaded without account. */
                link.removeProperty(PROPERTY_DOWNLOADLINK_ADULT_CONTENT);
            }
        } else {
            /*
             * Users with account shall always be able to view adult content this when logged in it shall be impossible to determine whether
             * or not the current link leads to adult content.
             */
            if (adultContentBlocked) {
                /* This should never happen! */
                logger.warning("User is [supposed to be] logged in but we still got blocked adult content!");
            }
        }
        String downloadurl = null;
        final String fid = getFID(link);
        br.getHeaders().put("Accept", "*/*");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        // Check if account wants to force MP3 stream download
        if (account != null && isAudio && this.getPluginConfig().getBooleanProperty(ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES, default_ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES)) {
            /* User prefers stream download for .mp3 files --> Does not use up any account traffic. */
            link.setProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE, true);
            return getDllinkAudioStream(link);
        }
        // Account users check for traffic (if traffic checking is enabled)
        final boolean accountHasLessTrafficThanRequiredForThisFile = account != null && account.getAccountInfo() != null && !account.getAccountInfo().isSpecialTraffic() && account.getAccountInfo().getTrafficLeft() < link.getView().getBytesTotal();
        // Perform a special file info check for account users
        if (account != null) {
            final Browser brc = br.cloneBrowser();
            brc.getPage("https://" + getHost() + "/action/fileDetails/Index/" + fid);
            final String filesize = brc.getRegex("<p class=\"fileSize\">([^<>\"]*?)</p>").getMatch(0);
            if (filesize != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesize.replace(",", ".")));
            }
            if (StringUtils.containsIgnoreCase(brc.getURL(), "fileDetails/Unavailable")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (accountHasLessTrafficThanRequiredForThisFile) {
                /* Users can override traffic check. For this case we'll check if we have enough traffic for this file here. */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Not enough traffic to download this file", 15 * 60 * 1000l);
            }
        }
        // Get the verification token
        final String requestVerificationToken = br.getRegex("<div id=\"content\">\\s*?<input name=\"__RequestVerificationToken\" type=\"hidden\" value=\"([^<>\"]*?)\"").getMatch(0);
        if (requestVerificationToken == null) {
            logger.warning("Failed to find requestVerificationToken");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        // Set cookies and make initial request
        br.setCookie(getHost(), "cookiesAccepted", "1");
        if (account != null) {
            br.setCookie(getHost(), "__RequestVerificationToken_Lw__", requestVerificationToken);
            br.getHeaders().put("Referer", link.getPluginPatternMatcher());
        }
        final Browser content_br = br.cloneBrowser();
        br.postPage("https://" + getHost() + "/action/License/DownloadContext", "FileId=" + fid + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
        Map<String, Object> entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        String content = (String) entries.get("Content");
        content_br.getRequest().setHtmlCode(content);
        // Check for access denied
        final String accessDenied = "Nie masz w tej chwili uprawnień do tego pliku lub dostęp do niego nie jest w tej chwili możliwy z innych powodów";
        if (StringUtils.containsIgnoreCase(content, accessDenied)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form form_DownloadWarningAccept = content_br.getFormbyAction("/action/License/DownloadWarningAccept");
        if (form_DownloadWarningAccept != null) {
            form_DownloadWarningAccept.put("__RequestVerificationToken", Encoding.urlEncode(requestVerificationToken));
            br.submitForm(form_DownloadWarningAccept);
            entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            content = (String) entries.get("Content");
            content_br.getRequest().setHtmlCode(content);
        }
        if (account != null) {
            // Handle "don't show box" dialog
            downloadurl = content_br.getRegex("href=\"(https?://[^\"]+)\"[^>]*class=\"downloadFileWithDM").getMatch(0);
            if (downloadurl != null) {
                /**
                 * Download of self-owned files: E.g. Ten plik pobierasz całkowicie za darmo, ponieważ został on przez Ciebie dodany do
                 * Chomika <br>
                 * -> This means "You can download this file completely free of charge because you own it"
                 */
                logger.info("Found direct-downloadurl for self-owned file: " + downloadurl);
                link.setProperty(PROPERTY_DOWNLOADLINK_OWNED_BY_USERNAME, account.getUser());
                return downloadurl;
            }
            link.removeProperty(PROPERTY_DOWNLOADLINK_OWNED_BY_USERNAME);
            if (content_br.containsHTML("dontShowBoxInSession")) {
                /* TODO: Check if this is still needed */
                br.postPage("/action/chomikbox/DontDownloadWithBox", "__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
                br.postPage("/action/License/Download", "FileId=" + fid + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
                entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                content = (String) entries.get("Content");
                content_br.getRequest().setHtmlCode(content);
            }
            // Handle large transfer acceptance
            final Form form_acceptLargeTransfer = content_br.getFormbyAction("/action/License/acceptLargeTransfer");
            final Form form_AcceptOwnTransfer = content_br.getFormbyAction("/action/License/AcceptOwnTransfer");
            if (form_acceptLargeTransfer != null) {
                /* 2025-04-28: This still exists */
                form_acceptLargeTransfer.put("__RequestVerificationToken", Encoding.urlEncode(requestVerificationToken));
                br.submitForm(form_acceptLargeTransfer);
                entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                content = (String) entries.get("Content");
                content_br.getRequest().setHtmlCode(content);
            } else if (form_AcceptOwnTransfer != null) {
                form_AcceptOwnTransfer.put("__RequestVerificationToken", Encoding.urlEncode(requestVerificationToken));
                br.submitForm(form_AcceptOwnTransfer);
                entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                content = (String) entries.get("Content");
                content_br.getRequest().setHtmlCode(content);
            }
        } else {
            // Handle captcha for free users
            final Form form_DownloadNotLoggedCaptchaEntered = content_br.getFormbyAction("/action/License/DownloadNotLoggedCaptchaEntered");
            final Form form_Download = content_br.getFormbyAction("/action/License/Download");
            if (form_DownloadNotLoggedCaptchaEntered != null) {
                logger.info("Download captcha required");
                form_DownloadNotLoggedCaptchaEntered.put("__RequestVerificationToken", Encoding.urlEncode(requestVerificationToken));
                final String rcSiteKey = content_br.getRegex("'sitekey':\\s*'([^']+)").getMatch(0);
                if (rcSiteKey == null) {
                    /* Plugin broken or premium only content */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* Handle captcha */
                logger.info("Handling captcha");
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, rcSiteKey).getToken();
                form_DownloadNotLoggedCaptchaEntered.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                br.submitForm(form_DownloadNotLoggedCaptchaEntered);
                entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                content = (String) entries.get("Content");
                content_br.getRequest().setHtmlCode(content);
            } else if (form_Download != null) {
                form_Download.put("__RequestVerificationToken", Encoding.urlEncode(requestVerificationToken));
                br.submitForm(form_Download);
                entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                content = (String) entries.get("Content");
                content_br.getRequest().setHtmlCode(content);
            }
            if (content_br.containsHTML("(Aby pobrać ten plik, musisz być zalogowany lub wysłać jeden SMS\\.|Właściciel tego chomika udostępnia swój transfer, ale nie ma go już w wystarczającej|wymaga opłacenia kosztów transferu z serwerów Chomikuj\\.pl)")) {
                throw new AccountRequiredException();
            } else if (StringUtils.containsIgnoreCase(content, accessDenied)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        // Extract the download URL from the response
        downloadurl = (String) entries.get("redirectUrl");
        if (!StringUtils.isEmpty(downloadurl)) {
            logger.info("Found official downloadurl");
            return downloadurl;
        }
        logger.info("Account with sufficient balance required for official download -> Checking if stream download fallback is possible");
        String adultContentPrefix = "";
        if (adultContentBlocked) {
            adultContentPrefix = "[Adult content] ";
        }
        if (isAudio || isVideo) {
            /* Fall back to stream download if possible && allowed */
            if (!allowStreamDownloadFallback) {
                if (account != null) {
                    throw new AccountRequiredException("Buy account balance or allow stream download fallback in plugin settings to download this file.");
                } else {
                    throw new AccountRequiredException(adultContentPrefix + "Add an account or allow stream download fallback in plugin settings to download this file.");
                }
            }
            /* Stream download */
            if (isVideo) {
                /* Download video stream (free download) */
                logger.info("Attempting to download MP4 stream");
                downloadurl = getDllinkVideoStream(link);
            } else {
                /* Download mp3 stream */
                logger.info("Attempting to download MP3 stream");
                downloadurl = getDllinkAudioStream(link);
            }
            /* Reset verifiedFilesize if it was set because file size of stream download may differ from previously set verifiedFilesize. */
            link.setVerifiedFileSize(-1);
            link.setProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE, true);
            return downloadurl;
        }
        /* Failure -> Try to find out why a download of this file was impoossible. */
        if (StringUtils.containsIgnoreCase(content, "\"BuyAdditionalTransfer")) {
            /* E.g. Próbujesz pobrać plik o rozmiarze 103,06 MB. Każdy plik powyżej 1 MB wymaga opłacenia kosztów trasferu. */
            if (account != null) {
                throw new AccountUnavailableException("Not enough traffic available", 5 * 60 * 1000);
            } else {
                throw new AccountRequiredException();
            }
        }
        if (adultContentBlocked) {
            throw new AccountRequiredException("Account required to download adult content");
        }
        if (accountHasLessTrafficThanRequiredForThisFile) {
            throw new AccountUnavailableException("Not enough traffic available", 5 * 60 * 1000);
        }
        if (StringUtils.containsIgnoreCase(downloadurl, "#SliderTransfer")) {
            throw new AccountUnavailableException("Traffic limit reached", 5 * 60 * 1000);
        }
        /* Dead end */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private String getDllinkAudioStream(final DownloadLink link) throws Exception {
        final String fid = getFID(link);
        final String dllink = "https://" + this.getHost() + "/Audio.ashx?id=" + fid + "&type=2&tp=mp3";
        return dllink;
    }

    private String getDllinkVideoStream(final DownloadLink link) throws Exception {
        final String fid = getFID(link);
        final String dllink = "https://" + this.getHost() + "/Video.ashx?id=" + fid + "&type=1&ts=" + new Random().nextInt(1000000000) + "&file=video&start=0";
        return dllink;
    }

    private String getFID(final DownloadLink dl) {
        return dl.getStringProperty(ChoMikujPlFolder.PROPERTY_FILEID);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = getDirectlinkProperty(account);
        final String storedDirecturl = this.getStoredDirectlink(link, account);
        String dllink = null;
        if (storedDirecturl != null) {
            logger.info("Using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            logger.info("Obtaining fresh directurl");
            requestFileInformation(link, account);
            dllink = this.getDllink(link, account);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                logger.warning("The final dllink seems not to be a file!");
                if (StringUtils.containsIgnoreCase(br.getURL(), "Error.aspx")) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error", 5 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        } catch (final InterruptedException e) {
            throw e;
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dllink);
        }
        dl.startDownload();
    }

    private String getDirectlinkProperty(final Account account) {
        if (account != null) {
            return "dllink_account";
        } else {
            return "dllink_free";
        }
    }

    private String getStoredDirectlink(final DownloadLink link, final Account account) {
        final String directlinkproperty = getDirectlinkProperty(account);
        final String dllink = link.getStringProperty(directlinkproperty);
        return dllink;
    }

    public void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String url_path_user_profile = "/" + Encoding.urlEncode(account.getUser());
            if (cookies != null) {
                br.setCookies(cookies);
                if (!force) {
                    /* Do not verify cookies */
                    return;
                }
                br.getPage("https://" + getHost() + url_path_user_profile);
                if (this.isLoggedInHTML(br)) {
                    logger.info("Successfully loggedin via cookies");
                    /* Save new cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    logger.info("Failed to login via cookies");
                }
            }
            logger.info("Performing full login");
            br.clearCookies(null);
            br.setCookiesExclusive(true);
            br.getPage("https://" + getHost());
            // final Form loginform = br.getFormByRegex("loginDummy");
            // if (loginform == null) {
            // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            // }
            // loginform.setAction("/action/Login/TopBarLogin");
            // loginform.put("Login", Encoding.urlEncode(account.getUser()));
            // loginform.put("Password", Encoding.urlEncode(account.getPass()));
            // br.submitForm(loginform);
            String postData = "ReturnUrl=&Login=" + Encoding.urlEncode(account.getUser()) + "&Password=" + Encoding.urlEncode(account.getPass());
            final String[] requestVerificationTokens = br.getRegex("<input name=\"__RequestVerificationToken\" type=\"hidden\" value=\"([^<>\"\\']+)\"").getColumn(0);
            if (requestVerificationTokens.length > 0) {
                logger.info("Found " + requestVerificationTokens.length + "x '__RequestVerificationToken' values");
                /*
                 * 2019-10-17: Strange - website contains this value twice (well different values, same key) and uses them in login POST
                 * data. According to my tests, login works even without these tokens or with them set to "".
                 */
                for (final String requestVerificationToken : requestVerificationTokens) {
                    postData += "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken);
                }
            } else {
                logger.info("Failed to find any '__RequestVerificationToken' - trying to login without it");
            }
            final PostRequest postRequest = br.createPostRequest("/action/Login/TopBarLogin", postData);
            postRequest.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            br.getPage(postRequest);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            /* E.g. success: {"IsSuccess":true,"Message":null,"Data":{"LoggedIn":true},"Type":3} */
            if (!Boolean.TRUE.equals(entries.get("IsSuccess"))) {
                throw new AccountInvalidException((String) entries.get("Message"));
            } else if (!this.isLoggedInCookie(br)) {
                throw new AccountInvalidException();
            }
            /* Double-check */
            br.getPage(url_path_user_profile);
            if (!isLoggedInHTML(br)) {
                throw new AccountInvalidException();
            }
            br.setCookie(br.getHost(), "cookiesAccepted", "1");
            br.setCookie(br.getHost(), "spt", "0");
            br.setCookie(br.getHost(), "rcid", "1");
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        if (this.getStoredDirectlink(link, acc) != null) {
            /* Stored directurl available -> Assume that no captcha will be needed. */
            return false;
        } else if (acc == null && (!this.allowStreamDownloadFallback() || !this.looksLikeStreamableFile(link))) {
            /**
             * Captcha required for <br>
             * : - Original file downloads without account
             */
            return true;
        } else {
            /**
             * Captcha not required for: <br>
             * - Downloads with account <br>
             * - Stream downloads
             */
            return false;
        }
    }

    /** Checks for presence of logged-in related html. */
    private boolean isLoggedInHTML(final Browser br) {
        return br.containsHTML("/Login/LogOut\"");
    }

    /** Checks for presence of logged-in cookie. */
    private boolean isLoggedInCookie(final Browser br) {
        return br.getCookie("https://" + getHost() + "/", "RememberMe", Cookies.NOTDELETEDPATTERN) != null;
    }

    @Override
    public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
        if (this.isOwnedBy(link, account)) {
            /* Current account owns that file -> Can download it without traffic deduction. */
            return true;
        } else if (this.getStoredDirectlink(link, account) != null) {
            /* Stored directurl available -> Assume that it is still valid so it can be used without using up additional traffic. */
            return true;
        } else if (link.hasProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE)) {
            /* Stream download will not deduct traffic. */
            return true;
        } else if (this.allowStreamDownloadFallback() && this.looksLikeStreamableFile(link)) {
            /* Stream download will happen as fallback -> Stream download will not deduct traffic. */
            return true;
        } else {
            return super.enoughTrafficFor(link, account);
        }
    }

    @Override
    public void update(final DownloadLink downloadLink, final Account account, final long bytesTransfered) {
        if (this.isOwnedBy(downloadLink, account)) {
            /* Do not deduct traffic for downloads of self-owned items. */
            return;
        } else {
            super.update(downloadLink, account, bytesTransfered);
        }
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
        link.removeProperty(PROPERTY_DOWNLOADLINK_ADULT_CONTENT);
        link.removeProperty(PROPERTY_DOWNLOADLINK_STREAM_DOWNLOAD_ACTIVE);
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES, "Account download: Prefer download of stream versions of audio files in account mode?\r\n<html><b>Avoids traffic usage for audio files!</b></html>").setDefaultValue(default_ACCOUNT_DOWNLOAD_AVOID_TRAFFIC_USAGE_FOR_AUDIO_FILES));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.ALLOW_STREAM_DOWNLOAD_AS_FALLBACK, "Allow fallback to preview/stream download if original file is only downloadable with (paid) account?").setDefaultValue(default_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.CRAWL_SUBFOLDERS, "Crawl subfolders in folders").setDefaultValue(default_CRAWL_SUBFOLDERS));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.IGNORE_TRAFFIC_LIMIT, "Ignore traffic limit? Useful to download self uploaded files or stream download in account mode.").setDefaultValue(default_IGNORE_TRAFFIC_LIMIT));
    }
}