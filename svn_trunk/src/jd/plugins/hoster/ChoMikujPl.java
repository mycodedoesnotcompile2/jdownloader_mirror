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

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.Random;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.antiDDoSForHost;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.PostRequest;
import jd.nutils.JDHash;
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
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.decrypter.ChoMikujPlFolder;

@HostPlugin(revision = "$Revision: 51020 $", interfaceVersion = 3, names = { "chomikuj.pl" }, urls = { "https?://chomikujdecrypted\\.pl/.*?,\\d+$" })
public class ChoMikujPl extends antiDDoSForHost {
    private String               dllink                                                = null;
    private static final String  ACCESSDENIED                                          = "Nie masz w tej chwili uprawnień do tego pliku lub dostęp do niego nie jest w tej chwili możliwy z innych powodów\\.";
    private static final String  MAINPAGE                                              = "https://chomikuj.pl/";
    /* Plugin settings */
    public static final String   DECRYPTFOLDERS                                        = "DECRYPTFOLDERS";
    private static final String  AVOIDPREMIUMMP3TRAFFICUSAGE                           = "AVOIDPREMIUMMP3TRAFFICUSAGE";
    private static final boolean default_AVOIDPREMIUMMP3TRAFFICUSAGE                   = false;
    private static final String  FREE_ANONYMOUS_MODE_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK = "FREE_ANONYMOUS_MODE_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK";
    private static final String  IGNORE_TRAFFIC_LIMIT                                  = "IGNORE_TRAFFIC_LIMIT";
    private Browser              cbr                                                   = null;
    private boolean              adultContent                                          = false;

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
    protected Browser prepBrowser(final Browser prepBr, final String host) {
        if (!(browserPrepped.containsKey(prepBr) && browserPrepped.get(prepBr) == Boolean.TRUE)) {
            super.prepBrowser(prepBr, host);
            /* define custom browser headers and language settings */
            prepBr.setAllowedResponseCodes(new int[] { 500 });
        }
        return prepBr;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
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
        return requestFileInformation(link, account, false);
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

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        adultContent = false;
        dllink = null;
        final String fid = getFID(link);
        final String mainlink = getMainlink(link);
        if (fid == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (account != null) {
            this.login(account, false);
        }
        if (mainlink != null) {
            /* Try to find better filename - usually only needed for single links. */
            getPage(mainlink);
            if (isDownload) {
                this.passwordHandling(link, account);
            }
            if (br.getHttpConnection() != null && br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (requiresPasswordPrompt(br)) {
                logger.info("Cannot fetch file information because password is required");
                return AvailableStatus.TRUE;
            } else if (!br.containsHTML(fid)) {
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
            adultContent = br.containsHTML("\"FormAdultViewAccepted\"");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        final String remainingTraffic = br.getRegex("<strong>([^<>\"]*?)</strong>\\s*transferu").getMatch(0);
        if (remainingTraffic != null) {
            if (this.getPluginConfig().getBooleanProperty(ChoMikujPl.IGNORE_TRAFFIC_LIMIT, false) || this.getPluginConfig().getBooleanProperty(ChoMikujPl.AVOIDPREMIUMMP3TRAFFICUSAGE, default_AVOIDPREMIUMMP3TRAFFICUSAGE)) {
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

    private boolean isVideo(final DownloadLink dl) {
        String filename = dl.getFinalFileName();
        if (filename == null) {
            filename = dl.getName();
        }
        if (filename == null) {
            return false;
        }
        if (!filename.contains(".")) {
            return false;
        }
        final String ext = filename.substring(filename.lastIndexOf("."));
        if (ext.matches("\\.(avi|flv|mp4|mpg|rmvb|divx|wmv|mkv)")) {
            return true;
        }
        return false;
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
        final boolean isPremium = account != null && AccountType.PREMIUM.equals(account.getType());
        final boolean isVideo = isVideo(link);
        final boolean isAudio = StringUtils.endsWithCaseInsensitive(link.getName(), ".mp3");
        String downloadUrl = null;
        final String fid = getFID(link);
        br.getHeaders().put("Accept", "*/*");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        // Check if account wants to force MP3 stream download
        if (isPremium && isAudio && this.getPluginConfig().getBooleanProperty(AVOIDPREMIUMMP3TRAFFICUSAGE, default_AVOIDPREMIUMMP3TRAFFICUSAGE)) {
            /* User wants to force stream download for .mp3 files --> Does not use up any premium traffic. */
            return getDllinkAudioStream(link);
        }
        // Premium users check for traffic (if traffic checking is enabled)
        final boolean accountHasLessTrafficThanRequiredForThisFile = isPremium && !account.getAccountInfo().isSpecialTraffic() && account.getAccountInfo().getTrafficLeft() < link.getView().getBytesTotal();
        String content = null;
        try {
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
            br.setCookie(this.getHost(), "cookiesAccepted", "1");
            if (isPremium) {
                br.setCookie("http://chomikuj.pl/", "__RequestVerificationToken_Lw__", requestVerificationToken);
                br.getHeaders().put("Referer", link.getPluginPatternMatcher());
            }
            postPageWithCleanup(br, "https://" + getHost() + "/action/License/DownloadContext", "FileId=" + fid + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
            Map<String, Object> entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            content = (String) entries.get("Content");
            // Check for access denied
            if (cbr.containsHTML(ACCESSDENIED)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String serializedUserSelection = cbr.getRegex("name=\"SerializedUserSelection\" type=\"hidden\" value=\"([^<>\"]*?)\"").getMatch(0);
            final String serializedOrgFile = cbr.getRegex("name=\"SerializedOrgFile\" type=\"hidden\" value=\"([^<>\"]*?)\"").getMatch(0);
            if (serializedUserSelection == null || serializedOrgFile == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (content.contains("action=\"/action/License/DownloadWarningAccept\"")) {
                /* This step is mostly required */
                postPageWithCleanup(br, "https://chomikuj.pl/action/License/DownloadWarningAccept", "FileId=" + fid + "&SerializedUserSelection=" + Encoding.urlEncode(serializedUserSelection) + "&SerializedOrgFile=" + Encoding.urlEncode(serializedOrgFile) + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
                entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                content = (String) entries.get("Content");
            }
            if (account != null) {
                // Handle "don't show box" dialog
                if (cbr.containsHTML("dontShowBoxInSession")) {
                    /* TODO: Check if this is still needed */
                    postPageWithCleanup(br, "/action/chomikbox/DontDownloadWithBox", "__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
                    postPageWithCleanup(br, "/action/License/Download", "FileId=" + fid + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
                    entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    content = (String) entries.get("Content");
                }
                // Handle large transfer acceptance
                if (cbr.containsHTML("/action/License/acceptLargeTransfer")) {
                    cleanupBrowser(br, PluginJSonUtils.unescape(br.getRequest().getHtmlCode()));
                    logger.info(cbr.getRequest().getHtmlCode());
                    final Form f = cbr.getFormbyAction("/action/License/acceptLargeTransfer");
                    if (f == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    submitFormWithCleanup(br, f);
                } else if (cbr.containsHTML("/action/License/AcceptOwnTransfer")) {
                    cleanupBrowser(cbr, PluginJSonUtils.unescape(br.getRequest().getHtmlCode()));
                    logger.info(cbr.getRequest().getHtmlCode());
                    final Form f = cbr.getFormbyAction("/action/License/AcceptOwnTransfer");
                    if (f == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    f.remove(null);
                    f.remove(null);
                    f.put("__RequestVerificationToken", Encoding.urlEncode(requestVerificationToken));
                    submitFormWithCleanup(br, f);
                }
            } else {
                // Handle captcha for free users
                if (br.containsHTML("g\\-recaptcha")) {
                    // TODO: Check if this is still required
                    final String rcSiteKey = PluginJSonUtils.getJson(PluginJSonUtils.unescape(br.getRequest().getHtmlCode()), "sitekey");
                    if (rcSiteKey == null || serializedUserSelection == null || serializedOrgFile == null) {
                        /* Plugin broken or premium only content */
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    /* Handle captcha */
                    logger.info("Handling captcha");
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, rcSiteKey).getToken();
                    final String postData = "FileId=" + fid + "&SerializedUserSelection=" + Encoding.urlEncode(serializedUserSelection) + "&SerializedOrgFile=" + Encoding.urlEncode(serializedOrgFile) + "&FileName=" + Encoding.urlEncode(link.getName()) + "&g-recaptcha-response=" + Encoding.urlEncode(recaptchaV2Response) + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken);
                    postPageWithCleanup(br, "/action/License/DownloadNotLoggedCaptchaEntered", postData);
                    entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    content = (String) entries.get("Content");
                } else {
                    postPageWithCleanup(br, "/action/License/Download", "FileId=" + fid + "&__RequestVerificationToken=" + Encoding.urlEncode(requestVerificationToken));
                    entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    content = (String) entries.get("Content");
                }
                if (cbr.containsHTML("(Aby pobrać ten plik, musisz być zalogowany lub wysłać jeden SMS\\.|Właściciel tego chomika udostępnia swój transfer, ale nie ma go już w wystarczającej|wymaga opłacenia kosztów transferu z serwerów Chomikuj\\.pl)")) {
                    throw new AccountRequiredException();
                } else if (cbr.containsHTML(ACCESSDENIED)) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
            // Extract the download URL from the response
            downloadUrl = (String) entries.get("redirectUrl");
            if (StringUtils.isEmpty(downloadUrl)) {
                if (StringUtils.containsIgnoreCase(content, "\"BuyAdditionalTransfer")) {
                    /* E.g. Próbujesz pobrać plik o rozmiarze 103,06 MB. Każdy plik powyżej 1 MB wymaga opłacenia kosztów trasferu. */
                    if (account != null) {
                        throw new AccountUnavailableException("Not enough traffic available", 5 * 60 * 1000);
                    } else {
                        throw new AccountRequiredException();
                    }
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
            }
            if (StringUtils.containsIgnoreCase(downloadUrl, "#SliderTransfer")) {
                throw new AccountUnavailableException("Traffic limit reached", 5 * 60 * 1000);
            }
        } catch (final Exception e) {
            if (isPremium) {
                throw e;
            }
            /* Fall back to stream download for free users if possible && allowed */
            if (!isAudio && !isVideo) {
                /* Stream download impossible -> We failed to find a downloadlink */
                // return null;
                /* Check for possible error message in "content" field. */
                if (!StringUtils.isEmpty(content) && content.length() <= 200) {
                    /* Try to display more precise errormessage, avoid plugin defect. */
                    throw new PluginException(LinkStatus.ERROR_FATAL, content);
                } else {
                    throw e;
                }
            }
            logger.info("Failed to get official downloadurl --> Checking if stream download fallback is possible");
            if (!this.getPluginConfig().getBooleanProperty(FREE_ANONYMOUS_MODE_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK, true)) {
                throw new AccountRequiredException("Add a premium account or allow stream download fallback in plugin settings to download this file.");
            }
            /* Stream download */
            if (isVideo) {
                /* Download video stream (free download) */
                logger.info("Attempting to download MP4 stream");
                getPageWithCleanup(br, "https://" + this.getHost() + "/ShowVideo.aspx?id=" + fid);
                if (br.getURL().contains("chomikuj.pl/Error404.aspx") || cbr.containsHTML("(Nie znaleziono|Strona, której szukasz nie została odnaleziona w portalu\\.<|>Sprawdź czy na pewno posługujesz się dobrym adresem)")) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                downloadUrl = getDllinkVideoStream(link);
            } else {
                /* Download mp3 stream */
                logger.info("Attempting to download MP3 stream");
                downloadUrl = getDllinkAudioStream(link);
            }
        }
        return downloadUrl;
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
        dllink = checkDirectLink(link, account);
        if (dllink == null) {
            requestFileInformation(link, account, true);
            if (adultContent) {
                throw new AccountRequiredException("Account required to download adult content");
            }
            this.dllink = this.getDllink(link, account);
            if (StringUtils.isEmpty(this.dllink)) {
                /* 2020-04-20: Lazy handling because most files are premiumonly: Final downloadlink not found = premiumonly */
                throw new AccountRequiredException();
            }
        }
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        boolean resume = this.isResumeable(link, account);
        int maxChunks = this.getMaxChunks(link, account);
        final boolean isAudioStreamDownload = StringUtils.containsIgnoreCase(dllink, "/Audio.ashx");
        final boolean isVideoStreamPreviewDownload = StringUtils.containsIgnoreCase(dllink, "/Preview.ashx");
        if (isAudioStreamDownload || isVideoStreamPreviewDownload) {
            resume = true;
            maxChunks = 0;
        }
        if (!resume) {
            maxChunks = 1;
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resume, maxChunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            logger.warning("The final dllink seems not to be a file!");
            if (StringUtils.containsIgnoreCase(br.getURL(), "Error.aspx")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error", 5 * 60 * 1000l);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (account != null) {
            link.setProperty("dllink_account", dllink);
        } else {
            link.setProperty("dllink_free", dllink);
        }
        dl.startDownload();
    }

    private String checkDirectLink(final DownloadLink link, final Account account) {
        final String directlinkproperty;
        if (account != null) {
            directlinkproperty = "dllink_account";
        } else {
            directlinkproperty = "dllink_free";
        }
        final String dllink = link.getStringProperty(directlinkproperty);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                br2.setFollowRedirects(true);
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    return dllink;
                } else {
                    link.removeProperty(directlinkproperty);
                    return null;
                }
            } catch (final Exception e) {
                logger.log(e);
                link.removeProperty(directlinkproperty);
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    public void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            br.setFollowRedirects(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(cookies);
                if (!force) {
                    /* Do not verify cookies */
                    return;
                }
                getPageWithCleanup(br, MAINPAGE);
                if (this.isLoggedIn(br)) {
                    logger.info("Successfully loggedin via cookies");
                    /* Save new cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    logger.info("Failed to login via cookies");
                }
            }
            logger.info("Performing full login");
            br.clearCookies(account.getHoster());
            prepBrowser(br, account.getHoster());
            br.setCookiesExclusive(true);
            br.setFollowRedirects(true);
            getPageWithCleanup(br, MAINPAGE);
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
            PostRequest postRequest = br.createPostRequest("/action/Login/TopBarLogin", postData);
            postRequest.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            // postPageRawWithCleanup(br, "/action/Login/TopBarLogin",
            // "rememberLogin=true&rememberLogin=false&ReturnUrl=&Login=" + Encoding.urlEncode(account.getUser()) + "&Password=" +
            // Encoding.urlEncode(account.getPass()) + "&__RequestVerificationToken=" +
            // Encoding.urlEncode(requestVerificationToken));
            br.getPage(postRequest);
            if (!isLoggedIn(br)) {
                throw new AccountInvalidException();
            }
            br.setCookie(br.getHost(), "cookiesAccepted", "1");
            br.setCookie(br.getHost(), "spt", "0");
            br.setCookie(br.getHost(), "rcid", "1");
            br.getPage("/" + Encoding.urlEncode(account.getUser()));
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    /** Checks for presence of logged-in cookie. */
    private boolean isLoggedIn(final Browser br) {
        return br.getCookie(MAINPAGE, "RememberMe", Cookies.NOTDELETEDPATTERN) != null;
    }

    /** Performs request and then puts cleaned up html into cbr browser instance. */
    private void getPageWithCleanup(final Browser br, final String url) throws Exception {
        getPage(br, url);
        cbr = br.cloneBrowser();
        cleanupBrowser(cbr, correctBR(br.getRequest().getHtmlCode()));
    }

    /** Performs request and then puts cleaned up html into cbr browser instance. */
    private void postPageWithCleanup(final Browser br, final String url, final String postData) throws Exception {
        postPage(br, url, postData);
        cbr = br.cloneBrowser();
        cleanupBrowser(cbr, correctBR(br.getRequest().getHtmlCode()));
    }

    /** Performs request and then puts cleaned up html into cbr browser instance. */
    private void submitFormWithCleanup(final Browser br, final Form form) throws Exception {
        submitForm(br, form);
        cbr = br.cloneBrowser();
        cleanupBrowser(cbr, correctBR(br.getRequest().getHtmlCode()));
    }

    private String correctBR(final String input) {
        return input.replace("\\", "");
    }

    /**
     * This allows backward compatibility for design flaw in setHtmlCode(), It injects updated html into all browsers that share the same
     * request id. This is needed as request.cloneRequest() was never fully implemented like browser.cloneBrowser().
     *
     * @param ibr
     *            Import Browser
     * @param t
     *            Provided replacement string output browser
     * @author raztoki
     */
    private void cleanupBrowser(final Browser ibr, final String t) throws Exception {
        String dMD5 = JDHash.getMD5(ibr.getRequest().getHtmlCode());
        // preserve valuable original request components.
        final String oURL = ibr.getURL();
        final URLConnectionAdapter con = ibr.getRequest().getHttpConnection();
        Request req = new Request(oURL) {
            {
                boolean okay = false;
                try {
                    final Field field = this.getClass().getSuperclass().getDeclaredField("requested");
                    field.setAccessible(true);
                    field.setBoolean(this, true);
                    okay = true;
                } catch (final Throwable e2) {
                    e2.printStackTrace();
                }
                if (okay == false) {
                    try {
                        requested = true;
                    } catch (final Throwable e) {
                        e.printStackTrace();
                    }
                }
                httpConnection = con;
                setHtmlCode(t);
            }

            public long postRequest() throws IOException {
                return 0;
            }

            public void preRequest() throws IOException {
            }
        };
        ibr.setRequest(req);
        if (ibr.isDebug()) {
            logger.info("\r\ndirtyMD5sum = " + dMD5 + "\r\ncleanMD5sum = " + JDHash.getMD5(ibr.getRequest().getHtmlCode()) + "\r\n");
            // System.out.println(ibr.getRequest().getHtmlCode());
        }
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.ChomikujPlScript;
    }

    // for the decrypter, so we have only one session of antiddos
    public void getPage(final String url) throws Exception {
        super.getPage(url);
    }

    // for the decrypter, so we have only one session of antiddos
    public void postPage(final String url, final String parameter) throws Exception {
        super.postPage(url, parameter);
    }

    // for the decrypter, so we have only one session of antiddos
    public void submitForm(final Form form) throws Exception {
        super.submitForm(form);
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.AVOIDPREMIUMMP3TRAFFICUSAGE, "Account download: Prefer download of stream versions of audio files in account mode?\r\n<html><b>Avoids premium traffic usage for audio files!</b></html>").setDefaultValue(default_AVOIDPREMIUMMP3TRAFFICUSAGE));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.FREE_ANONYMOUS_MODE_ALLOW_STREAM_DOWNLOAD_AS_FALLBACK, "Allow fallback to stream download if original file is not downloadable without account?").setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.DECRYPTFOLDERS, "Crawl subfolders in folders").setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), ChoMikujPl.IGNORE_TRAFFIC_LIMIT, "Ignore trafficlimit in account (e.g. useful to download self uploaded files or stream download in account mode)?").setDefaultValue(false));
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}