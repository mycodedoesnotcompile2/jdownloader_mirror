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
package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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
import jd.plugins.MultiHostHost;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 50404 $", interfaceVersion = 3, names = { "esoubory.cz" }, urls = { "https?://(?:www\\.)?esoubory\\.cz/(?:[a-z]{2}/)?(?:file|soubor|redir)/[a-f0-9]{8}/[a-z0-9\\-]+(?:/?|\\.html)" })
public class EsouboryCz extends PluginForHost {
    public EsouboryCz(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/credits/buy/");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setAllowedResponseCodes(new int[] { 400 });
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    /* Using similar API (and same owner): esoubory.cz, filesloop.com */
    @Override
    public String getAGBLink() {
        return "https://www." + getHost();
    }

    private static final String          API_BASE                                       = "https://www.esoubory.cz/api";
    /* 2025-01-03: API for selfhosted content is broken */
    private static final boolean         USE_API_IN_ACCOUNT_MODE_FOR_SELFHOSTED_CONTENT = false;
    private static MultiHosterManagement mhm                                            = new MultiHosterManagement("esoubory.cz");

    private String getLinkpart(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), "/([a-f0-9]{8}/[a-z0-9\\-]+)(?:/?|\\.html)").getMatch(0);
    }

    private String getContentURL(final DownloadLink link) {
        final String linkpart = getLinkpart(link);
        if (linkpart != null) {
            /* Prefer English language. */
            return "https://www." + this.getHost() + "/en/file/" + linkpart;
        } else {
            return link.getPluginPatternMatcher();
        }
    }

    @Override
    public boolean isProxyRotationEnabledForLinkChecker() {
        return false;
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
        return new Regex(link.getPluginPatternMatcher(), "(?i)(?:file|soubor|redir)/([a-f0-9]{8})").getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, Exception {
        final Account aa = AccountController.getInstance().getValidAccount(this);
        return requestFileInformation(link, aa);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws IOException, Exception {
        String name_url = new Regex(link.getPluginPatternMatcher(), "(?i)(?:file|soubor|redir)/[a-f0-9]{8}/([a-z0-9\\-]+)").getMatch(0);
        if (name_url != null && !link.isNameSet()) {
            /* Fix file extension e.g. "-rar" -> ".rar". */
            name_url = name_url.replaceFirst("-([a-z0-9]{2,5})$", ".$1");
            link.setName(name_url);
        }
        String filename;
        String filesize;
        if (account != null && USE_API_IN_ACCOUNT_MODE_FOR_SELFHOSTED_CONTENT) {
            /* API */
            br.getPage(API_BASE + "/exists?token=" + loginAPI(account, false) + "&url=" + Encoding.urlEncode(link.getPluginPatternMatcher()));
            final Map<String, Object> data = this.checkErrorsAPI(br, link, account);
            /* File information can be given even if the file is offline. */
            filename = (String) data.get("filename");
            filesize = PluginJSonUtils.getJson(br, "filesize");
            if (filesize != null && filesize.matches("\\d+")) {
                link.setDownloadSize(Long.parseLong(filesize));
            }
            if (filename != null) {
                link.setFinalFileName(filename);
            }
            if (!Boolean.TRUE.equals(data.get("exists"))) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        } else {
            /* Website */
            br.getPage(getContentURL(link));
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("<h1>\\s*404 error - page not found")) {
                /* Error 404 without responsecode 404 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!this.canHandle(br.getURL())) {
                /* E.g. redirect to https://www.esoubory.cz/search/blabla.html */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            // final Regex linkinfo = br.getRegex("<h1>([^<>\"]*?)<span class=\"bluetext upper\">\\(([^<>\"]*?)\\)</span>");
            final Regex linkinfo = br.getRegex("<h1>\\s*([^<>]*?)\\((\\d+((,|\\.)\\d+)? (K|M|G)B)\\)\\s*</h1>");
            filename = linkinfo.getMatch(0);
            filesize = linkinfo.getMatch(1);
            if (filesize == null) {
                filesize = br.getRegex("<span class=\"fa fa-hdd-o\"></span>([^<]+)</span>").getMatch(0);
            }
            String fileextensionWithDot = br.getRegex("<span class=\"fa fa\\-file\"></span>([^<]+)</span>").getMatch(0);
            if (filename != null) {
                filename = Encoding.htmlDecode(filename).trim();
                if (fileextensionWithDot != null) {
                    fileextensionWithDot = fileextensionWithDot.trim();
                    final String fileextensionWithoutDotButSpace = fileextensionWithDot.replace(".", " ");
                    /* Remove file title ending with space + file-extension " mkv" */
                    filename = filename.replaceFirst("(?i)" + fileextensionWithoutDotButSpace + "$", "");
                    filename = this.applyFilenameExtension(filename, fileextensionWithDot);
                }
                /* Do not set the final filename here as we'll have the API when downloading via account anyways! */
                link.setName(filename);
            }
            if (filesize != null) {
                filesize = filesize.replace(",", ".");
                link.setDownloadSize(SizeFormatter.getSize(filesize));
            }
            /* Check for other offline traits after collecting file information. */
            /* 2023-06-26: Some files have their file-information (filesize/filename) given but are offline. */
            /* 2025-01-03: This text is always in their html code even for files that are available so I've removed this check for now. */
            // if (br.containsHTML(">\\s*File is not available anymore")) {
            // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            // }
            if (filename == null) {
                logger.warning("Failed to find filename");
            }
            if (filesize == null) {
                logger.warning("Failed to find filesize");
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDL(link, account);
    }

    private void handleDL(final DownloadLink link, final Account account) throws Exception {
        String finallink = checkDirectLink(link, "esouborydirectlink");
        final String contentURL = getContentURL(link);
        final boolean isSelfhostedContent = new Regex(contentURL, this.getSupportedLinks()).patternFind();
        if (finallink == null) {
            if (isSelfhostedContent && !USE_API_IN_ACCOUNT_MODE_FOR_SELFHOSTED_CONTENT) {
                /* 2018-12-27: API Support broken for selfhosted content! */
                /* 2020-05-12: API is working fine again for selfhosted content */
                loginWebsite(account, false);
                requestFileInformation(link, account);
                /* Downloadlink has to be accessed otherwise we're not able to download via 'finallink' below! */
                br.getPage(contentURL);
                finallink = "https://www." + this.getHost() + "/redir/" + getLinkpart(link) + ".html";
                // br.setFollowRedirects(false);
                // final String continue_url = "https://www.esoubory.cz/redir/" + new Regex(link.getPluginPatternMatcher(),
                // "([^/]+/[^/]+)(?:\\.html)?$").getMatch(0) + ".html";
                // br.getPage(continue_url);
                // finallink = br.getRedirectLocation();
            } else {
                if (isSelfhostedContent) {
                    requestFileInformation(link, account);
                }
                br.getPage(API_BASE + "/filelink?token=" + loginAPI(account, false) + "&url=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
                final Map<String, Object> data = this.checkErrorsAPI(br, link, account);
                finallink = data.get("link").toString();
            }
        }
        if (StringUtils.isEmpty(finallink)) {
            logger.warning("Failed to find final downloadlink");
            if (isSelfhostedContent) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 50);
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, finallink, true, -2);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (isSelfhostedContent) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                mhm.handleErrorGeneric(account, link, "Unknown download error", 50);
            }
        }
        link.setProperty("esouborydirectlink", finallink);
        dl.startDownload();
    }

    /** 2018-12-27: Required for some parts of the plugin for which the API fails. */
    private void loginWebsite(final Account account, final boolean verify) throws IOException, PluginException {
        final Cookies cookies = account.loadCookies("");
        if (cookies != null) {
            logger.info("Attempting cookie login");
            br.setCookies(getHost(), cookies);
            if (!verify) {
                /* Do not verify cookies */
                return;
            }
            br.getPage("https://www." + getHost() + "/en/");
            if (isLoggedinWebsite(br)) {
                /* Cookie login successful */
                logger.info("Cookie login successful");
                return;
            }
            /* Full login required */
            logger.info("Cookie login failed");
            br.clearCookies(br.getHost());
        }
        logger.info("Performing full login");
        br.getPage("https://www." + getHost() + "/en/account/login/");
        final Form loginform = br.getFormbyProperty("name", "FormLogin_form");
        if (loginform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        loginform.put("email", account.getUser());
        loginform.put("password", account.getPass());
        loginform.put("remember", "1");
        br.submitForm(loginform);
        if (!isLoggedinWebsite(br)) {
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
        }
        account.saveCookies(br.getCookies(br.getHost()), "");
    }

    private boolean isLoggedinWebsite(final Browser br) {
        return br.containsHTML("/account/logout/");
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        handleDL(link, account);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final String token = loginAPI(account, true);
        if (!br.getURL().contains("/accountinfo?token=")) {
            br.getPage(API_BASE + "/accountinfo?token=" + token);
        }
        final AccountInfo ai = new AccountInfo();
        /* Their entire system is credit based. Once the bought traffic is empty, the account cannot be used anymore. */
        ai.setTrafficRefill(false);
        Map<String, Object> data = this.checkErrorsAPI(br, account);
        final long trafficLeftMB = ((Number) data.get("credit")).longValue();
        ai.setTrafficLeft(trafficLeftMB * 1024 * 1024);
        if (trafficLeftMB > 0) {
            account.setType(AccountType.PREMIUM);
        } else {
            /* Account without traffic == Free Account --> Useless */
            account.setType(AccountType.FREE);
        }
        account.setConcurrentUsePossible(true);
        account.setMaxSimultanDownloads(-1);
        br.getPage(API_BASE + "/list");
        /*
         * E.g. {"error":"","data":{"list":"http:\/\/www.edisk.cz;https:\/\/datoid.cz;https:\/\/webshare.cz;https:\/\/www.shareprofi.com"}}
         */
        data = this.checkErrorsAPI(br, account);
        final String hostsStr = (String) data.get("list");
        final String[] hosts = hostsStr.split(";");
        final ArrayList<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final String hostAsURL : hosts) {
            String domain = Browser.getHost(hostAsURL, true);
            domain = domain.replaceFirst("(?i)www.", "");
            final MultiHostHost mhost = new MultiHostHost(domain);
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private String loginAPI(final Account account, final boolean validate) throws Exception {
        synchronized (account) {
            String token = account.getStringProperty("token", null);
            if (token != null) {
                if (!validate) {
                    logger.info("Trust token without checking");
                    return token;
                }
                br.getPage(API_BASE + "/accountinfo?token=" + token);
                try {
                    /**
                     * 2021-07-26: This may also returns teh following on invalid token </br>
                     * {"error":"","data":{"credit":0,"last_login":null}}
                     */
                    final Map<String, Object> data = checkErrorsAPI(this.br, account);
                    if (data.get("last_login") != null) {
                        logger.info("Token login successful");
                        return token;
                    } else {
                        /* This should never happen */
                        logger.info("Token login failed");
                    }
                } catch (final Exception ignore) {
                    logger.log(ignore);
                    logger.info("Token login failed");
                }
            }
            logger.info("Performing full login");
            br.getPage(API_BASE + "/login?email=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            final Map<String, Object> data = checkErrorsAPI(this.br, account);
            token = (String) data.get("token");
            if (StringUtils.isEmpty(token)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            account.setProperty("token", token);
            return token;
        }
    }

    private Map<String, Object> checkErrorsAPI(final Browser br, final Account account) throws PluginException {
        return checkErrorsAPI(br, null, account);
    }

    private Map<String, Object> checkErrorsAPI(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String error = (String) entries.get("error");
        /* "error":"" == default! */
        if (StringUtils.isEmpty(error)) {
            /* No error */
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            return data;
        }
        if (error.equalsIgnoreCase("invalid-email")) {
            throw new AccountInvalidException();
        } else if (error.equalsIgnoreCase("invalid-api-access")) {
            throw new AccountInvalidException("Token expired");
        } else if (error.equalsIgnoreCase("not-enough-credits")) {
            throw new AccountUnavailableException(_GUI.T.account_error_no_traffic_left(), 5 * 60 * 1000l);
        } else if (error.equalsIgnoreCase("invalid-file-url")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            /* Undefined error --> Treat as account-error */
            if (link == null) {
                throw new AccountUnavailableException(error, 5 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, error);
            }
        }
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    if (con.getCompleteContentLength() > 0) {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                    return dllink;
                } else {
                    throw new IOException();
                }
            } catch (final Exception e) {
                logger.log(e);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        return false;
    }
}