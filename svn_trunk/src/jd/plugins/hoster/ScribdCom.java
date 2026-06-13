//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
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

@HostPlugin(revision = "$Revision: 52899 $", interfaceVersion = 2, names = { "scribd.com" }, urls = { "https?://(?:www\\.)?(?:(?:de|ru|es)\\.)?scribd\\.com/(doc|document|book|embeds|read)/\\d+" })
public class ScribdCom extends PluginForHost {
    private final String        SETTING_PREFERRED_FORMAT                 = "formats";
    private final String        PROPERTY_DOWNLOADLINK_LAST_CHOSEN_FORMAT = "last_chosen_format";
    /** The list of server values displayed to the user */
    private final String[]      allFormats                               = new String[] { "PDF", "TXT", "DOCX" };
    private static final String TYPE_DOCUMENT                            = ".+/(doc|document)/.+";
    private static final String TYPE_AUDIO                               = ".+/audiobook/.+";
    private Map<String, Object> entries                                  = null;

    public ScribdCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www.scribd.com");
        setConfigElements();
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), "/(\\d+)(?:/[^/]+)?$").getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException, InterruptedException {
        prepFreeBrowser(this.br);
        // final boolean checkViaJson = !link.getPluginPatternMatcher().matches(TYPE_AUDIO);
        String title = null;
        String description = null;
        final String fid = this.getFID(link);
        boolean is_audiobook = false;
        String filesizeStr = null;
        String fileExtension = "pdf";
        final String userPreferredFormat = this.getPreferredFormat(link);
        int counter400 = 0;
        do {
            br.getPage(link.getPluginPatternMatcher());
            counter400++;
        } while (counter400 <= 5 && br.getHttpConnection().getResponseCode() == 400);
        if (br.getURL().contains("/removal/") || br.getURL().contains("/deleted/")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 400) {
            logger.info("Server returns error 400");
            return AvailableStatus.UNCHECKABLE;
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        boolean is_deleted = false;
        try {
            final String json = br.getRegex("<script type=\"application/json\" data-hypernova-key=\"doc_page\"[^>]+><[^\\{]*?(\\{[^<]*\\})[^\\}<]*</script>").getMatch(0);
            if (json != null) {
                entries = restoreFromString(json, TypeRef.MAP);
                Map<String, Object> docmap = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "page/word_document");
                if (docmap == null) {
                    /* 2024-03-07 */
                    docmap = (Map<String, Object>) entries.get("wordDocument");
                }
                entries = docmap;
                title = (String) entries.get("title");
                final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) docmap.get("formats");
                if (ressourcelist != null && ressourcelist.size() > 0) {
                    for (final Map<String, Object> ressource : ressourcelist) {
                        filesizeStr = ressource.get("filesize").toString();
                        fileExtension = ressource.get("extension").toString();
                        if (fileExtension.equalsIgnoreCase(userPreferredFormat)) {
                            break;
                        }
                    }
                }
            } else {
                logger.warning("Failed to find any json source");
            }
            /* 2019-08-11: TODO: Find out what these are good for: 'secret_password' and 'access_key' */
        } catch (final Throwable e) {
        }
        if (is_deleted) {
            /* 2019-08-12: Rare case */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (StringUtils.isEmpty(title)) {
            /* Fallback */
            title = new Regex(br.getURL(), "/" + fid + "/([^/]+)").getMatch(0); // url slug
        }
        if (Boolean.TRUE.equals(is_audiobook)) {
            fileExtension = "mp3";
        }
        if (title != null) {
            link.setName(fid + "-" + Encoding.htmlDecode(title).trim() + "." + fileExtension);
        } else {
            logger.warning("Failed to find title -> Fallback to file_id as title");
            link.setName(fid + "." + fileExtension);
        }
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.warning("Failed to find filesize");
        }
        if (!StringUtils.isEmpty(description) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
        /* saving session info can result in avoiding 400, 410 server errors */
        final Map<String, String> cookies = new HashMap<String, String>();
        final Cookies add = br.getCookies(this.getHost());
        for (final Cookie c : add.getCookies()) {
            cookies.put(c.getKey(), c.getValue());
        }
        synchronized (cookieMonster) {
            cookieMonster.set(cookies);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final boolean fetchDataViaPurchaseHistory = true;
        String accountType = null;
        String accountPaymentType = null;
        String expiredateStr = null;
        /* The userID might be used in our crawler later */
        String userID = null;
        long expireTimestamp = 0;
        if (fetchDataViaPurchaseHistory) {
            /* 2019-08-12: Alternative way */
            /* First attempt - go through payment history and grab the latest entry + expire-date */
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            brc.getHeaders().put("sec-fetch-mode", "cors");
            brc.getHeaders().put("sec-fetch-site", "same-origin");
            brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            brc.getPage("/account-settings/payment-transactions");
            try {
                Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Object> ressourcelist = (List<Object>) entries.get("payment_transactions");
                for (final Object transactionO : ressourcelist) {
                    entries = (Map<String, Object>) transactionO;
                    final boolean refunded = ((Boolean) entries.get("refunded")).booleanValue();
                    final Object descriptionO = entries.get("description");
                    if (refunded || descriptionO == null) {
                        continue;
                    }
                    accountPaymentType = (String) entries.get("payment_method");
                    entries = (Map<String, Object>) descriptionO;
                    expiredateStr = (String) entries.get("valid_until");
                    if (!StringUtils.isEmpty(expiredateStr)) {
                        break;
                    }
                }
                /* E.g. "Gültig: 8/12/19 - 9/11/19" */
                final String[] createDataAndExpireDate = new Regex(expiredateStr, "(\\d{1,2}/\\d{1,2}/\\d{1,2})").getColumn(0);
                if (createDataAndExpireDate != null && createDataAndExpireDate.length >= 2) {
                    expiredateStr = createDataAndExpireDate[1];
                    expireTimestamp = TimeFormatter.getMilliSeconds(expiredateStr, "MM/dd/yy", Locale.ENGLISH);
                }
            } catch (final Throwable e) {
                e.printStackTrace();
                logger.warning("Exception happened during parsing expire date");
            }
        } else {
            br.getPage("/account-settings/account");
            final String json_account = br.getRegex("ReactDOM\\.render\\(React\\.createElement\\(Scribd\\.AccountSettings\\.Show, (\\{.*?\\})\\), document\\.getElementById").getMatch(0);
            try {
                Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(json_account);
                final long userIDLong = JavaScriptEngineFactory.toLong(JavaScriptEngineFactory.walkJson(entries, "user/id"), 0);
                if (userIDLong > 0) {
                    userID = Long.toString(userIDLong);
                }
                entries = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "membership_info/plan_info");
                /* 2019-08-12: E.g. "cancelled_with_valid_to" */
                accountType = (String) entries.get("type");
                expiredateStr = (String) entries.get("valid_to_date");
                expireTimestamp = TimeFormatter.getMilliSeconds(expiredateStr, "yyyy-MM-dd HH:mm:ss ZZZ", Locale.ENGLISH);
            } catch (final Throwable e) {
            }
        }
        if (StringUtils.isEmpty(userID)) {
            /* Fallback */
            userID = br.getRegex("var _user_id\\s*?=\\s*?\"(\\d+)\";").getMatch(0);
        }
        final AccountInfo ai = new AccountInfo();
        String accountStatus = null;
        if (expireTimestamp > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            accountStatus = "Premium account";
            if ("cancelled_with_valid_to".equalsIgnoreCase(accountType)) {
                accountStatus += " [Subscription cancelled]";
            } else if (!StringUtils.isEmpty(accountType)) {
                /* TODO: 2019-08-12: Find out which other versions of "type" they have! */
                accountStatus += " [Active subscription]";
            }
            if (accountPaymentType != null) {
                accountStatus += " [Paid via " + accountPaymentType + "]";
            }
            ai.setValidUntil(expireTimestamp, br);
        } else {
            account.setType(AccountType.FREE);
        }
        ai.setStatus(accountStatus);
        ai.setUnlimitedTraffic();
        // if (!StringUtils.isEmpty(userID)) {
        // account.setProperty("userid", userID);
        // }
        return ai;
    }

    @Override
    public String getAGBLink() {
        return "http://" + getHost() + "/terms";
    }

    private String getPreferredFormat(final DownloadLink link) {
        final String preChosenFormat = link.getStringProperty(PROPERTY_DOWNLOADLINK_LAST_CHOSEN_FORMAT);
        if (preChosenFormat != null) {
            return preChosenFormat;
        }
        switch (getPluginConfig().getIntegerProperty(SETTING_PREFERRED_FORMAT, -1)) {
        case 0:
            return "pdf";
        case 1:
            return "txt";
        case 2:
            return "docx";
        default:
            return "pdf";
        }
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
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (link.getPluginPatternMatcher().matches(TYPE_DOCUMENT)) {
            /* Account required to be able to download anything */
            throw new AccountRequiredException();
        } else {
            throw new PluginException(LinkStatus.ERROR_FATAL, "This item is not downloadable at all or only for registered users");
        }
    }

    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        requestFileInformation(link);
        boolean is_downloadable = true;
        boolean is_downloadable_for_premium_users = false;
        boolean is_view_restricted_archive = false;
        boolean show_archive_paywall = false;
        /* TODO: 2026-06-12: Check download of audio books */
        boolean is_audiobook = false;
        try {
            is_downloadable = ((Boolean) entries.get("is_downloadable")).booleanValue();
            is_view_restricted_archive = ((Boolean) entries.get("is_view_restricted_archive")).booleanValue();
            show_archive_paywall = ((Boolean) entries.get("show_archive_paywall")).booleanValue();
        } catch (final Throwable e) {
            logger.info("Possible json parsing issues, moving forward to download attempt anyways");
            e.printStackTrace();
        }
        // final boolean is_paid = ((Boolean) entries.get("is_paid")).booleanValue();
        /* 2019-08-11: TODO: Find out what 'is_credit_restricted' and 'is_paid' means */
        // final boolean is_credit_restricted = ((Boolean) entries.get("is_credit_restricted")).booleanValue();
        final boolean isPremium = account.getType() == AccountType.PREMIUM;
        if (is_downloadable_for_premium_users && !isPremium) {
            throw new AccountRequiredException("Only downloadable for premium users");
        } else if (!is_downloadable) {
            /* 2019-08-11: Not downloadable at all (?!) */
            throw new PluginException(LinkStatus.ERROR_FATAL, "This file is not downloadable");
        } else if (is_view_restricted_archive && show_archive_paywall && !isPremium) {
            this.premiumonlyArchiveViewRestricted();
            /* Unreachable code */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (is_audiobook) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Audiobooks cannot be downloaded yet!");
        }
        final String userPreferredFormat = this.getPreferredFormat(link);
        final String fileId = this.getFID(link);
        if (fileId == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final List<Map<String, Object>> formats = (List<Map<String, Object>>) entries.get("formats");
        if (formats == null || formats.isEmpty()) {
            /* E.g. for free accounts, this will return an empty list of items */
            logger.info("Seems like not a single download is available --> This item is READ-ONLY");
            // if (account == null) {
            // throw new AccountRequiredException();
            // }
            if (link.getPluginPatternMatcher().matches(TYPE_DOCUMENT)) {
                /*
                 * Should never happen - maybe we were logged-out or account is not premium but item is only downloadable for premium users
                 * ... or not downloadable at all for some reason.
                 */
                throw new AccountRequiredException();
            } else {
                /* E.g. not even website provides download-button. Maybe downloadable inside their own app (DRM protected). */
                throw new PluginException(LinkStatus.ERROR_FATAL, "This item is not downloadable");
            }
        }
        String formatToDownload = "pdf";
        boolean foundUserPreferredFormat = false;
        for (final Map<String, Object> format : formats) {
            formatToDownload = format.get("extension").toString();
            if (formatToDownload.equalsIgnoreCase(userPreferredFormat)) {
                foundUserPreferredFormat = true;
                break;
            }
        }
        if (!foundUserPreferredFormat) {
            logger.info("Failed to find user preferred format");
        }
        final Browser brc = br.cloneBrowser();
        brc.setFollowRedirects(false);
        brc.getPage("/document_downloads/" + fileId + "?extension=" + formatToDownload);
        if (brc.containsHTML("Sorry, downloading this document in the requested format has been disallowed")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Format " + formatToDownload + " is not available for this file!");
        } else if (brc.containsHTML("You do not have access to download this document|Invalid document format")) {
            /* This will usually go along with response 403. */
            if (account != null) {
                logger.info("This file might not be downloadable at all");
            }
            throw new AccountRequiredException("This file can only be downloaded by premium users");
        }
        final String dllink = brc.getRedirectLocation();
        if (dllink == null) {
            /* 2020-07-20: */
            // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            if (brc.getRequest().getHtmlCode().length() <= 100) {
                /* 2020-07-20: E.g. errormessage: All download limits exceeded from your IP (123.123.123.123). */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Website-error: " + brc.getRequest().getHtmlCode());
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadurl");
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            /* Assume that our current account type = free and the file is not downloadable */
            throw new AccountRequiredException();
        }
        final String fname = getFileNameFromConnection(dl.getConnection());
        if (fname != null) {
            link.setFinalFileName(Encoding.htmlDecode(fname));
        }
        /* Remember in order to avoid resume with other format. */
        link.setProperty(PROPERTY_DOWNLOADLINK_LAST_CHOSEN_FORMAT, formatToDownload);
        dl.startDownload();
    }

    public void login(final Account account, final boolean validate) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            prepBRGeneral(br);
            final Cookies userCookies = account.loadUserCookies();
            if (userCookies == null || userCookies.isEmpty()) {
                showCookieLoginInfo();
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_required());
            }
            br.setCookies(userCookies);
            if (!validate) {
                return;
            }
            logger.info("Verifying login cookies");
            br.getPage("https://www." + this.getHost() + "/");
            if (!isLoggedIN(br)) {
                logger.info("User Cookie login failed");
                if (account.hasEverBeenValid()) {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                } else {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                }
            }
        }
    }

    private boolean isLoggedIN(final Browser br) {
        if (br.containsHTML("/logout")) {
            return true;
        } else if (br.containsHTML("isLoggedIn\":true")) {
            /* 2026-05-07 */
            return true;
        } else {
            return false;
        }
    }

    private void premiumonlyArchiveViewRestricted() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_FATAL, "You need to upload a document in order to be able to download this document");
    }

    private boolean                        coLoaded      = false;
    private static AtomicReference<Object> cookieMonster = new AtomicReference<Object>();

    @SuppressWarnings("unchecked")
    private Browser prepFreeBrowser(final Browser prepBR) {
        prepBRGeneral(prepBR);
        // loading previous cookie session results in less captchas
        synchronized (cookieMonster) {
            if (cookieMonster.get() != null && cookieMonster.get() instanceof Map<?, ?>) {
                final Map<String, String> cookies = (Map<String, String>) cookieMonster.get();
                for (Map.Entry<String, String> entry : cookies.entrySet()) {
                    prepBR.setCookie(this.getHost(), entry.getKey(), entry.getValue());
                }
                coLoaded = true;
            }
        }
        return prepBR;
    }

    private static void prepBRGeneral(final Browser br) {
        br.getHeaders().put("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0");
        br.setAllowedResponseCodes(new int[] { 400, 410, 500 });
        br.setLoadLimit(br.getLoadLimit() * 5);
        br.setCookie("https://www.scribd.com/", "lang", "en");
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        link.removeProperty(PROPERTY_DOWNLOADLINK_LAST_CHOSEN_FORMAT);
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_COMBOBOX_INDEX, getPluginConfig(), SETTING_PREFERRED_FORMAT, allFormats, "Preferably download files in this format:").setDefaultValue(0));
    }
}