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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.plugins.components.config.DebridLinkFrConfig;
import org.jdownloader.plugins.components.config.DebridLinkFrConfig.PreferredDomain;
import org.jdownloader.plugins.components.realDebridCom.api.json.CodeResponse;
import org.jdownloader.plugins.components.realDebridCom.api.json.TokenResponse;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
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
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 50803 $", interfaceVersion = 4, names = { "debrid-link.com" }, urls = { "" })
public class DebridLinkCom extends PluginForHost {
    private static MultiHosterManagement mhm                                                 = new MultiHosterManagement("debrid-link.com");
    private static final String          PROPERTY_DIRECTURL                                  = "directurl";
    private static final String          PROPERTY_MAXCHUNKS                                  = "maxchunks";
    private static final String          PROPERTY_ACCOUNT_ACCESS_TOKEN                       = "access_token";
    private static final String          PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_CREATED     = "refresh_token_timestamp_created";
    private static final String          PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_VALID_UNTIL = "access_token_timestamp_valid_until";
    private static final String          PROPERTY_ACCOUNT_REFRESH_TOKEN                      = "refresh_token";

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    private int getMaxChunks(final DownloadLink link) {
        final int maxChunksStored = link.getIntegerProperty(PROPERTY_MAXCHUNKS, 1);
        if (maxChunksStored > 1) {
            /* Minus maxChunksStored -> Up to X chunks */
            return -maxChunksStored;
        } else {
            return maxChunksStored;
        }
    }

    public DebridLinkCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept-Language", "en-gb, en;q=0.9");
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setCustomCharset("UTF-8");
        br.setAllowedResponseCodes(new int[] { 400 });
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/tos";
    }

    private String getApiBase() {
        return "https://" + getConfiguredDomain() + "/api/v2";
    }

    private String getApiBaseOauth() {
        return "https://" + getConfiguredDomain() + "/api/oauth";
    }

    private String getClientID() {
        return "5PyzfhqQM0PgFPEArhBadw";
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ac = new AccountInfo();
        login(account, true);
        if (br.getRequest() == null || !br.getURL().contains("/account/infos")) {
            callAPIGetAccountInfo(br);
        }
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        entries = (Map<String, Object>) entries.get("value");
        /* Set censored E-Mail address of user as username. */
        final String emailCensored = (String) entries.get("email");
        if (!StringUtils.isEmpty(emailCensored)) {
            account.setUser(emailCensored);
        }
        /* User could have entered his real password -> We don't want to store that! */
        account.setPass(null);
        final int accountType = ((Number) entries.get("accountType")).intValue();
        final Number premiumLeft = (Number) entries.get("premiumLeft");
        final String registerDate = (String) entries.get("registerDate");
        final Object serverDetected = entries.get("serverDetected");
        final boolean isFree;
        switch (accountType) {
        case 0:
            account.setType(AccountType.FREE);
            isFree = true;
            break;
        case 1:
            if (premiumLeft != null) {
                ac.setValidUntil(System.currentTimeMillis() + (premiumLeft.longValue() * 1000l));
            }
            account.setType(AccountType.PREMIUM);
            isFree = false;
            break;
        case 2:
            ac.setStatus("Lifetime Account");
            account.setType(AccountType.LIFETIME);
            isFree = false;
            break;
        default:
            account.setType(AccountType.UNKNOWN);
            isFree = true;
            logger.warning("Unknown account type: " + accountType);
            break;
        }
        if (!StringUtils.isEmpty(registerDate)) {
            ac.setCreateTime(TimeFormatter.getMilliSeconds(registerDate, "yyyy-MM-dd", Locale.ENGLISH));
        }
        ac.setAccountBalance(((Number) entries.get("pts")).doubleValue());
        /* Update list of supported hosts */
        /* https://debrid-link.com/api_doc/v2/downloader-regex */
        br.getPage(this.getApiBase() + "/downloader/hosts?keys=status%2CisFree%2Cname%2Cdomains");
        final Map<String, Object> resp_downloader_hosts = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> hosters = (List<Map<String, Object>>) resp_downloader_hosts.get("value");
        br.getPage(this.getApiBase() + "/downloader/limits/all");
        final Map<String, Object> resp_downloader_limits_all = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> resp_downloader_limits_all_value = (Map<String, Object>) resp_downloader_limits_all.get("value");
        final List<Map<String, Object>> resp_downloader_limits_all_value_hosters = (List<Map<String, Object>>) resp_downloader_limits_all_value.get("hosters");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hosterinfo : hosters) {
            final String hostname = hosterinfo.get("name").toString();
            final MultiHostHost mhost = new MultiHostHost();
            final int status = ((Number) hosterinfo.get("status")).intValue();
            final boolean isFreeHost = ((Boolean) hosterinfo.get("isFree")).booleanValue();
            if (status == -1) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (status == 0) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (isFree && !isFreeHost) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
            }
            /* Add all domains of this host */
            final List<String> domains = (List<String>) hosterinfo.get("domains");
            mhost.setDomains(domains);
            /* Find- and set limits */
            Map<String, Object> limit_info_map = null;
            for (final Map<String, Object> hoster_limit_info : resp_downloader_limits_all_value_hosters) {
                final String hoster_limit_info_hostname = hoster_limit_info.get("name").toString();
                if (hoster_limit_info_hostname.equals(hostname)) {
                    limit_info_map = hoster_limit_info;
                    break;
                }
            }
            if (limit_info_map != null) {
                final Map<String, Object> trafficLimit = (Map<String, Object>) limit_info_map.get("daySize");
                mhost.setTrafficMax(((Number) trafficLimit.get("value")).longValue());
                mhost.setTrafficLeft(mhost.getTrafficMax() - ((Number) trafficLimit.get("current")).longValue());
                final Map<String, Object> downloadsNumberLimit = (Map<String, Object>) limit_info_map.get("dayCount");
                mhost.setLinksMax(((Number) downloadsNumberLimit.get("value")).longValue());
                mhost.setLinksLeft(mhost.getLinksMax() - ((Number) downloadsNumberLimit.get("current")).longValue());
                /* Log items with currently reached limits. */
                if (mhost.getLinksLeft() <= 0) {
                    logger.info("Currently traffic quota limited host: " + hostname);
                }
                if (mhost.getTrafficLeft() <= 0) {
                    logger.info("Currently link quota limited host: " + hostname);
                }
            }
            supportedhosts.add(mhost);
        }
        ac.setMultiHostSupportV2(this, supportedhosts);
        /** How much percent of the (daily?) quota is used up so far? */
        final Map<String, Object> usagePercentMap = (Map<String, Object>) resp_downloader_limits_all_value.get("usagePercent");
        final int usedPercent = ((Number) usagePercentMap.get("current")).intValue();
        if (!StringUtils.isEmpty(ac.getStatus())) {
            ac.setStatus(ac.getStatus() + " | " + usedPercent + "% used");
        }
        /**
         * 2021-02-23: This service doesn't allow users to use it whenever they use a VPN/Proxy. </br>
         * Accounts can be checked but downloads will not work!
         */
        if (serverDetected != null && serverDetected instanceof Boolean && ((Boolean) serverDetected).booleanValue()) {
            throw new AccountUnavailableException("VPN/Proxy detected: Turn it off to be able to use this account", 5 * 60 * 1000l);
        } else if (usedPercent >= 100) {
            throw new AccountUnavailableException("Reached daily download limit", 5 * 60 * 1000l);
        }
        return ac;
    }

    private boolean accountAccessTokenExpired(final Account account) {
        return System.currentTimeMillis() > account.getLongProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_VALID_UNTIL, 0);
    }

    private boolean accountAccessTokenNeedsRefresh(final Account account) {
        /* Refresh token every 24 hours. */
        return System.currentTimeMillis() - account.getLongProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_CREATED, 0) > 24 * 60 * 60 * 1000l;
    }

    private String accountGetAccessToken(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN);
    }

    private String accountGetRefreshToken(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_REFRESH_TOKEN);
    }

    private void dumpSession(final Account account) {
        account.removeProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN);
        account.removeProperty(PROPERTY_ACCOUNT_REFRESH_TOKEN);
        account.removeProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_VALID_UNTIL);
    }

    private void callAPIGetAccountInfo(final Browser br) throws IOException {
        br.getPage(this.getApiBase() + "/account/infos");
    }

    private void login(final Account account, final boolean verifyLogin) throws Exception {
        synchronized (account) {
            long expiresIn = 0;
            if (this.accountGetAccessToken(account) != null && !accountAccessTokenExpired(account) && !accountAccessTokenNeedsRefresh(account)) {
                /* Check existing token */
                logger.info("Attempting token login");
                br.getHeaders().put("Authorization", "Bearer " + this.accountGetAccessToken(account));
                if (!verifyLogin) {
                    logger.info("Trust token without check");
                    return;
                } else {
                    this.callAPIGetAccountInfo(br);
                    errHandling(account, null);
                    logger.info("Token login successful | Token is valid for: " + TimeFormatter.formatMilliSeconds(account.getLongProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_VALID_UNTIL, 0) - System.currentTimeMillis(), 0));
                    return;
                }
            } else if (this.accountGetRefreshToken(account) != null) {
                logger.info("Trying to refresh access_token");
                final UrlQuery query = new UrlQuery();
                query.add("client_id", Encoding.urlEncode(this.getClientID()));
                query.add("refresh_token", this.accountGetRefreshToken(account));
                query.add("grant_type", "refresh_token");
                br.postPage(this.getApiBaseOauth() + "/token", query);
                final TokenResponse tokenResponse = restoreFromString(br.getRequest().getHtmlCode(), new TypeRef<TokenResponse>(TokenResponse.class) {
                });
                if (!StringUtils.isEmpty(tokenResponse.getAccess_token())) {
                    logger.info("Refresh token successful");
                    br.getHeaders().put("Authorization", "Bearer " + tokenResponse.getAccess_token());
                    account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN, tokenResponse.getAccess_token());
                    account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_CREATED, System.currentTimeMillis());
                    this.accountSetTokenValidity(account, tokenResponse.getExpires_in());
                    return;
                } else {
                    /* This should never happen except maybe if the user has manually revoked API access! */
                    logger.info("Refresh token failed");
                    /* Dump session to make sure we do full login next time! */
                    this.dumpSession(account);
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "Sitzung abgelaufen: Aktualisiere diesen Account, um dich neu einzuloggen", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "Session expired: Refresh this account to re-login", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
            } else {
                logger.info("Full login required");
                final UrlQuery queryDevicecode = new UrlQuery();
                queryDevicecode.add("client_id", Encoding.urlEncode(this.getClientID()));
                queryDevicecode.add("scopes", "get.account,get.post.downloader");
                br.postPage(this.getApiBaseOauth() + "/device/code", queryDevicecode);
                final CodeResponse code = restoreFromString(this.br.getRequest().getHtmlCode(), new TypeRef<CodeResponse>(CodeResponse.class) {
                });
                final Thread dialog = showPINLoginInformation(code.getVerification_url(), code.getUser_code());
                final UrlQuery queryPollingLogin = new UrlQuery();
                queryPollingLogin.add("client_id", Encoding.urlEncode(this.getClientID()));
                queryPollingLogin.appendEncoded("code", code.getDevice_code());
                queryPollingLogin.appendEncoded("grant_type", "urn:ietf:params:oauth:grant-type:device_code");
                // queryPollingLogin.add("grant_type", "http%3A%2F%2Foauth.net%2Fgrant_type%2Fdevice%2F1.0");
                long waitedSeconds = 0;
                final Browser br2 = this.br.cloneBrowser();
                try {
                    do {
                        logger.info("Waiting for user to authorize application: " + waitedSeconds);
                        Thread.sleep(code.getInterval() * 1000l);
                        waitedSeconds += code.getInterval();
                        br2.postPage(this.getApiBaseOauth() + "/token", queryPollingLogin);
                        /*
                         * E.g. returns the following as long as we're waiting for the user to authorize: {"error":"authorization_pending"}
                         */
                        final TokenResponse tokenResponse = restoreFromString(br2.toString(), new TypeRef<TokenResponse>(TokenResponse.class) {
                        });
                        if (!StringUtils.isEmpty(tokenResponse.getAccess_token())) {
                            expiresIn = tokenResponse.getExpires_in();
                            account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN, tokenResponse.getAccess_token());
                            account.setProperty(PROPERTY_ACCOUNT_REFRESH_TOKEN, tokenResponse.getRefresh_token());
                            account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_CREATED, System.currentTimeMillis());
                            this.accountSetTokenValidity(account, expiresIn);
                            br.getHeaders().put("Authorization", "Bearer " + this.accountGetAccessToken(account));
                            return;
                        } else if (!dialog.isAlive()) {
                            logger.info("Dialog closed!");
                            break;
                        } else if (waitedSeconds >= code.getExpires_in()) {
                            logger.info("Timeout reached: " + code.getExpires_in());
                            break;
                        }
                    } while (true);
                } finally {
                    dialog.interrupt();
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
    }

    /**
     * Sets token validity. </br>
     * 2021-02-19: Token validity is set to 1 month via: https://debrid-link.com/webapp/account/apps
     */
    private void accountSetTokenValidity(final Account account, final long expiresIn) {
        account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN_TIMESTAMP_VALID_UNTIL, System.currentTimeMillis() + expiresIn * 1000l);
    }

    private Thread showPINLoginInformation(final String pin_url, final String user_code) {
        final String pin_url_to_open_in_browser;
        if (!pin_url.contains(user_code) && !pin_url.contains("#")) {
            pin_url_to_open_in_browser = pin_url + "#?code=" + user_code;
        } else {
            pin_url_to_open_in_browser = pin_url;
        }
        final String domain = getHost();
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = domain + " - Login";
                        message += "Hallo liebe(r) debrid-link NutzerIn\r\n";
                        message += "Um deinen debrid-link Account in JDownloader verwenden zu können, musst du folgende Schritte beachten:\r\n";
                        message += "1. Öffne diesen Link im Browser falls das nicht automatisch passiert:\r\n\t'" + pin_url + "'\t\r\n";
                        message += "2. Gib diesen PIN Code ein: " + user_code + "\r\n";
                        message += "Dein Account sollte nach einigen Sekunden von JDownloader akzeptiert werden.\r\n";
                    } else {
                        title = domain + " - Login";
                        message += "Hello dear debrid-link user\r\n";
                        message += "In order to use this service in JDownloader, you need to follow these steps:\r\n";
                        message += "1. Open this URL in your browser if it is not opened automatically:\r\n\t'" + pin_url + "'\t\r\n";
                        message += "2. Enter this PIN code: " + user_code + "\r\n";
                        message += "Your account should be accepted in JDownloader within a few seconds.\r\n";
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(2 * 60 * 1000);
                    if (CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                        CrossSystem.openURL(pin_url_to_open_in_browser);
                    }
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }

    /** List of errors: https://debrid-link.com/api_doc/v2/errors */
    private Map<String, Object> errHandling(final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException jme) {
            final String errortext = "Bad API response";
            if (link != null) {
                mhm.handleErrorGeneric(account, link, errortext, 50, 5 * 60 * 1000l);
            } else {
                throw Exceptions.addSuppressed(new AccountUnavailableException(errortext, 1 * 60 * 1000l), jme);
            }
        }
        final boolean success = ((Boolean) entries.get("success")).booleanValue();
        if (success) {
            /* No error */
            return entries;
        }
        final String errorkey = (String) entries.get("error");
        /* List of error key to message obtained from: https://debrid-link.com/api_doc/v2/errors */
        final Map<String, String> errorKeyToMessageMap = new HashMap<String, String>();
        errorKeyToMessageMap.put("access_denied", "The resource owner denied the request for authorization. (Invalid scope or something else)");
        errorKeyToMessageMap.put("accountLocked", "User account is currently locked");
        errorKeyToMessageMap.put("authorization_pending", "The end user hasn't yet completed the user-interaction steps");
        errorKeyToMessageMap.put("badArguments", "The request is malformed, a required parameter is missing or a parameter has an invalid value.");
        errorKeyToMessageMap.put("badFilePassword", "The password for the link is invalid or empty.");
        errorKeyToMessageMap.put("badFileUrl", "The link format is not valid.");
        errorKeyToMessageMap.put("badId", "Resources ID(s) not found");
        errorKeyToMessageMap.put("badSign", "Check the sign parameter");
        errorKeyToMessageMap.put("badToken", "The session does not exist or expired. For OAuth2, you should try to refresh the access_token if you have a refresh_token. For token/key auth, you must create a new one.");
        errorKeyToMessageMap.put("captchaRequired", "Max attempts to login user. IP locked for next 24 hours");
        errorKeyToMessageMap.put("disabledServerHost", "Server / VPN are not allowed on this host");
        errorKeyToMessageMap.put("expired_token", "The 'device_code' has expired, and the device authorization session has concluded");
        errorKeyToMessageMap.put("fileNotAvailable", "This file seems to be temporarily unavailable on the host side.");
        errorKeyToMessageMap.put("fileNotFound", "The filehoster returned a 'file not found' error.");
        errorKeyToMessageMap.put("floodDetected", "API rate limit reached for the endpoint, retry after 1 hour");
        errorKeyToMessageMap.put("freeServerOverload", "No server available for free users");
        errorKeyToMessageMap.put("hidedToken", "The token is not enabled. Redirect the user to 'validTokenUrl'");
        errorKeyToMessageMap.put("hostNotValid", "The filehoster is not supported");
        errorKeyToMessageMap.put("internalError", "Internal error, retry later");
        errorKeyToMessageMap.put("invalid_client", "The client_id is invalid");
        errorKeyToMessageMap.put("invalid_request", "The request is malformed, a required parameter is missing or a parameter has an invalid value");
        errorKeyToMessageMap.put("invalid_scope", "The scope is malformed or invalid");
        errorKeyToMessageMap.put("maintenanceHost", "The filehoster is in maintenance");
        errorKeyToMessageMap.put("maxAttempts", "Max attempts to login user. IP locked for next 24 hours");
        errorKeyToMessageMap.put("maxData", "Limitation of data per day reached");
        errorKeyToMessageMap.put("maxDataHost", "Limitation of data per day for this host reached");
        errorKeyToMessageMap.put("maxLink", "Limitation of number of links per day reached");
        errorKeyToMessageMap.put("maxLinkHost", "Limitation of number of links per day for this host reached");
        errorKeyToMessageMap.put("maxTorrent", "Limitation of torrents per day reached");
        errorKeyToMessageMap.put("notAddTorrent", "Unable to add the torrent, check the URL");
        errorKeyToMessageMap.put("notDebrid", "Unable to generate link, maybe the host is down");
        errorKeyToMessageMap.put("notFreeHost", "This filehoster is not available for free members");
        errorKeyToMessageMap.put("serverNotAllowed", "Server / VPN are not allowed by default. The user must contact us");
        errorKeyToMessageMap.put("server_error", "Internal error, retry later");
        errorKeyToMessageMap.put("torrentTooBig", "The torrent is too big or has too many files");
        errorKeyToMessageMap.put("unauthorized_client", "The client is not authorized to request an authorization code using this method");
        errorKeyToMessageMap.put("unknowR", "Endpoint not found");
        errorKeyToMessageMap.put("unsupported_grant_type", "Authorization grant is not supported");
        errorKeyToMessageMap.put("unsupported_response_type", "response_type is not supported");
        final String message;
        if (errorKeyToMessageMap.containsKey(errorkey)) {
            message = errorKeyToMessageMap.get(errorkey);
        } else {
            /* Fallback: Use error key as message */
            logger.info("Detected unexpected error key: " + errorkey);
            message = errorkey;
        }
        final HashSet<String> accountErrorsPermanent = new HashSet<String>();
        accountErrorsPermanent.add("badToken");
        final HashSet<String> accountErrorsTemporary = new HashSet<String>();
        accountErrorsTemporary.add("serverNotAllowed");
        accountErrorsTemporary.add("floodDetected");
        accountErrorsTemporary.add("accountLocked");
        accountErrorsTemporary.add("maxLink");
        accountErrorsTemporary.add("maxData");
        final HashSet<String> downloadErrorsHostUnavailable = new HashSet<String>();
        downloadErrorsHostUnavailable.add("notDebrid");
        downloadErrorsHostUnavailable.add("hostNotValid");
        downloadErrorsHostUnavailable.add("notFreeHost");
        downloadErrorsHostUnavailable.add("maintenanceHost");
        downloadErrorsHostUnavailable.add("noServerHost");
        downloadErrorsHostUnavailable.add("maxLinkHost");
        downloadErrorsHostUnavailable.add("maxDataHost");
        downloadErrorsHostUnavailable.add("disabledServerHost");
        downloadErrorsHostUnavailable.add("freeServerOverload");
        final HashSet<String> downloadErrorsFileUnavailable = new HashSet<String>();
        downloadErrorsFileUnavailable.add("fileNotAvailable");
        downloadErrorsFileUnavailable.add("badFileUrl");
        downloadErrorsFileUnavailable.add("badFilePassword");
        if ("fileNotFound".equals(errorkey)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (accountErrorsPermanent.contains(errorkey)) {
            /**
             * E.g. if user revokes access via debrid-link website.
             */
            account.removeProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN);
            throw new AccountInvalidException(message);
        } else if (accountErrorsTemporary.contains(errorkey)) {
            throw new AccountUnavailableException(message, 5 * 60 * 1000);
        } else if (downloadErrorsHostUnavailable.contains(errorkey)) {
            mhm.putError(account, link, 5 * 60 * 1000l, message);
        } else if (downloadErrorsFileUnavailable.contains(errorkey)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message);
        } else {
            /*
             * Unknown/Generic error --> Assume it is a download issue but display it as temp. account issue if no DownloadLink is given.
             */
            logger.info("Unknown API error: " + errorkey);
            if (link != null) {
                mhm.handleErrorGeneric(account, link, message, 50);
            } else {
                /* Temp disable account */
                throw new AccountUnavailableException(message, 5 * 60 * 1000);
            }
        }
        /* This code should never be reached. */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, message);
    }

    private boolean isErrorPasswordRequiredOrWrong(final String str) {
        return str != null && str.equalsIgnoreCase("badFilePassword");
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 0;
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception, PluginException {
        /* This should never be called. */
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        this.login(account, false);
        if (!attemptStoredDownloadurlDownload(link)) {
            boolean enteredCorrectPassword = false;
            String passCode = link.getDownloadPassword();
            for (int wrongPasswordAttempts = 0; wrongPasswordAttempts <= 2; wrongPasswordAttempts++) {
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("url", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
                if (passCode != null) {
                    query.appendEncoded("password", passCode);
                }
                br.postPage(this.getApiBase() + "/downloader/add", query);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (this.isErrorPasswordRequiredOrWrong((String) entries.get("error"))) {
                    wrongPasswordAttempts += 1;
                    passCode = getUserInput("Password?", link);
                    /*
                     * Do not reset initial password. Multihosters are prone to error - we do not want to remove the users' initial manually
                     * typed in PW!
                     */
                    // link.setDownloadPassword(null);
                    continue;
                } else {
                    enteredCorrectPassword = true;
                    break;
                }
            }
            if (!enteredCorrectPassword) {
                /* Allow next candidate to try */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Wrong password entered");
            } else if (passCode != null) {
                link.setDownloadPassword(passCode);
            }
            Map<String, Object> entries = this.errHandling(account, link);
            entries = (Map<String, Object>) entries.get("value");
            link.setProperty(PROPERTY_MAXCHUNKS, entries.get("chunk"));
            final String dllink = entries.get("downloadUrl").toString();
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), getMaxChunks(link));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                try {
                    errHandling(account, link);
                } catch (JSonMapperException ignore) {
                    logger.log(ignore);
                }
                if (br.containsHTML("Unable to download the file on the host server")) {
                    mhm.handleErrorGeneric(account, link, "Unable to download the file on the host server", 50, 5 * 60 * 1000l);
                }
                logger.warning("Unhandled download error on Service Provider side:");
                mhm.handleErrorGeneric(account, link, "final_downloadurl_isnot_a_file", 50, 5 * 60 * 1000l);
            }
            link.setProperty(PROPERTY_DIRECTURL, dl.getConnection().getURL().toString());
        }
        /*
         * 2019-09-06: They sometimes return wrong final filenames e.g. for vidoza.net filenames will all get changed to "video.mp4". This
         * is a small workaround. In general, it prefers the filenames of the original plugin if they're longer.
         */
        String final_filename = null;
        final String previous_filename = link.getName();
        final String this_filename = getFileNameFromConnection(dl.getConnection());
        if (previous_filename != null && this_filename != null && (this_filename.length() < previous_filename.length() || link.getHost().contains("vidoza"))) {
            final_filename = previous_filename;
        } else {
            final_filename = this_filename;
        }
        if (final_filename != null) {
            link.setFinalFileName(final_filename);
        }
        dl.startDownload();
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link) throws Exception {
        final String url = link.getStringProperty(PROPERTY_DIRECTURL);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, null), this.getMaxChunks(link));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final IOException e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (final Throwable e2) {
            }
            return false;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink link) throws Exception {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    protected String getConfiguredDomain() {
        /* Returns user-defined value which can be used to circumvent GEO-block. */
        PreferredDomain cfgdomain = PluginJsonConfig.get(DebridLinkFrConfig.class).getPreferredDomain();
        if (cfgdomain == null) {
            cfgdomain = PreferredDomain.DEFAULT;
        }
        switch (cfgdomain) {
        case DOMAIN1:
            return "debrid-link.com";
        case DEFAULT:
        default:
            return this.getHost();
        }
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return DebridLinkFrConfig.class;
    }
}