//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.PluginProgress;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 49913 $", interfaceVersion = 3, names = { "rapids.pl" }, urls = { "" })
public class RapidsPl extends PluginForHost {
    /* API documentation: https://new.rapids.pl/api */
    private static final String  API_BASE            = "https://api.rapids.pl/api";
    /* Connection limits: 2020-03-24: According to API docs "Max Connections: 15 per user/minute" --> WTF --> Set it to unlimited for now */
    private static final int     defaultMAXDOWNLOADS = -1;
    private static final int     defaultMAXCHUNKS    = 1;
    private static final boolean defaultRESUME       = false;
    private static final String  PROPERTY_logintoken = "token";
    private static final String  PROPERTY_directlink = "directlink";

    @SuppressWarnings("deprecation")
    public RapidsPl(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost());
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        /* Set headers according to API docs */
        br.getHeaders().put("Content-Type", "application/json");
        br.getHeaders().put("x-lang", "en");
        br.setAllowedResponseCodes(new int[] { 401, 423 });
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/pomoc/regulamin";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* Without account its not possible to download the link */
            return false;
        }
        return true;
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* handle premium should never get called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /* Stolen from LinkSnappyCom/CboxeraCom */
    private String cacheDLChecker(final DownloadLink link) throws Exception {
        final PluginProgress waitProgress = new PluginProgress(0, 100, null) {
            protected long lastCurrent    = -1;
            protected long lastTotal      = -1;
            protected long startTimeStamp = -1;

            @Override
            public PluginTaskID getID() {
                return PluginTaskID.WAIT;
            }

            @Override
            public String getMessage(Object requestor) {
                if (requestor instanceof ETAColumn) {
                    final long eta = getETA();
                    if (eta >= 0) {
                        return TimeFormatter.formatMilliSeconds(eta, 0);
                    }
                    return "";
                }
                return "Preparing your delayed file";
            }

            @Override
            public void updateValues(long current, long total) {
                super.updateValues(current, total);
                if (startTimeStamp == -1 || lastTotal == -1 || lastTotal != total || lastCurrent == -1 || lastCurrent > current) {
                    lastTotal = total;
                    lastCurrent = current;
                    startTimeStamp = System.currentTimeMillis();
                    // this.setETA(-1);
                    return;
                }
                long currentTimeDifference = System.currentTimeMillis() - startTimeStamp;
                if (currentTimeDifference <= 0) {
                    return;
                }
                long speed = (current * 10000) / currentTimeDifference;
                if (speed == 0) {
                    return;
                }
                long eta = ((total - current) * 10000) / speed;
                this.setETA(eta);
            }
        };
        waitProgress.setIcon(new AbstractIcon(IconKey.ICON_WAIT, 16));
        waitProgress.setProgressSource(this);
        int lastProgress = -1;
        try {
            final int maxWaitSeconds = 300;
            /* 2020-04-16: API docs say checking every 15 seconds is recommended, we always check after 5 seconds. */
            final int waitSecondsPerLoop = 5;
            int waitSecondsLeft = maxWaitSeconds;
            /* 0 = initializing (return code on first request), 1 = initializing2 (???), 2 = pending, 3 = done, 4 = error */
            int delayedStatus = 0;
            Integer currentProgress = 0;
            String finalDownloadurl = null;
            do {
                logger.info(String.format("Waiting for file to get loaded onto server - seconds left %d / %d", waitSecondsLeft, maxWaitSeconds));
                link.addPluginProgress(waitProgress);
                waitProgress.updateValues(currentProgress.intValue(), 100);
                this.sleep(waitSecondsPerLoop, link);
                if (currentProgress.intValue() != lastProgress) {
                    // lastProgressChange = System.currentTimeMillis();
                    lastProgress = currentProgress.intValue();
                }
                br.postPageRaw(API_BASE + "/files/check-and-add", String.format("{\"file\":\"%s\"}", link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
                try {
                    /* We have to use the parser here because json contains two 'status' objects ;) */
                    Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    final List<Object> data = (List<Object>) entries.get("data");
                    if (data.size() > 1) {
                        /* This should never happen */
                        logger.warning("WTF data array contains more than one item");
                    }
                    entries = (Map<String, Object>) data.get(0);
                    entries = (Map<String, Object>) entries.get("file");
                    Map<String, Object> statusmap = (Map<String, Object>) entries.get("status");
                    delayedStatus = (int) JavaScriptEngineFactory.toLong(statusmap.get("code"), 0);
                    finalDownloadurl = (String) entries.get("download");
                    final int tmpCurrentProgress = (int) JavaScriptEngineFactory.toLong(statusmap.get("percentages"), 0);
                    if (tmpCurrentProgress > currentProgress) {
                        /* Do not allow the progress to "go back" - rather leave it stuck! */
                        currentProgress = tmpCurrentProgress;
                    }
                } catch (final Throwable e) {
                    logger.info("Error parsing json response");
                    break;
                }
                waitSecondsLeft -= waitSecondsPerLoop;
            } while (delayedStatus != 3 && delayedStatus != 4);
            /* 2020-04-16: Downloadurl may always be available! Be sure to only return it on correct status!! It will not work otherwise! */
            if (delayedStatus == 3) {
                logger.info("Downloadurl should be available");
                return finalDownloadurl;
            } else {
                /* Either delayedStatus == 4 --> Error or timeout reached */
                if (delayedStatus == 4) {
                    logger.info("Error happened during serverside download");
                } else if (waitSecondsLeft <= 0) {
                    logger.info(String.format("Timeout happened during serverside download - exceeded %d seconds", maxWaitSeconds));
                } else {
                    logger.info("Unknown error happened during serverside download");
                }
                return null;
            }
        } finally {
            link.removePluginProgress(waitProgress);
        }
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        String dllink = checkDirectLink(link, this.getHost() + PROPERTY_directlink);
        if (dllink == null) {
            this.loginAPI(account, false, false);
            /*
             * 2020-04-16: We'll use the cacheChecker for all URLs as we do not know which ones provide direct downloads and which ones
             * don't (well we could know this before but the current method is safer as the status of such hosts could change at any time).
             */
            dllink = cacheDLChecker(link);
            if (StringUtils.isEmpty(dllink)) {
                handleErrors(this.br, account, link);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "dllinknull", 5 * 60 * 1000);
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, defaultRESUME, defaultMAXCHUNKS);
        link.setProperty(this.getHost() + PROPERTY_directlink, dl.getConnection().getURL().toExternalForm());
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            handleErrors(this.br, account, link);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown download error", 5 * 60 * 1000);
        }
        this.dl.startDownload();
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                /* 2020-04-16: HeadRequest is not possible! */
                con = br2.openGetConnection(dllink);
                if (con.isContentDisposition()) {
                    return dllink;
                } else {
                    return null;
                }
            } catch (final Exception e) {
                logger.log(e);
                dllink = null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> userinfo = loginAPI(account, true, true);
        final AccountInfo ai = new AccountInfo();
        final String trafficleft = PluginJSonUtils.getJson(br, "transfer");
        if (trafficleft == null || !trafficleft.matches("\\d+")) {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(defaultMAXDOWNLOADS);
        } else {
            /*
             * Basically all accounts are premium - they only have traffic based accounts so instead of expiring, they will be out of
             * traffic at some point.
             */
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(defaultMAXDOWNLOADS);
            ai.setTrafficLeft(Long.parseLong(trafficleft));
        }
        br.getPage(API_BASE + "/services");
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("data");
        for (final Map<String, Object> hostinfo : ressourcelist) {
            final MultiHostHost mhost = new MultiHostHost();
            mhost.setName(hostinfo.get("name").toString());
            // final boolean download_by_direct = ((Boolean) entries.get("download_by_direct")).booleanValue();
            if (Boolean.FALSE.equals(hostinfo.get("is_active"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            mhost.setDomains((List<String>) hostinfo.get("domains"));
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private Map<String, Object> loginAPI(final Account account, final boolean forceAuthCheck, final boolean forceAccessUserInfoPage) throws IOException, PluginException, InterruptedException {
        String token = account.getStringProperty(PROPERTY_logintoken);
        final String urlUserInfo = "/users";
        /* 2020-04-15: Token expires after max 14 days but can get invalid at any time --> Refresh every 45 minutes */
        final int token_refresh_minutes = 45;
        final boolean needs_token_refresh = System.currentTimeMillis() - account.getCookiesTimeStamp("") >= token_refresh_minutes * 60 * 1000l;
        Map<String, Object> entries = null;
        if (token != null && needs_token_refresh) {
            logger.info(String.format("Token needs to be refreshed as it is older than %d minutes", token_refresh_minutes));
        } else if (token != null) {
            logger.info("Attempting token login");
            br.getHeaders().put("Authorization", "Bearer " + token);
            if (!forceAuthCheck) {
                /* Do not check token */
                return null;
            }
            br.getPage(API_BASE + urlUserInfo);
            try {
                entries = this.handleErrors(br, account, null);
                logger.info("Token login successful");
                return entries;
            } catch (final Throwable e) {
                logger.log(e);
                logger.info("Token login failed");
            }
        }
        /* Clear previous headers & cookies */
        logger.info("Performing full login");
        final String postData = String.format("{\"username\": \"%s\",\"password\": \"%s\"}", account.getUser(), account.getPass());
        br.postPageRaw(API_BASE + "/auth/login", postData);
        entries = this.handleErrors(br, account, null);
        token = PluginJSonUtils.getJson(br, "access_token");
        if (StringUtils.isEmpty(token)) {
            handleErrors(br, account, null);
            /* This should never happen - do not permanently disable accounts for unexpected login errors! */
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "Unknown login failure", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
        }
        logger.info("Login successful");
        account.setProperty(PROPERTY_logintoken, token);
        /* We don't really need the cookies but the timestamp ;) */
        account.saveCookies(br.getCookies(br.getHost()), "");
        br.getHeaders().put("Authorization", "Bearer " + token);
        if (forceAccessUserInfoPage) {
            br.getPage(API_BASE + urlUserInfo);
            entries = this.handleErrors(br, account, null);
        }
        return entries;
    }

    private Map<String, Object> handleErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final int responsecode = br.getHttpConnection().getResponseCode();
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Object codeO = entries.get("code");
        if (!(codeO instanceof Number)) {
            /* No error */
            return entries;
        }
        final int errorcode = ((Number) codeO).intValue();
        String errormsg = entries.get("message").toString();
        if (StringUtils.isEmpty(errormsg)) {
            errormsg = "Unknown error";
        }
        if (responsecode == 401) {
            /* Login invalid */
            if (errormsg.equalsIgnoreCase("TOKEN_EXPIRED")) {
                /* Indicates that the account did work before --> Only temp. disable it - token should be auto-refreshed on next check! */
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        } else if (responsecode == 403) {
            /* Account blocked or not yet activated */
            throw new AccountUnavailableException(errormsg, 1 * 60 * 1000l);
        } else if (responsecode == 423) {
            /* TODO: Add API login captcha handling if possible */
            /*
             * E.g. {"message":"Adres z kt\u00f3rego si\u0119 \u0142\u0105czysz zosta\u0142 zablokowany","data":{"captcha-public-key":
             * "6LdSiKUUAAAAALCIB4OPOc4eIc4JA8JRbKD-yIuW"}}
             */
            if (StringUtils.isEmpty(errormsg)) {
                errormsg = "Login captcha required";
            }
            throw new AccountUnavailableException(errormsg, 5 * 60 * 1000l);
        }
        if (link == null && errorcode != -1) {
            /* {"message":"A user with the specified credentials was not found","code":2038} */
            if (responsecode == 404 || errorcode == 2038) {
                /* Invalid logindata */
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                /* Unknown account error */
                throw new AccountUnavailableException(errormsg, 3 * 60 * 1000l);
            }
        } else {
            /* Handle download errors ONLY */
            if (errorcode == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, 3 * 60 * 1000l);
            }
        }
        final String errorcode_urlStr = new Regex(br.getURL(), "/error/(\\d+)").getMatch(0);
        if (errorcode_urlStr != null) {
            /*
             * 2020-04-21: There are about 10 URL-errorcodes available but really all are "Download failed because of XY" so this is the
             * only one we can/should handle.
             */
            if (errorcode_urlStr.equals("8")) {
                /* Expired session */
                throw new AccountUnavailableException("Session expired", 1 * 60 * 1000l);
            }
        }
        if (link == null) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, 3 * 60 * 1000l);
        } else {
            throw new AccountUnavailableException(errormsg, 3 * 60 * 1000);
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}