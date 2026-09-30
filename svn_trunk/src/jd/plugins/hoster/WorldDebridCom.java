//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
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

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 53511 $", interfaceVersion = 3, names = { "world-debrid.com" }, urls = { "" })
public class WorldDebridCom extends PluginForHost {
    private final String                 API_BASE               = "https://world-debrid.com/api/v1";

    private static MultiHosterManagement mhm                    = new MultiHosterManagement("world-debrid.com");
    private final String                 PROPERTY_ACCESS_TOKEN  = "access_token";
    private final String                 PROPERTY_TOKEN_EXPIRES = "access_token_expires";
    private final String                 PROPERTY_DIRECTURL     = "worlddebrid_directurl_";

    /* The API answered 402: no active subscription on this account. */
    private static class SubscriptionRequiredException extends Exception {
        private static final long serialVersionUID = 1L;

        private SubscriptionRequiredException(final String message) {
            super(message);
        }
    }

    public WorldDebridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://world-debrid.com/plans?utm_source=jdownloader&utm_medium=affiliate");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://world-debrid.com/auth/account";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        return str != null && str.trim().matches("wdu_live_[a-f0-9]{48}");
    }

    @Override
    public String getAGBLink() {
        return "https://world-debrid.com/terms";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookiesExclusive(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader:" + getVersion());
        br.setFollowRedirects(true);
        return br;
    }

    private Browser brapi = null;

    @Override
    public void clean() {
        brapi = null;
        super.clean();
    }

    private Browser api() {
        Browser ret = brapi;
        if (ret != null) {
            return ret;
        }
        ret = createNewBrowserInstance();
        ret.setAllowedResponseCodes(400, 401, 402, 403, 404, 429, 502, 503);
        return brapi = ret;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (!AccountType.PREMIUM.is(account)) {
            return false;
        }
        return super.canHandle(link, account);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* This host has no files of its own: it is a multihoster only. */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /* Connections per file, sent by the API (max_connections): staying under our per-account cap avoids 503s. */
    private final String PROPERTY_MAXCHUNKS = "worlddebrid_maxchunks";
    private final int    DEFAULT_MAXCHUNKS  = 4;

    private int getMaxChunks(final DownloadLink link, final Account account) {
        final int maxchunks = Math.max(1, account.getIntegerProperty(PROPERTY_MAXCHUNKS, DEFAULT_MAXCHUNKS));
        if (maxchunks > 1) {
            return -maxchunks;
        }
        return 1;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        /* A direct URL belongs to the account that resolved it. */
        final String directurlProperty = PROPERTY_DIRECTURL + accountFingerprint(account);
        String dllink = link.getStringProperty(directurlProperty);
        final boolean stored = dllink != null;
        if (!stored) {
            final Map<String, Object> body = new HashMap<String, Object>();
            body.put("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
            final Map<String, Object> data;
            try {
                data = callAPI(account, link, "/links/resolve", body);
            } catch (final SubscriptionRequiredException e) {
                throw new AccountUnavailableException(e.getMessage(), TimeUnit.MINUTES.toMillis(30));
            }
            dllink = (String) data.get("url");
            if (StringUtils.isEmpty(dllink)) {
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 10, TimeUnit.MINUTES.toMillis(5));
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 503) {
                    /*
                     * Too many parallel transfers on this account: wait on this link only, the account and the host are fine. The direct
                     * URL stays valid, keep it so the retry does not unlock the link again.
                     */
                    link.setProperty(directurlProperty, dllink);
                    final String retryAfter = dl.getConnection().getHeaderField("Retry-After");
                    final long wait = retryAfter != null && retryAfter.matches("\\d+") ? TimeUnit.SECONDS.toMillis(Long.parseLong(retryAfter)) : TimeUnit.SECONDS.toMillis(30);
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Too many parallel downloads", wait);
                }
                if (stored) {
                    link.removeProperty(directurlProperty);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired");
                }
                mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to downloadable content", 10, TimeUnit.MINUTES.toMillis(5));
            }
        } catch (final Exception e) {
            final boolean busy = e instanceof PluginException && ((PluginException) e).getLinkStatus() == LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE;
            if (stored && !busy) {
                link.removeProperty(directurlProperty);
            }
            throw e;
        }
        link.setProperty(directurlProperty, dllink);
        dl.startDownload();
    }

    @SuppressWarnings("unchecked")
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        synchronized (account) {
            final AccountInfo ai = new AccountInfo();
            /* The API gives no username: show a fingerprint of the key, never a part of it, so two keys stay two accounts. */
            account.setUser("World-Debrid " + accountFingerprint(account));
            final Map<String, Object> user;
            try {
                user = callAPI(account, null, "/account", null);
            } catch (final SubscriptionRequiredException e) {
                account.setType(AccountType.FREE);
                ai.setExpired(true);
                ai.setTrafficLeft(0);
                ai.setStatus(e.getMessage());
                return ai;
            }
            if (!Boolean.TRUE.equals(user.get("plan_active"))) {
                account.setType(AccountType.FREE);
                ai.setExpired(true);
                ai.setTrafficLeft(0);
                return ai;
            }
            account.setType(AccountType.PREMIUM);
            /* Parallel limits decided by World-Debrid: max_downloads files, max_connections chunks each (0 = no file limit). */
            final Number maxDownloads = (Number) user.get("max_downloads");
            account.setMaxSimultanDownloads(maxDownloads != null && maxDownloads.intValue() > 0 ? maxDownloads.intValue() : -1);
            final Number maxConnections = (Number) user.get("max_connections");
            account.setProperty(PROPERTY_MAXCHUNKS, maxConnections);
            final String premiumUntil = (String) user.get("premium_until");
            if (premiumUntil != null) {
                ai.setValidUntil(TimeFormatter.getMilliSeconds(premiumUntil, "yyyy-MM-dd'T'HH:mm:ssXXX", Locale.ENGLISH), api());
            }
            final Map<String, Object> quota = (Map<String, Object>) user.get("quota");
            if (quota != null && quota.get("daily_gb_limit") != null) {
                final double one_gb = 1024d * 1024d * 1024d;
                final long max = (long) (((Number) quota.get("daily_gb_limit")).doubleValue() * one_gb);
                final long used = (long) (((Number) quota.get("daily_gb_used")).doubleValue() * one_gb);
                ai.setTrafficMax(max);
                ai.setTrafficLeft(Math.max(0, max - used));
            } else {
                ai.setUnlimitedTraffic();
            }
            final Map<String, Object> hostsData = callAPI(account, null, "/hosts", null);
            final List<MultiHostHost> hosts = new ArrayList<MultiHostHost>();
            for (final Object host : (List<Object>) hostsData.get("hosts")) {
                hosts.add(new MultiHostHost(host.toString()));
            }
            account.setConcurrentUsePossible(true);
            ai.setMultiHostSupportV2(this, hosts);
            return ai;
        }
    }

    private static String accountFingerprint(final Account account) {
        final String key = account.getPass() == null ? "" : account.getPass().trim();
        return Hash.getMD5(Hash.getSHA256(key)).substring(0, 4);
    }

    /** Access token of this account, exchanged from the user's link token (API key field) and cached until it expires. */
    private String getAccessToken(final Account account, final boolean forceNew) throws Exception {
        synchronized (account) {
            final String cached = account.getStringProperty(PROPERTY_ACCESS_TOKEN);
            if (!forceNew && cached != null && account.getLongProperty(PROPERTY_TOKEN_EXPIRES, 0) > System.currentTimeMillis() + TimeUnit.MINUTES.toMillis(1)) {
                return cached;
            }
            final String linkToken = account.getPass() == null ? "" : account.getPass().trim();
            if (!looksLikeValidAPIKey(linkToken)) {
                throw new AccountInvalidException("Enter your World-Debrid link token (wdu_live_...). Create it on world-debrid.com, account page, Integrations, JDownloader.");
            }
            final Map<String, Object> body = new HashMap<String, Object>();
            body.put("link_token", linkToken);
            final Browser api = api();
            final Request req = api.createJSonPostRequest(API_BASE + "/auth/token", JSonStorage.serializeToJson(body));
            /* Identifies JDownloader as a client. Not a secret: every request also needs the user's own link token. */
            req.getHeaders().put("Authorization", "Bearer " + "wdp_live_70478c57ad653b34cd4dd27c46fd8462eba5ad05a12e7a00");
            api.getPage(req);
            final Map<String, Object> data = handleResponse(account, api, null);
            final String token = (String) data.get("access_token");
            final Number expiresIn = (Number) data.get("expires_in");
            account.setProperty(PROPERTY_ACCESS_TOKEN, token);
            account.setProperty(PROPERTY_TOKEN_EXPIRES, System.currentTimeMillis() + (expiresIn == null ? 3600 : expiresIn.longValue()) * 1000l);
            return token;
        }
    }

    /** GET when postJson is null, POST otherwise. A rejected access token is renewed once. */
    private Map<String, Object> callAPI(final Account account, final DownloadLink link, final String path, final Map<String, Object> json) throws Exception {
        for (int attempt = 0; attempt < 2; attempt++) {
            final String token = getAccessToken(account, attempt > 0);
            final Browser api = api();
            final Request req = json == null ? api.createGetRequest(API_BASE + path) : api.createJSonPostRequest(API_BASE + path, json);
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + token);
            api.getPage(req);
            if (api.getHttpConnection().getResponseCode() == 401 && attempt == 0) {
                account.removeProperty(PROPERTY_ACCESS_TOKEN);
                if (link != null) {
                    sleep(1000, link);
                } else {
                    Thread.sleep(1000);
                }
                continue;
            }
            return handleResponse(account, api, link);
        }
        throw new AccountInvalidException();
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> handleResponse(final Account account, final Browser br, final DownloadLink link) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            if (link != null) {
                mhm.handleErrorGeneric(account, link, "Invalid API response", 10, TimeUnit.MINUTES.toMillis(5));
            }
            throw new AccountUnavailableException(e, "Invalid API response", TimeUnit.MINUTES.toMillis(5));
        }
        if (Boolean.TRUE.equals(entries.get("success"))) {
            return (Map<String, Object>) entries.get("data");
        }
        final String message = entries.get("error") == null ? "Unknown error" : entries.get("error").toString();
        final boolean retryable = Boolean.TRUE.equals(entries.get("retryable"));
        final int code = br.getHttpConnection().getResponseCode();
        switch (code) {
        case 401:
            /* Link token unknown or revoked. */
            account.removeProperty(PROPERTY_ACCESS_TOKEN);
            throw new AccountInvalidException(message);
        case 402:
            /* No active subscription, or daily fair-use exhausted. */
            throw new SubscriptionRequiredException(message);
        case 403:
            /* Account suspended, or the same key used from too many places in 24 hours. */
            throw new AccountUnavailableException(message, TimeUnit.HOURS.toMillis(1));
        case 429: {
            final String retryAfter = br.getRequest().getResponseHeader(HTTPConstants.HEADER_RESPONSE_RETRY_AFTER);
            final long wait = retryAfter != null && retryAfter.matches("\\d+") ? TimeUnit.SECONDS.toMillis(Long.parseLong(retryAfter)) : TimeUnit.MINUTES.toMillis(1);
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, message, wait);
            }
            throw new AccountUnavailableException(message, wait);
        }
        case 503:
            throw new AccountUnavailableException(message, TimeUnit.HOURS.toMillis(1));
        default:
            if (link == null) {
                throw new AccountUnavailableException(message, TimeUnit.MINUTES.toMillis(5));
            } else if (retryable) {
                mhm.handleErrorGeneric(account, link, message, 10, TimeUnit.MINUTES.toMillis(5));
            } else {
                /* This link cannot be resolved by World-Debrid: let JDownloader try another account or the host itself. */
                mhm.putError(account, link, TimeUnit.HOURS.toMillis(1), message);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

}