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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.BearerAuthentication;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.InputField;
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
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 52501 $", interfaceVersion = 3, names = {}, urls = {})
public class DeepbridCom extends PluginForHost {
    private static MultiHosterManagement mhm                        = new MultiHosterManagement("deepbrid.com");
    private static final int             defaultMAXCHUNKS           = 1;
    private static final boolean         defaultRESUME              = true;
    /* DownloadLink properties */
    private static final String          PROPERTY_ACCOUNT_maxchunks = "maxchunks";
    private static final String          PROPERTY_TICKET_URL        = "ticketurl";
    /* Supported links patterns */
    public static final Pattern          PATTERN_F                  = Pattern.compile("/dl\\?f=([a-f0-9]{32})", Pattern.CASE_INSENSITIVE);
    public static final Pattern          PATTERN_TORRENT            = Pattern.compile("/mytorrents\\?torrent=(\\d+)&file=([a-zA-Z0-9]+)", Pattern.CASE_INSENSITIVE);

    /** API docs: https://www.deepbrid.com/api-docs */
    private String getApiBase() {
        return "https://www." + getHost() + "/api/v1";
    }

    private String getWebsiteBase() {
        return "https://www." + getHost();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
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
        final String contenturl = link.getPluginPatternMatcher();
        Regex finfo = new Regex(contenturl, PATTERN_TORRENT);
        if (finfo.patternFind()) {
            return finfo.getMatch(0) + "_" + finfo.getMatch(1);
        }
        finfo = new Regex(contenturl, PATTERN_F);
        return finfo.getMatch(0);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "deepbrid.com" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_F.pattern() + "|" + PATTERN_TORRENT.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public DeepbridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(getWebsiteBase() + "/signup");
    }

    @Override
    public String getAGBLink() {
        return getWebsiteBase() + "/page/terms";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookiesExclusive(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        Account account = null;
        if (new Regex(link.getPluginPatternMatcher(), PATTERN_TORRENT).patternFind()) {
            /* Account required to check/download such links -> Try to get account */
            account = AccountController.getInstance().getValidAccount(this.getHost());
        }
        return requestFileInformation(link, account);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        this.setBrowserExclusive();
        final String contenturl = link.getPluginPatternMatcher();
        final Regex regex_f = new Regex(contenturl, PATTERN_F);
        if (regex_f.patternFind()) {
            /**
             * 2025-09-19: Do not login if user owns a premium account! <br>
             * Such links only work for anonymous users and free account users! <br>
             * Logged in users will be redirected to "/service". <br>
             * This is not a bug but intended behavior by the deepbrid admins.
             */
            if (account != null && AccountType.FREE.equals(account.getType())) {
                this.login(account, false);
            }
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML(">\\s*Wrong request code")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String filename = br.getRegex("Filename\\s*</div>\\s*<h2[^>]*>([^<]+)</h2>").getMatch(0);
            final String filesize = br.getRegex("Filesize\\s*</div>\\s*<h2[^>]*>([^<]+)</h2>").getMatch(0);
            if (filename != null) {
                filename = Encoding.htmlDecode(filename).trim();
                link.setName(filename);
            } else {
                logger.warning("Failed to find filename");
            }
            if (filesize != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesize));
            } else {
                logger.warning("Failed to find filesize");
            }
        } else {
            /* PATTERN_TORRENT */
            if (account == null) {
                /* Torrent cloud files can only be checked when account is available */
                return AvailableStatus.UNCHECKABLE;
            }
            this.login(account, false);
            final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
            if (!isDownload) {
                /**
                 * 2025-09-18: HEAD-request takes a very long time to be processed. <br>
                 * I've reported this to their support and switched to using a GET-request until they send me a confirmation that
                 * HEAD-request is working as expected.
                 */
                // this.basicLinkCheck(br, br.createHeadRequest(contenturl), link, null, null);
                this.basicLinkCheck(br, br.createGetRequest(contenturl), link, null, null);
            }
        }
        return AvailableStatus.TRUE;
    }

    protected void throwConnectionExceptions(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null && link.getPluginPatternMatcher() != null && canHandle(link.getPluginPatternMatcher())) {
            /* Without account, it is only possible to download URLs for files which are on the server of this multihost! */
            return true;
        } else {
            return super.canHandle(link, account);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleSelfhostedFileDownload(link, null);
    }

    private void handleSelfhostedFileDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        requestFileInformation(link, account);
        final String contenturl = link.getPluginPatternMatcher();
        final String directlinkproperty = "directurl";
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            if (new Regex(contenturl, PATTERN_TORRENT).patternFind()) {
                /* Direct download */
                if (account == null) {
                    throw new AccountRequiredException("Account required to download torrent cloud files");
                }
                dllink = contenturl;
            } else {
                final String ticketurl = link.getStringProperty(PROPERTY_TICKET_URL);
                if (ticketurl != null) {
                    logger.info("Re-using stored ticketurl: " + ticketurl);
                    br.getPage(ticketurl);
                } else {
                    // TODO: 2026-03-12: Check if this is still working
                    final Form dlform = br.getFormbyKey("download");
                    if (dlform == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    /** 2025-09-18: Captcha not required anymore. */
                    final boolean requiresCaptcha = false;
                    if (requiresCaptcha) {
                        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                        dlform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                    }
                    final InputField dlfield = dlform.getInputField("download");
                    if (dlfield != null && dlfield.getValue() == null) {
                        dlform.put("download", "");
                    }
                    br.submitForm(dlform);
                    logger.info("Newly generated Ticket-URL: " + br.getURL());
                }
                dllink = br.getRegex("(https?://[^\"']+/dl/[^\"']+)").getMatch(0);
                if (StringUtils.isEmpty(dllink)) {
                    dllink = br.getRegex("location\\.href='(https?://[^']+)';\">\\s*<i data-feather=\"download\"").getMatch(0);
                }
                if (StringUtils.isEmpty(dllink)) {
                    if (ticketurl != null) {
                        /* Delete stored ticket-URL and try again! */
                        link.removeProperty(PROPERTY_TICKET_URL);
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Try again with fresh ticket-URL");
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                /*
                 * Store that URL as we can use it multiple times to generate new direct-urls for that particular file without wasting
                 * traffic!
                 */
                link.setProperty(PROPERTY_TICKET_URL, br.getURL());
            }
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = jd.plugins.BrowserAdapter.openDownload(brc, link, dllink, false, 1);
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                brc.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleSelfhostedFileDownload(link, account);
    }

    private void handleDL(final Account account, final DownloadLink link) throws Exception {
        final String directlinkproperty = this.getHost() + "directlink";
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            this.login(account, false);
            final String dlpw = link.getDownloadPassword();
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
            if (dlpw != null) {
                query.appendEncoded("pass", dlpw);
            }
            br.postPage(this.getApiBase() + "/generate/link", query);
            final Map<String, Object> resp = (Map<String, Object>) this.handleErrorsAPI(br, account, link);
            dllink = resp.get("link").toString();
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen! */
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 10, 20 * 60 * 1000l);
            }
        }
        link.setProperty(directlinkproperty, dllink);
        int maxchunks = account.getIntegerProperty(PROPERTY_ACCOUNT_maxchunks, defaultMAXCHUNKS);
        if (maxchunks > 1) {
            maxchunks = -maxchunks;
        }
        logger.info("Max. allowed chunks: " + maxchunks);
        try {
            final Browser brc = br.cloneBrowser();
            dl = jd.plugins.BrowserAdapter.openDownload(brc, link, dllink, defaultRESUME, maxchunks);
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                brc.followConnection(true);
                handleErrorsAPI(brc, account, link);
                mhm.handleErrorGeneric(account, link, "Unknown download error", 10, 5 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        handleDL(account, link);
    }

    @SuppressWarnings({ "unchecked" })
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        try {
            final AccountInfo ai = new AccountInfo();
            final Map<String, Object> user = login(account, true);
            br.getPage(this.getApiBase() + "/user/stats");
            final Map<String, Object> user_stats = (Map<String, Object>) this.handleErrorsAPI(br, account, null);
            br.getPage(this.getApiBase() + "/user/limits");
            final Map<String, Object> user_limits = (Map<String, Object>) this.handleErrorsAPI(br, account, null);
            br.getPage(this.getApiBase() + "/hosts");
            final List<Map<String, Object>> supportedhostslist = (List<Map<String, Object>>) this.handleErrorsAPI(br, account, null);
            final Number points = (Number) user.get("fidelity_points");
            if (points != null) {
                ai.setPremiumPoints(points.intValue());
            }
            String user_account_type = user.get("type").toString();
            if (StringUtils.isEmpty(user_account_type)) {
                /* This should never happen */
                user_account_type = "Unknown";
            }
            final Number maxSimultaneousDownloads = (Number) user.get("maxDownloads");
            if ("premium".equalsIgnoreCase(user_account_type)) {
                account.setType(AccountType.PREMIUM);
                if (maxSimultaneousDownloads != null) {
                    logger.info("Using API maxdownloads: " + maxSimultaneousDownloads);
                    account.setMaxSimultanDownloads(maxSimultaneousDownloads.intValue());
                } else {
                    /* Set to default/unlimited */
                    account.setMaxSimultanDownloads(-1);
                }
                final String validuntil = user.get("expiration").toString();
                /* Correct expire-date - add 24 hours */
                ai.setValidUntil(TimeFormatter.getMilliSeconds(validuntil, "yyyy-MM-dd", Locale.ROOT) + TimeUnit.DAYS.toMillis(1), br);
            } else {
                account.setType(AccountType.FREE);
                /*
                 * 2021-01-03: Usually there are 15 Minutes wait time between downloads in free mode -> Do not allow simultaneous downloads
                 * or no downloads at all.
                 */
                account.setMaxSimultanDownloads(1);
                ai.setExpired(true);
            }
            ai.setUnlimitedTraffic();
            final Number maxConnections = (Number) user.get("maxConnections");
            if (maxConnections != null) {
                logger.info("Setting maxchunks value: " + maxConnections);
                account.setProperty(PROPERTY_ACCOUNT_maxchunks, maxConnections.intValue());
            }
            final Map<String, Object> domain_down_mapping = new HashMap<String, Object>();
            final List<MultiHostHost> mhosts = new ArrayList<MultiHostHost>();
            for (final Map<String, Object> entries : supportedhostslist) {
                /* List can be given in two different varieties */
                for (final Map.Entry<String, Object> entry : entries.entrySet()) {
                    final MultiHostHost mhost = new MultiHostHost();
                    final String onlineStatus = (String) entry.getValue();
                    final String[] domains = entry.getKey().split(",");
                    final String downSinceDate = new Regex(onlineStatus, "(?i)down \\((.+)\\)$").getMatch(0);
                    for (String domain : domains) {
                        domain = domain.toLowerCase(Locale.ROOT);
                        mhost.addDomain(domain);
                        if (downSinceDate != null) {
                            domain_down_mapping.put(domain, downSinceDate);
                        } else if (!onlineStatus.equalsIgnoreCase("up")) {
                            /* Domain is down but without any given "down since date" */
                            domain_down_mapping.put(domain, true);
                        }
                    }
                    mhosts.add(mhost);
                }
            }
            /*
             * Obtain domain limit information. This API request contains only entries that have individual limits. All others have no
             * individual limits.
             */
            final List<Map<String, Object>> hosts_with_individual_limits = (List<Map<String, Object>>) user_limits.get("hosters");
            final Map<String, Map<String, Object>> domain_to_individual_host_limits_map = new HashMap<String, Map<String, Object>>();
            for (final Map<String, Object> host_with_individual_limits : hosts_with_individual_limits) {
                final String[] domains = host_with_individual_limits.get("hoster").toString().split(",");
                for (final String domain : domains) {
                    domain_to_individual_host_limits_map.put(domain.toLowerCase(Locale.ROOT), host_with_individual_limits);
                }
            }
            /**
             * Now bring all information together which we've collected, mainly to: <br>
             * - eliminate duplicates <br>
             * - collect individual host limits and apply them to our MultiHostHost items <br>
             * - collect "down since date" values and apply them to our MultiHostHost items
             */
            for (final MultiHostHost mhost : mhosts) {
                Map<String, Object> individual_host_limit_map = null;
                Object downState = null;
                /*
                 * Same filehost can have multiple domains and we don't know how API manages this so let's go through all domains and check
                 * for a limit map -> First hit wins.
                 */
                for (final String domain : mhost.getDomains()) {
                    if (individual_host_limit_map == null) {
                        individual_host_limit_map = domain_to_individual_host_limits_map.get(domain);
                    }
                    if (downState == null) {
                        downState = domain_down_mapping.get(domain);
                    }
                    if (individual_host_limit_map != null && downState != null) {
                        /* Early-break loop although it is very unlikely that a host has individual limits && is down at this moment. */
                        break;
                    }
                }
                /* Set individual host limits */
                if (individual_host_limit_map != null) {
                    final String limit_type = individual_host_limit_map.get("type").toString();
                    final long remaining = ((Number) individual_host_limit_map.get("remaining")).longValue();
                    final long limit = ((Number) individual_host_limit_map.get("limit")).longValue();
                    if (limit_type.equalsIgnoreCase("bandwidth")) {
                        mhost.setTrafficLeftAndMax(remaining, limit);
                    } else if (limit_type.equalsIgnoreCase("links")) {
                        mhost.setLinksLeftAndMax(remaining, limit);
                    } else {
                        logger.warning("Hoster: " + mhost.getDomain() + " | Found unsupported limit type: " + limit_type);
                    }
                }
                /* Set special down flag if any domain of this entry is down */
                if (downState != null) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                    if (downState instanceof String) {
                        mhost.setStatusText("Down since " + downState);
                    }
                }
            }
            account.setConcurrentUsePossible(true);
            ai.setMultiHostSupportV2(this, mhosts);
            /* Set account status text visible to user in GUI */
            final long stats_bandwidth_bytes = ((Number) user_stats.get("bandwidth_bytes")).longValue();
            final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
            ai.setStatus("Type: " + Character.toUpperCase(user_account_type.charAt(0)) + user_account_type.substring(1) + " | Total Files: " + user_stats.get("downloads") + " | Cloud Torrents: " + user_stats.get("torrents") + " | Bandwidth Used: " + SIZEUNIT.formatValue(maxSizeUnit, stats_bandwidth_bytes));
            final boolean setDebugStatus = false;
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && setDebugStatus) {
                ai.setStatus(account.getType().getLabel() + " | MaxDls: " + maxSimultaneousDownloads + " MaxCon: " + maxConnections);
            }
            return ai;
        } catch (final PluginException pe) {
            /** TODO: Remove this try catch block after 2026-07 */
            if (!this.looksLikeValidAPIKey(account.getPass())) {
                display_api_migration_dialog(account);
            }
            throw pe;
        }
    }

    private Map<String, Object> login(final Account account, final boolean verifyLogin) throws Exception {
        synchronized (account) {
            // br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + account.getPass());
            br.addAuthentication(new BearerAuthentication(getHost(), account.getPass(), null));
            if (!verifyLogin) {
                return null;
            }
            br.getPage(getApiBase() + "/user");
            final Map<String, Object> resp = (Map<String, Object>) this.handleErrorsAPI(br, account, null);
            /*
             * For cookie login user can enter whatever he wants in "username" field. We want unique usernames so user cannot add the same
             * account twice!
             */
            account.setUser(resp.get("username").toString());
            return resp;
        }
    }

    private Object handleErrorsAPI(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final Object resp;
        try {
            resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        } catch (final JSonMapperException ignore) {
            /* Check for non json errors */
            if (StringUtils.endsWithCaseInsensitive(br.getRedirectLocation(), "/login")) {
                throw new AccountInvalidException();
            }
            /* Dead end */
            final String msg = "Invalid API response";
            final long waitMillis = 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis, ignore);
            } else {
                throw new AccountUnavailableException(ignore, msg, waitMillis);
            }
        }
        if (!(resp instanceof Map)) {
            return resp;
        }
        final Map<String, Object> entries = (Map<String, Object>) resp;
        final Number errorCodeO = (Number) entries.get("error");
        if (errorCodeO == null) {
            /* No error */
            return entries;
        }
        final int errorCode = errorCodeO.intValue();
        final String errorMsg = (String) entries.get("message");
        if (errorCode == 0) {
            /* No error */
            return entries;
        }
        if (errorCode == 3) {
            /* Link/Host not supported */
            mhm.putError(account, link, 5 * 60 * 1000l, errorMsg);
            /* This code should never be reached. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (errorCode == 8) {
            /* Account limit reached -> Waittime required */
            /*
             * E.g. {"error":8, "message":"You have already downloaded, wait \u003Cb\u003E 14:11 minutes\u003C\/b\u003E to download again.
             * \u003Ca href=\"..\/signup\" target=\"_blank\"\u003EUpgrade to premium\u003C\/a\u003E and forget waiting times and enjoy
             * unlimited features!" }
             */
            final Regex waittimeRegex = new Regex(errorMsg, ".*You have already downloaded, wait.*(\\d{1,2}):(\\d{1,2}).*minute.*");
            final String waitMinutesStr = waittimeRegex.getMatch(0);
            final String waitSecondsStr = waittimeRegex.getMatch(1);
            if (waitMinutesStr != null && waitSecondsStr != null) {
                throw new AccountUnavailableException(errorMsg, Integer.parseInt(waitMinutesStr) * 60 * 1000l + Integer.parseInt(waitSecondsStr) * 1001l);
            } else {
                throw new AccountUnavailableException(errorMsg, 5 * 60 * 1000l);
            }
        } else if (errorCode == 9) {
            /* Hosters' limit reached for this day */
            mhm.putError(account, link, 5 * 60 * 1000l, errorMsg);
            /* This code should never be reached. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (errorCode == 10) {
            /* Filehoster under maintenance on our site */
            mhm.putError(account, link, 5 * 60 * 1000l, errorMsg);
            /* This code should never be reached. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (errorCode == 15) {
            /* Service detected usage of proxy which they do not tolerate */
            /*
             * 2020-08-18: E.g. {"error":15, "message":"Proxy, VPN or VPS detected. If you think this is a mistake, please \u003Ca
             * href=\"..\/helpdesk\" target=\"_blank\"\u003Ecreate a support ticket\u003C\/a\u003E requesting to whitelist your account, we
             * will be so happy to assist you!" }
             */
            throw new AccountUnavailableException("Proxy, VPN or VPS detected. Contact " + getHost() + " support!", 15 * 60 * 1000l);
        }
        /* 2026-03-13: Example response when user adds API key of free account: {"error":403,"message":"Premium account required"} */
        /* Unknown error */
        if (PluginEnvironment.ACCOUNT_CHECK.isCurrentPluginEnvironment()) {
            throw new AccountInvalidException(errorMsg);
        }
        /* Unknown download error */
        mhm.handleErrorGeneric(account, link, errorMsg, 10, 5 * 60 * 1000l);
        /* This code should never be reached. */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    protected String getAPILoginHelpURL() {
        return getWebsiteBase() + "/devices";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-f0-9]{64}")) {
            return true;
        } else {
            return false;
        }
    }

    /** TODO: Remove the code down below after 2026-07 */
    @Override
    public void validateLogins(final Account account) throws AccountInvalidException {
        try {
            super.validateLogins(account);
        } catch (final AccountInvalidException aie) {
            display_api_migration_dialog(account);
            throw aie;
        }
    }

    private static final String HAS_DISPLAYED_MIGRATION_DIALOG = "HAS_DISPLAYED_MIGRATION_DIALOG";

    /** TODO: Remove the code down below after 2026-07 */
    private void display_api_migration_dialog(final Account account) {
        if (account == null) {
            throw new IllegalArgumentException();
        }
        synchronized (account) {
            if (account.hasProperty(HAS_DISPLAYED_MIGRATION_DIALOG)) {
                /* Message has already been displayed for this account */
                return;
            }
            account.setProperty(HAS_DISPLAYED_MIGRATION_DIALOG, System.currentTimeMillis());
        }
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "<html>";
                    final String title;
                    final String language = System.getProperty("user.language").toLowerCase(Locale.ROOT);
                    if ("de".equals(language)) {
                        title = "Deepbrid: Der Login-Typ hat sich geändert";
                        message += "<br>Ab sofort benötigst du einen API Key, um deinen deepbrid Account in JDownloader verwenden zu können.";
                        message += "<br>Diesen findest du hier: <a href=\"https://www.deepbrid.com/devices\">deepbrid.com/devices</a>";
                    } else if ("es".equals(language)) {
                        title = "Deepbrid: El tipo de inicio de sesión ha cambiado";
                        message += "<br>A partir de ahora necesitas una clave API para usar tu cuenta de deepbrid en JDownloader.";
                        message += "<br>Puedes encontrarla aquí: <a href=\"https://www.deepbrid.com/devices\">deepbrid.com/devices</a>";
                    } else if ("fr".equals(language)) {
                        title = "Deepbrid: Le type de connexion a changé";
                        message += "<br>Désormais, vous avez besoin d'une clé API pour utiliser votre compte deepbrid dans JDownloader.";
                        message += "<br>Vous pouvez la trouver ici : <a href=\"https://www.deepbrid.com/devices\">deepbrid.com/devices</a>";
                    } else {
                        title = "Deepbrid: The login type has changed";
                        message += "<br>From now on you need an API key to use your deepbrid account in JDownloader.";
                        message += "<br>You can find it here: <a href=\"https://www.deepbrid.com/devices\">deepbrid.com/devices</a>";
                    }
                    message += "</html>";
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | Dialog.STYLE_HTML, title, message);
                    dialog.setTimeout(300 * 1000);
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
    }
}