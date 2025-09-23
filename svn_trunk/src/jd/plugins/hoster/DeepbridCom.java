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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
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

@HostPlugin(revision = "$Revision: 51543 $", interfaceVersion = 3, names = {}, urls = {})
public class DeepbridCom extends PluginForHost {
    private static final String          API_BASE                   = "https://www.deepbrid.com/backend-dl/index.php";
    private static MultiHosterManagement mhm                        = new MultiHosterManagement("deepbrid.com");
    private static final int             defaultMAXCHUNKS           = 1;
    private static final boolean         defaultRESUME              = true;
    /* DownloadLink properties */
    private static final String          PROPERTY_ACCOUNT_maxchunks = "maxchunks";
    private static final String          PROPERTY_TICKET_URL        = "ticketurl";
    /* Supported links patterns */
    public static final Pattern          PATTERN_F                  = Pattern.compile("/dl\\?f=([a-f0-9]{32})", Pattern.CASE_INSENSITIVE);
    public static final Pattern          PATTERN_TORRENT            = Pattern.compile("/mytorrents\\?torrent=(\\d+)&file=([a-zA-Z0-9]+)", Pattern.CASE_INSENSITIVE);

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
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
        this.enablePremium("https://www." + getHost() + "/signup");
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/page/terms";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookiesExclusive(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        /* See handleRedirects */
        br.setFollowRedirects(false);
        return br;
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
        if (!link.isNameSet()) {
            /* Fallback-filename */
            final String weak_filename = this.getFID(link);
            link.setName(weak_filename);
        }
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
            getPage(br, contenturl);
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
                logger.info("Torrent cloud files can only be checked when account is available!");
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
            /* Without account it is only possible to download URLs for files which are on the server of this multihost! */
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
                    getPage(br, ticketurl);
                } else {
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
                    submitForm(br, dlform);
                    logger.info("Newly generated Ticket-URL: " + br.getURL());
                    ;
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
            brc.setFollowRedirects(true);
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
            if (account.getType() == AccountType.PREMIUM) {
                /* Use API in premium mode */
                postPage(br, API_BASE + "?page=api&app=jdownloader&action=generateLink", "pass=&link=" + Encoding.urlEncode(link.getPluginPatternMatcher()));
            } else {
                /* Use website for free account downloads */
                postPage(br, API_BASE + "?page=api&action=generateLink", "pass=&link=" + Encoding.urlEncode(link.getPluginPatternMatcher()));
            }
            final Map<String, Object> resp = this.handleErrorsAPI(br, account, link);
            dllink = resp.get("link").toString();
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen! */
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 10, 20 * 60 * 1000l);
            }
        }
        link.setProperty(directlinkproperty, dllink);
        int maxchunks = account.getIntegerProperty(PROPERTY_ACCOUNT_maxchunks, defaultMAXCHUNKS);
        if (maxchunks == 1) {
            maxchunks = 1;
        } else if (maxchunks > 0) {
            maxchunks = -maxchunks;
        } else {
            maxchunks = defaultMAXCHUNKS;
        }
        logger.info("Max. allowed chunks: " + maxchunks);
        try {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
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
        try {
            this.dl.startDownload();
        } catch (final Exception e) {
            /* Special errorhandling, see https://board.jdownloader.org/showthread.php?t=80906 */
            final File file = new File(dl.getDownloadable().getFileOutputPart());
            if (file.exists() && file.length() < 5000) {
                final String content = IO.readFileToString(file);
                if (StringUtils.containsIgnoreCase(content, "VinaGet")) {
                    logger.log(e);
                    logger.info("ServerError workaround: VinaGet");
                    link.setChunksProgress(null);
                    file.delete();
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "ServerError, retry later!", 15 * 60 * 1000l, e);
                }
            }
            throw e;
        }
    }

    private void getPage(final Browser br, final String url) throws IOException {
        br.getPage(url);
        handleRedirects(br);
    }

    private void postPage(final Browser br, final String url, final String postData) throws IOException {
        br.postPage(url, postData);
        handleRedirects(br);
    }

    private void submitForm(final Browser br, final Form form) throws IOException {
        br.submitForm(form);
        handleRedirects(br);
    }

    /**
     * Special handling as API requests [when user is not logged in] may resul in json response but at the same time return a 302 redirect
     * to a html page which we do not want.
     */
    private void handleRedirects(final Browser br) throws IOException {
        final String redirect = br.getRedirectLocation();
        if (redirect == null) {
            return;
        }
        if (br.getURL().contains(API_BASE) || StringUtils.containsIgnoreCase(br.getURL(), API_BASE.replaceFirst("www\\.", ""))) {
            logger.info("Ignoring redirect to keep json in browser | Redirect: " + redirect);
        } else {
            br.followRedirect(true);
        }
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        handleDL(account, link);
    }

    @SuppressWarnings({ "unchecked" })
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> userinfo = login(account, true);
        Number points = (Number) userinfo.get("points");
        if (points == null) {
            points = (Number) userinfo.get("fidelity_points");
        }
        if (points != null) {
            final int pointsInt = points.intValue();
            ai.setPremiumPoints(pointsInt);
        }
        final String type = userinfo.get("type").toString();
        final Number maxSimultaneousDownloads = (Number) userinfo.get("maxDownloads");
        if ("premium".equalsIgnoreCase(type)) {
            account.setType(AccountType.PREMIUM);
            if (maxSimultaneousDownloads != null) {
                logger.info("Using API maxdownloads: " + maxSimultaneousDownloads);
                account.setMaxSimultanDownloads(maxSimultaneousDownloads.intValue());
            } else {
                /* Set to default/unlimited */
                account.setMaxSimultanDownloads(-1);
            }
            final String validuntil = userinfo.get("expiration").toString();
            /* Correct expire-date - add 24 hours */
            ai.setValidUntil(TimeFormatter.getMilliSeconds(validuntil, "yyyy-MM-dd", Locale.ENGLISH) + 24 * 60 * 60 * 1000, br);
            ai.setUnlimitedTraffic();
        } else {
            account.setType(AccountType.FREE);
            /*
             * 2021-01-03: Usually there are 15 Minutes wait time between downloads in free mode -> Do not allow simultaneous downloads or
             * no downloads at all.
             */
            account.setMaxSimultanDownloads(1);
            /**
             * No downloads possible via free account via API. <br>
             * Via website, free downloads are possible but we were too lazy to add extra login support via website in order to allow free
             * account downloads.
             */
            ai.setTrafficLeft(0);
            ai.setExpired(true); // 2023-07-21
        }
        final Number maxConnections = (Number) userinfo.get("maxConnections");
        if (maxConnections != null) {
            logger.info("Setting maxchunks value: " + maxConnections);
            account.setProperty(PROPERTY_ACCOUNT_maxchunks, maxConnections.intValue());
        }
        getPage(br, API_BASE + "?page=api&action=hosters");
        final List<Object> supportedhostslistO;
        final Object supportedhostsO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (supportedhostsO instanceof Map) {
            /*
             * 2023-11-13: A map of string objects called "0", "1" an so on. Basically they are returnign a map of what is supposed to be an
             * array.
             */
            supportedhostslistO = new ArrayList<Object>();
            final Iterator<Entry<String, Object>> iterator = ((Map<String, Object>) supportedhostsO).entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                supportedhostslistO.add(entry.getValue());
            }
        } else {
            supportedhostslistO = (List<Object>) supportedhostsO;
        }
        final HashMap<String, MultiHostHost> crippledDomainToMultihostHost = new HashMap<String, MultiHostHost>();
        final Map<String, String> domainWorkarounds = new HashMap<String, String>();
        /* 2020-05-20: Workaround: https://board.jdownloader.org/showthread.php?t=84429 */
        domainWorkarounds.put("icerbox", "icerbox.com");
        domainWorkarounds.put("filestore", "filestore.me");
        final Map<String, String> hostsToDownSinceDate = new HashMap<String, String>();
        for (final Object hostO : supportedhostslistO) {
            /* List can be given in two different varieties */
            if (!(hostO instanceof Map)) {
                logger.warning("Found invalid host object: " + hostO);
                continue;
            }
            final Map<String, Object> entries = (Map<String, Object>) hostO;
            for (final Map.Entry<String, Object> entry : entries.entrySet()) {
                final MultiHostHost mhost = new MultiHostHost();
                final boolean isOnline;
                String downSinceDate = null;
                final String onlineStatus = (String) entry.getValue();
                if ("up".equalsIgnoreCase(onlineStatus)) {
                    isOnline = true;
                } else {
                    isOnline = false;
                    downSinceDate = new Regex(onlineStatus, "down \\((.+)\\)$").getMatch(0);
                }
                final String[] domains = entry.getKey().split(",");
                for (String domain : domains) {
                    domain = domain.toLowerCase(Locale.ENGLISH);
                    if (crippledDomainToMultihostHost.containsKey(domain)) {
                        continue;
                    }
                    if (!isOnline) {
                        hostsToDownSinceDate.put(domain, downSinceDate);
                    }
                    mhost.addDomain(domain);
                    crippledDomainToMultihostHost.put(domain, mhost);
                    /**
                     * Add domain variations to internal mapping in an attempt to avoid double-entries. <br>
                     * Only do this for domains containing exactly one dot.
                     */
                    final String[] domainParts = domain.split("\\.");
                    if (domainParts.length == 2) {
                        crippledDomainToMultihostHost.put(domainParts[0], mhost);
                    }
                }
            }
        }
        boolean findHostersFromWebsiteSuccess = false;
        findHosterInfoFromWebsite: try {
            /*
             * Neither their API nor their website contains TLDs which is very bad ... but also their API-List and website list differ so
             * this is another workaround ...
             */
            logger.info("Checking for additional supported hosts on website (API list = unreliable)");
            getPage(br, "/service");
            final HashSet<String> crippled_hosts_from_website = new HashSet<String>();
            final String[] crippled_hosts1 = br.getRegex("class=\"tooltip hosters[^\"]+\" title=\"([^\"]+)\"").getColumn(0);
            if (crippled_hosts1 != null && crippled_hosts1.length > 0) {
                crippled_hosts_from_website.addAll(Arrays.asList(crippled_hosts1));
            }
            final String[] crippled_hosts2 = br.getRegex("class=\"hosters [^\"]* tooltip\" title=\"([^\"]+)\"").getColumn(0);
            if (crippled_hosts2 != null && crippled_hosts2.length > 0) {
                crippled_hosts_from_website.addAll(Arrays.asList(crippled_hosts2));
            }
            final String[] crippled_hosts3 = br.getRegex("<div class=\"ml-4 mr-auto\">\\s*<div class=\"font-medium\">([^<]+)</div>").getColumn(0);
            if (crippled_hosts3 != null && crippled_hosts3.length > 0) {
                crippled_hosts_from_website.addAll(Arrays.asList(crippled_hosts3));
            }
            if (crippled_hosts_from_website.isEmpty()) {
                logger.warning("Failed to find any supported hosts on website -> Parser broken?");
                break findHosterInfoFromWebsite;
            }
            for (String crippled_host : crippled_hosts_from_website) {
                crippled_host = crippled_host.toLowerCase(Locale.ENGLISH);
                if (crippledDomainToMultihostHost.containsKey(crippled_host)) {
                    /* Skip items we already found */
                    continue;
                }
                logger.info("Adding host from website which has not been given via API: " + crippled_host);
                final MultiHostHost mhost = new MultiHostHost(crippled_host);
                crippledDomainToMultihostHost.put(crippled_host, mhost);
            }
            /*
             * TODO: For free accounts: parse global free account limits from html such as "max links per day" and "filenext files per day"
             */
            /* Get and set hoster detail limits -> This is where it gets really ugly as parsing their html is hell! */
            final String[] hoster_details_htmls = br.getRegex("<div class=\"ml-4 mr-auto\">(.*?)</div></div>").getColumn(-1);
            if (hoster_details_htmls != null && hoster_details_htmls.length > 0) {
                for (final String hoster_details_html : hoster_details_htmls) {
                    String crippled_host = new Regex(hoster_details_html, "<div class=\"font-medium\">([^<]+)</div>").getMatch(0);
                    if (crippled_host == null) {
                        logger.warning("Possible parser failure: " + hoster_details_html);
                        continue;
                    }
                    crippled_host = Encoding.htmlDecode(crippled_host).trim();
                    crippled_host = crippled_host.toLowerCase(Locale.ENGLISH);
                    boolean isNew = false;
                    boolean foundAndSetIndividualHostLimits = false;
                    MultiHostHost mhost = crippledDomainToMultihostHost.get(crippled_host);
                    if (mhost == null) {
                        mhost = new MultiHostHost(crippled_host);
                        isNew = true;
                    }
                    final String limit_text = new Regex(hoster_details_html, "<div class=\"text-xs font-medium text-right\">([^<]+)</div>").getMatch(0);
                    setIndividualHostTrafficLimits: if (limit_text != null) {
                        if (!limit_text.contains("/")) {
                            logger.warning("Cannot parse limit string: " + limit_text);
                            break setIndividualHostTrafficLimits;
                        }
                        if (StringUtils.containsIgnoreCase(limit_text, "links")) {
                            final String[] link_limits_list = limit_text.replaceFirst("(?i)\\s*links", "").split("/");
                            final String link_limit_used_str = link_limits_list[0].trim();
                            final String link_limit_max_str = link_limits_list[1].trim();
                            if (!link_limit_used_str.matches("\\d+")) {
                                logger.warning("Cannot parse links used string: " + link_limit_used_str);
                                break setIndividualHostTrafficLimits;
                            }
                            if (!link_limit_max_str.matches("\\d+")) {
                                logger.warning("Cannot parse links max string: " + link_limit_max_str);
                                break setIndividualHostTrafficLimits;
                            }
                            final long links_max = Long.parseLong(link_limit_max_str);
                            final long links_used = Long.parseLong(link_limit_used_str);
                            mhost.setLinksMax(links_max);
                            mhost.setLinksLeft(links_max - links_used);
                        } else {
                            final String[] traffic_limit_list = limit_text.split("/");
                            final String traffic_limit_used_str = traffic_limit_list[0].trim();
                            final String traffic_limit_max_str = traffic_limit_list[1].trim();
                            if (!traffic_limit_used_str.matches("\\d+.*")) {
                                logger.warning("Cannot parse traffic used string: " + traffic_limit_used_str);
                                break setIndividualHostTrafficLimits;
                            }
                            if (!traffic_limit_max_str.matches("\\d+.*")) {
                                logger.warning("Cannot parse traffic max string: " + traffic_limit_max_str);
                                break setIndividualHostTrafficLimits;
                            }
                            final long traffic_max_bytes = SizeFormatter.getSize(traffic_limit_max_str);
                            final long traffic_used_bytes = SizeFormatter.getSize(traffic_limit_used_str);
                            mhost.setTrafficMax(traffic_max_bytes);
                            mhost.setTrafficLeft(traffic_max_bytes - traffic_used_bytes);
                        }
                        foundAndSetIndividualHostLimits = true;
                    }
                    if (isNew) {
                        logger.info("Added domain found only in individual limits text: " + crippled_host);
                        crippledDomainToMultihostHost.put(crippled_host, mhost);
                    } else if (foundAndSetIndividualHostLimits) {
                        logger.info("Added individual host limits for domain: " + crippled_host);
                    } else {
                        /* Entry hasn't been changed */
                    }
                }
            } else {
                logger.warning("Failed to find host detail limits");
            }
            findHostersFromWebsiteSuccess = true;
        } catch (final Throwable e) {
            logger.log(e);
            logger.warning("Website-workaround to find additional supported hosts failed");
        }
        /* 2025-09-22: Small workaround for their API returning both domains as separate entries. */
        if (crippledDomainToMultihostHost.containsKey("drop.download") && crippledDomainToMultihostHost.containsKey("dropapk.to")) {
            crippledDomainToMultihostHost.remove("dropapk.to");
            crippledDomainToMultihostHost.remove("dropapk");
        }
        if (crippledDomainToMultihostHost.containsKey("drop") && crippledDomainToMultihostHost.containsKey("dropapk")) {
            crippledDomainToMultihostHost.remove("dropapk.to");
            crippledDomainToMultihostHost.remove("dropapk");
        }
        final ArrayList<MultiHostHost> supportedhostsWithoutDupes = new ArrayList<MultiHostHost>();
        for (final MultiHostHost mhost : crippledDomainToMultihostHost.values()) {
            if (supportedhostsWithoutDupes.contains(mhost)) {
                /* Skip dupes */
                continue;
            }
            supportedhostsWithoutDupes.add(mhost);
            /* Fix domains if any domain of this multihost is on our domain workaround list */
            String downSinceDate = null;
            String realDomain = null;
            for (final String domain : mhost.getDomains()) {
                if (downSinceDate == null) {
                    downSinceDate = hostsToDownSinceDate.get(domain);
                }
                if (realDomain == null) {
                    realDomain = domainWorkarounds.get(domain);
                }
            }
            if (realDomain != null) {
                /* Overwrite previously set domains with real ones. */
                logger.info("Corrected domain: " + mhost.getDomain() + " -->" + realDomain);
                mhost.setDomain(realDomain);
            }
            /* Set special down flag if any domain of this entry is down */
            if (downSinceDate != null) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                mhost.setStatusText("Down since " + downSinceDate);
            }
        }
        account.setConcurrentUsePossible(true);
        ai.setMultiHostSupportV2(this, supportedhostsWithoutDupes);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            ai.setStatus(account.getType().getLabel() + " | MaxDls: " + maxSimultaneousDownloads + " MaxCon: " + maxConnections + " | WebsiteHostsParser: " + findHostersFromWebsiteSuccess);
            // if (!findHostersFromWebsiteSuccess) {
            // throw new AccountInvalidException("!!DEV!! Website hosts parser failed!!");
            // }
        }
        return ai;
    }

    private Map<String, Object> login(final Account account, final boolean verifyCookies) throws Exception {
        synchronized (account) {
            /* Load cookies */
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            if (cookies != null || userCookies != null) {
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!verifyCookies) {
                    /* Do not verify cookies */
                    return null;
                }
                logger.info("Trying to login via cookies");
                try {
                    final Map<String, Object> userinfo = checkLoginAPI(br, account, true);
                    logger.info("UserCookie login successful");
                    if (userCookies == null) {
                        /* Only save cookies when needed. */
                        account.saveCookies(br.getCookies(br.getHost()), "");
                    }
                    return userinfo;
                } catch (final PluginException e) {
                    logger.info("Cookie login failed");
                    if (userCookies != null) {
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    } else {
                        logger.info("Cookie login failed --> Full login required");
                        account.clearCookies("");
                    }
                }
            }
            logger.info("Attempting full login");
            getPage(br, "https://www." + account.getHoster());
            getPage(br, "https://www." + account.getHoster() + "/login");
            final Form loginform = br.getFormbyProperty("name", "login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("amember_login", Encoding.urlEncode(account.getUser()));
            loginform.put("amember_pass", Encoding.urlEncode(account.getPass()));
            loginform.put("remember_login", "1");
            if (br.containsHTML("google\\.com/recaptcha/api")) {
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                loginform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            }
            submitForm(br, loginform);
            if (br.containsHTML(">\\s*The user name or password is incorrect\\s*<")) {
                throw new AccountInvalidException();
            }
            final Map<String, Object> userinfo = checkLoginAPI(br, account, false);
            account.saveCookies(br.getCookies(br.getHost()), "");
            return userinfo;
        }
    }

    private Map<String, Object> checkLoginAPI(final Browser br, final Account account, final boolean allowSetUsernameOnAccount) throws Exception {
        final String urlpart = "?page=api&action=accountInfo";
        getPage(br, API_BASE + urlpart);
        if (StringUtils.endsWithCaseInsensitive(br.getRedirectLocation(), "/login")) {
            throw new AccountInvalidException();
        }
        final Map<String, Object> resp = this.handleErrorsAPI(br, account, null);
        final String username = (String) resp.get("username");
        boolean loggedInViaCookies = false;
        if (!StringUtils.isEmpty(username)) {
            loggedInViaCookies = true;
        }
        /* Failure would redirect us to /login */
        final boolean urlOk = br.getURL().contains(urlpart);
        if (loggedInViaCookies && urlOk) {
            if (allowSetUsernameOnAccount) {
                /*
                 * For cookie login user can enter whatever he wants in "username" field. We want unique usernames so user cannot add the
                 * same account twice!
                 */
                account.setUser(username);
            }
            return resp;
        } else {
            throw new AccountInvalidException();
        }
    }

    private Map<String, Object> handleErrorsAPI(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            if (StringUtils.endsWithCaseInsensitive(br.getRedirectLocation(), "/login")) {
                throw new AccountInvalidException();
            } else if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 60 * 1000l, ignore);
            } else {
                throw new AccountUnavailableException(ignore, "Invalid API response", 60 * 1000);
            }
        }
        final Number errorCodeO = (Number) entries.get("error");
        if (errorCodeO == null) {
            /* No error */
            return entries;
        }
        final int errorCode = errorCodeO.intValue();
        final String errorMsg = (String) entries.get("message");
        if (errorCode == 0) {
            /* All ok */
            return entries;
        }
        if (errorCode == 1) {
            /* No link entered - this should never happen */
            mhm.handleErrorGeneric(account, link, "api_error_1", 10, 5 * 60 * 1000l);
        } else if (errorCode == 2) {
            /* http:// or https:// required --> Should never happen(?) */
            mhm.handleErrorGeneric(account, link, "api_error_2", 10, 5 * 60 * 1000l);
        } else if (errorCode == 3) {
            /* Link/Host not supported */
            mhm.putError(account, link, 5 * 60 * 1000l, errorMsg);
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
        } else if (errorCode == 10) {
            /* Filehoster under maintenance on our site */
            mhm.putError(account, link, 5 * 60 * 1000l, errorMsg);
        } else if (errorCode == 15) {
            /* Service detected usage of proxy which they do not tolerate */
            /*
             * 2020-08-18: E.g. {"error":15, "message":"Proxy, VPN or VPS detected. If you think this is a mistake, please \u003Ca
             * href=\"..\/helpdesk\" target=\"_blank\"\u003Ecreate a support ticket\u003C\/a\u003E requesting to whitelist your account, we
             * will be so happy to assist you!" }
             */
            throw new AccountUnavailableException("Proxy, VPN or VPS detected. Contact " + getHost() + " support!", 15 * 60 * 1000l);
        } else {
            /* Unknown error */
            if (link == null) {
                /* Error happened during login */
                throw new AccountUnavailableException(errorMsg, 5 * 60 * 1000l);
            } else {
                mhm.handleErrorGeneric(account, link, errorMsg, 10, 5 * 60 * 1000l);
            }
        }
        /* This code should never be reached. */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }
}