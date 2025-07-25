package org.jdownloader.plugins.components;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.download.HashInfo;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.captcha.v2.challenge.hcaptcha.AbstractHCaptcha;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperHostPluginHCaptcha;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51249 $", interfaceVersion = 2, names = {}, urls = {})
public abstract class TurbobitCore extends PluginForHost {
    /* Settings */
    public static final String             SETTING_FREE_PARALLEL_DOWNLOADSTARTS          = "SETTING_FREE_PARALLEL_DOWNLOADSTARTS";
    public static final String             SETTING_PREFERRED_DOMAIN                      = "SETTING_PREFERRED_DOMAIN";
    private static String                  PROPERTY_DOWNLOADLINK_checked_atleast_onetime = "checked_atleast_onetime";
    private static final int               FREE_MAXDOWNLOADS_PLUGINSETTING               = 20;
    private static final boolean           prefer_single_linkcheck_via_mass_linkchecker  = true;
    private static final String            TYPE_premiumRedirectLinks                     = "(?i)(?:https?://[^/]+/)?/?download/redirect/[A-Za-z0-9]+/([a-z0-9]+)";
    private static Map<String, AtomicLong> hostLastPremiumCaptchaProcessedTimestampMap   = new HashMap<String, AtomicLong>();

    /* Properties */

    /**
     * Override this to define a list of dead domains which should be avoided / when present in added URLs, main domain should be preferred.
     */
    protected List<String> getDeadDomains() {
        return null;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private Browser prepBrowserGeneral(final Browser brc) {
        brc.getHeaders().put("Pragma", null);
        brc.getHeaders().put("Cache-Control", null);
        brc.getHeaders().put("Accept-Charset", null);
        brc.getHeaders().put("Accept", "text/html, application/xhtml+xml, */*");
        brc.getHeaders().put("Accept-Language", "en-EN");
        brc.getHeaders().put("Referer", null);
        brc.setCustomCharset("UTF-8");
        brc.setCookie(getMainpage(), "JD", "1");
        brc.setCookie(getMainpage(), "set_user_lang_change", "en");
        brc.setCookie(getMainpage(), "user_lang", "en"); // 2025-07-02
        return brc;
    }

    private Browser prepBrowserWebsiteV1(final Browser prepBr) {
        prepBrowserGeneral(prepBr);
        br.setCookie(getMainpage(), "site_version", "1");
        return prepBr;
    }

    private Browser prepBrowserWebsiteV2(final Browser br) {
        prepBrowserGeneral(br);
        br.getHeaders().put("Accept", "application/json, text/plain, */*");
        br.setCookie(getMainpage(), "site_version", "2");
        return br;
    }

    public TurbobitCore(final PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
        enablePremium("https://" + getHost() + "/turbo/v2/");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        try {
            final String fuid = getFUID(link);
            return this.getHost() + "://" + fuid;
        } catch (PluginException e) {
            e.printStackTrace();
            return super.getLinkID(link);
        }
    }

    protected boolean isFastLinkcheckEnabled() {
        return true;
    }

    protected abstract boolean allowWebsiteV2Handling();

    /**
     * 2019-05-11: There is also an API-version of this but it seems like it only returns online/offline - no filename/filesize:
     * https://hitfile.net/linkchecker/api
     */
    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        /**
         * Enabled = Do not check for filesize via single-linkcheck on first time linkcheck - only on the 2nd linkcheck and when the
         * filesize is not known already. This will speedup the linkcheck! </br> Disabled = Check for filesize via single-linkcheck even
         * first time links get added as long as no filesize is given. This will slow down the linkcheck and cause more http requests in a
         * short amount of time!
         */
        final boolean fastLinkcheck = isFastLinkcheckEnabled();
        final List<DownloadLink> linksForDeepCheck = new ArrayList<DownloadLink>();
        try {
            final Browser brc = createNewBrowserInstance();
            prepBrowserWebsiteV1(brc);
            brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            brc.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final List<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            brc.getPage("https://" + getConfiguredDomain() + "/linkchecker?site_version=1&from_mirror=1");
            while (true) {
                links.clear();
                while (true) {
                    /* we test 50 links at once */
                    if (index == urls.length || links.size() > 49) {
                        break;
                    }
                    links.add(urls[index]);
                    index++;
                }
                sb.delete(0, sb.capacity());
                sb.append("links_to_check=");
                for (final DownloadLink dl : links) {
                    sb.append(Encoding.urlEncode(this.getContentURL(dl)));
                    sb.append("%0A");
                }
                /* remove last */
                sb.delete(sb.length() - 3, sb.length());
                /*
                 * '/linkchecker/csv' is the official "API" method but this will only return fileID and online/offline - not even the
                 * filename
                 */
                brc.postPage("https://" + brc.getHost() + "/linkchecker/check", sb.toString());
                for (final DownloadLink link : links) {
                    final String file_id = getFUID(link);
                    if (brc.containsHTML("<td>" + file_id + "</td>\\s*<td>\\s*</td>\\s*<td[^>]*>\\s*<img src=\"[^\"]*/error\\.png")) {
                        /* File is offline */
                        link.setAvailable(false);
                    } else {
                        final Regex fileInfo = brc.getRegex("<td>" + file_id + "</td>\\s*(<td>([^<]+)</td>\\s*)?<td style=\"text-align:center;\">\\s*<img src=\"[^\"]*/(done|error)\\.png\"");
                        if (fileInfo.patternFind()) {
                            /* File is online */
                            final String name = fileInfo.getMatch(1);
                            link.setAvailable(true);
                            link.setFinalFileName(Encoding.htmlDecode(name).trim());
                            final boolean checkedBeforeAlready = link.getBooleanProperty(PROPERTY_DOWNLOADLINK_checked_atleast_onetime, false);
                            if (link.getKnownDownloadSize() < 0 && (checkedBeforeAlready || !fastLinkcheck)) {
                                linksForDeepCheck.add(link);
                            }
                            /* Allows it to look for the filesize on 2nd linkcheck. */
                            link.setProperty(PROPERTY_DOWNLOADLINK_checked_atleast_onetime, true);
                        } else {
                            /*
                             * 2020-01-27: E.g.
                             * "<p>Number of requests exceeded the limit. Please wait 5 minutes to check links again</p></div>"
                             */
                            link.setAvailableStatus(AvailableStatus.UNCHECKED);
                            logger.warning("Unable to check link: " + link.getPluginPatternMatcher());
                        }
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        } finally {
            for (final DownloadLink link : linksForDeepCheck) {
                logger.info("Performing deep linkcheck for: " + link.getPluginPatternMatcher());
                try {
                    final AvailableStatus availableStatus = requestFileInformation_Website(link, null);
                    link.setAvailableStatus(availableStatus);
                } catch (PluginException e) {
                    logger.log(e);
                    final AvailableStatus availableStatus;
                    switch (e.getLinkStatus()) {
                    case LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE:
                    case LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE:
                        availableStatus = AvailableStatus.UNCHECKABLE;
                        break;
                    case LinkStatus.ERROR_FILE_NOT_FOUND:
                        availableStatus = AvailableStatus.FALSE;
                        break;
                    case LinkStatus.ERROR_PREMIUM:
                        if (e.getValue() == PluginException.VALUE_ID_PREMIUM_ONLY) {
                            availableStatus = AvailableStatus.UNCHECKABLE;
                            break;
                        }
                    default:
                        availableStatus = AvailableStatus.UNCHECKABLE;
                        break;
                    }
                    link.setAvailableStatus(availableStatus);
                } catch (final Throwable e) {
                    logger.log(e);
                }
            }
        }
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (prefer_single_linkcheck_via_mass_linkchecker && supports_mass_linkcheck()) {
            return requestFileInformation_Mass_Linkchecker(link);
        } else {
            return requestFileInformation_WebsiteV1(link, null);
        }
    }

    public AvailableStatus requestFileInformation_Mass_Linkchecker(final DownloadLink link) throws IOException, PluginException {
        checkLinks(new DownloadLink[] { link });
        if (!link.isAvailabilityStatusChecked()) {
            return AvailableStatus.UNCHECKED;
        } else {
            if (link.isAvailabilityStatusChecked() && !link.isAvailable()) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                return AvailableStatus.TRUE;
            }
        }
    }

    /** Checks links via website, auto decides whether to use websiteV1 or websiteV2. */
    private AvailableStatus requestFileInformation_Website(final DownloadLink link, final Account account) throws Exception {
        if (allowWebsiteV2Handling()) {
            return requestFileInformation_WebsiteV2(link, account);
        } else {
            return requestFileInformation_WebsiteV1(link, account);
        }
    }

    private AvailableStatus requestFileInformation_WebsiteV1(final DownloadLink link, final Account account) throws Exception {
        /* premium links should not be accessed here, we will just return true */
        setBrowserExclusive();
        accessContentURLWebsiteV1(br, link);
        if (isFileOfflineWebsite(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String titlePattern = "<title>\\s*(?:Download\\s+file|Datei\\s+downloaden|Descargar\\s+el\\s+archivo|Télécharger\\s+un\\s+fichier|Scarica\\s+il\\s+file|Pobierz\\s+plik|Baixar\\s+arquivo|İndirilecek\\s+dosya|ファイルのダウンロード)\\s*(.*?)\\s*\\(([\\d\\.,]+\\s*[BMKGTP]{1,2})\\)\\s*\\|\\s*(?:TurboBit|Hitfile)\\.net";
        String fileName = br.getRegex(titlePattern).getMatch(0);
        String fileSize = br.getRegex(titlePattern).getMatch(1);
        if (fileName == null) {
            fileName = br.getRegex("<span class\\s*=\\s*(\"|')file\\-title\\1[^>]*>\\s*(.*?)\\s*</span>").getMatch(1);
            if (StringUtils.contains(fileName, "...")) {
                final String[] split = fileName.split("\\.\\.\\.");
                if (split.length == 2) {
                    final String customTitlePattern = "<title>\\s*[^<]*\\s*(" + Pattern.quote(split[0]) + ".*?" + Pattern.quote(split[1]) + ")\\s*\\(([\\d\\., ]+\\s*[BMKGTP]{1,2}\\s*)\\)";
                    final String fromTitle = br.getRegex(customTitlePattern).getMatch(0);
                    if (fromTitle != null && fromTitle.length() > fileName.length()) {
                        fileName = fromTitle;
                    }
                    if (fileSize == null) {
                        fileSize = br.getRegex(customTitlePattern).getMatch(1);
                    }
                }
            }
        }
        if (fileSize == null) {
            /* E.g. for hitfile.net, filesize is in brakets '(")(")' */
            fileSize = br.getRegex("class\\s*=\\s*\"file-size\"\\s*>\\s*\\(?\\s*([^<]*?)\\s*\\)?\\s*<").getMatch(0);
        }
        if (fileName != null) {
            link.setName(fileName);
        }
        if (fileSize != null) {
            fileSize = fileSize.replace("М", "M");
            fileSize = fileSize.replace("к", "k");
            fileSize = fileSize.replace("Г", "g");
            fileSize = fileSize.replace("б", "");
            if (!fileSize.endsWith("b")) {
                fileSize = fileSize + "b";
            }
            link.setDownloadSize(SizeFormatter.getSize(fileSize.trim().replace(",", ".").replace(" ", "")));
        }
        return AvailableStatus.TRUE;
    }

    private AvailableStatus requestFileInformation_WebsiteV2(final DownloadLink link, final Account account) throws Exception {
        final String fid = this.getFUID(link);
        final Browser brc = this.prepBrowserWebsiteV2(br.cloneBrowser());
        brc.postPageRaw(this.getWebsiteV2Base() + "/api/download/info", "{\"fileId\":\"" + fid + "\",\"referrer\":null,\"site\":null,\"shortDomain\":\"\"}");
        final Map<String, Object> entries = this.checkErrorsWebsiteV2(brc, link, account);
        final Map<String, Object> file = (Map<String, Object>) entries.get("file");
        /* This is a good source for filename information -> Make use of it */
        link.setFinalFileName(file.get("name").toString());
        link.setVerifiedFileSize(((Number) file.get("size")).longValue());
        if (Boolean.TRUE.equals(entries.get("premiumOnlyDownload")) && (account == null || account.getType() != AccountType.PREMIUM) && this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD) {
            throw new AccountRequiredException();
        }
        return AvailableStatus.TRUE;
    }

    public static boolean isFileOfflineWebsite(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("(<div class=\"code-404\">404</div>|Файл не найден\\. Возможно он был удален\\.<br|(?:Document|File|Page)\\s*(was)?\\s*not found|It could possibly be deleted\\.)");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> userinfo = login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setUnlimitedTraffic();
        if (allowWebsiteV2Handling()) {
            final Map<String, Object> entries;
            if (userinfo != null && StringUtils.endsWithCaseInsensitive(br.getURL(), "/api/user/info")) {
                /* Use result that has already been parsed */
                entries = userinfo;
            } else {
                entries = getUserInformationWebsiteV2(br, account);
            }
            final Map<String, Object> entries_premium = (Map<String, Object>) entries.get("premium");
            final String status = entries_premium.get("status").toString();
            if (status.equalsIgnoreCase("banned")) {
                account.setType(AccountType.PREMIUM);
                // for example:Status: Banned. You have reached the limit of your daily traffic quota
                getAndSetPremiumInformationWebsiteV1_in_website_v2_handling(account, ai);
                throw new AccountUnavailableException("You have reached limit of premium downloads", 30 * 60 * 1000l);
            } else if (status.equalsIgnoreCase("active")) {
                account.setType(AccountType.PREMIUM);
                final String expiredateStr = entries_premium.get("expiredAt").toString();
                ai.setValidUntil(TimeFormatter.getMilliSeconds(expiredateStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), br);
                getAndSetPremiumInformationWebsiteV1_in_website_v2_handling(account, ai);
            } else {
                account.setType(AccountType.FREE);
            }
        } else {
            // >Turbo access till 27.09.2015</span>
            String expire = br.getRegex(">\\s*Turbo access till\\s*(.*?)\\s*</span>").getMatch(0);
            if (expire == null) {
                /* 2019-05-22: hitfile.net */
                expire = br.getRegex("'/premium(?:/info)?'\\s*>\\s*(\\d+\\.\\d+\\.\\d+)\\s*<").getMatch(0);
            }
            if (expire != null) {
                ai.setValidUntil(TimeFormatter.getMilliSeconds(expire.trim(), "dd.MM.yyyy", Locale.ENGLISH), br);
                account.setType(AccountType.PREMIUM);
                getAndSetPremiumInformationWebsiteV1(account, ai);
            } else {
                account.setType(AccountType.FREE);
            }
        }
        return ai;
    }

    private void getAndSetPremiumInformationWebsiteV1_in_website_v2_handling(final Account account, final AccountInfo ai) throws AccountUnavailableException {
        try {
            getAndSetPremiumInformationWebsiteV1(account, ai);
        } catch (final AccountUnavailableException e) {
            throw e;
        } catch (final Exception e) {
            logger.log(e);
            logger.warning("Exception happened during obtaining premium traffic");
        }
    }

    /**
     * Only call this for premium accounts!!
     *
     * @throws AccountUnavailableException
     */
    protected void getAndSetPremiumInformationWebsiteV1(final Account account, final AccountInfo ai) throws IOException, AccountUnavailableException {
        if (account.getType() != AccountType.PREMIUM) {
            logger.warning("DEVELOPER MISTAKE!! ONLY CALL THIS FUNCTION FOR PREMIUM ACCOUNTS!!");
        }
        logger.info("Obtaining premium traffic information");
        final Browser brc = br.cloneBrowser();
        brc.getPage("https://" + br.getHost(false) + "/premium/info?site_version=1&from_mirror=1");
        ai.setProperty("getAndSetPremiumInformationWebsiteV1", true);
        final Regex traffic_daily = brc.getRegex("The rest of the traffic until the end of the day:\\s*<b>(\\d+[^<]+)</b>\\s*\\(of (\\d+[^<]+)/day\\)");
        if (traffic_daily.patternFind()) {
            final long traffic_daily_left = SizeFormatter.getSize(traffic_daily.getMatch(0));
            final long traffic_daily_max = SizeFormatter.getSize(traffic_daily.getMatch(1));
            ai.setTrafficLeft(traffic_daily_left);
            ai.setTrafficMax(traffic_daily_max);
        } else {
            logger.warning("Failed to find daily traffic left information");
        }
        final Regex traffic_monthly = brc.getRegex("The rest of the monthly traffic:\\s*<b>(\\d+[^<]+)</b>\\s*\\(of (\\d+[^<]+)/month\\)");
        String monthlyTrafficLeftInfo = "N/A";
        if (traffic_monthly.patternFind()) {
            final String traffic_monthly_left = traffic_monthly.getMatch(0);
            final String traffic_monthly_max = traffic_monthly.getMatch(1);
            monthlyTrafficLeftInfo = traffic_monthly_left + "/" + traffic_monthly_max;
        } else {
            logger.warning("Failed to find monthly traffic left information");
        }
        ai.setStatus(account.getType().getLabel() + " | Monthly traffic left: " + monthlyTrafficLeftInfo);
        final long dateEndTime = getDateEndTimeWebsiteV1(brc, account);
        if (dateEndTime != -1) {
            ai.setValidUntil(dateEndTime);
        }
        Long endBlockingTime = null;
        if (brc.containsHTML("<span class='glyphicon glyphicon-ok banturbo'>") || (endBlockingTime = getBlockingEndTime(brc, account)) > 0) {
            if (endBlockingTime == null) {
                endBlockingTime = getBlockingEndTime(brc, account);
            }
            if (endBlockingTime > 0) {
                final String readableTime = new SimpleDateFormat("yyyy-MM-dd' 'HH':'mm':'ss", Locale.ENGLISH).format(new Date(endBlockingTime));
                final long wait = Math.max(5 * 60 * 1000l, Math.min(endBlockingTime - System.currentTimeMillis(), 30 * 60 * 1000l));
                throw new AccountUnavailableException("You have reached limit of premium downloads:" + readableTime, wait);
            } else {
                throw new AccountUnavailableException("You have reached limit of premium downloads", 30 * 60 * 1000l);
            }
        }
    }

    /**
     * Alternative way to check if we're logged in: https://app.turbobit.net/api/site-data <br>
     * {"country":{"id":12345,"code":"de"},"defaultLanguage":"en","userFingerprint":"REDACTED","user":{"login":"xxx@xxy.org","premium":{
     * "status":"active","expiredAt":"2025-01-01
     * 01:01:01"}},"contacts":{"supportEmail":"REDACTED","abuseEmail":"REDACTED","helpUrl":"https:\
     * /\/trbt.cc\/zendesk\/redirect?path=https:\
     * /\/turbobit-net.zendesk.com","dmcaUrl":"\/\/www.dmca.com\/Protection\/Status.aspx?ID=REDACTED
     * &refurl=https:\/\/app.turbobit.net\/rules
     * \/abuse","help2Url":"https:\/\/help2.turbobit.net\/hc\/en-us"},"correctUrl":null,"siteV2Available":true,"shortDomain":"trbt.cc"}
     */
    private Map<String, Object> getUserInformationWebsiteV2(final Browser br, final Account account) throws IOException, PluginException {
        br.getPage(getWebsiteV2Base() + "/api/user/info");
        /* 2025-07-07: Redirects to this page when we are not logged in: https://app.turbobit.net?login=true */
        final Map<String, Object> entries = this.checkErrorsWebsiteV2(br, null, account);
        return entries;
    }

    protected static long getBlockingEndTime(final Browser br, final Account account) {
        final String[] endTimes = br.getRegex("(?:Ban|Blocking) end (?:date|time)\\s*:\\s*(?:</span>)?\\s*(?:<b>)?\\s*(\\d{4}-\\d{2}-\\d{2}\\s*\\d{2}:\\d{2}:\\d{2})\\s*<").getColumn(0);
        if (endTimes == null || endTimes.length == 0) {
            return -1;
        }
        final long now = System.currentTimeMillis();
        for (final String endTime : endTimes) {
            final long timeStamp = TimeFormatter.getMilliSeconds(endTime, "yyyy-MM-dd' 'HH':'mm':'ss", Locale.ENGLISH);
            if (timeStamp > 0 && timeStamp > now) {
                return timeStamp;
            }
        }
        return -1;
    }

    protected static long getDateEndTimeWebsiteV1(final Browser br, final Account account) {
        final String[] endTimes = br.getRegex("Date end\\s*:\\s*(?:<b>)?\\s*(\\d{2}\\.\\d{2}\\.\\d{4}\\s*\\d{2}:\\d{2})\\s*<").getColumn(0);
        if (endTimes == null || endTimes.length == 0) {
            return -1;
        }
        final long now = System.currentTimeMillis();
        for (final String endTime : endTimes) {
            final long timeStamp = TimeFormatter.getMilliSeconds(endTime, "dd'.'MM'.'yyyy' 'HH':'mm", Locale.ENGLISH);
            if (timeStamp > 0 && timeStamp > now) {
                return timeStamp;
            }
        }
        return -1;
    }

    @Override
    public String getAGBLink() {
        return getMainpage() + "/rules";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        if (this.getPluginConfig().getBooleanProperty(SETTING_FREE_PARALLEL_DOWNLOADSTARTS, false)) {
            return FREE_MAXDOWNLOADS_PLUGINSETTING;
        } else {
            return 1;
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    protected String getAPIKey() {
        final String userDefinedAPIKey = this.getPluginConfig().getStringProperty("APIKEY");
        if (StringUtils.isEmpty(userDefinedAPIKey) || StringUtils.equalsIgnoreCase(userDefinedAPIKey, "DEFAULT")) {
            /* No APIKey set or default? Return default key. */
            return getAPIKeyDefault();
        } else {
            return userDefinedAPIKey;
        }
    }

    public String getAPIKeyDefault() {
        return null;
    }

    /**
     * @return true: Website supports https and plugin will prefer https. <br />
     *         false: Website does not support https - plugin will avoid https. <br />
     *         default: true
     */
    public boolean supports_https() {
        return true;
    }

    public boolean supports_mass_linkcheck() {
        return true;
    }

    /** E.g. '.html' needed at the end of downloadurls: turbobit.net - e.g. NOT needed: hitfile.net */
    public boolean downloadurls_need_html_ending() {
        return true;
    }

    /** If no waittime is found or it is less than this, a fallback waittime will get used. */
    public int minimum_pre_download_waittime_seconds() {
        return 60;
    }

    /** Waittime which is used if no waittime was found or the found waittime is less than minimum_pre_download_waittime_seconds */
    protected int get_fallback_waittime() {
        return 600;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleFree(link, null);
    }

    protected void accessContentURLWebsiteV1(final Browser br, final DownloadLink link) throws Exception {
        br.getPage(getContentURL(link));
    }

    protected String getContentURL(final DownloadLink link) throws PluginException {
        return getContentURL(getConfiguredDomain(), this.getFUID(link));
    }

    /** Returns base content URL for given DownloadLink. Most times this will be: https://<domain>/<fuid>.html */
    protected String getContentURL(final String domain, final String fuid) throws PluginException {
        final String domainToUse;
        final List<String> deadDomains = this.getDeadDomains();
        if (deadDomains != null && deadDomains.contains(domain)) {
            /* Domain which we want to use is dead -> Use main domain */
            domainToUse = this.getHost();
        } else {
            domainToUse = domain;
        }
        String contentURL = "https://" + domainToUse + "/" + fuid;
        if (downloadurls_need_html_ending()) {
            contentURL += ".html";
        }
        return contentURL;
    }

    /** Handles free- and free account downloads */
    protected void handleFree(final DownloadLink link, Account account) throws Exception {
        handleDownload(link, account);
    }

    protected void checkPremiumOnlyWebsiteV1(final DownloadLink link, final Account account, final Browser br) throws PluginException {
        String msg = br.getRegex("<div class=\"free-limit-note\"[^>]*>\\s*(Limit reached for free download of this file\\.)").getMatch(0);
        if (msg != null) {
            msg = Encoding.htmlDecode(msg).trim();
            throw new AccountRequiredException(msg);
        } else if (br.containsHTML("<a\\s*data-premium-only-download") || br.containsHTML("href\\s*=\\s*\"/download/free/[^>]*?data-premium-only-download")) {
            throw new AccountRequiredException();
        }
    }

    /**
     * Fills in captchaForm. </br> DOES NOT SEND CAPTCHA-FORM!!
     */
    protected boolean processCaptchaFormWebsiteV1(final DownloadLink link, final Account account, final Form captchaform, final Browser br, final boolean optionalCaptcha) throws PluginException, InterruptedException {
        if (AbstractHCaptcha.containsHCaptcha(br)) {
            final String response = new CaptchaHelperHostPluginHCaptcha(this, br).getToken();
            captchaform.put("g-recaptcha-response", Encoding.urlEncode(response));
            captchaform.put("h-captcha-response", Encoding.urlEncode(response));
            return true;
        } else if (AbstractRecaptchaV2.containsRecaptchaV2Class(br)) {
            /* ReCaptchaV2 */
            final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
            captchaform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            return true;
        } else if (!optionalCaptcha) {
            /* This should not happen - see old captcha handling in TurboBitNet class revision 40594 */
            logger.warning("Captcha-handling failed: Captcha handling was executed and captcha is expected to be there but is not there");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            return false;
        }
    }

    protected void checkErrorsLastResort(final Browser br, final DownloadLink downloadLink, final Account account) throws Exception {
        final String waittime = br.getRegex("limit\\s?:\\s?(\\d+)\\s*,").getMatch(0);
        if (waittime != null) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Integer.parseInt(waittime) * 1001l);
        } else if (br.containsHTML("Timeout\\.limit\\s*>\\s*0")) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, 10 * 60 * 1001l);
        }
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private final void handleDownload(final DownloadLink link, final Account account) throws Exception {
        /* support for public premium links */
        if (link.getPluginPatternMatcher().matches(TYPE_premiumRedirectLinks)) {
            if (handlePremiumLink(link, account)) {
                return;
            } else {
                logger.info("Download of pre given directurl failed --> Attempting normal free download");
            }
        }
        if (account != null) {
            this.login(account, false);
        } else {
            if (checkShowFreeDialog(getHost())) {
                super.showFreeDialog(getHost());
            }
        }
        final DownloadType dltype;
        if (account == null) {
            dltype = DownloadType.GUEST_FREE;
        } else if (account.getType() == AccountType.PREMIUM) {
            dltype = DownloadType.ACCOUNT_PREMIUM;
        } else {
            dltype = DownloadType.ACCOUNT_FREE;
        }
        String directlink = null;
        final String fid = this.getFUID(link);
        if (allowWebsiteV2Handling()) {
            /* WebsiteV2 */
            final Browser brc = this.prepBrowserWebsiteV2(br.cloneBrowser());
            if (account != null && account.getType() == AccountType.PREMIUM) {
                /* Premium download */
                /* Linkcheck can be skipped here as the following request will return an error if the file_id is invalid/offline. */
                // requestFileInformation_WebsiteV2(link, account);
                brc.postPageRaw(this.getWebsiteV2Base() + "/api/download/info", "{\"fileId\":\"" + fid + "\",\"referrer\":null,\"site\":null,\"shortDomain\":\"\"}");
                final Map<String, Object> downloadmap = this.checkErrorsWebsiteV2(brc, link, account);
                final List<String> mirrors = (List<String>) downloadmap.get("downloadUrls");
                final int chosenMirrorIndex = new Random().nextInt(mirrors.size());
                directlink = mirrors.get(new Random().nextInt(mirrors.size()));
                logger.info("Available premium mirrors: " + mirrors.size() + " | Chosen mirror[" + chosenMirrorIndex + "] --> " + directlink);
            } else {
                requestFileInformation_WebsiteV2(link, account);
                brc.postPageRaw(this.getWebsiteV2Base() + "/api/download/free/init", "{\"fileId\":\"" + fid + "\"}");
                final Map<String, Object> freedl = this.checkErrorsWebsiteV2(brc, link, account);
                final Map<String, Object> freedl_ipBan = (Map<String, Object>) freedl.get("ipBan");
                if (freedl_ipBan != null) {
                    final int seconds = ((Number) freedl_ipBan.get("delay")).intValue();
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, seconds * 1000l);
                }
                brc.getPage("/api/captcha");
                final Map<String, Object> captchainfo = this.checkErrorsWebsiteV2(brc, link, account);
                final String reCaptchaIndex = captchainfo.get("index").toString();
                final String reCaptchaKey = captchainfo.get("publicKey").toString();
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, brc, reCaptchaKey).getToken();
                brc.postPageRaw("/api/download/free/captcha", "{\"fileId\":\"" + fid + "\",\"g-recaptcha-response\":\"" + recaptchaV2Response + "\",\"g-captcha-index\":" + reCaptchaIndex + "}");
                final Map<String, Object> delaymap = this.checkErrorsWebsiteV2(brc, link, account);
                final int waitSeconds = ((Number) delaymap.get("delay")).intValue();
                this.sleep(waitSeconds * 1000, link);
                brc.postPageRaw("/api/download/free/prepare", "{\"fileId\":\"" + fid + "\"}");
                /* Expected answer: {"success":true} */
                this.checkErrorsWebsiteV2(brc, link, account);
                brc.getHeaders().put("Referer", "https://new.turbobit.net/download/started/" + fid);
                brc.postPageRaw("/api/download/free/start", "{\"fileId\":\"" + fid + "\"}");
                /*
                 * Expected answer:
                 * {"downloadUrl":"https...","file":{"id":"REDACTED","name":"REDACTED.rar","size":1234567890},"expectedDownloadTime":{"free"
                 * :1234,"premium":1234}}
                 */
                final Map<String, Object> downloadmap = this.checkErrorsWebsiteV2(brc, link, account);
                directlink = downloadmap.get("downloadUrl").toString();
                if (!directlink.contains("?")) {
                    /* Small workaround. TODO: Check if this is really needed */
                    directlink += "?site_version=1&from_mirror=1";
                }
            }
        } else {
            /* WebsiteV1 */
            requestFileInformation_WebsiteV1(link, account);
            if (account != null && account.getType() == AccountType.PREMIUM) {
                /* Premium download */
                handlePremiumWebsiteV1(link, account);
                return;
            } else {
                /* Free + Free Account download */
                br.getPage("/download/free/" + this.getFUID(link));
                if (isFileOfflineWebsite(this.br)) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                checkPremiumOnlyWebsiteV1(link, account, br);
                boolean hasRetried = false;
                Form captchaform = null;
                while (true) {
                    final Form[] allForms = br.getForms();
                    if (allForms != null && allForms.length != 0) {
                        for (final Form aForm : allForms) {
                            if (aForm.containsHTML("captcha")) {
                                captchaform = aForm;
                                break;
                            }
                        }
                    }
                    if (captchaform != null) {
                        break;
                    }
                    handleGeneralErrors(br, account);
                    if (StringUtils.containsIgnoreCase(br.getURL(), "/download/free/")) {
                        if (!hasRetried && br.containsHTML("/download/free/" + Pattern.quote(fid))) {
                            // from a log where the first call to this, just redirected to main page and set some cookies
                            br.getPage("/download/free/" + fid);
                            hasRetried = true;
                            continue; // Retry the loop instead of recursive call
                        }
                        /* 2019-04-24: This should not happen anymore but still we should retry if it happens. */
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Captcha form fail", 1 * 60 * 1000l);
                    }
                    checkErrorsLastResort(br, link, account);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* Fix Form */
                if (StringUtils.equalsIgnoreCase(captchaform.getAction(), "#")) {
                    captchaform.setAction(br.getURL());
                }
                if (!captchaform.hasInputFieldByName("captcha_type") && captchaform.containsHTML("recaptcha2")) {
                    /* E.g. hitfile.net */
                    captchaform.put("captcha_type", "recaptcha2");
                }
                if (!captchaform.hasInputFieldByName("captcha_subtype") && captchaform.containsHTML("captcha_subtype")) {
                    /* E.g. hitfile.net */
                    captchaform.put("captcha_subtype", "");
                }
                processCaptchaFormWebsiteV1(link, account, captchaform, br, false);
                br.submitForm(captchaform);
                if (br.getHttpConnection().getResponseCode() == 302 || br.containsHTML("<div\\s*class\\s*=\\s*\"captcha-error\"\\s*>\\s*Incorrect")) {
                    /* This should never happen - solving took too long? */
                    invalidateLastChallengeResponse();
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                final String continueLink = br.getRegex("\\$\\('#timeoutBox'\\)\\.load\\(\"(/[^\"]+)\"\\);").getMatch(0);
                if (continueLink == null) {
                    checkErrorsLastResort(br, link, account);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* Pre download wait */
                int wait = 0;
                final String wait_str = br.getRegex("minLimit\\s*:\\s*(\\d+)").getMatch(0);
                if (wait_str == null) {
                    logger.warning("Using fallback pre-download-wait");
                    wait = get_fallback_waittime();
                } else {
                    wait = Integer.parseInt(wait_str);
                    /* Check for too short/too long waittime. */
                    if (wait > 800 || wait < minimum_pre_download_waittime_seconds()) {
                        /* We do not want to wait too long! */
                        wait = get_fallback_waittime();
                    }
                }
                if (wait > 250) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Limit reached or IP already loading", wait * 1001l);
                }
                this.sleep(wait * 1001l, link);
                final Browser br2 = br.cloneBrowser();
                br2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                br2.getPage(continueLink);
                /* 2019-07-11: New for turbobit.net */
                final String continueLink2 = br2.getRegex("(\"|')(/?/download/started/[^\"\\']+)\\1").getMatch(1);
                if (!StringUtils.isEmpty(continueLink2)) {
                    br2.getPage(continueLink2);
                }
                directlink = br2.getRegex("(\"|')(/?/download/redirect/[^\"\\']+)\\1").getMatch(1);
                handleDownloadRedirectErrorsWebsiteV1(directlink, link);
            }
        }
        initDownload(dltype, link, account, directlink);
        handleErrorsPreDownloadstart(dl.getConnection());
        dl.startDownload();
    }

    /** Handles errors */
    private void handleDownloadRedirectErrorsWebsiteV1(final String redirect, final DownloadLink link) throws PluginException {
        if (StringUtils.isEmpty(redirect)) {
            logger.info("'redirect' downloadurl is null");
            if (br.toString().matches("Error: \\d+")) {
                // unknown error...
                throw new PluginException(LinkStatus.ERROR_RETRY);
            } else if (br.toString().matches("^The file is not avaliable now because of technical problems\\. <br> Try to download it once again after 10-15 minutes\\..*?")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File not avaiable due to technical problems.", 15 * 60 * 1001l);
            } else if (br.containsHTML("<a href=\\'/" + this.getLinkID(link) + "(\\.html)?\\'>new</a>")) {
                /* Expired downloadlink - rare issue. If user has added such a direct-URL, we're not able to retry. */
                /**
                 * 2019-05-14: TODO: Even premium-directurls should contain the linkid so we should be able to use that to 'convert' such
                 * problematic URLs to 'normal' URLs. Keep in mind that this is a VERY VERY rare case!
                 */
                /*
                 * <div class="action-block"><p>Der Link ist abgelaufen. Fordern Sie bitte <a href='/FUID.html'>new</a> download
                 * link.</p></div></div> </div> Example: http://turbobit.net/download/redirect/TEST/TEST
                 */
                if (link.getPluginPatternMatcher().matches(TYPE_premiumRedirectLinks)) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Generated Premium link has expired");
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to generate final downloadlink");
                }
            }
            final String linkerror = br.getRegex("<div\\s*id\\s*=\\s*\"brin-link-error\"\\s*>\\s*([^>]+)\\s*</div>").getMatch(0);
            if (linkerror != null) {
                /* 2019-07-10: E.g. <div id="brin-link-error">Failed to generate link. Internal server error. Please try again.</div> */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, linkerror);
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown Error - failed to find redirect-url to final downloadurl");
        } else if (redirect.matches("^https?://[^/]+/?$")) {
            /* Redirect to mainpage --> expired/invalid? */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Premium link no longer valid");
        }
    }

    /**
     * fuid = case sensitive.
     *
     * @param link
     * @return
     * @throws PluginException
     */
    private String getFUID(final DownloadLink link) throws PluginException {
        /* standard links turbobit.net/uid.html && turbobit.net/uid/filename.html */
        if (link == null || link.getPluginPatternMatcher() == null) {
            return null;
        }
        if (link.getPluginPatternMatcher().matches(TYPE_premiumRedirectLinks)) {
            final String fuid = new Regex(link.getPluginPatternMatcher(), TYPE_premiumRedirectLinks).getMatch(0);
            if (fuid == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                return fuid;
            }
        } else {
            String fuid = new Regex(link.getPluginPatternMatcher(), "(?i)https?://[^/]+/([A-Za-z0-9]+)(?:/[^/]+)?(?:\\.html)?$").getMatch(0);
            if (fuid == null) {
                /* download/free/ */
                fuid = new Regex(link.getPluginPatternMatcher(), "(?i)download/free/([A-Za-z0-9]+)").getMatch(0);
            }
            if (fuid == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                return fuid;
            }
        }
    }

    protected AtomicLong getLastPremiumCaptchaProcessedTimestamp() {
        synchronized (hostLastPremiumCaptchaProcessedTimestampMap) {
            AtomicLong ret = hostLastPremiumCaptchaProcessedTimestampMap.get(this.getHost());
            if (ret == null) {
                ret = new AtomicLong(-1);
                hostLastPremiumCaptchaProcessedTimestampMap.put(getHost(), ret);
            }
            return ret;
        }
    }

    @Deprecated
    protected void handlePremiumCaptchaWebsiteV1(final Browser br, final DownloadLink link, final Account account) throws Exception {
        Form premiumCaptchaForm = null;
        for (final Form form : br.getForms()) {
            if ((form.containsHTML(">\\s*Please enter captcha to continue\\s*<") || form.hasInputFieldByName("captcha_type") || form.hasInputFieldByName("g-captcha-index")) && form.hasInputFieldByName("check")) {
                premiumCaptchaForm = form;
                break;
            }
        }
        if (premiumCaptchaForm == null) {
            return;
        }
        /* 2021-03-30: Captchas can sometimes happen in premium mode (wtf but confirmed!) */
        final AtomicLong lastPremiumCaptchaProcessedTimestamp = getLastPremiumCaptchaProcessedTimestamp();
        final long lastPremiumCaptchaRequestedTimestamp = lastPremiumCaptchaProcessedTimestamp.get();
        synchronized (lastPremiumCaptchaProcessedTimestamp) {
            logger.info("Detected premium download-captcha");
            if (!lastPremiumCaptchaProcessedTimestamp.compareAndSet(lastPremiumCaptchaRequestedTimestamp, System.currentTimeMillis())) {
                // TODO: Check if a retry makes sense here when one captcha was solved by the user
                logger.info("Captcha has just been solved -> We might be able to skip this and all other subsequent premium captchas by just retrying");
            }
            processCaptchaFormWebsiteV1(link, account, premiumCaptchaForm, br, false);
            br.submitForm(premiumCaptchaForm);
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    @Deprecated
    private void handlePremiumWebsiteV1(final DownloadLink link, final Account account) throws Exception {
        if (link.getPluginPatternMatcher().matches(TYPE_premiumRedirectLinks)) {
            /* Direct-downloadable public premium link. */
            if (handlePremiumLink(link, account)) {
                return;
            } else {
                logger.info("Download of pre given directurl failed --> Attempting normal premium download");
            }
        }
        requestFileInformation_WebsiteV1(link, account);
        login(account, false);
        sleep(2000, link);
        accessContentURLWebsiteV1(br, link);
        handlePremiumCaptchaWebsiteV1(br, link, account);
        String dllink = null;
        final String[] mirrors = br.getRegex("('|\")(https?://([a-z0-9\\.]+)?[^/\\'\"]+//?download/redirect/.*?)\\1").getColumn(1);
        if (mirrors == null || mirrors.length == 0) {
            if (br.containsHTML("You have reached the.*? limit of premium downloads")) {
                throw new AccountUnavailableException("Downloadlimit reached", 30 * 60 * 1000l);
            } else if (br.containsHTML("'>\\s*Premium access is blocked\\s*<")) {
                logger.info("Premium access is blocked --> No traffic available?");
                throw new AccountUnavailableException("Error 'Premium access is blocked' --> No traffic available?", 30 * 60 * 1000l);
            }
            this.handleGeneralErrors(br, account);
            logger.warning("dllink equals null, plugin seems to be broken!");
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadlink");
        }
        br.setFollowRedirects(false);
        for (int i = 0; i < mirrors.length; i++) {
            final String currentlink = mirrors[i];
            logger.info("Checking mirror: " + i + "/" + mirrors.length + ": " + currentlink);
            br.getPage(currentlink);
            if (br.getHttpConnection().getResponseCode() == 503) {
                logger.info("Too many connections on current account via current IP");
                throw new AccountUnavailableException("Too many connections on current account via current IP", 30 * 1000l);
            }
            if (br.getRedirectLocation() == null) {
                logger.info("Skipping broken mirror reason#1: " + currentlink);
                continue;
            }
            dllink = br.getRedirectLocation();
            try {
                if (initDownload(DownloadType.ACCOUNT_PREMIUM, link, account, dllink)) {
                    break;
                }
            } catch (final PluginException e) {
                final boolean isLastMirror = mirrors.length - 1 == i;
                if (isLastMirror) {
                    throw e;
                } else {
                    logger.log(e);
                    logger.info("Skipping broken mirror reason#2: " + dllink);
                    continue;
                }
            }
            /* Ugly workaround */
            logger.info("Skipping non working mirror: " + dllink);
        }
        if (dl == null) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadlink");
        }
        dl.startDownload();
    }

    /**
     * 2019-05-11: Their final-downloadlinks usually contain the md5 checksum of the file and this is the only place we can get it from.
     * This function tries to find this md5 value and sets it if possible.
     */
    protected boolean getAndSetMd5Hash(final DownloadLink link, final String dllink) {
        final String md5Value = new Regex(dllink, "(?i)md5=([a-f0-9]{32})").getMatch(0);
        final HashInfo md5Hash = HashInfo.parse(md5Value, true, false);
        if (md5Hash != null) {
            final HashInfo existingHash = link.getHashInfo();
            if (existingHash == null || md5Hash.isStrongerThan(existingHash) || (existingHash.getType() == md5Hash.getType() && !existingHash.equals(md5Hash))) {
                logger.info("Found hash on downloadstart:" + md5Hash + "|Existing:" + existingHash);
                link.setHashInfo(md5Hash);
            }
            return true;
        }
        return false;
    }

    static enum DownloadType {
        ACCOUNT_PREMIUM,
        ACCOUNT_FREE,
        GUEST_FREE,
        GUEST_PREMIUMLINK;
    }

    /** 2019-05-11: Limits seem to be the same for all of their services. */
    private boolean initDownload(final DownloadType dltype, final DownloadLink link, final Account account, final String directlink) throws Exception {
        if (directlink == null) {
            /* Developer mistake */
            logger.warning("dllink is null");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        boolean success = false;
        final boolean previousFollowRedirectState = br.isFollowingRedirects();
        try {
            switch (dltype) {
            case ACCOUNT_PREMIUM:
            case GUEST_PREMIUMLINK:
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, directlink, true, 0);
                break;
            default:
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, directlink, true, 1);
                break;
            }
            if (dl.getConnection().getURL().getPath().startsWith("/error/download/ip")) {
                br.followConnection(true);
                // 403 by itself
                // response code 403 && <p>You have reached the limit of downloads from this IP address, please contact our
                if (dltype == DownloadType.ACCOUNT_PREMIUM) {
                    throw new AccountUnavailableException("403: You have reached the limit of downloads from this IP address", 30 * 60 * 1000l);
                } else {
                    // some reason we have different error handling for free.
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "You cannot download this file with your current IP", 60 * 60 * 1000l);
                }
            } else if (dl.getConnection().getResponseCode() == 403) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403");
            } else if (dl.getConnection().getResponseCode() == 404) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404");
            } else if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                handleGeneralErrors(br, account);
                return false;
            } else {
                getAndSetMd5Hash(link, dl.getConnection().getURL().toExternalForm());
                success = true;
                return true;
            }
        } finally {
            br.setFollowRedirects(previousFollowRedirectState);
            try {
                if (!success) {
                    dl.getConnection().disconnect();
                    dl = null;
                }
            } catch (final Throwable t) {
            }
            logger.info("Mirror is " + (success ? "okay: " : "down: ") + directlink);
        }
    }

    /** Attempts to download pre-given premium direct-URLs. */
    protected boolean handlePremiumLink(final DownloadLink link, final Account account) throws Exception {
        if (!link.getPluginPatternMatcher().matches(TYPE_premiumRedirectLinks)) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (initDownload(DownloadType.GUEST_PREMIUMLINK, link, account, link.getPluginPatternMatcher())) {
            handleErrorsPreDownloadstart(dl.getConnection());
            dl.startDownload();
            return true;
        } else {
            logger.info("Download of supposedly direct-downloadable premium link failed");
            this.dl = null;
            return false;
        }
    }

    private void handleGeneralErrors(final Browser br, final Account account) throws PluginException {
        if (br.containsHTML("Try to download it once again after")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'Try again later'", 20 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*Ссылка просрочена\\. Пожалуйста получите")) {
            /* Either user waited too long for the captcha or maybe slow servers */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'link expired'", 5 * 60 * 1000l);
        } else if (br.containsHTML("Our service is currently unavailable in your country\\.")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, this.getHost() + " is currently unavailable in your country!");
        } else if (br.containsHTML(">\\s*Your IP exceeded the max\\.? number of files that can be downloaded")) {
            final Regex durationRegex = br.getRegex("You will be able to download at high speed again in (\\d+) hour\\(s\\) (\\d+) minute");
            final String hoursStr = durationRegex.getMatch(0);
            final String minutesStr = durationRegex.getMatch(1);
            long wait = 30 * 60 * 1000l;
            if (hoursStr != null && minutesStr != null) {
                wait = (Long.parseLong(hoursStr) * 60 * 60 + Long.parseLong(minutesStr) * 60) * 1000l;
            }
            if (account != null) {
                throw new AccountUnavailableException("Your IP exceeded the max number of files that can be downloaded", wait);
            } else {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Your IP exceeded the max number of files that can be downloaded", 5 * 60 * 1000l);
            }
        }
    }

    private Map<String, Object> checkErrorsWebsiteV2(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        /* Wait milliseconds for unknown/generic errors */
        final long waitmillis = 60 * 1000;
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String errortext = "Invalid API response";
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errortext, waitmillis);
            } else {
                throw new AccountUnavailableException(errortext, waitmillis);
            }
        }
        final String error_name = (String) entries.get("error_name");
        if (error_name == null) {
            /* No error */
            return entries;
        }
        if (error_name.equalsIgnoreCase("file_is_not_available_for_download")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        // TODO: Add translation and support for more errors
        if (link == null) {
            /* Account related error e.g. password_incorrect, invalid_captcha */
            throw new AccountInvalidException(error_name);
        } else {
            throw new PluginException(LinkStatus.ERROR_FATAL, error_name);
        }
    }

    private void handleErrorsPreDownloadstart(final URLConnectionAdapter con) throws PluginException {
        if (con.getLongContentLength() == 0) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error, server sends empty file", 5 * 60 * 1000l);
        }
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    private Map<String, Object> login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            setBrowserExclusive();
            /*
             * We have to reuse old User-Agent, else the cookie will become invalid
             */
            final Cookies cookies = account.loadCookies("");
            prepBrowserWebsiteV1(br);
            br.getPage(this.getMainpage());
            final String curr_domain = br.getHost();
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(curr_domain, cookies);
                if (allowWebsiteV2Handling()) {
                    br.clearCookies(null);
                    try {
                        final Map<String, Object> entries = getUserInformationWebsiteV2(br, account);
                        logger.info("Cookie login successful");
                        /* Set new cookie timestamp */
                        br.setCookies(curr_domain, cookies);
                        return entries;
                    } catch (final PluginException ignore) {
                        logger.log(ignore);
                        logger.info("Cookie login failed");
                    }
                } else {
                    /* Request same URL again, this time with cookies set */
                    br.getPage(br.getURL());
                    if (isLoggedIN(br)) {
                        logger.info("Cookie login successful");
                        /* Set new cookie timestamp */
                        br.setCookies(curr_domain, cookies);
                        return null;
                    }
                    logger.info("Cookie login failed");
                    if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        logger.warning("Cookie login failed MAKE SURE THAT YOU RE-USED THE SAME USER-AGENT AS USED FOR THE FIRST LOGIN ELSE COOKIE LOGIN WILL NOT WORK!!!");
                    }
                }
            }
            /* lets set a new User-Agent */
            logger.info("Performing full login");
            prepBrowserWebsiteV1(br);
            br.getPage("https://" + curr_domain + "/login");
            boolean requiredLoginCaptcha = false;
            Form loginform = findAndPrepareLoginForm(br, account);
            if (loginform != null) {
                br.submitForm(loginform);
                loginform = findAndPrepareLoginForm(br, account);
                if (!isLoggedIN(br) && loginform != null) {
                    logger.info("Loginform is present again after login attempt");
                    /* Check for stupid login captcha */
                    DownloadLink link = getDownloadLink();
                    if (link == null) {
                        link = new DownloadLink(this, "Account", account.getHoster(), getMainpage(), true);
                        this.setDownloadLink(link);
                    }
                    processCaptchaFormWebsiteV1(link, account, loginform, br, true);
                    if (loginform.containsHTML("class=\"reloadCaptcha\"")) {
                        /* Old captcha - e.g. wayupload.com */
                        requiredLoginCaptcha = true;
                        final String captchaurl = br.getRegex("(https?://[^/]+/captcha/securimg[^\"<>]+)").getMatch(0);
                        if (captchaurl == null) {
                            logger.warning("Failed to find captchaURL");
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                        final String code = this.getCaptchaCode(captchaurl, link);
                        loginform.put("user%5Bcaptcha_response%5D", Encoding.urlEncode(code));
                        loginform.put("user%5Bcaptcha_type%5D", "securimg");
                        loginform.put("user%5Bcaptcha_subtype%5D", "9");
                    }
                    br.submitForm(loginform);
                }
                universalLoginErrorhandling(br);
                if (!isLoggedIN(br) && br.containsHTML("<div[^>]*id\\s*=\\s*\"activation-form\"")) {
                    // <h1>Premium activation</h1>
                    // <div id="activation-form">
                    // <input-premium-block
                    // predefined-premium-key="XXXX"
                    // predefined-email="YYYYYYY"
                    // :logged-in="false"
                    // :auto-submit="true"
                    // custom-handler=""
                    // >
                    // </input-premium-block>
                    // <div class="block-title"> Please, enter the premium code below, if you already have it. </div>
                    String predefinedpremiumkey = br.getRegex("predefined-premium-key\\s*=\\s*\"(.*?)\"").getMatch(0);
                    String predefinedemail = br.getRegex("predefined-email\\s*=\\s*\"(.*?)\"").getMatch(0);
                    if (StringUtils.isEmpty(predefinedemail)) {
                        predefinedemail = account.getUser();
                    }
                    if (StringUtils.isEmpty(predefinedpremiumkey)) {
                        // same as account password?
                        predefinedpremiumkey = account.getPass();
                    }
                    final UrlQuery loginquery = new UrlQuery();
                    loginquery.appendEncoded("premium", predefinedpremiumkey);
                    loginquery.appendEncoded("email", predefinedemail);
                    br.postPage("/payments/premium/process", loginquery);
                    final Map<String, Object> response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    if (Boolean.TRUE.equals(response.get("success"))) {
                        final String redirect = (String) response.get("redirect");
                        if (redirect != null) {
                            br.getPage(redirect);
                        }
                        if (isLoggedIN(br)) {
                            return null;
                        } else {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                    }
                    final String message = (String) response.get("message");
                    throw new AccountInvalidException(message);
                }
                if (!isLoggedIN(br)) {
                    throw new AccountInvalidException();
                }
                if (requiredLoginCaptcha) {
                    /* Display hint to user on how to disable login captchas. */
                    showLoginCaptchaInformation(account);
                }
            } else if (allowWebsiteV2Handling()) {
                /* Assume that we are on website version 2.0 (aka new.turbobit.net/login) */
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("email", account.getUser());
                postdata.put("password", account.getPass());
                postdata.put("captcha", true);
                postdata.put("g-recaptcha-response", "");
                postdata.put("g-captcha-index", 4);
                br.postPageRaw(getWebsiteV2Base() + "/api/auth/login", JSonStorage.serializeToJson(postdata));
                final Map<String, Object> entries = this.checkErrorsWebsiteV2(br, null, account);
                return entries;
            }
            account.saveCookies(br.getCookies(curr_domain), "");
            return null;
        }
    }

    private Thread showLoginCaptchaInformation(final Account account) {
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = account.getHoster() + " - Login Captcha";
                        message += "Hallo liebe(r) " + account.getHoster() + " NutzerIn\r\n";
                        message += "Um den Account dieses Anbieters in JDownloader verwenden zu können, musst du derzeit ein Login-Captcha eingeben.\r\n";
                        message += "Falls dich das stört, kannst du folgendes tun:\r\n";
                        message += "1. Logge dich im Browser ein.\r\n";
                        message += "2. Navigiere zu: " + account.getHoster() + "/user/settings\r\n";
                        message += "3. Entferne das Häckchen bei 'Captcha einsetzen, um meinen Account zu schützen' und klicke unten auf speichern.\r\n";
                        message += "Dein Account sollte sich ab sofort ohne Login-Captchas in JD hinzufügen/prüfen lassen.\r\n";
                    } else {
                        title = account.getHoster() + " - Login-Captcha";
                        message += "Hello dear " + account.getHoster() + " user\r\n";
                        message += "In order to add/check your account of this service, you have to solve login-captchas at this moment.\r\n";
                        message += "If that is annoying to you, you can deactivate them as follows:\r\n";
                        message += "1. Login via browser.\r\n";
                        message += "2. Open this page: " + account.getHoster() + "/user/settings\r\n";
                        message += "3. Uncheck the checkbox 'Use a captcha to protect my account'.\r\n";
                        message += "From now on, you should be able to add/check your account in JD without the need of a login-captcha.\r\n";
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(2 * 60 * 1000);
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

    protected boolean isLoggedIN(final Browser br) {
        return br.containsHTML("/user/logout");
    }

    private Form findAndPrepareLoginForm(Browser br, final Account account) throws PluginException {
        if (account == null) {
            return null;
        }
        final Form loginForm = br.getFormbyAction("/user/login");
        if (loginForm == null) {
            return null;
        }
        loginForm.put("user%5Blogin%5D", Encoding.urlEncode(account.getUser()));
        loginForm.put("user%5Bpass%5D", Encoding.urlEncode(account.getPass()));
        return loginForm;
    }

    public static void universalLoginErrorhandling(final Browser br) throws PluginException {
        if (br.containsHTML(">\\s*Limit of login attempts exceeded for your account")) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nMaximale Anzahl von Loginversuchen überschritten - dein Account wurde temporär gesperrt!\r\nBestätige deinen Account per E-Mail um ihn zu entsperren.\r\nFalls du keine E-Mail bekommen hast, gib deine E-Mail Adresse auf folgender Seite ein und lasse dir erneut eine zuschicken: " + br.getHost() + "/restoreaccess", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nLimit of login attempts exceeded for your account - your account is locked!\r\nConfirm your account via e-mail to unlock it.\r\nIf you haven't received an e-mail, enter your e-mail address on the following site so the service can send you a new confirmation mail: " + br.getHost() + "/restoreaccess", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
    }

    public String getMainpage() {
        if (supports_https()) {
            return "https://" + this.getConfiguredDomain() + "/";
        } else {
            return "http://" + this.getConfiguredDomain() + "/";
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }

    /** For some hosts, users can configure their preferred domain. */
    protected String getConfiguredDomain() {
        return this.getHost();
    }

    private String getWebsiteV2Base() {
        return "https://app." + getConfiguredDomain();
    }

    protected void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SETTING_FREE_PARALLEL_DOWNLOADSTARTS, "Activate parallel downloadstarts in free mode?\r\n<html><p style=\"color:#F62817\"><b>Warning: This setting can lead to a lot of non-accepted captcha popups!</b></p></html>").setDefaultValue(false));
        // getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), "APIKEY", "Define custom APIKey (can be
        // found on website in account settings ['/user/settings'])").setDefaultValue(""));
    }

    /* NO OVERRIDE!! We need to stay 0.9*compatible */
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        }
        if (acc.getType() == AccountType.FREE) {
            /* free accounts also have captchas */
            return true;
        }
        return false;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.Turbobit_Turbobit;
    }

    @Override
    public String buildExternalDownloadURL(final DownloadLink link, final PluginForHost buildForThisPlugin) {
        try {
            return getContentURL(this.getHost(), this.getFUID(link));
        } catch (final PluginException e) {
            return null;
        }
    }
}