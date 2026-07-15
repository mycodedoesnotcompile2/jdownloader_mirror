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
package jd.plugins.decrypter;

import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Browser.BrowserException;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.SaveTv;

@DecrypterPlugin(revision = "$Revision: 52981 $", interfaceVersion = 3, names = { "save.tv" }, urls = { "https?://(www\\.)?save\\.tv/STV/M/obj/archive/(?:Horizontal)?VideoArchive\\.cfm.*" })
public class SaveTvDecrypter extends PluginForDecrypt {
    public SaveTvDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setLoadLimit(br.getLoadLimit() * 3);
        br.setFollowRedirects(true);
        return br;
    }

    /* Settings stuff */
    @SuppressWarnings("deprecation")
    private final SubConfiguration cfg                                          = SubConfiguration.getConfig("save.tv");
    private final String           CRAWLER_ONLY_ADD_NEW_IDS                     = "CRAWLER_ONLY_ADD_NEW_IDS";
    private final String           CRAWLER_ACTIVATE                             = "CRAWLER_ACTIVATE";
    private static final String    CRAWLER_PROPERTY_TELECASTIDS_ADDED           = "CRAWLER_PROPERTY_TELECASTIDS_ADDED";
    private static final String    CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS          = "CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS";
    private static final String    CRAWLER_PROPERTY_LASTCRAWL_LATEST_START_DATE = "CRAWLER_PROPERTY_LASTCRAWL_LATEST_START_DATE";
    private static final String    CRAWLER_PROPERTY_LASTCRAWL                   = "CRAWLER_PROPERTY_LASTCRAWL";
    /* Decrypter constants */
    private static final int       API_ENTRIES_PER_REQUEST                      = 1000;
    /* Website gets max 35 items per request. Using too much = server will hate us and return response code 500! */
    private static final int       SITE_ENTRIES_PER_REQUEST                     = 100;
    /*
     * Max time in which save.tv recordings are saved inside a users' account. This value is only used to cleanup the internal HashMap of
     * 'already downloaded' telecastIDs!
     */
    private static final long      TELECAST_ID_EXPIRE_TIME                      = 62 * 24 * 60 * 60 * 1000l;
    /* Decrypter variables */
    final ArrayList<DownloadLink>  ret                                          = new ArrayList<DownloadLink>();
    final ArrayList<String>        dupecheckList                                = new ArrayList<String>();
    private Map<String, Long>      crawledTelecastIDsMap                        = new HashMap<String, Long>();
    private long                   only_grab_entries_of_specified_timeframe     = 0;
    private long                   tdifference_millis                           = 0;
    private int                    totalLinksNum                                = 0;
    private int                    totalAccountsNum                             = 0;
    private int                    totalAccountsLoggedInSuccessfulNum           = 0;
    private int                    requestCountMax                              = 1;
    private long                   timestamp_crawl_started                      = 0;
    private long                   timestamp_last_crawl_ended                   = 0;
    private long                   timestamp_last_record_started                = 0;
    /* Settings */
    private boolean                crawler_DialogsEnabled                       = true;
    private boolean                api_enabled                                  = false;
    private boolean                only_grab_new_entries                        = false;
    /* If this != null, API is currently used */
    private boolean                fast_linkcheck                               = false;

    @Override
    public boolean isProxyRotationEnabledForLinkCrawler() {
        return false;
    }

    /*
     * Never run multiple crawl instances of this plugin at once - but actually because we only have one matching input-URL it is not
     * possible anyways.
     */
    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    @SuppressWarnings({ "deprecation", "unchecked" })
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        timestamp_crawl_started = System.currentTimeMillis();
        api_enabled = jd.plugins.hoster.SaveTv.is_API_enabled(this.getHost());
        fast_linkcheck = cfg.getBooleanProperty(jd.plugins.hoster.SaveTv.CRAWLER_ENABLE_FAST_LINKCHECK, SaveTv.defaultCRAWLER_ENABLE_FAST_LINKCHECK);
        crawler_DialogsEnabled = cfg.getBooleanProperty(jd.plugins.hoster.SaveTv.CRAWLER_ENABLE_DIALOGS, jd.plugins.hoster.SaveTv.defaultCRAWLER_ENABLE_DIALOGS);
        only_grab_new_entries = cfg.getBooleanProperty(CRAWLER_ONLY_ADD_NEW_IDS, jd.plugins.hoster.SaveTv.defaultCRAWLER_ONLY_ADD_NEW_IDS);
        only_grab_entries_of_specified_timeframe = cfg.getLongProperty(jd.plugins.hoster.SaveTv.CRAWLER_GRAB_TIMEFRAME_COUNT, jd.plugins.hoster.SaveTv.defaultCRAWLER_GRAB_TIMEFRAME_COUNT);
        final List<Account> accounts = AccountController.getInstance().getValidAccounts(this.getHost());
        if (accounts == null || accounts.isEmpty()) {
            throw new AccountRequiredException("Save.tv Premium Account benötigt");
        }
        final String source = UrlQuery.parse(param.getCryptedUrl()).get("source");
        final boolean isFromJDToolbar = StringUtils.equalsIgnoreCase(source, "jdtoolbar");
        /*
         * If the source URL was added via JDownloader toolbar, it shall be processed even if the user has deactivated the archive crawler.
         */
        synchronized (jd.plugins.hoster.SaveTv.LOCK) {
            checkFeatureDialogCrawler();
        }
        if (!isFromJDToolbar && !cfg.getBooleanProperty(CRAWLER_ACTIVATE, jd.plugins.hoster.SaveTv.defaultCRAWLER_ACTIVATE)) {
            logger.info("save.tv: Decrypting save.tv archives is disabled, doing nothing...");
            ret.add(this.createOfflinelink(param.getCryptedUrl(), "Archiv_Crawler_in_Plugin_Einstellungen_deaktiviert", "Archiv Crawler in Plugin Einstellungen deaktiviert!"));
            return ret;
        }
        totalAccountsNum = accounts.size();
        try {
            for (final Account account : accounts) {
                if (!getUserLogin(account, false)) {
                    logger.info("Failed to log in account: " + account.getUser());
                    continue;
                }
                timestamp_last_crawl_ended = account.getLongProperty(CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS, 0);
                timestamp_last_record_started = account.getLongProperty(CRAWLER_PROPERTY_LASTCRAWL_LATEST_START_DATE, 0);
                dupecheckList.clear();
                totalAccountsLoggedInSuccessfulNum++;
                if (only_grab_new_entries) {
                    /*
                     * Load list of saved IDs + timestamp when they were added. Always start with a fresh map here (per account!) so that
                     * data of a previously processed account cannot leak into this account's filtering/results, e.g. if this account has
                     * never been crawled with this setting before (crawledIDSMap == null).
                     */
                    crawledTelecastIDsMap = new HashMap<String, Long>();
                    final Object crawledIDSMap = account.getProperty(CRAWLER_PROPERTY_TELECASTIDS_ADDED);
                    if (crawledIDSMap != null && crawledIDSMap instanceof Map) {
                        crawledTelecastIDsMap.putAll((Map<String, Long>) crawledIDSMap);
                    }
                } else {
                    tdifference_millis = only_grab_entries_of_specified_timeframe * 24 * 60 * 60 * 1000;
                }
                if (api_enabled) {
                    api_decrypt_All(account);
                } else {
                    site_decrypt_All(account);
                }
                /*
                 * Must run after the decrypt call: it persists crawledTelecastIDsMap, which addID_site() only populates with this run's
                 * newly found telecastIDs while site_decrypt_All() is running.
                 */
                cleanAndSaveMapOfAddedTelecastIDs(account);
                if (ret.size() > 0) {
                    account.setProperty(CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS, System.currentTimeMillis());
                }
                account.setProperty(CRAWLER_PROPERTY_LASTCRAWL, System.currentTimeMillis());
            }
            logger.info("save.tv: total links found: " + ret.size() + " of " + totalLinksNum);
        } catch (final Throwable e) {
            logger.info("save.tv: total links found: " + ret.size() + " of " + totalLinksNum);
            if (ret.size() >= totalLinksNum) {
                /* This can happen if the user aborts but the crawler already found all links. */
                handleEndDialogs();
                return ret;
            }
            if (e instanceof InterruptedException) {
                logger.log(e);
                logger.info("Decrypt process aborted by user: " + param);
                if (crawler_DialogsEnabled) {
                    try {
                        String message = "Save.tv - Der Crawler wurde frühzeitig vom Benutzer beendet!\r\n";
                        message += "Es wurden bisher " + ret.size() + " von " + totalLinksNum + " Links (telecastIDs) gefunden!";
                        message += getDialogEnd();
                        jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
                    } catch (Throwable e2) {
                        logger.log(e2);
                    }
                }
            } else if (e instanceof BrowserException) {
                try {
                    logger.log(e);
                    String message = "Save.tv - leider wurden nicht alle Links des Archives gefunden!\r\n";
                    message += "Während dem Crawlen ist es zu einem Serverfehler gekommen!\r\n";
                    message += "Wir empfehlen, es zu einem späteren Zeitpunkt nochmals zu versuchen.\r\n";
                    message += "Es wurden nur " + ret.size() + " von " + totalLinksNum + " Links (telecastIDs) gefunden!";
                    message += getDialogEnd();
                    jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
                } catch (Throwable ebr) {
                    logger.log(ebr);
                }
            } else {
                try {
                    logger.log(e);
                    String message = "Save.tv - leider wurden nicht alle Links des Archives gefunden!\r\n";
                    message += "Während dem Crawlen ist es zu einem unbekannten Fehler gekommen!\r\n";
                    message += "Wir empfehlen, es zu einem späteren Zeitpunkt nochmals zu versuchen und uns den Fehler ggf. zu melden.\r\n";
                    message += "Es wurden nur " + ret.size() + " von " + totalLinksNum + " Links (telecastIDs) gefunden!";
                    message += getDialogEnd();
                    jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
                } catch (Throwable ebr) {
                    logger.log(ebr);
                }
            }
            throw e;
        }
        handleEndDialogs();
        return ret;
    }

    /** This will first grab all IDs, then their details as the 2nd used request returns more information than the first one. */
    @SuppressWarnings("unchecked")
    private void api_decrypt_All(final Account account) throws Exception {
        /*
         * We need the parameters here already as we might limit what the API sends us so we should apply the filter to the 'count' request
         * as well so that we get a correct number.
         */
        final String api_records_parameters = getParametersRecordsAPI();
        /* First let's find the number of items to expect */
        api_GET(this.br, "/records/count" + "?" + api_records_parameters);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        totalLinksNum += (int) JavaScriptEngineFactory.toLong(entries.get("count"), 0);
        if (totalLinksNum == 0) {
            /* Can e.g. happen if user uses custom API parameters or user has zero recordings available. */
            logger.info("There are zero entries in archive of current account");
            return;
        }
        /* Find out how many requests we will need */
        final BigDecimal bd = new BigDecimal((double) totalLinksNum / API_ENTRIES_PER_REQUEST);
        requestCountMax = bd.setScale(0, BigDecimal.ROUND_UP).intValue();
        // /* We do not want entries which are in the future! */
        // final String formattedMaxDate = formatToStvDate(time_crawl_started);
        /* Now let's decrypt everything */
        int offset = 0;
        int requestCount = 1;
        /**
         * 'recordstates' Values: <br />
         * 1 = The user has requested the format. <br />
         * 2 = The format was successfully recorded or the recording process failed.<br />
         * 3 = The format was recorded and encoded successfully and the user can download the format.<br />
         * 4 = The recording or encoding process produced errors. The user cannot download the format.<br />
         * 5 = The user has deleted the format. <br />
         * (Comma separated)
         */
        final String api_get_data = "?fields=" + jd.plugins.hoster.SaveTv.getRecordsFieldsValue() + "&" + api_records_parameters + "&offset=" + offset;
        /* API does not tell us the total number of telecastIDs so let's find that out first! */
        List<Object> resourceList;
        long latestRecordTimeTemp = 0;
        pagination: do {
            api_GET(this.br, "/records" + api_get_data + offset);
            resourceList = (List<Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
            for (final Object telecastID_o : resourceList) {
                final DownloadLink link = addID_api(account, telecastID_o);
                latestRecordTimeTemp = link.getLongProperty(jd.plugins.hoster.SaveTv.PROPERTY_originaldate, 0);
                if (latestRecordTimeTemp > timestamp_last_record_started) {
                    timestamp_last_record_started = latestRecordTimeTemp;
                }
                offset++;
            }
            logger.info("Request " + requestCount + " of " + requestCountMax + " | Found " + resourceList.size() + " telecastIDs so far");
            requestCount++;
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            if (resourceList.size() < API_ENTRIES_PER_REQUEST) {
                logger.info("Stopping because: Reached end");
                break pagination;
            }
        } while (!this.isAbort());
        account.setProperty(CRAWLER_PROPERTY_LASTCRAWL_LATEST_START_DATE, timestamp_last_record_started);
    }

    @Deprecated
    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void site_decrypt_All(final Account account) throws Exception {
        boolean is_groups_enabled = false;
        boolean groups_enabled_by_user = false;
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        getPageSafe(account, "https://www." + getHost() + "/STV/M/obj/archive/JSON/VideoArchiveApi.cfm?" + "iEntriesPerPage=1&iCurrentPage=1&dStartdate=0");
        is_groups_enabled = !br.containsHTML("\"IGROUPCOUNT\":1\\.0");
        groups_enabled_by_user = is_groups_enabled;
        final String totalLinksInsideCurrentAccount = PluginJSonUtils.getJsonValue(this.br, "ITOTALENTRIES");
        if (totalLinksInsideCurrentAccount == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final int totalLinksInsideCurrentAccount_int = (int) Double.parseDouble(totalLinksInsideCurrentAccount);
        /* Parse as double as 'totalLinks' can contain dots although it makes absolutely no sense as that number will always be flat! */
        totalLinksNum += totalLinksInsideCurrentAccount_int;
        if (totalLinksInsideCurrentAccount_int == 0) {
            logger.info("WTF zero entries in archive of current account");
            return;
        }
        /* Save on account to display in account information */
        account.setProperty(SaveTv.PROPERTY_acc_count_telecast_ids, Integer.toString(totalLinksInsideCurrentAccount_int));
        final BigDecimal bd = new BigDecimal((double) totalLinksNum / SITE_ENTRIES_PER_REQUEST);
        requestCountMax = bd.setScale(0, BigDecimal.ROUND_UP).intValue();
        int added_entries;
        final long date_current = System.currentTimeMillis();
        /* 2 months before current date */
        final long date_start = date_current - 5259492000l;
        /* One month after current date */
        final long date_end = date_current + 2629746000l;
        final String targetFormat = "yyyy-MM-dd";
        final SimpleDateFormat formatter = new SimpleDateFormat(targetFormat);
        final String date_start_formatted = formatter.format(date_start);
        final String date_end_formatted = formatter.format(date_end);
        try {
            for (int request_num = 1; request_num <= requestCountMax; request_num++) {
                added_entries = 0;
                logger.info("Decrypting request " + request_num + " of " + requestCountMax);
                if (is_groups_enabled) {
                    /* Disable stupid groups setting to crawl faster and to make it work anyways */
                    logger.info("Disabling groups setting");
                    br.postPage("/STV/M/obj/user/submit/submitVideoArchiveOptions.cfm", "ShowGroupedVideoArchive=false");
                    is_groups_enabled = false;
                }
                /* 2016-09-14: dStartdate and dEnddate parameters are important now! */
                br.postPage("/STV/M/obj/archive/JSON/VideoArchiveApi.cfm", "iEntriesPerPage=" + SITE_ENTRIES_PER_REQUEST + "&iCurrentPage=" + request_num + "&dStartdate=" + date_start_formatted + "&dEnddate=" + date_end_formatted);
                final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
                final List<Object> resource_data_list = (List) entries.get("ARRVIDEOARCHIVEENTRIES");
                for (final Object singleid_information : resource_data_list) {
                    addID_site(account, singleid_information);
                    added_entries++;
                }
                logger.info("Found " + added_entries + " entries in request " + request_num + " of " + requestCountMax);
                if (added_entries == 0) {
                    logger.info("Can't find any entries, stopping at request: " + request_num + " of " + requestCountMax);
                    break;
                }
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
        } finally {
            try {
                if (groups_enabled_by_user && !is_groups_enabled) {
                    /* Restore users' groups-setting after decryption if changed */
                    logger.info("Re-enabling groups setting");
                    br.postPage("https://www." + this.getHost() + "/STV/M/obj/user/submit/submitVideoArchiveOptions.cfm", "ShowGroupedVideoArchive=true");
                    logger.info("Successfully re-enabled groups setting");
                }
            } catch (final Throwable settingfail) {
                logger.info("Failed to restore previous groups setting");
            }
        }
    }

    @SuppressWarnings("unchecked")
    private DownloadLink addID_api(final Account account, final Object json_o) throws ParseException, DecrypterException, PluginException {
        final Map<String, Object> entries = (Map<String, Object>) json_o;
        final String telecast_id = Long.toString(JavaScriptEngineFactory.toLong(entries.get("telecastId"), -1));
        if (telecast_id.equalsIgnoreCase("-1")) {
            throw new DecrypterException("Decryption aborted because of BAD telecastID");
        } else if (dupecheckList.contains(telecast_id)) {
            throw new DecrypterException("Decryption aborted because of dupecheck-failure!");
        }
        final DownloadLink link = createStvDownloadlink(account, telecast_id);
        link.setAvailable(true);
        jd.plugins.hoster.SaveTv.parseFilenameInformation_api(link, entries, true);
        jd.plugins.hoster.SaveTv.parseQualityTagAPI(link, jd.plugins.hoster.SaveTv.jsonGetFormatArrayAPI(entries));
        if (telecastID_IS_Allowed(link)) {
            link.setName(jd.plugins.hoster.SaveTv.getFilename(this, link));
            distribute(link);
            ret.add(link);
        }
        dupecheckList.add(telecast_id);
        return link;
    }

    @Deprecated
    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void addID_site(final Account account, final Object object_source) throws ParseException, DecrypterException, PluginException {
        Map<String, Object> entries = (Map<String, Object>) object_source;
        entries = (Map<String, Object>) entries.get("STRTELECASTENTRY");
        final String telecastURL = (String) entries.get("SDETAILSURL");
        final String telecast_id = new Regex(telecastURL, "(\\d+)$").getMatch(0);
        if (dupecheckList.contains(telecast_id)) {
            throw new DecrypterException("Decryption aborted because of dupecheck-failure!");
        }
        final DownloadLink link = createStvDownloadlink(account, telecast_id);
        if (fast_linkcheck) {
            link.setAvailable(true);
        }
        jd.plugins.hoster.SaveTv.parseFilenameInformation_site(link, entries);
        jd.plugins.hoster.SaveTv.parseQualityTagWebsite(link, (List) entries.get("ARRALLOWDDOWNLOADFORMATS"));
        if (telecastID_IS_Allowed(link)) {
            link.setName(jd.plugins.hoster.SaveTv.getFilename(this, link));
            distribute(link);
            ret.add(link);
        }
        /* No matter whether we added the ID or not - we need it on our dupecheck list! */
        dupecheckList.add(telecast_id);
    }

    private DownloadLink createStvDownloadlink(final Account account, final String telecastID) {
        final String account_username = account.getUser();
        final String telecast_url = "https://www.save.tv/STV/M/obj/archive/VideoArchiveDetails.cfm?TelecastId=" + telecastID;
        final DownloadLink link = createDownloadlink(telecast_url);
        link.setName(telecastID + ".mp4");
        link.setContentUrl(telecast_url);
        /* Property is needed to later determine, which url is downloadable via which account. */
        link.setProperty(jd.plugins.hoster.SaveTv.PROPERTY_downloadable_via_username, account_username);
        return link;
    }

    /**
     * Checks if telecastID should be added in respects of the users' settings. Used by API- and website mode. <br />
     * The API also offers a server-side "lastUpdateDate" filter on /v3/records (selects records updated/created after a given date, exposed
     * via the record field "updatedate") which would track completion time instead of broadcast start time and could replace this local
     * telecastID map. Not used here because the local map already works well enough for our purposes.
     */
    private boolean telecastID_IS_Allowed(final DownloadLink link) {
        if (only_grab_new_entries) {
            final String telecastID = getTelecastID(link);
            if (crawledTelecastIDsMap.containsKey(telecastID)) {
                return false;
            }
            /* User only wants telecastIDs which he did not add before and this ID has not been added before --> Allow to add it! */
            crawledTelecastIDsMap.put(telecastID, link.getLongProperty(jd.plugins.hoster.SaveTv.PROPERTY_originaldate, 0));
            return true;
        }
        if (tdifference_millis > 0) {
            /* User only wants telecastIDs of a user-defined time-range. */
            final long datemillis = link.getLongProperty(jd.plugins.hoster.SaveTv.PROPERTY_originaldate, 0);
            final long current_tdifference = timestamp_crawl_started - datemillis;
            if (current_tdifference > tdifference_millis) {
                return false;
            }
        }
        return true;
    }

    private void cleanAndSaveMapOfAddedTelecastIDs(final Account account) {
        /*
         * Let's clean our ID map. TelecastIDs automatically get deleted after XX days so we do not need to store them longer than that as
         * it will eat up more RAM/space for no reason. 2017-09-22: This is currently only used for website crawling.
         */
        synchronized (crawledTelecastIDsMap) {
            final Iterator<Entry<String, Long>> it = crawledTelecastIDsMap.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Long> entry = it.next();
                final long timestamp = entry.getValue();
                if (System.currentTimeMillis() - timestamp >= TELECAST_ID_EXPIRE_TIME) {
                    /* Remove old entries */
                    it.remove();
                }
            }
        }
        /* Save telecastID map so later we know what is new and what we crawled before ;) */
        account.setProperty(CRAWLER_PROPERTY_TELECASTIDS_ADDED, crawledTelecastIDsMap);
    }

    /**
     * Not to be mistaken by the function of the host plugin. This basically does the same but with less code so that our crawler is more
     * efficient.
     */
    private static String getTelecastID(final DownloadLink link) {
        return new Regex(link.getDownloadURL(), "(\\d+)$").getMatch(0);
    }

    public static String formatMillisecondsToStvDateAPI(final long millis) {
        final Date theDate = new Date(millis);
        final SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss'Z'");
        final String formattedDate = formatter.format(theDate);
        return formattedDate;
    }

    public static String formatTimestampToGermanDate(final long millis) {
        final Date theDate = new Date(millis);
        final SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy HH:mm");
        final String formattedDate = formatter.format(theDate) + " Uhr";
        return formattedDate;
    }

    /** Returns basic parameters for '/records' request. */
    private String getParametersRecordsAPI() {
        final String user_defined_api_parameters_raw = cfg.getStringProperty(jd.plugins.hoster.SaveTv.CUSTOM_API_PARAMETERS_CRAWLER, jd.plugins.hoster.SaveTv.defaultCUSTOM_API_PARAMETERS_CRAWLER);
        UrlQuery userQuery = new UrlQuery();
        try {
            userQuery = UrlQuery.parse(user_defined_api_parameters_raw);
        } catch (MalformedURLException e) {
            logger.log(e);
            logger.info("User has entered invalid custom UrlQuery");
        }
        final boolean user_defined_api_parameters_contains_forbidden_parameters = userQuery.containsKey("limit") || userQuery.containsKey("fields") || userQuery.containsKey("nopagingheader") || userQuery.containsKey("offset");
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("nopagingheader", "false");
        query.appendEncoded("limit", Integer.toString(API_ENTRIES_PER_REQUEST));
        if (!userQuery.containsKey("recordstates")) {
            query.appendEncoded("recordstates", "2");
        }
        if (only_grab_entries_of_specified_timeframe > 0) {
            final String formattedMinDate = formatMillisecondsToStvDateAPI(System.currentTimeMillis() - (only_grab_entries_of_specified_timeframe * 24 * 60 * 60 * 1000));
            logger.info("User only wants last X entries --> Only crawling everything > " + formattedMinDate);
            if (!userQuery.containsKey("minstartdate")) {
                query.appendEncoded("minstartdate", formattedMinDate);
            } else {
                logger.warning("Cannot apply user defined minstartdate because user has put a minstartdate parameter into the custom list of parameters");
            }
        }
        if (!user_defined_api_parameters_contains_forbidden_parameters) {
            for (final Entry<String, String> userParameter : userQuery.toMap(true).entrySet()) {
                query.appendEncoded(userParameter.getKey(), userParameter.getValue());
            }
        }
        return query.toString();
    }

    @SuppressWarnings("deprecation")
    private boolean getUserLogin(final Account account, final boolean force) throws Exception {
        if (account == null) {
            return false;
        }
        try {
            if (api_enabled) {
                jd.plugins.hoster.SaveTv.login_api(this.br, account, force);
            } else {
                jd.plugins.hoster.SaveTv.login_website(this.br, account, force);
            }
        } catch (final PluginException e) {
            account.setValid(false);
            return false;
        }
        return true;
    }

    private void getPageSafe(final Account account, final String url) throws Exception {
        // Limits made by me (pspzockerscene):
        // Max 6 logins possible
        // Max 15 accesses of the link possible
        // -> Max 21 total requests
        int failcounter_url = 0;
        for (int i = 0; i <= 2; i++) {
            boolean failed = true;
            do {
                try {
                    br.getPage(url);
                    failed = false;
                } catch (final BrowserException e) {
                    failed = true;
                    failcounter_url++;
                    if (failcounter_url > 4) {
                        logger.info("Failed to avoid timeouts / server issues");
                        throw e;
                    }
                }
            } while (failed);
            if (br.getURL().contains(jd.plugins.hoster.SaveTv.URL_LOGGED_OUT)) {
                for (int i2 = 0; i2 <= 1; i2++) {
                    logger.info("Link redirected to login page, logging in again to retry this: " + url);
                    logger.info("Try " + i2 + " of 1");
                    try {
                        getUserLogin(account, true);
                    } catch (final BrowserException e) {
                        logger.info("Login " + i2 + " of 1 failed, re-trying...");
                        continue;
                    }
                    logger.info("Re-Login " + i2 + " of 1 successful...");
                    break;
                }
                continue;
            }
            break;
        }
    }

    /**
     * Performs save.tv API GET requests. <br />
     * TODO: Add error handling
     *
     * @throws Exception
     */
    private String api_GET(final Browser br, String url) throws Exception {
        url = jd.plugins.hoster.SaveTv.correctURLAPI(url);
        br.getPage(url);
        return br.getRequest().getHtmlCode();
    }

    private void handleEndDialogs() {
        if (!crawler_DialogsEnabled) {
            return;
        }
        if (only_grab_new_entries && ret.size() == 0) {
            /* User recently added all new entries and now there are no new entries available. */
            try {
                String message = "Save.tv - es wurden keine neuen Aufnahmen gefunden!";
                if (timestamp_last_crawl_ended > 0) {
                    message += String.format("\r\nBedenke, dass du am %s bereits alle neuen Aufnahmen eingefügt hast.", formatTimestampToGermanDate(timestamp_last_crawl_ended));
                }
                message += "\r\n" + getDialogAccountsInfo();
                message += getDialogEnd();
                jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
            } catch (final Throwable e) {
                logger.log(e);
            }
        } else if (only_grab_new_entries) {
            /* User recently added all new entries and now there are no new entries available. */
            try {
                String message = "Save.tv - es wurden " + ret.size() + " neue Aufnahmen gefunden!";
                if (timestamp_last_crawl_ended > 0) {
                    message += String.format("\r\nDas sind alle neuen Aufnahmen seitdem zuletzt neue gefunden wurden am %s", formatTimestampToGermanDate(timestamp_last_crawl_ended));
                }
                message += "\r\n" + getDialogAccountsInfo();
                message += getDialogEnd();
                jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
            } catch (final Throwable e) {
                logger.log(e);
            }
        } else if (only_grab_entries_of_specified_timeframe > 0 && ret.size() == 0) {
            try {
                String message = "Save.tv - leider wurden keine Links gefunden!";
                message += "\r\nBedenke, dass du nur alle Aufnahmen der letzten " + only_grab_entries_of_specified_timeframe + " Tage möchtest.";
                message += String.format("\r\nDas sind alle Aufnahmen ab dem %s.", formatTimestampToGermanDate(System.currentTimeMillis() - (only_grab_entries_of_specified_timeframe * 24 * 60 * 60 * 1000)));
                message += "\r\nVermutlich gab es in diesem Zeitraum keine neuen Aufnahmen!";
                message += "\r\n" + getDialogAccountsInfo();
                message += getDialogEnd();
                jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
            } catch (final Throwable e) {
                logger.log(e);
            }
        } else if (only_grab_entries_of_specified_timeframe > 0) {
            try {
                String message = "Save.tv Archiv-Crawler - alle Aufnahmen der letzten " + only_grab_entries_of_specified_timeframe + " Tage wurden gefunden!";
                message += String.format("\r\nDas sind alle Aufnahmen ab dem %s.", formatTimestampToGermanDate(System.currentTimeMillis() - (only_grab_entries_of_specified_timeframe * 24 * 60 * 60 * 1000)));
                message += "\r\nEs wurden " + ret.size() + " Links gefunden!";
                message += "\r\n" + getDialogAccountsInfo();
                message += getDialogEnd();
                jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
            } catch (final Throwable e) {
                logger.log(e);
            }
        } else if (ret.size() >= totalLinksNum) {
            try {
                String message = "Save.tv - alle Links des Archives wurden gefunden!";
                message += "\r\nEs wurden " + ret.size() + " von " + totalLinksNum + " Links gefunden!";
                message += "\r\n" + getDialogAccountsInfo();
                message += getDialogEnd();
                jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
            } catch (final Throwable e) {
                logger.log(e);
            }
        } else if (ret.size() < totalLinksNum) {
            try {
                String message = "Save.tv - leider wurden nicht alle Links des Archives gefunden!";
                message += "\r\nEs wurden nur " + ret.size() + " von " + totalLinksNum + " Links (telecastIDs) gefunden!";
                message += "\r\n" + getDialogAccountsInfo();
                message += getDialogEnd();
                jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
    }

    private String getDialogAccountsInfo() {
        final String message = "Es wurden Archive von insgesamt " + totalAccountsNum + " save.tv Account(s) durchsucht.\r\nDavon erfolgreich: " + totalAccountsLoggedInSuccessfulNum;
        return message;
    }

    private String getDialogEnd() {
        final long crawl_duration = System.currentTimeMillis() - timestamp_crawl_started;
        String message = "\r\n";
        message += "Dauer des Crawlvorganges: " + TimeFormatter.formatMilliSeconds(crawl_duration, 0);
        message += "\r\n\r\nGenervt von diesen Info-Dialogen? In den Plugin Einstellungen kannst du sie deaktivieren ;)";
        return message;
    }

    @SuppressWarnings("deprecation")
    private void checkFeatureDialogCrawler() {
        SubConfiguration config = null;
        try {
            config = getPluginConfig();
            if (config.getBooleanProperty("featuredialog_crawler_Shown", Boolean.FALSE) == false) {
                if (config.getProperty("featuredialog_crawler_Shown2") == null) {
                    showFeatureDialogCrawler();
                } else {
                    config = null;
                }
            } else {
                config = null;
            }
        } catch (final Throwable e) {
        } finally {
            if (config != null) {
                config.setProperty("featuredialog_crawler_Shown", Boolean.TRUE);
                config.setProperty("featuredialog_crawler_Shown2", "shown");
                config.save();
            }
        }
    }

    private void showFeatureDialogCrawler() {
        try {
            String message = "Hallo lieber save.tv Nutzer/liebe save.tv Nutzerin\r\n";
            message += "Das save.tv Crawler Plugin bietet folgende Features:\r\n";
            message += "- Info-Dialoge\r\n";
            message += "- Die Möglichkeit, immer nur neue abgeschlossene Aufnahmen zu crawlen\r\n";
            message += "- Die Möglichkeit, wahlweise alle oder nur Aufnahmen der letzten X Tage zu crawlen\r\n";
            message += "- Einfügen aller Aufnahmen des Save.tv Videoarchivs über den Save.tv Button in der Toolbar oben\r\n";
            message += "\r\n";
            message += "Um den Archiv-Crawler nutzen zu können, musst du einen gültigen save.tv Account in JD eintragen und den Crawler in den Plugin Einstellungen aktivieren.\r\n";
            message += "\r\n";
            message += "Die Crawler Einstellungen findest du unter: Einstellungen -> Plugin Einstellungen -> save.tv";
            message += jd.plugins.hoster.SaveTv.getMessageEnd();
            jd.plugins.hoster.SaveTv.showAutoCloseDialog("save.tv Archiv-Crawler", message);
        } catch (Throwable e) {
            logger.log(e);
        }
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account account) {
        return false;
    }
}
