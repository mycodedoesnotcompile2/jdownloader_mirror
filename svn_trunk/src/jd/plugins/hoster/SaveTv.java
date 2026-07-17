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

import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.translate._JDT;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginConfigPanelNG;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SyncSaveTvToolbarAction;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 52997 $", interfaceVersion = 3, names = {}, urls = {})
public class SaveTv extends PluginForHost {
    /* Static information */
    /* API functions developed for API version 3.0.0.1631 */
    // private static final String API_APP_NAME = "JDownloader";
    private static final String   API_PUBLIC_KEY                               = "2005d51304a04a6398bfbc1f3c2a1c8b";
    private static final String   API_SECRET_KEY                               = "3711128ae57644e9a6278adda57a85de457a257fc3ca4130ab5ac863940923be";
    public static final String    HOST_STATIC                                  = "save.tv";
    /* Linktypes */
    public static final String    LINKTYPE_TELECAST_ID                         = "(?i).+/STV/M/obj/archive/VideoArchiveDetails\\.cfm\\?TelecastID=(\\d+)";
    /*
     * User has programmed something but it has not aired yet (is not downloadable yet) OR it is offline for a long time already
     */
    public static final String    LINKTYPE_TELECAST_ID_RECORD_OVERVIEW         = "(?i).+/STV/M/obj/TC/SendungsDetails\\.cfm\\?TelecastID=\\d+";
    public static final String    LINKTYPE_TELECAST_ID_VIDEO_ARCHIVE_STREAMING = "(?i).+/STV/M/obj/archive/VideoArchiveStreaming\\.cfm\\?TelecastID=\\d+";
    public static final String    LINKTYPE_DIRECT                              = "https?://[A-Za-z0-9\\-]+\\.save\\.tv/\\d+_\\d+_.+";
    /* API static information */
    public static final String    API_BASE                                     = "https://api.save.tv/v3";
    public static final String    API_BASE_AUTH                                = "https://auth.save.tv/";
    public static final double    QUALITY_HD_MB_PER_SECOND                     = 0.3735593220338983;
    public static final double    QUALITY_H264_NORMAL_MB_PER_SECOND            = 0.1944268524382521;
    public static final double    QUALITY_H264_MOBILE_MB_PER_SECOND            = 0.0740975300823306;
    /* Properties */
    private static final String   CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS          = "CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS";
    private static final String   CRAWLER_PROPERTY_LASTCRAWL                   = "CRAWLER_PROPERTY_LASTCRAWL";
    /* Frequently used internal plugin properties */
    public static final String    PROPERTY_ACCOUNT_API_SESSIONID               = "sessionid";
    public static final String    PROPERTY_quality                             = "quality";
    public static final String    PROPERTY_plainfilename                       = "plainfilename";
    public static final String    PROPERTY_server_filename                     = "server_filename";
    public static final String    PROPERTY_acc_username                        = "acc_username";
    public static final String    PROPERTY_ad_free                             = "ad_free";
    public static final String    PROPERTY_producecountry                      = "producecountry";
    /* https://www.devisenrechner.info/laenderkuerzel-laenderabkuerzungen-laendercode-iso-3166.htm */
    public static final String    PROPERTY_producecountry_short                = "producecountry_short";
    public static final String    PROPERTY_genre                               = "genre";
    public static final String    PROPERTY_type                                = "type";
    public static final String    PROPERTY_produceyear                         = "produceyear";
    public static final String    PROPERTY_plain_tv_station                    = "plain_tv_station";
    public static final String    PROPERTY_episodename                         = "episodename";
    public static final String    PROPERTY_originaldate                        = "originaldate";
    public static final String    PROPERTY_episodenumber                       = "episodenumber";
    public static final String    PROPERTY_seasonnumber                        = "seasonnumber";
    public static final String    PROPERTY_acc_count_telecast_ids              = "acc_count_telecast_ids";
    public static final String    PROPERTY_acc_type                            = "acc_type";
    public static final String    PROPERTY_acc_count_archive_entries           = "acc_count_archive_entries";
    public static final String    PROPERTY_category_id                         = "category_id";
    public static final String    PROPERTY_stv_randomnumber                    = "stv_randomnumber";
    public static final String    PROPERTY_originaldate_end                    = "originaldate_end";
    public static final String    PROPERTY_site_runtime_seconds_withads        = "site_runtime_minutes";
    public static final String    PROPERTY_site_runtime_seconds_adsfree        = "site_runtime_minutes_adsfree";
    public static final String    PROPERTY_acc_expire                          = "acc_expire";
    public static final String    PROPERTY_acc_package                         = "acc_package";
    public static final String    PROPERTY_acc_price                           = "acc_price";
    public static final String    PROPERTY_acc_runtime                         = "acc_runtime";
    public static final String    PROPERTY_has_moved                           = "has_moved";
    public static final String    PROPERTY_downloadable_via_username           = "downloadable_via";
    public static final String    PROPERTY_refresh_token                       = "refresh_token";
    public static final String    PROPERTY_expires_in                          = "expires_in";
    /**
     * Format-ID and ads-free state which were actually used to request the currently running/resumable download. <br />
     * Set right before a download really starts, read (with priority over current settings) when a download resumes so that a resumed
     * filestream is always continued with the exact same format it was started with - even if the user changes his settings in the
     * meantime.
     */
    public static final String    PROPERTY_download_format_id                  = "download_format_id";
    public static final String    PROPERTY_download_ads_free                   = "download_ads_free";
    /* Settings stuff */
    private static final String   PROPERTY_USEORIGINALFILENAME                 = "USEORIGINALFILENAME";
    public static final String    PROPERTY_PREFERADSFREE                       = "PREFERADSFREE";
    private static final String   ADS_FREE_UNAVAILABLE_HOURS                   = "DOWNLOADONLYADSFREE_RETRY_HOURS_2";
    private final static String   SELECTED_VIDEO_FORMAT                        = "selected_video_format";
    /* Text strings displayed to the user in various cases */
    private final static String   USERTEXT_NOCUTAVAILABLE                      = "Für diese Sendung steht (noch) keine Schnittliste zur Verfügung";
    /* The list of qualities/formats displayed to the user */
    private static final String[] FORMATS                                      = new String[] { "HD", "H.264 HQ", "H.264 MOBILE" };
    /* Crawler settings */
    private static final String   CRAWLER_ONLY_ADD_NEW_IDS                     = "CRAWLER_ONLY_ADD_NEW_IDS";
    private static final String   CRAWLER_ACTIVATE                             = "CRAWLER_ACTIVATE";
    public static final String    CRAWLER_ENABLE_FAST_LINKCHECK                = "CRAWLER_ENABLE_FASTER_2";
    public static final String    CRAWLER_ENABLE_DIALOGS                       = "CRAWLER_ENABLE_DIALOGS";
    public static final String    CRAWLER_GRAB_TIMEFRAME_COUNT                 = "CRAWLER_GRAB_TIMEFRAME_COUNT";
    public static final String    SYNC_TOOLBAR_ENABLE_DIALOG                   = "SYNC_TOOLBAR_ENABLE_DIALOG";
    private static final String   DELETE_TELECAST_ID_AFTER_DOWNLOAD            = "DELETE_TELECAST_ID_AFTER_DOWNLOAD";
    private static final String   DELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS    = "DELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS";
    /* Custom filename settings stuff */
    private static final String   CUSTOM_DATE                                  = "CUSTOM_DATE";
    private static final String   CUSTOM_FILENAME_MOVIES                       = "CUSTOM_FILENAME_MOVIES";
    private static final String   CUSTOM_FILENAME_SERIES                       = "CUSTOM_FILENAME_SERIES_2017_10_02";
    public static final String    CUSTOM_API_PARAMETERS_CRAWLER                = "CUSTOM_API_PARAMETERS_CRAWLER";
    /*
     * Spelling of the string value is kept as-is on purpose: it is a persisted per-user config key, changing it would reset existing users'
     * setting.
     */
    private static final String   CUSTOM_FILENAME_SEPARATION_MARK              = "CUSTOM_FILENAME_SEPERATION_MARK";
    private static final String   CUSTOM_FILENAME_EMPTY_TAG_STRING             = "CUSTOM_FILENAME_EMPTY_TAG_STRING";
    private static final String   FORCE_ORIGINALFILENAME_SERIES                = "FORCE_ORIGINALFILENAME_SERIES";
    private static final String   FORCE_ORIGINALFILENAME_MOVIES                = "FORCE_ORIGINALFILENAME_MOVIES";
    /* Download connections constants */
    private static final boolean  ACCOUNT_PREMIUM_RESUME                       = true;
    private static final int      ACCOUNT_PREMIUM_MAXCHUNKS                    = -2;
    /* Other API/site errorhandling constants */
    public static final String    URL_LOGGED_OUT                               = "Token=MSG_LOGOUT_B";
    /* Property / Filename constants / States / Small user display texts */
    public static final String    STATE_QUALITY_LQ                             = "LQ";
    public static final String    STATE_QUALITY_HQ                             = "HQ";
    public static final String    STATE_QUALITY_HD                             = "HD";
    public static final String    STATE_QUALITY_UNKNOWN                        = "XX";
    public static final String    STATE_ad_free_true                           = "true";
    public static final String    STATE_ad_free_false                          = "false";
    public static final String    STATE_ad_free_unknown                        = "XX";
    public static final String    EXTENSION_default                            = ".mp4";
    /* Save.tv internal quality/format constants (IDs) for the API & website */
    private static final int      SITE_FORMAT_HD                               = 6;
    private static final int      SITE_FORMAT_HQ                               = 5;
    private static final int      SITE_FORMAT_LQ                               = 4;
    /* Other */
    public static Object          LOCK                                         = new Object();
    private Account               currAcc                                      = null;
    private DownloadLink          currDownloadlink                             = null;

    @SuppressWarnings("deprecation")
    public SaveTv(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/stv/s/obj/registration/RegPage1.cfm");
        setConfigElements();
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String telecast_id = getTelecastId(link);
        if (telecast_id != null) {
            return this.getHost() + "://telecast-id/" + telecast_id;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/STV/S/misc/terms.cfm";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "save.tv" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    /* Website URLs, e.g. https://www.save.tv/STV/M/obj/archive/VideoArchiveDetails.cfm?TelecastID=12345678 */
    private static final Pattern PATTERN_TELECAST = Pattern.compile("/STV/M/obj/(?:archive/VideoArchiveDetails|archive/VideoArchiveStreaming|TC/SendungsDetails)\\.cfm\\?TelecastID=(\\d+).*");
    /* Direct video-CDN URLs served from an arbitrary subdomain, e.g. https://xyz123.save.tv/1_12345678_something */
    private static final Pattern PATTERN_DIRECT   = Pattern.compile("/\\d+_(\\d+)_.+");

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_TELECAST.pattern() + "|https?://[A-Za-z0-9\\-]+\\." + buildHostsPatternPart(domains) + PATTERN_DIRECT.pattern());
        }
        return ret.toArray(new String[0]);
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
    public boolean isProxyRotationEnabledForLinkChecker() {
        return false;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        /* Forget which format was used for a previous download attempt so it can be freshly re-evaluated from current settings. */
        link.removeProperty(PROPERTY_download_format_id);
        link.removeProperty(PROPERTY_download_ads_free);
    }

    private void setConstants(final Account account, final DownloadLink link) {
        this.currAcc = account;
        this.currDownloadlink = link;
    }

    public static String getAPIClientID() {
        return API_PUBLIC_KEY;
    }

    public static String getAPISecretKey() {
        return API_SECRET_KEY;
    }

    @Override
    public boolean assignPlugin(Account account) {
        SyncSaveTvToolbarAction.registerExtender();
        return super.assignPlugin(account);
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        final String telecast_ID = getTelecastId(link);
        return telecast_ID + EXTENSION_default;
    }

    /**
     * @property "category": 0 = undefined, 1 = movies,category: 2 = series, 3 = show, 7 = music
     */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        correctDownloadLink(link);
        br.setFollowRedirects(true);
        /* Show telecast-ID + extension as dummy name for all error cases */
        final String telecast_ID = getTelecastId(link);
        if (telecast_ID == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "telecastID is null");
        }
        Account account = null;
        final String account_username_via_which_url_is_downloadable = getDownloadableViaUsername(link);
        final List<Account> all_stv_accounts = AccountController.getInstance().getValidAccounts(this.getHost());
        final String statustext_account_needed = "Kann Links ohne gültigen und dazugehörigen Account nicht überprüfen";
        if (all_stv_accounts == null || all_stv_accounts.size() == 0) {
            link.getLinkStatus().setStatusText(statustext_account_needed);
            checkAccountNeededDialog();
            return AvailableStatus.UNCHECKABLE;
        } else if (account_username_via_which_url_is_downloadable == null && all_stv_accounts.size() == 1) {
            /* User probably added save.tv urls manually and has only one account --> Allow these to be downloaded via this account! */
            account = all_stv_accounts.get(0);
            link.setProperty(PROPERTY_downloadable_via_username, account.getUser());
        } else {
            /* Find account via which we can use to download our url. */
            for (final Account accountTmp : all_stv_accounts) {
                if (this.canHandle(link, accountTmp)) {
                    account = accountTmp;
                    break;
                }
            }
            if (account == null) {
                link.getLinkStatus().setStatusText(statustext_account_needed);
                checkAccountNeededDialog();
                return AvailableStatus.UNCHECKABLE;
            }
        }
        requestFileInformation(link, account);
        return AvailableStatus.TRUE;
    }

    private void requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (link == null || account == null) {
            throw new IllegalArgumentException();
        }
        setConstants(account, link);
        if (is_API_enabled(this.getHost())) {
            requestFileInformationAPI(link, account);
        } else {
            requestFileInformationWebsite(link, account);
        }
        link.setAvailable(true);
        final String availablecheck_filename = getFilename(this, link);
        /*
         * Reset (final) filename from previous state so we can use the final filename as final filename later even if it has changed before
         */
        link.setFinalFileName(null);
        link.setName(null);
        link.setName(availablecheck_filename);
    }

    @Deprecated
    @SuppressWarnings({ "unchecked" })
    private void requestFileInformationWebsite(final DownloadLink link, final Account account) throws Exception {
        login_website(this.br, account, false);
        getPageSafe("https://www." + this.getHost() + "/STV/M/obj/archive/JSON/VideoArchiveDetailsApi.cfm?TelecastID=" + getTelecastId(link), account);
        if (!br.getURL().contains("/JSON/") || this.br.getHttpConnection().getResponseCode() == 404) {
            /* Offline#1 - offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
        final Object aRRALLOWDDOWNLOADFORMATS = entries.get("ARRALLOWDDOWNLOADFORMATS");
        final Object sTRRECORDORDER = entries.get("STRRECORDORDER");
        if (aRRALLOWDDOWNLOADFORMATS == null || sTRRECORDORDER == null) {
            /*
             * Offline#2 - expired (download not possible anymore - if user tries to download something that has not been recorded yet, code
             * will NOT jump into this as json will already contain some download information / these two important json objects)
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final List<Object> sourcelist = jsonGetVideoSourcelist(entries);
        entries = (Map<String, Object>) entries.get("TELECASTDETAILS");
        parseFilenameInformation_site(link, entries);
        parseQualityTagWebsite(link, sourcelist);
        link.setAvailable(true);
    }

    private void requestFileInformationAPI(final DownloadLink link, final Account account) throws Exception {
        login_api(this.br, account, false);
        /* Let's assume that all URLs the user adds are telecastIDs which have already been recorded. */
        /* TODO: Maybe add error handling based on recordStateID, see: https://api.save.tv:443/v3/recordstates */
        final String telecastID = getTelecastId(link);
        List<Object> qualityList = null;
        Map<String, Object> entries = null;
        boolean existsRecord = false;
        if (isTypeTelecastIDOverview(link)) {
            /* telecast info --> If available, find record info */
            callAPITelecastsSingle(telecastID);
            if (isOfflineAPI(this.br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            existsRecord = ((Boolean) entries.get("existsRecord")).booleanValue();
            if (existsRecord) {
                /* Item downloadable --> Find quality list */
                logger.info("Assumed not-yet-recorded telecastID is recorded and downloadable");
                callAPIRecordsSingle(telecastID);
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                /* Set current correct downloadurl so that on next linkcheck, we can request the record information right away. */
                link.setUrlDownload(buildArchiveDownloadURL(link));
            }
        } else {
            /* record info --> If NOT available, find telecast info */
            callAPIRecordsSingle(telecastID);
            if (!isOfflineAPI(this.br)) {
                /* Item downloadable --> Find quality list */
                existsRecord = true;
                /*
                 * Only parse json if we know that the telecastID is not offline because else we might get an Exception as we get a Map
                 * instead of the expected LinkedHashMap!
                 */
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            } else {
                /* Item not downloadable --> At least try to get general information about this ID */
                logger.info("Failed to find record --> Checking if maybe it hasn't been recorded yet or is too old (offline)");
                callAPITelecastsSingle(telecastID);
                if (isOfflineAPI(this.br)) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            }
        }
        if (existsRecord) {
            qualityList = jsonGetFormatArrayAPI(entries);
        }
        parseFilenameInformation_api(link, entries, existsRecord);
        parseQualityTagAPI(link, qualityList);
        if (!existsRecord) {
            if (link.getLongProperty(PROPERTY_originaldate_end, 0) < System.currentTimeMillis()) {
                /*
                 * TODO: Maybe make this boolean public to make SURE that we do never try to download such URLs as we KNOW that they are NOT
                 * downloadable.
                 */
                logger.info("This telecastID has probably been deleted a long time ago --> Found information but it is offline nevertheless");
                /*
                 * As a final attempt to provide a working example URL for the user, let's set the 'overview-URL' to make sure whenever he
                 * copies the URL he will have a URL which actually shows information in browser. We could also verify this status via
                 * TELECAST_ID_EXPIRE_TIME but it leaves room for errors open so let's assume that this is a definite offline case!
                 */
                link.setContentUrl(buildNotYetRecordedOrOfflineDownloadURL(link));
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!allowNonProgrammedTelecastIDs()) {
                logger.info("telecastID exists but has not been programmed to record AND non programmed telecastIDs are not allowed at the moment --> Display as offline");
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            logger.info("telecastID is online but has not not been programmed by the user --> NOT downloadable at this stage");
        }
    }

    private void callAPITelecastsSingle(final String telecastID) throws Exception {
        api_GET(this.br, "/telecasts/" + telecastID + "?fields=country%2C%20description%2C%20enddate%2C%20episode%2C%20existsrecord%2C%20id%2C%20startdate%2C%20subject%2C%20subtitle%2C%20title%2C%20tvcategory.id%2C%20tvcategory.name%2C%20tvstation.id%2C%20tvstation.name%2C%20tvsubcategory.id%2C%20tvsubcategory.name%2C%20year");
    }

    private void callAPIRecordsSingle(final String telecastID) throws Exception {
        api_GET(this.br, "/records/" + telecastID + "?fields=" + getRecordsFieldsValue());
    }

    public static String getRecordsFieldsValue() {
        /*
         * Note: the record-level "createdate", "startdate" and "enddate" fields are intentionally NOT requested here (unlike their
         * telecast.* counterparts): parseFilenameInformation_api() only ever reads the "telecast" sub-object's dates, so the record-level
         * ones would just be unused response payload.
         */
        return "adfreeavailable%2C%20adfreelength%2C%20defect.encoding.telecast.availablelength%2C%20defect.encoding.telecast.expectedlength%2C%20telecast.hasmoved%2C%20formats%2C%20formats.recordformat.id%2C%20formats.recordformat.name%2C%20formats.recordstate.id%2C%20formats.recordstate.name%2C%20formats.retentiondate%2C%20formats.uncutvideosize%2C%20isadcutenabled%2C%20telecast.country%2C%20telecast.description%2C%20telecast.enddate%2C%20telecast.episode%2C%20telecast.id%2C%20telecast.startdate%2C%20telecast.subject%2C%20telecast.subtitle%2C%20telecast.title%2C%20telecast.tvcategory.id%2C%20telecast.tvcategory.name%2C%20telecast.tvstation.id%2C%20telecast.tvstation.name%2C%20telecast.tvsubcategory.id%2C%20telecast.tvsubcategory.name%2C%20telecast.year%2C%20telecastid";
    }

    /** See method 'handleErrorsAPI' for more information on offline content. */
    private boolean isOfflineAPI(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 404;
    }

    public static String getProduceCountryShort(String produceCountriesLong) {
        if (produceCountriesLong == null) {
            return null;
        }
        produceCountriesLong = produceCountriesLong.toUpperCase();
        /* ISO2 to country-code (german) according to ISO 3166 */
        final Map<String, String> countryMapper = new HashMap<String, String>();
        /* 2019-12-13: I build a small 'converter' and extracted most of this list from here: https://de.switch-case.com/46232452 */
        countryMapper.put("AFGHANISTAN", "AF");
        countryMapper.put("ÅLAND", "AX");
        countryMapper.put("ALBANIEN", "AL");
        countryMapper.put("ALGERIEN", "DZ");
        countryMapper.put("AMERIKANISCH-SAMOA", "AS");
        countryMapper.put("ANDORRA", "AD");
        countryMapper.put("ANGOLA", "AO");
        countryMapper.put("ANGUILLA", "AI");
        countryMapper.put("ANTARKTIKA", "AQ");
        countryMapper.put("ANTIGUA UND BARBUDA", "AG");
        countryMapper.put("ARGENTINIEN", "AR");
        countryMapper.put("ARMENIEN", "AM");
        countryMapper.put("ARUBA", "AW");
        countryMapper.put("AUSTRALIEN", "AU");
        countryMapper.put("ÖSTERREICH", "AT");
        countryMapper.put("ASERBAIDSCHAN", "AZ");
        countryMapper.put("BAHAMAS", "BS");
        countryMapper.put("BAHRAIN", "BH");
        countryMapper.put("BANGLADESCH", "BD");
        countryMapper.put("BARBADOS", "BB");
        countryMapper.put("WEISSRUSSLAND", "BY");
        countryMapper.put("BELGIEN", "BE");
        countryMapper.put("BELIZE", "BZ");
        countryMapper.put("BENIN", "BJ");
        countryMapper.put("BERMUDA", "BM");
        countryMapper.put("BHUTAN", "BT");
        countryMapper.put("BOLIVIEN", "BO");
        countryMapper.put("BOSNIEN UND HERZEGOWINA", "BA");
        countryMapper.put("BOTSWANA", "BW");
        countryMapper.put("BOUVETINSEL", "BV");
        countryMapper.put("BRASILIEN", "BR");
        countryMapper.put("BRITISCHES TERRITORIUM IM INDISCHEN OZEAN", "IO");
        countryMapper.put("KLEINERE INSELBESITZUNGEN DER VEREINIGTEN STAATEN", "UM");
        countryMapper.put("BRITISCHE JUNGFERNINSELN", "VG");
        countryMapper.put("AMERIKANISCHE JUNGFERNINSELN", "VI");
        countryMapper.put("BRUNEI", "BN");
        countryMapper.put("BULGARIEN", "BG");
        countryMapper.put("BURKINA FASO", "BF");
        countryMapper.put("BURUNDI", "BI");
        countryMapper.put("KAMBODSCHA", "KH");
        countryMapper.put("KAMERUN", "CM");
        countryMapper.put("KANADA", "CA");
        countryMapper.put("KAP VERDE", "CV");
        countryMapper.put("KAIMANINSELN", "KY");
        countryMapper.put("ZENTRALAFRIKANISCHE REPUBLIK", "CF");
        countryMapper.put("TSCHAD", "TD");
        countryMapper.put("CHILE", "CL");
        countryMapper.put("CHINA", "CN");
        countryMapper.put("WEIHNACHTSINSEL", "CX");
        countryMapper.put("KOKOSINSELN", "CC");
        countryMapper.put("KOLUMBIEN", "CO");
        countryMapper.put("UNION DER KOMOREN", "KM");
        countryMapper.put("KONGO", "CG");
        countryMapper.put("KONGO (DEM. REP.)", "CD");
        countryMapper.put("COOKINSELN", "CK");
        countryMapper.put("COSTA RICA", "CR");
        countryMapper.put("KROATIEN", "HR");
        countryMapper.put("KUBA", "CU");
        countryMapper.put("ZYPERN", "CY");
        countryMapper.put("TSCHECHISCHE REPUBLIK", "CZ");
        countryMapper.put("DÄNEMARK", "DK");
        countryMapper.put("DSCHIBUTI", "DJ");
        countryMapper.put("DOMINICA", "DM");
        countryMapper.put("DOMINIKANISCHE REPUBLIK", "DO");
        countryMapper.put("ECUADOR", "EC");
        countryMapper.put("ÄGYPTEN", "EG");
        countryMapper.put("EL SALVADOR", "SV");
        countryMapper.put("ÄQUATORIAL-GUINEA", "GQ");
        countryMapper.put("ERITREA", "ER");
        countryMapper.put("ESTLAND", "EE");
        countryMapper.put("ÄTHIOPIEN", "ET");
        countryMapper.put("FALKLANDINSELN", "FK");
        countryMapper.put("FÄRÖER-INSELN", "FO");
        countryMapper.put("FIDSCHI", "FJ");
        countryMapper.put("FINNLAND", "FI");
        countryMapper.put("FRANKREICH", "FR");
        countryMapper.put("FRANZÖSISCH GUYANA", "GF");
        countryMapper.put("FRANZÖSISCH-POLYNESIEN", "PF");
        countryMapper.put("FRANZÖSISCHE SÜD- UND ANTARKTISGEBIETE", "TF");
        countryMapper.put("GABUN", "GA");
        countryMapper.put("GAMBIA", "GM");
        countryMapper.put("GEORGIEN", "GE");
        countryMapper.put("DEUTSCHLAND", "DE");
        countryMapper.put("GHANA", "GH");
        countryMapper.put("GIBRALTAR", "GI");
        countryMapper.put("GRIECHENLAND", "GR");
        countryMapper.put("GRÖNLAND", "GL");
        countryMapper.put("GRENADA", "GD");
        countryMapper.put("GUADELOUPE", "GP");
        countryMapper.put("GUAM", "GU");
        countryMapper.put("GUATEMALA", "GT");
        countryMapper.put("GUERNSEY", "GG");
        countryMapper.put("GUINEA", "GN");
        countryMapper.put("GUINEA-BISSAU", "GW");
        countryMapper.put("GUYANA", "GY");
        countryMapper.put("HAITI", "HT");
        countryMapper.put("HEARD UND DIE MCDONALDINSELN", "HM");
        countryMapper.put("HEILIGER STUHL", "VA");
        countryMapper.put("HONDURAS", "HN");
        countryMapper.put("HONG KONG", "HK");
        countryMapper.put("UNGARN", "HU");
        countryMapper.put("ISLAND", "IS");
        countryMapper.put("INDIEN", "IN");
        countryMapper.put("INDONESIEN", "ID");
        countryMapper.put("ELFENBEINKÜSTE", "CI");
        countryMapper.put("IRAN", "IR");
        countryMapper.put("IRAK", "IQ");
        countryMapper.put("IRLAND", "IE");
        countryMapper.put("INSEL MAN", "IM");
        countryMapper.put("ISRAEL", "IL");
        countryMapper.put("ITALIEN", "IT");
        countryMapper.put("JAMAIKA", "JM");
        countryMapper.put("JAPAN", "JP");
        countryMapper.put("JERSEY", "JE");
        countryMapper.put("JORDANIEN", "JO");
        countryMapper.put("KASACHSTAN", "KZ");
        countryMapper.put("KENIA", "KE");
        countryMapper.put("KIRIBATI", "KI");
        countryMapper.put("KUWAIT", "KW");
        countryMapper.put("KIRGISISTAN", "KG");
        countryMapper.put("LAOS", "LA");
        countryMapper.put("LETTLAND", "LV");
        countryMapper.put("LIBANON", "LB");
        countryMapper.put("LESOTHO", "LS");
        countryMapper.put("LIBERIA", "LR");
        countryMapper.put("LIBYEN", "LY");
        countryMapper.put("LIECHTENSTEIN", "LI");
        countryMapper.put("LITAUEN", "LT");
        countryMapper.put("LUXEMBURG", "LU");
        countryMapper.put("MACAO", "MO");
        countryMapper.put("MAZEDONIEN", "MK");
        countryMapper.put("MADAGASKAR", "MG");
        countryMapper.put("MALAWI", "MW");
        countryMapper.put("MALAYSIA", "MY");
        countryMapper.put("MALEDIVEN", "MV");
        countryMapper.put("MALI", "ML");
        countryMapper.put("MALTA", "MT");
        countryMapper.put("MARSHALLINSELN", "MH");
        countryMapper.put("MARTINIQUE", "MQ");
        countryMapper.put("MAURETANIEN", "MR");
        countryMapper.put("MAURITIUS", "MU");
        countryMapper.put("MAYOTTE", "YT");
        countryMapper.put("MEXIKO", "MX");
        countryMapper.put("MIKRONESIEN", "FM");
        countryMapper.put("MOLDAWIE", "MD");
        countryMapper.put("MONACO", "MC");
        countryMapper.put("MONGOLEI", "MN");
        countryMapper.put("MONTENEGRO", "ME");
        countryMapper.put("MONTSERRAT", "MS");
        countryMapper.put("MAROKKO", "MA");
        countryMapper.put("MOSAMBIK", "MZ");
        countryMapper.put("MYANMAR", "MM");
        countryMapper.put("NAMIBIA", "NA");
        countryMapper.put("NAURU", "NR");
        countryMapper.put("NÉPAL", "NP");
        countryMapper.put("NIEDERLANDE", "NL");
        countryMapper.put("NEUKALEDONIEN", "NC");
        countryMapper.put("NEUSEELAND", "NZ");
        countryMapper.put("NICARAGUA", "NI");
        countryMapper.put("NIGER", "NE");
        countryMapper.put("NIGERIA", "NG");
        countryMapper.put("NIUE", "NU");
        countryMapper.put("NORFOLKINSEL", "NF");
        countryMapper.put("NORDKOREA", "KP");
        countryMapper.put("NÖRDLICHE MARIANEN", "MP");
        countryMapper.put("NORWEGEN", "NO");
        countryMapper.put("OMAN", "OM");
        countryMapper.put("PAKISTAN", "PK");
        countryMapper.put("PALAU", "PW");
        countryMapper.put("PALÄSTINA", "PS");
        countryMapper.put("PANAMA", "PA");
        countryMapper.put("PAPUA-NEUGUINEA", "PG");
        countryMapper.put("PARAGUAY", "PY");
        countryMapper.put("PERU", "PE");
        countryMapper.put("PHILIPPINEN", "PH");
        countryMapper.put("PITCAIRN", "PN");
        countryMapper.put("POLEN", "PL");
        countryMapper.put("PORTUGAL", "PT");
        countryMapper.put("PUERTO RICO", "PR");
        countryMapper.put("KATAR", "QA");
        countryMapper.put("RÉUNION", "RE");
        countryMapper.put("RUMÄNIEN", "RO");
        countryMapper.put("RUSSLAND", "RU");
        countryMapper.put("RUANDA", "RW");
        countryMapper.put("SAINT-BARTHÉLEMY", "BL");
        countryMapper.put("SANKT HELENA", "SH");
        countryMapper.put("ST. KITTS UND NEVIS", "KN");
        countryMapper.put("SAINT LUCIA", "LC");
        countryMapper.put("SAINT MARTIN", "MF");
        countryMapper.put("SAINT-PIERRE UND MIQUELON", "PM");
        countryMapper.put("SAINT VINCENT UND DIE GRENADINEN", "VC");
        countryMapper.put("SAMOA", "WS");
        countryMapper.put("SAN MARINO", "SM");
        countryMapper.put("SÃO TOMÉ UND PRÍNCIPE", "ST");
        countryMapper.put("SAUDI-ARABIEN", "SA");
        countryMapper.put("SENEGAL", "SN");
        countryMapper.put("SERBIEN", "RS");
        countryMapper.put("SEYCHELLEN", "SC");
        countryMapper.put("SIERRA LEONE", "SL");
        countryMapper.put("SINGAPUR", "SG");
        countryMapper.put("SLOWAKEI", "SK");
        countryMapper.put("SLOWENIEN", "SI");
        countryMapper.put("SALOMONEN", "SB");
        countryMapper.put("SOMALIA", "SO");
        countryMapper.put("REPUBLIK SÜDAFRIKA", "ZA");
        countryMapper.put("SÜDGEORGIEN UND DIE SÜDLICHEN SANDWICHINSELN", "GS");
        countryMapper.put("SÜDKOREA", "KR");
        countryMapper.put("SÜDSUDAN", "SS");
        countryMapper.put("SPANIEN", "ES");
        countryMapper.put("SRI LANKA", "LK");
        countryMapper.put("SUDAN", "SD");
        countryMapper.put("SURINAME", "SR");
        countryMapper.put("SVALBARD UND JAN MAYEN", "SJ");
        countryMapper.put("SWASILAND", "SZ");
        countryMapper.put("SCHWEDEN", "SE");
        countryMapper.put("SCHWEIZ", "CH");
        countryMapper.put("SYRIEN", "SY");
        countryMapper.put("TAIWAN", "TW");
        countryMapper.put("TADSCHIKISTAN", "TJ");
        countryMapper.put("TANSANIA", "TZ");
        countryMapper.put("THAILAND", "TH");
        countryMapper.put("TIMOR-LESTE", "TL");
        countryMapper.put("TOGO", "TG");
        countryMapper.put("TOKELAU", "TK");
        countryMapper.put("TONGA", "TO");
        countryMapper.put("TRINIDAD UND TOBAGO", "TT");
        countryMapper.put("TUNESIEN", "TN");
        countryMapper.put("TÜRKEI", "TR");
        countryMapper.put("TURKMENISTAN", "TM");
        countryMapper.put("TURKS- UND CAICOSINSELN", "TC");
        countryMapper.put("TUVALU", "TV");
        countryMapper.put("UGANDA", "UG");
        countryMapper.put("UKRAINE", "UA");
        countryMapper.put("VEREINIGTE ARABISCHE EMIRATE", "AE");
        countryMapper.put("VEREINIGTES KÖNIGREICH", "GB");
        countryMapper.put("VEREINIGTE STAATEN VON AMERIKA", "US");
        countryMapper.put("URUGUAY", "UY");
        countryMapper.put("USBEKISTAN", "UZ");
        countryMapper.put("VANUATU", "VU");
        countryMapper.put("VENEZUELA", "VE");
        countryMapper.put("VIETNAM", "VN");
        countryMapper.put("WALLIS UND FUTUNA", "WF");
        countryMapper.put("WESTSAHARA", "EH");
        countryMapper.put("JEMEN", "YE");
        countryMapper.put("SAMBIA", "ZM");
        countryMapper.put("SIMBABWE", "ZW");
        /* 2019-12-13: Special save.tv API versions go here */
        countryMapper.put("GROSSBRITANNIEN", "GB");
        countryMapper.put("VOLKSREPUBLIK CHINA", "CN");
        /* 2019-12-13: Sometimes they use 'AE', sometimes 'Ä' ... */
        countryMapper.put("RUMAENIEN", "RO");
        countryMapper.put("OESTERREICH", "AT");
        /*
         * Sometimes we got multiple countries, sometimes only one. Some are already given in their short variant, most in their long
         * variant!
         */
        final String[] countriesLong;
        if (produceCountriesLong.contains("")) {
            countriesLong = produceCountriesLong.split(" / ");
        } else {
            /* Single country */
            countriesLong = new String[1];
            countriesLong[0] = produceCountriesLong;
        }
        String ret = "";
        int counter = 0;
        for (final String produceCountryLong : countriesLong) {
            final String countryShort;
            if (countryMapper.containsKey(produceCountryLong)) {
                countryShort = countryMapper.get(produceCountryLong);
            } else {
                /* Fallback */
                countryShort = produceCountryLong;
            }
            ret += countryShort;
            if (counter < countriesLong.length - 1) {
                /* We are not yet at the last element --> Add 'filler' */
                ret += " / ";
            }
            counter++;
        }
        return ret;
    }

    @SuppressWarnings("deprecation")
    public static String getFilename(final Plugin plugin, final DownloadLink link) throws ParseException {
        /*
         * No custom filename if not all required tags are given, if the user prefers original filenames or if custom user regexes for
         * specified series or movies match to force original filenames
         */
        final SubConfiguration cfg = SubConfiguration.getConfig(plugin.getHost());
        final boolean force_original_general = (cfg.getBooleanProperty(PROPERTY_USEORIGINALFILENAME, defaultPROPERTY_USEORIGINALFILENAME) || link.getLongProperty(PROPERTY_category_id, 0) == 0);
        final String site_title = link.getStringProperty(PROPERTY_plainfilename);
        final String server_filename = link.getStringProperty(PROPERTY_server_filename, null);
        final String fake_original_filename = getFakeOriginalFilename(plugin, link);
        boolean force_original_series = false;
        boolean force_original_movies = false;
        String formattedFilename;
        if (isSeries(link)) {
            formattedFilename = cfg.getStringProperty(CUSTOM_FILENAME_SERIES, defaultCUSTOM_FILENAME_SERIES);
            try {
                if (site_title.matches(cfg.getStringProperty(FORCE_ORIGINALFILENAME_SERIES, defaultFORCE_ORIGINALFILENAME_SERIES))) {
                    force_original_series = true;
                }
            } catch (final Throwable e) {
                System.out.println("FORCE_ORIGINALFILENAME_SERIES custom regex failed");
            }
        } else {
            formattedFilename = cfg.getStringProperty(CUSTOM_FILENAME_MOVIES, defaultCUSTOM_FILENAME_MOVIES);
            try {
                if (site_title.matches(cfg.getStringProperty(FORCE_ORIGINALFILENAME_MOVIES, defaultFORCE_ORIGINALFILENAME_MOVIES))) {
                    force_original_movies = true;
                }
            } catch (final Throwable e) {
                System.out.println("FORCE_ORIGINALFILENAME_MOVIES custom regex failed");
            }
        }
        /* If user wants to use the original server filename in a custom filename we need to have it present here - if not, we fake it */
        final boolean force_original_original_missing = (formattedFilename.contains("*server_dateiname*") && server_filename == null);
        if (force_original_original_missing) {
            link.setProperty(PROPERTY_server_filename, fake_original_filename.substring(0, fake_original_filename.lastIndexOf(".")));
        }
        final boolean force_original_filename = (force_original_general || force_original_series || force_original_movies);
        String filename;
        if (force_original_filename) {
            filename = fake_original_filename;
        } else {
            filename = getFormattedFilename(plugin, link);
        }
        /* Cut filenames for Windows systems if necessary */
        if (CrossSystem.isWindows() && filename.length() > 255) {
            filename = filename.replace(EXTENSION_default, "");
            if (filename.length() >= 251) {
                filename = filename.substring(0, 250) + EXTENSION_default;
            } else {
                filename += EXTENSION_default;
            }
        }
        return filename;
    }

    /**
     * Get- and set the information, we later need for the custom filenames. <br />
     * Their json is crazy regarding data types thus we have a lot of type conversions here ...
     */
    @Deprecated
    public static void parseFilenameInformation_site(final DownloadLink link, final Map<String, Object> sourcemap) throws PluginException {
        /*
         * Caution with data types - if e.g. a movie is named "1987" they will actually use a double- or long value - this is totally crazy
         * as everything can happen here. Imagine a movie is named "true" ...
         */
        final Object site_title_o = sourcemap.get("STITLE");
        String site_title = null;
        if (site_title_o != null) {
            site_title = site_title_o.toString();
        }
        long datemillis = 0;
        /* For series only */
        final Object season_episode_information = sourcemap.get("SFOLGE");
        final Object episodename_o = sourcemap.get("SSUBTITLE");
        String episodename = null;
        if (episodename_o != null) {
            episodename = episodename_o.toString();
        }
        /* General */
        final String genre = (String) sourcemap.get("SCHAR");
        final String producecountry = (String) sourcemap.get("SCOUNTRY");
        final Object produceyear_o = sourcemap.get("SPRODUCTIONYEAR");
        String produceyear = null;
        if (produceyear_o instanceof Double) {
            /* Yes - they acrtually return a YEAR as double value */
            produceyear = Integer.toString((int) ((Double) produceyear_o).doubleValue());
        } else if (produceyear_o instanceof Number) {
            /* In case they correct their horrible json we might as well get a long value --> Handle this too :) */
            produceyear = Long.toString(JavaScriptEngineFactory.toLong(produceyear_o, 0));
        } else if (produceyear_o instanceof String) {
            produceyear = (String) produceyear_o;
        }
        final int category = (int) JavaScriptEngineFactory.toLong(sourcemap.get("TVCATEGORYID"), -1);
        final String runtime_start = (String) sourcemap.get("DSTARTDATE");
        /* For hosterplugin */
        String runtime_end = (String) sourcemap.get("ENDDATE");
        /* For decrypterplugin */
        if (runtime_end == null) {
            runtime_end = (String) sourcemap.get("DENDDATE");
        }
        final long runtime_end_long = TimeFormatter.getMilliSeconds(runtime_end, "yyyy-MM-dd HH:mm:ss", Locale.GERMAN);
        datemillis = TimeFormatter.getMilliSeconds(runtime_start, "yyyy-MM-dd HH:mm:ss", Locale.GERMAN);
        final int site_runtime_seconds = (int) ((runtime_end_long - datemillis) / 1000);
        final boolean hasMoved = (site_title != null && site_title.matches(".*?kurzfristige Programmänderung.*?")) ? true : false;
        final String tv_station = (String) sourcemap.get("STVSTATIONNAME");
        /* Set properties which are needed for filenames */
        /* Add series information */
        setFilenameInformationSeasonnumberEpisodenumberUniversal(link, season_episode_information);
        /* Sometimes episodetitle == episodenumber (double) --> Do NOT set it as episodetitle is NOT given in this case! */
        if (episodename != null && !episodename.matches("\\d+\\.\\d+")) {
            link.setProperty(PROPERTY_episodename, correctData(link.getHost(), episodename));
        }
        /* Add other information */
        if (!produceyear.equals("0")) {
            link.setProperty(PROPERTY_produceyear, produceyear);
        }
        if (genre != null) {
            link.setProperty(PROPERTY_genre, correctData(link.getHost(), genre));
        }
        if (producecountry != null) {
            link.setProperty(PROPERTY_producecountry, correctData(link.getHost(), producecountry));
            link.setProperty(PROPERTY_producecountry_short, getProduceCountryShort(producecountry));
        }
        if (tv_station != null) {
            link.setProperty(PROPERTY_plain_tv_station, correctData(link.getHost(), tv_station));
        }
        parseCategoryID(link, category, episodename);
        if (site_title != null) {
            link.setProperty(PROPERTY_plainfilename, correctData(link.getHost(), site_title));
        }
        link.setProperty(PROPERTY_originaldate, datemillis);
        link.setProperty(PROPERTY_originaldate_end, runtime_end_long);
        link.setProperty(PROPERTY_site_runtime_seconds_withads, site_runtime_seconds);
        link.setProperty(PROPERTY_has_moved, hasMoved);
    }

    /**
     * Get- and set the information we later need for the custom filenames. <br />
     *
     * @param link
     *            : Given DownloadLink
     * @param entries
     *            : Given json --> Java Map
     * @param hasToGrabTelecastMap
     *            : For downloadable telecastIDs we're not yet at the telecastMap which is why we have to grab it (this is set to true)!
     */
    @SuppressWarnings("unchecked")
    public static void parseFilenameInformation_api(final DownloadLink link, Map<String, Object> entries, final boolean hasToGrabTelecastMap) throws PluginException {
        if (entries == null) {
            return;
        }
        /* First grab the only property (in this function) which is only available for finished 'records' which are downloadable. */
        final short isAdsFreeAvailable = jsonGetAdsFreeAvailableAPIDetailed(entries);
        final int runtime_seconds_adsfree = (int) JavaScriptEngineFactory.toLong(entries.get("adFreeLength"), -1);
        /*
         * 2025: Trial usage of the API's "defect" info to detect incompletely encoded recordings before we even try to download them.
         * "availableLength" < "expectedLength" means the encoding process did not (yet?) produce the full expected video length.
         */
        final Object defectAvailableLengthO = JavaScriptEngineFactory.walkJson(entries, "defect/encoding/telecast/availableLength");
        final Object defectExpectedLengthO = JavaScriptEngineFactory.walkJson(entries, "defect/encoding/telecast/expectedLength");
        if (defectAvailableLengthO != null && defectExpectedLengthO != null) {
            final long defectAvailableLength = JavaScriptEngineFactory.toLong(defectAvailableLengthO, 0);
            final long defectExpectedLength = JavaScriptEngineFactory.toLong(defectExpectedLengthO, 0);
            if (defectExpectedLength > 0 && defectAvailableLength < defectExpectedLength) {
                System.out.println("Recording seems to be incompletely encoded: available=" + defectAvailableLength + "s expected=" + defectExpectedLength + "s | telecastID=" + getTelecastId(link));
            }
        }
        if (hasToGrabTelecastMap) {
            entries = (Map<String, Object>) entries.get("telecast");
            if (entries == null) {
                return;
            }
        }
        // ---------------------------------------------
        final String runtime_start = ((String) entries.get("startDate")).replace("Z", "+0000");
        final String runtime_end = ((String) entries.get("endDate")).replace("Z", "+0000");
        final long runtime_end_long = TimeFormatter.getMilliSeconds(runtime_end, "yyyy-MM-dd'T'HH:mm:ssZ", Locale.GERMANY);
        final long runtime_start_long = TimeFormatter.getMilliSeconds(runtime_start, "yyyy-MM-dd'T'HH:mm:ssZ", Locale.GERMANY);
        final int site_runtime_seconds_withads = (int) ((runtime_end_long - runtime_start_long) / 1000);
        // ---------------------------------------------
        final Object hasMovedO = entries.get("hasMoved");
        final boolean hasMoved = hasMovedO != null ? ((Boolean) hasMovedO).booleanValue() : false;
        final String site_title = (String) entries.get("title");
        /* For series only */
        final Object season_episode_information = entries.get("episode");
        final String episodename = (String) entries.get("subTitle");
        /* General */
        final String genre = (String) JavaScriptEngineFactory.walkJson(entries, "tvSubCategory/name");
        final String producecountry = (String) entries.get("country");
        final String produceyear = Long.toString(JavaScriptEngineFactory.toLong(entries.get("year"), 0));
        final int category = (int) JavaScriptEngineFactory.toLong(JavaScriptEngineFactory.walkJson(entries, "tvCategory/id"), -1);
        final String tv_station = (String) JavaScriptEngineFactory.walkJson(entries, "tvStation/name");
        /* Set properties which are needed for filenames */
        /* Add series information */
        setFilenameInformationSeasonnumberEpisodenumberUniversal(link, season_episode_information);
        if (episodename != null) {
            link.setProperty(PROPERTY_episodename, correctData(link.getHost(), episodename));
        }
        if (!produceyear.equals("0")) {
            link.setProperty(PROPERTY_produceyear, correctData(link.getHost(), produceyear));
        }
        /* Add other information */
        if (genre != null) {
            link.setProperty(PROPERTY_genre, correctData(link.getHost(), genre));
        }
        if (producecountry != null) {
            link.setProperty(PROPERTY_producecountry, correctData(link.getHost(), producecountry));
            link.setProperty(PROPERTY_producecountry_short, getProduceCountryShort(producecountry));
        }
        if (tv_station != null) {
            link.setProperty(PROPERTY_plain_tv_station, correctData(link.getHost(), tv_station));
        }
        parseCategoryID(link, category, episodename);
        if (site_title != null) {
            /* This should actually never be null */
            link.setProperty(PROPERTY_plainfilename, correctData(link.getHost(), site_title));
        }
        /*
         * Set ad-free state on DownloadLink for e.g. usage in filename later.
         */
        if (isAdsFreeAvailable == 1) {
            link.setProperty(PROPERTY_ad_free, STATE_ad_free_true);
        } else if (isAdsFreeAvailable == 0) {
            link.setProperty(PROPERTY_ad_free, STATE_ad_free_false);
        } else {
            // Status unknown - do not set status at all
        }
        link.setProperty(PROPERTY_originaldate, runtime_start_long);
        link.setProperty(PROPERTY_originaldate_end, runtime_end_long);
        link.setProperty(PROPERTY_site_runtime_seconds_withads, site_runtime_seconds_withads);
        if (runtime_seconds_adsfree > -1) {
            link.setProperty(PROPERTY_site_runtime_seconds_adsfree, runtime_seconds_adsfree);
        }
        link.setProperty(PROPERTY_has_moved, hasMoved);
    }

    public static void setFilenameInformationSeasonnumberEpisodenumberUniversal(final DownloadLink link, final Object episodeO) {
        if (episodeO == null) {
            return;
        }
        if (episodeO instanceof Double) {
            /* Website may sometimes return episodenumber only ... as double! */
            link.setProperty(PROPERTY_episodenumber, (int) ((Double) episodeO).doubleValue());
        } else if (episodeO instanceof String) {
            final String episodeInformation = episodeO instanceof String ? (String) episodeO : null;
            String episodenumber = null;
            String seasonnumber = null;
            if (episodeInformation != null && (new Regex(episodeInformation, "S\\d+").matches() || new Regex(episodeInformation, "E\\d+").matches())) {
                episodenumber = new Regex(episodeInformation, "E(\\d+)").getMatch(0);
                seasonnumber = new Regex(episodeInformation, "S(\\d+)").getMatch(0);
            } else if (episodeInformation != null && episodeInformation.matches("\\d+")) {
                episodenumber = episodeInformation;
            }
            if (seasonnumber != null) {
                link.setProperty(PROPERTY_seasonnumber, Integer.parseInt(seasonnumber));
            }
            if (episodenumber != null) {
                link.setProperty(PROPERTY_episodenumber, Integer.parseInt(episodenumber));
            }
        } else {
            /* Set nothing */
            return;
        }
    }

    public static void parseCategoryID(final DownloadLink link, int category, final String episodename) {
        /* Happens in decrypter - errorhandling! */
        final int episodenumber = getEpisodeNumber(link);
        if (category == -1 && (episodename != null || episodenumber > -1)) {
            /* Force id for series if we are sure that our object belongs to that category! */
            link.setProperty(PROPERTY_category_id, 2);
            return;
        }
        link.setProperty(PROPERTY_category_id, category);
    }

    /** Sets available quality as PROPERTY_quality and sets filesize. */
    @Deprecated
    public static void parseQualityTagWebsite(final DownloadLink link, final List<Object> sourcelist) {
        final int selected_video_format = getConfiguredVideoFormatID(link);
        /*
         * If we have no source, we can select HQ if the user chose HQ because it is always available. If the user selects any other quality
         * we need to know whether it exists or not and then set the data.
         */
        final String finalQualityStr;
        if (sourcelist == null) {
            /* No qualities given from website/API */
            finalQualityStr = STATE_QUALITY_HQ;
        } else {
            final int quality_best = jsonGetBestQualityIdWebsite(sourcelist);
            final boolean isHDAvailable = quality_best == getBestFormatID();
            switch (selected_video_format) {
            case SITE_FORMAT_HD:
                if (isHDAvailable) {
                    finalQualityStr = STATE_QUALITY_HD;
                } else {
                    finalQualityStr = STATE_QUALITY_HQ;
                }
                break;
            case SITE_FORMAT_HQ:
                finalQualityStr = STATE_QUALITY_HQ;
                break;
            case SITE_FORMAT_LQ:
                if (sourcelist.size() == 2) {
                    /* Mobile version available (should always be the case!) */
                    finalQualityStr = STATE_QUALITY_LQ;
                } else {
                    finalQualityStr = STATE_QUALITY_HQ;
                }
                break;
            default:
                finalQualityStr = STATE_QUALITY_HQ;
            }
        }
        link.setProperty(PROPERTY_quality, finalQualityStr);
        link.setDownloadSize(calculateFilesize(link, finalQualityStr));
    }

    /**
     * Sets available quality as PROPERTY_quality and sets filesize. <br />
     * Has fallback for all possible errorcases!
     */
    public static void parseQualityTagAPI(final DownloadLink link, final List<Object> sourcelist) {
        final int selected_video_format = getConfiguredVideoFormatID(link);
        /*
         * If we have no source, we can select HQ if the user chose HQ because it is always available. If the user selects any other quality
         * we need to know whether it exists or not and then set the data.
         */
        final String finalQualityStr;
        if (sourcelist == null) {
            /* No qualities given from website/API */
            finalQualityStr = STATE_QUALITY_HQ;
        } else {
            // final String quality_best = jsonGetBestQualityIdAPI(sourcelist);
            final boolean isHDAvailable = sourcelist.size() == 3;
            switch (selected_video_format) {
            case SITE_FORMAT_HD:
                if (isHDAvailable) {
                    finalQualityStr = STATE_QUALITY_HD;
                } else {
                    finalQualityStr = STATE_QUALITY_HQ;
                }
                break;
            case SITE_FORMAT_HQ:
                finalQualityStr = STATE_QUALITY_HQ;
                break;
            case SITE_FORMAT_LQ:
                if (sourcelist.size() == 2) {
                    /* Mobile version available (should always be the case!) */
                    finalQualityStr = STATE_QUALITY_LQ;
                } else {
                    finalQualityStr = STATE_QUALITY_HQ;
                }
                break;
            default:
                finalQualityStr = STATE_QUALITY_HQ;
            }
        }
        link.setProperty(PROPERTY_quality, finalQualityStr);
        /* Set download file size based on given format information. */
        try {
            final int finalFormat = convertQualityStringToInternalID(finalQualityStr);
            final boolean user_prefers_adsfree = getPreferAdsFree(link);
            for (final Object formatO : sourcelist) {
                final Map<String, Object> entries = (Map<String, Object>) formatO;
                final Map<String, Object> recordFormat = (Map<String, Object>) entries.get("recordFormat");
                final int thisFormatValue = ((Number) recordFormat.get("id")).intValue();
                if (thisFormatValue != finalFormat) {
                    /* Skip unwanted formats */
                    continue;
                }
                /**
                 * 2021-02-11: Both serverside given filesizes are very vague. For downloads with ads we use the serverside given
                 * information. </br>
                 * For ad-free downloads we'll calculate it on our own as that is more precise! </br>
                 * duration_seconds_adsfree == 0 if adFree is unavailable while cutVideoSize == uncutVideoSize (which is of course not true,
                 * that's just what their backend does with that here).
                 */
                final long filesizeAdFree = ((Number) entries.get("cutVideoSize")).longValue();
                final long filesizeWithAds = ((Number) entries.get("uncutVideoSize")).longValue();
                final int duration_seconds_adsfree = link.getIntegerProperty(PROPERTY_site_runtime_seconds_adsfree, 0);
                if (user_prefers_adsfree && filesizeAdFree > 0 && duration_seconds_adsfree > 0) {
                    final double mb_per_second = getBitrateForFormat(finalFormat);
                    final double calculated_filesize = mb_per_second * duration_seconds_adsfree * 1024 * 1024;
                    link.setDownloadSize((long) calculated_filesize);
                } else {
                    link.setDownloadSize(filesizeWithAds * 1024 * 1024);
                }
                break;
            }
        } catch (final Exception e) {
            /* This should never happen */
            System.out.print("WTF");
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        synchronized (LOCK) {
            checkFeatureDialogAll();
        }
        requestFileInformation(link, account);
        /* Check whether content has been recorded already or not! */
        final long runtime_end = link.getLongProperty(PROPERTY_originaldate_end, System.currentTimeMillis() + 1);
        final long released_since = System.currentTimeMillis() - runtime_end;
        if (released_since < 0) {
            /*
             * Content not yet recorded --> Show errormessage with waittime. Waittime = Releasedate(END-Timestamp of video) of content + 10
             * minutes! Most times Stv needs between 10 and 60 minutes to get the downloads ready.
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Diese Sendung wurde noch nicht aufgenommen!", released_since * (-1) + 10 * 60 * 60 * 1000l);
        }
        if (this.apiActive()) {
            handlePremiumAPI(link, account);
        } else {
            handlePremiumWebsite(link, account);
        }
    }

    @SuppressWarnings({ "unchecked" })
    @Deprecated
    public void handlePremiumWebsite(final DownloadLink link, final Account account) throws Exception {
        boolean preferAdsFree = getPreferAdsFree(link);
        /* Check if ads-free version is available */
        final String ad_Free_availability = PluginJSonUtils.getJsonValue(br, "BADFREEAVAILABLE");
        final boolean isAdsFreeAvailable;
        if (!StringUtils.isEmpty(ad_Free_availability) && (ad_Free_availability.equals("1") || ad_Free_availability.equalsIgnoreCase("true"))) {
            /* Set ad-free state on DownloadLink for e.g. usage in filename later. */
            link.setProperty(PROPERTY_ad_free, STATE_ad_free_true);
            isAdsFreeAvailable = true;
        } else {
            /* ad_Free_availability == "2" */
            /* Set ad-free state on DownloadLink for e.g. usage in filename later. */
            link.setProperty(PROPERTY_ad_free, STATE_ad_free_false);
            isAdsFreeAvailable = false;
        }
        final boolean downloadAdsFreeValue = verifyAdsFreeUserSelection(link, preferAdsFree, isAdsFreeAvailable);
        /* Set download options (ads-free or with ads) and get download url */
        Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
        final List<Object> sourcelist = jsonGetVideoSourcelist(entries);
        final int best_quality_id = jsonGetBestQualityIdWebsite(sourcelist);
        int stv_request_selected_format_id_value = getConfiguredVideoFormatID(link);
        final boolean desired_format_is_available = jsonIsDesiredFormatAvailableWebsite(sourcelist, stv_request_selected_format_id_value);
        if (!desired_format_is_available) {
            logger.info("Desired format is not available - falling back to highest format/quality possible");
            stv_request_selected_format_id_value = best_quality_id;
        }
        String dllink = checkDirectLink(link, stv_request_selected_format_id_value, downloadAdsFreeValue);
        if (StringUtils.isEmpty(dllink)) {
            requestDownloadWebsite(link, stv_request_selected_format_id_value, downloadAdsFreeValue);
            entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
            /* 2016-07-06: Collecting errors: */
            /*
             * {"ERROR":
             * "Durch Anwendung der Schnittliste wurde der gesamte Inhalt der Aufnahme entfernt. Um den Inhalt ihrer Aufnahme zu betrachten, laden Sie bitte die ungeschnittene Version."
             * ,"TELECASTID":1.2579225E7}
             */
            final String error = (String) entries.get("ERROR");
            if (!StringUtils.isEmpty(error)) {
                /* 2016-07-06: As long as we haven't collected *all* possible errors let's just have generic handling for all. */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Fehler beim Downloadversuch: " + error, 10 * 60 * 1000l);
            }
            final boolean dlurl_success = ((Boolean) entries.get("SUCCESS")).booleanValue();
            dllink = (String) entries.get("DOWNLOADURL");
            if (StringUtils.isEmpty(dllink) && !dlurl_success) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Download aus unbekannten Gründen zurzeit nicht möglich", 10 * 60 * 1000l);
            }
        }
        /*
         * Remember the format that is actually being downloaded so that a resumed filestream always continues with the exact same format,
         * even if the user changes his settings in the meantime.
         */
        link.setProperty(PROPERTY_download_format_id, stv_request_selected_format_id_value);
        link.setProperty(PROPERTY_download_ads_free, downloadAdsFreeValue);
        handleDownload(link, account, dllink);
    }

    @SuppressWarnings("unchecked")
    public void handlePremiumAPI(final DownloadLink link, final Account account) throws Exception {
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final boolean preferAdsFree = getPreferAdsFree(link);
        int formatIDselected = getConfiguredVideoFormatID(link);
        int formatIDFallback = getDefaultFormatID();
        int formatIDtemp;
        boolean selectedFormatIsAvailable = false;
        final boolean isAdsFreeAvailable = jsonGetAdsFreeAvailableAPI(entries);
        final boolean downloadAdsFreeValue = verifyAdsFreeUserSelection(link, preferAdsFree, isAdsFreeAvailable);
        List<Object> qualityList = jsonGetFormatArrayAPI(entries);
        /* Now let's find the best quality AND find out whether the user-selected format is available or not. */
        for (final Object qualityo : qualityList) {
            entries = (Map<String, Object>) qualityo;
            entries = (Map<String, Object>) entries.get("recordFormat");
            formatIDtemp = jsonGetFormatArrayGetIDAPI(entries);
            if (formatIDtemp > formatIDFallback) {
                formatIDFallback = formatIDtemp;
            }
            if (formatIDtemp == formatIDselected) {
                selectedFormatIsAvailable = true;
            }
        }
        if (selectedFormatIsAvailable) {
            logger.info("Selected format is available --> Downloading that: " + formatIDselected);
        } else {
            logger.info("Selected format is not available --> Downloading highest format available instead: " + formatIDFallback);
            formatIDselected = formatIDFallback;
        }
        String dllink = checkDirectLink(link, formatIDselected, downloadAdsFreeValue);
        if (StringUtils.isEmpty(dllink)) {
            accessDownloadPageAPI(link, formatIDselected, downloadAdsFreeValue);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            dllink = (String) entries.get("downloadUrl");
            String server_filename = entries.get("fileName").toString();
            server_filename = fixCharIssues(server_filename);
            server_filename = server_filename.substring(0, server_filename.lastIndexOf("."));
            link.setProperty(PROPERTY_server_filename, server_filename);
        }
        /*
         * Remember the format that is actually being downloaded so that a resumed filestream always continues with the exact same format,
         * even if the user changes his settings in the meantime.
         */
        link.setProperty(PROPERTY_download_format_id, formatIDselected);
        link.setProperty(PROPERTY_download_ads_free, downloadAdsFreeValue);
        handleDownload(link, account, dllink);
    }

    /**
     * Check whether the users' ads-free selection is available. <br />
     * If not, act according to the users' settings and either wait and retry or download the version with ads instead. <br />
     *
     * @return: Basically this can change preferAdsFree from true to false if ads-free version is now available and this is allowed by the
     *          user. <br />
     *          Can be used an API- and website mode.
     */
    private boolean verifyAdsFreeUserSelection(final DownloadLink link, boolean preferAdsFree, boolean isAdsFreeAvailable) throws PluginException {
        final int adsFreeLength = link.getIntegerProperty(PROPERTY_site_runtime_seconds_adsfree, -1);
        if (isAdsFreeAvailable && adsFreeLength == 0) {
            /* 2017-10-02: Try to prevent errors because of "DOWNLOADSESSIONVIDEOFILESSERVICE_NOCONTENT" at this stage already. */
            logger.info("adsFreeLength is ZERO but according to API, adFree version is available --> changing isAdsFreeAvailable to false");
            isAdsFreeAvailable = false;
        }
        if (preferAdsFree && !isAdsFreeAvailable) {
            /*
             * User wants ads-free but it's not available -> Wait X [User-Defined DOWNLOADONLYADSFREE_RETRY_HOURS] hours, status can still
             * change but probably won't -> If defined by user, force version with ads after a user defined amount of retries.
             */
            logger.info("Ad-free version is unavailable");
            final SubConfiguration cfg = SubConfiguration.getConfig(getHost());
            final long userDefinedWaitHours = cfg.getLongProperty(ADS_FREE_UNAVAILABLE_HOURS, SaveTv.defaultADS_FREE_UNAVAILABLE_HOURS);
            final long timestamp_releasedate = link.getLongProperty(PROPERTY_originaldate_end, 0);
            final long timestamp_with_ads_allowed = getTimestampWhenDownloadWithAdsIsAllowed(link);
            final boolean allow_load_with_ads = userDefinedWaitHours > 0 && System.currentTimeMillis() > timestamp_with_ads_allowed;
            if (allow_load_with_ads) {
                logger.info("Ad-free version is unavailable AND download with-ads is allowed by user defined timeout --> Downloading version with ads instead");
                preferAdsFree = false;
            } else if (userDefinedWaitHours == 0) {
                logger.info("Ad-free version is unavailable AND download with ads is prohibited by user timeout(default==never_allow_ads) --> Waiting");
                errorAdsFreeUnavailable(60 * 60 * 1000l, null);
            } else {
                logger.info("Ad-free version is unavailable AND download with ads is prohibited by user timeout(" + userDefinedWaitHours + ") --> Waiting");
                final String adsfree_unavailable_message;
                final long waitMillis;
                final long waitMillisDefault = 60 * 60 * 1000l;
                if (timestamp_releasedate > 0) {
                    final long millis_time_remaining_until_download_with_ads_allowed = timestamp_with_ads_allowed - System.currentTimeMillis();
                    adsfree_unavailable_message = getErrormessageDownloadWithadsInTimeX(millis_time_remaining_until_download_with_ads_allowed);
                    if (millis_time_remaining_until_download_with_ads_allowed < waitMillisDefault) {
                        /* Small waittime exact waittime */
                        waitMillis = millis_time_remaining_until_download_with_ads_allowed;
                    } else {
                        /* Long waittime - wait default wait */
                        waitMillis = waitMillisDefault;
                    }
                } else {
                    adsfree_unavailable_message = "Werbefreie Version noch nicht verfügbar";
                    waitMillis = waitMillisDefault;
                }
                errorAdsFreeUnavailable(waitMillis, adsfree_unavailable_message);
            }
        } else if (preferAdsFree) {
            /* User selection (with- or without ads) is available! */
            logger.info("Ad-free version is available AND preferred by the user AND will be downloaded");
        } else {
            logger.info("Version with ads is available AND preferred by the user AND will be downloaded");
        }
        return preferAdsFree;
    }

    /** Handles download for API- and website mode */
    private void handleDownload(final DownloadLink link, final Account account, final String dllink) throws Exception {
        final SubConfiguration cfg = SubConfiguration.getConfig(getHost());
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen! */
            logger.warning("Final downloadlink is null");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        logger.info("Final downloadlink = " + dllink + " starting download...");
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, ACCOUNT_PREMIUM_RESUME, ACCOUNT_PREMIUM_MAXCHUNKS);
        final boolean looksLikeDownloadableContent = this.looksLikeDownloadableContent(dl.getConnection());
        if (!looksLikeDownloadableContent && dl.getConnection().getResponseCode() == 404) {
            /* Guard: server told us straight up that this is gone. */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Serverfehler 404", 15 * 60 * 1000l);
        }
        if (!looksLikeDownloadableContent) {
            /* Guard: server sent HTML/an error page instead of the actual file --> Handle (known) errors. */
            logger.warning("Received HTML code instead of the file!");
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unbekannter Serverfehler 1 - bitte dem JDownloader Support mit Log melden!", 60 * 60 * 1000l);
        }
        if (dl.getConnection().getCompleteContentLength() <= 1048576l) {
            /* Guard: avoid downloading (too small) trash data. */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Serverfehler: Datei vom Server zu klein: " + SIZEUNIT.formatValue((SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue(), dl.getConnection().getCompleteContentLength()), 60 * 60 * 1000l);
        }
        /* This is for checking server speed. */
        final String previouscomment = link.getComment();
        if (previouscomment == null || previouscomment.contains("Aktuell/Zuletzt verwendeter direkter Downloadlink:")) {
            link.setComment("Aktuell/Zuletzt verwendeter direkter Downloadlink: " + dllink);
        }
        if (!link.hasProperty(PROPERTY_server_filename)) {
            String server_filename = getFileNameFromConnection(dl.getConnection());
            server_filename = fixCharIssues(server_filename);
            server_filename = server_filename.substring(0, server_filename.lastIndexOf("."));
            link.setProperty(PROPERTY_server_filename, server_filename);
        }
        final String final_filename = getFilename(this, link);
        link.setFinalFileName(final_filename);
        try {
            if (!this.dl.startDownload()) {
                /* Download did not finish (stopped/cancelled by user or connection loss) --> Nothing more to do here. */
                return;
            }
            /* Download finished successfully --> Handle optional telecastID cleanup. */
            if (cfg.getBooleanProperty(DELETE_TELECAST_ID_AFTER_DOWNLOAD, defaultDELETE_TELECAST_ID_AFTER_DOWNLOAD)) {
                logger.info("Download finished --> User WANTS telecastID " + getTelecastId(link) + " deleted");
                killTelecastID(link);
            }
            markTelecastIdAsDownloaded(link);
        } catch (final PluginException e) {
            if (e.getLinkStatus() == LinkStatus.ERROR_ALREADYEXISTS && cfg.getBooleanProperty(DELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS, defaultDELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS)) {
                /*
                 * Best-effort cleanup only - if it fails for whatever reason we must still rethrow the original ERROR_ALREADYEXISTS below
                 * instead of a new/different exception.
                 */
                try {
                    logger.info("ERROR_ALREADYEXISTS --> User WANTS telecastID " + getTelecastId(link) + " deleted");
                    killTelecastID(link);
                } catch (final Throwable ignore) {
                    /* Do not fail here, throw Exception which happened previously */
                }
            }
            throw e;
        }
    }

    /**
     * Check if a stored directlink exists under property and if so, check if it is still valid (leads to a downloadable content [NOT
     * html]). <br />
     * This will only returns something if the user hasn't changed the settings from what he had before to make sure we won't download
     * invalid videofiles.
     */
    private String checkDirectLink(final DownloadLink link, final int formatID, final boolean adsFree) {
        final String property = getDirectlinkProperty(link, formatID, adsFree);
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            logger.info("Found saved downloadurl --> Checking if it is still valid");
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    return dllink;
                }
            } catch (final Exception e) {
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        } else {
            logger.info("Failed to find saved downloadurl");
        }
        return null;
    }

    /**
     * Returns property to find final downloadurl based on users' settings. <br />
     * This way we make sure that we grab the correct saved downloadurl.
     */
    private String getDirectlinkProperty(final DownloadLink link, final int formatID, final boolean adsFree) {
        final boolean hasMoved = link.getBooleanProperty(PROPERTY_has_moved, false);
        return "directurl_" + formatID + "_" + Boolean.toString(adsFree) + "_" + Boolean.toString(hasMoved);
    }

    /**
     * Returns whether user wants to download the ads-free version or the version with ads. <br />
     * Parameters inside the user-added URL can override his basic plugin settings. This function respects that.
     */
    public static boolean getPreferAdsFree(final DownloadLink link) {
        final Boolean preferAdsFreeStored = link.getBooleanProperty(PROPERTY_download_ads_free);
        if (preferAdsFreeStored != null) {
            /*
             * A download using this ads-free setting has already been started before --> Keep using it so a resumed filestream never
             * continues with a different setting than it was started with, even if the user changes his settings in the meantime.
             */
            return preferAdsFreeStored.booleanValue();
        }
        final String preferAdsFreeUrl = new Regex(link.getDownloadURL(), "adsfree=(true|false)").getMatch(0);
        final boolean preferAdsFree;
        if (preferAdsFreeUrl != null) {
            /* Parameters in urls can override plugin settings! */
            preferAdsFree = Boolean.parseBoolean(preferAdsFreeUrl);
        } else {
            preferAdsFree = getPreferAdsFreePluginConfig();
        }
        return preferAdsFree;
    }

    /**
     * Returns whether user wants to download the ads-free version or the version with ads. <br />
     * Only based on users' config!!
     */
    public static boolean getPreferAdsFreePluginConfig() {
        final SubConfiguration cfg = SubConfiguration.getConfig(HOST_STATIC);
        final boolean preferAdsFreeConfig = cfg.getBooleanProperty(PROPERTY_PREFERADSFREE, defaultPROPERTY_PREFERADSFREE);
        return preferAdsFreeConfig;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai;
        setConstants(account, null);
        login(this.br, account, true);
        if (is_API_enabled(account.getHoster())) {
            ai = fetchAccountInfoAPI(account);
        } else {
            ai = fetchAccountInfoWebsite(account);
        }
        ai.setUnlimitedTraffic();
        return ai;
    }

    /**
     * @throws Exception
     */
    private AccountInfo fetchAccountInfoAPI(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        api_GET(br, "/user?fields=contract.isrunning%2C%20contract.packagename");
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> contract = (Map<String, Object>) entries.get("contract");
        String package_name = (String) contract.get("packageName");
        if (StringUtils.isEmpty(package_name)) {
            package_name = ACCOUNTTYPE_UNKNOWN;
        }
        if (Boolean.TRUE.equals(contract.get("isRunning"))) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        ai.setStatus(package_name);
        account.setProperty(PROPERTY_acc_type, package_name);
        return ai;
    }

    @Deprecated
    private AccountInfo fetchAccountInfoWebsite(final Account account) {
        final AccountInfo ai = new AccountInfo();
        /* Do not fail here */
        String package_name = null;
        try {
            /*
             * Get long lasting login cookie. Keep in mind that such a cookie can only exist once for every account so in case a user uses
             * multiple JDs it might happen that they "steal" themselves this cookie but it should still work fine for up to 3 JDownloader
             * instances.
             */
            String long_cookie = br.getCookie("http://" + this.getHost(), "SLOCO");
            if (long_cookie == null || long_cookie.trim().equals("bAutoLoginActive=1")) {
                logger.info("Long session cookie does not exist yet/anymore - enabling it");
                br.postPage("https://www." + this.getHost() + "/STV/M/obj/user/submit/submitAutoLogin.cfm", "IsAutoLogin=true&Messages=");
                long_cookie = br.getCookie(br.getHost(), "SLOCO");
                if (long_cookie == null || long_cookie.trim().equals("")) {
                    logger.info("Failed to get long session cookie");
                } else {
                    logger.info("Successfully received long session cookie and saved cookies");
                    account.saveCookies(this.br.getCookies(account.getHoster()), "");
                }
            } else {
                logger.info("Long session cookie exists");
            }
            /* Find account details */
            String price = null;
            br.getPage("https://www." + this.getHost() + "/STV/M/obj/user/JSON/userConfigApi.cfm?iFunction=2");
            final String acc_username = br.getRegex("\"SUSERNAME\":(\\d+)").getMatch(0);
            final String user_packet_id = PluginJSonUtils.getJsonValue(br, "CURRENTARTICLEID");
            /* Find the price of the package which the user uses. */
            final String all_packages_string = br.getRegex("\"ARRRENEWARTICLES\":\\[(.*?)\\]").getMatch(0);
            final String[] all_packets = all_packages_string.split("\\},\\{");
            for (final String packet : all_packets) {
                if (packet.contains("\"ID\":" + user_packet_id + ".0")) {
                    price = PluginJSonUtils.getJsonValue(packet, "IPRICE");
                    if (price != null) {
                        break;
                    }
                }
            }
            final String expireDate_str = PluginJSonUtils.getJsonValue(br, "DCURRENTARTICLEENDDATE");
            final long expireDate_real = TimeFormatter.getMilliSeconds(expireDate_str, "yyyy-MM-dd HH:mm:ss", Locale.GERMAN);
            long expireDate_user_display = expireDate_real;
            final long timeleft = System.currentTimeMillis() - expireDate_real;
            if ((timeleft > 0 && timeleft < 24 * 60 * 60 * 1000l) || (timeleft < 0 && timeleft > -2 * 60 * 60 * 1000l)) {
                /*
                 * Account expired less then 24 hours ago or is only valid for 2 hours or less --> Add 24 hours to it so in case the user
                 * has a subscription JD does not deactivate the account because save.tv needs some time to show the new expire date.
                 */
                expireDate_user_display += 24 * 60 * 60 * 1000;
            }
            ai.setValidUntil(expireDate_user_display);
            account.setProperty(PROPERTY_acc_expire, expireDate_real);
            package_name = PluginJSonUtils.getJsonValue(br, "SCURRENTARTICLENAME");
            if (StringUtils.isEmpty(package_name)) {
                /* This should never happen */
                package_name = ACCOUNTTYPE_UNKNOWN;
            }
            final String runtime = new Regex(package_name, "(\\d+ Monate)").getMatch(0);
            account.setProperty(PROPERTY_acc_package, correctData(account.getHoster(), package_name));
            if (price != null) {
                account.setProperty(PROPERTY_acc_price, correctData(account.getHoster(), price));
            }
            if (runtime != null) {
                account.setProperty(PROPERTY_acc_runtime, correctData(account.getHoster(), runtime));
            }
            if (acc_username != null) {
                account.setProperty(PROPERTY_acc_username, correctData(account.getHoster(), acc_username));
            }
            br.getPage("https://www." + this.getHost() + "/STV/M/obj/archive/JSON/VideoArchiveApi.cfm?iEntriesPerPage=1");
            final String totalLinks = PluginJSonUtils.getJsonValue(br, "ITOTALENTRIES");
            if (totalLinks != null) {
                account.setProperty(PROPERTY_acc_count_telecast_ids, totalLinks);
            }
            account.setType(AccountType.PREMIUM);
        } catch (final Throwable e) {
            /* Should not happen but a failure of the account detail crawler won't hurt - we logged in fine! */
            logger.info("Extended account check via website failed");
        }
        ai.setStatus(package_name);
        account.setProperty(PROPERTY_acc_type, package_name);
        return ai;
    }

    /** Performs login respecting api setting */
    private void login(final Browser br, final Account account, final boolean force) throws Exception {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        if (is_API_enabled(account.getHoster())) {
            login_api(br, account, force);
        } else {
            login_website(br, account, force);
        }
    }

    @Deprecated
    public static void login_website(final Browser br, final Account account, final boolean force) throws Exception {
        final String lang = System.getProperty("user.language");
        site_prepBrowser(br);
        synchronized (account) {
            try {
                br.setCookiesExclusive(true);
                final Cookies cookies = account.loadCookies("");
                if (cookies != null && !force) {
                    br.setCookies(account.getHoster(), cookies);
                    return;
                }
                final String postData = "sUsername=" + Encoding.urlEncode(account.getUser()) + "&sPassword=" + Encoding.urlEncode(account.getPass()) + "&bAutoLoginActivate=1";
                br.postPage("https://www." + account.getHoster() + "/STV/M/Index.cfm?sk=PREMIUM", postData);
                if (br.containsHTML("No htmlCode read")) {
                    br.getPage("https://www." + account.getHoster() + "/STV/M/obj/TVProgCtr/tvctShow.cfm");
                }
                if (!br.containsHTML("class=\"member\\-nav\\-li member\\-nav\\-account \"") || br.containsHTML("Bitte verifizieren Sie Ihre Logindaten")) {
                    if ("de".equalsIgnoreCase(lang)) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen? Versuche folgendes:\r\n1. Falls dein Passwort Sonderzeichen enthält, ändere es (entferne diese) und versuche es erneut!\r\n2. Gib deine Zugangsdaten per Hand (ohne kopieren/einfügen) ein.", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nYou're sure that the username and password you entered are correct? Some hints:\r\n1. If your password contains special characters, change it (remove them) and try again!\r\n2. Type in your username/password by hand without copy & paste.", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                final String acc_count_archive_entries = br.getRegex(">(\\d+) Sendungen im Archiv<").getMatch(0);
                if (acc_count_archive_entries != null) {
                    account.setProperty(PROPERTY_acc_count_archive_entries, acc_count_archive_entries);
                }
                account.saveCookies(br.getCookies(account.getHoster()), "");
            } catch (final PluginException e) {
                account.clearCookies("");
                throw e;
            }
        }
    }

    public static String getAccessTokenAndSetHeaderAPI(final Browser br, final Account account) {
        final String api_access_token = account.getStringProperty(PROPERTY_ACCOUNT_API_SESSIONID, null);
        if (api_access_token != null) {
            setAPIAuthHeaders(br, api_access_token);
        }
        return api_access_token;
    }

    public static void login_api(final Browser br, final Account account, final boolean force) throws Exception {
        synchronized (account) {
            api_prepBrowser(br);
            String api_access_token = getAccessTokenAndSetHeaderAPI(br, account);
            String refresh_token = account.getStringProperty(PROPERTY_refresh_token, null);
            long expires_in = account.getLongProperty(PROPERTY_expires_in, 0);
            final boolean token_expired = expires_in > 0 && expires_in <= System.currentTimeMillis();
            if (refresh_token != null && (force || token_expired)) {
                /* Avoid full login - try to refresh old token instead! */
                api_POST(br, API_BASE_AUTH + "token", "grant_type=refresh_token&client_id=" + getAPIClientID() + "&client_secret=" + getAPISecretKey() + "&refresh_token=" + Encoding.urlEncode(refresh_token));
                try {
                    parseLoginInfo(br, account);
                } catch (final PluginException e) {
                    api_access_token = null;
                    /* Prepare browser for full login - remove old/invalid "Authorization" header */
                    br.clearCookies(account.getHoster());
                    br.getHeaders().remove("Authorization");
                }
            }
            if (api_access_token == null) {
                /* Only generate new access_token if we have none */
                /* New token required */
                api_POST(br, API_BASE_AUTH + "token", "grant_type=password&client_id=" + getAPIClientID() + "&client_secret=" + getAPISecretKey() + "&username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
                parseLoginInfo(br, account);
            }
        }
    }

    public static void parseLoginInfo(final Browser br, final Account account) throws PluginException {
        String api_access_token = PluginJSonUtils.getJson(br, "access_token");
        String refresh_token = PluginJSonUtils.getJson(br, "refresh_token");
        /* 2017-09-18: This will usually last 3599 seconds --> ~1 hour */
        final String seconds_expires_in_str = PluginJSonUtils.getJson(br, "expires_in");
        if (br.getHttpConnection().getResponseCode() == 400 || StringUtils.isEmpty(api_access_token) || StringUtils.isEmpty(refresh_token) || StringUtils.isEmpty(seconds_expires_in_str)) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen? Versuche folgendes:\r\n1. Falls dein Passwort Sonderzeichen enthält, ändere es (entferne diese) und versuche es erneut!\r\n2. Gib deine Zugangsdaten per Hand (ohne kopieren/einfügen) ein.", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nYou're sure that the username and password you entered are correct? Some hints:\r\n1. If your password contains special characters, change it (remove them) and try again!\r\n2. Type in your username/password by hand without copy & paste.", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
        /* 60 seconds of tolerance to avoid requests happening while our token expires. */
        long expires_in = (Long.parseLong(seconds_expires_in_str) - 60) * 1000;
        account.setProperty(PROPERTY_ACCOUNT_API_SESSIONID, api_access_token);
        account.setProperty(PROPERTY_refresh_token, refresh_token);
        account.setProperty(PROPERTY_expires_in, System.currentTimeMillis() + expires_in);
        setAPIAuthHeaders(br, api_access_token);
    }

    /* Always compare to the decrypter */
    @SuppressWarnings("deprecation")
    private void getPageSafe(final String url, final Account account) throws Exception {
        this.br.getPage(url);
        handleErrorsWebsite(br, account);
    }

    /*
     * Handles website json errors.
     */
    @Deprecated
    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void handleErrorsWebsite(final Browser br, final Account account) throws Exception {
        /*
         * Do NOT use the json parser for the first check as especially when used via decrypter, the amount of data can be huge which can
         * lead to parser memory problems/crashes.
         */
        final boolean isInErrorState = br.containsHTML("\\d+\\.\\d+E\\d+,\"NOK\",\"");
        if (isInErrorState) {
            final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
            final List<Object> errorlist = (List) entries.get("ARRVIDEOURL");
            final String errormessage = (String) errorlist.get(2);
            if (errormessage.contains("Aufnahme zu betrachten, laden Sie bitte die ungeschnittene Version")) {
                errorAdsFreeUnavailable(60 * 60 * 1000, null);
            } else if (errormessage.equals("unknown error")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Serverfehler: 'unknown error'", 15 * 60 * 1000l);
            } else {
                /* We can retry on unknown errorstates here as we know that Stv will usually fix them soon. */
                logger.warning("Unhandled / Unknown error happened");
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unbehandelter Fehler: " + errormessage, 30 * 60 * 1000l);
            }
        } else if (br.getURL().contains(URL_LOGGED_OUT)) {
            logger.info("We were loggedout for unknown reasons --> Trying to perform full login");
            login_website(br, account, true);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Loginproblem", 30 * 1000l);
        }
    }

    private String getErrormessageDownloadWithadsInTimeX(final long millis_time_remaining_until_download_with_ads_allowed) {
        return String.format("%s bis Download mit Werbung | Werbefreie Version noch nicht verfügbar", TimeFormatter.formatMilliSeconds(millis_time_remaining_until_download_with_ads_allowed, 0));
    }

    /**
     * Execute this if a download attempt was made but the length of the adFree version is 0. <br />
     * Waits either the user-defined waittime or at least the forcedWaittime.
     */
    private void errorAdsFreeUnavailableWithForcedWaittime(final DownloadLink link, final long forcedWaittime) throws PluginException {
        final String errormessage;
        final long waittime;
        final long timestamp_with_ads_allowed = getTimestampWhenDownloadWithAdsIsAllowed(link);
        final long timestamp_current = System.currentTimeMillis();
        if (timestamp_with_ads_allowed > timestamp_current) {
            waittime = timestamp_with_ads_allowed - System.currentTimeMillis();
            errormessage = getErrormessageDownloadWithadsInTimeX(waittime);
        } else {
            waittime = forcedWaittime;
            errormessage = USERTEXT_NOCUTAVAILABLE;
        }
        errorAdsFreeUnavailable(waittime, errormessage);
    }

    private void errorAdsFreeUnavailable(final long waitMillis, String errormessage) throws PluginException {
        if (errormessage == null) {
            errormessage = USERTEXT_NOCUTAVAILABLE;
        }
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormessage, waitMillis);
    }

    private void handleErrorsAPI(final Browser br, final Account account) throws Exception {
        /* TODO: Fill me */
        if (br.getHttpConnection().getResponseCode() == 401) {
            login_api(br, account, true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Loginproblem", 1 * 60 * 1000l);
        }
        Map<String, Object> error_map = null;
        try {
            final Object errorO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (errorO instanceof List) {
                final List<Object> errorlist = (List<Object>) errorO;
                error_map = (Map<String, Object>) errorlist.get(1);
            }
        } catch (final Throwable e) {
        }
        if (error_map == null) {
            /* No error */
            return;
        }
        logger.info("An API error happened: " + error_map);
        final String humanReadableErrormessage = (String) error_map.get("userMessage");
        final String id = (String) error_map.get("id");
        if (humanReadableErrormessage != null) {
            logger.info("API_error: " + humanReadableErrormessage);
        }
        if (id.equalsIgnoreCase("DOWNLOADSESSIONVIDEOFILESSERVICE_NOCONTENT")) {
            logger.info("AdFree version is empty --> Failed to start download");
            errorAdsFreeUnavailableWithForcedWaittime(this.currDownloadlink, 60 * 60 * 1000);
        } else if (id.equalsIgnoreCase("NOTFOUND_TELECAST_ID")) {
            logger.info("Offline message inside errorhandling --> This is supposed to be handled correctly by other code");
            // /* Usually this goes along with a 404 response */
            // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            /** TODO: Collect errors at this stage */
            if (PluginEnvironment.ACCOUNT_CHECK.isCurrentPluginEnvironment()) {
                /* Account error */
                throw new AccountUnavailableException(humanReadableErrormessage, 5 * 60 * 1000l);
            } else {
                /* Error during download */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, humanReadableErrormessage, 5 * 60 * 1000l);
            }
        }
    }

    /**
     * @param link
     *            DownloadLink
     * @param user_selected_video_quality
     *            : Vom Benutzer bevorzugte Qualitätsstufe
     * @param downloadWithoutAds
     *            : Videos mit angewandter Schnittliste bevorzugen oder nicht
     * @throws Exception
     */
    @SuppressWarnings("unused")
    private void requestDownloadAPI(final DownloadLink link, final String user_selected_video_quality, final String downloadWithoutAds) throws Exception {
        api_POST(this.br, "https://www." + link.getHost() + "/STV/M/obj/cRecordOrder/croGetDownloadUrl.cfm?null.GetDownloadUrl", "ajax=true&clientAuthenticationKey=&callCount=1&c0-scriptName=null&c0-methodName=GetDownloadUrl&c0-id=&c0-param0=number:" + getTelecastId(link) + "&" + user_selected_video_quality + "&c0-param2=boolean:" + downloadWithoutAds + "&xml=true&");
    }

    /**
     * @param link
     *            :DownloadLink
     * @param user_selected_video_quality
     *            : ID of user selected quality
     * @param downloadWithoutAds
     *            : Download with- or without ads
     */
    @Deprecated
    private void requestDownloadWebsite(final DownloadLink link, final int user_selected_video_quality, final boolean downloadWithoutAds) throws Exception {
        final String downloadoverview_url = "https://www." + link.getHost() + "/STV/M/obj/cRecordOrder/croGetDownloadUrl2.cfm?TelecastId=" + getTelecastId(link) + "&iFormat=" + user_selected_video_quality + "&bAdFree=" + Boolean.toString(downloadWithoutAds);
        this.getPageSafe(downloadoverview_url, this.currAcc);
    }

    /**
     * @param link
     *            :DownloadLink
     * @param user_selected_video_quality
     *            : ID of user selected quality
     * @param downloadWithoutAds
     *            : Download with- or without ads
     * @throws Exception
     */
    private void accessDownloadPageAPI(final DownloadLink link, final int user_selected_video_quality, final boolean downloadWithoutAds) throws Exception {
        api_GET(this.br, String.format(Locale.ROOT, "/records/%s/downloads/%d?adfree=%s", getTelecastId(link), user_selected_video_quality, Boolean.toString(downloadWithoutAds)));
    }

    /**
     * Deletes a desired telecastID from the users' account(!) <br />
     * Works for API- and website mode.
     *
     * @param link
     *            DownloadLink: The DownloadLink whose telecastID will be deleted.
     */
    private void killTelecastID(final DownloadLink link) throws Exception {
        if (apiActive()) {
            killTelecastIDAPI(link);
        } else {
            killTelecastIDWebsite(link);
        }
    }

    /**
     * Marks a downloaded file serverside as downloaded. <br />
     * Only usable in API mode. Will do nothing in website mode! <br />
     * On success, user will see the text "Completely downloaded" on the Stv telecastID site in the section "Tags".
     *
     * @throws Exception
     */
    private void markTelecastIdAsDownloaded(final DownloadLink link) throws Exception {
        if (!this.apiActive()) {
            logger.info("API is not available --> Cannot mark telecastID as downloaded");
            return;
        }
        logger.info("Mark telecastID as downloaded ...");
        final String telecastID = getTelecastId(link);
        api_POST(this.br, "/records/" + telecastID + "/tags/download-completed", "id=" + telecastID);
        final int responsecode = this.br.getHttpConnection().getResponseCode();
        if (responsecode == 200) {
            logger.info("Successfully marked telecastID as downloaded");
        } else {
            logger.info("Unknown status: Not sure whether telecastID has been marked as downloaded or not");
        }
    }

    /**
     * Deletes a desired telecastID from the users' account(!) <br />
     *
     * @param link
     *            DownloadLink: The DownloadLink whose telecastID will be deleted.
     */
    @Deprecated
    private void killTelecastIDWebsite(final DownloadLink link) throws Exception {
        try {
            final String deleteurl = "https://www." + link.getHost() + "/STV/M/obj/cRecordOrder/croDelete.cfm?TelecastID=" + getTelecastId(link);
            this.br.getPage(deleteurl);
            if (br.containsHTML("\"ok\"")) {
                logger.info("Successfully deleted telecastID");
            } else {
                logger.warning("Failed to delete telecastID");
            }
        } catch (final Throwable e) {
            logger.info("Failed to delete telecastID");
        }
    }

    /**
     * Deletes a desired telecastID from the users' account(!) <br />
     *
     * @param link
     *            DownloadLink: The DownloadLink whose telecastID will be deleted.
     */
    private void killTelecastIDAPI(final DownloadLink link) throws Exception {
        try {
            api_GET(this.br, "/records/" + getTelecastId(link));
            final long responsecode = br.getHttpConnection().getResponseCode();
            if (responsecode == 422) {
                logger.info("Failed to delete telecastID");
            } else if (responsecode == 200) {
                logger.info("Successfully deleted telecastID");
            } else {
                logger.info("Unknown status: Not sure whether telecastID has been deleted or not");
            }
        } catch (final Throwable e) {
            logger.info("Failed to delete telecastID");
        }
    }

    @Deprecated
    private static void site_prepBrowser(final Browser br) {
        br.setReadTimeout(3 * 60 * 1000);
        br.setConnectTimeout(3 * 60 * 1000);
        br.getHeaders().put("User-Agent", "JDownloader");
    }

    /** Prepare Browser for API usage. */
    public static Browser api_prepBrowser(final Browser br) {
        br.setReadTimeout(3 * 60 * 1000);
        br.setConnectTimeout(3 * 60 * 1000);
        br.getHeaders().put("User-Agent", "JDownloader");
        /* Important header; without it, we might get XML instead! */
        br.getHeaders().put("Accept", "application/json");
        br.setAllowedResponseCodes(new int[] { 400, 422 });
        return br;
    }

    /** Set Authorization header(s) for API usage. */
    public static void setAPIAuthHeaders(final Browser br, final String auth_token) {
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + auth_token);
    }

    /**
     * Returns boolean to indicate whether API usage is enabled and working (= login token exists and is valid). <br />
     */
    private boolean apiActive() {
        return (this.currAcc != null && this.currAcc.getStringProperty(PROPERTY_ACCOUNT_API_SESSIONID, null) != null && is_API_enabled(this.getHost()));
    }

    private long getTimestampWhenDownloadWithAdsIsAllowed(final DownloadLink link) {
        final SubConfiguration cfg = SubConfiguration.getConfig(getHost());
        final long userDefinedWaitHours = cfg.getLongProperty(ADS_FREE_UNAVAILABLE_HOURS, SaveTv.defaultADS_FREE_UNAVAILABLE_HOURS);
        final long timestamp_releasedate = link.getLongProperty(PROPERTY_originaldate_end, 0);
        final long timestamp_with_ads_allowed = timestamp_releasedate + userDefinedWaitHours * 60 * 60 * 1000;
        return timestamp_with_ads_allowed;
    }

    public static long calculateFilesize(final DownloadLink link, final String formatString) {
        return calculateFilesize(link, convertQualityStringToInternalID(formatString));
    }

    /**
     * Returns estimate filesize based on calculation via time and hardcoded bitrate. <br />
     * Especially useful whenever filesize is not given via website / API but still 'nice to have'. <br />
     * Keep in mind that this calculation must not be very accurate.
     */
    public static long calculateFilesize(final DownloadLink link, final int formatID) {
        final int duration_relevant = getDurationDependingOnUserSettings(link);
        final double mb_per_second = getBitrateForFormat(formatID);
        final double calculated_filesize = mb_per_second * duration_relevant * 1024 * 1024;
        return (long) calculated_filesize;
    }

    public static double getBitrateForFormat(final int format) {
        switch (format) {
        case SITE_FORMAT_LQ:
            return QUALITY_H264_MOBILE_MB_PER_SECOND;
        case SITE_FORMAT_HQ:
            return QUALITY_H264_NORMAL_MB_PER_SECOND;
        case SITE_FORMAT_HD:
            return QUALITY_HD_MB_PER_SECOND;
        default:
            return QUALITY_HD_MB_PER_SECOND;
        }
    }

    /** AdsFree duration < Duration of video with ads */
    public static int getDurationDependingOnUserSettings(final DownloadLink link) {
        final int duration_seconds_adsfree = link.getIntegerProperty(PROPERTY_site_runtime_seconds_adsfree, 0);
        final int duration_seconds_withads = link.getIntegerProperty(PROPERTY_site_runtime_seconds_withads, 0);
        final int duration_relevant;
        final boolean user_prefers_adsfree = getPreferAdsFree(link);
        if (user_prefers_adsfree && duration_seconds_adsfree > 0) {
            duration_relevant = duration_seconds_adsfree;
        } else {
            duration_relevant = duration_seconds_withads;
        }
        return duration_relevant;
    }

    /** @return true: formatID is known, false: formatID is unknown */
    public static boolean isKnownFormatID(final int formatID) {
        return (formatID > 3 && formatID < 7);
    }

    /**
     * Returns selected quality id. <br />
     * This is only that complicated because the user selection can be overridden via parameters inside the URL.
     */
    public static int getConfiguredVideoFormatID(final DownloadLink link) {
        final int videoformatStored = link.getIntegerProperty(PROPERTY_download_format_id, -1);
        if (videoformatStored != -1) {
            /*
             * A download using this format has already been started before --> Keep using it so a resumed filestream never continues with a
             * different format than it was started with, even if the user changes his settings in the meantime.
             */
            return videoformatStored;
        }
        final int videoformatURL = getConfiguredVideoFormatUrl(link);
        if (videoformatURL != -1 && isKnownFormatID(videoformatURL)) {
            return videoformatURL;
        }
        return getConfiguredVideoFormatConfig();
    }

    /** Returns default formatID. 2017-08-11: Returns ID for HQ format --> Website-default */
    public static int getDefaultFormatID() {
        return SITE_FORMAT_HQ;
    }

    /** Returns BEST formatID. 2017-08-11: Returns ID for HD format --> Website-best */
    public static int getBestFormatID() {
        return SITE_FORMAT_HD;
    }

    /** Converts quality Strings e.g. "HD" to their corresponding IDs: --> "HD" --> 6 */
    public static int convertQualityStringToInternalID(final String qualityString) {
        final int qualityID;
        if (qualityString == null) {
            qualityID = SITE_FORMAT_HQ;
        } else if (qualityString.equals(STATE_QUALITY_HD)) {
            qualityID = SITE_FORMAT_HD;
        } else if (qualityString.equals(STATE_QUALITY_HQ)) {
            qualityID = SITE_FORMAT_HQ;
        } else {
            qualityID = SITE_FORMAT_LQ;
        }
        return qualityID;
    }

    /** Returns user selected formatID based on PluginConfiguration */
    @SuppressWarnings("deprecation")
    public static int getConfiguredVideoFormatConfig() {
        switch (SubConfiguration.getConfig(HOST_STATIC).getIntegerProperty(SELECTED_VIDEO_FORMAT, defaultSELECTED_VIDEO_FORMAT)) {
        case 0:
            return SITE_FORMAT_HD;
        case 1:
            return SITE_FORMAT_HQ;
        case 2:
            return SITE_FORMAT_LQ;
        default:
            /* Should never happen */
            return getBestFormatID();
        }
    }

    /**
     * Returns formatID from inside user-added URL. <br />
     * Keep in mind, this does NOT verify whether the found value is correct or not. <br />
     *
     * @return Either formatID from inside URL or -1 if nothing found.
     */
    public static int getConfiguredVideoFormatUrl(final DownloadLink link) {
        if (link == null) {
            /* Fallback - upper functions should now use formatID which user has selected in plugin settings. */
            return -1;
        }
        final String format_from_url = new Regex(link.getDownloadURL(), "(?i)preferformat=(\\d+)").getMatch(0);
        if (format_from_url == null) {
            /* Fallback - upper functions should now use formatID which user has selected in plugin settings. */
            return -1;
        }
        /* Convert official videoformat-number to internal number. */
        return Integer.parseInt(format_from_url);
    }

    /** Calculates runtime in minutes based on given filesize and hardcoded bitrates. */
    @Deprecated
    @SuppressWarnings("unused")
    private double site_get_calculated_runtime_minutes(final DownloadLink link, final long page_size_mb) {
        double run_time_calculated = 0;
        final int selected_video_format = getConfiguredVideoFormatID(link);
        switch (selected_video_format) {
        case SITE_FORMAT_HD:
            run_time_calculated = page_size_mb / QUALITY_HD_MB_PER_SECOND;
            break;
        case SITE_FORMAT_HQ:
            run_time_calculated = page_size_mb / QUALITY_H264_NORMAL_MB_PER_SECOND;
            break;
        case SITE_FORMAT_LQ:
            run_time_calculated = page_size_mb / QUALITY_H264_MOBILE_MB_PER_SECOND;
            break;
        default:
            run_time_calculated = page_size_mb / QUALITY_H264_NORMAL_MB_PER_SECOND;
            break;
        }
        return run_time_calculated;
    }

    @SuppressWarnings("unchecked")
    public static List<Object> jsonGetFormatArrayAPI(final Map<String, Object> entries) {
        if (entries == null) {
            return null;
        }
        final Object formatsO = entries.get("formats");
        if (formatsO == null || !(formatsO instanceof List)) {
            return null;
        }
        return (List<Object>) formatsO;
    }

    public static int jsonGetFormatArrayGetIDAPI(final Map<String, Object> entries) {
        return (int) JavaScriptEngineFactory.toLong(entries.get("id"), SITE_FORMAT_HD);
    }

    /** Ads-Free status -1 = Unknown, 0 = false, 1 = true */
    public static short jsonGetAdsFreeAvailableAPIDetailed(final Map<String, Object> entries) {
        final Object adFreeAvailableO = entries != null ? entries.get("adFreeAvailable") : null;
        final short adsfreeStatus;
        if (adFreeAvailableO == null || !(adFreeAvailableO instanceof Boolean)) {
            adsfreeStatus = -1;
        } else {
            final boolean adsFree = ((Boolean) adFreeAvailableO).booleanValue();
            if (adsFree) {
                adsfreeStatus = 1;
            } else {
                adsfreeStatus = 0;
            }
        }
        return adsfreeStatus;
    }

    /** Ads-Free status true, false (NOT unknown!) */
    public static boolean jsonGetAdsFreeAvailableAPI(final Map<String, Object> entries) {
        final short adsFreeStatus = jsonGetAdsFreeAvailableAPIDetailed(entries);
        if (adsFreeStatus == 1) {
            return true;
        } else {
            return false;
        }
    }

    @Deprecated
    @SuppressWarnings("unchecked")
    public static int jsonGetBestQualityIdWebsite(final List<Object> sourcelist) {
        final long recordingformat;
        if (sourcelist != null && sourcelist.size() > 0) {
            final Map<String, Object> entries = (Map<String, Object>) sourcelist.get(sourcelist.size() - 1);
            recordingformat = jsonGetRecordingformatid(entries);
        } else {
            /* Fallback to a format which is always available. */
            recordingformat = getDefaultFormatID();
        }
        return (int) recordingformat;
    }

    @SuppressWarnings("unchecked")
    public static String jsonGetBestQualityIdAPI(final List<Object> sourcelist) {
        if (sourcelist != null && sourcelist.size() > 0) {
            Map<String, Object> entries = null;
            long quality_max = SITE_FORMAT_HQ;
            long quality_temp;
            for (final Object qualityo : sourcelist) {
                entries = (Map<String, Object>) qualityo;
                quality_temp = JavaScriptEngineFactory.toLong(JavaScriptEngineFactory.walkJson(entries, "recordFormat/id"), SITE_FORMAT_HQ);
                if (quality_temp > quality_max) {
                    quality_max = quality_temp;
                }
            }
            return Long.toString(quality_max);
        } /* Fallback to a format which is always available. */
        return Long.toString(SITE_FORMAT_HQ);
    }

    @Deprecated
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static List<Object> jsonGetVideoSourcelist(final Map<String, Object> sourcemap) {
        if (sourcemap == null) {
            return null;
        }
        final List<Object> sourcelist = (List) sourcemap.get("ARRALLOWDDOWNLOADFORMATS");
        return sourcelist;
    }

    @Deprecated
    public static int jsonGetRecordingformatid(final Map<String, Object> entries) {
        final long recordingformatid;
        final Object recordingformatido;
        if (entries != null) {
            recordingformatido = entries.get("RECORDINGFORMATID");
        } else {
            /* Errorhandling */
            recordingformatido = null;
        }
        if (recordingformatido instanceof Number) {
            recordingformatid = ((Number) recordingformatido).longValue();
        } else {
            recordingformatid = getDefaultFormatID();
        }
        return (int) recordingformatid;
    }

    /** Checks whether user-defined formatID is available or not. */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static boolean jsonIsDesiredFormatAvailableWebsite(final List<Object> sourcelist, final int desiredFormat) {
        if (sourcelist == null) {
            return false;
        }
        Map<String, Object> entries = null;
        int format_id = -1;
        for (final Object vsorceo : sourcelist) {
            entries = (Map<String, Object>) vsorceo;
            format_id = jsonGetRecordingformatid(entries);
            if (format_id == desiredFormat) {
                return true;
            }
        }
        return false;
    }

    /**
     * Performs save.tv API POST requests without error handling. <br />
     *
     * @throws Exception
     */
    private static String api_POST(final Browser br, String url, final String postdata) throws Exception {
        url = correctURLAPI(url);
        br.postPage(url, postdata);
        return br.getRequest().getHtmlCode();
    }

    /**
     * Performs save.tv API GET requests. <br />
     *
     * @throws Exception
     */
    private String api_GET(final Browser br, String url) throws Exception {
        url = correctURLAPI(url);
        br.getPage(url);
        handleErrorsAPI(br, this.currAcc);
        return br.getRequest().getHtmlCode();
    }

    public static String correctURLAPI(String url) {
        if (url.startsWith("/")) {
            /* Just in case we use another host e.g. for login. */
            url = API_BASE + url;
        }
        return url;
    }

    /*
     * 2017-11-14: Permanently enabled API usage and disabled website as it is not required anymore in the future. Please leave the old code
     * so that we can easily switch in case of major API issues.
     */
    public static boolean is_API_enabled(final String host) {
        // return SubConfiguration.getConfig(host).getBooleanProperty(USEAPI, defaultUSEAPI);
        return true;
    }

    /** Corrects all kinds of Strings which Stv provides, also makes filenames look nicer. */
    @SuppressWarnings("deprecation")
    public static String correctData(final String host, final String input) {
        String output = Encoding.htmlDecode(input);
        output = output.replace("_", " ");
        output = output.trim();
        output = output.replaceAll("(\r|\n)", "");
        output = output.replace("/", SubConfiguration.getConfig(host).getStringProperty(CUSTOM_FILENAME_SEPARATION_MARK, defaultCUSTOM_FILENAME_SEPARATION_MARK));
        /* Correct spaces */
        final String[] unneededSpaces = new Regex(output, ".*?([ ]{2,}).*?").getColumn(0);
        if (unneededSpaces != null && unneededSpaces.length != 0) {
            for (String unneededSpace : unneededSpaces) {
                output = output.replace(unneededSpace, " ");
            }
        }
        return output;
    }

    @SuppressWarnings("deprecation")
    private static String getTelecastId(final DownloadLink link) {
        final String telecastID;
        final String url = link.getDownloadURL();
        if (new Regex(url, Pattern.compile(LINKTYPE_DIRECT)).matches()) {
            /* Convert directurls --> 'Normal' Stv 'telecastID'-URLs */
            telecastID = new Regex(url, Pattern.compile("https?://[A-Za-z0-9\\-]+\\.save\\.tv/\\d+_(\\d+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
        } else {
            telecastID = new Regex(url, Pattern.compile("telecastid=(\\d+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
        }
        return telecastID;
    }

    /**
     * @return: Returns a random number for the DownloadLink - uses saved random number if existent - if not, creates random number and
     *          saves it.
     */
    private static String getRandomNumber(final DownloadLink link) {
        String randomnumber = link.getStringProperty(PROPERTY_stv_randomnumber, null);
        if (randomnumber == null) {
            final DecimalFormat df = new DecimalFormat("0000");
            randomnumber = df.format(new Random().nextInt(10000));
            link.setProperty(PROPERTY_stv_randomnumber, randomnumber);
        }
        return randomnumber;
    }

    @SuppressWarnings("deprecation")
    public static String getFormattedFilename(final Plugin plugin, final DownloadLink link) throws ParseException {
        final SubConfiguration cfg = SubConfiguration.getConfig(plugin.getHost());
        final String customStringForEmptyTags = getCustomStringForEmptyTags(plugin.getHost());
        final String acc_username = getDownloadableViaUsername(link);
        final String server_filename = link.getStringProperty(PROPERTY_server_filename, customStringForEmptyTags);
        final String site_title = link.getStringProperty(PROPERTY_plainfilename, customStringForEmptyTags);
        final String ext = link.getStringProperty(PROPERTY_type, EXTENSION_default);
        final String quality = link.getStringProperty(PROPERTY_quality, STATE_QUALITY_UNKNOWN);
        final String ad_free = getAdFreeText(link);
        final String genre = link.getStringProperty(PROPERTY_genre, customStringForEmptyTags);
        final String producecountry = link.getStringProperty(PROPERTY_producecountry, customStringForEmptyTags);
        final String producecountry_short = link.getStringProperty(PROPERTY_producecountry_short, customStringForEmptyTags);
        final String produceyear = link.getStringProperty(PROPERTY_produceyear, customStringForEmptyTags);
        final String randomnumber = getRandomNumber(link);
        final String telecastid = getTelecastId(link);
        final String tv_station = link.getStringProperty(PROPERTY_plain_tv_station, customStringForEmptyTags);
        final long site_category = link.getIntegerProperty(PROPERTY_category_id, -1);
        final String site_category_str = site_category == -1 ? defaultCUSTOM_FILENAME_EMPTY_TAG_STRING : Long.toString(site_category);
        final long date = link.getLongProperty(PROPERTY_originaldate, 0l);
        String formattedDate = null;
        final String userDefinedDateFormat = cfg.getStringProperty(CUSTOM_DATE, defaultCUSTOM_DATE);
        Date theDate = new Date(date);
        if (userDefinedDateFormat != null) {
            try {
                final SimpleDateFormat formatter = new SimpleDateFormat(userDefinedDateFormat);
                formattedDate = formatter.format(theDate);
            } catch (Exception e) {
                /* prevent user error killing plugin */
                formattedDate = defaultCUSTOM_FILENAME_EMPTY_TAG_STRING;
            }
        }
        String formattedFilename = null;
        if (!isSeries(link)) {
            /* For all links except series */
            formattedFilename = cfg.getStringProperty(CUSTOM_FILENAME_MOVIES, defaultCUSTOM_FILENAME_MOVIES);
            if (formattedFilename == null || formattedFilename.equals("")) {
                formattedFilename = defaultCUSTOM_FILENAME_MOVIES;
            }
            /* Make sure that the user entered a VALID custom filename - if not, use the default name */
            if (!formattedFilename.contains("*endung*") || (!formattedFilename.contains("*videotitel*") && !formattedFilename.contains("*zufallszahl*") && !formattedFilename.contains("*telecastid*") && !formattedFilename.contains("*sendername*") && !formattedFilename.contains("*username*") && !formattedFilename.contains("*quality*") && !formattedFilename.contains("*server_dateiname*"))) {
                formattedFilename = defaultCUSTOM_FILENAME_MOVIES;
            }
            formattedFilename = formattedFilename.replace("*zufallszahl*", randomnumber);
            formattedFilename = formattedFilename.replace("*telecastid*", telecastid);
            formattedFilename = formattedFilename.replace("*datum*", formattedDate);
            formattedFilename = formattedFilename.replace("*produktionsjahr*", produceyear);
            formattedFilename = formattedFilename.replace("*endung*", ext);
            formattedFilename = formattedFilename.replace("*quality*", quality);
            formattedFilename = formattedFilename.replace("*werbefrei*", ad_free);
            formattedFilename = formattedFilename.replace("*sendername*", tv_station);
            formattedFilename = formattedFilename.replace("*kategorie*", site_category_str);
            formattedFilename = formattedFilename.replace("*genre*", genre);
            formattedFilename = formattedFilename.replace("*produktionsland*", producecountry);
            formattedFilename = formattedFilename.replace("*produktionsland_kurz*", producecountry_short);
            formattedFilename = formattedFilename.replace("*username*", acc_username);
            /* Insert actual filename at the end to prevent errors with tags */
            formattedFilename = formattedFilename.replace("*server_dateiname*", server_filename);
            formattedFilename = formattedFilename.replace("*videotitel*", site_title);
        } else {
            /* For series */
            final String episodename = link.getStringProperty(PROPERTY_episodename, customStringForEmptyTags);
            final int seasonnumber = getSeasonNumber(link);
            final int episodenumber = getEpisodeNumber(link);
            final String seasonAndEpisodenumber = getSeasonnumberAndEpisodenumber(seasonnumber, episodenumber);
            formattedFilename = cfg.getStringProperty(CUSTOM_FILENAME_SERIES, defaultCUSTOM_FILENAME_SERIES);
            if (formattedFilename == null || formattedFilename.equals("")) {
                formattedFilename = defaultCUSTOM_FILENAME_SERIES;
            }
            /* Make sure that the user entered a VALID custom filename - if not, use the default name */
            if (!formattedFilename.contains("*endung*") || (!formattedFilename.contains("*serientitel*") && !formattedFilename.contains("*episodenname*") && !formattedFilename.contains("*staffelnummer*") && !formattedFilename.contains("*episodennummer*") && !formattedFilename.contains("*episodennummer_und_staffelnummer*") && !formattedFilename.contains("*zufallszahl*") && !formattedFilename.contains("*telecastid*") && !formattedFilename.contains("*sendername*") && !formattedFilename.contains("*username*") && !formattedFilename.contains("*quality*") && !formattedFilename.contains("*server_dateiname*"))) {
                formattedFilename = defaultCUSTOM_FILENAME_SERIES;
            }
            formattedFilename = formattedFilename.replace("*zufallszahl*", randomnumber);
            formattedFilename = formattedFilename.replace("*telecastid*", telecastid);
            formattedFilename = formattedFilename.replace("*datum*", formattedDate);
            formattedFilename = formattedFilename.replace("*produktionsjahr*", produceyear);
            formattedFilename = formattedFilename.replace("*staffelnummer*", Integer.toString(seasonnumber));
            formattedFilename = formattedFilename.replace("*episodennummer*", Integer.toString(episodenumber));
            formattedFilename = formattedFilename.replace("*episodennummer_und_staffelnummer*", seasonAndEpisodenumber);
            formattedFilename = formattedFilename.replace("*endung*", ext);
            formattedFilename = formattedFilename.replace("*quality*", quality);
            formattedFilename = formattedFilename.replace("*werbefrei*", ad_free);
            formattedFilename = formattedFilename.replace("*sendername*", tv_station);
            formattedFilename = formattedFilename.replace("*kategorie*", site_category_str);
            formattedFilename = formattedFilename.replace("*genre*", genre);
            formattedFilename = formattedFilename.replace("*produktionsland*", producecountry);
            formattedFilename = formattedFilename.replace("*produktionsland_kurz*", producecountry_short);
            formattedFilename = formattedFilename.replace("*username*", acc_username);
            /* Insert filename at the end to prevent errors with tags */
            formattedFilename = formattedFilename.replace("*server_dateiname*", server_filename);
            formattedFilename = formattedFilename.replace("*serientitel*", site_title);
            formattedFilename = formattedFilename.replace("*episodenname*", episodename);
        }
        formattedFilename = fixCharIssues(formattedFilename);
        return formattedFilename;
    }

    /** Returns either the original server filename or one that is very similar to the original */
    public static String getFakeOriginalFilename(final Plugin plugin, final DownloadLink link) throws ParseException {
        final String ext = link.getStringProperty(PROPERTY_type, EXTENSION_default);
        final long date = link.getLongProperty(PROPERTY_originaldate, 0l);
        String formattedDate = null;
        /* Get correctly formatted date */
        String dateFormat = "yyyy-MM-dd";
        SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");
        Date theDate = new Date(date);
        try {
            formatter = new SimpleDateFormat(dateFormat);
            formattedDate = formatter.format(theDate);
        } catch (Exception e) {
            /* prevent user error killing plugin */
            formattedDate = "";
        }
        /* Get correctly formatted time */
        dateFormat = "HHmm";
        String time = "0000";
        try {
            formatter = new SimpleDateFormat(dateFormat);
            time = formatter.format(theDate);
        } catch (Exception e) {
            /* prevent user error killing plugin */
            time = "0000";
        }
        final String acc_username = getDownloadableViaUsername(link);
        String formattedFilename = link.getStringProperty(PROPERTY_server_filename, null);
        if (formattedFilename != null) {
            /* Server = already original filename - no need to 'fake' anything */
            formattedFilename += EXTENSION_default;
        } else {
            final String title = convertNormalDataToServer(link.getStringProperty(PROPERTY_plainfilename, getCustomStringForEmptyTags(plugin.getHost())));
            String episodename = link.getStringProperty(PROPERTY_episodename, null);
            final int episodenumber = getEpisodeNumber(link);
            formattedFilename = title + "_";
            if (episodename != null) {
                episodename = convertNormalDataToServer(episodename);
                formattedFilename += episodename + "_";
            }
            /* Only add "Folge" if episodenumber is available */
            if (episodenumber > -1) {
                formattedFilename += "Folge" + episodenumber + "_";
            }
            formattedFilename += formattedDate + "_";
            formattedFilename += time + "_" + acc_username;
            /*
             * Finally, make sure we got no double underscores. Do this before we set the file extension es dots will be replaced within the
             * convertNormalDataToServer method!
             */
            formattedFilename = convertNormalDataToServer(formattedFilename);
            formattedFilename += ext;
        }
        formattedFilename = fixCharIssues(formattedFilename);
        return formattedFilename;
    }

    private static int getSeasonNumber(final DownloadLink link) {
        return link.getIntegerProperty(PROPERTY_seasonnumber, -1);
    }

    private static int getEpisodeNumber(final DownloadLink link) {
        return link.getIntegerProperty(PROPERTY_episodenumber, -1);
    }

    /**
     * Returns data in format S00E00. <br />
     * If only the episodenumber is available (which is often the case), it will only returns e.g. 'E00' [same for season only but I've
     * never seen this case]!
     */
    private static String getSeasonnumberAndEpisodenumber(final int seasonnumber, final int episodenumber) {
        String result;
        if (seasonnumber > -1 || episodenumber > -1) {
            result = "";
            if (seasonnumber > -1) {
                result += String.format(Locale.ROOT, "S%02d", seasonnumber);
            }
            if (episodenumber > -1) {
                result += String.format(Locale.ROOT, "E%02d", episodenumber);
            }
        } else {
            result = getCustomStringForEmptyTags(HOST_STATIC);
        }
        return result;
    }

    public static String getAdFreeText(final DownloadLink link) {
        final String ad_free_status = link.getStringProperty(PROPERTY_ad_free, "XX");
        return ad_free_status;
    }

    public static boolean allowNonProgrammedTelecastIDs() {
        /*
         * 2017-09-21: Set this to false for now - it might be useful in the future. In general it makes no sense to display non programmed
         * IDs as online.
         */
        return false;
    }

    /**
     * @return true: DownloadLink is a series false: DownloadLink is no series based on existing information.
     */
    private static boolean isSeries(final DownloadLink link) {
        /* For series */
        final String episodename = link.getStringProperty(PROPERTY_episodename);
        final int episodenumber = getEpisodeNumber(link);
        /* If we have an episodename and/or episodenumber, we have a series, category does not matter then */
        final boolean forceSeries = (!StringUtils.isEmpty(episodename) || episodenumber != -1);
        /* Check if we have a series or movie category */
        final int category_id = link.getIntegerProperty(PROPERTY_category_id, 0);
        final boolean belongsToCategoryMovie = (category_id == 0 || category_id == 1 || category_id == 3 || category_id == 7);
        final boolean isSeries = (forceSeries || !belongsToCategoryMovie);
        return isSeries;
    }

    /**
     * Returns the user-defined string to be used for empty filename-tags.Empty filename tag = needed data is not there (null) and
     * CUSTOM_FILENAME_EMPTY_TAG_STRING will be used instead.
     */
    @SuppressWarnings("deprecation")
    public static String getCustomStringForEmptyTags(final String host) {
        final SubConfiguration cfg = SubConfiguration.getConfig(host);
        final String customStringForEmptyTags = cfg.getStringProperty(CUSTOM_FILENAME_EMPTY_TAG_STRING, defaultCUSTOM_FILENAME_EMPTY_TAG_STRING);
        return customStringForEmptyTags;
    }

    /**
     * Returns username that is allowed to download this item. <br>
     * null = no username is determined yet.
     */
    private static String getDownloadableViaUsername(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_downloadable_via_username, null);
    }

    /**
     * Helps to get good looking original server-filenames, correct things, before corrected by correctData, in the end data/filename should
     * be 99% close to the originals. After all this does not have to be perfect as this data is only displayed to the user (e.g. a faked
     * 'original server filename' but NEVER used e.g. as a final filename.
     */
    private static String convertNormalDataToServer(String parameter) {
        /* Corrections with spaces */
        parameter = parameter.replace(" - ", "_");
        parameter = parameter.replace(" + ", "_");
        parameter = parameter.replace("(", "_");
        parameter = parameter.replace(")", "_");
        /* Correction via replaces */
        parameter = parameter.replace(" ", "_");
        parameter = parameter.replace("é", "_");
        parameter = parameter.replace("ä", "ae");
        parameter = parameter.replace("Ä", "Ae");
        parameter = parameter.replace("ö", "oe");
        parameter = parameter.replace("Ö", "Oe");
        parameter = parameter.replace("ü", "ue");
        parameter = parameter.replace("Ü", "Ue");
        parameter = parameter.replace("ß", "ss");
        /* Correction via replace remove */
        parameter = parameter.replace("+", "");
        parameter = parameter.replace("!", "");
        parameter = parameter.replace("'", "");
        parameter = parameter.replace("\"", "");
        parameter = parameter.replace("&", "");
        parameter = parameter.replace(",", "");
        parameter = parameter.replace(".", "");
        parameter = parameter.replace("?", "");
        /* Multiple underscores do never occur in server filenames --> Fix this here */
        final String[] underscores = new Regex(parameter, "(_{2,})").getColumn(0);
        if (underscores != null && underscores.length != 0) {
            for (final String underscoress : underscores) {
                parameter = parameter.replace(underscoress, "_");
            }
        }
        return parameter;
    }

    /* Helps to get good looking custom filenames out of server filenames */
    @SuppressWarnings("unused")
    private static String convertServerDataToNormal(String parameter) {
        parameter = parameter.replace("_", " ");
        parameter = parameter.replace("ae", "ä");
        parameter = parameter.replace("AE", "Ä");
        parameter = parameter.replace("Ae", "Ä");
        parameter = parameter.replace("oe", "ö");
        parameter = parameter.replace("OE", "Ö");
        parameter = parameter.replace("Oe", "Ö");
        parameter = parameter.replace("ue", "ü");
        parameter = parameter.replace("UE", "Ü");
        parameter = parameter.replace("Ue", "Ü");
        return parameter;
    }

    /* Correct characters of serverside encoding failures */
    private static String fixCharIssues(final String input) {
        String output = input;
        /* Part 1 */
        output = output.replace("ÃŸ", "ß");
        output = output.replace("Ã„", "Ä");
        output = output.replace("Ãœ", "Ü");
        output = output.replace("Ã–", "Ö");
        /* Part 2 */
        output = output.replace("Ã¶", "ö");
        output = output.replace("Ã¤", "ä");
        output = output.replace("Ã¼", "ü");
        // output = output.replace("Ã?", "");
        return output;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* Without account its not possible to download any link from this host. */
            return false;
        }
        final String account_username_from_which_url_was_added = getDownloadableViaUsername(link);
        if (account_username_from_which_url_was_added != null && !StringUtils.equals(account_username_from_which_url_was_added, account.getUser())) {
            /* Link is from another account -> Users can only download the items they "recorded" with their account. */
            return false;
        }
        return super.canHandle(link, account);
    }

    public static boolean isTypeTelecastIDOverview(final DownloadLink link) {
        return new Regex(link.getDownloadURL(), Pattern.compile(LINKTYPE_TELECAST_ID_RECORD_OVERVIEW, Pattern.CASE_INSENSITIVE)).matches();
    }

    public static boolean isTypeTelecastID(final DownloadLink link) {
        return new Regex(link.getDownloadURL(), Pattern.compile(LINKTYPE_TELECAST_ID, Pattern.CASE_INSENSITIVE)).matches();
    }

    public static boolean isTypeTelecastIDVideoArchiveStreaming(final DownloadLink link) {
        return new Regex(link.getDownloadURL(), Pattern.compile(LINKTYPE_TELECAST_ID_VIDEO_ARCHIVE_STREAMING, Pattern.CASE_INSENSITIVE)).matches();
    }

    public static boolean isTypeDirect(final DownloadLink link) {
        return new Regex(link.getDownloadURL(), Pattern.compile(LINKTYPE_DIRECT, Pattern.CASE_INSENSITIVE)).matches();
    }

    @Override
    public String buildExternalDownloadURL(final DownloadLink link, final PluginForHost buildForThisPlugin) {
        if (isTypeDirect(link)) {
            /*
             * Directurls can expire --> Return Archive-Downloadurl if URL is online and telecastID overview URL if status is unknown or
             * offline
             */
            if (link.isAvailable()) {
                return buildArchiveDownloadURL(link);
            } else {
                return buildNotYetRecordedOrOfflineDownloadURL(link);
            }
        } else {
            return link.getPluginPatternMatcher();
        }
    }

    /** Returns 'normal' save.tv downloadurl. */
    private String buildArchiveDownloadURL(final DownloadLink link) {
        final String telecastID = getTelecastId(link);
        return String.format("https://www.%s/STV/M/obj/archive/VideoArchiveDetails.cfm?TelecastId=%s", getHost(), telecastID);
    }

    /**
     * Returns downloadurl for non-yet recorded shows or, in some cases this is also suitable for records which are already offline --> Via
     * this URL the user will still get information via browser. <br />
     * LINKTYPE_RECORD_OVERVIEW
     */
    private String buildNotYetRecordedOrOfflineDownloadURL(final DownloadLink link) {
        final String telecastID = getTelecastId(link);
        return String.format("https://www.%s/STV/M/obj/TC/SendungsDetails.cfm?TelecastId=%s", this.getHost(), telecastID);
    }

    @Override
    public String getDescription() {
        return "JDownloader's Save.tv Plugin vereinfacht das Herunterladen aufgenommener Sendungen von save.tv.";
    }

    private final static String  defaultCUSTOM_FILENAME_MOVIES                    = "*quality* ¦ *videotitel* ¦ *produktionsjahr* ¦ *telecastid**endung*";
    private final static String  defaultCUSTOM_FILENAME_SERIES                    = "*serientitel* ¦ *quality* ¦ *episodennummer_und_staffelnummer* ¦ *episodenname* ¦ *telecastid*|*datum*|*endung*";
    private final static String  defaultCUSTOM_FILENAME_SEPARATION_MARK           = "+";
    private final static String  defaultCUSTOM_FILENAME_EMPTY_TAG_STRING          = "-";
    private final static String  defaultFORCE_ORIGINALFILENAME_SERIES             = "";
    private final static String  defaultFORCE_ORIGINALFILENAME_MOVIES             = "";
    public static final String   defaultCUSTOM_API_PARAMETERS_CRAWLER             = "";
    public static final boolean  defaultCRAWLER_ACTIVATE                          = false;
    public static final boolean  defaultCRAWLER_ONLY_ADD_NEW_IDS                  = false;
    public static final int      defaultCRAWLER_GRAB_TIMEFRAME_COUNT              = 0;
    public static final boolean  defaultCRAWLER_ENABLE_DIALOGS                    = true;
    public static final boolean  defaultSYNC_TOOLBAR_ENABLE_DIALOG                = true;
    public static final boolean  defaultCRAWLER_ENABLE_FAST_LINKCHECK             = true;
    private final static int     defaultADS_FREE_UNAVAILABLE_HOURS                = 24;
    private static final boolean defaultPROPERTY_PREFERADSFREE                    = true;
    private static final boolean defaultPROPERTY_USEORIGINALFILENAME              = false;
    private static final int     defaultSELECTED_VIDEO_FORMAT                     = 0;
    private static final String  defaultCUSTOM_DATE                               = "dd.MM.yyyy";
    public static final boolean  defaultUSEAPI                                    = true;
    private static final boolean defaultDELETE_TELECAST_ID_AFTER_DOWNLOAD         = false;
    private static final boolean defaultDELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS = false;
    private static final String  ACCOUNTTYPE_UNKNOWN                              = "Unbekanntes Paket";

    private void setConfigElements() {
        /* Crawler settings */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Archiv-Crawler Einstellungen:"));
        final ConfigEntry crawlerActivate = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SaveTv.CRAWLER_ACTIVATE, "Archiv-Crawler aktivieren?\r\nINFO: Fügt das komplette Archiv oder Teile davon beim Einfügen dieses Links ein:\r\n'https://www.save.tv/STV/M/obj/archive/VideoArchive.cfm\r\n").setDefaultValue(defaultCRAWLER_ACTIVATE);
        getConfig().addEntry(crawlerActivate);
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        final ConfigEntry crawlerAddNew = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SaveTv.CRAWLER_ONLY_ADD_NEW_IDS, "Nur <b>neue</b> abgeschlossene Aufnahmen crawlen?<br />JDownloader gleicht dein save.tv Archiv ab mit den Einträgen, die du bereits eingefügt hast und zeigt immer nur neue Einträge an!<br /><html><b>Wichtig:</b> JDownloader kann nicht wissen, welche Sendungen du bereits außerhalb von JDownloader geladen hast - nur, welche bereits in JDownloader eingefügt wurden!<br /></html>").setDefaultValue(defaultCRAWLER_ONLY_ADD_NEW_IDS).setEnabledCondidtion(crawlerActivate, true);
        getConfig().addEntry(crawlerAddNew);
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SPINNER, getPluginConfig(), SaveTv.CRAWLER_GRAB_TIMEFRAME_COUNT, "Nur Aufnahmen der letzten X Tage crawlen?\r\nAnzahl der Tage, die gecrawlt werden sollen [0 = komplettes Archiv]:", 0, 62, 1).setDefaultValue(defaultCRAWLER_GRAB_TIMEFRAME_COUNT).setEnabledCondidtion(crawlerAddNew, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SaveTv.CRAWLER_ENABLE_DIALOGS, "<html>Info Dialoge des Archiv-Crawlers aktivieren?<br />[Erscheinen nach dem Crawlen oder im Fehlerfall] </html>").setDefaultValue(defaultCRAWLER_ENABLE_DIALOGS).setEnabledCondidtion(crawlerActivate, true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SaveTv.SYNC_TOOLBAR_ENABLE_DIALOG, "<html>Info Dialog beim Klick auf den save.tv Toolbar-Button anzeigen?<br />[Weist darauf hin, dass dabei das komplette Videoarchiv gecrawlt wird] </html>").setDefaultValue(defaultSYNC_TOOLBAR_ENABLE_DIALOG));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        /* Format & Quality settings */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Format & Qualitäts-Einstellungen:"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_COMBOBOX_INDEX, getPluginConfig(), SELECTED_VIDEO_FORMAT, FORMATS, "Bevorzugtes Format (ist dieses nicht verfügbar, wird das beste verfügbare genommen):").setDefaultValue(defaultSELECTED_VIDEO_FORMAT));
        final ConfigEntry preferAdsFree = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SaveTv.PROPERTY_PREFERADSFREE, JDL.L("plugins.hoster.SaveTv.PreferAdFreeVideos", "Aufnahmen mit angewandter Schnittliste bevorzugen?")).setDefaultValue(defaultPROPERTY_PREFERADSFREE);
        getConfig().addEntry(preferAdsFree);
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SPINNER, getPluginConfig(), SaveTv.ADS_FREE_UNAVAILABLE_HOURS, "Download von Aufnahmen ohne Schnittliste erzwingen, sofern X Stunden nach Aufnahmedatum keine Schnittliste verfügbar ist? <html><b><br />[0 = nie erzwingen = ausschließlich werbefreie Aufnahmen herunterladen]</b></html>", 0, 720, 24).setDefaultValue(defaultADS_FREE_UNAVAILABLE_HOURS).setEnabledCondidtion(preferAdsFree, true));
        /* Filename settings */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Dateiname Einstellungen:"));
        final ConfigEntry origName = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SaveTv.PROPERTY_USEORIGINALFILENAME, JDL.L("plugins.hoster.SaveTv.UseOriginalFilename", "Original (Server) Dateinamen verwenden? <html><b>[Können sich vor Start des Downloads ändern!]</b></html>")).setDefaultValue(defaultPROPERTY_USEORIGINALFILENAME);
        getConfig().addEntry(origName);
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_DATE, "Setze das Datumsformat:\r\nWichtige Information dazu:\r\nDas Datum erscheint im angegebenen Format im Dateinamen, allerdings nur,\r\nwenn man das *datum* Tag auch verwendet (siehe Benutzerdefinierte Dateinamen für Filme und Serien unten)\r\nBeispiel: dd.MM.yyyy HH:MM").setDefaultValue(defaultCUSTOM_DATE).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        /* General settings description */
        final StringBuilder description_custom_filenames_howto = new StringBuilder();
        description_custom_filenames_howto.append("<html><b>Erklärung zur Nutzung eigener Dateinamen:</b><br />");
        description_custom_filenames_howto.append("Eigene Dateinamen lassen sich unten über ein Tag-System (siehe weiter unten) nutzen.<br />");
        description_custom_filenames_howto.append("Das bedeutet, dass man die Struktur seiner gewünschten Dateinamen definieren kann.<br />");
        description_custom_filenames_howto.append("Dabei hat man Tags wie z.B. *telecastid*, die dann durch Daten ersetzt werden.<br />");
        description_custom_filenames_howto.append("Wichtig dabei ist, dass Tags immer mit einem Stern starten und enden.<br />");
        description_custom_filenames_howto.append("Man darf nichts zwischen ein Tag schreiben z.B. *-telecastid-*, da das<br />");
        description_custom_filenames_howto.append("zu unschönen Dateinamen führt und das Tag nicht die Daten ersetzt werden kann.<br />");
        description_custom_filenames_howto.append("Wenn man die Tags trennen will muss man die anderen Zeichen zwischen Tags<br />");
        description_custom_filenames_howto.append("z.B. '-*telecastid*-*endung*' -> Der Dateiname würde dann in etwa so aussehen: '-7573789-.mp4' (ohne die '')<br />");
        description_custom_filenames_howto.append("<b>WICHTIG:</b> Tags, zu denen die Daten fehlen, werden standardmäßig durch '-' (Bindestrich) ersetzt!<br />");
        description_custom_filenames_howto.append("Fehlen z.B. die Daten zu *genre*, steht statt statt dem Genre dann ein Bindestrich ('-') an dieser Stelle im Dateinamen.<br />");
        description_custom_filenames_howto.append("Gut zu wissen: Statt dem Bindestrich lässt sich hierfür unten auch ein anderes Zeichen bzw. Zeichenfolge definieren.<br />");
        description_custom_filenames_howto.append("Außerdem: Für Filme und Serien gibt es unterschiedliche Tags.<br />");
        description_custom_filenames_howto.append("Kaputtmachen kannst du mit den Einstellungen prinzipiell nichts also probiere es einfach aus ;)<br />");
        description_custom_filenames_howto.append("<b>Tipp:</b> Die Save.tv Plugin Einstellungen lassen sich rechts oben wieder auf ihre Standardwerte zurücksetzen!<br />");
        description_custom_filenames_howto.append("</html>");
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, description_custom_filenames_howto.toString()).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        final StringBuilder description_custom_filenames_tags = new StringBuilder();
        description_custom_filenames_tags.append("<html><b>Erklärung der <u>allgemein</u> verfügbaren Tags:</b><br />");
        description_custom_filenames_tags.append("*server_dateiname* = Original Dateiname (ohne Dateiendung)<br />");
        description_custom_filenames_tags.append("*username* = Benutzername<br />");
        description_custom_filenames_tags.append("*datum* = Datum der Ausstrahlung der aufgenommenen Sendung<br />[Erscheint im oben definierten Format, wird von der save.tv Seite ausgelesen]<br />");
        description_custom_filenames_tags.append("*genre* = Das Genre<br />");
        description_custom_filenames_tags.append("*produktionsland* = Name des Produktionslandes<br />");
        description_custom_filenames_tags.append("*produktionsland_kurz* = Name des Produktionslandes nach ISO 3166 Standard z.B. 'DE' statt 'Deutschland'<br />");
        description_custom_filenames_tags.append("*produktionsjahr* = Produktionsjahr<br />");
        description_custom_filenames_tags.append("*sendername* = Name des TV-Senders auf dem die Sendung ausgestrahlt wurde");
        description_custom_filenames_tags.append("*kategorie* = Kategorie, siehe telecast-ID Seite<br />");
        description_custom_filenames_tags.append("*quality* = Qualitätsstufe des Downloads - Entspricht den Werten 'LQ', 'HQ', 'HD' oder 'XX' für den unbekannten Status<br />");
        description_custom_filenames_tags.append("*werbefrei* = Schnittliste-Status des Downloads - Entspricht den Werten 'true', 'false' oder 'XX' für den unbekannten Status<br />");
        description_custom_filenames_tags.append("*zufallszahl* = Eine vierstellige Zufallszahl<br />[Nützlich um Dateinamenkollisionen zu vermeiden]<br />");
        description_custom_filenames_tags.append("*telecastid* = Die id, die in jedem save.tv Link steht: TelecastID=XXXXXXX<br />[Nützlich um Dateinamenkollisionen zu vermeiden]<br />");
        description_custom_filenames_tags.append("*endung* = Die Dateiendung, in diesem Fall immer '.mp4'<br />");
        /* Description of tags for movies ONLY */
        description_custom_filenames_tags.append("<b>Erklärung der <u>nur für Filme</u> verfügbaren Tags:</b><br />");
        description_custom_filenames_tags.append("*videotitel* = Name des Videos ohne Dateiendung<br />");
        /* Description of tags for series ONLY */
        description_custom_filenames_tags.append("<b>Erklärung der <u>nur für Serien</u> verfügbaren Tags:</b><br />");
        description_custom_filenames_tags.append("*serientitel* = Name der Serie<br />");
        description_custom_filenames_tags.append("*episodenname* = Name der Episode<br />");
        description_custom_filenames_tags.append("*staffelnummer* = Staffelnummer<br />");
        description_custom_filenames_tags.append("*episodennummer* = Episodennummer<br />");
        description_custom_filenames_tags.append("*episodennummer_und_staffelnummer* = Episodennummer mit Staffelnummer z.B. 'S01E03'<br />");
        description_custom_filenames_tags.append("</html>");
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, description_custom_filenames_tags.toString()).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        /* Filename settings for movies */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_FILENAME_MOVIES, "Eigener Dateiname für Filme/Shows:").setDefaultValue(defaultCUSTOM_FILENAME_MOVIES).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        /* Filename settings for series */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_FILENAME_SERIES, "Eigener Dateiname für Serien:").setDefaultValue(defaultCUSTOM_FILENAME_SERIES).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        /* Advanced settings */
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Erweiterte Einstellungen:\r\n<html><p style=\"color:#F62817\"><b>Warnung: Ändere die folgenden Einstellungen nur, wenn du weißt was du tust!\r\nMit einem Klick auf den gelben Pfeil rechts oben kannst du jederzeit zu den Standardeinstellungen zurück.</b></p></html>"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Soll die telecastID in bestimmten Situationen aus dem save.tv Archiv gelöscht werden?\r\n<html><p style=\"color:#F62817\"><b>Warnung:</b> Gelöschte telecastIDs können nicht wiederhergestellt werden!</p></html>"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), DELETE_TELECAST_ID_AFTER_DOWNLOAD, "Erfolgreich heruntergeladene telecastIDs aus dem save.tv Archiv löschen?").setDefaultValue(defaultDELETE_TELECAST_ID_AFTER_DOWNLOAD));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), DELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS, "Falls Datei bereits auf der Festplatte existiert, telecastIDs aus dem save.tv Archiv löschen?").setDefaultValue(defaultDELETE_TELECAST_ID_IF_FILE_ALREADY_EXISTS));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_API_PARAMETERS_CRAWLER, "Crawler: eigene API Parameter definieren (alles außer 'limit,fields,nopagingheader,paging,offset') [urlEncoded]:\r\nBeispiel: 'tags=record:manual&fsk=6'\r\nWeitere Informationen siehe: api.save.tv/v3/docs/index#!/Records_|_get/Records_Get<html><p style=\"color:#F62817\"><b>Warnung:</b> Falsche Werte können den Crawler funktionsunfähig machen und andere Crawler-Einstellungen beeinflussen (ggf. Einstellungen zurücksetzen)!</p></html>").setDefaultValue(defaultCUSTOM_API_PARAMETERS_CRAWLER));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_FILENAME_SEPARATION_MARK, "Trennzeichen als Ersatz für '/'  (da ungültig in Dateinamen):").setDefaultValue(defaultCUSTOM_FILENAME_SEPARATION_MARK).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_FILENAME_EMPTY_TAG_STRING, "Zeichen, mit dem Tags ersetzt werden sollen, deren Daten fehlen:").setDefaultValue(defaultCUSTOM_FILENAME_EMPTY_TAG_STRING).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        final StringBuilder sbmore = new StringBuilder();
        sbmore.append("<html>Definiere Filme oder Serien, für die trotz obiger Einstellungen die Originaldateinamen<br />");
        sbmore.append("verwendet werden sollen.<br />");
        sbmore.append("Manche mehrteiligen Filme haben dieselben Titel und bei manchen Serien fehlen die Episodennamen,<br />");
        sbmore.append("wodurch sie alle dieselben Dateinamen bekommen -> JDownloader denkt es seien Duplikate/Mirrors und lädt nur<br />");
        sbmore.append("einen der scheinbar gleichen Dateien.<br />");
        sbmore.append("Um dies zu verhindern, kann man in den Eingabefeldern Namen solcher Filme/Serien eintragen,<br />");
        sbmore.append("für die trotz obiger Einstellungen der Original Dateiname verwendet werden soll.<br />");
        sbmore.append("Beispiel: 'serienname 1|serienname 2|usw.' (ohne die '')<br />");
        sbmore.append("Die Eingabe erfolgt als RegEx. Wer nicht weiß was das ist -> Google</html>");
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, sbmore.toString()).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), FORCE_ORIGINALFILENAME_SERIES, JDL.L("plugins.hoster.savetv.forceoriginalnameforspecifiedseries", "Original Dateinamen für folgende Serien erzwingen [Eingabe erfolgt in RegEx]:")).setDefaultValue(defaultFORCE_ORIGINALFILENAME_SERIES).setEnabledCondidtion(origName, false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), FORCE_ORIGINALFILENAME_MOVIES, JDL.L("plugins.hoster.savetv.forcefilenameforspecifiedmovies", "Original Dateinamen für folgende Filme erzwingen [Eingabe erfolgt in RegEx]:")).setDefaultValue(defaultFORCE_ORIGINALFILENAME_MOVIES).setEnabledCondidtion(origName, false));
    }

    /** Number of seconds after which info dialogs (see showAutoCloseDialog) close themselves automatically. */
    public static final int DIALOG_AUTO_CLOSE_SECONDS = 180;

    /** Shows a purely informational dialog which closes itself automatically after DIALOG_AUTO_CLOSE_SECONDS. */
    public static void showAutoCloseDialog(final String title, final String message) {
        showAutoCloseDialog(title, message, DIALOG_AUTO_CLOSE_SECONDS);
    }

    /** Shows a purely informational dialog which closes itself automatically after the given number of seconds. */
    public static void showAutoCloseDialog(final String title, final String message, final int timeoutSeconds) {
        final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | UIOManager.BUTTONS_HIDE_CANCEL, title, message);
        dialog.setTimeout(timeoutSeconds * 1000);
        UIOManager.I().show(ConfirmDialogInterface.class, dialog);
    }

    public static String getMessageEnd() {
        String message = "";
        message += "\r\n\r\n";
        message += "Falls du Fehler findest oder Fragen hast, melde dich jederzeit gerne in unserem Supportforum:\r\nhttps://board.jdownloader.org/\r\n";
        message += "\r\n";
        message += "Dieses Fenster wird nur einmal angezeigt.\r\nAlle wichtigen Informationen findest du ebenfalls in den save.tv Plugin Einstellungen.\r\n";
        message += "\r\n";
        message += "- Das JDownloader Team wünscht weiterhin viel Spaß mit JDownloader und save.tv! -";
        return message;
    }

    @SuppressWarnings("deprecation")
    private void checkAccountNeededDialog() {
        synchronized (LOCK) {
            SubConfiguration config = null;
            try {
                config = getPluginConfig();
                if (config.getBooleanProperty("accNeededShown", Boolean.FALSE) == false) {
                    if (config.getProperty("accNeededShown2") == null) {
                        showAccNeededDialog();
                    } else {
                        config = null;
                    }
                } else {
                    config = null;
                }
            } catch (final Throwable e) {
            } finally {
                if (config != null) {
                    config.setProperty("accNeededShown", Boolean.TRUE);
                    config.setProperty("accNeededShown2", "shown");
                    config.save();
                }
            }
        }
    }

    private void showAccNeededDialog() {
        try {
            String message = "Hallo lieber save.tv Nutzer.\r\n";
            message += "Um über JDownloader Videos aus deinem save.tv Archiv herunterladen zu können, musst du\r\nzunächst deinen save.tv Account in JDownloader eintragen.";
            message += "\r\n";
            message += "Das geht unter:\r\n";
            message += "Einstellungen -> Accountverwaltung -> Hinzufügen -> save.tv\r\n";
            message += "\r\n";
            message += "Sobald du deinen Account eingetragen hast, kannst du aus deinem save.tv Archiv\r\n";
            message += "Links dieses Formats in JDownloader einfügen und herunterladen:\r\n";
            message += "https://www.save.tv/STV/M/obj/archive/VideoArchive.cfm";
            message += getMessageEnd();
            showAutoCloseDialog("save.tv", message);
        } catch (Throwable e) {
            logger.log(e);
        }
    }

    @SuppressWarnings("deprecation")
    private void checkFeatureDialogAll() {
        SubConfiguration config = null;
        try {
            config = getPluginConfig();
            if (config.getBooleanProperty("featuredialog_all_Shown", Boolean.FALSE) == false) {
                if (config.getProperty("featuredialog_all_Shown2") == null) {
                    showFeatureDialogAll();
                } else {
                    config = null;
                }
            } else {
                config = null;
            }
        } catch (final Throwable e) {
        } finally {
            if (config != null) {
                config.setProperty("featuredialog_all_Shown", Boolean.TRUE);
                config.setProperty("featuredialog_all_Shown2", "shown");
                config.save();
            }
        }
    }

    private void showFeatureDialogAll() {
        try {
            String message = "Hallo lieber save.tv Nutzer/liebe save.tv Nutzerin\r\n";
            message += "Du bist gerade dabei, deine erste Save.tv Aufnahme mit JDownloader herunterzuladen.\r\n";
            message += "Das save.tv Plugin bietet folgende Features:\r\n";
            message += "- Automatisierter Download von save.tv Links (telecast-IDs)\r\n";
            message += "- Laden des kompletten save.tv Videoarchivs über wenige Klicks\r\n";
            message += "--> Oder wahlweise nur alle Links der letzten X Tage\r\n";
            message += "- Einfügen aller Aufnahmen des Save.tv Videoarchivs über den Save.tv Button in der Toolbar oben\r\n";
            message += "- Benutzerdefinierte Dateinamen über ein Tag-System mit vielen Möglichkeiten\r\n";
            message += "- Alles unter Beachtung der Schnittlisten-Einstellungen und des Formats\r\n";
            message += "- Und vieles mehr...\r\n";
            message += "\r\n";
            message += "Diese Einstellungen sind verfügbar unter:\r\nEinstellungen -> Plugin Einstellungen -> save.tv";
            message += getMessageEnd();
            showAutoCloseDialog("save.tv", message);
        } catch (Throwable e) {
            logger.log(e);
        }
    }

    @Override
    public void extendAccountSettingsPanel(Account account, PluginConfigPanelNG panel) {
        super.extendAccountSettingsPanel(account, panel);
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return;
        }
        final String accType = account.getStringProperty(PROPERTY_acc_type, ACCOUNTTYPE_UNKNOWN);
        final String accUsername = account.getStringProperty(PROPERTY_acc_username, "?");
        String acc_expire = "Unbekannt";
        final String acc_package = account.getStringProperty(PROPERTY_acc_package, "?");
        final String acc_price = account.getStringProperty(PROPERTY_acc_price, "?");
        final String acc_runtime = account.getStringProperty(PROPERTY_acc_runtime, "?");
        final String acc_count_archive_entries = account.getStringProperty(PROPERTY_acc_count_archive_entries, "?");
        final String acc_count_telecast_ids = account.getStringProperty(PROPERTY_acc_count_telecast_ids, "?");
        final String user_lastcrawl_newlinks_date;
        final String user_lastcrawl_date;
        final long time_last_crawl_ended_newlinks = account.getLongProperty(CRAWLER_PROPERTY_LASTCRAWL_NEWLINKS, 0);
        final long time_last_crawl_ended = account.getLongProperty(CRAWLER_PROPERTY_LASTCRAWL, 0);
        final String maxchunks;
        if (ACCOUNT_PREMIUM_MAXCHUNKS == 0) {
            maxchunks = "20";
        } else if (ACCOUNT_PREMIUM_MAXCHUNKS < 1) {
            maxchunks = Integer.toString(-ACCOUNT_PREMIUM_MAXCHUNKS);
        } else {
            maxchunks = Integer.toString(ACCOUNT_PREMIUM_MAXCHUNKS);
        }
        final SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy 'um' HH:mm 'Uhr'");
        if (time_last_crawl_ended_newlinks > 0 && time_last_crawl_ended > 0) {
            user_lastcrawl_date = formatter.format(time_last_crawl_ended);
            user_lastcrawl_newlinks_date = formatter.format(time_last_crawl_ended_newlinks);
        } else {
            user_lastcrawl_date = "Nie";
            user_lastcrawl_newlinks_date = "Nie";
        }
        if (account.getAccountInfo().getValidUntil() > 0) {
            acc_expire = formatter.format(account.getAccountInfo().getValidUntil());
        }
        panel.addStringPair(_GUI.T.lit_username(), accUsername);
        panel.addStringPair(_GUI.T.lit_account_type(), accType);
        panel.addStringPair(_GUI.T.lit_package(), acc_package);
        panel.addStringPair(_GUI.T.lit_runtime(), acc_runtime);
        panel.addStringPair(_GUI.T.lit_expire_date(), acc_expire);
        panel.addStringPair(_GUI.T.lit_price(), acc_price);
        panel.addStringPair("Sendungen im Archiv:", acc_count_archive_entries);
        panel.addStringPair("Ladbare Sendungen im Archiv (telecast-IDs):", acc_count_telecast_ids);
        panel.addStringPair("Datum des letzten erfolgreichen Crawlvorganges: ", user_lastcrawl_date);
        panel.addStringPair("Zuletzt erfolgreich telecastIDs per Crawler hinzugefügt:", user_lastcrawl_newlinks_date);
        panel.addHeader(_GUI.T.lit_download(), new AbstractIcon(IconKey.ICON_DOWNLOAD, 18));
        panel.addStringPair(_GUI.T.lit_max_simultanous_downloads(), "20");
        panel.addStringPair(_GUI.T.lit_max_chunks_per_link(), maxchunks);
        panel.addStringPair(_GUI.T.lit_interrupted_downloads_are_resumable(), _JDT.T.literally_yes());
    }

    @Override
    public boolean allowHandle(final DownloadLink link, final PluginForHost plugin) {
        /*
         * Do not allow multihost plugins to handle items from this plugin since every downloadlink is bound to the users' save.tv account
         * so it is impossible for other users to access the same content via this contentID.
         */
        return link.getHost().equalsIgnoreCase(plugin.getHost());
    }
}