package jd.plugins.hoster;

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
//along with this program.  If not, see <http://www.gnu.org/licenses/>.;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasicSpecialFilejoker;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.reconnect.ipcheck.BalancedWebIPCheck;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51272 $", interfaceVersion = 3, names = {}, urls = {})
public class NovaFileCom extends XFileSharingProBasicSpecialFilejoker {
    public NovaFileCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
        this.setConfigElements();
        /*
         * 2020-01-16: Without waiting between downloads, users are more likely to experience error-response 503 and will get more captchas
         * in premium mode. Captchas in premium mode will always happen - even with long delays between starting downloads!
         */
        this.setStartIntervall(10000l);
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-08-21: All checked and set <br />
     * captchatype-info: 2019-08-21: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "novafile.org", "novafile.com" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    protected URL_TYPE getURLType(final String url) {
        /* 2023-05-18: Legacy: Those URLs are now processed by separate crawler plugin NfileCc. */
        if (url != null && url.matches("(?i)https?://[^/]*nfile\\.cc/([A-Za-z0-9]+)$")) {
            return URL_TYPE.SHORT;
        } else {
            return super.getURLType(url);
        }
    }

    @Override
    protected boolean supportsShortURLs() {
        /* 2023-05-18: Legacy: Those URLs are now processed by separate crawler plugin NfileCc. */
        return true;
    }

    protected String getFUID(final String url, URL_TYPE type) {
        if (url != null && type != null) {
            try {
                switch (type) {
                case SHORT:
                    return new Regex(new URL(url).getPath(), "/([A-Za-z0-9]+)").getMatch(0);
                default:
                    return super.getFUID(url, type);
                }
            } catch (MalformedURLException e) {
                logger.log(e);
            }
        }
        return null;
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String getAnnotationPatternPart() {
        return "/(?:embed-|file/)?[a-z0-9]{8,}(?:/[^/]+(?:\\.html)?)?";
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + getAnnotationPatternPart());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return false;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 1;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        /* 2019-08-21: Special */
        super.scanInfo(html, fileInfo);
        final String filename = new Regex(html, "class=\"name\">([^<>\"]+)<").getMatch(0);
        if (filename != null) {
            fileInfo[0] = filename;
        }
        final String filesize = new Regex(html, "class=\"size\">([^<>\"]+)<").getMatch(0);
        if (filesize != null) {
            fileInfo[1] = filesize;
        }
        return fileInfo;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2019-08-21: Special */
        return false;
    }

    @Override
    protected String regexWaittime(Browser br) {
        /* 2019-08-21: Special */
        String wait = new Regex(br.getRequest().getHtmlCode(), "class=\"alert\\-success[^\"]*\">(\\d+)</span>").getMatch(0);
        if (StringUtils.isEmpty(wait)) {
            wait = super.regexWaittime(br);
        }
        return wait;
    }

    @Override
    protected void checkErrors(final Browser br, final String correctedBR, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        /* 2019-08-21: Special */
        super.checkErrors(br, correctedBR, link, account, checkAll);
        if (new Regex(correctedBR, "You have reached the daily download limit").matches()) {
            /* It does not tell us how long we have to wait */
            if (account != null) {
                throw new AccountUnavailableException("You have reached the daily download limit", FREE_RECONNECTWAIT_DEFAULT);
            } else {
                this.setDownloadStarted(link, false);
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "You have reached the daily download limit", FREE_RECONNECTWAIT_DEFAULT);
            }
        } else if (new Regex(correctedBR, "We have detected an unusual activity from multiple IPs using your account|Your account has been temporarily suspended|Please check your email and follow the link that has been sent to you to reactivate").matches()) {
            /* 2020-05-19: E.g. accountsharing / usage of VPN */
            throw new AccountUnavailableException("Your account has been temporarily suspended", 10 * 60 * 1000l);
        }
    }

    @Override
    public void checkServerErrors(final Browser br, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        /* 2020-05-19: Special */
        super.checkServerErrors(br, link, account);
        if (new Regex(br.getRequest().getHtmlCode().trim(), ">\\s*Wrong IP").matches()) {
            /*
             * 2020-05-19: May happen when user uses a VPN - this can then especially happen in premium mode for all downloads (via API?!).
             */
            /* jdlog://1827915302851/ */
            if (this.internal_useAPIZeusCloudManager(link, account)) {
                /* 2020-05-20: Workaround attempt --> Try via website next time */
                this.tempDisableAPI(account, "Server error: 'Wrong IP' - attempting website workaround");
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error: 'Wrong IP'", 2 * 60 * 60 * 1000l);
            }
        }
    }

    @Override
    protected String getDllink(final DownloadLink link, final Account account, final Browser br, final String src) {
        /* 2019-08-21: Special */
        final String dllink = super.getDllink(link, account, br, src);
        if (!StringUtils.isEmpty(dllink)) {
            /*
             * 2019-08-21: This is kind of a workaround: If we found a downloadlink we will soon start the download --> Save timestamp so
             * our special reconnect handling knows that the limit is reached now and user will not have to solve extra captchas!
             */
            setDownloadStarted(link, true);
        }
        return dllink;
    }

    /* *************************** SPECIAL RECONNECT STUFF STARTS HERE *************************** */
    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String dllink = checkDirectLink(link, account);
        if (dllink != null) {
            /*
             * Found directurl? Start download! This is there to prevent our experimental reconnect handling from triggering when the user
             * starts downloads which have been started before and thus have a working saved directurl stored!
             */
            super.handleDownload(link, account, dllink, null);
        } else {
            /* No directurl? Check for saved reconnect-limit and if there is none, continue via template-handling! */
            currentIP.set(new BalancedWebIPCheck(null).getExternalIP().getIP());
            synchronized (CTRLLOCK) {
                /* Load list of saved IPs + timestamp of last download */
                final Object lastdownloadmap = this.getPluginConfig().getProperty(PROPERTY_LASTDOWNLOAD);
                if (lastdownloadmap != null && lastdownloadmap instanceof Map && blockedIPsMap.isEmpty()) {
                    blockedIPsMap = (Map<String, Long>) lastdownloadmap;
                }
            }
            /**
             * Experimental reconnect handling to prevent having to enter a captcha just to see that a limit has been reached!
             */
            if (this.getPluginConfig().getBooleanProperty(EXPERIMENTALHANDLING, default_eh)) {
                /*
                 * If the user starts a download in free or free-acount mode the waittime is on his IP. This also affects free accounts if
                 * he tries to start more downloads via free accounts afterwards BUT nontheless the limit is only on his IP so he CAN
                 * download using the same free accounts after performing a reconnect!
                 */
                long lastdownload = getPluginSavedLastDownloadTimestamp();
                long passedTimeSinceLastDl = System.currentTimeMillis() - lastdownload;
                if (passedTimeSinceLastDl < FREE_RECONNECTWAIT_DEFAULT) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, FREE_RECONNECTWAIT_DEFAULT - passedTimeSinceLastDl);
                }
            }
            super.doFree(link, account);
        }
    }

    private static final long              FREE_RECONNECTWAIT_DEFAULT = 45 * 60 * 1000L;
    private final String                   EXPERIMENTALHANDLING       = "EXPERIMENTALHANDLING";
    private static AtomicReference<String> currentIP                  = new AtomicReference<String>();
    private static Map<String, Long>       blockedIPsMap              = new HashMap<String, Long>();
    private static Object                  CTRLLOCK                   = new Object();
    private static final String            PROPERTY_LASTDOWNLOAD      = "NOVAFILE_lastdownload_timestamp";

    @SuppressWarnings("deprecation")
    private void setDownloadStarted(final DownloadLink dl, final boolean startedNow) {
        synchronized (CTRLLOCK) {
            try {
                final String currentIP = NovaFileCom.currentIP.get();
                if (currentIP != null) {
                    if (startedNow) {
                        blockedIPsMap.put(currentIP, System.currentTimeMillis());
                    } else {
                        blockedIPsMap.put(currentIP, System.currentTimeMillis() - (FREE_RECONNECTWAIT_DEFAULT + 30000));
                    }
                    getPluginConfig().setProperty(PROPERTY_LASTDOWNLOAD, blockedIPsMap);
                }
            } catch (final Exception ignore) {
                logger.warning("Error happened while trying to save download_started_timestamp");
                ignore.printStackTrace();
            }
        }
    }

    private long getPluginSavedLastDownloadTimestamp() {
        long lastdownload = 0;
        synchronized (blockedIPsMap) {
            final Iterator<Entry<String, Long>> it = blockedIPsMap.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Long> ipentry = it.next();
                final String ip = ipentry.getKey();
                final long timestamp = ipentry.getValue();
                if (System.currentTimeMillis() - timestamp >= FREE_RECONNECTWAIT_DEFAULT) {
                    /* Remove old entries */
                    it.remove();
                }
                if (ip.equals(currentIP.get())) {
                    lastdownload = timestamp;
                }
            }
        }
        return lastdownload;
    }

    private final boolean default_eh = false;

    public void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), EXPERIMENTALHANDLING, "Activate reconnect workaround for freeusers: Prevents having to enter additional captchas in between downloads.").setDefaultValue(default_eh));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), XFileSharingProBasicSpecialFilejoker.PROPERTY_SETTING_USE_API, "Use API? (Recommended!)\r\nIf turned off, website will be used and you might have to enter captchas even in premium mode!").setDefaultValue(default_PROPERTY_SETTING_USE_API));
    }

    /* *************************** SPECIAL API STUFF STARTS HERE *************************** */
    @Override
    protected boolean useAPIZeusCloudManager(final Account account) {
        /*
         * 2020-05-20: API would always return wait times for free accounts --> Do not use API for free account downloads anymore Example
         * json: {"file_size":"100000000","file_name":"10Mo.dat","file_code":"xxxxxxxxxxxx",
         * "message":"You have to wait 5 hours, 56 minutes, 10 seconds until the next download becomes available."}
         */
        final boolean allow_api_only_usage = true;
        final boolean setting_use_api = this.getPluginConfig().getBooleanProperty(XFileSharingProBasicSpecialFilejoker.PROPERTY_SETTING_USE_API, default_PROPERTY_SETTING_USE_API);
        if (!allow_api_only_usage) {
            /* API usage is internally disabled. */
            return false;
        } else if (account == null) {
            return setting_use_api;
        } else if (account.getType() == AccountType.FREE) {
            /* 2020-05-20: API usage is not allowed anymore for free accounts! */
            return false;
        } else {
            /* 2020-05-20: API usage is still allowed for premium accounts. */
            return setting_use_api;
        }
    }

    @Override
    protected boolean tryAPILoginInWebsiteMode(final Account account) {
        /* 2020-05-20: Verified and working */
        return true;
    }

    @Override
    protected boolean tryAPILoginInWebsiteMode_get_account_info_from_api(final Account account) {
        /*
         * 2023-11-22: Only use this for free accounts as their API returns invalid 'trafficleft' values for PREMIUM accounts! Seems like
         * this field is simply not updated anymore!
         */
        if (account.getType() == AccountType.PREMIUM) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    protected String getRelativeAPIBaseAPIZeusCloudManager() {
        return "/napi";
    }

    @Override
    protected String getRelativeAPILoginParamsFormatAPIZeusCloudManager() {
        return "?op=login&login=%s&pass=%s";
    }
}