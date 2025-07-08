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
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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

@HostPlugin(revision = "$Revision: 51188 $", interfaceVersion = 3, names = {}, urls = {})
public class OneHundretSixteenPanXyz extends PluginForHost {
    public OneHundretSixteenPanXyz(PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium("https://www." + getHost() + "/vip");
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/about";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "116pan.xyz" }); // formerly known as 116pan.com
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/f/([A-Za-z0-9]{6,})");
        }
        return ret.toArray(new String[0]);
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> props = getPageProps(br, link, null);
        final Map<String, Object> file = (Map<String, Object>) props.get("file");
        final String title = file.get("file_name").toString();
        final String file_extension = file.get("file_extension").toString();
        final Number file_size = (Number) file.get("file_size");
        link.setFinalFileName(title + "." + file_extension);
        if (file_size != null) {
            link.setVerifiedFileSize(file_size.longValue());
        }
        // file.get("vipfile");
        final String internal_file_id = file.get("file_id").toString();
        link.setProperty("internal_file_id", internal_file_id);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = "directurl_" + (account != null ? account.getType().getLabel() : null);
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            if (account == null || account.getType() != AccountType.PREMIUM) {
                /*
                 * 2025-07-07: Free download looks to be broken -> Returns error even when correct captcha is entered:
                 * {"message":"Unauthenticated."}
                 */
                throw new AccountRequiredException("Premium account required");
            }
            final String internal_file_id = link.getStringProperty("internal_file_id");
            if (internal_file_id == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String code = getCaptchaCode("/captcha?" + System.currentTimeMillis(), link);
            final Random random = new Random();
            final String[] commonResolutions = { "1920x1080", "1366x768", "1440x900", "1536x864", "1280x720", "1024x768", "1680x1050", "1600x900", "1280x1024", "1920x1200", "2560x1440", "3840x2160", "1280x800", "1152x864", "1024x600", "1400x1050", "1280x960", "1600x1200", "2560x1600", "3440x1440", "2560x1080", "1920x1440", "1280x768", "1360x768", "1440x1080" };
            final String randomResolution = commonResolutions[random.nextInt(commonResolutions.length)];
            final Map<String, Object> postdata = new HashMap<String, Object>();
            final int rand_x = random.nextInt(100);
            final int rand_y = random.nextInt(100);
            postdata.put("captcha", code);
            postdata.put("click_pos", rand_x + "," + rand_y);
            postdata.put("ref", "");
            postdata.put("screen", randomResolution);
            postdata.put("type", "normal");
            br.postPageRaw("/f/" + internal_file_id + "/generate-download", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> data = this.checkErrorsWebapi(br, link, account);
            /* TODO: Add generic error handling and check for wrong captcha */
            dllink = br.getRegex("").getMatch(0);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                }
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private Map<String, Object> login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String url_relative_dashboard = "/dashboard";
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return null;
                }
                br.getPage("https://www." + this.getHost() + url_relative_dashboard);
                /* Redirects to "/login" if we are not logged in. */
                if (StringUtils.containsIgnoreCase(br.getURL(), url_relative_dashboard)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return this.getPageProps(br, null, account);
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + this.getHost() + "/login");
            final Map<String, Object> props_before_login = checkErrorsWebapi(br, null, account);
            final String x_inertia_version = props_before_login.get("version").toString();
            final String csrftoken = br.getRegex("name=\"csrf-token\" content=\"([^\"]+)").getMatch(0);
            if (csrftoken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String code = getCaptchaCode("/captcha/default?" + System.currentTimeMillis(), this.getDownloadLink());
            if (!code.matches("\\d+")) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Invalid captcha format: Only numbers are allowed");
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("captcha", code);
            postdata.put("login", account.getUser());
            postdata.put("password", account.getPass());
            postdata.put("remember", true);
            // br.setAllowedResponseCodes(419);
            br.getHeaders().put("Content-Type", "application/json");
            br.getHeaders().put("Origin", "https://www.116pan.xyz");
            br.getHeaders().put("x-csrf-token", csrftoken);
            /* 2025-07-07: Browser sends true, we use false */
            br.getHeaders().put("x-inertia", "true");
            br.getHeaders().put("x-inertia-version", x_inertia_version);
            br.getHeaders().put("x-requested-with", "XMLHttpRequest");
            /* 2025-07-07: Website sends x-csrf-token header twice with different values */
            // br.getHeaders().put("x-xsrf-token", "");
            // br.setAllowedResponseCodes(409);
            br.postPageRaw("/login", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> props_after_login = checkErrorsWebapi(br, null, account);
            final String errorMsg = (String) props_after_login.get("captchaError");
            if (errorMsg != null) {
                /* Wrong captcha or wrong login credentials */
                if (errorMsg.equalsIgnoreCase("验证码错误，请重新输入")) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                } else {
                    throw new AccountInvalidException(errorMsg);
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return props_after_login;
        }
    }

    /** Find json in html and return map "props" in parsed json. */
    private Map<String, Object> getPageProps(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        String json = br.getRegex("data-page=\"([^\"]+)").getMatch(0);
        json = Encoding.htmlOnlyDecode(json);
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        return (Map<String, Object>) entries.get("props");
    }

    private Map<String, Object> checkErrorsWebapi(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        /* Wait milliseconds for unknown/generic errors */
        final long waitmillis = 60 * 1000;
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String errortext = "Invalid Web-API response";
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errortext, waitmillis);
            } else {
                throw new AccountUnavailableException(errortext, waitmillis);
            }
        }
        final Map<String, Object> props = (Map<String, Object>) entries.get("props");
        // TODO: Implement error handling
        // final String error_message = (String) entries.get("message");
        final String error_name = (String) entries.get("blablub_TODO");
        if (error_name == null) {
            /* No error */
            return props;
        }
        return null;
        // if (error_name.equalsIgnoreCase("file_is_not_available_for_download")) {
        // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        // }
        // if (link == null) {
        // /* Account related error e.g. password_incorrect, invalid_captcha */
        // throw new AccountInvalidException(error_name);
        // } else {
        // throw new PluginException(LinkStatus.ERROR_FATAL, error_name);
        // }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> props = login(account, true);
        final Map<String, Object> user = (Map<String, Object>) props.get("user");
        final Map<String, Object> user_download_quota = (Map<String, Object>) user.get("download_quota");
        final Number reg_time = (Number) user.get("reg_time");
        final Number vip_end_time = (Number) user.get("vip_end_time");
        if (reg_time != null) {
            ai.setCreateTime(reg_time.longValue() * 1000);
        }
        ai.setUnlimitedTraffic();
        if (Boolean.TRUE.equals(user.get("is_vip"))) {
            if (vip_end_time != null) {
                ai.setValidUntil(vip_end_time.longValue() * 1000, br);
            }
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        if (user_download_quota != null) {
            final int dls_daily_limit = ((Number) user_download_quota.get("daily_limit")).intValue();
            final int dls_remaining = ((Number) user_download_quota.get("remaining")).intValue();
            if (dls_remaining <= 0) {
                ai.setTrafficLeft(0);
            } else {
                ai.setUnlimitedTraffic();
            }
            ai.setStatus(account.getType().getLabel() + " | Downloads left: " + dls_daily_limit + "/" + dls_remaining);
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* Free accounts can have captchas */
            return true;
        } else {
            /* Premium accounts do not have captchas */
            return false;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}