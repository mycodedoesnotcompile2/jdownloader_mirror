//jDownloader - Downloadmanager
//Copyright (C) 2015  JD-Team support@jdownloader.org
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
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
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

@HostPlugin(revision = "$Revision: 50897 $", interfaceVersion = 3, names = { "icerbox.com" }, urls = { "https?://(?:www\\.)?icerbox\\.com/([A-Z0-9]{8})" })
public class IcerBoxCom extends PluginForHost {
    private final String        baseURL                   = "https://icerbox.com";
    private final String        apiURL                    = "https://icerbox.com/api/v1";
    private static final String PROPERTY_PREMIUM_REQUIRED = "premiumRequired";

    public IcerBoxCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(baseURL + "/premium");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.addAllowedResponseCodes(500);
        br.setFollowRedirects(true);
        return br;
    }

    private Browser createNewBrowserInstanceAPI() {
        final Browser br = createNewBrowserInstance();
        br.getHeaders().put("Accept", "application/json");
        return br;
    }

    @Override
    public String getAGBLink() {
        return baseURL + "/tos";
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        /* at this given time only premium can be downloaded */
        if (account == null || account.getType() != AccountType.PREMIUM) {
            return false;
        }
        return super.canHandle(link, account);
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

    public boolean checkLinks(final DownloadLink[] urls) {
        boolean okay = true;
        try {
            final Browser br = this.createNewBrowserInstanceAPI();
            final List<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    if (links.size() == 100 || index == urls.length) {
                        break;
                    }
                    links.add(urls[index]);
                    index++;
                }
                final StringBuilder sb = new StringBuilder();
                {
                    boolean atLeastOneDL = false;
                    for (final DownloadLink dl : links) {
                        if (atLeastOneDL) {
                            sb.append(",");
                        }
                        sb.append(getFID(dl));
                        atLeastOneDL = true;
                    }
                }
                br.getPage(apiURL + "/files?ids=" + sb);
                final Map<String, Object> entries = this.handleApiErrors(br, null, null);
                final List<Map<String, Object>> data = (List<Map<String, Object>>) entries.get("data");
                for (final DownloadLink dl : links) {
                    final String fid = this.getFID(dl);
                    Map<String, Object> fileinfo = null;
                    for (final Map<String, Object> tempmap : data) {
                        if (StringUtils.equals(tempmap.get("id").toString(), fid)) {
                            fileinfo = tempmap;
                            break;
                        }
                    }
                    /* Set fallback-filename */
                    if (!dl.isNameSet()) {
                        dl.setName(fid);
                    }
                    if (fileinfo == null) {
                        /* This should never happen but if no info for this file is found, we can only assume it's offline. */
                        dl.setAvailable(false);
                        continue;
                    }
                    final String status = fileinfo.get("status").toString();
                    if ("active".equalsIgnoreCase(status)) {
                        dl.setAvailable(true);
                    } else {
                        dl.setAvailable(false);
                    }
                    final String name = (String) fileinfo.get("name");
                    final Number size = (Number) fileinfo.get("size");
                    final String md5 = (String) fileinfo.get("md5");
                    final Boolean free_available = (Boolean) fileinfo.get("free_available");
                    if (name != null) {
                        dl.setFinalFileName(name);
                    }
                    if (size != null) {
                        dl.setVerifiedFileSize(size.longValue());
                    }
                    if (md5 != null) {
                        dl.setMD5Hash(md5);
                    }
                    if (Boolean.TRUE.equals(free_available)) {
                        dl.removeProperty(PROPERTY_PREMIUM_REQUIRED);
                    } else {
                        dl.setProperty(PROPERTY_PREMIUM_REQUIRED, true);
                    }
                    if (fileinfo.get("password") != null) {
                        dl.setPasswordProtected(true);
                    } else {
                        dl.setPasswordProtected(false);
                    }
                }
                if (index == urls.length) {
                    /* All links were checked */
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return okay;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformationApi(link);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload_API(link, null);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        synchronized (account) {
            final AccountInfo ai = new AccountInfo();
            final Browser ajax = this.createNewBrowserInstanceAPI();
            String token = account.getStringProperty("token", null);
            boolean tokenLoginSuccess = false;
            Map<String, Object> entries = null;
            if (StringUtils.isNotEmpty(token)) {
                logger.info("Validating stored logintoken");
                ajax.getHeaders().put("Authorization", "Bearer " + token);
                ajax.getPage(apiURL + "/user/account");
                try {
                    entries = this.handleApiErrors(ajax, account, null);
                    logger.info("Token login successful");
                    tokenLoginSuccess = true;
                } catch (final PluginException ple) {
                    logger.log(ple);
                    logger.info("Token login failed");
                    account.removeProperty("token");
                }
            }
            if (!tokenLoginSuccess) {
                logger.info("Performing full login");
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("email", account.getUser());
                query.appendEncoded("password", account.getPass());
                ajax.postPage(apiURL + "/auth/login", query);
                // recaptcha can happen here on overloaded website due to brute force attack
                if (ajax.getHttpConnection().getResponseCode() == 429) {
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, ajax, "6LcKRRITAAAAAExk3Pb2MfEBMP7HGTk8HG4cRBXv").getToken();
                    query.appendEncoded("g-recaptcha-response", recaptchaV2Response);
                    ajax.postPage(apiURL + "/auth/login", query);
                    entries = this.handleApiErrors(ajax, account, null);
                } else {
                    entries = this.handleApiErrors(ajax, account, null);
                }
                token = entries.get("token").toString();
                account.setProperty("token", token);
                ajax.getHeaders().put("Authorization", "Bearer " + token);
                ajax.getPage(apiURL + "/user/account");
                entries = this.handleApiErrors(ajax, account, null);
            }
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            final Map<String, Object> data_package = (Map<String, Object>) data.get("package");
            final Map<String, Object> data_premium = (Map<String, Object>) data.get("premium");
            final Map<String, Object> data_register_date = (Map<String, Object>) data.get("register_date");
            final String registerDateStr = (String) data_register_date.get("date");
            if (!StringUtils.isEmpty(registerDateStr)) {
                final String registerDateStrFixed = fixDateString(registerDateStr);
                ai.setCreateTime(TimeFormatter.getMilliSeconds(registerDateStrFixed, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH));
            }
            final String packageName;
            if (Boolean.TRUE.equals(data.get("has_premium"))) {
                /* Premium account */
                final String premiumExpireDateStr = (String) data_premium.get("date");
                final Number package_bandwidth_Max = (Number) data_package.get("bandwidth");
                final Number package_bandwidth_Daily_Max = (Number) data_package.get("volume");
                final Number bandwitdh_Used = ((Number) data.get("downloaded"));
                final Number bandwidth_Daily_Used = ((Number) data.get("downloaded_today"));
                final long bandwidth_Available = package_bandwidth_Max.longValue() - bandwitdh_Used.longValue();
                final long max = Math.min(bandwidth_Available, package_bandwidth_Daily_Max.longValue());
                final long left = max - bandwidth_Daily_Used.longValue();
                ai.setTrafficLeft(left);
                ai.setTrafficMax(max);
                final String premiumExpireDateStrFixed = fixDateString(premiumExpireDateStr);
                ai.setValidUntil(TimeFormatter.getMilliSeconds(premiumExpireDateStrFixed, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), ajax);
                account.setType(AccountType.PREMIUM);
                packageName = data_package.get("name").toString();
            } else {
                /* Free account */
                // ai.setTrafficLeft(0);
                ai.setExpired(true);
                account.setType(AccountType.FREE);
                packageName = "Free";
            }
            ai.setStatus(account.getType().getLabel() + " | Package: " + packageName);
            return ai;
        }
    }

    private String fixDateString(final String input) {
        /* Remove nano seconds */
        return input.replaceFirst("\\.0{6}", "");
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload_API(link, account);
    }

    private void handleDownload_API(final DownloadLink link, final Account account) throws Exception {
        /* links that require premium when we don't have a premium account. */
        requestFileInformationApi(link);
        if (link.hasProperty(PROPERTY_PREMIUM_REQUIRED) && (account == null || AccountType.FREE.equals(account.getType()))) {
            throw new AccountRequiredException();
        }
        final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
        final String storedDirecturl = this.getStoredDirecturl(link, account);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            final String token = account.getStringProperty("token", null);
            final String url = apiURL + "/dl/ticket";
            br.getHeaders().put("Authorization", "Bearer " + token);
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("file", getFID(link));
            br.postPage(url, query);
            final Map<String, Object> entries = handleApiErrors(br, account, link);
            dllink = entries.get("url").toString();
        }
        try {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, true, 0);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                link.setProperty(directlinkproperty, Property.NULL);
                handleDownloadErrors(account, link, true);
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
            link.setProperty(directlinkproperty, dllink);
        }
        dl.startDownload();
    }

    private final void handleDownloadErrors(final Account account, final DownloadLink downloadLink, final boolean lastChance) throws PluginException, IOException {
        br.followConnection(true);
        if (br.getRequest().getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private Map<String, Object> handleApiErrors(final Browser br, final Account account, final DownloadLink link) throws Exception {
        Map<String, Object> entries = null;
        try {
            final Object parsedJson = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (!(parsedJson instanceof Map)) {
                /**
                 * Response can also be array: ["user_not_found"] <br>
                 */
                throw new AccountInvalidException();
            }
            entries = (Map<String, Object>) parsedJson;
        } catch (final JSonMapperException ignore) {
            final String msg = "Invalid API response";
            final long waitMillis = 1 * 60 * 1000;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis);
            } else {
                throw new AccountUnavailableException(msg, waitMillis);
            }
        }
        /* E.g. {"message":null,"status_code":401} */
        /* This status code is usually the same as the http status code. */
        final String message = (String) entries.get("message");
        final String status_enum_string = (String) entries.get("err");
        if (status_enum_string != null) {
            /* Example: {"message":"Password expired. Enter your Account page and change it.","err":"PASSWORD_RENEW"} */
            throw new AccountInvalidException(message);
        }
        final Number status_codeO = (Number) entries.get("status_code");
        final int status_code = status_codeO != null ? status_codeO.intValue() : br.getHttpConnection().getResponseCode();
        if (status_code == 200) {
            /* No error */
            // final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            return entries;
        }
        /* Check for specific account errors */
        if (status_code == 401) {
            throw new AccountInvalidException();
        } else if (status_code == 404) {
            /* {"message":"404 Not Found","status_code":404} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (status_code == 422 || status_code == 429) {
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        if (link == null) {
            /* No DownloadLink given -> Must be account-check */
            throw new AccountInvalidException();
        }
        /* Assume that we got a download related error */
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "API error " + message + " | Code: " + status_code);
    }

    private AvailableStatus requestFileInformationApi(final DownloadLink link) throws IOException, PluginException {
        final boolean checked = checkLinks(new DownloadLink[] { link });
        // we can't throw exception in checklinks! This is needed to prevent multiple captcha events!
        if (!checked || !link.isAvailabilityStatusChecked()) {
            link.setAvailableStatus(AvailableStatus.UNCHECKABLE);
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return link.getAvailableStatus();
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            return;
        }
        link.removeProperty(PROPERTY_PREMIUM_REQUIRED);
    }

    protected String getDownloadModeDirectlinkProperty(final Account account) {
        if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            /* Premium account */
            return "premlink";
        } else if (account != null && AccountType.FREE.equals(account.getType())) {
            return "freelink2";
        } else {
            /* Free(anonymous) and unknown account type */
            return "freelink";
        }
    }

    public boolean hasCaptcha(final DownloadLink link, final jd.plugins.Account acc) {
        final String storedDirecturl = getStoredDirecturl(link, acc);
        if (storedDirecturl == null) {
            /* Stored direct url available -> Assume it's still valid -> No captcha needed */
            return false;
        } else if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* free accounts also have captchas */
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
        final String storedDirecturl = getStoredDirecturl(link, account);
        /*
         * E.g. account may not have enough traffic left but we still got a stored directurl from a previous downloadstart --> Allow
         * download attempt anyways as we can be quite sure that it will still be valid.
         */
        if (StringUtils.isNotEmpty(storedDirecturl)) {
            return true;
        } else {
            return super.enoughTrafficFor(link, account);
        }
    }

    private String getStoredDirecturl(final DownloadLink link, final Account account) {
        final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
        final String dllink = link.getStringProperty(directlinkproperty);
        return dllink;
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }
}