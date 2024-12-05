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
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

/**
 *
 * @author raztoki
 *
 */
@HostPlugin(revision = "$Revision: 50291 $", interfaceVersion = 3, names = { "icerbox.com" }, urls = { "https?://(?:www\\.)?icerbox\\.com/([A-Z0-9]{8})" })
public class IcerBoxCom extends PluginForHost {
    private final String baseURL = "https://icerbox.com";
    private final String apiURL  = "https://icerbox.com/api/v1";

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
        br.setFollowRedirects(true);
        br.addAllowedResponseCodes(500);
        return br;
    }

    @Override
    public String getAGBLink() {
        return baseURL + "/tos";
    }

    private void setConstants(final Account account) {
        if (account != null) {
            if (account.getType() == AccountType.FREE) {
                // free account
                chunks = 1;
                resumes = false;
            } else {
                // premium account
                chunks = 0;
                resumes = true;
            }
            logger.finer("setConstants = " + account.getUser() + " @ Account Download :: isFree = null, upperChunks = " + chunks + ", Resumes = " + resumes);
        } else {
            // free non account
            chunks = 1;
            resumes = false;
            logger.finer("setConstants = Guest Download :: isFree = null, upperChunks = " + chunks + ", Resumes = " + resumes);
        }
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
            final Browser br = this.createNewBrowserInstance();
            br.getHeaders().put("Accept", "application/json");
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
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
                if (br.containsHTML("In these moments we are upgrading the site system")) {
                    for (final DownloadLink dl : links) {
                        dl.getLinkStatus().setStatusText("Hoster is in maintenance mode. Try again later");
                        dl.setAvailableStatus(AvailableStatus.UNCHECKABLE);
                    }
                    return true;
                }
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
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
                        /* This should never happen! */
                        dl.setProperty("apiInfo", Property.NULL);
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
                        dl.removeProperty("premiumRequired");
                    } else {
                        dl.setProperty("premiumRequired", true);
                    }
                    if (fileinfo.get("password") != null) {
                        dl.setPasswordProtected(true);
                    } else {
                        dl.setPasswordProtected(false);
                    }
                    dl.setProperty("apiInfo", Boolean.TRUE);
                }
                if (index == urls.length) {
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
        if (useAPI()) {
            return requestFileInformationApi(link);
        } else {
            return AvailableStatus.UNCHECKABLE;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        setConstants(null);
        if (useAPI()) {
            handleDownload_API(link, null);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    /**
     * useAPI frame work? <br />
     * Override this when incorrect
     *
     * @return
     */
    private boolean useAPI() {
        return true;
        // return getPluginConfig().getBooleanProperty(preferAPI, preferAPIdefault);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        return fetchAccountInfoApi(account);
    }

    private AccountInfo fetchAccountInfoApi(final Account account) throws Exception {
        synchronized (account) {
            final AccountInfo ai = new AccountInfo();
            final Browser ajax = this.createNewBrowserInstance();
            ajax.getHeaders().put("Accept", "application/json");
            String token = account.getStringProperty("token", null);
            if (StringUtils.isNotEmpty(token)) {
                logger.info("Validating stored logintoken");
                ajax.getHeaders().put("Authorization", "Bearer " + token);
                ajax.getPage(apiURL + "/user/account");
                if (ajax.containsHTML("\"token_invalid\"") || ajax.getRequest().getHttpConnection().getResponseCode() != 200 || ajax.getHostCookie("ddl", Cookies.NOTDELETEDPATTERN) == null) {
                    token = null;
                    /* Do not retry with this known to be invalid token. */
                    account.removeProperty("token");
                    logger.info("Token login failed");
                } else {
                    logger.info("Token login successful");
                }
            }
            if (StringUtils.isEmpty(token)) {
                logger.info("Performing full login");
                ajax.postPage(apiURL + "/auth/login", "email=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
                handleApiErrors(ajax, account, null);
                // recaptcha can happen here on brute force attack
                if (ajax.getHttpConnection().getResponseCode() == 429) {
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, ajax, "6LcKRRITAAAAAExk3Pb2MfEBMP7HGTk8HG4cRBXv").getToken();
                    ajax.postPage(apiURL + "/auth/login", "email=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()) + "&g-recaptcha-response=" + Encoding.urlEncode(recaptchaV2Response));
                    handleApiErrors(ajax, account, null);
                    if (ajax.getHttpConnection().getResponseCode() == 429 || ajax.getHttpConnection().getResponseCode() == 422) {
                        throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                    } else if (ajax.getHttpConnection().getResponseCode() == 401) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                // token
                token = PluginJSonUtils.getJsonValue(ajax, "token");
                if (token == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                account.setProperty("token", token);
                ajax.getHeaders().put("Authorization", "Bearer " + token);
                ajax.getPage(apiURL + "/user/account");
                final String err = PluginJSonUtils.getJsonValue(ajax, "err");
                if (StringUtils.equals("PASSWORD_RENEW", err)) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "Password expired. Enter your Account page and change it.", PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
            }
            final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(ajax.toString());
            final Boolean is_premium = (Boolean) JavaScriptEngineFactory.walkJson(entries, "data/has_premium");
            final String expire = (String) JavaScriptEngineFactory.walkJson(entries, "data/premium/date");
            final Number bandwidth_Max = (Number) JavaScriptEngineFactory.walkJson(entries, "data/package/bandwidth");
            final Number bandwitdh_Used = ((Number) JavaScriptEngineFactory.walkJson(entries, "data/downloaded"));
            final Number bandwidth_Daily_Max = (Number) JavaScriptEngineFactory.walkJson(entries, "data/package/volume");
            final Number bandwidth_Daily_Used = ((Number) JavaScriptEngineFactory.walkJson(entries, "data/downloaded_today"));
            // free account
            if (Boolean.FALSE.equals(is_premium)) {
                /* Free account */
                ai.setTrafficLeft(0);
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "Free Accounts of this provider are not supported", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                /* Premium account */
                final long bandwidth_Available = bandwidth_Max.longValue() - bandwitdh_Used.longValue();
                final long max = Math.min(bandwidth_Available, bandwidth_Daily_Max.longValue());
                final long left = max - bandwidth_Daily_Used.longValue();
                ai.setTrafficLeft(left);
                ai.setTrafficMax(max);
                ai.setValidUntil(TimeFormatter.getMilliSeconds(expire.replaceFirst("\\.0{6}", ""), "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), ajax);
                account.setType(AccountType.PREMIUM);
                return ai;
            }
        }
    }

    @Override
    public void handlePremium(final DownloadLink downloadLink, final Account account) throws Exception {
        setConstants(account);
        if (useAPI()) {
            handleDownload_API(downloadLink, account);
            return;
        }
    }

    private void handleDownload_API(final DownloadLink link, final Account account) throws Exception {
        /* links that require premium when we don't have a premium account. */
        if (link.hasProperty("premiumRequired") && (account == null || AccountType.FREE.equals(account.getType()))) {
            throw new AccountRequiredException();
        }
        requestFileInformationApi(link);
        final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
        dllink = checkDirectLink(link, directlinkproperty);
        if (StringUtils.isEmpty(dllink)) {
            br.getHeaders().put("Accept", "application/json");
            final String token = account.getStringProperty("token", null);
            if (token == null) {
                // this should not happen.
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String url = apiURL + "/dl/ticket";
            br.getHeaders().put("Authorization", "Bearer " + token);
            br.postPage(url, "file=" + getFID(link));
            dllink = PluginJSonUtils.getJsonValue(br, "url");
            handleApiErrors(br, account, link);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resumes, chunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            link.setProperty(directlinkproperty, Property.NULL);
            handleDownloadErrors(account, link, true);
        }
        link.setProperty(directlinkproperty, dllink);
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

    private void handleApiErrors(final Browser br, final Account account, final DownloadLink link) throws Exception {
        final int resCode = br.getHttpConnection().getResponseCode();
        if (resCode == 401 && link == null) {
            br.followConnection();
            throw new AccountInvalidException();
        } else if (resCode == 403 || resCode == 401) {
            br.followConnection();
            if (account != null && link == null) {
                // banned user within login only
                throw new AccountInvalidException("Your account has been banned!");
            }
            // admin states that download routine can throw this as well.
            // we will invalidate the session token, throw temp hoster unvail, and relogin.
            else if (account != null && link != null) {
                if (br.containsHTML("Download this file will exceed your bandwidth")) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Not enough traffic left!", 1 * 60 * 60 * 1000l);
                }
                synchronized (account) {
                    final long was = account.getAccountInfo().getTrafficLeft();
                    // relogin due to limit been reached and token equals null.
                    account.setAccountInfo(fetchAccountInfoApi(account));
                    final long now = account.getAccountInfo().getTrafficLeft();
                    // throw retry so that core can re-analyse if account has traffic left.
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "You are possibly out of available traffic; 'was = " + was + "; now = " + now + ";", 30 * 1000l);
                }
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
    }

    private String checkDirectLink(final DownloadLink downloadLink, final String property) {
        if (property == null) {
            return null;
        }
        final String dllink = downloadLink.getStringProperty(property);
        if (dllink == null) {
            return null;
        }
        URLConnectionAdapter con = null;
        try {
            final Browser br2 = br.cloneBrowser();
            br2.setFollowRedirects(true);
            con = br2.openHeadConnection(dllink);
            if (!this.looksLikeDownloadableContent(con)) {
                throw new IOException();
            } else {
                return dllink;
            }
        } catch (final Exception e) {
            logger.log(e);
            downloadLink.setProperty(property, Property.NULL);
            return null;
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
    }

    private AvailableStatus requestFileInformationApi(final DownloadLink link) throws IOException, PluginException {
        final boolean checked = checkLinks(new DownloadLink[] { link });
        // we can't throw exception in checklinks! This is needed to prevent multiple captcha events!
        if (!checked || !link.isAvailabilityStatusChecked()) {
            link.setAvailableStatus(AvailableStatus.UNCHECKABLE);
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return getAvailableStatus(link);
    }

    private AvailableStatus getAvailableStatus(DownloadLink link) {
        try {
            final Field field = link.getClass().getDeclaredField("availableStatus");
            field.setAccessible(true);
            Object ret = field.get(link);
            if (ret != null && ret instanceof AvailableStatus) {
                return (AvailableStatus) ret;
            }
        } catch (final Throwable e) {
        }
        return AvailableStatus.UNCHECKED;
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
        link.setProperty("apiInfo", Property.NULL);
        link.setProperty("freelink2", Property.NULL);
        link.setProperty("freelink", Property.NULL);
        link.setProperty("premlink", Property.NULL);
        link.setProperty("premiumRequired", Property.NULL);
    }

    private String  dllink  = null;
    private int     chunks  = 0;
    private boolean resumes = true;

    protected String getDownloadModeDirectlinkProperty(final Account account) {
        if (AccountType.PREMIUM.equals(account.getType())) {
            /* Premium account */
            return "premlink";
        } else if (AccountType.FREE.equals(account.getType())) {
            return "freelink2";
        } else {
            /* Free(anonymous) and unknown account type */
            return "freelink";
        }
    }

    public boolean hasCaptcha(final DownloadLink downloadLink, final jd.plugins.Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* free accounts also have captchas */
            return true;
        } else {
            return false;
        }
    }
    // @Override
    // public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
    // final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
    // final String dllink = link.getStringProperty(directlinkproperty);
    // /*
    // * E.g. account doesn't have enough traffic left but we still got a stored directurl from a previous downloadstart --> Allow
    // * download attempt anyways as we can be quite sure that it will still be valid.
    // */
    // if (StringUtils.isNotEmpty(dllink)) {
    // return true;
    // } else {
    // return super.enoughTrafficFor(link, account);
    // }
    // }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }
}