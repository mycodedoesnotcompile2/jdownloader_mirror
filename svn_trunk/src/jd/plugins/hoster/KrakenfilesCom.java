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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = { "krakenfiles.com" }, urls = { "https?://(?:www\\.)?krakenfiles\\.com/view/([a-z0-9]+)/file\\.html" })
public class KrakenfilesCom extends PluginForHost {
    public KrakenfilesCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
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
            return false;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return false;
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
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Set Fallback-name */
            link.setName(fid);
        }
        /* This json is part of their embed functions :) */
        br.getPage("https://" + getHost() + "/json/" + fid);
        final Object jsonO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (!(jsonO instanceof Map)) {
            /* 2023-11-21: Returns empty array when given fileID is invalid. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = (Map<String, Object>) jsonO;
        final String hash = (String) entries.get("hash");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!StringUtils.equalsIgnoreCase(hash, fid)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setFinalFileName(entries.get("title").toString());
        link.setDownloadSize(SizeFormatter.getSize(entries.get("size").toString()));
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        String directlinkproperty = "directurl";
        if (account != null) {
            directlinkproperty += "_account_" + account.getType();
            this.login(account, false);
        }
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            br.getPage(link.getPluginPatternMatcher());
            final Form dlform = br.getFormbyActionRegex(".*?download/.*?");
            if (dlform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dlform.setMethod(MethodType.POST);
            /* 2024-11-05: lol Even premium users need to enter that captcha, lol */
            boolean captchaNeeded = false;
            if (CaptchaHelperHostPluginRecaptchaV2.containsRecaptchaV2Class(dlform)) {
                captchaNeeded = true;
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                dlform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            }
            br.submitForm(dlform);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String status = (String) entries.get("status");
            if ("error".equalsIgnoreCase(status)) {
                final String msg = entries.get("msg").toString();
                if (captchaNeeded && msg.equalsIgnoreCase("captcha not valid")) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                } else {
                    /* Unknown error */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, entries.get("msg").toString());
                }
            }
            dllink = entries.get("url").toString();
            if (StringUtils.isEmpty(dllink) && dllink.startsWith("http")) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 405) {
                /**
                 * 2023-02-05: This sometimes happens. Strange! See: https://board.jdownloader.org/showthread.php?t=95055 </br>
                 * This may happen when we're trying to open too many connections.
                 */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 405 'Method not allowed'", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unexpected response: Expected file, got html");
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                this.br.setCookies(this.getHost(), cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost());
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/login");
            final Form loginform = br.getFormbyProperty("id", "login-form");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("email", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
            loginform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            loginform.put("valid_form_click", "send");
            br.submitForm(loginform);
            if (!isLoggedin(br)) {
                final String loginInvalidError = br.getRegex("class=\"text-danger\"[^>]*>\\s*<em[^>]*></em>([^<]+)</span>").getMatch(0);
                if (loginInvalidError != null) {
                    throw new AccountInvalidException(Encoding.htmlDecode(loginInvalidError).trim());
                } else {
                    throw new AccountInvalidException();
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("/logout\"");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        long premiumTimeRemainingMillis = 0;
        final String premiumDaysLeftStr = br.getRegex("(\\d+) days? left").getMatch(0);
        if (premiumDaysLeftStr != null) {
            premiumTimeRemainingMillis = (Long.parseLong(premiumDaysLeftStr) + 1) * 24 * 60 * 60 * 1000;
        }
        ai.setUnlimitedTraffic();
        if (premiumTimeRemainingMillis > 0) {
            ai.setValidUntil(System.currentTimeMillis() + premiumTimeRemainingMillis);
            account.setType(AccountType.PREMIUM);
            account.setConcurrentUsePossible(true);
        } else {
            account.setType(AccountType.FREE);
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
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
    // private Object checkErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException,
    // InterruptedException {
    // try {
    // final Object jsonO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
    // if (jsonO == null || !(jsonO instanceof Map)) {
    // return jsonO;
    // }
    // final Map<String, Object> entries = (Map<String, Object>) jsonO;
    // final String status = (String) entries.get("status");
    // if (!"error".equalsIgnoreCase(status)) {
    // return entries;
    // }
    // // TODO: Add functionality
    // final String msg = entries.get("msg").toString();
    // if (link == null) {
    // throw new AccountInvalidException(msg);
    // }
    // if (msg.equalsIgnoreCase("captcha not valid")) {
    // throw new PluginException(LinkStatus.ERROR_CAPTCHA);
    // } else {
    // /* Unknown error */
    // throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, entries.get("msg").toString());
    // }
    // } catch (final JSonMapperException jme) {
    // final String errortext = "Bad API response";
    // // TODO
    // }
    // return null;
    // }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}