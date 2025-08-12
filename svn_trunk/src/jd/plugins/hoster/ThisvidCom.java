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

import java.util.ArrayList;
import java.util.List;

import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.AccountInvalidException;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51313 $", interfaceVersion = 3, names = {}, urls = {})
public class ThisvidCom extends KernelVideoSharingComV2 {
    public ThisvidCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www.thisvid.com/");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(this.getHost(), "kt_tcookie", "1");
        br.setCookie(this.getHost(), "kt_is_visited", "1");
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "thisvid.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    @Override
    protected boolean hasFUIDInsideURL(final String url) {
        return false;
    }

    public static String[] getAnnotationUrls() {
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultVideosPatternWithoutFileID(getPluginDomains());
    }

    @Override
    public void login(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies;
            final Cookies userCookies;
            if ((userCookies = account.loadUserCookies()) != null) {
                this.br.setCookies(userCookies);
                if (!validateCookies) {
                    return;
                }
                if (!validateCookies) {
                    logger.info("Trust cookies without check");
                    return;
                }
                br.getPage("https://" + this.getHost() + "/");
                if (isLoggedIN(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(this.br.getCookies(this.getHost()), "");
                    return;
                } else {
                    logger.info("User Cookie login failed");
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                }
            } else if ((cookies = account.loadCookies("")) != null) {
                br.setCookies(cookies);
                if (!validateCookies) {
                    logger.info("Trust cookies without check");
                    return;
                }
                br.getPage("https://" + this.getHost() + "/");
                if (isLoggedIN(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(this.br.getCookies(this.getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            br.clearCookies(this.getHost());
            br.getPage("https://" + this.getHost() + "/login.php");
            final Form loginform = br.getFormbyProperty("id", "logon_form");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("pass", Encoding.urlEncode(account.getPass()));
            loginform.put("remember_me", "1");
            final String captchaURL = br.getRegex("(/captcha/logon/[^\"]+)\"").getMatch(0);
            if (captchaURL != null) {
                final String code = this.getCaptchaCode(captchaURL, this.getDownloadLink());
                loginform.put("code", Encoding.urlEncode(code));
            } else if (AbstractRecaptchaV2.containsRecaptchaV2Class(loginform)) {
                final String rcKey = br.getRegex("data-recaptcha-key=\"([^\"]+)\"").getMatch(0);
                final String token;
                if (rcKey != null) {
                    token = new CaptchaHelperHostPluginRecaptchaV2(this, br, rcKey).getToken();
                } else {
                    token = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                }
                loginform.put("g-recaptcha-response", Encoding.urlEncode(token));
            }
            br.submitForm(loginform);
            if (!isLoggedIN(br)) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlSlug) {
        return generateContentURLDefaultVideosPatternWithoutFileID(host, fuid, urlSlug);
    }
}