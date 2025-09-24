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

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51545 $", interfaceVersion = 3, names = {}, urls = {})
public class KatfileCom extends XFileSharingProBasic {
    public KatfileCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:2019-03-01: premium untested, set FREE account limits <br />
     * captchatype-info: 2019-03-01: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "katfile.cloud", "katfile.com" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2025-09-09: Main domain changed from katfile.com to katfile.cloud */
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    }

    @Override
    protected boolean supports_lifetime_account() {
        return true;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return false;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    @Override
    public Form findFormDownload1Free(final Browser br) throws Exception {
        final Form download1 = super.findFormDownload1Free(br);
        final boolean formFixRequired = false;
        if (formFixRequired && download1 != null) {
            /* 2022-09-02 - fixed in Form class */
            final String formkey = "method_free";
            final InputField method_free = download1.getInputField(formkey);
            String value = method_free.getValue();
            if (value == null) {
                value = "Start Download";
            }
            download1.remove("method_free");
            download1.put("method_free", value);
        }
        return download1;
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        super.doFree(link, account);
    }

    @Override
    public Form findFormDownload2Premium(final DownloadLink link, final Account account, final Browser br) throws Exception {
        final Form ret = super.findFormDownload2Premium(link, account, br);
        if (ret != null) {
            /* 2022-01-07: Special antiBot handling / captcha in premium mode. */
            handleCaptcha(link, br, ret);
        }
        return ret;
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return -2;
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
        return -1;
    }

    @Override
    protected boolean supportsAPIMassLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean supportsAPISingleLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("/404-remove|>\\s*The file expired|>\\s*The file was deleted by its owner")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }
    // @Override
    // protected boolean isPremiumOnlyURL(final Browser br) {
    // final String url = br != null ? br.getURL() : null;
    // if (url == null) {
    // return false;
    // } else if (StringUtils.containsIgnoreCase(url, "/?op=registration&redirect=")) {
    // return true;
    // } else {
    // return super.isPremiumOnlyURL(br);
    // }
    // }

    @Override
    protected String getPremiumOnlyErrorMessage(final Browser br) {
        if (br.containsHTML(">\\s*This file is available for Premium")) {
            return "This file is available for Premium";
        } else if (StringUtils.containsIgnoreCase(br.getURL(), "/?op=registration&redirect=")) {
            return "Account required to download this file";
        } else {
            return super.getPremiumOnlyErrorMessage(br);
        }
    }

    @Override
    protected String regexWaittime(final String html) {
        final String waitStr = new Regex(html, "var estimated_time = (\\d+)").getMatch(0);
        if (waitStr != null) {
            /* Small hack: These aren't seconds but tenths of a second */
            final int realSeconds = Integer.parseInt(waitStr) / 10;
            return Integer.toString(realSeconds);
        }
        return super.regexWaittime(html);
    }
}