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

import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver.CAPTCHA_TYPE;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 52004 $", interfaceVersion = 3, names = {}, urls = {})
public class SharemodsCom extends XFileSharingProBasic {
    public SharemodsCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sharemods.com" });
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
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
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

    @Override
    public int getMaxChunks(final Account account) {
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
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    protected String getDllink(final DownloadLink link, final Account account, final Browser br, final String src) {
        if (account == null || AccountType.FREE.equals(account.getType())) {
            /* 2025-12-16: Dirty hack to make "doFree" overridden handling down below work. */
            return null;
        }
        /* 2023-04-11 */
        final String dllink = new Regex(src, "href=\"(https://[^\"]+)\"\\s*id=\"downloadbtn\"").getMatch(0);
        if (dllink != null) {
            return dllink;
        } else {
            return super.getDllink(link, account, br, src);
        }
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        try {
            super.doFree(link, account);
        } catch (final PluginException e) {
            if (e.getLinkStatus() != LinkStatus.ERROR_PLUGIN_DEFECT) {
                throw e;
            }
        }
        final String nextStepURL = br.getRegex("href=\"(https://[^ \"]+)\"[^>]*class=\"btn btn-primary\"").getMatch(0);
        if (nextStepURL == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        getPage(nextStepURL);
        final Form captchaform = br.getFormbyProperty("id", "vform");
        if (captchaform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String cfTurnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br).getToken();
        captchaform.put("cf-turnstile-response", Encoding.urlEncode(cfTurnstileResponse));
        br.setFollowRedirects(false);
        this.submitForm(captchaform);
        final String finallink = br.getRedirectLocation();
        if (finallink == null) {
            // TODO: Add error handling for invalid captcha
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.setFollowRedirects(true);
        handleDownload(link, account, null, finallink);
    }

    @Override
    public CAPTCHA_TYPE[] getExpectedCaptchaTypes() {
        // 2025-12-16
        return new CAPTCHA_TYPE[] { CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE };
    }
}