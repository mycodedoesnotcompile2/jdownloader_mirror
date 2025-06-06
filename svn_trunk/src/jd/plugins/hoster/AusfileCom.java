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

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50899 $", interfaceVersion = 3, names = {}, urls = {})
public class AusfileCom extends XFileSharingProBasic {
    public AusfileCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: null 4dignum solvemedia reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "ausfile.com" });
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
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return false;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return false;
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
    public boolean isLoggedin(Browser br) {
        boolean loggedin = super.isLoggedin(br);
        if (!loggedin) {
            logger.info("Special logincheck ...");
            /**
             * please use valid combinations only! login or email alone without xfss is NOT valid!
             */
            final boolean login_xfss_CookieOkay = StringUtils.isAllNotEmpty(br.getCookie(getMainPage(), "login", Cookies.NOTDELETEDPATTERN), br.getCookie(getMainPage(), "xfss", Cookies.NOTDELETEDPATTERN));
            final String htmlWithoutScriptTags = br.toString().replaceAll("(?s)(<script.*?</script>)", "");
            final String ahref = "<a[^<]*href\\s*=\\s*\"[^\"]*";
            /* 2019-07-08: Special */
            final boolean logoutOkay = new Regex(htmlWithoutScriptTags, ahref + "logout\\.php").matches();
            logger.info("login_xfss_CookieOkay:" + login_xfss_CookieOkay);
            logger.info("logoutOkay:" + logoutOkay);
            loggedin = login_xfss_CookieOkay && logoutOkay;
            logger.info("loggedin:" + loggedin);
        }
        return loggedin;
    }

    /**
     * 2019-04-05: Special: Host stores free-waittimes on IP + cookie so without deleting cookies, it may happen that even after a
     * reconnect, plugin will just loop through waittimes (as cookies are re-used because of Cloudflare support). Simply deleting them
     * solves this issue!
     */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.br.clearCookies(link.getHost());
        return super.requestFileInformation(link);
    }

    /** 2019-04-05: Special workaround! */
    @Override
    public String regexWaittime(final String html) {
        String ttt = super.regexWaittime(html);
        if (StringUtils.isEmpty(ttt)) {
            ttt = new Regex(html, "class=\"count\">(\\d+)</span>").getMatch(0);
        }
        if (ttt != null) {
            final double wait = Double.parseDouble(ttt);
            if (wait > 0) {
                final int realWait = (int) wait / 10;
                ttt = Integer.toString(realWait);
            }
        }
        return ttt;
    }

    @Override
    public boolean isPremiumOnly(final Browser br) {
        if (br.containsHTML("(?i)which is available for direct purchases only|Please select one of options below in order to purchase an access to this file")) {
            return true;
        } else {
            return super.isPremiumOnly(br);
        }
    }
}