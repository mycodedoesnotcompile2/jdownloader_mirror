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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

import org.appwork.utils.Regex;
import org.jdownloader.plugins.components.XFileSharingProBasic;

@HostPlugin(revision = "$Revision: 51328 $", interfaceVersion = 3, names = {}, urls = {})
public class WorldBytezCom extends XFileSharingProBasic {
    public WorldBytezCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2019-09-16: unknown<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "worldbytez.net", "worldbytez.com", "worldbytez.org" });
        return ret;
    }

    @Override
    public String rewriteHost(String host) {
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
        return buildAnnotationUrlsWorldbytez(getPluginDomains());
    }

    public static String[] buildAnnotationUrlsWorldbytez(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            String regex = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + XFileSharingProBasic.getDefaultAnnotationPatternPart();
            regex += "|https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/\\?op=login\\&redirect=[a-z0-9]{12}";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    private static final String TYPE_SPECIAL = "(?i)https?://[^/]+/\\?op=login\\&redirect=([a-z0-9]+)";

    @Override
    public void correctDownloadLink(final DownloadLink link) {
        final Regex special = new Regex(link.getPluginPatternMatcher(), TYPE_SPECIAL);
        if (special.patternFind()) {
            link.setPluginPatternMatcher("https://" + this.getHost() + super.buildURLPath(link, special.getMatch(0), URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD));
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return true;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
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
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        /*
         * 2020-04-09: Special: Premiumonly files do not show any error. Instead, the download2 Form is just missing and there is no Form at
         * all in their HTML.
         */
        try {
            super.doFree(link, account);
        } catch (final PluginException e) {
            if (e.getLinkStatus() == LinkStatus.ERROR_PLUGIN_DEFECT && this.findFormDownload2Free(br) == null) {
                /* Account required to download this file */
                throw new AccountRequiredException();
            } else {
                throw e;
            }
        }
    }

    @Override
    protected void checkErrorsLastResort(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        if (br.containsHTML("(?i)>[^<]*Your account has been temporarily blocked due to multiple IPs") && account != null) {
            throw new AccountInvalidException("Your account has been temporarily blocked due to multiple IPs. Please change your password..after changing the password the account returns to normal");
        } else if (br.containsHTML(">\\s*If You Are Using VPN Please Disable To Continue Downloading\\s*<") && account != null) {

            /*
             * <h2 style="color: aliceblue;">Oops File Not Found</h2>
             * 
             * <h2 style="color: aliceblue;">If You Are Using VPN Please Disable To Continue Downloading</h2> <h2 style="color: aliceblue;">
             * We apologize for any inconvenience caused.</h2>
             */
            throw new AccountUnavailableException("VPN/IP blocked!", 15 * 60 * 1000l);
        }
        super.checkErrorsLastResort(br, link, account);
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
        /* 2024-01-23: Max limit according to admin */
        return 5;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2023-10-09 */
        return false;
    }
}