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
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51062 $", interfaceVersion = 3, names = {}, urls = {})
public class FreedlInk extends XFileSharingProBasic {
    public FreedlInk(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2023-12-06: reCaptchaV2 <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "freedl.ink", "frdl.to", "frdl.io", "frdl.is", "frdl.my" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("frdl.to");
        deadDomains.add("frdl.is");
        return deadDomains;
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
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public Form findFormDownload1Free(Browser br) throws Exception {
        final Form ret = super.findFormDownload1Free(br);
        if (ret == null) {
            return null;
        }
        ret.put("download_free", "1");
        return ret;
    }

    @Override
    protected Form findFormDownload2Free(Browser br) {
        final Form ret = super.findFormDownload2Free(br);
        if (ret == null) {
            return null;
        }
        ret.put("download_free", "1");
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
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    protected String regexWaittime(final String html) {
        // 2024-12-31
        String waitSecondsStr = new Regex(html, "seconds\\.html\\(\\s*(\\d+)\\s*\\)").getMatch(0);
        if (waitSecondsStr == null) {
            waitSecondsStr = new Regex(html, "var sec = (\\d+);").getMatch(0);
        }
        if (waitSecondsStr != null) {
            return waitSecondsStr;
        } else {
            return super.regexWaittime(html);
        }
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (StringUtils.containsIgnoreCase(br.getURL(), "/404.html")) {
            return true;
        } else {
            return super.isOffline(link, br);
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
    protected String getCaptchaCode(final String method, final String captchaAddress, final DownloadLink downloadLink) throws Exception {
        return super.getCaptchaCode("xfilesharingprobasic_special_5digit", captchaAddress, downloadLink);
    }
}