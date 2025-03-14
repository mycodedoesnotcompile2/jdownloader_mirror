//jDownloader - Downloadmanager
//Copyright (C) 2016  JD-Team support@jdownloader.org
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
import org.jdownloader.plugins.components.YetiShareCore;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50708 $", interfaceVersion = 2, names = {}, urls = {})
public class FireloadCom extends YetiShareCore {
    public FireloadCom(PluginWrapper wrapper) {
        super(wrapper);
        // this.enablePremium(getPurchasePremiumURL());
    }

    /**
     * DEV NOTES YetiShare<br />
     ****************************
     * mods: See overridden functions<br />
     * limit-info: 2020-07-27: No limits at all <br />
     * captchatype-info: 2020-07-27: null<br />
     * other: <br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "fireload.com" });
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
        return YetiShareCore.buildAnnotationUrls(getPluginDomains());
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

    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 0;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    public int getMaxSimultaneousFreeAccountDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    public String[] scanInfo(final DownloadLink link, final String[] fileInfo) {
        super.scanInfo(link, fileInfo);
        String betterFilesize = br.getRegex(">\\s*File Size:\\s*</strong>([^<]+)</p>").getMatch(0);
        if (betterFilesize == null) {
            /* 2025-02-26 */
            betterFilesize = br.getRegex("class='item-size'[^>]*>([^<]+)<span").getMatch(0);
        }
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        if (StringUtils.isEmpty(fileInfo[0])) {
            fileInfo[0] = br.getRegex("<title>([^<]+) \\| Fireload</title>").getMatch(0);
            if (StringUtils.isEmpty(fileInfo[0])) {
                fileInfo[0] = br.getRegex("lass='fileApps'>\\s*<p>([^<]+)</p><div").getMatch(0);
            }
        }
        return fileInfo;
    }

    @Override
    public boolean supports_availablecheck_over_info_page(final DownloadLink link) {
        /* 2020-07-27: Special */
        return false;
    }

    @Override
    public String regexWaittime(final Browser br) {
        final String waitSecondsStr = br.getRegex("\"dwait\":\\s*\"?(\\d+)").getMatch(0);
        if (waitSecondsStr != null) {
            return waitSecondsStr;
        } else {
            return super.regexWaittime(br);
        }
    }
}