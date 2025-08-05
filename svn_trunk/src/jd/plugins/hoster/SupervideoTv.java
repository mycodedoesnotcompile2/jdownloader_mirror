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

import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigVideo;
import org.jdownloader.plugins.components.config.XFSConfigVideoSupervideoTv;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51300 $", interfaceVersion = 3, names = {}, urls = {})
public class SupervideoTv extends XFileSharingProBasic {
    public SupervideoTv(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2020-01-04: null<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "supervideo.tv", "supervideo.cc" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
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
            return -2;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        /* 2020-01-04: Special */
        return true;
    }

    @Override
    protected String findAPIKey(final Browser br) throws Exception {
        /* 2020-01-04: Special: Their API is broken! */
        return null;
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 7;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return 7;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 7;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        return false;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        final String betterFilename = br.getRegex("<h1 class=\"download__title\">([^<]+)</h1>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    public Class<? extends XFSConfigVideo> getConfigInterface() {
        return XFSConfigVideoSupervideoTv.class;
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("class=\"fake-signup\"")) {
            /* 2025-08-24 e.g. /e/2rzwt8lywxx3 */
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    public ArrayList<String> getCleanupHTMLRegexes() {
        /* 2025-08-04: Workaround as default handling filters stuff we need, aka class="fake-signup" */
        final ArrayList<String> regexStuff = new ArrayList<String>();
        return regexStuff;
    }

    @Override
    protected String buildURLPath(final DownloadLink link, final String fuid, final URL_TYPE type) {
        if (type == URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD) {
            return "/v/" + fuid;
        } else {
            return super.buildURLPath(link, fuid, type);
        }
    }
}