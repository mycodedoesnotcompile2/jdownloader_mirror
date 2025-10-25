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
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51727 $", interfaceVersion = 3, names = {}, urls = {})
public class DarkiboxCom extends XFileSharingProBasic {
    public DarkiboxCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: <br />
     * captchatype-info: 2023-10-06: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "darkibox.com" });
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
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 0;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
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

    private boolean uglyTempWorkaround = false;

    @Override
    protected String getDllinkViaOfficialVideoDownload(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        /* 2024-04-17: Removed special handling as it looks like this website has disabled official video downloads for free-users. */
        // final URL_TYPE type = getURLType(br.getURL());
        // if (type != URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD) {
        // /* 2023-10-06: This skips pre download wait */
        // this.getPage(buildURLPath(link, this.getFUIDFromURL(link), URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD));
        // }
        if (!uglyTempWorkaround && Thread.currentThread() instanceof SingleDownloadController) {
            // Wait before download1 form is sent
            final int waitSeconds;
            final String waitStr = this.regexWaittime(br);
            if (waitStr != null) {
                waitSeconds = Integer.parseInt(waitStr);
            } else {
                /* Fallback */
                waitSeconds = 3;
            }
            this.sleep(waitSeconds * 1001l, this.getDownloadLink());
            uglyTempWorkaround = true;
        }
        return super.getDllinkViaOfficialVideoDownload(br, link, account, returnFilesize);
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        /* 2023-10-06: For "/d/..." links. */
        final String betterFilename = new Regex(html, "(?i)<h3 [^>]*>\\s*Download\\s*([^<]*?)\\s*</h\\d+>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account);
        if (br.containsHTML(">\\s*You are not able to download Files")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Website error 'You are not able to download Files'");
        }
    }

    @Override
    protected AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        final AccountInfo ai = super.fetchAccountInfoWebsite(account);
        if (false && Account.AccountType.PREMIUM.equals(account.getType()) && ai.isUnlimitedTraffic()) {
            // website only shows limit but not left traffic?!
            this.getPage("?op=my_account");
            fetchAccountInfoWebsiteTraffic(br, account, ai);
        }
        return ai;
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        return true;
    }

    @Override
    protected String[] supportsPreciseExpireDate() {
        return new String[] { "/?op=payments" };
    }

    @Override
    protected String regexWaittime(final String html) {
        final String waitSeconds = new Regex(html, ">\\s*(\\d+)\\s*</span>\\s*seconds\\s*</span>").getMatch(0);
        if (waitSeconds != null) {
            return waitSeconds;
        } else {
            return super.regexWaittime(html);
        }
    }

    @Override
    protected String regExTrafficLeft(final Browser br) {
        String trafficleftStr = br.getRegex("Bandwidth limit</div>\\s*<h6>\\s*(\\d+[^<]+)/\\s*\\d+ Days\\s*<").getMatch(0);
        if (trafficleftStr != null) {
            /* Convert French -> English */
            trafficleftStr = trafficleftStr.replace("Go", "Gb");
            trafficleftStr = trafficleftStr.replace("Mo", "Mb");
            return trafficleftStr;
        } else {
            return super.regExTrafficLeft(br);
        }
    }
}