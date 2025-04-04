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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 50810 $", interfaceVersion = 3, names = {}, urls = {})
public class VtubeTo extends XFileSharingProBasic {
    public VtubeTo(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2021-09-07: No limits (HLS streaming). <br />
     * captchatype-info: 2021-09-07: null<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vtube.network", "vtube.to", "vtplayer.net", "vtbe.to" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("vtube.to");
        deadDomains.add("vtplayer.net");
        return deadDomains;
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2023-05-30: Merged with vtplayer.net plugin. */
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
    public String[] scanInfo(final String[] fileInfo) {
        super.scanInfo(fileInfo);
        final String betterFiletitle = br.getRegex("<h3 [^>]*>Watch ([^<]+)</h3>").getMatch(0);
        if (betterFiletitle != null) {
            fileInfo[0] = betterFiletitle;
        }
        return fileInfo;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2022-01-24: Disabled else it may pick up false positives! */
        return false;
    }

    @Override
    public AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account) throws Exception {
        final AvailableStatus status = super.requestFileInformationWebsite(link, account);
        if (status == AvailableStatus.TRUE) {
            /**
             * 2023-06-06: Special offline check: Some items look like they're online but they are offline -> Check thumbnailURL to find the
             * real status.
             */
            final String fuid = this.getFUIDFromURL(link);
            final String thumbnailURL = br.getRegex("(https?://pix\\.[^/]+/" + fuid + "\\.jpg)").getMatch(0);
            if (thumbnailURL != null) {
                logger.info("Performing extended offline-check");
                URLConnectionAdapter con = null;
                final Browser brc = br.cloneBrowser();
                try {
                    con = brc.openHeadConnection(thumbnailURL);
                    if (!this.looksLikeDownloadableContent(con)) {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                } finally {
                    try {
                        con.disconnect();
                    } catch (final Throwable e) {
                    }
                }
            } else {
                logger.warning("Failed to find thumbnailURL");
            }
        }
        return status;
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML(">\\s*File Not Found|The file expired")) {
            return true;
        } else if (br.containsHTML("/assets/bge2\\.jpg")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }
}