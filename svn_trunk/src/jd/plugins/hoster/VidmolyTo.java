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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51526 $", interfaceVersion = 3, names = {}, urls = {})
public class VidmolyTo extends XFileSharingProBasic {
    public VidmolyTo(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    @Override
    protected String buildNormalURLPath(DownloadLink link, final String fuid) {
        return "/" + fuid;
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2020-05-18: null<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vidmoly.net", "vidmoly.me", "vidmoly.to" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("vidmoly.to");
        return deadDomains;
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2025-09-19: Changed main domain from vidmoly.to to vidmoly.net */
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static final String getDefaultAnnotationPatternPartCustom() {
        /* 2020-05-18: Special */
        return "/(?:embed-|dl?/|w/)?[a-z0-9]{12}(/[^/]+)?(\\.html)?";
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + getDefaultAnnotationPatternPartCustom());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getFUIDFromURL(final DownloadLink dl) {
        /* 2020-05-18: Special */
        try {
            if (dl != null && dl.getPluginPatternMatcher() != null) {
                final String result = new Regex(new URL(dl.getPluginPatternMatcher()).getPath(), "/(?:embed-|dl?/|w/)?([a-z0-9]{12})").getMatch(0);
                return result;
            } else {
                return null;
            }
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return null;
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
            return -2;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return -2;
        } else {
            /* Free(anonymous) and unknown account type */
            return -2;
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
    protected boolean supports_availablecheck_filesize_via_embedded_video() {
        /* 2020-05-18: Special */
        return true;
    }

    @Override
    protected boolean isVideohosterEmbed() {
        /* 2020-05-18: Special */
        return true;
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        /* 2020-05-18: Special */
        return true;
    }

    public String[] scanInfo(final String[] fileInfo) {
        /* 2020-05-18: Special */
        super.scanInfo(fileInfo);
        if (StringUtils.isEmpty(fileInfo[0])) {
            fileInfo[0] = br.getRegex(">([^>]+)</span><br>\\s+<span style=").getMatch(0);
        }
        final String betterFilesize = br.getRegex(">\\((\\d+[^<]+)\\) Download\\s*</a>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        return fileInfo;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2020-05-18: Special */
        return false;
    }

    @Override
    public String getLoginURL() {
        /* 2022-01-20 */
        return getMainPage() + "/";
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("/notice\\.php")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account, checkAll);
        if (br.containsHTML("<title>\\s*Please wait\\s*</title>")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Anti bot page says 'Please wait'");
        }
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        return false;
    }

    @Override
    protected String buildURLPath(final DownloadLink link, final String fuid, final URL_TYPE type) {
        if (type == URL_TYPE.NORMAL) {
            /* 2025-03-18: Special: .html ending needed */
            return "/" + fuid + ".html";
        } else {
            return super.buildURLPath(link, fuid, type);
        }
    }
}