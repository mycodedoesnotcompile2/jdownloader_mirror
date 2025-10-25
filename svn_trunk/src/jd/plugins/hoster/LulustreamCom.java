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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

@HostPlugin(revision = "$Revision: 51719 $", interfaceVersion = 3, names = {}, urls = {})
public class LulustreamCom extends XFileSharingProBasic {
    public LulustreamCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2023-06-20: null <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "lulustream.com", "luluvdo.com", "luluvid.com", "lulu.st", "tnmr.org", "lulu0.ovh", "luluvdoo.com", "cdn1.site" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("tnmr.org");
        deadDomains.add("luluvdo.com");
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
        return LulustreamCom.buildAnnotationUrlsLulustream(getPluginDomains());
    }

    public static final String getAnnotationPatternPartLulustream() {
        return "/(d/[A-Za-z0-9]+|(d|e)/[a-z0-9]{12}|embed(\\-|/)[a-z0-9]{12}|[a-z0-9]{12}(/[^/]+(?:\\.html)?)?)";
    }

    public static String[] buildAnnotationUrlsLulustream(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + LulustreamCom.getAnnotationPatternPartLulustream());
        }
        return ret.toArray(new String[0]);
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

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        /* 2020-08-31: Special */
        return true;
    }

    @Override
    public String getFilenameFromURL(final DownloadLink dl) {
        return null;
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filesize_alt_fast() {
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        return false;
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML(">\\s*Oops! Page Not Found|>\\s*Sorry, the page you're looking for doesn't exist|>\\s*File is no longer available|>\\s*No such file|>\\s*File Not Found")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        String betterFilename = br.getRegex("name=\"og:title\" content=\"([^\"]+)\"").getMatch(0);
        if (betterFilename == null) {
            betterFilename = br.getRegex("<h1 class=\"h5\">([^<]+)</h1>").getMatch(0);
        }
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    protected String getDllinkViaOfficialVideoDownload(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        final URL_TYPE type = getURLType(br.getURL());
        if (type != URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD) {
            /* 2024-04-17: Special handling because stream downloads are DRM protected */
            this.getPage(buildURLPath(link, this.getFUIDFromURL(link), URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD));
        }
        return super.getDllinkViaOfficialVideoDownload(br, link, account, returnFilesize);
    }

    @Override
    protected Boolean requiresCaptchaForOfficialVideoDownload() {
        return Boolean.FALSE;
    }

    @Override
    protected URL_TYPE getURLType(final String url) {
        if (url == null) {
            return null;
        }
        if (url.matches("(?i)^https?://[A-Za-z0-9\\-\\.:]+/embed/([a-z0-9]{12}).*")) {
            return URL_TYPE.EMBED_VIDEO;
        } else {
            return super.getURLType(url);
        }
    }

    @Override
    protected String getFUID(final String url, URL_TYPE type) {
        if (url == null || type == null) {
            return null;
        }
        try {
            if (type == URL_TYPE.EMBED_VIDEO) {
                final String path = new URL(url).getPath();
                return new Regex(path, "(?i)/embed(?:-|/)([a-z0-9]{12})").getMatch(0);
            }
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return super.getFUID(url, type);
    }

    @Override
    protected String getDllink(final DownloadLink link, final Account account, final Browser br, String src) {
        final String ret = super.getDllink(link, account, br, src);
        if (StringUtils.containsIgnoreCase(ret, ".m3u8")) {
            /*
             * 2025-04-24: Their HLS streams are DRM protected thus we cannot download them. Return null to signal upper handling to try to
             * obtain official video downloadlink instead.
             */
            return null;
        } else {
            return ret;
        }
    }

    @Override
    protected boolean containsRecaptchaV2Class(String string) {
        /* 2025-04-24: Workaround for upper handling returning wrong value here. */
        return false;
    }
}