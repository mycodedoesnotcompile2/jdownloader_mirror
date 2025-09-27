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
import java.util.regex.Pattern;

import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigVideo;
import org.jdownloader.plugins.components.config.XFSConfigVideoSupervideoTv;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51577 $", interfaceVersion = 3, names = {}, urls = {})
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
        ret.add(new String[] { "supervideo.cc", "supervideo.tv" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2025-09-26: Changed main domain from supervideo.tv to supervideo.cc */
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

    private static final Pattern PATTERN_NORMAL                    = Pattern.compile("/([a-z0-9]{12})$", Pattern.CASE_INSENSITIVE);
    /* 2025-09-26: Not supported at this moment but we'll allow it anyways. */
    private static final Pattern PATTERN_OFFICIAL_VIDEO_DOWNLOAD   = Pattern.compile("/d/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_OFFICIAL_VIDEO_DOWNLOAD_2 = Pattern.compile("/v/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_EMBED_NEW                 = Pattern.compile("/e/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_EMBED_OLD                 = Pattern.compile("/embed-([a-z0-9]{12})\\.html", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_NORMAL.pattern() + "|" + PATTERN_OFFICIAL_VIDEO_DOWNLOAD.pattern() + "|" + PATTERN_OFFICIAL_VIDEO_DOWNLOAD_2.pattern() + "|" + PATTERN_EMBED_NEW + "|" + PATTERN_EMBED_NEW + "|" + PATTERN_EMBED_OLD + ")");
        }
        return ret.toArray(new String[0]);
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
    protected String findAPIKey(final Browser br) {
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
        String betterFileTitle = new Regex(html, "<h1 class=\"download__title\">([^<]+)</h1>").getMatch(0);
        if (betterFileTitle == null) {
            betterFileTitle = new Regex(html, "<h5 class=\"text-white\"[^>]*>([^<]+)</h5>").getMatch(0);
        }
        if (betterFileTitle != null) {
            /* Overwrite possible previous result since we know that this one is better. */
            fileInfo[0] = betterFileTitle;
        }
        final String betterFilesize = new Regex(html, "<div class=\"col-auto\"[^>]*>\\s*(\\d+[^<]+)</div>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
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

    @Override
    protected URL_TYPE getURLType(final String url) {
        if (url == null) {
            return null;
        }
        if (new Regex(url, PATTERN_OFFICIAL_VIDEO_DOWNLOAD).patternFind()) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else if (new Regex(url, PATTERN_OFFICIAL_VIDEO_DOWNLOAD_2).patternFind()) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else if (new Regex(url, PATTERN_EMBED_OLD).patternFind()) {
            return URL_TYPE.EMBED_VIDEO;
        } else if (new Regex(url, PATTERN_EMBED_NEW).patternFind()) {
            return URL_TYPE.EMBED_VIDEO_2;
        } else if (new Regex(url, PATTERN_NORMAL).patternFind()) {
            return URL_TYPE.NORMAL;
        } else {
            logger.info("Unknown URL_TYPE: " + url);
            return null;
        }
    }

    @Override
    protected String getFUID(final String url, URL_TYPE type) {
        if (url == null || type == null) {
            return null;
        }
        return new Regex(url, "/([a-z0-9]{12})$").getMatch(0);
    }

    @Override
    protected int getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds() {
        if (br != null && br.getURL() != null && br.getURL().contains("/v/")) {
            /* Small workaround: Zero wait needed when we're currently on "/v/..." links. */
            return 0;
        } else {
            return 2;
        }
    }
}