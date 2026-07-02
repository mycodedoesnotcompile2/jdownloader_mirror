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
import java.util.regex.Pattern;

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

@HostPlugin(revision = "$Revision: 52939 $", interfaceVersion = 3, names = {}, urls = {})
public class VidmolyTo extends XFileSharingProBasic {
    public VidmolyTo(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
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
        ret.add(new String[] { "vidmoly.me", "vidmoly.biz", "vidmoly.to", "vidmoly.net" });
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
        /* 2026-01-27: Changed main domain from vidmoly.net to vidmoly.biz */
        /*
         * 2026-01-29: Changed main domain from vidmoly.biz to vidmoly.me as .biz domain displays this text atm:
         * {"name":"embed-service","version":"1.0.0","status":"running"}
         */
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

    /**
     * Path patterns for the different URL types seen for this host, see enum URL_TYPE in super class. <br />
     * All of them use a 12 char alphanumeric fuid.
     */
    private static final Pattern PATTERN_NORMAL      = Pattern.compile("/([a-z0-9]{12})(/[^/]+)?(\\.html)?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_EMBED_VIDEO = Pattern.compile("/embed-([a-z0-9]{12})(\\.html)?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_DOWNLOAD    = Pattern.compile("/dl?/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_WATCH       = Pattern.compile("/w/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);
    /* 2026-07-01: New URL type */
    private static final Pattern PATTERN_VIDEO       = Pattern.compile("/v/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);

    public static final String getDefaultAnnotationPatternPartCustom() {
        /* 2020-05-18: Special */
        /* 2026-07-01: Built from the PATTERN_* constants above so both stay in sync. */
        return "(" + PATTERN_EMBED_VIDEO.pattern() + "|" + PATTERN_DOWNLOAD.pattern() + "|" + PATTERN_WATCH.pattern() + "|" + PATTERN_VIDEO.pattern() + "|" + PATTERN_NORMAL.pattern() + ")";
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + getDefaultAnnotationPatternPartCustom());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getFUIDFromURL(final DownloadLink link) {
        if (link == null || link.getPluginPatternMatcher() == null) {
            return null;
        }
        /* 2020-05-18: Special */
        /* 2026-07-01: Uses the PATTERN_* constants above so both stay in sync. In each of them the fuid is capturing group 1. */
        try {
            final String path = new URL(link.getPluginPatternMatcher()).getPath();
            final Pattern[] patterns = new Pattern[] { PATTERN_EMBED_VIDEO, PATTERN_DOWNLOAD, PATTERN_WATCH, PATTERN_VIDEO, PATTERN_NORMAL };
            for (final Pattern pattern : patterns) {
                final String fuid = new Regex(path, pattern).getMatch(0);
                if (fuid != null) {
                    return fuid;
                }
            }
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return super.getFUIDFromURL(link);
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
        return Integer.MAX_VALUE;
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

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        /* 2020-05-18: Special */
        super.scanInfo(html, fileInfo);
        String betterFilename = new Regex(html, ">([^>]+)</span><br>\\s+<span style=").getMatch(0);
        if (betterFilename == null) {
            betterFilename = new Regex(html, "<title>([^<]+)</title>").getMatch(0);
        }
        if (betterFilename != null) {
            betterFilename = betterFilename.replaceFirst("(?i) - Kino$", "");
            fileInfo[0] = betterFilename;
        }
        final String betterFilesize = new Regex(html, ">\\((\\d+[^<]+)\\) Download\\s*</a>").getMatch(0);
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
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account);
        if (br.containsHTML("<title>\\s*Please wait\\s*</title>")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Anti bot page says 'Please wait'");
        }
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        return false;
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        return false;
    }

    @Override
    protected String buildURLPath(final DownloadLink link, final String fuid, final URL_TYPE type) {
        if (type == URL_TYPE.NORMAL) {
            /* 2025-03-18: Special: .html ending needed */
            return "/v/" + fuid;
        } else {
            return super.buildURLPath(link, fuid, type);
        }
    }
}