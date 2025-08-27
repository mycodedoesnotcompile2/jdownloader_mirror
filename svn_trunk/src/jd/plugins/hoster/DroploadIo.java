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

import jd.PluginWrapper;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51374 $", interfaceVersion = 3, names = {}, urls = {})
public class DroploadIo extends XFileSharingProBasic {
    public DroploadIo(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2023-03-27: null <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "dropload.io", "dropload.tv" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL                    = Pattern.compile("/([a-z0-9]{12})$", Pattern.CASE_INSENSITIVE);
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
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        String betterFileTitle = new Regex(html, "<h1>([^<]+)</h1>\\s*<div class=\"videoplayer-controlbar\">").getMatch(0);
        if (betterFileTitle == null) {
            betterFileTitle = new Regex(html, "<h5 class=\"text-white\"[^>]*>([^<]+)</h5>").getMatch(0);
        }
        if (betterFileTitle != null) {
            /* Overwrite possible previous result since we know that this one is better. */
            fileInfo[0] = betterFileTitle;
        }
        return fileInfo;
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
    /* Auto handling works fine thus no override is needed. */
    // @Override
    // protected boolean isVideohoster_enforce_video_filename() {
    // return false;
    // }
}