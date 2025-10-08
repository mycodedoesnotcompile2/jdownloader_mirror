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
import java.util.Map;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.PluginBrowser;

import org.appwork.utils.Regex;
import org.jdownloader.plugins.components.config.KVSConfig;
import org.jdownloader.plugins.components.config.KVSConfigRule34videoCom;

@HostPlugin(revision = "$Revision: 51622 $", interfaceVersion = 3, names = {}, urls = {})
public class Rule34videoCom extends KernelVideoSharingComV2 {
    public Rule34videoCom(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "rule34video.com", "rule34video.party" });
        ret.add(new String[] { "rule34gen.com" });
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
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultVideosPattern(getPluginDomains());
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlSlug) {
        return generateContentURLDefaultVideosPattern(host, fuid, urlSlug);
    }

    @Override
    protected String regexNormalTitleWebsite(final Browser br) {
        final Map<String, Object> videoObject = ((PluginBrowser) br).getVideoObject();
        if (videoObject != null) {
            final String name = (String) videoObject.get("name");
            if (name != null) {
                return name;
            }
        }
        return super.regexNormalTitleWebsite(br);
    }

    @Override
    protected AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        final AvailableStatus status = super.requestFileInformationWebsite(link, account, isDownload);
        /* Collect some information for custom filenames */
        String uploader = br.getRegex("class=\"avatar\"[^>]*title=\"([^\"]+)").getMatch(0);
        if (uploader == null) {
            /* 2024-10-07 */
            uploader = br.getRegex("src=\"[^\"]+/avatars/[^\"]+\" alt=\"[^\"]+\"/>\\s*</div>\\s*(.*?)\\s*<").getMatch(0);
            if (uploader == null) {
                uploader = br.getRegex(">\\s*Uploaded by\\s*</div>\\s*<a[^>]*>\\s*(?:<div.*?</div>|<span.*?</span>)\\s*(.*?)\\s*<").getMatch(0);
            }
        }
        if (uploader != null) {
            uploader = Encoding.htmlDecode(uploader).trim();
            // 'Uploaded by'
            link.setProperty(PROPERTY_USERNAME, uploader);
        }
        final String artistHTML = br.getRegex("<div[^>]*>Artist</div>(.*?)class\\s*=\\s*\"label\"\\s*>\\s*Uploaded by").getMatch(0);
        final String artists[] = new Regex(artistHTML, "class\\s*=\\s*\"name\"\\s*>\\s*(.*?)\\s*<").getColumn(0);
        if (artists != null) {
            final StringBuilder sb = new StringBuilder();
            for (String artist : artists) {
                if (sb.length() > 0) {
                    sb.append(",");
                }
                sb.append(artist);
            }
            link.setProperty("artist", sb.toString());
        }
        final String uploaddate = br.getRegex("\"uploadDate\"\\s*:\\s*\"([^\"]+)").getMatch(0);
        if (uploaddate != null) {
            link.setProperty(PROPERTY_DATE, uploaddate);
        }
        final String fuid = getFUIDFromURL(link.getPluginPatternMatcher());
        if (fuid != null && !link.hasProperty(PROPERTY_FUID)) {
            link.setProperty(PROPERTY_FUID, fuid);
        }
        return status;
    }

    @Override
    protected boolean isOfflineWebsite(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 403) {
            return true;
        } else {
            return super.isOfflineWebsite(br);
        }
    }

    @Override
    protected boolean preferTitleHTML() {
        return true;
    }

    @Override
    public Class<? extends KVSConfig> getConfigInterface() {
        return KVSConfigRule34videoCom.class;
    }
}