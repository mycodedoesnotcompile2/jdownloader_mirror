//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53591 $", interfaceVersion = 3, names = {}, urls = {})
public class BlockfilestoreCom extends PluginForHost {
    public BlockfilestoreCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/terms";
    }

    private static List<String[]> getPluginDomains() {
        return jd.plugins.decrypter.BlockfilestoreComFolder.getPluginDomains();
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

    private static final Pattern PATTERN_API_DOWNLOAD = Pattern.compile("/api/download\\?id=([a-f0-9\\-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_DOWN         = Pattern.compile("/down/([a-f0-9\\-]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_API_DOWNLOAD.pattern().substring(1) + "|" + PATTERN_DOWN.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    private String getBaseURL() {
        return "https://www." + getHost();
    }

    /* Connection stuff */
    private static final boolean FREE_RESUME        = true;
    private static final int     FREE_MAXCHUNKS     = 0;
    private static final String  PROPERTY_DIRECTURL = "directurl";

    @Override
    public String getPluginContentURL(DownloadLink link) {
        final String file_id = this.getFID(link);
        if (file_id == null) {
            return null;
        }
        return getBaseURL() + "/api/download?id=" + file_id;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        final String url = link.getPluginPatternMatcher();
        final String fid = new Regex(url, PATTERN_API_DOWNLOAD).getMatch(0);
        if (fid != null) {
            return fid;
        }
        return new Regex(url, PATTERN_DOWN).getMatch(0);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    @SuppressWarnings("unchecked")
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        br.setFollowRedirects(true);
        br.getPage(getBaseURL() + "/api/file/get-public-file?fileId=" + getFID(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        if (result == null) {
            /* No file data returned --> File offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setFinalFileName(result.get("name").toString());
        link.setProperty(PROPERTY_DIRECTURL, result.get("url").toString());
        /* Size is optional: API may return null */
        final Object sizeO = result.get("size");
        if (sizeO != null) {
            link.setVerifiedFileSize(((Number) sizeO).longValue());
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(link, FREE_RESUME, FREE_MAXCHUNKS);
    }

    private void handleDownload(final DownloadLink link, final boolean resumable, final int maxchunks) throws Exception, PluginException {
        /* 2026-10-12: Pre download wait and some http requests which the browser is doing can be skipped atm. */
        final String directurl = link.getStringProperty(PROPERTY_DIRECTURL);
        if (StringUtils.isEmpty(directurl)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, resumable, maxchunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}