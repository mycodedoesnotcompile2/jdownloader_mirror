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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51616 $", interfaceVersion = 3, names = {}, urls = {})
public class StreamflashSx extends PluginForHost {
    public StreamflashSx(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2025-07-08: turbostream.tv is now streamflash.sx */
        return this.rewriteHost(getPluginDomains(), host);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "streamflash.sx", "turbostream.tv" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("turbostream.tv");
        return deadDomains;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_WATCH    = Pattern.compile("/watch\\.php\\?(?:id|v)=([a-zA-Z0-9]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_DOWNLOAD = Pattern.compile("/download_page\\.php\\?video_id=([a-zA-Z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_WATCH.pattern() + "|" + PATTERN_DOWNLOAD.pattern() + ")");
        }
        return ret.toArray(new String[0]);
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
        String fid = new Regex(url, PATTERN_WATCH).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(url, PATTERN_DOWNLOAD).getMatch(0);
        return fid;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    private String getContentURL(final DownloadLink link) {
        if (link == null) {
            return null;
        }
        final String originalURL = link.getPluginPatternMatcher();
        if (originalURL == null) {
            return null;
        }
        /* link cleanup, prefer https if possible */
        try {
            final URL url = new URL(originalURL);
            final String urlHost = this.getHost();
            final String urlHostWithoutWww = urlHost.replaceFirst("(?i)www\\.", "");
            final String protocol = "https://";
            /* Get full host with subdomain and correct base domain. */
            final String pluginHost = this.getHost();
            final List<String> deadDomains = this.getDeadDomains();
            final String host;
            if (deadDomains != null && (deadDomains.contains(urlHost) || deadDomains.contains(urlHostWithoutWww))) {
                /* Fallback to plugin domain */
                /* e.g. down.xx.com -> down.yy.com, keep subdomain(s) */
                host = urlHost.replaceFirst("(?i)" + Pattern.quote(Browser.getHost(url, false)) + "$", pluginHost);
            } else {
                /* Use preferred host */
                host = urlHost;
            }
            if (new Regex(originalURL, PATTERN_DOWNLOAD).patternFind()) {
                /* Build URL-type we need. */
                return protocol + host + "/watch.php?v=" + this.getFID(link);
            } else {
                return originalURL.replaceFirst("(?i)^https?://[^/]+", protocol + host);
            }
        } catch (final MalformedURLException e) {
            /* Return unmodified url. */
            logger.log(e);
            return originalURL;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        final String ext_default = ".mp4";
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(fid + ext_default);
        }
        this.setBrowserExclusive();
        br.getPage(this.getContentURL(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*This video was deleted")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* 2025-07-08: Filename in h5 tag is only available for original file uploader */
        String filename = br.getRegex("<h5 style=\"color: #e0e0e0; font-weight: 500;\"[^>]*>([^<]+)</h5>").getMatch(0);
        if (StringUtils.isEmpty(filename)) {
            /* Mimic original website title */
            filename = "Video #" + fid;
        }
        filename = this.correctOrApplyFileNameExtension(filename, ext_default, null);
        filename = Encoding.htmlDecode(filename).trim();
        link.setFinalFileName(filename);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String fid = this.getFID(link);
        try {
            final String viewToken = br.getRegex("const viewToken = \"([a-f0-9]{64})\";").getMatch(0);
            if (viewToken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find viewToken");
            }
            final Browser brc = br.cloneBrowser();
            brc.getPage("/record_view.php?id=" + fid + "&token=" + viewToken);
        } catch (final Throwable e) {
            logger.log(e);
            logger.info("View count handling failed");
        }
        final String officialVideoDownload = br.getRegex("\"(download_page\\.php\\?video_id=" + fid + ")").getMatch(0);
        if (officialVideoDownload != null) {
            /* Prefer official video download */
            logger.info("Performing official video download");
            br.getPage(officialVideoDownload);
            final Form dlform = br.getFormbyKey("video_id");
            if (dlform == null) {
                if (br.getHttpConnection().getResponseCode() == 403) {
                    /* Along with plaintext response "Downloads are not allowed for this video." */
                    /*
                     * This should never happen because the download button existed before. It could theoretically happen if the uploader
                     * decided to disable official video downloads in exactly the moment where we are trying to perform an official video
                     * download.
                     */
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Downloads are not allowed for this video");
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, this.isResumeable(link, null), this.getMaxChunks(link, null));
        } else {
            logger.info("Performing stream download");
            final String dllink = "/stream.php?id=" + fid + "&type=video";
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        }
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}