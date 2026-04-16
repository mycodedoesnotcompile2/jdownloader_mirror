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
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52663 $", interfaceVersion = 3, names = {}, urls = {})
public class ImagepondNet extends PluginForHost {
    public ImagepondNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "imagepond.net" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_IMAGE_OR_VIDEO    = Pattern.compile("/i/([\\w\\-\\.]+)");
    private static final Pattern PATTERN_IMAGE             = Pattern.compile("/image/([\\w\\-\\.]+)");
    private static final Pattern PATTERN_VIDEO             = Pattern.compile("/videos/([\\w\\-\\.]+)");
    private static final String  PROPERTY_INTERNAL_FILE_ID = "internal_file_id";

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_IMAGE_OR_VIDEO.pattern().substring(1) + "|" + PATTERN_IMAGE.pattern().substring(1) + "|" + PATTERN_VIDEO.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        String fid = link.getStringProperty(PROPERTY_INTERNAL_FILE_ID);
        if (fid == null) {
            /* Get fid from url */
            fid = getFID(link);
        }
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return getFID(link.getPluginPatternMatcher());
    }

    private String getFID(final String url) {
        String fid = new Regex(url, PATTERN_IMAGE_OR_VIDEO).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(url, PATTERN_IMAGE).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(url, PATTERN_VIDEO).getMatch(0);
        return fid;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        /* 2026-04-15: Set to 1 as this plugin is mostly used to download very small files. */
        return 1;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        if (new Regex(link.getPluginPatternMatcher(), PATTERN_IMAGE).patternFind()) {
            /* We know that the file will be an image -> Return name with assumed file extension */
            return this.getFID(link) + ".jpeg";
        } else if (new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO).patternFind()) {
            /* We know that the file will be a video -> Return name with assumed file extension */
            return this.getFID(link) + ".mp4";
        } else {
            /* Can be image or video -> Return string without file extension */
            return this.getFID(link);
        }
    }

    /** Return url to overview page of media item. */
    private String getContentURL(final DownloadLink link) {
        final String original_url = link.getPluginPatternMatcher();
        final Regex regex_video = new Regex(original_url, PATTERN_VIDEO);
        if (regex_video.patternFind()) {
            /* Special case: Overview page differs from format of added url. */
            return "https://www." + getHost() + "/i/" + regex_video.getMatch(0);
        }
        return original_url;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        final String contenturl = getContentURL(link);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!link.hasProperty(PROPERTY_INTERNAL_FILE_ID)) {
            final String fid_from_browser_url = this.getFID(br.getURL());
            final String fid_from_added_url = this.getFID(link);
            if (fid_from_browser_url != null && !StringUtils.equals(fid_from_added_url, fid_from_browser_url)) {
                /* E.g. when user adds legacy "/image/..." links and they redirect to new "/i/..." url-format." */
                logger.info("Setting new file_id as property | id from added url: " + fid_from_added_url + " | real/new id: " + fid_from_browser_url);
                link.setProperty(PROPERTY_INTERNAL_FILE_ID, fid_from_browser_url);
            }
        }
        String filename = br.getRegex("property=\"og:title\" content=\"([^\"]+)").getMatch(0);
        String filesize = br.getRegex("\\d+×\\d+</span>\\s*<span>([^<]+)</span>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /* The id in this URL can be a different one than the id in the URL added by the user. */
        String dllink = br.getRegex("/i/\\w+/download").getMatch(-1);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dllink += "/file";
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
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