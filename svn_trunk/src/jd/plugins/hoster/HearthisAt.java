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

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLSearch;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.DownloadInterface;

@HostPlugin(revision = "$Revision: 54166 $", interfaceVersion = 3, names = {}, urls = {})
public class HearthisAt extends PluginForHost {
    public HearthisAt(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* Each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0] -> main domain */
        ret.add(new String[] { "hearthis.at" });
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

    /* Group 1: username, group 2: track title/slug */
    private static final Pattern PATTERN_FILE = Pattern.compile("/([^/]+)/([A-Za-z0-9-\\.]+)/?", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE.pattern());
        }
        return ret.toArray(new String[0]);
    }

    /**
     * PATTERN_FILE only describes the path, so it must be applied behind the protocol and host. Otherwise it could match parts of the
     * protocol/host.
     */
    private Regex getURLInfo(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), Pattern.compile("^https?://[^/]+" + PATTERN_FILE.pattern(), Pattern.CASE_INSENSITIVE));
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE, "en-gb, en;q=0.9");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.AUDIO_STREAMING };
    }

    /* DEV NOTES */
    // Tags:
    // protocol: no https
    // other:
    /* Connection stuff */
    private boolean             free_resume    = false;
    private int                 free_maxchunks = 1;
    private String              dllink         = null;
    private static final String extDefault     = ".mp3";

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/nutzungsbedingungen/";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        }
        return super.getLinkID(link);
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return fid + extDefault;
        }
        return super.getDefaultFileName(link);
    }

    private String getFID(final DownloadLink link) {
        final Regex urlinfo = getURLInfo(link);
        return urlinfo.getMatch(0) + " - " + urlinfo.getMatch(1);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final boolean isDownload = PluginEnvironment.DOWNLOAD.isCurrentPluginEnvironment();
        dllink = null;
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final Regex urlinfo = getURLInfo(link);
        final String url_title = urlinfo.getMatch(1);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (StringUtils.containsIgnoreCase(br.getURL(), "/search/")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        // String filename = br.getRegex("data-playlist-title=\"([^<>\"]*?)\"").getMatch(0); // Always get "pause" (1st match)
        String title = br.getRegex("<title>\\s*([^<>]*?)\\s*\\|[^<>]+</title>").getMatch(0);
        if (title == null) {
            /* Fallback */
            title = urlinfo.getMatch(0) + " - " + url_title;
        }
        // final String downloadlink_stream = br.getRegex("data-mp3=\"(http[^<>\"]*?)\"").getMatch(0); // Always get dummy link (1st)
        String downloadlink_stream = HTMLSearch.searchMetaTag(br, "og:audio");
        if (downloadlink_stream == null) {
            final String[] mp3Links = br.getRegex("data-(?:mp3|src)\\s*=\\s*\"(http[^<>\"]*?)\"").getColumn(0);
            for (final String mp3Link : mp3Links) {
                if (!mp3Link.contains("/untitled/")) {
                    downloadlink_stream = mp3Link;
                    break;
                }
            }
        }
        br.setFollowRedirects(true);
        /**
         * The filesize of the official download is shown next to the download button, e.g. 'Download (18 MB)' or, if the user needs to be
         * logged in and has to follow the uploader, 'Follow to Download (162 MB)'. Set it in both cases and even if we end up using the
         * stream. This also makes the (slow) direct-URL check below obsolete in many cases. <br>
         * 2026-11-25: File sizes of stream and official download look to be identical.
         */
        /*
         * Get the complete HTML element (<a> or <button>) of the download button. Attribute values are skipped as a whole because they may
         * contain '>' (e.g. in CSS classes). All information is taken from this element only, independent of the website language (the
         * aria-label is translated, e.g. 'Folgen zum Download (162 MB)').
         */
        /**
         * Example account required for official download: <br>
         * /mixkatalog/art-of-mix-art-of-compilation-volume-6/ <br>
         * Example official download possible without account: <br>
         * /markoannderofficial/cosmic-fate-aoz/
         */
        final String officialDownloadButtonHTML = br.getRegex("(<(?:a|button)\\s(?:[^>\"]|\"[^\"]*\")*?data-player-action=\"download\"(?:[^>\"]|\"[^\"]*\")*>)").getMatch(0);
        /*
         * 'data-lock' (e.g. data-lock="login") means that the download is locked and can only be used when logged in and following the
         * user.
         */
        final boolean officialDownloadRequiresAccount = officialDownloadButtonHTML != null && officialDownloadButtonHTML.contains("data-lock=");
        final String officialDownloadFilesize = officialDownloadButtonHTML != null ? new Regex(officialDownloadButtonHTML, "aria-label=\"[^\"]*\\(\\s*([^\\)\"]+?)\\s*\\)\"").getMatch(0) : null;
        if (officialDownloadFilesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(officialDownloadFilesize));
        }
        if (officialDownloadRequiresAccount) {
            logger.info("Official downloadurl found but it can only be used when logged in and following the uploader");
        }
        find_official_downloadurl: {
            String officialDownloadURL = null;
            /*
             * 2026-11-25: Only use the URL of the 'Download' link ('href'). If the download button is locked (user has to be logged in and
             * follow the uploader), its URL ('data-href') cannot be used for downloading.
             */
            if (officialDownloadButtonHTML != null && !officialDownloadRequiresAccount) {
                officialDownloadURL = new Regex(officialDownloadButtonHTML, "\\shref=\"(https?://[^\"]+)\"").getMatch(0);
            }
            if (officialDownloadURL != null) {
                logger.info("Successfully found official downloadurl");
                this.dllink = officialDownloadURL;
            } else if (!officialDownloadRequiresAccount) {
                /* Locked download buttons are already logged above */
                logger.warning("Failed to find official downloadurl");
            }
        }
        if (dllink == null && downloadlink_stream != null) {
            /* We failed to get a 'real' downloadlink --> Fallback to stream (lower quality) */
            /* Stream urls can be downloaded with multiple connections and resume is possible! */
            free_resume = true;
            free_maxchunks = 1; // 2021-09-30: Disabled chunkload as it causes issues
            dllink = Encoding.htmlDecode(downloadlink_stream);
            logger.info("\n\n-> Failed to get download link -> Checking stream link: " + dllink + "\n");
        }
        title = Encoding.htmlDecode(title);
        title = title.trim();
        if (link.getFinalFileName() == null) {
            link.setFinalFileName(title + extDefault);
        }
        if (officialDownloadFilesize == null && !StringUtils.isEmpty(this.dllink) && !isDownload && !link.isSizeSet()) {
            final Browser br2 = br.cloneBrowser();
            br2.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, "https://" + this.getHost() + "/");
            final GetRequest request = br2.createGetRequest(dllink);
            request.getHeaders().put(OPEN_RANGE_REQUEST);
            basicLinkCheck(br2, request, link, link.getFinalFileName(), extDefault);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (link.getBooleanProperty("ticket89269", false)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        // required to enforce use of range requests
        /* 2026-11-25: Resume only works with new (unpublished) download core */
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume && DownloadInterface.isNewHTTPCore(), free_maxchunks);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
