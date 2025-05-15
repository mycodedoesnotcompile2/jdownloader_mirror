//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
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

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51062 $", interfaceVersion = 2, names = {}, urls = {})
public class YourUploadCom extends PluginForHost {
    private static final Pattern PATTERN_EMBED = Pattern.compile("/embed/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_WATCH = Pattern.compile("/watch/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_FILE  = Pattern.compile("/download\\?file=(\\d+)", Pattern.CASE_INSENSITIVE);

    public YourUploadCom(PluginWrapper wrapper) {
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
        return "https://" + getHost() + "/index.php?act=pages&page=terms-of-service";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "yourupload.com" });
        ret.add(new String[] { "yucache.net" });
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_EMBED.pattern() + "|" + PATTERN_WATCH.pattern() + "|" + PATTERN_FILE.pattern() + ")");
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
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_WATCH).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_EMBED).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
        if (fid != null) {
            return fid;
        }
        return null;
    }

    @SuppressWarnings("deprecation")
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        final String file_id = this.getFID(link);
        if (!link.isNameSet()) {
            /* Set weak filename */
            link.setName(file_id + ".mp4");
        }
        String filename = null;
        String filesize = null;
        final Regex regex_file = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE);
        if (regex_file.patternFind()) {
            br.getPage(link.getPluginPatternMatcher());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            filename = br.getRegex("<h1>Download ([^<>\"]+)</h1>").getMatch(0);
        } else {
            /* Links of type "/watch/..." and "/embed/..." -> Use same format internally */
            final String contenturl = "https://www." + getHost() + "/watch/" + file_id;
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            filename = br.getRegex("class=\"container\"[^>]*>\\s*<h1>([^<]+)</h1>").getMatch(0);
            filesize = br.getRegex("Size\\s*</td>\\s*<td>([^<]+)</td>").getMatch(0);
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename.trim());
            link.setFinalFileName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final Regex regex_file = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE);
        if (!regex_file.patternFind()) {
            /* Aka "/watch/..." link or "/embed/..." link */
            final String nextStepURL = br.getRegex("(/download\\?file=\\d+)").getMatch(0);
            if (nextStepURL == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.getPage(nextStepURL);
        }
        String dllink = br.getRegex("data-url=\"(/download\\?[^\"]+)").getMatch(0);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dllink = Encoding.htmlOnlyDecode(dllink);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String etagHeader = dl.getConnection().getHeaderField("ETag");
        if (etagHeader != null && etagHeader.equalsIgnoreCase("W/\"3208b07-17e68b9dcf7\"")) {
            /* 2022-09-24: Big bucks bunny demo video */
            /* Content-Length: 52464391 */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        dl.startDownload();
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}