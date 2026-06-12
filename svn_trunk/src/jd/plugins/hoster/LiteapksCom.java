//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
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

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.LiteapksComCrawler;

@HostPlugin(revision = "$Revision: 52895 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { LiteapksComCrawler.class })
public class LiteapksCom extends PluginForHost {
    public LiteapksCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* Connection stuff */
    private static final boolean free_resume       = true;
    private static final int     free_maxchunks    = 0;
    private static final int     free_maxdownloads = -1;
    private String               dllink            = null;

    public static List<String[]> getPluginDomains() {
        return LiteapksComCrawler.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/download/([\\w\\-]+)-(\\d+)/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        final Regex regex = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        return regex.getMatch(1) + "_" + regex.getMatch(2);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        if (!link.isNameSet()) {
            final Regex regex = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
            link.setName(regex.getMatch(0) + "_" + regex.getMatch(2) + ".apk");
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        dllink = br.getRegex("href=\"(https?://[^\"]+)\" download>").getMatch(0);
        if (dllink == null) {
            final String dllink_b64encoded = br.getRegex("data-link=\"(aHR0[^\"]+)").getMatch(0);
            if (dllink_b64encoded != null) {
                dllink = Encoding.Base64Decode(dllink_b64encoded);
                if (!dllink.contains("token=") && !dllink.contains("?")) {
                    /* 2026-06-11: See: https://liteapks.com/wp-content/themes/liteapks/js/site.js?ver=1773647695 */
                    long timeToLive = System.currentTimeMillis() / 1000 + 3600 * 3;
                    String step1 = Encoding.Base64Encode(Long.toString(timeToLive));
                    String token = Encoding.Base64Encode(step1);
                    dllink += "?token=" + token;
                }
            }
        }
        if (dllink != null) {
            final String filename = Plugin.getFileNameFromURL(new URL(dllink));
            if (filename != null) {
                link.setName(Encoding.htmlDecode(filename));
            }
        }
        String filesize = br.getRegex("id=\"download-size-label\"[^>]*>\\((\\d+[^<]+)\\)</span>").getMatch(0);
        if (filesize != null) {
            if (StringUtils.endsWithCaseInsensitive(filesize, "M")) {
                filesize += "b";
            }
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume, free_maxchunks);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}