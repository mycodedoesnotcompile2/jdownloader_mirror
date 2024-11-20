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

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 50177 $", interfaceVersion = 3, names = {}, urls = {})
public class StreamtapeCom extends PluginForHost {
    public StreamtapeCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-and-conditions";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "streamtape.com", "streamtape.cc", "streamtape.net", "streamtape.to", "tapecontent.net", "strtape.tech", "strcloud.in", "strcloud.club", "strcloud.link", "shavetape.cash", "streamta.pe", "strtpe.link", "streamadblocker.xyz", "strtape.cloud", "tapeadvertisement.com", "streamta.site", "streamtape.xyz", "watchadsontape.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return Plugin.buildAnnotationNames(StreamtapeCom.getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return this.buildSupportedNames(StreamtapeCom.getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : StreamtapeCom.getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + Plugin.buildHostsPatternPart(domains) + "/(?:v|e)/([A-Za-z0-9\\-_]+)");
        }
        return ret.toArray(new String[0]);
    }

    /* Connection stuff */
    private static final boolean FREE_RESUME       = true;
    private static final int     FREE_MAXCHUNKS    = 0;
    private static final int     FREE_MAXDOWNLOADS = 20;

    // private static final boolean ACCOUNT_FREE_RESUME = true;
    // private static final int ACCOUNT_FREE_MAXCHUNKS = 0;
    // private static final int ACCOUNT_FREE_MAXDOWNLOADS = 20;
    // private static final boolean ACCOUNT_PREMIUM_RESUME = true;
    // private static final int ACCOUNT_PREMIUM_MAXCHUNKS = 0;
    // private static final int ACCOUNT_PREMIUM_MAXDOWNLOADS = 20;
    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = this.getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    /** 2020-04-21: They got an API which may be interesting for account support: https://streamtape.com/api */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String extDefault = ".mp4";
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + extDefault);
        }
        this.setBrowserExclusive();
        this.br.setFollowRedirects(true);
        this.br.setAllowedResponseCodes(new int[] { 500 });
        final String passCode = link.getDownloadPassword();
        if (passCode != null) {
            /* Make sure that what we get is a valid URL. */
            String referer = passCode;
            if (!passCode.startsWith("http")) {
                referer = "https://" + passCode;
            }
            try {
                this.br.setCurrentURL(new URL(referer).toString());
            } catch (MalformedURLException e) {
                this.logger.log(e);
                logger.info("Invalid referer value provided by user: " + passCode);
            }
        }
        this.br.getPage(link.getPluginPatternMatcher());
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.containsHTML(">\\s*Video not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String titleSafe = br.getRegex("\"showtitle\":\"([^\"]+)").getMatch(0);
        String title = this.br.getRegex("name=\"og:title\" content=\"([^<>\"]+)\"").getMatch(0);
        if (titleSafe != null) {
            /* Safe source -> Set final filename */
            titleSafe = PluginJSonUtils.unescape(titleSafe);
            final String filename = this.applyFilenameExtension(titleSafe, extDefault);
            link.setFinalFileName(filename);
        } else if (title != null) {
            /* "Unsafe" source -> Do notset final filename */
            title = Encoding.htmlDecode(title).trim();
            final String filename = this.applyFilenameExtension(title, extDefault);
            link.setName(filename);
        }
        String filesize = this.br.getRegex("<p class=\"subheading\">([^<>\"]+)</p>").getMatch(0);
        if (filesize != null) {
            /* They're using different decimal separators than normal -> Fix that for our parser. */
            filesize = filesize.replace(",", "").replace(".", ",");
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        this.requestFileInformation(link);
        this.doFree(link, StreamtapeCom.FREE_RESUME, StreamtapeCom.FREE_MAXCHUNKS, "free_directlink");
    }

    private void doFree(final DownloadLink link, final boolean resumable, final int maxchunks, final String directlinkproperty) throws Exception, PluginException {
        String dllink = this.checkDirectLink(link, directlinkproperty);
        if (dllink == null) {
            /* 2021-01-04 cat/mouse */
            dllink = this.br.getRegex("(id=[^\"'&]*&expires=\\d+&ip=[^\"'&]*&token=[^\"'&]*?)('|\"|<)").getMatch(0);
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* Build full URL */
            dllink = Encoding.htmlOnlyDecode(dllink);
            dllink = "//streamtape.com/get_video?" + dllink + "&stream=1";
            /* 2021-08-09: New attempt: Fix URL as we know that the "token" value may be wrong. */
            final String correctedToken = this.br.getRegex("document\\.getElementById[^<]*\\&token=([A-Z0-9\\-_]+)").getMatch(0);
            if (correctedToken != null) {
                // final String urlWithoutParams = dllink.substring(0, dllink.lastIndexOf("?"));
                final UrlQuery query = UrlQuery.parse(dllink);
                query.addAndReplace("token", correctedToken);
                dllink = query.toString();
                dllink = "//streamtape.com/get_video?" + dllink;
            }
            if (StringUtils.isEmpty(dllink)) {
                this.logger.warning("Failed to find final downloadurl");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        this.br.getHeaders().put(Plugin.OPEN_RANGE_REQUEST);
        this.dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, dllink, resumable, maxchunks);
        if (!this.looksLikeDownloadableContent(this.dl.getConnection())) {
            try {
                this.br.followConnection(true);
            } catch (IOException e) {
                this.logger.log(e);
            }
            if (this.dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (this.dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else if (this.dl.getConnection().getCompleteContentLength() == 7975279) {
            /* Dummy video --> Cat mouse games */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setProperty(directlinkproperty, this.dl.getConnection().getURL().toExternalForm());
        this.dl.startDownload();
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        final String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = this.br.cloneBrowser();
                br2.setFollowRedirects(true);
                con = br2.openHeadConnection(dllink);
                if (!this.looksLikeDownloadableContent(con)) {
                    throw new IOException();
                } else {
                    return dllink;
                }
            } catch (final Exception e) {
                this.logger.log(e);
                link.setProperty(property, Property.NULL);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        } else {
            return null;
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return StreamtapeCom.FREE_MAXDOWNLOADS;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}