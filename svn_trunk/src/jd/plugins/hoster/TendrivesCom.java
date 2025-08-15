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

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51329 $", interfaceVersion = 3, names = {}, urls = {})
public class TendrivesCom extends PluginForHost {
    public TendrivesCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final boolean SKIP_CAPTCHA = true;

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

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "10drives.com" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/b/([A-Za-z0-9]{10,})");
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getRequest().getHtmlCode().equals("404")) {
            /* Plaintext response with content "404" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("<div>\\s*File Not Found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = null, filesize = null;
        final String[] tables = br.getRegex("<table class=\"table\">(.*?)</table>").getColumn(0);
        for (final String table : tables) {
            if (!table.contains("<th>Filename")) {
                continue;
            }
            final String[] rows = new Regex(table, "<td>([^<]+)").getColumn(0);
            if (rows.length < 2) {
                continue;
            }
            filename = rows[0];
            filesize = rows[1];
            break;
        }
        if (filename == null) {
            /* 2025-08-14 */
            filename = br.getRegex("style=\"font-size: 24px; display: block; color: #1a1a1a; margin-bottom: 10px;\">([^<]+)</strong>").getMatch(0);
        }
        if (filesize == null) {
            /* 2025-08-14 */
            filesize = br.getRegex(">\\s*File Size:([^<]+)</span>").getMatch(0);
        }
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
        final String directlinkproperty = "directurl";
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            requestFileInformation(link);
            String continuelink = br.getRegex("href=\"(https://[^\"]+)\"[^>]*>\\s*Continue").getMatch(0);
            if (StringUtils.isEmpty(continuelink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (!SKIP_CAPTCHA) {
                if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    /* Untested/Unfinished code and thus IDE-only */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br);
                final String cfTurnstileResponse = ts.getToken();
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("secondaryToken", cfTurnstileResponse);
                query.appendEncoded("sitekey", ts.getSiteKey());
                br.postPage("/cdn-cgi/challenge-platform/h/b/rc/TODO_FIXME", query);
                final Browser brc = br.cloneBrowser();
                brc.getPage("https://10drives.com/op/setc?k=" + Encoding.urlEncode(cfTurnstileResponse));
            }
            br.getPage(continuelink);
            String dllink = br.getRegex("id=\"lsdwnbtn\" href=\"(https?://[^\"]+)").getMatch(0);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        if (SKIP_CAPTCHA) {
            return false;
        } else {
            return true;
        }
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    @Override
    protected void throwConnectionExceptions(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        super.throwConnectionExceptions(br, con);
        if (con.getResponseCode() == 502) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 502 bad gateway", 5 * 60 * 1000l);
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}