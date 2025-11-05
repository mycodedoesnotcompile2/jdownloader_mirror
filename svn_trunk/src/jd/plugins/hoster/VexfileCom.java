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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

@HostPlugin(revision = "$Revision: 51793 $", interfaceVersion = 3, names = {}, urls = {})
public class VexfileCom extends PluginForHost {
    public VexfileCom(PluginWrapper wrapper) {
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
        return "https://" + getHost() + "/tos";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vexfile.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/download/([A-Za-z0-9]+)");
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
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("class=\"card-dt\"[^>]*>\\s*<h4>([^<]+)</h4>").getMatch(0);
        if (StringUtils.isEmpty(filename)) {
            filename = br.getRegex("title:\\s*'([^'\"]+)").getMatch(0);
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesize = br.getRegex("class=\"file-size\"[^>]*>\\s*<[^>]*>\\s*</i>([^<]+)").getMatch(0);
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
            final String continuelink = br.getRegex("href=\"([^\"]+)\"[^>]*class=\"generate-link blocked\"").getMatch(0);
            if (continuelink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find continuelink");
            }
            final String file_id = br.getRegex("id=\"file_id\" value=\"(\\d+)\"").getMatch(0);
            if (file_id == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find file_id");
            }
            final String csrftoken = br.getRegex("'X-CSRF-TOKEN':\\s*'([^\"']+)").getMatch(0);
            if (csrftoken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find file_id");
            }
            final String cfTurnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br).getToken();
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("file_id", file_id);
            postdata.put("token", cfTurnstileResponse);
            final Browser brc = br.cloneBrowser();
            final Request req = brc.createJSonPostRequest("/verify-cf-captcha", JSonStorage.serializeToJson(postdata));
            req.getHeaders().put("Content-Type", "application/json");
            req.getHeaders().put("X-CSRF-TOKEN", csrftoken);
            /* This should set at least two cookies: XSRF-TOKEN, vexfilecom_session */
            brc.getPage(req);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (Boolean.FALSE.equals(entries.get("success"))) {
                /* This should be a rare case. */
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            br.getPage(continuelink);
            String dllink = br.getRegex("\"(/[^\"]+)\"[^>]*class=\"generate-link\"").getMatch(0);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            this.handleConnectionErrors(br, dl.getConnection());
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return true;
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
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}