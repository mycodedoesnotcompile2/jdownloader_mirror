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
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PostRequest;
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
import jd.plugins.components.SiteType.SiteTemplate;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

@HostPlugin(revision = "$Revision: 52767 $", interfaceVersion = 3, names = {}, urls = {})
public class ZincdriveCom extends PluginForHost {
    public ZincdriveCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookie(getHost(), "adb", "0");
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "zincdrive.com", "zdrive.to" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/([a-zA-Z0-9]{12})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_NORMAL.pattern());
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
        br = createNewBrowserInstance();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("<title>Download File: ([^<]+) — ZincDrive\\s*</title>").getMatch(0);
        if (StringUtils.isEmpty(filename)) {
            filename = br.getRegex("\"name\":\\s*'([^\"']+)'").getMatch(0);
        }
        final String filesize = br.getRegex("\"contentSize\":\\s*'(\\d+[^\"']+)'").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        String csrftoken = br.getRegex("name=\"csrf-token\" content=\"([^\"]+)\"").getMatch(0);
        if (csrftoken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String fid = this.getFID(link);
        Browser brc = br.cloneBrowser();
        {
            final Form form0 = br.getFormbyProperty("id", "zdForm");
            if (form0 == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            form0.put("blocked", "1");
            form0.put("_token", Encoding.urlEncode(csrftoken));
            brc.submitForm(form0);
        }
        Map<String, Object> entries = null;
        gateway: {
            brc = br.cloneBrowser();
            final PostRequest post = brc.createPostRequest("/" + fid + "/gateway/proceed", "");
            /* This request gains us two cookies which are needed for the subsequent "/file" GET request. */
            post.setContentType("application/json");
            post.getHeaders().put("Accept", "*/*");
            post.getHeaders().put("Origin", "https://" + br.getHost());
            post.getHeaders().put("X-CSRF-TOKEN", csrftoken);
            brc.getPage(post);
            entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!("/" + fid + "/file").equals(entries.get("redirect_url"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        br.getPage("/" + fid + "/file");
        down_1Form: {
            final Form down_1Form = br.getFormbyProperty("id", "down_1Form");
            if (down_1Form == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            sleep(TimeUnit.SECONDS.toMillis(10), link);
            br.submitForm(down_1Form);
        }
        final Form down_2Form;
        down_2Form: {
            down_2Form = br.getFormbyProperty("id", "down_2Form");
            if (down_2Form == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            sleep(TimeUnit.SECONDS.toMillis(10), link);
            br.submitForm(down_2Form);
        }
        generate: {
            sleep(TimeUnit.SECONDS.toMillis(24), link);
            final PostRequest post = brc.createPostRequest("/" + fid + "/file/generate", "_dlt=" + down_2Form.getInputFieldByName("_dlt").getValue());
            post.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            post.getHeaders().put("X-CSRF-TOKEN", csrftoken);
            br.getPage(post);
            entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        }
        final String dllink = (String) entries.get("download_link");
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find final downloadurl");
            final String msg = StringUtils.firstNotEmpty((String) entries.get("message"), (String) entries.get("error"));
            if (msg != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
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

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.UnknownNewFilehosterScript2026;
    }
}