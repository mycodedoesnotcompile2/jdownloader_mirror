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

import org.appwork.exceptions.WTFException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

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

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class VikingfileCom extends PluginForHost {
    public VikingfileCom(PluginWrapper wrapper) {
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
        return "https://" + getHost() + "/terms";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vikingfile.com", "vikingf1le.us.to" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/f/([A-Za-z0-9]{10})");
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

    /** Docs: https://vikingfile.com/api */
    private String getApiBase() {
        return "https://" + getHost() + "/api";
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        try {
            final StringBuilder sb = new StringBuilder();
            final List<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* We check up to 100 links at once */
                    if (index == urls.length || links.size() == 100) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    sb.append("hash[]=" + this.getFID(link));
                    sb.append("&");
                }
                // remove last "&"
                sb.deleteCharAt(sb.length() - 1);
                br.postPageRaw(getApiBase() + "/check-file", sb.toString());
                final List<Map<String, Object>> items = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
                if (items == null || items.size() != links.size()) {
                    /* This should never happen */
                    throw new WTFException();
                }
                for (int i = 0; i < links.size(); i++) {
                    final DownloadLink link = links.get(i);
                    final String fid = this.getFID(link);
                    if (!link.isNameSet()) {
                        /* Set weak filename */
                        link.setName(fid);
                    }
                    final Map<String, Object> data = items.get(i);
                    if (Boolean.TRUE.equals(data.get("exist"))) {
                        link.setAvailable(true);
                        link.setFinalFileName(data.get("name").toString());
                        link.setVerifiedFileSize(((Number) data.get("size")).longValue());
                    } else {
                        /* Deleted file or invalid fileID. */
                        link.setAvailable(false);
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            return false;
        }
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformationAPI(link);
    }

    public AvailableStatus requestFileInformationAPI(final DownloadLink link) throws IOException, PluginException {
        checkLinks(new DownloadLink[] { link });
        if (!link.isAvailabilityStatusChecked()) {
            return AvailableStatus.UNCHECKED;
        } else if (link.isAvailabilityStatusChecked() && !link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            return AvailableStatus.TRUE;
        }
    }

    public AvailableStatus requestFileInformationWebsite(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("id=\"filename\"[^>]*>([^<]+)<").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        String filesize = br.getRegex("id=\"size\"[^>]*>([^<]+)<").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = "directurl_" + (account != null ? account.getType().getLabel() : null);
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformationAPI(link);
            requestFileInformationWebsite(link);
            final String cfkey = br.getRegex("sitekey\\s*:\\s*'([^']+)'").getMatch(0);
            if (cfkey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, cfkey);
            final String cfTurnstileResponse = ts.getToken();
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("cf-turnstile-response", cfTurnstileResponse);
            br.postPage(br.getURL(), query);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            dllink = entries.get("link").toString();
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                }
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean canHandle(final DownloadLink downloadLink, final Account account) throws Exception {
        return true;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return true;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}