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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.FilemoonSxCrawler;

@HostPlugin(revision = "$Revision: 52256 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { FilemoonSxCrawler.class })
public class BysejikuarCom extends PluginForHost {
    public BysejikuarCom(PluginWrapper wrapper) {
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

    private static List<String[]> getPluginDomains() {
        return FilemoonSxCrawler.getPluginDomains();
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2026-01-29: Website had multiple domain changes so we need this. */
        return this.rewriteHost(getPluginDomains(), host);
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:d|e|download)/([A-Za-z0-9]{12})");
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

    private String getApiBase() {
        // return "https://" + getHost() + "/api";
        /* 2026-01-29: Hardcoded domain */
        return "https://bysejikuar.com/api";
    }

    private Map<String, Object> dldata         = null;
    private String              chosen_quality = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dldata = null;
        chosen_quality = null;
        final String fid = this.getFID(link);
        br.getPage(getApiBase() + "/videos/" + fid + "/details");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        String filename = entries.get("title").toString();
        filename = Encoding.htmlDecode(filename).trim();
        if (!StringUtils.endsWithCaseInsensitive(filename, ".mp4")) {
            filename += ".mp4";
        }
        link.setFinalFileName(filename);
        final String description = (String) entries.get("description");
        if (!StringUtils.isEmpty(description) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
        if (!link.isSizeSet() || PluginEnvironment.DOWNLOAD.isCurrentPluginEnvironment()) {
            this.fetchDldata(link);
            final List<Map<String, Object>> dloptions = (List<Map<String, Object>>) this.dldata.get("options");
            long filesizeMax = -1;
            for (final Map<String, Object> dloption : dloptions) {
                final long size_bytes = ((Number) dloption.get("size_bytes")).longValue();
                if (size_bytes > filesizeMax || this.chosen_quality == null) {
                    filesizeMax = size_bytes;
                    this.chosen_quality = dloption.get("quality").toString();
                }
            }
            link.setDownloadSize(filesizeMax);
        }
        return AvailableStatus.TRUE;
    }

    private Map<String, Object> fetchDldata(final DownloadLink link) throws Exception {
        final String fid = this.getFID(link);
        br.getPage(getApiBase() + "/videos/" + fid + "/downloads");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        this.dldata = entries;
        return entries;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String directlinkproperty = "free_directlink";
        final String storedDirectlink = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirectlink != null) {
            logger.info("Trying to re-use stored directlink: " + storedDirectlink);
            dllink = storedDirectlink;
        } else {
            /* 2026-01-27: Typically 30 seconds */
            final int countdown_seconds = ((Number) this.dldata.get("countdown_seconds")).intValue();
            if (countdown_seconds >= 500) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            if (Boolean.TRUE.equals(this.dldata.get("recaptcha_required"))) {
                final String rcKey = this.dldata.get("recaptcha_site_key").toString();
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, rcKey).getToken();
                postdata.put("recaptcha_token", recaptchaV2Response);
            }
            final String fid = this.getFID(link);
            br.postPageRaw(this.getApiBase() + "/videos/" + fid + "/downloads/countdown", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String countdown_token = entries.get("countdown_token").toString();
            final Map<String, Object> postdata2 = new HashMap<String, Object>();
            postdata2.put("countdown_token", countdown_token);
            postdata2.put("quality", this.chosen_quality);
            this.sleep(countdown_seconds * 1000, link);
            br.postPageRaw(this.getApiBase() + "/videos/" + fid + "/downloads/authorize", JSonStorage.serializeToJson(postdata2));
            final Map<String, Object> entries2 = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String final_token = entries2.get("token").toString();
            dllink = "/api/videos/stream/" + final_token;
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            this.handleConnectionErrors(br, dl.getConnection());
        } catch (final Exception e) {
            if (storedDirectlink != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirectlink == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return true;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}