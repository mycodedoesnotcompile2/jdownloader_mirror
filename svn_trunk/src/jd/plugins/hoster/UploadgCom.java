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
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52635 $", interfaceVersion = 3, names = {}, urls = {})
public class UploadgCom extends PluginForHost {
    public UploadgCom(PluginWrapper wrapper) {
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
        return "https://" + getHost() + "/pages/terms-of-service";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "uploadg.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/drive/s/([a-zA-Z0-9]{30})");

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

    private String turnstile_site_key         = null;
    private String internal_file_id           = null;
    private String internal_file_hash         = null;
    private Number download_countdown_seconds = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        /* Nullification */
        turnstile_site_key = null;
        internal_file_id = null;
        internal_file_hash = null;
        download_countdown_seconds = null;
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String json = br.getRegex("window\\.bootstrapData = (\\{.*?\\});").getMatch(0);
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        final Map<String, Object> linkinfo = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "loaders/shareableLinkPage/link");
        final Map<String, Object> entry = (Map<String, Object>) linkinfo.get("entry");
        link.setFinalFileName(entry.get("name").toString());
        link.setVerifiedFileSize(((Number) entry.get("file_size")).longValue());
        if (entry.get("deleted_at") != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Collect data we need for downloading */
        try {
            internal_file_id = linkinfo.get("id").toString();
            internal_file_hash = entry.get("hash").toString();
            final Map<String, Object> ads = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "settings/ads");
            turnstile_site_key = ads.get("turnstile_site_key").toString();
            download_countdown_seconds = (Number) ads.get("download_countdown_seconds");
        } catch (final Throwable e) {
            logger.log(e);
            logger.warning("Failed to collect all data required for downloading");
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
            if (StringUtils.isEmpty(turnstile_site_key)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String xsrftoken = br.getCookie(br.getHost(), "XSRF-TOKEN");
            if (StringUtils.isEmpty(xsrftoken)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
            final String cfTurnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, turnstile_site_key).getToken();
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("token", cfTurnstileResponse);
            if (download_countdown_seconds != null) {
                /**
                 * 2026-04-09: At this moment it's 5 seconds which means that captcha solving should always take longer than the time we
                 * need to wait.
                 */
                final long timePassedMillis = Time.systemIndependentCurrentJVMTimeMillis() - timeBefore;
                final long timeLeftMillis = (download_countdown_seconds.longValue() * 1000) - timePassedMillis;
                if (timeLeftMillis > 0) {
                    this.sleep(timeLeftMillis, link);
                } else {
                    /* Captcha solution required more time than the pre download seconds we are required to wait. */
                    logger.info("Pre download waittime seconds " + download_countdown_seconds + " passed during captcha");
                }
            } else {
                logger.info("No pre download wait required?");
            }
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Accept", "application/json");
            brc.getHeaders().put("Content-Type", "application/json");
            brc.getHeaders().put("x-xsrf-token", xsrftoken);
            // brc.getHeaders().put("", "");
            brc.postPageRaw("/api/v1/turnstile/verify", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(entries.get("success"))) {
                /* This should be a rare case */
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            /* We get this token but it's never needed anywhere. */
            // final String download_token = entries.get("download_token").toString();
            final String dllink = "/api/v1/file-entries/download/" + internal_file_hash + "?shareable_link=" + internal_file_id + "&password=null";
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
            /* Important otherwise we will get http response 403 */
            brc.getHeaders().put("Referer", "https://" + getHost() + "/");
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