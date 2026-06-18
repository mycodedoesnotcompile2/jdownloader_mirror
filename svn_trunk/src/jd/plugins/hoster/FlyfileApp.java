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
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;

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

@HostPlugin(revision = "$Revision: 52911 $", interfaceVersion = 3, names = {}, urls = {})
public class FlyfileApp extends PluginForHost {
    public FlyfileApp(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
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
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "flyfile.app" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/v/([a-zA-Z0-9]{10,})/?");
    private final String         API_BASE       = "https://api.flyfile.app/api";

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
        return this.getFID(link) + ".mp4";
    }

    private Map<String, Object> entries = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        /* Nullification */
        entries = null;
        this.setBrowserExclusive();
        br.getPage(API_BASE + "/public/file/" + this.getFID(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* e.g. {"message":"Invalid or expired link"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        link.setFinalFileName(entries.get("name").toString());
        final Map<String, Object> videoSettings = (Map<String, Object>) entries.get("videoSettings");
        if (Boolean.TRUE.equals(videoSettings.get("disableDownload"))) {
            /* Official download is disabled -> Set size of best stream download */
            /* Find highest stream quality and set file size */
            final Map<String, Object> videoAsset = (Map<String, Object>) entries.get("videoAsset");
            final List<Map<String, Object>> qualities = (List<Map<String, Object>>) videoAsset.get("qualities");
            Map<String, Object> bestQuality = null;
            long bestSizeBytes = -1;
            for (final Map<String, Object> quality : qualities) {
                if (!"READY".equalsIgnoreCase(quality.get("status").toString())) {
                    continue;
                }
                final long sizeBytes = ((Number) quality.get("sizeBytes")).longValue();
                if (sizeBytes > bestSizeBytes) {
                    bestSizeBytes = sizeBytes;
                    bestQuality = quality;
                }
            }
            if (bestQuality != null) {
                link.setDownloadSize(((Number) bestQuality.get("sizeBytes")).longValue());
            }
        } else {
            /* Official download is allowed -> Set size of original file */
            link.setVerifiedFileSize(((Number) entries.get("size")).longValue());
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String status = entries.get("status").toString();
        if (!StringUtils.equalsIgnoreCase(status, "READY")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Unexpected file status: " + status);
        }
        final String fid = entries.get("token").toString();
        final Map<String, Object> videoSettings = (Map<String, Object>) entries.get("videoSettings");
        final String dllink;
        if (Boolean.TRUE.equals(videoSettings.get("disableDownload"))) {
            /* Official download is disabled -> Download stream */
            br.getPage("/api/streaming/assign/" + fid);
            final Map<String, Object> streaminfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String stream_host = streaminfo.get("url").toString();
            final String stream_token = streaminfo.get("token").toString();
            final String hlsMaster = stream_host + "/hls/" + stream_token + "/master.m3u8";
            br.getPage(hlsMaster);
            final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(this.br));
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, hlsbest.getDownloadurl());
            dl.startDownload();
        } else {
            /* Official download is allowed -> Preferably do that. */
            final boolean skipCaptchaViaSpecialWorkaround = true;
            if (skipCaptchaViaSpecialWorkaround) {
                /*
                 * 2026-06-17: Special: Streaming URLs can also be used for official download and generating them works without the need of
                 * solving a captcha.
                 */
                br.getPage("/api/streaming/assign/" + fid);
                final Map<String, Object> streaminfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final String stream_host = streaminfo.get("url").toString();
                final String stream_token = streaminfo.get("token").toString();
                dllink = stream_host.replace("streaming-", "downloader-") + "/download/" + stream_token;
            } else {
                br.getHeaders().put("Accept", "application/json, text/plain, */*");
                br.getHeaders().put("Origin", "https://flyfile.app");
                br.getHeaders().put("Referer", "https://flyfile.app/download/" + fid);
                // br.getHeaders().put("", "");
                // br.getHeaders().put("", "");
                String nonce = null;
                final boolean requiresNonce = true;
                if (requiresNonce) {
                    /**
                     * TODO: 2026-06-17: fix this request, it always returns: <br>
                     * {"statusCode":415,"code":"FST_ERR_CTP_INVALID_MEDIA_TYPE","error":"Unsupported Media Type","message":"Unsupported
                     * Media Type"}
                     */
                    br.postPageRaw("/api/download/nonce", "");
                    final Map<String, Object> resp_nonce = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    nonce = resp_nonce.get("nonce").toString();
                }
                /* Hardcoded Turnstile sitekey date: 2026-06-17 */
                final String cfTurnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, "0x4AAAAAADQoQ3fUclo7VmxF").getToken();
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("hasAdblock", false);
                postdata.put("nonce", "");
                postdata.put("quality", "original");
                postdata.put("turnstileToken", cfTurnstileResponse);
                br.postPageRaw("/api/download/assign/" + fid, JSonStorage.serializeToJson(postdata));
                final Map<String, Object> dlinfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final String dl_host = dlinfo.get("url").toString();
                final String dl_token = dlinfo.get("token").toString();
                dllink = dl_host + "/download/" + dl_token;
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            this.handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        /* 2026-06-17: Cloudflare Turnstile captcha */
        return true;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}