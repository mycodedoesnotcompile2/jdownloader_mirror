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
import java.util.List;
import java.util.Map;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51241 $", interfaceVersion = 3, names = {}, urls = {})
public class LixstreamCom extends PluginForHost {
    public LixstreamCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public void init() {
        super.init();
        Browser.setRequestIntervalLimitGlobal("lixstreamingcaio.com", 250);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/#/agreement/terms";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        /*
         * Current list of domains can be found here: https://lixstream.com/#/file -> Select an uploaded file -> Share -> Dialog pops up ->
         * See "Choose domain"
         */
        ret.add(new String[] { "lixstream.com", "dood-hd.com", "videymv.com", "videymv.net", "videy.tv", "doodmv.com", "doodmv.net", "doodtv.net", "poopmv.com", "poopmv.net", "poopmv.org", "teratvs.org", "vidcloudmv.org", "vide-q.com", "vide0.me", "teramv.com", "teraboxtv.net", "vidcloudtv.net", "videb.org" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("poopmv.com");
        return deadDomains;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:e|s)/([a-zA-Z0-9]{8,})");
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

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    private String dllink = null;

    @Override
    public void clean() {
        super.clean();
        dllink = null;
    }

    private final String ext_default = ".mp4";

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        final String fid = this.getFID(link);
        return fid + ext_default;
    }

    private void getPage(final DownloadLink link, Browser br, Request request) throws Exception {
        int round = 0;
        while (true) {
            final URLConnectionAdapter con = br.openRequestConnection(request);
            if (con.getResponseCode() == 400 && round++ < 5) {
                br.followConnection(true);
                sleep(150, link);
                request = request.cloneRequest();
            } else {
                br.followConnection();
                break;
            }
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        final String fid = this.getFID(link);
        this.setBrowserExclusive();
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Referer", link.getPluginPatternMatcher());
        Request request = brc.createPostRequest("https://api.lixstreamingcaio.com/v2/s/home/resources/" + fid, "");
        request.getHeaders().put("Content-Type", "application/json");
        getPage(link, brc, request);
        if (brc.getHttpConnection().getResponseCode() == 400) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server issues (error 400)", 15 * 60 * 1000l);
        } else if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
        if (files.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> fileinfo = files.get(0);
        final Number filesize = (Number) fileinfo.get("size");
        String filename = fileinfo.get("display_name").toString();
        /* Small filename correction: Some video files' names end with ".m3u8" -> Remove that suffix */
        filename = filename.replaceFirst("(?i)\\.m3u8$", "");
        filename = this.correctOrApplyFileNameExtension(filename, ext_default, null);
        link.setFinalFileName(filename);
        Boolean isProgressiveVideoStreamDownload = null;
        try {
            if (this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD) {
                final String internal_file_id = fileinfo.get("id").toString();
                final String url_thumbnail = fileinfo.get("thumbnail").toString();
                if (internal_file_id == null || url_thumbnail == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String uid = new Regex(url_thumbnail, "(?i)xbox-streaming/(\\d+)").getMatch(0);
                if (uid == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                request = brc.createRequest("/v2/s/assets/f?id=" + internal_file_id + "&uid=" + uid);
                request.getHeaders().put("Content-Type", "application/json");
                getPage(link, brc, request);
                final Map<String, Object> downloadinfo = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final String encrypted_downloadurl = downloadinfo.get("url").toString();
                try {
                    final byte[] crypted = Base64.decode(encrypted_downloadurl);
                    final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
                    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec("GNgN1lHXIFCQd8hSEZIeqozKInQTFNXj".getBytes("UTF-8"), "AES"), new IvParameterSpec("2Xk4dLo38c9Z2Q2a".getBytes("UTF-8")));
                    byte[] plain = cipher.doFinal(crypted);
                    dllink = new String(plain, "UTF-8");
                    isProgressiveVideoStreamDownload = StringUtils.endsWithCaseInsensitive(dllink, ".mp4");
                    link.setResumeable(isProgressiveVideoStreamDownload);
                } catch (Exception e) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, e);
                }
            }
        } finally {
            if (filesize != null) {
                if (Boolean.TRUE.equals(isProgressiveVideoStreamDownload)) {
                    /* Progressive video stream download -> We know the expected file size 100% -> Set verified filesize */
                    link.setVerifiedFileSize(filesize.longValue());
                } else {
                    link.setDownloadSize(filesize.longValue());
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
        }
        if (StringUtils.endsWithCaseInsensitive(dllink, ".mp4")) {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, getMaxChunks(link, null));
            handleConnectionErrors(br, dl.getConnection());
        } else {
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, dllink);
        }
        /* 2025-07-11: Prevent upper handling from using a filename from URL over the one we've set. */
        dl.setAllowFilenameFromURL(false);
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
}