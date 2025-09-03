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
import java.util.Set;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.config.BeegComConfig;
import org.jdownloader.plugins.components.config.BeegComConfig.MODE;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51427 $", interfaceVersion = 2, names = { "beeg.com" }, urls = { "https?://(?:www\\.|beta\\.)?beeg\\.com/(-\\d+$|-?\\d+\\?t=\\d+-\\d+|-?\\d{8,}$)" })
public class BeegCom extends PluginForHost {
    private String dllink[] = null;

    public BeegCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/contacts/";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private static final String PROPERTY_IS_HLS  = "is_hls";
    private static final String PROPERTY_QUALITY = "what_quality";

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
        if (link.getPluginPatternMatcher() == null) {
            return null;
        } else {
            // remove leading 0 because
            // error": "code=400, message=parameter \"id\" in path has an error: value 0123456789012345: an invalid integer:
            return new Regex(link.getPluginPatternMatcher(), "/-?0*(\\d+)").getMatch(0);
        }
    }

    @Override
    public String getMirrorID(final DownloadLink link) {
        String fid = null;
        if (link != null && StringUtils.equals(getHost(), link.getHost()) && (fid = getFID(link)) != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getMirrorID(link);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    @Override
    public void clean() {
        super.clean();
        dllink = null;
    }

    private String getContentURL(final DownloadLink link) {
        String url = link.getPluginPatternMatcher();
        final String domainFromURL = Browser.getHost(url, true);
        /* Remove subdomains such as "www." or "beta.".s */
        url = url.replaceFirst(Pattern.quote(domainFromURL), this.getHost());
        return url;
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        dllink = null;
        final String extDefault = ".mp4";
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + extDefault);
        }
        final String videoidOriginal = getFID(link);
        if (videoidOriginal == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        this.setBrowserExclusive();
        String title = null;
        String extraParams = "";
        final String timeParams = UrlQuery.parse(link.getPluginPatternMatcher()).get("t");
        final Regex timeParamsRegex = new Regex(timeParams, "(\\d+)-(\\d+)");
        if (timeParams != null && timeParamsRegex.matches()) {
            extraParams = "?fc_start=" + timeParamsRegex.getMatch(0) + "&fc_end=" + timeParamsRegex.getMatch(1);
        }
        br.getPage("https://store.externulls.com/facts/file/" + videoidOriginal + extraParams);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getRequest().getHtmlCode().length() < 100) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        /* 2021-07-26: Seems like all objects in this array lead to the same video. */
        final List<Map<String, Object>> vids = (List<Map<String, Object>>) entries.get("fc_facts");
        final Map<String, Object> file = (Map<String, Object>) entries.get("file");
        final List<Map<String, Object>> data = (List<Map<String, Object>>) file.get("data");
        final Map<String, Object> stuff = (Map<String, Object>) file.get("stuff");
        if (stuff != null) {
            /* 2024-05-06: The "stuff" map si not always given. */
            title = (String) stuff.get("sf_name");
        }
        if (StringUtils.isEmpty(title) && data != null && data.size() > 0) {
            title = (String) data.get(0).get("cd_value");
        }
        Map<String, String> qualities_http = null;
        Map<String, String> qualities_hls = null;
        for (final Map<String, Object> vid : vids) {
            qualities_http = (Map<String, String>) vid.get("resources");
            qualities_hls = (Map<String, String>) vid.get("hls_resources");
            if (qualities_http != null || qualities_hls != null) {
                break;
            }
        }
        if (qualities_http == null || qualities_http.size() == 0) {
            /* E.g. videos without "t" parameter inside URL. */
            qualities_http = (Map<String, String>) file.get("resources");
        }
        if (qualities_hls == null || qualities_hls.size() == 0) {
            qualities_hls = (Map<String, String>) file.get("hls_resources");
        }

        Boolean isHLS = null;
        String mp4URL[] = null;
        if (qualities_http != null) {
            mp4URL = filter(qualities_http, link);
        }
        String hlsURL[] = null;
        if (qualities_hls != null) {
            hlsURL = filter(qualities_hls, link);
        }
        if (mp4URL != null && hlsURL != null) {
            if (isHLS(link) || BeegComConfig.QUALITY.valueOf(hlsURL[0]).ordinal() < BeegComConfig.QUALITY.valueOf(mp4URL[0]).ordinal()) {
                dllink = hlsURL;
                isHLS = true;
            } else {
                dllink = mp4URL;
                isHLS = false;
            }
        } else if (hlsURL != null) {
            dllink = hlsURL;
            isHLS = true;
        } else if (mp4URL != null) {
            dllink = mp4URL;
            isHLS = false;
        }
        if (isHLS(link) && !Boolean.TRUE.equals(isHLS)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (dllink != null && !StringUtils.isEmpty(dllink[1]) && !dllink[1].startsWith("http")) {
            dllink[1] = "https://video.beeg.com/" + dllink[1];
        }
        if (!StringUtils.isEmpty(title)) {
            title = title.trim();
            link.setFinalFileName(this.applyFilenameExtension(title, extDefault));
        }
        br.getHeaders().put("Referer", link.getPluginPatternMatcher());
        if (dllink != null) {
            link.setProperty(PROPERTY_QUALITY, dllink[0]);
            if (Boolean.TRUE.equals(isHLS)) {
                link.setProperty(PROPERTY_IS_HLS, true);
            } else {
                link.removeProperty(PROPERTY_IS_HLS);
            }
            if (!isDownload && !isHLS) {
                basicLinkCheck(br.cloneBrowser(), br.createGetRequest(dllink[1]), link, link.getFinalFileName(), extDefault);
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (isHLS(link)) {
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, this.dllink[1]);
        } else {
            /* 2025-08-04: Important: Avoid wrong referer! */
            // final Browser br_download = br;
            final Browser br_download = this.createNewBrowserInstance();
            br_download.getHeaders().put("Referer", link.getPluginPatternMatcher());
            dl = jd.plugins.BrowserAdapter.openDownload(br_download, link, dllink[1], true, 0);
            handleConnectionErrors(br_download, dl.getConnection());
        }
        dl.startDownload();
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private boolean isHLS(final DownloadLink link) {
        if (link.hasProperty(PROPERTY_IS_HLS)) {
            return true;
        } else {
            return false;
        }
    }

    private String decryptKey(final String key, final String salt) {
        String decodeKey = Encoding.htmlDecode(key);
        int s = salt.length();
        StringBuffer t = new StringBuffer();
        for (int o = 0; o < decodeKey.length(); o++) {
            char l = decodeKey.charAt(o);
            int n = o % s;
            int i = salt.charAt(n) % 21;
            t.append(String.valueOf(Character.toChars(l - i)));
        }
        String result = t.toString();
        result = strSplitReverse(result, 3, true);
        return result;
    }

    protected BeegComConfig.QUALITY[] sort(final DownloadLink link) throws Exception {
        final List<BeegComConfig.QUALITY> ret = new ArrayList<BeegComConfig.QUALITY>();
        final String selectedQuality = link.getStringProperty(PROPERTY_QUALITY);
        if (selectedQuality != null) {
            try {
                ret.add(BeegComConfig.QUALITY.valueOf(selectedQuality));
            } catch (Exception ignore) {
            }
        }
        if (ret.size() == 0) {
            final BeegComConfig config = PluginJsonConfig.get(getConfigInterface());
            final Set<BeegComConfig.QUALITY> qualities;
            final MODE mode = config.getQualityMode();
            switch (mode) {
            case BEST:
                qualities = null;
                break;
            case BEST_SELECTED:
                qualities = config.getPreferredQuality();
                break;
            default:
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported:" + mode);
            }
            for (final BeegComConfig.QUALITY quality : BeegComConfig.QUALITY.values()) {
                if (qualities == null || qualities.contains(quality)) {
                    ret.add(quality);
                }
            }
        }
        return ret.toArray(new BeegComConfig.QUALITY[0]);
    }

    protected String[] filter(final Map<String, String> results, final DownloadLink link) throws Exception {
        for (BeegComConfig.QUALITY quality : sort(link)) {
            final String result;
            switch (quality) {
            case Q2160P:
                result = results.get("fl_cdn_2160");
                break;
            case Q1080P:
                result = results.get("fl_cdn_1080");
                break;
            case Q720P:
                result = results.get("fl_cdn_720");
                break;
            case Q480P:
                result = results.get("fl_cdn_480");
                break;
            case Q360P:
                result = results.get("fl_cdn_360");
                break;
            case Q240P:
                result = results.get("fl_cdn_240");
                break;
            default:
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported:" + quality);
            }
            if (StringUtils.isNotEmpty(result)) {
                return new String[] { quality.name(), result };
            }
        }
        return null;
    }

    @Override
    public Class<BeegComConfig> getConfigInterface() {
        return BeegComConfig.class;
    }

    private String strSplitReverse(final String key, final int e, final boolean t) {
        String n = key;
        StringBuffer r = new StringBuffer();
        if (t) {
            int a = n.length() % e;
            if (a > 0) {
                r.append(new StringBuffer(n.substring(0, a)).reverse());
                n = n.substring(a);
            }
        }
        for (; n.length() > e;) {
            r.append(new StringBuffer(n.substring(0, e)).reverse());
            n = n.substring(e);
        }
        r.append(new StringBuffer(n).reverse());
        return r.reverse().toString();
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
        link.removeProperty(PROPERTY_IS_HLS);
        link.removeProperty(PROPERTY_QUALITY);
    }

    @Override
    public void resetPluginGlobals() {
    }
}