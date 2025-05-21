//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51083 $", interfaceVersion = 2, names = { "5sing.kugou.com" }, urls = { "https?://(?:www\\.)?5sing\\.kugou\\.com/(f|y)c/(\\d+)\\.html" })
public class FiveSingCom extends PluginForHost {
    public FiveSingCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public FEATURE[] getFeatures() {
        return new FEATURE[] { FEATURE.AUDIO_STREAMING };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        /* 2025-05-15: They do not support https */
        final String contenturl = link.getPluginPatternMatcher().replaceFirst("https://", "http://");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("/images/404/error\\.jpg")) {
            /* Example: /fc/6359899.html */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String extension = br.getRegex("(<em>)?格式：(</em>)?([^<>\"]*?)(<|&)").getMatch(2);
        if (StringUtils.isEmpty(extension)) {
            extension = "mp3";
        }
        final String songTitle = br.getRegex("song_title\" title=\"([^<>\"]*?)\"").getMatch(0);
        final String song_id = br.getRegex("var SongID[^<>\"\t\n\r]*= ([^<>\"]*?);").getMatch(0);
        final String stype = br.getRegex("var SongType[^<>\"\t\n\r]*= \"([^<>\"]*?)\";").getMatch(0);
        /* 2025-05-19: File size is not always available in html code. */
        String filesize = br.getRegex("(<em>)?大小：(</em>)?([^<>\"]*?)(<|\")").getMatch(2);
        if (songTitle != null) {
            link.setFinalFileName(stype + "-" + Encoding.htmlDecode(songTitle).trim() + "-" + song_id + "." + Encoding.htmlDecode(extension).trim());
        } else {
            logger.warning("Failed to find filename information");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize + "b"));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        String src = br.getRegex("\"ticket\":\\s*\"([^\"]+)").getMatch(0);
        if (src == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        src = Encoding.Base64Decode(src);
        String dllink = new Regex(src, "\"file\":\"(https?:[^\"]*?)\"").getMatch(0);
        if (StringUtils.isEmpty(dllink)) {
            final Map<String, Object> map = restoreFromString(src, TypeRef.MAP);
            final String song_id = map.get("songID").toString();
            final String songType = map.get("songType").toString();
            if (StringUtils.isEmpty(songType) || StringUtils.isEmpty(song_id)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Browser brc = br.cloneBrowser();
            final boolean useNewAPI = true;
            final Map<String, Object> data;
            if (useNewAPI) {
                String mid = br.getCookie(br.getHost(), "kg_mid", Cookies.NOTDELETEDPATTERN);
                if (StringUtils.isEmpty(mid)) {
                    /* This is just a random UUID thing */
                    mid = Hash.getMD5(Long.toString(System.currentTimeMillis()));
                    br.setCookie(br.getHost(), "kg_mid", mid);
                }
                String dfid = br.getCookie(br.getHost(), "kg_dfid", Cookies.NOTDELETEDPATTERN);
                final String appkey = br.getRegex("appkey:\\s*'([^']+)'").getMatch(0);
                if (StringUtils.isEmpty(appkey)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String uuid = mid;
                final String appid = "2918";
                final String clienttime = Long.toString(System.currentTimeMillis() / 1000);
                final String clientver = "1000";
                final String version = "6.6.72";
                if (StringUtils.isEmpty(dfid)) {
                    logger.info("Obtaining fresh dfid");
                    final String platid = "4";
                    final String userid = "0";
                    final String thisClientVers = "0";
                    final UrlQuery query = new UrlQuery();
                    query.appendEncoded("appid", appid);
                    query.appendEncoded("platid", platid);
                    query.appendEncoded("clientver", thisClientVers);
                    query.appendEncoded("clienttime", clienttime);
                    query.appendEncoded("mid", mid);
                    query.appendEncoded("userid", userid);
                    query.appendEncoded("uuid", uuid);
                    query.appendEncoded("p.token", "");
                    final String signature = computeSign2(query, appid);
                    query.appendEncoded("signature", signature);
                    /* js fingerprinting: They don't really care what we send. */
                    final String fingerprintStuff = "{\"appCodeName\":\"\",\"appName\":\"\",\"appVersion\":\"\",\"connection\":\"\",\"doNotTrack\":\"\",\"hardwareConcurrency\":8,\"language\":\"\",\"languages\":\"\",\"maxTouchPoints\":0,\"mimeTypes\":\"\",\"platform\":\"\",\"plugins\":\"\",\"userAgent\":\"\",\"colorDepth\":24,\"pixelDepth\":24,\"screenResolution\":\"\",\"timezoneOffset\":-120,\"sessionStorage\":true,\"localStorage\":true,\"indexedDB\":true,\"cookie\":true,\"adBlock\":false,\"devicePixelRatio\":1,\"hasLiedOs\":false,\"hasLiedLanguages\":false,\"hasLiedResolution\":false,\"hasLiedBrowser\":false,\"webglRenderer\":\"\",\"webglVendor\":\"\",\"canvas\":\"\",\"fonts\":\"\",\"dt\":\"\",\"time\":\"\",\"userid\":\"\",\"mid\":\"" + mid + "\",\"uuid\":\"" + uuid + "\",\"appid\":1058,\"webdriver\":false,\"callPhantom\":false,\"tempKgMid\":\"\",\"referrer\":\"\",\"source\":\""
                            + link.getPluginPatternMatcher() + "\",\"clientAppid\":\"\",\"clientver\":\"\",\"clientMid\":\"\",\"clientDfid\":\"\",\"clientUserId\":\"\",\"audioKey\":\"\"}";
                    final String fingerprintStuffB64 = Encoding.Base64Encode(fingerprintStuff);
                    brc.postPageRaw("https://userservice.kugou.com/risk/v1/r_register_dev?" + query.toString(), fingerprintStuffB64);
                    final Map<String, Object> riskmap = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                    final Object dataO = riskmap.get("data");
                    if (dataO instanceof String) {
                        /* This should never happen */
                        throw new PluginException(LinkStatus.ERROR_FATAL, dataO.toString());
                    }
                    final Map<String, Object> riskmap_data = (Map<String, Object>) dataO;
                    dfid = riskmap_data.get("dfid").toString();
                    br.setCookie(br.getHost(), "kg_dfid", dfid);
                    if (this.isAbort()) {
                        throw new InterruptedException();
                    }
                }
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("appid", appid);
                query.appendEncoded("clientver", clientver);
                query.appendEncoded("mid", mid);
                query.appendEncoded("uuid", mid);
                query.appendEncoded("dfid", dfid);
                query.appendEncoded("songid", song_id);
                query.appendEncoded("songtype", songType);
                query.appendEncoded("version", version);
                query.appendEncoded("clienttime", clienttime);
                final String signature1 = computeSign1(query, appkey);
                query.appendEncoded("signature", signature1);
                brc.getPage("https://5sservice.kugou.com/song/getsongurl?" + query.toString());
                final Map<String, Object> map2 = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                if (Boolean.FALSE.equals(map2.get("success"))) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, map2.get("message").toString());
                }
                data = (Map<String, Object>) map2.get("data");
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            } else {
                /**
                 * Old API <br>
                 * 2025-05-16: Call itself still works but returns error
                 */
                brc.getPage("http://service.5sing.kugou.com/song/getsongurl?jsoncallback=jQuery" + System.currentTimeMillis() + "_" + System.currentTimeMillis() + "&songid=" + song_id + "&songtype=" + songType + "&from=web&version=6.6.72&_=1539798427612");
                final Map<String, Object> map2 = restoreFromString(new Regex(brc.getRequest().getHtmlCode(), "(\\{.+\\})").getMatch(0), TypeRef.MAP);
                data = (Map<String, Object>) map2.get("data");
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
            final String[] qualities = new String[] { "hq", "sq", "lq" };
            String md5hash = null;
            Object filesizeO = null;
            String ext = null;
            // Loop through qualities (best to worst) and break when first valid one is found
            for (String quality : qualities) {
                dllink = (String) data.get(quality + "url");
                if (StringUtils.isEmpty(dllink)) {
                    /* Skip invalid / non-existent qualities */
                    continue;
                }
                md5hash = (String) data.get(quality + "urlmd5");
                filesizeO = data.get(quality + "size");
                ext = (String) data.get(quality + "ext");
                break; // Exit the loop once we find a valid quality
            }
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (!StringUtils.isEmpty(md5hash)) {
                link.setMD5Hash(md5hash);
            }
            if (filesizeO != null) {
                link.setVerifiedFileSize(Long.parseLong(filesizeO.toString()));
            }
            /* Correct file extension in existing filename if needed */
            if (ext != null && link.getName() != null) {
                /* Correct file extension. This should not be needed (so far it was always .mp3 files), but let's play safe. */
                final String newFilename = this.correctOrApplyFileNameExtension(link.getName(), ext, null);
                if (!newFilename.equals(link.getName())) {
                    logger.info("Filename has changed: Old: " + link.getName() + " | New: " + newFilename);
                    link.setFinalFileName(newFilename);
                }
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* 2025-05-20: Set max chunks to 1 because the browser wouldn't use more either. Technically more are allowed. */
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Save for later usage */
        link.setProperty("directurl", dllink);
        dl.startDownload();
    }

    /**
     * See: <br>
     * https://5sstatic.kugou.com/public/common/inf_sign-2.0.0.min.js <br>
     * Function: t.signature = g(n.join(''))
     */
    private String computeSign1(final UrlQuery query, final String secret) {
        final List<String> vals = new ArrayList<String>();
        for (final KeyValueStringEntry kv : query.list()) {
            if (kv.getKey().equalsIgnoreCase("signature")) {
                continue;
            }
            vals.add(kv.getKey() + "=" + kv.getValue());
        }
        Collections.sort(vals);
        final String str = StringUtils.join(vals, "");
        final String signature = Hash.getMD5(secret + str + secret);
        return signature;
    }

    /** https://staticssl.kugou.com/verify/static/js/registerDev.v1.min.js */
    private String computeSign2(final UrlQuery query, final String secret) {
        final List<String> vals = new ArrayList<String>();
        for (final KeyValueStringEntry kv : query.list()) {
            if (kv.getKey().equalsIgnoreCase("signature")) {
                continue;
            }
            vals.add(kv.getValue());
        }
        Collections.sort(vals);
        final String str = StringUtils.join(vals, "");
        final String signature = Hash.getMD5(secret + str + secret);
        return signature;
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}