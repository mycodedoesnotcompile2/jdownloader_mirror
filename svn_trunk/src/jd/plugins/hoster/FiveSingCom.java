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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Hash;
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

@HostPlugin(revision = "$Revision: 51070 $", interfaceVersion = 2, names = { "5sing.kugou.com" }, urls = { "https?://(?:www\\.)?5sing\\.kugou\\.com/(f|y)c/(\\d+)\\.html" })
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
        br.setFollowRedirects(true);
        /* 2025-05-15: They do not support https */
        final String contenturl = link.getPluginPatternMatcher().replaceFirst("https://", "http://");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String extension = br.getRegex("(<em>)?格式：(</em>)?([^<>\"]*?)(<|&)").getMatch(2);
        if (extension == null && br.containsHTML("<em>演唱：</em>")) {
            extension = "mp3";
        }
        // final String filename = br.getRegex("var SongName[^<>\"\t\n\r]*= \"([^<>\"]*?)\"").getMatch(0);
        final String filename = br.getRegex("song_title\" title=\"([^<>\"]*?)\"").getMatch(0);
        final String song_id = br.getRegex("var SongID[^<>\"\t\n\r]*= ([^<>\"]*?);").getMatch(0);
        final String stype = br.getRegex("var SongType[^<>\"\t\n\r]*= \"([^<>\"]*?)\";").getMatch(0);
        String filesize = br.getRegex("(<em>)?大小：(</em>)?([^<>\"]*?)(<|\")").getMatch(2);
        if (filename != null && extension != null) {
            link.setFinalFileName(stype + "-" + Encoding.htmlDecode(filename).trim() + "-" + song_id + "." + Encoding.htmlDecode(extension).trim());
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
        String src = br.getRegex("\"ticket\": \"([^<>\"]*?)\"").getMatch(0);
        if (src == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        src = Encoding.Base64Decode(src).replace("\\", "");
        String dllink = new Regex(src, "\"file\":\"(https?:[^\"]*?)\"").getMatch(0);
        if (StringUtils.isEmpty(dllink)) {
            final Map<String, Object> map = restoreFromString(src, TypeRef.MAP);
            final String song_id = map.containsKey("songID") ? String.valueOf(map.get("songID")) : null;
            final String songType = map.containsKey("songType") ? String.valueOf(map.get("songType")) : null;
            if (StringUtils.isEmpty(songType) || StringUtils.isEmpty(song_id)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Browser brc = br.cloneBrowser();
            final boolean useNewAPI = DebugMode.TRUE_IN_IDE_ELSE_FALSE;
            final Map<String, Object> data;
            if (useNewAPI) {
                // TODO: Unfinished code
                // TODO: mod and dfid are null
                final String mid = br.getCookie(br.getHost(), "kg_mid", Cookies.NOTDELETEDPATTERN);
                final String dfid = br.getCookie(br.getHost(), "kg_dfid", Cookies.NOTDELETEDPATTERN);
                final String appkey = br.getRegex("appkey:\\s*'([^']+)'").getMatch(0);
                if (StringUtils.isEmpty(mid) || StringUtils.isEmpty(dfid) || StringUtils.isEmpty(appkey)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String appid = "2918";
                final String clienttime = Long.toString(System.currentTimeMillis());
                final String clientver = "1000";
                final String version = "6.6.72";
                final String songtype;
                if (StringUtils.containsIgnoreCase(link.getPluginPatternMatcher(), "/fc/")) {
                    songtype = "fc";
                } else {
                    songtype = "yc";
                }
                final StringBuilder sb = new StringBuilder();
                sb.append(appkey);
                sb.append("appid=" + appid);
                sb.append("clienttime=" + clienttime);
                sb.append("clientver=" + clientver);
                sb.append("dfid=" + dfid);
                sb.append("mid=" + mid);
                sb.append("songid=" + song_id);
                sb.append("songtype=" + songtype);
                sb.append("uuid=" + mid);
                sb.append("version=" + version);
                sb.append(appkey);
                /**
                 * See: <br>
                 * https://5sstatic.kugou.com/public/common/inf_sign-2.0.0.min.js <br>
                 * Function: t.signature = g(n.join(''))
                 */
                final String signature = Hash.getMD5(sb.toString());
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("appid", appid);
                query.appendEncoded("clientver", clientver);
                query.appendEncoded("mid", mid);
                query.appendEncoded("uuid", mid);
                query.appendEncoded("dfid", dfid);
                query.appendEncoded("songid", song_id);
                query.appendEncoded("songtype", songtype);
                query.appendEncoded("version", version);
                query.appendEncoded("clienttime", clienttime);
                query.appendEncoded("signature", signature);
                brc.getPage("https://5sservice.kugou.com/song/getsongurl?" + query.toString());
                final Map<String, Object> map2 = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                data = (Map<String, Object>) map2.get("data");
            } else {
                /* Old API */
                brc.getPage("http://service.5sing.kugou.com/song/getsongurl?jsoncallback=jQuery" + System.currentTimeMillis() + "_" + System.currentTimeMillis() + "&songid=" + song_id + "&songtype=" + songType + "&from=web&version=6.6.72&_=1539798427612");
                final Map<String, Object> map2 = restoreFromString(new Regex(brc.getRequest().getHtmlCode(), "(\\{.+\\})").getMatch(0), TypeRef.MAP);
                data = (Map<String, Object>) map2.get("data");
            }
            dllink = (String) data.get("hqurl");
            String md5hash = (String) data.get("hqurlmd5");
            Object filesizeO = data.get("hqsize");
            String ext = (String) data.get("hqext"); // usually "mp3"
            if (StringUtils.isEmpty(dllink)) {
                dllink = (String) map.get("lqurl");
                md5hash = (String) data.get("lqurlmd5");
                filesizeO = data.get("lqsize");
                ext = (String) data.get("lqext");
            }
            // TODO: Check if we can use this for CRC checking
            if (!StringUtils.isEmpty(md5hash)) {
                link.setMD5Hash(md5hash);
            }
            if (filesizeO != null) {
                // TODO: Check if this is the correct filesize value
                link.setVerifiedFileSize(Long.parseLong(filesizeO.toString()));
            }
            // TODO: Make use of "ext" value
        }
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
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