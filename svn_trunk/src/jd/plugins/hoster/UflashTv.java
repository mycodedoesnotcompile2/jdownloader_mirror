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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Base64;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51365 $", interfaceVersion = 2, names = { "uflash.tv" }, urls = { "https?://(?:www\\.)?uflash\\.tv/video/(\\d+)" })
public class UflashTv extends PluginForHost {
    public UflashTv(PluginWrapper wrapper) {
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

    private String dllink = null;

    @Override
    public String getAGBLink() {
        return "http://www." + getHost() + "/static/terms";
    }

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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        dllink = null;
        final String extDefault = ".mp4";
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid + extDefault);
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (StringUtils.containsIgnoreCase(br.getURL(), "/error/video_missing")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("VIDEO \\| ([^<>\"]*?) \\| UFLASH\\.TV").getMatch(0);
        title = Encoding.htmlDecode(title).trim();
        if (title == null) {
            title = fid;
        }
        if (this.isPrivateVideo(br)) {
            link.getLinkStatus().setStatusText("Private videos are only downloadable via account");
            link.setName(title + extDefault);
            return AvailableStatus.TRUE;
        }
        String video = br.getRegex("var\\s*[\\w_]*\\s*=\\s*\"(a.*?)\"").getMatch(0);
        if (video != null) {
            video = Base64.decodeToString(video);
            if (StringUtils.contains(video, fid)) {
                dllink = video;
            }
        }
        link.setFinalFileName(title + extDefault);
        if (dllink == null) {
            br.postPage("/ajax/getvideo", "vid=" + fid);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            dllink = entries.get("video_src").toString();
        }
        if (dllink != null && !link.isSizeSet() && !PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment())) {
            dllink = Encoding.htmlOnlyDecode(dllink);
            this.basicLinkCheck(br, br.createGetRequest(dllink), link, title, extDefault);
        }
        return AvailableStatus.TRUE;
    }

    private boolean isPrivateVideo(final Browser br) {
        return br.containsHTML(">\\s*This is a private video");
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (this.isPrivateVideo(br)) {
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
        } else if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 1);
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /* 2025-08-25: Set to 1 to prevent error 503, see: https://board.jdownloader.org/showthread.php?t=97782 */
        return 1;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}
