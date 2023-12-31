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

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 48061 $", interfaceVersion = 3, names = { "redbull.tv" }, urls = { "http://redbull\\.tvdecrypted\\d+" })
public class RedbullTv extends PluginForHost {
    /** Settings stuff */
    private static final String                   FAST_LINKCHECK = "FAST_LINKCHECK";
    public static LinkedHashMap<String, String[]> formats        = new LinkedHashMap<String, String[]>(new LinkedHashMap<String, String[]>() {
                                                                     {
                                                                                                                                                                      /*
                                                                                                                                                                       * Format
                                                                                                                                                                       * -
                                                                                                                                                                       * name
                                                                                                                                                                       * :
                                                                                                                                                                       * videoCodec,
                                                                                                                                                                       * videoBitrate,
                                                                                                                                                                       * videoResolution,
                                                                                                                                                                       * audioCodec,
                                                                                                                                                                       * audioBitrate
                                                                                                                                                                       */
                                                                         put("64", new String[] { "AVC", "64", "0x0", "AAC LC", "64" });
                                                                         put("296", new String[] { "AVC", "296", "384x216", "AAC LC", "75,3" });
                                                                         put("496", new String[] { "AVC", "496", "384x216", "AAC LC", "75,3" });
                                                                         put("856", new String[] { "AVC", "856", "384x216", "AAC LC", "75,3" });
                                                                         put("1200", new String[] { "AVC", "1200", "640x360", "AAC LC", "75,3" });
                                                                         put("1800", new String[] { "AVC", "1800", "960x540", "AAC LC", "75,3" });
                                                                         put("2500", new String[] { "AVC", "2500", "1280x720", "AAC LC", "75,3" });
                                                                         put("3528", new String[] { "AVC", "3528", "1280x720", "AAC LC", "75,3" });
                                                                         put("4500", new String[] { "AVC", "4500", "1280x720", "AAC LC", "75,3" });
                                                                         put("6500", new String[] { "AVC", "6500", "1920x1080", "AAC LC", "75,3" });
                                                                         put("8500", new String[] { "AVC", "8500", "1920x1080", "AAC LC", "75,3" });
                                                                     }
                                                                 });
    private String                                DLLINK         = null;

    @SuppressWarnings("deprecation")
    public RedbullTv(final PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    @Override
    public String getAGBLink() {
        return "http://www.redbull.tv/information/terms";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        setBrowserExclusive();
        // final String mainlink = link.getStringProperty("mainlink", null);
        // br.getPage(mainlink);
        // if (br.getHttpConnection().getResponseCode() == 404) {
        // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        // }
        DLLINK = link.getStringProperty("directlink", null);
        URLConnectionAdapter con = null;
        try {
            con = br.openHeadConnection(DLLINK);
            if (this.looksLikeDownloadableContent(con)) {
                link.setVerifiedFileSize(con.getCompleteContentLength());
            } else {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            return AvailableStatus.TRUE;
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        br.setFollowRedirects(true);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, DLLINK, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public String getDescription() {
        return "JDownloader's Red Bull plugin helps downloading videoclips from redbull.com. You can chose between different video qualities.";
    }

    private void setConfigElements() {
        final Iterator<Entry<String, String[]>> it = formats.entrySet().iterator();
        while (it.hasNext()) {
            /*
             * Format-name:videoCodec, videoBitrate, videoResolution, audioCodec, audioBitrate
             */
            String usertext = "Load ";
            final Entry<String, String[]> videntry = it.next();
            final String internalname = videntry.getKey();
            final String[] vidinfo = videntry.getValue();
            final String videoCodec = vidinfo[0];
            final String videoBitrate = vidinfo[1];
            final String videoResolution = vidinfo[2];
            final String audioCodec = vidinfo[3];
            final String audioBitrate = vidinfo[4];
            if (videoCodec != null) {
                usertext += videoCodec + " ";
            }
            if (videoBitrate != null) {
                usertext += videoBitrate + " ";
            }
            if (videoResolution != null) {
                usertext += videoResolution + " ";
            }
            if (audioCodec != null || audioBitrate != null) {
                usertext += "with audio ";
                if (audioCodec != null) {
                    usertext += audioCodec + " ";
                }
                if (audioBitrate != null) {
                    usertext += audioBitrate;
                }
            }
            if (usertext.endsWith(" ")) {
                usertext = usertext.substring(0, usertext.lastIndexOf(" "));
            }
            final ConfigEntry vidcfg = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), internalname, JDL.L("plugins.hoster.RedbullTv.ALLOW_" + internalname, usertext)).setDefaultValue(true);
            getConfig().addEntry(vidcfg);
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }
}