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
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.FilesFmFolder;

@HostPlugin(revision = "$Revision: 50034 $", interfaceVersion = 3, names = { "files.fm" }, urls = { "https?://(?:\\w+\\.)?files\\.fm/(?:down\\.php\\?i=[a-z0-9]+(\\&n=[^/]+)?|f/[a-z0-9]+)" })
public class FilesFm extends PluginForHost {
    public FilesFm(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        FilesFmFolder.prepBR(br);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    /* Connection stuff */
    private static final boolean FREE_RESUME    = true;
    private static final int     FREE_MAXCHUNKS = 1;

    // private static final boolean ACCOUNT_FREE_RESUME = true;
    // private static final int ACCOUNT_FREE_MAXCHUNKS = 0;
    // private static final int ACCOUNT_FREE_MAXDOWNLOADS = 20;
    // private static final boolean ACCOUNT_PREMIUM_RESUME = true;
    // private static final int ACCOUNT_PREMIUM_MAXCHUNKS = 0;
    // private static final int ACCOUNT_PREMIUM_MAXDOWNLOADS = 20;
    //
    // /* don't touch the following! */
    // private static AtomicInteger maxPrem = new AtomicInteger(1);
    public void correctDownloadLink(final DownloadLink link) {
        link.setPluginPatternMatcher("https://files.fm/f/" + getLinkID(link));
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = new Regex(link.getPluginPatternMatcher(), "(?:i=|/f/)([a-z0-9]+)").getMatch(0);
        if (linkid != null) {
            return linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String dllink = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        dllink = null;
        this.setBrowserExclusive();
        final String mainlink = link.getStringProperty("mainlink");
        if (mainlink != null) {
            /* Referer needed to download. Not always given as users can also add directlinks without going over the decrypter. */
            this.br.getPage(mainlink);
            // this.br.getHeaders().put("Referer", mainlink);
        } else {
            br.getPage(link.getPluginPatternMatcher());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        final String linkid = this.getLinkID(link);
        final String linkpart = new Regex(link.getPluginPatternMatcher(), "(\\?i=.+)").getMatch(0);
        final String filename_url = new Regex(linkpart, "\\&n=(.+)").getMatch(0);
        String filename_header = null;
        URLConnectionAdapter con = null;
        final Browser brc = br.cloneBrowser();
        try {
            dllink = "https://files.fm/down.php?i=" + getLinkID(link);
            con = brc.openHeadConnection(dllink);
            if (con.getURL().toExternalForm().contains("/private")) {
                // https://files.fm/thumb_show.php?i=wfslpuh&n=20140908_073035.jpg&refresh1
                /* Maybe we have a picture without official "Download" button ... */
                logger.info("Checking for picture content without official download button");
                dllink = "/thumb_show.php" + linkpart + "&refresh1";
                con = brc.openHeadConnection(dllink);
            }
            if (!this.looksLikeDownloadableContent(con)) {
                /*
                 * Browser will download file via built in torrent downloader. We can't do that. Instead we will deliver the .torrent file
                 * to the user so the user can download the file behind it using a torrent downloader.
                 */
                final String webdlTorrentID = br.getRegex("new WebTorrentDownloadForm\\( \\'([a-z0-9]+)\\' \\)").getMatch(0);
                if (webdlTorrentID == null) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                // br.getHeaders().put("x-requested-with", "XMLHttpRequest");
                // br.postPage("/ajax/webtorrent_download_form.php?PHPSESSID=" + webdlTorrentID,
                // "action=init_client&folder_hash=&file_hash=" + linkid + "&file_hashes=%5B%5D");
                /**
                 * Large files are only available via web-/torrent download </br>
                 * 2021-05-17: Seems like all downloads are only available as P2P downloads --> In this case all we can do is to download
                 * the .torrent file so the user can manually download it using a Torrent client.
                 */
                logger.info("File is only available via torrent");
                dllink = String.format("https://%s/torrent/get_torrent.php?file_hash=%s", getHost(), linkid);
                String filename = null;
                try {
                    final String jsonFileInfo = br.getRegex("objMainShareParams = (\\{.*?\\});").getMatch(0);
                    Map<String, Object> entries = restoreFromString(jsonFileInfo, TypeRef.MAP);
                    entries = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "one_file/item_info");
                    filename = (String) entries.get("file_name");
                } catch (final Throwable e) {
                }
                /*
                 * Website returns non meaningful filenames when downloading torrent files --> Try to use original filename and append
                 * .torrent extension.
                 */
                final String originalFilename = link.getStringProperty("originalname");
                String torrentFilename;
                if (originalFilename != null) {
                    torrentFilename = originalFilename;
                } else if (filename != null) {
                    torrentFilename = filename;
                } else {
                    /* Fallback */
                    torrentFilename = linkid;
                }
                torrentFilename = applyFilenameExtension(torrentFilename, ".torrent");
                link.setFinalFileName(torrentFilename);
                // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                filename_header = Encoding.htmlDecode(Plugin.getFileNameFromDispositionHeader(con));
                if (filename_url == null && filename_header != null) {
                    link.setFinalFileName(filename_header);
                } else if (filename_url != null && filename_header.length() > filename_url.length()) {
                    link.setFinalFileName(filename_header);
                } else if (filename_url != null) {
                    link.setFinalFileName(filename_url);
                }
                if (con.isContentDecoded()) {
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link, FREE_RESUME, FREE_MAXCHUNKS);
    }

    private void doFree(final DownloadLink link, final boolean resumable, final int maxchunks) throws Exception, PluginException {
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resumable, maxchunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}