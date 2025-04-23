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
import java.util.Random;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.antiDDoSForHost;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.download.HashInfo;

@HostPlugin(revision = "$Revision: 50987 $", interfaceVersion = 3, names = { "apkmirror.com" }, urls = { "https?://(?:www\\.)?apkmirror\\.com/apk/(([^/]+)/([^/]+)/([^/]+)/([^/]+))\\-download/" })
public class ApkmirrorCom extends antiDDoSForHost {
    public ApkmirrorCom(PluginWrapper wrapper) {
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
        return "http://www." + getHost() + "/contact-us/";
    }

    /* Connection stuff */
    private static final boolean FREE_RESUME    = true;
    private static final int     FREE_MAXCHUNKS = 0;

    // private static final boolean ACCOUNT_FREE_RESUME = true;
    // private static final int ACCOUNT_FREE_MAXCHUNKS = 0;
    // private static final int ACCOUNT_FREE_MAXDOWNLOADS = 20;
    // private static final boolean ACCOUNT_PREMIUM_RESUME = true;
    // private static final int ACCOUNT_PREMIUM_MAXCHUNKS = 0;
    // private static final int ACCOUNT_PREMIUM_MAXDOWNLOADS = 20;
    //
    // /* don't touch the following! */
    // private static AtomicInteger maxPrem = new AtomicInteger(1);
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        String weakTitle = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
        if (!link.isNameSet()) {
            final Regex apkEnding = new Regex(weakTitle, "(?i)(.+)-apk$");
            if (apkEnding.patternFind()) {
                weakTitle = apkEnding.getMatch(0);
            }
            weakTitle = weakTitle.replace("/", "_");
            weakTitle = Encoding.htmlDecode(weakTitle).trim();
            link.setName(weakTitle + ".apk");
        }
        this.setBrowserExclusive();
        getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filesize = br.getRegex("\\(([0-9,]+) bytes\\)").getMatch(0);
        final String crcSourceHTML = br.getRegex("APK file hashes\\<(.+)").getMatch(0);
        if (crcSourceHTML != null) {
            final String checksumMd5 = new Regex(crcSourceHTML, " MD5:\\s*<span[^>]*>([a-f0-9]{32})</span>").getMatch(0);
            final String checksumSha1 = new Regex(crcSourceHTML, " SHA-1:\\s*<span[^>]*>([a-f0-9]{40})</span>").getMatch(0);
            final String checksumSha256 = new Regex(crcSourceHTML, " SHA-256:\\s*<span[^>]*>([a-f0-9]{64})</span>").getMatch(0);
            /* Set hashes for CRC check */
            if (!StringUtils.isEmpty(checksumMd5)) {
                link.addHashInfo(HashInfo.newInstanceSafe(checksumMd5, HashInfo.TYPE.MD5));
            }
            if (!StringUtils.isEmpty(checksumSha1)) {
                link.addHashInfo(HashInfo.newInstanceSafe(checksumSha1, HashInfo.TYPE.SHA1));
            }
            if (!StringUtils.isEmpty(checksumSha256)) {
                link.addHashInfo(HashInfo.newInstanceSafe(checksumSha256, HashInfo.TYPE.SHA256));
            }
        } else {
            logger.warning("Failed to find CRC file hash source html");
        }
        if (filesize != null) {
            filesize = filesize.replace(",", "");
            link.setVerifiedFileSize(Long.parseLong(filesize));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink lnk) throws Exception, PluginException {
        requestFileInformation(lnk);
        doFree(lnk, FREE_RESUME, FREE_MAXCHUNKS, "free_directlink");
    }

    private void doFree(final DownloadLink link, final boolean resumable, final int maxchunks, final String directlinkproperty) throws Exception, PluginException {
        if (!attemptStoredDownloadurlDownload(link, resumable, maxchunks, directlinkproperty)) {
            final String step1 = br.getRegex("\"([^\"]+/download/?\\?key=[a-f0-9]+[^\"]*)").getMatch(0);
            if (step1 == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final int randomSeconds = new Random().nextInt(10);
            final int waitSeconds = Math.min(2, randomSeconds);
            /* 2025-04-22: Looks like a random delay is required otherwise the download cannot be started. */
            this.sleep(waitSeconds * 1000l, link);
            getPage(step1);
            final String dllink = br.getRegex("\"([^\"]+download\\.php\\?id=[^\"]+)\"").getMatch(0);
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resumable, maxchunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toString());
        }
        dl.startDownload();
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final boolean resumable, final int maxchunks, final String directlinkproperty) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        boolean valid = false;
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, resumable, maxchunks);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                valid = true;
                return true;
            } else {
                link.removeProperty(directlinkproperty);
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            return false;
        } finally {
            if (!valid) {
                try {
                    dl.getConnection().disconnect();
                } catch (Throwable ignore) {
                }
                this.dl = null;
            }
        }
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