//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

@HostPlugin(revision = "$Revision: 51734 $", interfaceVersion = 2, names = { "filehippo.com" }, urls = { "https?://(?:www\\.)?filehippo\\.com(?:/[a-z]{2})?/download_([\\w-]+)" })
public class FileHippoCom extends PluginForHost {
    public FileHippoCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(getHost(), "FH_PreferredCulture", "en-US");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/info/disclaimer/";
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
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private static final String text_ErrorExternalDownloadUnsupported = "Download impossible - download-url points to external site";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("(<h1>404 Error</h1>|<b>Sorry the page you requested could not be found|Sorry an error occurred processing your request)")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getURL().matches("^https?://[^/]+/$")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String path = br._getURL().getPath();
        String filename = br.getRegex("Filename\\s*</dt>\\s*<dd[^>]*>([^<]+)</dd>").getMatch(0);
        if (filename == null) {
            /* Fallback */
            filename = path.replaceFirst("(download_)", "").replaceFirst("(/.+)", "");
        }
        if (filename != null) {
            final String applicationVersion = br.getRegex("data-qa=\"program-version\"[^>]*>([^<>\"]+)</p>").getMatch(0);
            if (applicationVersion != null) {
                filename += " " + applicationVersion;
            }
            link.setName(filename.trim());
        } else {
            logger.warning("Failed to find filename");
        }
        String filesize = br.getRegex("\\(([0-9,]+ bytes)\\)").getMatch(0);
        if (filesize == null) {
            filesize = br.getRegex("Download This Version\\s+<span class=\"normal\">\\(([^<>]*?)\\)<").getMatch(0);
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize.replace(",", "")));
        }
        final String sha1 = br.getRegex("SHA-1\\s*</dt>\\s*<dd[^>]*>\\s*<pre>([^<]+)").getMatch(0);
        if (sha1 != null) {
            link.setSha1Hash(sha1.trim());
        } else {
            final String md5 = br.getRegex("MD5 Checksum:\\s*</span> <span class=\"field\\-value\">([^<>\"]*?)</span>").getMatch(0);
            if (md5 != null) {
                link.setMD5Hash(md5.trim());
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /**
         * There are different types of downloads: <br>
         * 1. Normal downloads: /download_vlc-media-player-64 <br>
         * 2. affiliateDownload: /download_avast-cleanup/ -> Does not have any step after "/download/". <br>
         * 3. buyDownload or multiple types for one download: /download_2007-microsoft-office-add-in-microsoft-save-as-pdf-or-xps/ <br>
         */
        final String urlStepSlashDownload = br.getRegex("href=\"([^\"]+/download/?)\"[^>]*data-meta=\"download-(free|latest)-version\"").getMatch(0);
        if (urlStepSlashDownload != null) {
            /* e.g. added link: /download_adobe-reader-lite/ -> Next step: /download_adobe-reader-lite/download/ */
            br.getPage(urlStepSlashDownload);
        }
        String continuelink = br.getRegex("(/download_[\\w-]+/post_download/\\?dt=internalDownload)").getMatch(0);
        if (continuelink != null) {
            continuelink = Encoding.htmlOnlyDecode(continuelink);
            br.getPage(continuelink);
        }
        final String dltype = br.getRegex("data-dw-type=\"([^\"]+)").getMatch(0);
        final String dltoken = br.getRegex("data-dw-type=\"internalDownload\" data-dw-token=\"([^\"]+)").getMatch(0);
        if (dltoken == null) {
            if (StringUtils.containsIgnoreCase(continuelink, "external")) {
                /* Old handling/check */
                throw new PluginException(LinkStatus.ERROR_FATAL, text_ErrorExternalDownloadUnsupported);
            } else if (dltype != null) {
                if (dltype.equalsIgnoreCase("affiliateDownload")) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, text_ErrorExternalDownloadUnsupported);
                } else if (dltype.equalsIgnoreCase("buyDownload")) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "This is a paid item that needs to be purchased separately");
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Unsupported download type: " + dltype);
                }
            } else {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        String dllink = "/download-launch/?token=" + dltoken;
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        return false;
    }
}