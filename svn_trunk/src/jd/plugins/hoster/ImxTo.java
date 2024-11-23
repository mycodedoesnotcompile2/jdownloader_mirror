//jDownloader - Downloadmanager
//Copyright (C) 2012  JD-Team support@jdownloader.org
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

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.settings.GeneralSettings;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50213 $", interfaceVersion = 2, names = { "imx.to" }, urls = { "https?://(?:\\w+\\.)?imx\\.to/((?:u/)?(?:i|t)/\\d+/\\d+/\\d+/([a-z0-9]+)\\.[a-z]+|(?:i/|img\\-)[a-z0-9]+)" })
public class ImxTo extends PluginForHost {
    private static final String PROPERTY_DIRECTURL = "directurl";
    private static final String TYPE_THUMBNAIL     = "(?i)https?://[^/]+/(?:u/)?t/\\d+/\\d+/\\d+/([a-z0-9]+)\\.[a-z]+";
    private static final String TYPE_FULLSIZE      = "(?i)https?://[^/]+/(?:u/)?i/\\d+/\\d+/\\d+/([a-z0-9]+)\\.[a-z]+";

    public ImxTo(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/page/terms";
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
        if (link.getPluginPatternMatcher() == null) {
            return null;
        } else if (link.getPluginPatternMatcher().matches(TYPE_THUMBNAIL)) {
            return new Regex(link.getPluginPatternMatcher(), TYPE_THUMBNAIL).getMatch(0);
        } else if (link.getPluginPatternMatcher().matches(TYPE_FULLSIZE)) {
            return new Regex(link.getPluginPatternMatcher(), TYPE_FULLSIZE).getMatch(0);
        } else {
            /* Assume we have TYPE_FULLSIZE */
            return new Regex(link.getPluginPatternMatcher(), "/(?:img-)?([a-z0-9]+)$").getMatch(0);
        }
    }

    @Override
    public void correctDownloadLink(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            /* 2021-05-25: Don't do this because this way we won't get the original filenames! */
            // if (link.getPluginPatternMatcher().matches(TYPE_FULLSIZE)) {
            // link.setProperty(PROPERTY_DIRECTURL, link.getPluginPatternMatcher());
            // } else if (link.getPluginPatternMatcher().matches(TYPE_THUMBNAIL)) {
            // link.setProperty(PROPERTY_DIRECTURL, link.getPluginPatternMatcher().replace("/u/t/", "/u/i/"));
            // }
            // remember original direct full/thumbnai link
            final String url = link.getPluginPatternMatcher();
            if (url.matches(TYPE_THUMBNAIL)) {
                link.setProperty("imageLink", url);
            } else if (url.matches(TYPE_FULLSIZE)) {
                link.setProperty("imageLink", url);
            }
            final String newurl = "https://" + this.getHost() + "/i/" + fid;
            link.setPluginPatternMatcher(newurl);
            /*
             * Important as we pickup the 'img-' URLs without '.html' ending and we do not want the user to have broken content-URLs in JD!
             */
            if (url.matches(TYPE_THUMBNAIL) || url.matches(TYPE_FULLSIZE)) {
                link.setContentUrl(url);
            }
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException {
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + ".jpg");
        }
        this.setBrowserExclusive();
        final String storedDirecturl = link.getStringProperty(PROPERTY_DIRECTURL);
        if (storedDirecturl != null && !isDownload) {
            try {
                this.basicLinkCheck(br, br.createHeadRequest(storedDirecturl), link, null, null);
                logger.info("Availablecheck via directurl complete");
                return AvailableStatus.TRUE;
            } catch (final Throwable e) {
                logger.log(e);
                link.removeProperty(PROPERTY_DIRECTURL);
            }
        }
        br.setFollowRedirects(true);
        br.getPage("https://" + this.getHost() + "/i/" + this.getFID(link));
        if (br.getHttpConnection().getResponseCode() == 404 || !br.getURL().contains(this.getFID(link))) {
            String imageLink = link.getStringProperty("imageLink");
            if (imageLink != null) {
                imageLink = imageLink.replaceFirst("/t/", "/i/");
                imageLink = imageLink.replaceFirst("https?://x", "https://i");
                logger.info("Verify directurl:" + imageLink);
                try {
                    this.basicLinkCheck(br, br.createHeadRequest(imageLink), link, null, null);
                    logger.info("Availablecheck via directurl complete");
                    link.setProperty(PROPERTY_DIRECTURL, imageLink);
                    return AvailableStatus.TRUE;
                } catch (final Throwable e) {
                    logger.log(e);
                    link.removeProperty(PROPERTY_DIRECTURL);
                }
            }
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Find- and set directurl so we can save time and requests on download-start. */
        if (JsonConfig.create(GeneralSettings.class).isHashCheckEnabled() && link.getMD5Hash() == null && !isDownload) {
            /* Sometimes an extra step is needed to find the md5 hash. */
            logger.info("Trying to find md5 hash during linkcheck");
            this.sendContinueForm(br);
            if (link.getMD5Hash() == null) {
                logger.warning("Failed to find m5 hash");
            }
        }
        getAndSetFileInfo(link);
        final String dllink = findDownloadurl(this.br);
        if (dllink != null) {
            link.setProperty(PROPERTY_DIRECTURL, dllink);
        }
        return AvailableStatus.TRUE;
    }

    private void getAndSetFileInfo(final DownloadLink link) {
        String filename = br.getRegex("<title>\\s*IMX\\.to\\s*/\\s*([^<>\"]+)\\s*</title>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            filename = this.applyFilenameExtension(filename, ".jpg");
            link.setFinalFileName(filename);
        }
        final String filesize = br.getRegex("(?i)FILESIZE\\s*<span[^>]*>([^<]+)</span>").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        final String md5hash = br.getRegex("(?i)HASH\\s*<span[^>]*>([^<]+)</span>").getMatch(0);
        if (md5hash != null) {
            link.setMD5Hash(md5hash);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        final String storedDirecturl = link.getStringProperty(PROPERTY_DIRECTURL);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformation(link, true);
            if (this.sendContinueForm(br)) {
                getAndSetFileInfo(link);
            }
            dllink = findDownloadurl(this.br);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 1 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 1 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 503) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 503 too many connections", 1 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(PROPERTY_DIRECTURL);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(PROPERTY_DIRECTURL, dl.getConnection().getURL().toExternalForm());
        }
        dl.setAllowFilenameFromURL(true);// old core
        dl.startDownload();
    }

    private boolean sendContinueForm(final Browser br) throws IOException {
        /* Form is not always present */
        final Form continueForm = br.getFormbyKey("imgContinue");
        if (continueForm == null) {
            logger.info("Failed to find continueForm");
            return false;
        }
        logger.info("Sending imgContinue Form...");
        br.submitForm(continueForm);
        return true;
    }

    private String findDownloadurl(final Browser br) {
        return br.getRegex("\"(https?://[^/]+/u/i/[^\"]+)\" ").getMatch(0);
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /* 2022-07-11: More connections will lead to http error response 503 */
        return 1;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            return;
        }
        link.removeProperty(PROPERTY_DIRECTURL);
    }
}