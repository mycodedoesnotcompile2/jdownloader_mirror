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
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashSet;

import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51845 $", interfaceVersion = 3, names = { "imx.to" }, urls = { "https?://(?:www\\.)?imx\\.to/g/([a-z0-9]+)" })
public class ImxToGallery extends PluginForDecrypt {
    public ImxToGallery(PluginWrapper wrapper) {
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
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl().replaceFirst("(?i)^http://", "https://");
        final String galleryID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        String title = br.getRegex("class=\"title\"[^>]*>([^<]+)</div>").getMatch(0);
        if (title == null) {
            title = br.getRegex("<title>IMX\\.to / ([^<>\"]+)</title>").getMatch(0);
        }
        if (title != null) {
            title = Encoding.htmlDecode(title.trim());
        }
        final String titleForOfflineItems;
        if (title != null) {
            titleForOfflineItems = title;
        } else {
            titleForOfflineItems = galleryID;
        }
        final String galleryFilesizeStr = br.getRegex("Size\\s*<span [^>]*>([^<]+)</span>").getMatch(0);
        final String galleryNumberofItemsStr = br.getRegex("Images\\s*<span[^>]*>([0-9,]+)</span>").getMatch(0);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if ("0".equals(galleryNumberofItemsStr)) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, titleForOfflineItems);
        } else if (!br.getURL().contains(galleryID) && galleryFilesizeStr == null) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int numberofItems = -1;
        if (galleryNumberofItemsStr != null) {
            numberofItems = Integer.parseInt(galleryNumberofItemsStr.replace(",", ""));
        }
        long galleryFilesizeBytes = -1;
        if (galleryFilesizeStr != null) {
            galleryFilesizeBytes = SizeFormatter.getSize(galleryFilesizeStr);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        } else {
            /* Fallback */
            logger.warning("Failed to find gallery title");
            fp.setName(galleryID);
        }
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        pagination: do {
            final String[] imageIDs = br.getRegex("imx\\.to/i/([a-z0-9]+)").getColumn(0);
            if (imageIDs == null || imageIDs.length == 0) {
                if (galleryFilesizeBytes == 0) {
                    /* Empty gallery */
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, titleForOfflineItems);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            int numberofNewItemsThisPage = 0;
            for (final String imageID : imageIDs) {
                if (!dupes.add(imageID)) {
                    continue;
                }
                final DownloadLink img = createDownloadlink("https://" + getHost() + "/i/" + imageID);
                img._setFilePackage(fp);
                img.setName(imageID + ".jpg");
                if (ret.isEmpty() && imageIDs.length == 1 && galleryFilesizeBytes != -1) {
                    /* Single image in gallery -> Set filesize of gallery as size of single item. */
                    img.setDownloadSize(galleryFilesizeBytes);
                }
                img.setAvailable(true);
                ret.add(img);
                distribute(img);
                numberofNewItemsThisPage++;
            }
            logger.info("Crawled page " + "" + " | Foumd items this page: " + numberofNewItemsThisPage + " | Found items so far: " + ret.size() + "/" + numberofItems);
            final String nextPageUrl = br.getRegex("href=\"(/g/" + galleryID + "\\?page=" + (page + 1) + ")\"[^<]*class=\"next\"").getMatch(0);
            if (nextPageUrl == null) {
                logger.info("Stopping because: Reached end?");
                break pagination;
            } else if (numberofItems != -1 && ret.size() >= numberofItems) {
                /* Fail-safe 1 */
                logger.info("Stopping because: Found all items");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                /* Fail-safe 2 */
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            } else if (this.isAbort()) {
                /* Aborted by user */
                throw new InterruptedException();
            }
            /* Continue to next page */
            page++;
            br.getPage(nextPageUrl);
        } while (true);
        return ret;
    }
}
