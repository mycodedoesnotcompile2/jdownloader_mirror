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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.JpgChurch;

@DecrypterPlugin(revision = "$Revision: 52960 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { JpgChurch.class })
public class JpgChurchCrawler extends PluginForDecrypt {
    public JpgChurchCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return JpgChurch.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    /**
     * Matches both the new/short album urls (id only) and the old/long ones (slug + id, separated by a dot). <br>
     * New: https://goonbox.cr/a/4WrAA <br>
     * Old/long: https://goonbox.cr/a/annfigma-reddit.4WrAA
     */
    private static final Pattern PATTERN_ALBUM = Pattern.compile("/(?:album|a)/([\\w\\-\\.]+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_ALBUM.pattern());
        }
        return ret.toArray(new String[0]);
    }

    private String getAlbumIDCleaned(final CryptedLink param) throws PluginException {
        final String addedurl = param.getCryptedUrl();
        String path;
        try {
            path = new URL(addedurl).getPath();
        } catch (final Exception e) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Invalid url", e);
        }
        String id = new Regex(path, PATTERN_ALBUM).getMatch(0);
        if (id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find album id");
        }
        final int dotIndex = id.lastIndexOf(".");
        if (dotIndex >= 0 && dotIndex < id.length() - 1) {
            /* Old/long format e.g. "annfigma-reddit.4WrAA" -> API can only work with the plain id "4WrAA". */
            id = id.substring(dotIndex + 1);
        }
        return id;
    }

    @SuppressWarnings("unchecked")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String albumID = getAlbumIDCleaned(param);
        br.getPage("https://" + this.getHost() + "/api/albums/" + albumID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> album = (Map<String, Object>) entries.get("album");
        final String albumDescription = (String) album.get("description");
        final String albumTitle = (String) album.get("title");
        final FilePackage fp = FilePackage.getInstance();
        if (!StringUtils.isEmpty(albumTitle)) {
            fp.setName(albumTitle.trim());
        } else {
            fp.setName(albumID);
        }
        if (!StringUtils.isEmpty(albumDescription)) {
            fp.setComment(albumDescription);
        }
        fp.setPackageKey("jpgchurch://album/" + albumID);
        final int imagesCount = ((Number) album.get("images_count")).intValue();
        final Set<String> dupes = new HashSet<String>();
        int page = 1;
        int lastPage = 1;
        pagination: do {
            final List<Object> images = (List<Object>) entries.get("images");
            int newItemsThisPage = 0;
            for (final Object imageO : images) {
                final Map<String, Object> image = (Map<String, Object>) imageO;
                final String encodedID = (String) image.get("encoded_id");
                final String url = "https://" + this.getHost() + "/img/" + encodedID;
                if (!dupes.add(url)) {
                    logger.info("Skipping dupe: " + url);
                    continue;
                }
                newItemsThisPage++;
                final DownloadLink link = this.createDownloadlink(url);
                final String originalFilename = (String) image.get("original_filename");
                link.setFinalFileName(originalFilename);
                final Number sizeBytes = (Number) image.get("size_bytes");
                link.setVerifiedFileSize(sizeBytes.longValue());
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            }
            final Map<String, Object> paginationMap = (Map<String, Object>) entries.get("pagination");
            lastPage = ((Number) paginationMap.get("last_page")).intValue();
            logger.info("Crawled page " + page + "/" + lastPage + " | Found items so far: " + ret.size() + "/" + imagesCount);
            if (this.isAbort()) {
                throw new InterruptedException();
            } else if (ret.size() == imagesCount) {
                logger.info("Stopping because: Found all items");
                break pagination;
            } else if (page == lastPage) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (newItemsThisPage == 0) {
                logger.info("Stopping because: Found no new items on current page");
                break pagination;
            }
            /* Fetch next page (first page was already delivered as part of the initial album request) */
            page++;
            br.getPage("https://" + this.getHost() + "/api/albums/" + albumID + "/images?page=" + page);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } while (page <= lastPage);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    public static Form getPasswordForm(final Browser br) {
        Form pwform = br.getFormbyKey("content-password");
        if (pwform != null) {
            /* Fix bad default action */
            pwform.setAction("");
        }
        return pwform;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* Avoid hitting rate-limits. */
        return 1;
    }
}
