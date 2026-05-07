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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

@DecrypterPlugin(revision = "$Revision: 52781 $", interfaceVersion = 3, names = {}, urls = {})
public class FuraffinityNetCrawler extends PluginForDecrypt {
    public FuraffinityNetCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "furaffinity.net" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(gallery|scraps|user)/([^/]+)(/folder/(\\d+)(/([^/\\?]+))?)?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2020-08-19: Avoid http error-response 503 rate limit */
        return 1;
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        final String type = urlinfo.getMatch(0);
        final String username = urlinfo.getMatch(1);
        final String galleryID = urlinfo.getMatch(3);
        final String galleryTitle = urlinfo.getMatch(5);
        if (type.equalsIgnoreCase("user")) {
            /* These will go back into this crawler which will then crawl all items of that user. */
            ret.add(this.createDownloadlink("https://www." + this.getHost() + "/gallery/" + username));
            ret.add(this.createDownloadlink("https://www." + this.getHost() + "/scraps/" + username));
        } else {
            final FilePackage fp = FilePackage.getInstance();
            if (galleryTitle != null) {
                fp.setName(username + " - " + type + " - " + galleryTitle);
            } else if (galleryID != null) {
                fp.setName(username + " - " + type + " - " + galleryID);
            } else {
                fp.setName(username + " - " + type);
            }
            int page = 1;
            /* Login if account is available */
            final Account acc = AccountController.getInstance().getValidAccount(this.getHost());
            if (acc != null) {
                final PluginForHost plg = getNewPluginForHostInstance(getHost());
                ((jd.plugins.hoster.FuraffinityNet) plg).login(acc, false);
            }
            String path = null;
            final HashSet<String> dupes = new HashSet<String>();
            String nextpageurl = contenturl + "/" + page;
            pagination: do {
                br.getPage(nextpageurl);
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (br.containsHTML(">\\s*System Message")) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                if (page == 1) {
                    /* Init path -> We want the path without the page of pagination. */
                    path = br._getURL().getPath().substring(0, br._getURL().getPath().lastIndexOf("/"));
                }
                final String json = br.getRegex("id=\"js-submissionData\" type=\"application/json\"[^>]*>(\\{.*?\\})</script>").getMatch(0);
                final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
                final Iterator<Entry<String, Object>> iterator = entries.entrySet().iterator();
                int newItemsCounter = 0;
                while (iterator.hasNext()) {
                    final Entry<String, Object> entry = iterator.next();
                    final String itemID = entry.getKey();
                    if (!dupes.add(itemID)) {
                        continue;
                    }
                    final Map<String, Object> itemProperties = (Map<String, Object>) entry.getValue();
                    String title = (String) itemProperties.get("title");
                    final String description = (String) itemProperties.get("description");
                    if (StringUtils.isEmpty(title)) {
                        /* Fallback */
                        title = itemID;
                    }
                    title = Encoding.htmlDecode(title).trim();
                    final DownloadLink image = this.createDownloadlink("https://www.furaffinity.net/view/" + itemID);
                    image.setName(title + ".jpg");
                    if (!StringUtils.isEmpty(description)) {
                        image.setComment(description);
                    }
                    image.setAvailable(true);
                    image._setFilePackage(fp);
                    distribute(image);
                    newItemsCounter += 1;
                }
                logger.info("Crawled page " + page + " | Number of items on current page: " + newItemsCounter + " | Results so far: " + ret.size());
                if (newItemsCounter == 0) {
                    logger.info("Stopping because: Failed to find any new items");
                    break pagination;
                }
                page++;
                nextpageurl = br.getRegex(Pattern.quote(path) + "/" + page).getMatch(-1);
                if (nextpageurl == null) {
                    logger.info("Stopping because: Reached last page?");
                    break pagination;
                }
            } while (!this.isAbort());
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
