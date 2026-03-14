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
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52493 $", interfaceVersion = 3, names = {}, urls = {})
public class ImagetwistComGallery extends PluginForDecrypt {
    public ImagetwistComGallery(PluginWrapper wrapper) {
        super(wrapper);
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
        ret.add(new String[] { "imagetwist.com" });
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

    private static final Pattern PATTERN_1 = Pattern.compile("/p/(\\w+)/(\\d+)(/([^/]+))?");
    private static final Pattern PATTERN_2 = Pattern.compile("/\\?.+");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_1.pattern().substring(1) + "|" + PATTERN_2.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String contenturl = param.getCryptedUrl();
        final Regex urlinfo = new Regex(contenturl, PATTERN_1);
        String uploader_name = null;
        String folder_id = null;
        int items_per_page = 40;
        if (urlinfo.patternFind()) {
            uploader_name = urlinfo.getMatch(0);
            folder_id = urlinfo.getMatch(1);
        } else {
            final UrlQuery query = UrlQuery.parse(contenturl);
            uploader_name = query.get("usr_login");
            folder_id = query.get("fld_id");
            final String items_per_page_str = query.get("per_page");
            if (items_per_page_str != null && items_per_page_str.matches("\\d{2,3}")) {
                items_per_page = Integer.parseInt(items_per_page_str);
            }
        }
        final String title_from_url = urlinfo.getMatch(3);
        if (StringUtils.isEmpty(uploader_name) || StringUtils.isEmpty(folder_id)) {
            /*
             * We cannot simply exclude all invalid URLs using the plugin pattern so we need to catch theoretical cases where user adds
             * invalid urls for example: https://imagetwist.com/?blabla
             */
            logger.info("User added invalid url: " + contenturl);
            return ret;
        }
        /*
         * 2026-03-13: Their website is buggy so we need to use the url-format down below otherwise pagination may fail and return the same
         * items for every page.
         */
        contenturl = "https://" + getHost() + "/?fld_id=" + folder_id + "&per_page=" + items_per_page + "&op=user_public&usr_login=" + uploader_name;
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"err\"[^>]*>\\s*No such folder")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String title = br.getRegex("page_main_title\"[^>]*>([^<]+)<").getMatch(0);
        final String title_to_use;
        if (title != null) {
            title_to_use = Encoding.htmlDecode(title).trim();
        } else if (title_from_url != null) {
            /* Fallback */
            title_to_use = Encoding.htmlDecode(title_from_url).trim();
        } else {
            /* Final fallback */
            logger.warning("Failed to find nice title");
            title_to_use = folder_id;
        }
        final String number_of_files_str = br.getRegex("\\((\\d+) total\\)\\s*</small>").getMatch(0);
        if (number_of_files_str == null) {
            logger.info("Failed to find expected number of files -> Strong indicator that this gallery contains zero elements");
        } else {
            if (number_of_files_str.equals("0")) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, title_to_use);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title_to_use);
        fp.setPackageKey(this.getHost() + "://folder/" + folder_id);
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        pagination: do {
            final String[] file_ids = br.getRegex("(?-i)" + Pattern.quote(br.getHost()) + "/([a-z0-9]{12})").getColumn(0);
            if (file_ids == null || file_ids.length == 0) {
                logger.info("Stopping because: No links found on page " + page);
                break pagination;
            }
            final int sizeBefore = ret.size();
            for (final String file_id : file_ids) {
                if (!dupes.add(file_id)) {
                    /* Skip duplicates */
                    continue;
                }
                final String url = br.getURL("/" + file_id).toExternalForm();
                final DownloadLink image = createDownloadlink(url);
                /* Set temporary filename */
                image.setName(file_id + ".jpg");
                image._setFilePackage(fp);
                /* Assume that this item is online. */
                image.setAvailable(true);
                ret.add(image);
                distribute(image);
            }
            if (ret.size() == sizeBefore) {
                logger.info("Stopping because: No new items found on page " + page);
                // break;
            }
            logger.info("Crawled page " + page + " | Found " + ret.size() + "/" + number_of_files_str + " items so far");
            page++;
            String nextPageUrl = br.getRegex("'((\\?|/)[^\"']+page=" + page + ")'").getMatch(0);
            if (nextPageUrl == null) {
                logger.info("Stopping because: Failed to find next page URL");
                break pagination;
            }
            nextPageUrl = Encoding.htmlDecode(nextPageUrl);
            br.getPage(nextPageUrl);
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            /* 2026-03-13: Empty folder without given number_of_files_str */
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, title_to_use);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}