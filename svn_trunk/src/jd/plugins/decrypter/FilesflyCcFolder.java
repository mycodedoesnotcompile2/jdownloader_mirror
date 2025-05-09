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

import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.hoster.FilesflyCc;

@DecrypterPlugin(revision = "$Revision: 51053 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { FilesflyCc.class })
public class FilesflyCcFolder extends PluginForDecrypt {
    public FilesflyCcFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookie(getHost(), "lang", "english");
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return FilesflyCc.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folders/([a-z0-9\\-]{10,})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*No such folder")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Contains files\\s*:\\s*0")) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        final String folder_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        String folderTitle = br.getRegex("Folder\\s*:\\s*<strong>([^<]+)</strong>").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (folderTitle != null) {
            fp.setName(Encoding.htmlDecode(folderTitle).trim());
        } else {
            fp.setName(folder_id);
        }
        final String op = br.getRegex("op:\\s*'([^']+)").getMatch(0);
        final String fld_rand = br.getRegex("fld_rand:\\s*'([^']+)").getMatch(0);
        final String total = br.getRegex("total:\\s*'([^']+)").getMatch(0);
        final String perpage = br.getRegex("perpage:\\s*'([^']+)").getMatch(0);
        final String fld_id = br.getRegex("fld_id:\\s*'([^']+)").getMatch(0);
        final String token = br.getRegex("token:\\s*'([^']+)").getMatch(0);
        final int numberofItemsTotal = total != null ? Integer.parseInt(total) : -1;
        final int numberofItemsPerPage = perpage != null ? Integer.parseInt(perpage) : 50;
        int estimatedNumberofPages = 1;
        if (perpage != null && numberofItemsTotal > 0) {
            estimatedNumberofPages = numberofItemsTotal / numberofItemsPerPage;
        }
        if (numberofItemsTotal == 0) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        int page = 1;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> globalDupes = new HashSet<String>();
        pagination: do {
            final String[] urls = br.getRegex("(/[a-z0-9]{12})\"").getColumn(0);
            if (urls == null || urls.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final List<String> linksWithoutDupes = new ArrayList<String>();
            for (String url : urls) {
                if (linksWithoutDupes.contains(url)) {
                    continue;
                }
                linksWithoutDupes.add(url);
            }
            final String[] htmls = br.getRegex("<div class=\"info\">(.*?)</div>").getColumn(0);
            int index = 0;
            int numberofNewItemsThisPage = 0;
            for (String url : linksWithoutDupes) {
                if (!globalDupes.add(url)) {
                    continue;
                }
                numberofNewItemsThisPage++;
                url = br.getURL(url).toExternalForm();
                final DownloadLink link = createDownloadlink(url);
                link.setAvailable(true);
                if (htmls != null && htmls.length == linksWithoutDupes.size()) {
                    final String html = htmls[index];
                    final String filename = new Regex(html, ">([^<]+)</a>").getMatch(0);
                    if (filename != null) {
                        link.setName(Encoding.htmlDecode(filename).trim());
                    }
                    final String filesize = new Regex(html, "<span>Uploaded [^|]+ \\| (\\d+[^<]+)</span>").getMatch(0);
                    if (filesize != null) {
                        link.setDownloadSize(SizeFormatter.getSize(filesize));
                    }
                }
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                index++;
            }
            logger.info("Crawled page " + page + "/" + estimatedNumberofPages + " | Found items: " + ret.size() + "/" + total);
            if (ret.size() == numberofItemsTotal) {
                logger.info("Stopping because: Found all items");
                break pagination;
            } else if (numberofNewItemsThisPage < numberofItemsPerPage) {
                logger.info("Stopping because: Looks like we found all items");
                break pagination;
            } else if (op == null || fld_rand == null || fld_id == null || token == null) {
                logger.info("Stopping because: Pagination broken");
                break pagination;
            }
            /* Load next page */
            page++;
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("op", op);
            query.appendEncoded("load_files_list", "1");
            query.appendEncoded("page", Integer.toString(page));
            query.appendEncoded("fld_id", fld_id);
            query.appendEncoded("token", token);
            query.appendEncoded("fld_rand", fld_rand);
            br.postPage("/", query);
        } while (!this.isAbort());
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.SibSoft_XFileShare;
    }
}
