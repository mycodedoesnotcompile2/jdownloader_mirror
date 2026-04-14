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

import jd.PluginWrapper;
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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.FilesterMe;

@DecrypterPlugin(revision = "$Revision: 52649 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { FilesterMe.class })
public class FilesterMeFolder extends PluginForDecrypt {
    public FilesterMeFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(FilesterMe.getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(FilesterMe.getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/f/([a-f0-9]{16})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : FilesterMe.getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_NORMAL.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int page = 1;
        final HashSet<String> dupes = new HashSet<String>();
        FilePackage fp = FilePackage.getInstance();
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, PATTERN_NORMAL).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("class=\"folder-title\"[^>]*>([^<]+)</h1>").getMatch(0);
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        } else {
            logger.warning("Failed to find folder title");
            fp.setName(folder_id);
        }
        fp.setPackageKey("filester://folder/" + folder_id);
        pagination: do {
            final String[] filenames = br.getRegex("data-name=\"([^\"]+)").getColumn(0);
            final String[] filesizes = br.getRegex("data-size=\"(\\d+)").getColumn(0);
            final String[] file_ids = br.getRegex("downloadFile\\('([a-zA-Z0-9]+)'\\)").getColumn(0);
            if (file_ids == null || file_ids.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            int numberofNewItems = 0;
            int i = -1;
            for (final String file_id : file_ids) {
                i++;
                if (!dupes.add(file_id)) {
                    continue;
                }
                numberofNewItems++;
                final String url = "https://" + br.getHost() + "/d/" + file_id;
                final DownloadLink link = this.createDownloadlink(url);
                if (filenames != null && filenames.length == file_ids.length) {
                    link.setName(Encoding.htmlDecode(filenames[i]).trim());
                } else if (i == 0) {
                    /* Log only once */
                    logger.warning("Failed to find filename information");
                }
                if (filesizes != null && filesizes.length == file_ids.length) {
                    link.setDownloadSize(Long.parseLong(filesizes[i]));
                } else if (i == 0) {
                    /* Log only once */
                    logger.warning("Failed to find filesize information");
                }
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            if (numberofNewItems == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            } else if (!br.containsHTML("page=" + (page + 1))) {
                logger.info("Stopping because: Reached end?");
                break pagination;
            } else if (this.isAbort()) {
                throw new InterruptedException();
            }
            /* Continue to next page */
            page++;
            br.getPage(contenturl + "?page=" + page);
        } while (!this.isAbort());
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
