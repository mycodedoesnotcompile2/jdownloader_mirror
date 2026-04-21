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
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.FilesterMe;

@DecrypterPlugin(revision = "$Revision: 52685 $", interfaceVersion = 3, names = {}, urls = {})
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
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, PATTERN_NORMAL).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("class=\"folder-title\"[^>]*>\\s*([^<]+)\\s*</h1>").getMatch(0);
        String folder_title;
        if (title != null) {
            folder_title = Encoding.htmlDecode(title).trim();
        } else {
            logger.warning("Failed to find folder title, using folder_id as fallback");
            folder_title = folder_id;
        }
        dupes.add(folder_id);
        String path = this.getAdoptedCloudFolderStructure();
        if (path == null) {
            path = folder_title;
        } else {
            /* Current path = Last path + current folder_title. */
            path += "/" + folder_title;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(path);
        fp.setPackageKey("filester://folder/" + folder_id);
        pagination: do {
            final String[] filenames = br.getRegex("data-name=\"([^\"]+)").getColumn(0);
            final String[] filesizes = br.getRegex("data-size=\"(\\d+)").getColumn(0);
            final String[] file_ids = br.getRegex("downloadFile\\('([a-zA-Z0-9]+)'\\)").getColumn(0);
            final String[] folder_ids = br.getRegex("/f/([a-f0-9]{16})").getColumn(0);
            if ((file_ids == null || file_ids.length == 0) && (folder_ids == null || folder_ids.length == 0)) {
                /**
                 * 2026-04-20: Important! <br>
                 * Only check for empty folders if we were really unable to find any items! <br>
                 * Website is buggy so for folders with 0 files and 1+ subfolders it also displays the "empty folder" message!
                 */
                if (br.containsHTML("class=\"empty-state\"")) {
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, fp.getName());
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            int numberofNewItems = 0;
            if (file_ids != null) {
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
                    link.setRelativeDownloadFolderPath(path);
                    ret.add(link);
                    distribute(link);
                }
            }
            if (folder_ids != null) {
                for (final String folder_id_entry : folder_ids) {
                    if (!dupes.add(folder_id_entry)) {
                        continue;
                    }
                    numberofNewItems++;
                    final String url = "https://" + br.getHost() + "/f/" + folder_id_entry;
                    final DownloadLink link = this.createDownloadlink(url);
                    link.setRelativeDownloadFolderPath(path);
                    ret.add(link);
                    distribute(link);
                }
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
            /* Short delay in hope of avoiding rate limit. */
            this.sleep(2000, param);
            page++;
            br.getPage(contenturl + "?page=" + page);
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* Try to avoid rate limit */
        return 1;
    }
}
