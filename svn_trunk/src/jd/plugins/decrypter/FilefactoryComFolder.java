//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
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
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

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
import jd.plugins.hoster.FileFactory;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;

@DecrypterPlugin(revision = "$Revision: 51472 $", interfaceVersion = 2, names = { "filefactory.com" }, urls = { "https?://(?:www\\.)?filefactory\\.com/((?:folder|f)/[\\w]+|share/fi[a-z0-9,:]+)" })
@PluginDependencies(dependencies = { FileFactory.class })
public class FilefactoryComFolder extends PluginForDecrypt {
    public FilefactoryComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return jd.plugins.hoster.FileFactory.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/((?:folder|f)/[\\w]+|share/fi[a-z0-9,:]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String[] fileIDs = new Regex(param.getCryptedUrl(), "fi:([a-z0-9]+)").getColumn(0);
        if (fileIDs != null && fileIDs.length > 0) {
            /* 2019-08-17: New type of folder which contains all fileIDs inside URL */
            final FilePackage fp = FilePackage.getInstance();
            for (final String fileid : fileIDs) {
                final String url = "http://www." + this.getHost() + "/file/" + fileid;
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
            }
        } else {
            br.getHeaders().put("Accept-Language", "en-gb, en;q=0.8");
            br.getPage(param.getCryptedUrl() + "/?sort=filename&order=ASC&show=100&page=1");
            /* Error handling */
            if (br.getHttpConnection().getResponseCode() == 404) {
                /* Offline folder */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("(?i)No Files found in this folder")) {
                /* Empty folder */
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
            }
            final String fpName = br.getRegex("<h1>Files in <span>(.*?)</span>").getMatch(0);
            int maxPage = 1;
            final String maxPageStr = br.getRegex("data\\-paginator\\-totalPages=\"(\\d+)\"").getMatch(0);
            if (maxPageStr != null) {
                maxPage = Integer.parseInt(maxPageStr);
            }
            for (int i = 1; i <= maxPage; i++) {
                if (i > 1) {
                    br.getPage(param.getCryptedUrl() + "/?sort=filename&order=ASC&show=100&page=" + i);
                }
                final String links[] = br.getRegex(Pattern.compile("\"(https?://(?:www\\.)?filefactory\\.com/file/[^<>\"]*?)\"", Pattern.CASE_INSENSITIVE)).getColumn(0);
                for (String element : links) {
                    ret.add(createDownloadlink(element));
                }
            }
            if (!StringUtils.isEmpty(fpName)) {
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(Encoding.htmlDecode(fpName).trim());
                fp.addLinks(ret);
            }
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}