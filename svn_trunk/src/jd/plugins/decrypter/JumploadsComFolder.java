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
import java.util.List;

import org.appwork.utils.formatter.SizeFormatter;

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
import jd.plugins.hoster.JumploadsCom;

@DecrypterPlugin(revision = "$Revision: 51956 $", interfaceVersion = 3, names = {}, urls = {})
public class JumploadsComFolder extends PluginForDecrypt {
    public JumploadsComFolder(PluginWrapper wrapper) {
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
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "jumploads.com", "goloady.com" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([A-Za-z0-9]+)(/([^/]+))?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* 2021-04-20: Main domain has changed from goloady.com to jumploads.com */
        final String oldDomain = Browser.getHost(param.getCryptedUrl());
        final String contenturl = param.getCryptedUrl().replace(oldDomain + "/", this.getHost() + "/");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (JumploadsCom.is_file_offline_html(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"tc empty-dir\"")) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String folder_id = urlinfo.getMatch(0);
        String titleFromURL = urlinfo.getMatch(2);
        String titleFromHTML = br.getRegex("<h2 class=\"name[^\"]*\">([^<]+)</h2>").getMatch(0);
        final String[] htmls = br.getRegex("<a [^>]+ data-isd[^>]+>.*?</div>\\s+</a>").getColumn(-1);
        if (htmls == null || htmls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String singleHTML : htmls) {
            String url = new Regex(singleHTML, "(?i)href=\"(https?://(?:www\\.)?[^/]+/(?:file|folder)/[^\"]+)").getMatch(0);
            final String filename = new Regex(singleHTML, "file-name-\\d+\"[^>]*>([^<]+)</h2>").getMatch(0);
            String filesize = new Regex(singleHTML, "<h2 class=\"s14 c777 font\"[^>]*>([^<]+)</h2>").getMatch(0);
            if (url == null) {
                /* Skip invalid objects */
                continue;
            }
            final DownloadLink dl = createDownloadlink(url);
            dl.setAvailable(true);
            if (filename != null) {
                dl.setName(Encoding.htmlDecode(filename).trim());
            }
            if (filesize != null) {
                dl.setDownloadSize(SizeFormatter.getSize(filesize));
            }
            ret.add(dl);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (titleFromHTML != null) {
            titleFromHTML = Encoding.htmlDecode(titleFromHTML).trim();
            fp.setName(titleFromHTML);
        } else if (titleFromURL != null) {
            titleFromURL = Encoding.htmlDecode(titleFromURL).trim();
            fp.setName(titleFromURL);
        } else {
            /* Final fallback */
            logger.warning("Failed to find folder_title");
            fp.setName(folder_id);
        }
        fp.setPackageKey("jumploads://folder/" + folder_id);
        fp.addLinks(ret);
        return ret;
    }
}
