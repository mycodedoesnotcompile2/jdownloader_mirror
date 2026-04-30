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
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52743 $", interfaceVersion = 3, names = {}, urls = {})
public class HubcloudFoo extends PluginForDecrypt {
    public HubcloudFoo(PluginWrapper wrapper) {
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
        ret.add(new String[] { "hubcloud.foo" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/drive/([a-z0-9]{10,})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String nextStepUrl = br.getRegex("var url = '(https?://[^\"\\']+)';").getMatch(0);
        if (nextStepUrl == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        br.getPage(nextStepUrl);
        /* title = filename */
        String title = br.getRegex("<title>([^<]+)").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
        }
        long filesize = -1;
        final String filesizeStr = br.getRegex("File Size\\s*<i[^>]*>(\\d+[^<]+)</i>").getMatch(0);
        if (filesizeStr != null) {
            filesize = SizeFormatter.getSize(filesizeStr);
        }
        final String mirror_directurl = br.getRegex("href=\"(https://[^\"]+)\" id=\"fsl\"").getMatch(0);
        if (mirror_directurl != null) {
            /* "Download [FSL Server]" */
            final DownloadLink direct = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(mirror_directurl));
            /* Assume that this item is online and downloadable */
            direct.setAvailable(true);
            ret.add(direct);
        }
        final String mirror_hubcdn = br.getRegex("href=\"(https?://gpdl\\.hubcdn\\.fans/[^\"]+)\"").getMatch(0);
        if (mirror_hubcdn != null) {
            /* "Download [Server: 10Gbps]" */
            final DownloadLink hubcdn = this.createDownloadlink(mirror_hubcdn);
            ret.add(hubcdn);
        }
        final String mirror_pixeldrain = br.getRegex("var pxl = \"(https://[^\"]+)\";").getMatch(0);
        if (mirror_pixeldrain != null) {
            /* "Download [PixelServer: 2]" */
            final DownloadLink pixeldrain = this.createDownloadlink(mirror_pixeldrain);
            ret.add(pixeldrain);
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find any mirrors");
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        }
        fp.setPackageKey("hubcloud://folder/" + folder_id);
        for (final DownloadLink result : ret) {
            result._setFilePackage(fp);
            if (title != null) {
                result.setName(title);
            }
            if (filesize != 1) {
                result.setDownloadSize(filesize);
            }
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
