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

import org.appwork.utils.Regex;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.youtube.YoutubeHelper;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.MegaDpUa;

@DecrypterPlugin(revision = "$Revision: 51405 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { MegaDpUa.class })
public class MegaDpUaFolder extends PluginForDecrypt {
    public MegaDpUaFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        return MegaDpUa.getPluginDomains();
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
            final String hostsPatternPart = buildHostsPatternPart(domains);
            String regex = "https?://(?:www\\.)?" + hostsPatternPart + "/(?:[a-z]{2}/)?([A-Za-z0-9]{3,})";
            regex += "|https?://video\\." + hostsPatternPart + "/\\?video=[A-Za-z0-9\\-_]+";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    private static final String TYPE_VIDEO = "https?://video\\.[^/]+/\\?video=([A-Za-z0-9\\-_]+)";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Regex video = new Regex(param.getCryptedUrl(), TYPE_VIDEO);
        if (video.patternFind()) {
            final String videoID = video.getMatch(0);
            ret.add(this.createDownloadlink(YoutubeHelper.generateSingleVideoContentURL(videoID)));
        } else {
            br.getPage(param.getCryptedUrl());
            if (jd.plugins.hoster.MegaDpUa.isOffline(this.br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String passCode = null;
            if (this.getPasswordProtectedForm(br) != null) {
                final Form pwform = this.getPasswordProtectedForm(br);
                passCode = getUserInput("Password?", param);
                pwform.put("pass", Encoding.urlEncode(passCode));
                br.submitForm(pwform);
                if (this.getPasswordProtectedForm(br) != null) {
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
            }
            final PluginForHost plg = this.getNewPluginForHostInstance(this.getHost());
            final String[] folderHTMLs = br.getRegex("<tr><td><div class=\"urlfile\"(.*?)</td></tr>").getColumn(0);
            for (final String folderHTML : folderHTMLs) {
                final String filename = new Regex(folderHTML, ">([^<>\"]+)</div>").getMatch(0);
                final String filesize = new Regex(folderHTML, "<td style=[^>]*>([^<>\"]+)</td>").getMatch(0);
                final String url = new Regex(folderHTML, plg.getSupportedLinks()).getMatch(-1);
                final String directurl = new Regex(folderHTML, "class=\"hidden-link\"[^>]*data-link=\"(https://[^<>\"]+)").getMatch(0);
                if (url == null) {
                    /* Skip invalid items */
                    continue;
                }
                final String md5hash = new Regex(url, "([a-f0-9]{32})").getMatch(0);
                final DownloadLink dl = this.createDownloadlink(url);
                if (md5hash != null) {
                    dl.setMD5Hash(md5hash);
                }
                if (filename != null) {
                    /* 2021-02-26: They're tagging their filenames -> Prefer the ones we find here */
                    dl.setFinalFileName(Encoding.htmlDecode(filename).trim());
                }
                if (filesize != null) {
                    dl.setDownloadSize(SizeFormatter.getSize(filesize));
                }
                dl.setAvailable(true);
                if (passCode != null) {
                    dl.setDownloadPassword(passCode);
                }
                /* Saving this directurl will help us later so we can skip the password form ;) */
                if (directurl != null) {
                    dl.setProperty("free_directlink", directurl);
                }
                ret.add(dl);
            }
        }
        return ret;
    }

    private Form getPasswordProtectedForm(final Browser br) {
        return br.getFormbyKey("pass");
    }
}
