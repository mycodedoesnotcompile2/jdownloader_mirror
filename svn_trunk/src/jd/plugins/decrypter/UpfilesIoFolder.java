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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
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
import jd.plugins.hoster.UpfilesIo;

@DecrypterPlugin(revision = "$Revision: 50182 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { UpfilesIo.class })
public class UpfilesIoFolder extends PluginForDecrypt {
    public UpfilesIoFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        return UpfilesIo.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/f/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final UpfilesIo hosterplugin = (UpfilesIo) this.getNewPluginForHostInstance(this.getHost());
        final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
        final HashSet<String> dupes = new HashSet<String>();
        dupes.add(br.getURL());
        for (final String url : urls) {
            if (!dupes.add(url)) {
                continue;
            } else if (!hosterplugin.canHandle(url)) {
                continue;
            }
            final DownloadLink file = createDownloadlink(url);
            final String fid = hosterplugin.getFID(file);
            if (!UpfilesIo.isValidFileID(fid)) {
                /* Skip invalid items */
                continue;
            }
            file.setDefaultPlugin(hosterplugin);
            file.setHost(this.getHost());
            ret.add(file);
        }
        if (ret.isEmpty()) {
            /* No results but also no error -> Assume that folder is empty. */
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        String folderTitle = br.getRegex("<h3>Folder\\s*: ([^<]+)</h3>").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (folderTitle != null) {
            fp.setName(Encoding.htmlDecode(folderTitle).trim());
        } else {
            /* Fallback */
            final String folderID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
            fp.setName(folderID);
        }
        fp.addLinks(ret);
        return ret;
    }
}
