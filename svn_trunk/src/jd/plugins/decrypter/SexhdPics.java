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
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 50089 $", interfaceVersion = 3, names = {}, urls = {})
public class SexhdPics extends PluginForDecrypt {
    public SexhdPics(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sexhd.pics" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/gallery/([a-z0-9\\-]+)/([a-z0-9\\-]+)/([a-z0-9\\-]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 429) {
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
        }
        final String titleSlug = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(2);
        String title = br.getRegex("<h4>Gallery :([^<]+)</h4>").getMatch(0);
        if (title == null) {
            title = titleSlug.replace("-", " ").trim();
        }
        title = Encoding.htmlDecode(title).trim();
        final String[] links = br.getRegex("class=.?relativetop*><a href=\"([^\"]+)").getColumn(0);
        if (links == null || links.length == 0) {
            if (br.containsHTML("Gallery\\s*: ! !\\s*</h4>")) {
                /* Dead/empty gallery */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final HashSet<String> dupes = new HashSet<String>();
        for (String url : links) {
            if (!dupes.add(url)) {
                continue;
            }
            url = br.getURL(url).toExternalForm();
            final DownloadLink image = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
            image.setAvailable(true);
            ret.add(image);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2024-11-07: Avoid running into rate limit too early */
        return 1;
    }
}
