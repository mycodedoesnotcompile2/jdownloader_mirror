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
import java.util.Map;

import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50261 $", interfaceVersion = 3, names = {}, urls = {})
public class CdVsCom extends PluginForDecrypt {
    public CdVsCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "cd-vs.com" });
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
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/embed/([a-f0-9\\-]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contentID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* Without referer, we will get response code 404! */
        br.getHeaders().put("Referer", param.getCryptedUrl());
        br.getHeaders().put("Content-Type", "application/json");
        br.getPage("https://emb.cd-vs.com/api/get-video?id=" + contentID + "&counter=0&prev_id=");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Object urlsO = entries.get("urls");
        final Object urlO = entries.get("url");
        if (urlO == null && urlsO == null) {
            /* E.g. {"thumbnail":"\/assets\/img\/bg.jpg"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Results mostly link to blogger.com and final files are then mostly hosted on Google Drive or to seaporn.net. */
        if (urlsO != null) {
            final List<String> urls = (List<String>) urlsO;
            for (final String url : urls) {
                ret.add(createDownloadlink(url));
            }
        }
        if (urlO != null) {
            final String url = urlO.toString();
            ret.add(createDownloadlink(url));
        }
        return ret;
    }
}
