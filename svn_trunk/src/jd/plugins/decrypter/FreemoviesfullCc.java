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
import java.util.Map;

import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.GenericM3u8;

@DecrypterPlugin(revision = "$Revision: 51559 $", interfaceVersion = 3, names = {}, urls = {})
public class FreemoviesfullCc extends PluginForDecrypt {
    public FreemoviesfullCc(PluginWrapper wrapper) {
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
        ret.add(new String[] { "freemoviesfull.cc" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/watch-tv/([a-z0-9-]+)-(\\d+)\\.(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String contenturl_after_browser_call = br.getURL();
        final String episode_id = br.getRegex("data-episode=\"(\\d+)").getMatch(0);
        if (episode_id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage("/ajax/episode/servers/" + episode_id);
        if (this.isAbort()) {
            /* Aborted by user */
            throw new InterruptedException();
        }
        final String[] mirror_ids = br.getRegex("data-id=\"(\\d+)").getColumn(0);
        if (mirror_ids == null || mirror_ids.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        HashSet<String> dupes = new HashSet<String>();
        FilePackage fp = FilePackage.getInstance();
        fp.setName(br._getURL().getPath());
        int i = -1;
        int numberofErrorItems = 0;
        for (final String mirror_id : mirror_ids) {
            i++;
            if (this.isAbort()) {
                /* Aborted by user */
                throw new InterruptedException();
            }
            if (!dupes.add(mirror_id)) {
                /* Skip dupes */
                continue;
            }
            logger.info("Crawled mirror " + (i + 1) + "/" + mirror_ids.length);
            br.getPage("/ajax/episode/sources/" + mirror_id);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String type = (String) entries.get("type");
            if ("error".equalsIgnoreCase(type)) {
                logger.info("Skipping offline/broken mirror_id: " + mirror_id);
                numberofErrorItems++;
                continue;
            }
            if (fp == null || i == 0) {
                fp = FilePackage.getInstance();
                fp.setName(entries.get("title").toString());
                fp.setPackageKey(this.getHost() + "://episode/" + episode_id);
            }
            final String url = entries.get("link").toString();
            final DownloadLink link = this.createDownloadlink(url);
            link._setFilePackage(fp);
            /* Set extra information important for VidcloudStreameeeeee crawler. */
            link.setReferrerUrl(contenturl_after_browser_call);
            link.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, fp.getName());
            ret.add(link);
            distribute(link);
        }
        if (numberofErrorItems == mirror_ids.length) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
