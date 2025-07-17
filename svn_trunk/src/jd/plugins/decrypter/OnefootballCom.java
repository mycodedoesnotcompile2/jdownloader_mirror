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

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
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

@DecrypterPlugin(revision = "$Revision: 51178 $", interfaceVersion = 3, names = {}, urls = {})
public class OnefootballCom extends PluginForDecrypt {
    public OnefootballCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "onefootball.com", "onefootball.tv" });
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
            ret.add("https?://(?:tv\\.)?" + buildHostsPatternPart(domains) + "/(([a-z]{2})/)?clips/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        String lang = urlinfo.getMatch(0);
        if (lang == null) {
            lang = "es";
        }
        final String clipID = urlinfo.getMatch(2);
        br.getPage("https://onefootball.com/proxy-web-experience/" + lang + "/tv-hub/videos/" + clipID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* 2025-07-03: Server will answer with plaintext content "null" on wrong clip_id. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Map<String, Object> entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> videojsPlayer = (Map<String, Object>) entries.get("videojsPlayer");
        final List<Map<String, Object>> playlist = (List<Map<String, Object>>) videojsPlayer.get("playlist");
        for (final Map<String, Object> playlist_item : playlist) {
            final String url_poster = playlist_item.get("poster").toString();
            final DownloadLink thumbnail = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url_poster));
            thumbnail.setAvailable(true);
            ret.add(thumbnail);
            final List<Map<String, Object>> sources = (List<Map<String, Object>>) playlist_item.get("sources");
            for (final Map<String, Object> source : sources) {
                final String url_video = source.get("src").toString();
                ret.add(this.createDownloadlink(url_video));
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(entries.get("title").toString());
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
