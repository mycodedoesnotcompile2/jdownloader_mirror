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

import org.jdownloader.plugins.components.youtube.YoutubeHelper;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

/**
 *
 * @author butkovip
 *
 */
@DecrypterPlugin(revision = "$Revision: 51405 $", interfaceVersion = 2, urls = {}, names = {})
public class TopkySk extends PluginForDecrypt {
    public TopkySk(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "topky.sk" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/cl?/(\\d+)/(\\d+)/([a-zA-Z0-9\\-]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink cryptedLink, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.clearCookies(getHost());
        final String contenturl = cryptedLink.getCryptedUrl();
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String contentID = new Regex(contenturl, this.getSupportedLinks()).getMatch(1);
        // extract img.zoznam.sk like vids
        String[][] links = br.getRegex("fo\\.addVariable[(]\"file\", \"(.*?)\"[)]").getMatches();
        if (null != links && 0 < links.length) {
            for (String[] link : links) {
                if (null != link && 1 == link.length && null != link[0] && 0 < link[0].length()) {
                    ret.add(createDownloadlink(link[0]));
                }
            }
        }
        // extract youtube links
        links = br.getRegex("<PARAM NAME=\"movie\" VALUE=\"http://www.youtube.com/v/(.*?)&").getMatches();
        if (null != links && 0 < links.length) {
            for (String[] link : links) {
                if (null != link && 1 == link.length && null != link[0] && 0 < link[0].length()) {
                    ret.add(createDownloadlink(YoutubeHelper.generateSingleVideoContentURL(link[0])));
                }
            }
        }
        // extract instagram links
        final String[] instagramlinks = br.getRegex("data-instgrm-permalink=\"(https?://[^\"]+)").getColumn(0);
        if (null != instagramlinks && 0 < instagramlinks.length) {
            for (final String instagramlink : instagramlinks) {
                ret.add(createDownloadlink(instagramlink));
            }
        }
        // extract topky.sk http vids
        final String finallink = br.getRegex("<source src=\"(http[^<>\"]*?)\"").getMatch(0);
        if (finallink != null) {
            ret.add(createDownloadlink(DirectHTTP.createURLForThisPlugin(finallink)));
        }
        /* 2022-06-14: Selfhosted hls */
        final String[] hlsplaylists = br.getRegex("(https?://img\\.topky\\.sk/video/\\d+/master\\.m3u8)").getColumn(0);
        for (final String hlsplaylist : hlsplaylists) {
            ret.add(createDownloadlink(hlsplaylist));
        }
        final String urlSlug = br.getURL().substring(br.getURL().lastIndexOf("/") + 1);
        final String title = urlSlug.replace("-", " ").trim();
        if (br.containsHTML("class=\"box-audio-content\"")) {
            /*
             * Article read out as audio file. This is not availabble for all articles but the website also just tries it and hides the
             * audio button if the file is not available (lol).
             */
            final DownloadLink audio = createDownloadlink(DirectHTTP.createURLForThisPlugin("https://img.topky.sk/audio/" + contentID + ".mp3"));
            audio.setFinalFileName(contentID + "_" + title + ".mp3");
            ret.add(audio);
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey(this.getHost() + "_" + contentID);
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}