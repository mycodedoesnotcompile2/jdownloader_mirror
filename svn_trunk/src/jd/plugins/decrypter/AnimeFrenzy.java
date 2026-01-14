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
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.GenericM3u8;

@DecrypterPlugin(revision = "$Revision: 52088 $", interfaceVersion = 2, names = {}, urls = {})
public class AnimeFrenzy extends PluginForDecrypt {
    public AnimeFrenzy(PluginWrapper wrapper) {
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
        /* 2025-11-27: All domains except animefrenzy.cc are dead */
        ret.add(new String[] { "animefrenzy.cc", "animefrenzy.vip", "animefrenzy.net", "animefrenzy.org" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("animefrenzy.vip");
        deadDomains.add("animefrenzy.org");
        return deadDomains;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:anime|cartoon|watch|stream)/([\\w-]+)-(\\d+)(\\?ep=(\\d+))?");
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
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        final String seriesTitleSlug = urlinfo.getMatch(0);
        final String seriesTitleFromSlug = seriesTitleSlug.replace("-", " ").trim();
        final String seriesID = urlinfo.getMatch(1);
        final String episodeID = UrlQuery.parse(br.getURL()).get("ep");
        String title = br.getRegex("<title>(?:Watch\\s+)?([^<]+)\\s+(?:- Watch Anime Online|English\\s+[SD]ub\\s+)").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        } else {
            fp.setName(seriesTitleFromSlug);
        }
        if (episodeID == null && seriesID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Browser brc = br.cloneBrowser();
        if (episodeID != null) {
            /* 2026-01-09 */
            brc.getPage("https://nine.mewcdn.online/ajax/episode/servers?episodeId=" + episodeID);
            final Map<String, Object> epinfo = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(epinfo.get("status"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String html = epinfo.get("html").toString();
            br.getRequest().setHtmlCode(html);
            final String episodeinfo = br.getRegex("(Episode \\d+)").getMatch(0);
            if (episodeinfo != null) {
                fp.setName(seriesTitleFromSlug + " - " + episodeinfo);
            }
            final String[] mirror_ids = br.getRegex("data-id=\"(\\d+)").getColumn(0);
            for (final String mirror_id : mirror_ids) {
                brc.getPage("/ajax/episode/sources?id=" + mirror_id);
                final Map<String, Object> mirrorinfo = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final String url = mirrorinfo.get("link").toString();
                final DownloadLink link = this.createDownloadlink(url);
                /* Important for some items e.g. rapid-cloud.co, streameeeeee.site */
                link.setReferrerUrl(br.getURL());
                link.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, fp.getName());
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
        } else {
            /* Crawl all episodes of a series */
            /* Alternative domain: hianimez.to */
            brc.getPage("https://hianime.to/ajax/v2/episode/list/" + seriesID);
            final Map<String, Object> resp = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(resp.get("status"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String html = resp.get("html").toString();
            br.getRequest().setHtmlCode(html);
            final String[] urls = br.getRegex("(/watch/[a-zA-Z0-9-]+" + seriesID + "\\?ep=\\d+)").getColumn(0);
            if (urls == null || urls.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (String url : urls) {
                url = br.getURL(url).toExternalForm();
                final DownloadLink link = this.createDownloadlink(url);
                ret.add(link);
            }
        }
        fp.addLinks(ret);
        return ret;
    }
}