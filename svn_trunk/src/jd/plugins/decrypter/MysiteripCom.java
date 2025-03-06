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
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50749 $", interfaceVersion = 3, names = {}, urls = {})
public class MysiteripCom extends PluginForDecrypt {
    public MysiteripCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "mysiterip.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(\\d+)-([\\w\\-]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String content_id = urlinfo.getMatch(0);
        final String slug = urlinfo.getMatch(1);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(slug.replace("-", " ").trim());
        fp.setPackageKey(this.getHost() + "/content/" + content_id);
        br.getHeaders().put("x-requested-with", "XMLHttpRequest");
        /* Crawl thumbnails */
        final UrlQuery query1 = new UrlQuery();
        query1.appendEncoded("data_id", content_id);
        query1.appendEncoded("scr_page", "1");
        query1.appendEncoded("type", "screens");
        br.postPage("/wp-content/themes/chocowp/ajax.php", query1);
        final Map<String, Object> entries1 = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String html1 = entries1.get("scr_str").toString();
        final String[] imageurls = new Regex(html1, "so_galery_img\"[^<]*src=\"(https?:[^\"]+)").getColumn(0);
        for (final String imageurl : imageurls) {
            final DownloadLink image = this.createDownloadlink(imageurl);
            ret.add(image);
            image._setFilePackage(fp);
            distribute(image);
        }
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        /* Crawl link_ids */
        final UrlQuery query2 = new UrlQuery();
        query2.appendEncoded("data_id", content_id);
        query2.appendEncoded("scr_page", "1");
        query2.appendEncoded("type", "links");
        br.postPage("/wp-content/themes/chocowp/ajax.php", query2);
        final Map<String, Object> entries2 = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String html2 = entries2.get("fo_str").toString();
        final String[] data_ids = new Regex(html2, "data-num=\"(\\d+)\"").getColumn(0);
        if (data_ids == null || data_ids.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final HashSet<String> ids = new HashSet<String>();
        for (final String data_id : data_ids) {
            ids.add(data_id);
        }
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        /* Crawl file hoster links */
        int position = 1;
        for (final String id : ids) {
            logger.info("Crawling id " + position + "/" + ids.size() + " --> " + id);
            final UrlQuery query3 = new UrlQuery();
            query3.appendEncoded("data_id", content_id);
            query3.appendEncoded("type", "curr_link");
            query3.appendEncoded("num_link", id);
            br.postPage("/wp-content/themes/chocowp/ajax.php", query3);
            final Map<String, Object> entries3 = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String html3 = entries3.get("fo_str").toString();
            final String[] urls = new Regex(html3, "class=\"so_dwn_lnks so_dwn_curr_lnk\" target=\"_blank\" href=\"(https?:[^\"]+)").getColumn(0);
            for (final String url : urls) {
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            }
            if (this.isAbort()) {
                throw new InterruptedException();
            }
        }
        return ret;
    }
}
