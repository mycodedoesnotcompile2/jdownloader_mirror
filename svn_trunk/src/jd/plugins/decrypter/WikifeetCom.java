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
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.WikifeetComConfig;
import org.jdownloader.plugins.components.config.WikifeetComConfig.AlbumPackagenameScheme;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 50931 $", interfaceVersion = 3, names = {}, urls = {})
public class WikifeetCom extends PluginForDecrypt {
    public WikifeetCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "wikifeet.com", "wikifeetx.com" });
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
            ret.add("https?://(?!pics|thumbs\\.)(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/(?!photos|videos|privacy|upload|rules)([\\w-]+).*");
        }
        return ret.toArray(new String[0]);
    }

    // public static final String type_pic = "https?://(?:\\w+\\.)?pics\\.wikifeetx?\\.com";
    public static final String type_wikifeet = "https?://(?:\\w+\\.)?wikifeetx?\\.com/[^/]+";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String gallery_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String image_id = new Regex(param.getCryptedUrl(), "pid=(\\d+)").getMatch(0);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getRequest().getHtmlCode().length() <= 100) {
            /* Blank page -> Item is offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!param.getCryptedUrl().matches(type_wikifeet)) {
            /* Unsupported URL --> Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String json = this.br.getRegex("tdata = (\\{.+\\});").getMatch(0);
        if (json == null) {
            /* Not an image gallery */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        // final String contentID = br.getRegex("messanger\\.cid = (\\d+);").getMatch(0);
        String modelName = (String) entries.get("cname");
        if (modelName == null) {
            modelName = br.getRegex("messanger\\.cfname = '(.*?)';").getMatch(0);
        }
        if (modelName == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        modelName = Encoding.htmlDecode(modelName).trim();
        final List<Map<String, Object>> data = (List<Map<String, Object>>) entries.get("gallery");
        if (data.isEmpty()) {
            logger.info("Gallery contains zero elements");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        boolean foundTargetImage = false;
        for (final Map<String, Object> imagemap : data) {
            final Object removedO = imagemap.get("removed");
            final String pid = imagemap.get("pid").toString();
            final String directurl = "https://pics.wikifeet.com/" + pid + ".jpg";
            final DownloadLink dl = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
            final String finalFilename = modelName + "_" + pid + ".jpg";
            dl.setFinalFileName(finalFilename);
            dl.setProperty(DirectHTTP.FIXNAME, finalFilename);
            dl.setContentUrl("https://" + br.getHost() + "/" + gallery_id + "#pid=" + pid);
            dl.setLinkID(this.getHost() + "://image/" + pid);
            if (removedO != null && removedO.toString().equals("1")) {
                dl.setAvailable(false);
            } else {
                dl.setAvailable(true);
            }
            ret.add(dl);
            if (image_id != null && pid.equals(image_id)) {
                /* User wants specific image only */
                ret.clear();
                ret.add(dl);
                foundTargetImage = true;
                break;
            }
        }
        if (image_id != null && !foundTargetImage) {
            logger.info("Failed to find target image with id " + image_id + " | Returning all images instead");
        }
        final String fallbackValue = "[ Not set ]"; // Same as website
        final Object shoesizeO = entries.get("ssize");
        String shoesize = shoesizeO != null ? shoesizeO.toString() : null;
        if (shoesizeO != null) {
            shoesize = shoesizeO.toString();
        } else {
            shoesize = fallbackValue;
        }
        String birthplace = (String) entries.get("bplace");
        if (birthplace == null) {
            birthplace = br.getRegex("Birthplace\\s*:\\s*<span[^>]*>([^<]+)<a").getMatch(0);
        }
        if (!StringUtils.isEmpty(birthplace)) {
            birthplace = Encoding.htmlDecode(birthplace).trim();
        } else {
            birthplace = fallbackValue;
        }
        String birthdate = (String) entries.get("bdate");
        if (birthdate != null) {
            birthdate = Encoding.htmlDecode(birthdate).trim();
            /* Get same value which website displaye (yyyy-MM-dd) */
            final String dateUntilDay = new Regex(birthdate, "(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
            if (dateUntilDay != null) {
                birthdate = dateUntilDay;
            }
        } else {
            birthdate = fallbackValue;
        }
        final String imdbURL = br.getRegex("(http?://(?:www\\.)?imdb\\.com/name/nm\\d+)").getMatch(0);
        /* Generate packagename */
        final WikifeetComConfig cfg = PluginJsonConfig.get(getConfigInterface());
        final String customPackagenameScheme = cfg.getCustomPackagenameScheme();
        String packagename;
        if (!StringUtils.isEmpty(customPackagenameScheme) && cfg.getAlbumPackagenameScheme() == AlbumPackagenameScheme.CUSTOM) {
            packagename = customPackagenameScheme;
        } else {
            packagename = "*user*";
        }
        packagename = packagename.replace("*user*", modelName);
        packagename = packagename.replace("*birth_place*", birthplace != null ? birthplace : "");
        packagename = packagename.replace("*birth_date*", birthdate != null ? birthdate : "");
        packagename = packagename.replace("*shoe_size*", shoesize != null ? shoesize : "");
        packagename = packagename.replace("*imdb_url*", imdbURL != null ? imdbURL : "");
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(packagename);
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public Class<WikifeetComConfig> getConfigInterface() {
        return WikifeetComConfig.class;
    }
}
