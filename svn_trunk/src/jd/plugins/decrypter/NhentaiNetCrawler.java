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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.NhentaiNet;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.antiDDoSForHost;
import org.jdownloader.plugins.controller.LazyPlugin;

/**
 *
 * @author raztoki
 *
 */
@DecrypterPlugin(revision = "$Revision: 51332 $", interfaceVersion = 2, names = {}, urls = {})
public class NhentaiNetCrawler extends PluginForDecrypt {
    public NhentaiNetCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "nhentai.to", "nhentai.net" });
        /*
         * 2022-09-28: While nhentai.to and nhentai.xxx look pretty much the same they're different and not all content from nhentai.to is
         * available on nhentai.xxx!
         */
        ret.add(new String[] { "nhentai.xxx" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/g/(\\d+)/?$");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public void init() {
        super.init();
        Browser.setRequestIntervalLimitGlobal(getHost(), 1000);
    }

    public int getMaxConcurrentProcessingInstances() {
        /* 2020-06-25: Too many requests can lead to failures */
        return 1;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String parameter = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        final String galleryID = new Regex(parameter, this.getSupportedLinks()).getMatch(0);
        br.getPage(parameter);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        antiDDoSForHost.replaceCloudFlareEmailProtection(br);
        String title = null;
        try {
            String json = br.getRegex("JSON\\.parse\\(\"(\\{.*?)\"\\);").getMatch(0);
            if (json == null) {
                /* 2025-08-11, nhentai.xxx */
                json = br.getRegex("parseJSON\\((\"|')(\\{.*?)(\"|')\\);").getMatch(1);
            }
            json = PluginJSonUtils.unescape(json);
            final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
            final Map<String, Object> titles = (Map<String, Object>) entries.get("title");
            if (titles != null) {
                title = (String) titles.get("english");
                if (StringUtils.isEmpty(title)) {
                    title = (String) titles.get("japanese");
                }
            }
        } catch (final Throwable ignore) {
            logger.log(ignore);
        }
        if (title == null) {
            final String english = br.getRegex("(?:id|class)\\s*=\\s*\"info\"\\s*>\\s*<h1[^>]*>\\s*(.*?)\\s*</h1").getMatch(0);
            final String japanese = br.getRegex("(?:id|class)\\s*=\\s*\"info\"\\s*>.*?<h2[^>]*>\\s*(.*?)\\s*</h2").getMatch(0);
            title = StringUtils.firstNotEmpty(english, japanese);
            if (title != null) {
                title = title.replaceAll("<span[^>]*>", "").replaceAll("</span[^>]*>", "");
                title = Encoding.htmlDecode(title);
            }
        }
        if (StringUtils.isEmpty(title)) {
            /* Fallback */
            title = galleryID + " - nhentai gallery";
        } else {
            /**
             * 2021-02-08: Avoid merging of packages with the same name but different contents: Galleries can have the exact name but
             * different content!
             */
            title = galleryID + "_" + title;
        }
        final String[] urls = br.getRegex("(/g/" + galleryID + "/\\d+/?)").getColumn(0);
        if (urls == null || urls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final int estimatedNumberOfPages = urls.length;
        final DecimalFormat df = estimatedNumberOfPages > 999 ? new DecimalFormat("0000") : estimatedNumberOfPages > 99 ? new DecimalFormat("000") : new DecimalFormat("00");
        for (final String url : urls) {
            final int pageNumber = Integer.parseInt(new Regex(url, "(\\d+)/?$").getMatch(0));
            String extensionGuess = br.getRegex("/\\d+/(?:[^/]*/)?" + pageNumber + "t(\\.(?:png|jpe?g|webp|gif))").getMatch(0);
            if (extensionGuess == null) {
                extensionGuess = NhentaiNet.EXT_DEFAULT;
            }
            final DownloadLink dl = createDownloadlink(Request.getLocation(url, br.getRequest()));
            dl.setFinalFileName(df.format(pageNumber) + getFileNameExtensionFromString(url, extensionGuess));
            dl.setAvailable(true);
            ret.add(dl);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.addLinks(ret);
        // fp.setProperty(LinkCrawler.PACKAGE_ALLOW_MERGE, false);
        return ret;
    }

    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}