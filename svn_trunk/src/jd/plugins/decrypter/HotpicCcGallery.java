//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.HotpicCc;

@DecrypterPlugin(revision = "$Revision: 53114 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { HotpicCc.class })
public class HotpicCcGallery extends PluginForDecrypt {
    public HotpicCcGallery(final PluginWrapper wrapper) {
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
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST, LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    private static List<String[]> getPluginDomains() {
        return HotpicCc.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/album/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String albumID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("File Not Found\\s*</h1>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (StringUtils.equalsIgnoreCase(br._getURL().getPath(), "/404/")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Album title -> package name */
        String albumTitle = br.getRegex("property=('|\")og:title\\1\\s*content=('|\")([^\"']+)\\2").getMatch(2);
        if (StringUtils.isEmpty(albumTitle)) {
            albumTitle = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        }
        if (albumTitle != null) {
            albumTitle = Encoding.htmlDecode(albumTitle).trim();
            /* Remove sitename suffix e.g. " - HotPic.CC" */
            albumTitle = albumTitle.replaceFirst("(?i)\\s*[-|]\\s*HotPic\\.[A-Za-z]+\\s*$", "").trim();
        }
        if (StringUtils.isEmpty(albumTitle)) {
            albumTitle = albumID;
        }
        /* Find single image/media links + their titles */
        final String[][] items = br.getRegex("<a[^>]*href=\"(https?://[^\"]+/i/[A-Za-z0-9]+)\"[^>]*title=\"([^\"]*)\"").getMatches();
        if (items == null || items.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<String> dupe = new ArrayList<String>();
        for (final String[] item : items) {
            final String url = item[0];
            if (dupe.contains(url)) {
                continue;
            }
            dupe.add(url);
            final DownloadLink image = createDownloadlink(url);
            final String title = item[1];
            if (!StringUtils.isEmpty(title)) {
                image.setName(Encoding.htmlDecode(title).replace("-", " ").trim());
            }
            image.setAvailable(true);
            ret.add(image);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(albumTitle);
        fp.setPackageKey("hotpic://album/" + albumID);
        fp.addLinks(ret);
        return ret;
    }
}
