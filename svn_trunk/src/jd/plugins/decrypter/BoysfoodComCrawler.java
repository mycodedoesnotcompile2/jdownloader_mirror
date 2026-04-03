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

import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.hoster.BoysfoodCom;

@DecrypterPlugin(revision = "$Revision: 52606 $", interfaceVersion = 3, names = {}, urls = {})
public class BoysfoodComCrawler extends PornEmbedParser {
    public BoysfoodComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "boysfood.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:free-porn-videos|videos)/\\d+/[a-z0-9\\-]+");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        br.getPage(param.getCryptedUrl());
        if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        ret.addAll(findEmbedUrls());
        if (!ret.isEmpty()) {
            return ret;
        }
        String extern_url = br.getRegex("<iframe width[^>]*src=\"(http[^\"]+)").getMatch(0);
        if (extern_url != null) {
            logger.info("externID: " + extern_url);
            ret.add(createDownloadlink(extern_url));
            return ret;
        }
        /* Looks like selfhosted content */
        ret.add(createDownloadlink(param.getCryptedUrl()));
        return ret;
    }

    @Override
    protected boolean isOffline(final Browser br) {
        return isOfflineStatic(br);
    }

    public static boolean isOfflineStatic(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 404) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getFileTitle(final CryptedLink param, final Browser br) {
        return BoysfoodCom.getURLTitleCleaned(param.getCryptedUrl());
    }

    @Override
    protected boolean assumeSelfhostedContentOnNoResults() {
        return true;
    }

    @Override
    protected boolean allowResult(final String url) {
        final String embedregex = "https?://(?:www\\.)?" + buildHostsPatternPart(getPluginDomains().get(0)) + "/embed/\\d+";
        if (url.matches(embedregex)) {
            /* Do not allow self-embedded URLs. */
            return false;
        } else {
            return super.allowResult(url);
        }
    }
}
