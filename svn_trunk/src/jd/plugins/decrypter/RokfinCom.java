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
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52998 $", interfaceVersion = 3, names = {}, urls = {})
public class RokfinCom extends PluginForDecrypt {
    public RokfinCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "rokfin.com" });
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

    private static final Pattern PATTERN_POST   = Pattern.compile("/post/(\\d+)");
    private static final Pattern PATTERN_STREAM = Pattern.compile("/stream/(\\d+)");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_POST.pattern().substring(1) + "|" + PATTERN_STREAM.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        br.getPage(param.getCryptedUrl());
        br.followRedirect();
        final String postID = new Regex(param.getCryptedUrl(), PATTERN_POST).getMatch(0);
        final String streamID = new Regex(param.getCryptedUrl(), PATTERN_STREAM).getMatch(0);
        Browser brc = br.cloneBrowser();
        final Map<String, Object> map;
        if (postID != null) {
            // free posts with without authentication
            map = restoreFromString(brc.getPage("https://prod-api-v2.production.rokfin.com/api/v2/public/post/" + postID), TypeRef.MAP);
            final String type = (String) JavaScriptEngineFactory.walkJson(map, "content/contentType");
            if (!"video".equalsIgnoreCase(type) && !"audio".equalsIgnoreCase(type)) {
                logger.info("Unsupported type:" + type);
                return new ArrayList<DownloadLink>();
            }
            final String title = (String) JavaScriptEngineFactory.walkJson(map, "content/contentTitle");
            if (StringUtils.isEmpty(title)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String premiumPlan = StringUtils.valueOfOrNull(JavaScriptEngineFactory.walkJson(map, "premiumPlan"));
            if ("true".equalsIgnoreCase(premiumPlan) || "1".equalsIgnoreCase(premiumPlan)) {
                throw new AccountRequiredException(title);
            }
            final String videourl = (String) JavaScriptEngineFactory.walkJson(map, "content/contentUrl");
            if (StringUtils.isEmpty(videourl) || StringUtils.equalsIgnoreCase("fake.m3u8", videourl)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (StringUtils.containsIgnoreCase(videourl, ".mp4")) {
                final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
                final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(videourl));
                video.setFinalFileName(title + ".mp4");
                video.setAvailable(true);
                ret.add(video);
                return ret;
            } else {
                brc = br.cloneBrowser();
                brc.getPage(videourl);
                final ArrayList<DownloadLink> ret = GenericM3u8Decrypter.parseM3U8(this, videourl, brc, null, null, title);
                if (ret.size() > 1) {
                    final FilePackage fp = FilePackage.getInstance();
                    fp.setName(title);
                    fp.addLinks(ret);
                }
                return ret;
            }
        } else {
            // may require authentication
            map = restoreFromString(brc.getPage("https://prod-api-v2.production.rokfin.com/api/v2/public/stream/" + streamID), TypeRef.MAP);
            final String title = (String) JavaScriptEngineFactory.walkJson(map, "title");
            if (StringUtils.isEmpty(title)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String premium = StringUtils.valueOfOrNull(JavaScriptEngineFactory.walkJson(map, "premium"));
            if ("true".equalsIgnoreCase(premium) || "1".equalsIgnoreCase(premium)) {
                throw new AccountRequiredException(title);
            }
            final String m3u8 = (String) JavaScriptEngineFactory.walkJson(map, "vodUrl");
            if (StringUtils.isEmpty(m3u8)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            brc = br.cloneBrowser();
            brc.getPage(m3u8);
            final ArrayList<DownloadLink> ret = GenericM3u8Decrypter.parseM3U8(this, m3u8, brc, null, null, title);
            if (ret.size() > 1) {
                FilePackage fp = FilePackage.getInstance();
                fp.setName(title);
                fp.addLinks(ret);
            }
            return ret;
        }
    }
}
