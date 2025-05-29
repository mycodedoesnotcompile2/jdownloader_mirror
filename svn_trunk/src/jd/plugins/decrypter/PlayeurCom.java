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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

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

@DecrypterPlugin(revision = "$Revision: 51093 $", interfaceVersion = 3, names = {}, urls = {})
public class PlayeurCom extends PluginForDecrypt {
    public PlayeurCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(402);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "playeur.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/v/([a-zA-Z0-9-]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String videoID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage("https://api.playeur.com/v1/videos/" + videoID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String object = entries.get("object").toString();
        if (!StringUtils.equalsIgnoreCase(object, "video")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String video_status = entries.get("video_status").toString();
        if (!StringUtils.equalsIgnoreCase(video_status, "done")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String previewImageURL = entries.get("preview_image_url").toString();
        final String coverImageURL = entries.get("cover_image_url").toString();
        final String title = entries.get("title").toString();
        final String description = (String) entries.get("description");
        final Map<String, Object> rental = (Map<String, Object>) entries.get("rental");
        final Map<String, Object> tier = (Map<String, Object>) entries.get("tier");
        final Map<String, Object> videomap = (Map<String, Object>) entries.get("videos");
        final Map<String, Object> audiomap = (Map<String, Object>) entries.get("audio");
        boolean paidContent = false;
        if (br.getHttpConnection().getResponseCode() == 402) {
            paidContent = true;
        }
        if (!paidContent) {
            /* Check for paid content and log it */
            if (rental != null) {
                final Number tierN = (Number) rental.get("tier");
                if (tierN != null && tierN.intValue() != 0) {
                    paidContent = true;
                }
            }
            if (!paidContent) {
                if (tier != null) {
                    final Number price_tier = (Number) rental.get("price_tier");
                    if (price_tier != null && price_tier.intValue() != 0) {
                        paidContent = true;
                    }
                }
            }
        }
        if (paidContent) {
            /* Do not throw exception since we should still be able to crawl the video thumbnail/cover */
            // throw new AccountRequiredException();
            logger.info("Looks like paid content -> Only preview images may be available");
        }
        final DownloadLink image = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(coverImageURL));
        ret.add(image);
        final DownloadLink image2 = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(previewImageURL));
        ret.add(image2);
        if (videomap != null) {
            final Iterator<Entry<String, Object>> iterator = videomap.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(entry.getValue().toString()));
                ret.add(video);
            }
        }
        if (audiomap != null) {
            final Iterator<Entry<String, Object>> iterator = audiomap.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                final DownloadLink audio = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(entry.getValue().toString()));
                ret.add(audio);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey(this.getHost() + "/video/" + videoID);
        for (final DownloadLink result : ret) {
            if (!StringUtils.isEmpty(description)) {
                result.setComment(description);
            }
            result.setAvailable(true);
            result._setFilePackage(fp);
        }
        if (ret.isEmpty()) {
            /* Even for paid videos, at least the video cover should be available */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
