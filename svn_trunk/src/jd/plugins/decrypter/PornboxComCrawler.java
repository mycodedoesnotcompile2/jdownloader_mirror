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

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.PornboxCom;

@DecrypterPlugin(revision = "$Revision: 51438 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { PornboxCom.class })
public class PornboxComCrawler extends PluginForDecrypt {
    public PornboxComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return PornboxCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/application/watch-page/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String content_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        boolean userIsPremium = false;
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        final PornboxCom hosterplugin = (PornboxCom) this.getNewPluginForHostInstance(this.getHost());
        /* Login whenever an account is available. */
        if (account != null) {
            /* User has account -> Login and check which account type it is. */
            hosterplugin.login(account, false);
            userIsPremium = AccountType.PREMIUM.equals(account.getType());
        }
        br.getPage("https://" + getHost() + "/contents/" + content_id);
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* 404 response with plaintext content "Content not found" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        // final Map<String, Object> price = (Map<String, Object>) entries.get("price");
        final Map<String, Object> thumbnail = (Map<String, Object>) entries.get("thumbnail");
        final Map<String, Object> image_gallery_zip = (Map<String, Object>) entries.get("image_gallery_zip");
        final Map<String, Object> image_screenshot_zip = (Map<String, Object>) entries.get("image_screenshot_zip");
        final List<Map<String, Object>> medias = (List<Map<String, Object>>) entries.get("medias");
        // final Object subtitles = entries.get("subtitles");
        String title = entries.get("scene_name").toString();
        /* Add thumbnail (best version only) */
        final DownloadLink thumb = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(thumbnail.get("large").toString()));
        thumb.setFinalFileName(title + "_thumbnail.jpg");
        ret.add(thumb);
        final DownloadLink player_poster = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(entries.get("player_poster").toString()));
        player_poster.setFinalFileName(title + "_player_poster.jpg");
        ret.add(player_poster);
        /* Add video preview */
        final DownloadLink preview = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(entries.get("video_preview").toString()));
        preview.setFinalFileName(title + "_preview.mp4");
        ret.add(preview);
        if (Boolean.TRUE.equals(entries.get("is_purchased"))) {
        }
        /* Add image gallery zip; mostly only available for premium users. */
        if (image_gallery_zip != null) {
            final DownloadLink image_gallery = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(image_gallery_zip.get("url").toString()));
            image_gallery.setFinalFileName(title + "_image_gallery.zip");
            image_gallery.setVerifiedFileSize(((Number) image_gallery_zip.get("size")).longValue());
            ret.add(image_gallery);
        }
        /* Add image screenshot zip; mostly only available for premium users. */
        if (image_screenshot_zip != null) {
            final DownloadLink image_screenshot = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(image_screenshot_zip.get("url").toString()));
            image_screenshot.setFinalFileName(title + "_image_screenshot.zip");
            image_screenshot.setVerifiedFileSize(((Number) image_screenshot_zip.get("size")).longValue());
            ret.add(image_screenshot);
        }
        /* Add main video in all resolutions */
        for (final Map<String, Object> media : medias) {
            final String type = media.get("type").toString();
            if (userIsPremium && type.equalsIgnoreCase("free")) {
                /* Skip free content like previews and trailers if user owns a premium account */
                logger.info("Skipping media_id free content as user is premium " + media.get("media_id"));
                continue;
            }
            final List<Map<String, Object>> transcodings = (List<Map<String, Object>>) media.get("transcodings");
            if (transcodings == null) {
                /* Most likely a premium only item. It will have list-field "offers" available instead of "transcodings". */
                logger.info("Skipping media_id as it has no transcodings " + media.get("media_id"));
                continue;
            }
            for (final Map<String, Object> transcoding : transcodings) {
                final String video_label = transcoding.get("video_mode").toString();
                final DownloadLink video = this.createDownloadlink("");
                /*
                 * No browser-openable URL available -> Set added URL as contenturl -> This is what the user gets when URL is copied in GUI.
                 */
                video.setContentUrl(contenturl);
                video.setProperty(PornboxCom.PROPERTY_CONTENT_ID, transcoding.get("file_id"));
                video.setDefaultPlugin(hosterplugin);
                video.setHost(this.getHost());
                video.setFinalFileName(title + "_" + video_label + "_bitrate_" + transcoding.get("bitrate") + ".mp4");
                video.setVerifiedFileSize(((Number) transcoding.get("filesize")).longValue());
                ret.add(video);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey(this.getHost() + "/video/" + content_id);
        for (final DownloadLink result : ret) {
            result.setAvailable(true);
            result._setFilePackage(fp);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        /* 2025-09-03: This website does not have any captchas. */
        return false;
    }
}
