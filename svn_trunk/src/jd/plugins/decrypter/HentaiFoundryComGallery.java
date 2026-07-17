//jDownloader - Downloadmanager
//Copyright (C) 2008  JD-Team support@jdownloader.org
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
import java.util.regex.Pattern;

import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.HentaiFoundryCom;

@DecrypterPlugin(revision = "$Revision: 52998 $", interfaceVersion = 3, names = {}, urls = {})
public class HentaiFoundryComGallery extends PluginForDecrypt {
    public HentaiFoundryComGallery(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "hentai-foundry.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_GALLERY = Pattern.compile("/pictures/user/[A-Za-z0-9\\-_]+(?:/scraps)?(?:/(\\d+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_PROFILE = Pattern.compile("/user/[A-Za-z0-9\\-_]+/(profile|faves/pictures)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_GALLERY.pattern().substring(1) + "|" + PATTERN_PROFILE.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.XXX };
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        getUserLogin(false);
        br.setFollowRedirects(true);
        String contenturl = param.getCryptedUrl();
        if (new Regex(contenturl, Pattern.compile(".+/user/[A-Za-z0-9\\-_]+/profile", Pattern.CASE_INSENSITIVE)).matches()) {
            final String userID = new Regex(contenturl, Pattern.compile(".+/user/([A-Za-z0-9\\-_]+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
            ret.add(createDownloadlink("https://www." + getHost() + "/pictures/user/" + userID));
            ret.add(createDownloadlink("https://www." + getHost() + "/pictures/user/" + userID + "/scraps"));
            ret.add(createDownloadlink("https://www." + getHost() + "/stories/user/" + userID));
            return ret;
        }
        if (new Regex(contenturl, Pattern.compile(".+/pictures/user/[A-Za-z0-9\\-_]+/\\d+", Pattern.CASE_INSENSITIVE)).matches()) {
            ret.add(createDownloadlink(contenturl));
            return ret;
        }
        br.getPage(contenturl + "?enterAgree=1&size=0");
        if (br.getHttpConnection().getResponseCode() == 404) {
            ret.add(this.createOfflinelink(contenturl));
            return ret;
        } else if (br.containsHTML("class=\"empty\"")) {
            /* User has not uploaded any content */
            ret.add(this.createOfflinelink(contenturl));
            return ret;
        }
        final String fpName = new Regex(contenturl, "/user/(.+)").getMatch(0);
        int page = 1;
        String next = null;
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(fpName.trim()));
        do {
            if (this.isAbort()) {
                logger.info("Decryption aborted by user: " + contenturl);
                return ret;
            }
            logger.info("Decrypting page " + page);
            if (page > 1) {
                br.getPage(next);
            }
            String[] links = br.getRegex("<[^<>]+class='thumb_square'.*?</").getColumn(-1);
            if (links == null || links.length == 0) {
                return null;
            }
            for (String link : links) {
                String title = new Regex(link, "thumbTitle\"><[^<>]*?>([^<>]+)<").getMatch(0);
                final String url = new Regex(link, "\"(/pictures/user/[A-Za-z0-9\\-_]+/\\d+[^<>\"]*?)\"").getMatch(0);
                if (url == null) {
                    logger.warning("Decrypter broken for link: " + contenturl);
                    logger.info("link: " + link);
                    logger.info("title: " + title + "url: " + url);
                    return null;
                }
                final String pic_id = jd.plugins.hoster.HentaiFoundryCom.getFID(url);
                if (title != null) {
                    title = pic_id + "_" + Encoding.htmlDecode(title).trim();
                } else {
                    title = pic_id;
                }
                final DownloadLink dl = createDownloadlink(Request.getLocation(url, br.getRequest()));
                dl.setName(title);
                dl.setMimeHint(CompiledFiletypeFilter.ImageExtensions.BMP);
                dl.setAvailable(true);
                ret.add(dl);
                fp.add(dl);
                distribute(dl);
            }
            next = br.getRegex("class=\"next\"><a href=\"(/pictures/user/.*?/page/\\d+)\">Next").getMatch(0);
            if (next == null) {
                next = br.getRegex("class=\"next\"><a href=\"(/user/.*?/page/\\d+)\">Next").getMatch(0);
            }
            page++;
        } while (next != null);
        return ret;
    }

    /** Log in the account of the hostplugin */
    @SuppressWarnings("deprecation")
    private boolean getUserLogin(final boolean force) throws Exception {
        final HentaiFoundryCom hostPlugin = (HentaiFoundryCom) this.getNewPluginForHostInstance(getHost());
        final Account aa = AccountController.getInstance().getValidAccount(hostPlugin);
        if (aa == null) {
            logger.warning("There is no account available, continuing without logging in (if possible)");
            return false;
        }
        try {
            hostPlugin.login(aa, force);
        } catch (final PluginException e) {
            logger.warning("Login failed - continuing without login");
            aa.setValid(false);
            return false;
        }
        logger.info("Logged in successfully");
        return true;
    }
}
