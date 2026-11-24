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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.DirectHTTP;
import jd.utils.JDUtilities;

@DecrypterPlugin(revision = "$Revision: 54102 $", interfaceVersion = 3, names = {}, urls = {})
public class HighpornNetCrawler extends PluginForDecrypt {
    public HighpornNetCrawler(PluginWrapper wrapper) {
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
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "highporn.net" });
        /**
         * 2026-11-23: tanix.net mostly redirects to javhub.net <br>
         * tanix.net: /id/... --> Such links redirect to javhub.net <br>
         * TODO: possibly remove tanix.net domain from this plugin, it's unnecessary.
         */
        ret.add(new String[] { "tanix.net" });
        ret.add(new String[] { "japanhub.net" });
        /**
         * 2026-11-23: thatav.net website is very slow <br>
         * Looks like most of their video urls point to a broken/offline ad website: ja.tikpornk.com
         */
        ret.add(new String[] { "thatav.net" });
        ret.add(new String[] { "javhub.net" });
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
            if (domains[0].equals("javhub.net")) {
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/play/([A-Za-z0-9_\\-]+)(?:/[a-z0-9\\-]+)?");
            } else {
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/video/(\\d+)(?:/[a-z0-9\\-]+)?");
            }
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl().replaceFirst("(?i)www\\.", "");
        final String videoid = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        getPage(contenturl);
        if (!this.canHandle(br.getURL())) {
            logger.info("Redirect to external website: " + br.getURL());
            ret.add(this.createDownloadlink(br.getURL()));
            return ret;
        } else if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(videoid)) {
            /* Offline --> Redirect to (external) ads page / search-page */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (StringUtils.equalsIgnoreCase(Browser.getHost(contenturl), "javhub.net")) {
            return crawlJavhub(contenturl);
        }
        final String title = getTitle(this.br, contenturl);
        /* 2021-02-10: thatav.net */
        final String specialVideoURL = br.getRegex("\"file\":\\s*\"(https?://[^<>\"]+\\.mp4[^<>\"]+)").getMatch(0);
        if (specialVideoURL != null) {
            final DownloadLink dl = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(specialVideoURL));
            dl.setAvailable(true);
            if (title != null) {
                dl.setFinalFileName(title + ".mp4");
            }
            ret.add(dl);
            return ret;
        }
        boolean singleVideo = false;
        final String videoLink = br.getRegex("data-src\\s*=\\s*\"(https?[^<>\"]+)\"").getMatch(0); // If single link, no videoID
        String[] videoIDs = br.getRegex("data-src\\s*=\\s*\"([^\"]+)\"").getColumn(0);
        if (videoIDs == null || videoIDs.length == 0) {
            if (videoLink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            videoIDs = new String[1];
            videoIDs[0] = (Long.toString(System.currentTimeMillis())); // dummy videoID
            singleVideo = true;
        }
        final int padLength = StringUtils.getPadLength(videoIDs.length);
        int counter = 0;
        for (final String videoID : videoIDs) {
            if (isAbort()) {
                continue;
            }
            counter++;
            final String orderid_formatted = String.format(Locale.ROOT, "%0" + padLength + "d", counter);
            final String filename = title + "_" + orderid_formatted + ".mp4";
            final DownloadLink dl = createDownloadlink("highporndecrypted://" + videoID);
            dl.setFinalFileName(filename);
            /* Save final filename as property so the hoster plugin can set it again later. */
            dl.setProperty(jd.plugins.hoster.HighpornNet.PROPERTY_DECRYPTERNAME, filename);
            dl.setProperty(jd.plugins.hoster.HighpornNet.PROPERTY_MAINLINK, contenturl);
            dl.setContentUrl(contenturl);
            if (singleVideo) {
                dl.setProperty(jd.plugins.hoster.HighpornNet.PROPERTY_SINGLEVIDEO, true);
            } else {
                final String file = jd.plugins.hoster.HighpornNet.requestPlayUrl(br.getURL(), br, videoID);
                URLConnectionAdapter con = null;
                try {
                    con = br.cloneBrowser().openHeadConnection(file);
                    // referer check
                    if (this.looksLikeDownloadableContent(con)) {
                        dl.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                } catch (final IOException e) {
                    logger.log(e);
                } finally {
                    try {
                        con.disconnect();
                    } catch (final Throwable ignore) {
                    }
                }
                dl.setAvailable(true);
            }
            ret.add(dl);
        }
        if (title != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(Encoding.htmlDecode(title).trim());
            fp.addLinks(ret);
        }
        return ret;
    }

    /**
     * javhub.net: Every play button (e.g. different qualities) has an encoded "data-src" attribute. The hoster plugin turns it into the
     * final URL via /playapi.
     */
    private ArrayList<DownloadLink> crawlJavhub(final String contenturl) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String title = br.getRegex("<title>\\s*Watch Free Jav\\s+(.*?)\\s+from Javhub").getMatch(0);
        if (title == null) {
            /* Fallback: Slug of the URL */
            title = new Regex(contenturl, "/play/[^/]+/([^/\\?#]+)").getMatch(0);
        }
        if (title == null) {
            title = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        }
        title = Encoding.htmlDecode(title).trim();
        final String[][] buttons = br.getRegex("<button[^>]*class=\"[^\"]*playbtn[^\"]*\"[^>]*data-src=\"([^\"]+)\"[^>]*>(.*?)</button>").getMatches();
        if (buttons == null || buttons.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        for (final String[] button : buttons) {
            final String videoID = button[0];
            final String label = Encoding.htmlDecode(button[1].replaceAll("<[^>]+>", " ")).replaceAll("\\s+", " ").trim();
            final String filename = title + (label.length() > 0 ? "_" + label : "") + ".mp4";
            final DownloadLink dl = createDownloadlink("highporndecrypted://" + videoID);
            dl.setFinalFileName(filename);
            dl.setProperty(jd.plugins.hoster.HighpornNet.PROPERTY_DECRYPTERNAME, filename);
            dl.setProperty(jd.plugins.hoster.HighpornNet.PROPERTY_MAINLINK, contenturl);
            dl.setProperty(jd.plugins.hoster.HighpornNet.PROPERTY_PLAYAPI, true);
            dl.setContentUrl(contenturl);
            dl.setAvailable(true);
            ret.add(dl);
        }
        fp.addLinks(ret);
        return ret;
    }

    private void getPage(final String parameter) throws Exception {
        final PluginForHost plugin = JDUtilities.getPluginForHost("highporn.net");
        plugin.setBrowser(br);
        ((jd.plugins.hoster.HighpornNet) plugin).getPage(parameter);
    }

    public static boolean isOffline(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 404 || br.getURL().contains("/error/video_missing");
    }

    public static String getTitle(final Browser br, final String url) {
        String title = br.getRegex("property\\s*=\\s*\"og:title\" content=\"([^<>\"]+)\"").getMatch(0);
        if (title == null) {
            title = new Regex(url, "video/(.+)").getMatch(0);
        }
        return title;
    }
}
