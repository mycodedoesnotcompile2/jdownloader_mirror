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

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

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

@DecrypterPlugin(revision = "$Revision: 50259 $", interfaceVersion = 2, names = {}, urls = {})
public class Viet69Net extends PluginForDecrypt {
    public Viet69Net(PluginWrapper wrapper) {
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
        ret.add(new String[] { "viet69.moi", "viet69.gg", "viet69.in", "viet69.net", "viet69.co", "viet69.love", "viet69.page", "viet69.vc", "viet69.tube", "viet69.zip", "viet69.ec" });
        return ret;
    }

    private ArrayList<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("viet69.gg");
        deadDomains.add("viet69.co");
        deadDomains.add("viet69.love");
        deadDomains.add("viet69.vc");
        return deadDomains;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([a-z0-9\\-]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>() {
            @Override
            public boolean add(DownloadLink link) {
                distribute(link);
                return super.add(link);
            }
        };
        String contenturl = param.getCryptedUrl();
        for (final String deadDomain : getDeadDomains()) {
            contenturl = contenturl.replace(deadDomain, this.getHost());
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String title = new Regex(contenturl, this.getSupportedLinks()).getMatch(0).replace("-", " ").trim();
        final String[] videoIDs = br.getRegex("data-video=\"([^\"]+)").getColumn(0);
        if (videoIDs == null || videoIDs.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String[] videoDetails = br.getRegex("<div\\s+class\\s*=\\s*\"movieLoader\"\\s+data-movie\\s*=\\s*\"([^\"\\s]+)\"\\s+data-type\\s*=\\s*\"([^\"]*)\"").getRow(0);
        if (videoDetails == null || videoDetails.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String videoType = videoDetails[1];
        int index = 0;
        for (final String videoID : videoIDs) {
            logger.info("Crawling mirror " + (index + 1) + "/" + videoIDs.length + " -> " + videoID);
            final Browser br2 = br.cloneBrowser();
            br2.postPage("/get.video.php", "movie_id=" + videoID + "&type=" + videoType + "&index=1");
            String[] links = br2.getRegex("\"file\"\\s*:\\s*\"([^\"]+)\"").getColumn(0);
            if (links == null || links.length == 0) {
                links = br2.getRegex("file\\s*:\\s*\'([^']+)'").getColumn(0);
            }
            if (links != null && links.length > 0) {
                for (String link : links) {
                    if (StringUtils.containsIgnoreCase(link, ".m3u8")) {
                        final Browser brc = br2.cloneBrowser();
                        brc.getPage(link);
                        final ArrayList<DownloadLink> downloadLinks = GenericM3u8Decrypter.parseM3U8(this, link, brc, contenturl, null, title);
                        if (downloadLinks != null) {
                            ret.addAll(downloadLinks);
                        }
                    } else {
                        final DownloadLink downloadLink = createDownloadlink(Encoding.htmlDecode(link).trim());
                        if (title != null) {
                            downloadLink.setName(title);
                        }
                        ret.add(downloadLink);
                    }
                }
            } else {
                /* Look for embedded content */
                final String[] embedURLs = br2.getRegex("<iframe[^>]*src=\"(https?://[^\"]+)").getColumn(0);
                if (embedURLs != null && embedURLs.length > 0) {
                    for (final String embedURL : embedURLs) {
                        ret.add(this.createDownloadlink(embedURL));
                    }
                }
            }
            index++;
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        }
        fp.addLinks(ret);
        return ret;
    }
}