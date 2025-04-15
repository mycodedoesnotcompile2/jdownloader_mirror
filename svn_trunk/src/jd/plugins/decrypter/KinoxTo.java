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

import static java.lang.String.format;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.CrawledLink;
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

@DecrypterPlugin(revision = "$Revision: 50967 $", interfaceVersion = 3, names = {}, urls = {})
public class KinoxTo extends PluginForDecrypt {
    public KinoxTo(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final Pattern TYPE_SERIES       = Pattern.compile("/Stream/[A-Za-z0-9\\-_]+\\.html", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_VIDEO = Pattern.compile("/(\\d+)-([\\w-]+)\\.html", Pattern.CASE_INSENSITIVE);

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        final String[] names = new String[] { "kinox", "kinoy", "kinoz", "kinos" };
        final String[] tlds = new String[] { "ai", "af", "am", "click", "cloud", "club", "digital", "direct", "express", "fun", "fyi", "gratis", "gs", "gy", "io", "li", "lol", "me", "mobi", "ms", "nu", "pe", "party", "pub", "sg", "si", "space", "sx", "to", "tube", "tv", "wtf", "tel" };
        final String[] stringarray = new String[names.length * tlds.length];
        int index = 0;
        for (final String name : names) {
            for (final String tld : tlds) {
                stringarray[index] = name + "." + tld;
                index++;
            }
        }
        ret.add(stringarray);
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
            ret.add("https?://(?:[w0-9]*\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_SERIES.pattern() + "|" + TYPE_SINGLE_VIDEO.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    private String addr_id;

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Regex singlevideoregex = new Regex(br._getURL().getPath(), TYPE_SINGLE_VIDEO);
        if (singlevideoregex.patternFind()) {
            final String video_id = singlevideoregex.getMatch(0);
            final String url_slug = singlevideoregex.getMatch(1);
            String title = url_slug.replace("-", " ").trim();
            /* Fix title */
            title = title.replace("kostenlos auf deutsch", "").trim();
            final String[] urls = br.getRegex("data-link=\"([^\"]+)").getColumn(0);
            if (urls == null || urls.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            fp.setPackageKey("kinox://movie/" + video_id);
            for (String url : urls) {
                url = br.getURL(url).toExternalForm();
                if (StringUtils.containsIgnoreCase(url, "youtube.com/")) {
                    /* Do not include trailer */
                    continue;
                }
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
            }
        } else {
            addr_id = br.getRegex("Addr=([^<>\"\\&]*?)&").getMatch(0);
            final String url_name = new Regex(contenturl, "kino\\w\\.\\w+/Stream/([A-Za-z0-9\\-_]+)\\.html").getMatch(0);
            final String series_id = br.getRegex("SeriesID=(\\d+)").getMatch(0);
            String fpName = br.getRegex("<h1><span style=\"display: inline-block\">([^<>\"]*?)</span>").getMatch(0);
            fpName = Encoding.htmlDecode(fpName.trim());
            if (fpName == null) {
                fpName = url_name;
            }
            if (addr_id == null) {
                addr_id = url_name;
            }
            Browser br2 = br.cloneBrowser();
            br2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            if (br.containsHTML("id=\"SeasonSelection\"")) {
                if (series_id == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* Crawl all Seasons | Episodes | Mirrors of a Series */
                final String[][] season_info_all = br2.getRegex("value=\"\\d+\" rel=\"([0-9,]+)\"[^>]*?>Staffel (\\d+)</option>").getMatches();
                int firstSeason = 0;
                int firstEpisode = 0;
                if (param.getSource() instanceof CrawledLink) {
                    CrawledLink crawledLink = (CrawledLink) param.getSource();
                    // TODO: Please display dialog here!
                    // firstSeason = crawledLink.getFirstSeason();
                    // firstEpisode = crawledLink.getFirstEpisode();
                }
                for (final String[] season : season_info_all) {
                    final String season_number = season[1];
                    int season_number_int = Integer.parseInt(season_number);
                    if (season_number_int < firstSeason) {
                        continue;
                    }
                    final String[] season_episodes = season[0].split(",");
                    for (final String episode : season_episodes) {
                        if (this.isAbort()) {
                            getLogger().info("Decryption aborted by user");
                            return ret;
                        }
                        if ((season_number_int == firstSeason) && (Integer.parseInt(episode) < firstEpisode)) {
                            continue;
                        }
                        /* Crawl Season --> Find episodes */
                        br2 = br.cloneBrowser();
                        br2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                        br2.getPage("/aGET/MirrorByEpisode/?Addr=" + addr_id + "&SeriesID=" + series_id + "&Season=" + season_number + "&Episode=" + episode);
                        /* Crawl Episode --> Find mirrors */
                        crawlMirrors(param, ret, br2, fpName, season_number, episode);
                    }
                }
            } else {
                /* Crawl all Mirrors of a movie */
                crawlMirrors(param, ret, br2, fpName, null, null);
            }
        }
        return ret;
    }

    private void crawlMirrors(final CryptedLink param, final List<DownloadLink> ret, Browser br2, String fpName, final String season_number, final String episode) throws Exception {
        final String[] mirrors = br2.getRegex("(<li id=\"Hoster_\\d+\".*?</div>\\s*</li>)").getColumn(0);
        if (mirrors == null || mirrors.length == 0) {
            getLogger().info("No mirrors found.");
            // throw new DecrypterException("Decrypter broken"); // Link doesn't always exist
            return;
        }
        final DecimalFormat df = new DecimalFormat("00");
        final FilePackage fp = FilePackage.getInstance();
        if (season_number != null && episode != null) {
            final int season_int = Integer.parseInt(season_number);
            final String season_formatted = "S" + df.format(season_int);
            fpName += " " + season_formatted;
        }
        fp.setName(fpName);
        for (final String mirror : mirrors) {
            /* Crawl Mirrors --> Find directlinks */
            if (this.isAbort()) {
                getLogger().info("Decryption aborted by user");
                return;
            }
            final String hoster_id = new Regex(mirror, "Hoster_(\\d+)").getMatch(0);
            String mirror_id = new Regex(mirror, "Mirror=(\\d+)").getMatch(0);
            if (mirror_id == null) {
                if (new Regex(mirror, "><b>Mirror</b>:[^<>]+1/1<br\\s*/\\s*>").matches()) {
                    /* Only 1 mirror available */
                    mirror_id = "1";
                } else {
                    // regex pattern needs updating..
                    System.out.println("errrrrrrrrrrror");
                    getLogger().warning("Mirror regex pattern needs updating.");
                }
            }
            if (hoster_id == null || mirror_id == null) {
                return;
            }
            String geturl = "/aGET/Mirror/" + addr_id + "&Hoster=" + hoster_id + "&Mirror=" + mirror_id;
            if (season_number != null && episode != null) {
                geturl += "&Season=" + season_number + "&Episode=" + episode;
            }
            final Browser br3 = br.cloneBrowser();
            br3.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            try {
                br3.getPage(geturl);
            } catch (IOException e) {
                sleep(2000, param);
                br3.getPage(geturl);
            }
            String finallink = PluginJSonUtils.getJson(br3, "Stream");
            if (finallink == null) {
                getLogger().info("Unable to determine a link for mirror.");
                return;
            }
            finallink = new Regex(finallink, "(?:href|src)\\s*=\\s*('|\"|)(.*?)\\1").getMatch(1);
            final DownloadLink dl = createDownloadlink(Request.getLocation(finallink, br.getRequest()));
            if (season_number != null && episode != null) {
                dl.setProperty("fallback_filename", format("%sE%02d.mp4", fpName, Integer.parseInt(episode)));
            } else {
                dl.setProperty("fallback_filename", fpName + ".mp4");
            }
            fp.add(dl);
            ret.add(dl);
            distribute(dl);
        }
    }
}
