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

import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 54089 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { WCOForever.class })
public class WCOStream extends PluginForDecrypt {
    public WCOStream(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>(WCOForever.getPluginDomains());
        /* WCOStream itself, including its embed player (embed.wcostream.com) */
        ret.add(new String[] { WCOSTREAM_DOMAIN });
        return ret;
    }

    private static final String  WCOSTREAM_DOMAIN = "wcostream.com";
    private static final Pattern PATTERN_EMBED    = Pattern.compile("https?://embed\\." + Pattern.quote(WCOSTREAM_DOMAIN) + "/.+", Pattern.CASE_INSENSITIVE);

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

    @Override
    public Browser createNewBrowserInstance() {
        final Browser ret = super.createNewBrowserInstance();
        ret.setFollowRedirects(true);
        ret.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, Request.getSuggestedUserAgent("153.0"));
        return ret;
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            if (WCOSTREAM_DOMAIN.equals(domains[0])) {
                ret.add("https?://(?:www[0-9]*\\.|embed\\.)?" + buildHostsPatternPart(domains) + "/.+");
            } else {
                ret.add("https?://(?:www[0-9]*\\.)?" + buildHostsPatternPart(domains) + "/(?:anime/)?.+");
            }
        }
        return ret.toArray(new String[0]);
    }

    /**
     * Crawls the WCOStream embed player (embed.wcostream.com/inc/embed/index.php?file=...). The player page loads getvidlink.php which
     * returns the video IDs (enc = SD, hd = HD, fhd = FHD) and the server. Each ID is then downloadable via server/getvid?evid=ID.
     */
    private ArrayList<DownloadLink> crawlEmbedPage(final CryptedLink param) throws Exception {
        final String contenturl = param.getCryptedUrl();
        /* Embed player is meant to be loaded from within an iframe of WCOForever */
        br.getHeaders().put("Referer", "https://www." + WCOForever.getPluginDomains().get(0)[1] + "/");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /*
         * index.php only shows an announcement and an ad-block check (pre-init.js): It sends a beacon with a random nonce to /ad-verify and
         * then redirects to video-js.php with the same query plus 'n=NONCE'. Do the same.
         */
        final String query = new Regex(contenturl, "\\?(.+)$").getMatch(0);
        final String pid = new Regex(contenturl, "[?&]pid=(\\d+)").getMatch(0);
        final String nonce = UUID.randomUUID().toString().replace("-", "");
        final Browser brBeacon = br.cloneBrowser();
        brBeacon.getHeaders().put("Content-Type", "application/json");
        brBeacon.postPageRaw("/ad-verify", "{\"nonce\":\"" + nonce + "\",\"status\":\"clear\",\"id\":\"" + pid + "\"}");
        br.getPage("/inc/embed/video-js.php?" + query + "&n=" + nonce);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String vidlinkPath = br.getRegex("getJSON\\(\\s*\"([^\"]*getvidlink\\.php[^\"]*)\"").getMatch(0);
        if (vidlinkPath == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Browser brJson = br.cloneBrowser();
        brJson.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        brJson.getPage(vidlinkPath);
        final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(brJson.getRequest().getHtmlCode());
        final String server = (String) entries.get("server");
        final String enc = (String) entries.get("enc");
        /* HD and FHD are optional: Not every video has them. */
        final String hd = (String) entries.get("hd");
        final String fhd = (String) entries.get("fhd");
        /* SD: The server redirects to the final file host. Resolve it so we can check if the servers are at capacity. */
        final Browser brResolve = br.cloneBrowser();
        brResolve.getPage(server + "/getvid?evid=" + enc + "&json");
        String sdUrl = brResolve.getRegex("\"(https?:[^\"]+)\"").getMatch(0);
        if (sdUrl != null) {
            sdUrl = sdUrl.replace("\\/", "/");
            if (!hasSubdomain(new URL(sdUrl).getHost())) {
                /* Website shows "all servers are full" in this case. */
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "All servers are at capacity", TimeUnit.MINUTES.toMillis(10));
            }
        } else {
            sdUrl = server + "/getvid?evid=" + enc;
        }
        /* Build filename from the 'file' parameter e.g. 'benim-yeni/[Group] Title - S02E09 [English Dub].flv' */
        String name = new Regex(contenturl, "[?&]file=([^&]+)").getMatch(0);
        if (name != null) {
            name = Encoding.htmlDecode(Encoding.urlDecode(name, false));
            name = name.replaceFirst("^.*/", "").replaceFirst("\\.[A-Za-z0-9]{2,4}$", "").trim();
        }
        if (StringUtils.isEmpty(name)) {
            name = "WCOStream_" + new Regex(contenturl, "[?&]pid=(\\d+)").getMatch(0);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (StringUtils.isNotEmpty(fhd)) {
            ret.add(createEmbedQualityLink(server + "/getvid?evid=" + fhd, name, "FHD"));
        }
        if (StringUtils.isNotEmpty(hd)) {
            ret.add(createEmbedQualityLink(server + "/getvid?evid=" + hd, name, "HD"));
        }
        ret.add(createEmbedQualityLink(sdUrl, name, "SD"));
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(name);
        fp.addLinks(ret);
        return ret;
    }

    private DownloadLink createEmbedQualityLink(final String url, final String name, final String quality) {
        final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
        dl.setFinalFileName(name + " [" + quality + "].mp4");
        dl.setAvailable(true);
        dl.setProperty(DirectHTTP.PROPERTY_REQUEST_TYPE, "GET");
        return dl;
    }

    /** Returns false for hosts without subdomain e.g. 'wcostream.com' or '.wcostream.com' */
    private static boolean hasSubdomain(final String host) {
        if (StringUtils.isEmpty(host)) {
            return false;
        }
        final String[] parts = host.toLowerCase(Locale.ENGLISH).replaceFirst("\\.+$", "").split("\\.", -1);
        if (parts.length < 3) {
            return false;
        }
        for (final String part : parts) {
            if (part.length() == 0) {
                return false;
            }
        }
        return true;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        if (PATTERN_EMBED.matcher(param.getCryptedUrl()).matches()) {
            return crawlEmbedPage(param);
        }
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>\\s*([^<]+)\\s+\\|\\s+Watch").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
        }
        final Set<String> links = new LinkedHashSet<String>();
        // Handle list page
        Collections.addAll(links, br.getRegex("<li><a[^>]+href\\s*=\\s*\"\\s*([^\"]+)\\s*\"[^>]+class\\s*=\\s*\"[^\"]*sonra[^\"]*\"").getColumn(0));
        // Handle video page
        final Set<String> embedPageLinks = new LinkedHashSet<String>();
        String[][] encodedSections = br.getRegex("var\\s+\\w+\\s*=\\s*\"\\s*\"\\;\\s*var\\s+\\w+\\s*=\\s*\\[([^\\]]+)\\][^>]+replace[^>]+-\\s*(\\d+)").getMatches();
        if (encodedSections != null && encodedSections.length > 0) {
            for (String[] encodedSection : encodedSections) {
                String[] encodedStrings = new Regex(encodedSection[0], "\"\\s*([^\"\\s]+)\\s*\"").getColumn(0);
                int offset = Integer.parseInt(encodedSection[1]);
                StringBuilder builder = new StringBuilder();
                String decodedString = "";
                if (encodedStrings != null && encodedStrings.length > 0) {
                    for (String encodedString : encodedStrings) {
                        decodedString = Encoding.Base64Decode(encodedString);
                        decodedString = decodedString.replaceAll("[^\\d]+", ""); // Remove everything non-numeric
                        int decodedNumber = Integer.parseInt(decodedString) - offset; // Get the ASCII character ID
                        builder.append((char) decodedNumber);
                    }
                    decodedString = builder.toString();
                    decodedString = new Regex(decodedString, "src\\s*=\\s*\"([^\"]+)").getMatch(0);
                    embedPageLinks.add(decodedString);
                }
            }
        }
        Collections.addAll(embedPageLinks, br.getRegex("<meta[^>]+itemprop\\s*=\\s*\"embedURL\"[^>]+content=\"\\s*([^\"]+)\\s*\"").getColumn(0));
        if (!embedPageLinks.isEmpty()) {
            int counter = 0;
            for (String embedPageLink : embedPageLinks) {
                counter++;
                logger.info("Crawling mirror " + counter + "/" + embedPageLinks.size());
                if (StringUtils.isEmpty(embedPageLink)) {
                    continue;
                }
                final Browser br2 = br.cloneBrowser();
                br2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                br2.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
                br2.getPage(embedPageLink);
                String embedServiceLink = br2.getRegex("get\\s*\\(\\s*\"\\s*([^\"]+)\\s*\"\\s*\\)").getMatch(0);
                if (StringUtils.isNotEmpty(embedServiceLink)) {
                    br2.getPage(embedServiceLink);
                    String embedServiceResponse = br2.toString();
                    final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(embedServiceResponse);
                    if (entries == null) {
                        continue;
                    }
                    final String server = (String) entries.get("cdn");
                    final String enc = (String) entries.get("enc");
                    final String hd = (String) entries.get("hd");
                    if (StringUtils.isEmpty(server)) {
                        continue;
                    }
                    if (StringUtils.isNotEmpty(enc)) {
                        final String finallink = server + "/getvid?evid=" + enc;
                        links.add(finallink);
                        links.add("directhttp://" + finallink);
                    }
                    if (StringUtils.isNotEmpty(hd)) {
                        final String finallink = server + "/getvid?evid=" + enc;
                        links.add(finallink);
                        links.add("directhttp://" + finallink);
                    }
                }
                if (this.isAbort()) {
                    break;
                }
            }
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (String url : links) {
            if (StringUtils.isNotEmpty(url)) {
                url = Encoding.htmlDecode(url).replaceAll("^//", "https://");
                final DownloadLink dl = createDownloadlink(url);
                if (StringUtils.startsWithCaseInsensitive(url, "directhttp://")) {
                    dl.setFinalFileName(title + ".mp4");
                    dl.setProperty(DirectHTTP.PROPERTY_REQUEST_TYPE, "GET");
                }
                dl.setAvailable(true);
                ret.add(dl);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        }
        fp.setAllowMerge(true);
        fp.setAllowInheritance(true);
        fp.addLinks(ret);
        return ret;
    }
}