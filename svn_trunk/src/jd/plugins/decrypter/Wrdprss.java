//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.nutils.encoding.HTMLEntities;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 49690 $", interfaceVersion = 3, names = { "hoerbuch.in", "hi10anime.com", "scene-rls.com", "cgpersia.com" }, urls = { "https?://(www\\.)?hoerbuch\\.in/blog\\.php\\?id=[\\d]+", "https?://(www\\.)?hi10anime\\.com/\\?page_id=.+", "https?://((www|nfo)\\.)?scene-rls\\.(com|net)/[\\w-/]+/?$", "https?://(?:www\\.)?cgpersia\\.com/\\d+/\\d+/[^/$]+\\.html?" })
public class Wrdprss extends antiDDoSForDecrypt {
    private HashMap<String, String[]> defaultPasswords = new HashMap<String, String[]>();

    public Wrdprss(PluginWrapper wrapper) {
        super(wrapper);
        /* Die defaultpasswörter der einzelnen seiten */
        defaultPasswords.put("cgpersia.com", new String[] { "cgpersia.com" });
    }

    @Override
    protected boolean useRUA() {
        return true;
    }

    private String contenturl = null;

    @SuppressWarnings("deprecation")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        contenturl = param.getCryptedUrl().replace("watchseries-online.ch/", "watchseries-online.pl/");
        if (StringUtils.startsWithCaseInsensitive(param.getCryptedUrl(), "https")) {
            contenturl = contenturl.replaceFirst("^http://", "https://");
        }
        getPage(contenturl);
        br.followRedirect();
        if (br.getHttpConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Defaultpasswörter der Seite setzen */
        final ArrayList<String> link_passwds = new ArrayList<String>();
        final String[] passwords = defaultPasswords.get(this.getHost());
        if (passwords != null) {
            for (final String password : passwords) {
                link_passwds.add(password);
            }
        }
        final ArrayList<String[]> customHeaders = new ArrayList<String[]>();
        if (contenturl.matches(".+hi10anime\\.com.+")) {
            customHeaders.add(new String[] { "Referer", br.getURL() });
        }
        /* Passwort suchen */
        final String password = br.getRegex(Pattern.compile("<.*?>Passwor(?:t|d)[<|:].*?[>|:]\\s*(.*?)[\\||<]", Pattern.CASE_INSENSITIVE)).getMatch(0);
        if (password != null) {
            link_passwds.add(password.trim());
        }
        if (contenturl.matches(".+watchseries-online\\.be.+")) {
            if (br.getRedirectLocation() != null) {
                br.followRedirect();
            }
            final String BaseURL = new Regex(br.getBaseURL(), "(https?://[^/]+)/").getMatch(0);
            final String[] lnks = br.getRegex("href=\"([^\"]+)\">Play<").getColumn(0);
            for (final String link : lnks) {
                ret.add(createDownloadlink(BaseURL + link));
            }
            return ret;
        }
        if (contenturl.matches(".+cgpersia\\.com.+")) {
            if (br.getRedirectLocation() != null) {
                br.followRedirect();
            }
            // final String BaseURL = new Regex(br.getBaseURL(), "(https?://[^/]+)/").getMatch(0);
            final String[] preBlocks = br.getRegex("<pre>\\s*([^<]+)\\s*</pre>").getColumn(0);
            if (preBlocks != null) {
                for (final String preBlock : preBlocks) {
                    final String[] lnks = HTMLParser.getHttpLinks(preBlock, null);
                    for (final String link : lnks) {
                        ret.add(createDownloadlink(link));
                    }
                }
            }
            return ret;
        }
        /* Alle Parts suchen */
        final String[] links = br.getRegex(Pattern.compile("href=.*?((?:(?:https?|ftp):)?//[^\"']{2,}|(&#x[a-f0-9]{2};)+)", Pattern.CASE_INSENSITIVE)).getColumn(0);
        final HashSet<String> dupe = new HashSet<String>();
        for (String link : links) {
            if (link.matches("(&#x[a-f0-9]{2};)+")) {
                // decode
                link = HTMLEntities.unhtmlentities(link);
            }
            link = Request.getLocation(link, br.getRequest());
            if (!dupe.add(link)) {
                continue;
            }
            if (kanHandle(link)) {
                final DownloadLink dLink = createDownloadlink(link);
                if (link_passwds != null && link_passwds.size() > 0) {
                    dLink.setSourcePluginPasswordList(link_passwds);
                }
                if (!customHeaders.isEmpty()) {
                    dLink.setProperty(DirectHTTP.PROPERTY_HEADERS, customHeaders);
                }
                ret.add(dLink);
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String title = br.getRegex("<title>([^<]+)").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            fp.setName(title);
        }
        fp.addLinks(ret);
        return ret;
    }

    public boolean kanHandle(final String link) {
        final boolean ch = !canHandle(link);
        if (!ch) {
            return ch;
        } else {
            return !link.matches(".+\\.(css|xml)(.*)?");
        }
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.WordPress_Wordpress;
    }
}