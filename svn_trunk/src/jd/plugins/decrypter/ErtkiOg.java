//    jDownloader - Downloadmanager
//    Copyright (C) 2012  JD-Team support@jdownloader.org
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
import java.util.HashSet;
import java.util.List;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52740 $", interfaceVersion = 3, names = {}, urls = {})
public class ErtkiOg extends PluginForDecrypt {
    public ErtkiOg(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "erotelki.org" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_CONTENT  = Pattern.compile("/((photos|videos)/)?([\\w\\-]+)/(\\d+)\\-([\\w+\\-]+)\\.html", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_REDIRECT = Pattern.compile("/engine/go\\.php\\?url=([^<>\"']+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_CONTENT.pattern().substring(1) + "|" + PATTERN_REDIRECT.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }
    // DEV NOTES
    // newer content is base64encoded with occasional html encoding characters at
    // the end of string, mainly for =chars

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String parameter = param.getCryptedUrl();
        br.setFollowRedirects(false);
        br.setCookiesExclusive(true);
        final Regex regex_content = new Regex(parameter, PATTERN_CONTENT);
        if (regex_content.patternFind()) {
            final String title_from_url = regex_content.getMatch(4);
            br.getPage(parameter);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML(">\\s*К сожалению, данная страница для Вас не доступна, возможно был изменен ее адрес или она была удалена\\.")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String title = br.getRegex("<title>(.*?) \\&raquo\\;").getMatch(0);
            if (title == null) {
                title = br.getRegex("<h1 class=\"title_h\">(.*?)</h1>").getMatch(0);
            }
            if (title == null) {
                title = br.getRegex("<meta name=\"description\" content=\"NUDolls (.*?)\" />").getMatch(0);
            }
            final String[] regexes = { "url=([^<>\"']+)", "<a href=\"([^\"\\'<>]+)\" target=\"_blank\">", "href=\"(https?://(?:www\\.)?erotelki\\.org/uploads/posts/[^<>\"]*?)\" onclick=\"return hs\\.expand", "\"(https?://(?:www\\.)?erotelki\\.org/uploads/posts/[^<>\"]*?)\"" };
            final HashSet<String> dupes = new HashSet<String>();
            for (final String regex : regexes) {
                final String[] urls = br.getRegex(regex).getColumn(0);
                if (urls == null) {
                    continue;
                }
                for (final String url : urls) {
                    if (new Regex(url, PATTERN_REDIRECT).patternFind()) {
                        /* Skip these links here as they will be handled separately, see "url=" regex that picks up base64 elements. */
                        continue;
                    }
                    if (!dupes.add(url)) {
                        /* Skip dupes */
                        continue;
                    }
                    final String finalurl;
                    if (url.startsWith("aHR0")) {
                        /* Base64 encoded url */
                        finalurl = Encoding.Base64Decode(Encoding.htmlDecode(url));
                    } else if (url.matches("(?i).+erotelki\\.org/uploads/.+")) {
                        finalurl = DirectHTTP.createURLForThisPlugin(url);
                    } else {
                        finalurl = url;
                    }
                    ret.add(createDownloadlink(finalurl));
                }
            }
            final FilePackage fp = FilePackage.getInstance();
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
                fp.setName(title);
            } else {
                /* Fallback */
                logger.warning("Failed to find title in html code");
                fp.setName(title_from_url.replace("-", " ").trim());
            }
            fp.addLinks(ret);
        } else {
            final String finallink = Encoding.Base64Decode(Encoding.htmlDecode(new Regex(parameter, PATTERN_REDIRECT).getMatch(0)));
            ret.add(createDownloadlink(finallink));
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}