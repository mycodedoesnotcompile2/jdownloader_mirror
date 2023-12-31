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
import java.util.regex.Pattern;

import org.jdownloader.plugins.components.antiDDoSForDecrypt;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

@DecrypterPlugin(revision = "$Revision: 45935 $", interfaceVersion = 3, names = {}, urls = {})
public class MangahostCom extends antiDDoSForDecrypt {
    public MangahostCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    final static String[] domains = { "mangahost.com", "mangahost4.com", "mangahost.net", "mangahost.me", "mangahost.org", "mangahost.cc", "yesmangas.net", "mangahosts.com", "mangahost1.com", "mangahost2.com", "mangahosted.com" };

    /**
     * returns the annotation pattern array
     *
     */
    public static String[] getAnnotationUrls() {
        // construct pattern
        final String host = getHostsPattern();
        return new String[] { host + "/manga/[^/]+/([^\\s]*\\d+(\\.\\d+|[a-z])?|one-shot)" };
    }

    private static String getHostsPattern() {
        final StringBuilder pattern = new StringBuilder();
        for (final String name : domains) {
            pattern.append((pattern.length() > 0 ? "|" : "") + Pattern.quote(name));
        }
        final String hosts = "https?://(?:www\\.)?" + "(?:" + pattern.toString() + ")";
        return hosts;
    }

    @Override
    public String[] siteSupportedNames() {
        return domains;
    }

    /**
     * Returns the annotations names array
     *
     * @return
     */
    public static String[] getAnnotationNames() {
        return new String[] { "mangahost.com" };
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final String parameter = param.toString();
        br.setFollowRedirects(true);
        getPage(parameter);
        if (br.getHttpConnection().getResponseCode() == 403) {
            logger.info("GEO-blocked!!");
            decryptedLinks.add(this.createOfflinelink(parameter));
            return decryptedLinks;
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            decryptedLinks.add(this.createOfflinelink(parameter));
            return decryptedLinks;
        }
        final String host = br.getHost();
        final String fpName = br.getRegex("<title>(.*?)(?:\\s*\\|\\s*[^<]*)?</title>").getMatch(0);
        String[] links = null;
        if (br.containsHTML("var images")) {
            if (br.containsHTML("(jpe?g|png)\\.webp")) {
                links = br.getRegex("(https?://(?:img\\." + Pattern.quote(host) + "|img-host\\.filestatic\\d+?\\.xyz)/(?:br/)?images/[^<>\"\\']+\\.webp)").getColumn(0);
            } else {
                links = br.getRegex("(https?://(?:img\\." + Pattern.quote(host) + "|img-host\\.filestatic\\d+?\\.xyz)/(?:br/)?mangas_files/[^<>\"\\']+(jpe?g|png))").getColumn(0);
            }
        } else {
            // this is JSON, DO NOT universally unescape it.
            String pages = br.getRegex("var pages\\s*=\\s*(\\[\\{[^<>]+\\}\\])\\;").getMatch(0);
            final List<Object> resource = (List<Object>) JavaScriptEngineFactory.jsonToJavaObject(pages);
            if (links == null) {
                links = new String[resource.size()];
            }
            int i = 0;
            for (final Object page : resource) {
                links[i++] = (String) JavaScriptEngineFactory.walkJson(page, "url");
            }
        }
        if (links == null || links.length == 0) {
            logger.warning("Decrypter broken for link: " + parameter);
            return null;
        }
        for (String singleLink : links) {
            /* Correct final urls */
            singleLink = singleLink.replace("/images/", "/mangas_files/").replace(".webp", "");
            singleLink = "directhttp://" + singleLink;
            final DownloadLink dl = createDownloadlink(singleLink);
            dl.setAvailable(true);
            decryptedLinks.add(dl);
        }
        if (fpName != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(Encoding.htmlDecode(fpName.trim()));
            fp.addLinks(decryptedLinks);
        }
        return decryptedLinks;
    }
}
