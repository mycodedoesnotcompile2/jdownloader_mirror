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
import java.util.Collections;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51695 $", interfaceVersion = 2, names = { "jdloader" }, urls = { "(jdlist://.+)|((dlc|rsdf|ccf)://.*/.+)" })
public class DLdr extends PluginForDecrypt {
    public DLdr(PluginWrapper wrapper) {
        super(wrapper);
    }

    // @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink parameter, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        String jdlist = new Regex(parameter.getCryptedUrl(), Pattern.compile("jdlist://(.+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
        if (jdlist != null) {
            /* Links einlesen */
            jdlist = Encoding.Base64Decode(jdlist);
            final String[] possibleLinks = HTMLParser.getHttpLinks(Encoding.htmlDecode(jdlist), null, null);
            for (final String possibleLink : possibleLinks) {
                final DownloadLink link = createDownloadlink(possibleLink);
                decryptedLinks.add(link);
            }
        } else {
            /* Container einlesen */
            final String url;
            final String format;
            if (new Regex(parameter.getCryptedUrl(), Pattern.compile("dlc://", Pattern.CASE_INSENSITIVE)).matches()) {
                format = ".dlc";
                url = new Regex(parameter.getCryptedUrl(), Pattern.compile("dlc://(.+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
            } else if (new Regex(parameter.getCryptedUrl(), Pattern.compile("ccf://", Pattern.CASE_INSENSITIVE)).matches()) {
                format = ".ccf";
                url = new Regex(parameter.getCryptedUrl(), Pattern.compile("ccf://(.+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
            } else if (new Regex(parameter.getCryptedUrl(), Pattern.compile("rsdf://", Pattern.CASE_INSENSITIVE)).matches()) {
                format = ".rsdf";
                url = new Regex(parameter.getCryptedUrl(), Pattern.compile("rsdf://(.+)", Pattern.CASE_INSENSITIVE)).getMatch(0);
            } else {
                throw new DecrypterException("Unknown Container prefix");
            }
            final Browser brc = br.cloneBrowser();
            final ArrayList<DownloadLink> containerResults = loadContainerFile(brc, brc.createGetRequest("http://" + url), Collections.singletonMap("extension", format));
            if (containerResults != null) {
                return containerResults;
            }
        }
        return decryptedLinks;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public Boolean siteTesterDisabled() {
        return Boolean.TRUE;
    }
}