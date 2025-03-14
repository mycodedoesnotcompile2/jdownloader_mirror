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
import java.util.HashSet;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.PluginForDecrypt;

/**
 *
 * Decrypts base64 strings<br />
 * <br />
 *
 * NOTE: make sure the FIRST listening range is BASE64 otherwise this plugin will fail!
 *
 * @author raztoki
 *
 */
@DecrypterPlugin(revision = "$Revision: 49962 $", interfaceVersion = 3, names = { "rawBase64", "noriskdomain.com", "djurl.com", "hbrowse.com", "bookgn.com", "vip-files.net", "psdkeys.com", "hovatek.com", "ctvout.buzz" }, urls = { "(^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$)", "https?://(?:www\\.)?noriskdomain\\.com/[a-f0-9]{32}/analyze\\?u=([a-zA-Z0-9_/\\+\\=\\-%]+)", "https?://(?:\\w+\\.)?djurl\\.com/\\?r=([a-zA-Z0-9_/\\+\\=\\-%]+)", "https?://(?:\\w+\\.)?hbrowse\\.com/redirect/([a-zA-Z0-9_/\\+\\=\\-%]+)", "https?://(?:www\\.)?bookgn\\.com/engine/go\\.php\\?url=([a-zA-Z0-9_/\\+\\=\\-%]+)", "http://(?:www\\.)?vip-files\\.net/download\\.php\\?e=([a-zA-Z0-9_/\\+\\=\\-%]+)", "https?://(?:www\\.)?psdkeys\\.com/engine/go\\.php\\?url=([a-zA-Z0-9_/\\+\\=\\-%]+)", "https?://(?:www\\.)?hovatek\\.com/redirectcode\\.php\\?link=([a-zA-Z0-9_/\\+\\=\\-%]+)",
        "https?://(?:www\\.)?ctvout\\.buzz/#([a-zA-Z0-9_/\\+\\=\\-%]+)" })
public class GenericBase64Decrypter extends PluginForDecrypt {
    @Override
    public Boolean siteTesterDisabled() {
        // "saylicadebrid.tk" no dns, still works without!
        if ("saylicadebrid.tk".equals(getHost())) {
            return Boolean.TRUE;
        }
        return super.siteTesterDisabled();
    }

    public GenericBase64Decrypter(final PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.GENERIC };
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final String parameter = param.toString();
        final String b64 = Encoding.htmlDecode(new Regex(parameter, this.getSupportedLinks()).getMatch(0));
        if (b64 == null) {
            return null;
        }
        final HashSet<String> results = handleBase64Decode(b64);
        for (final String result : results) {
            decryptedLinks.add(createDownloadlink(result));
        }
        return decryptedLinks;
    }

    public final static HashSet<String> handleBase64Decode(final String b64) {
        if (b64 == null) {
            return null;
        }
        final HashSet<String> results = new HashSet<String>();
        String finallink = b64;
        int i = 0;
        // this covers nested encoding.
        while (i < 20 && finallink != null && !new Regex(finallink, "(?:ftp|https?)://.+").matches()) {
            i++;
            // cleanup crap after padding. this can break subsequent tries
            finallink = finallink.replaceFirst("(={1,2})[/\\w\\+]+$", "$1");
            finallink = Encoding.Base64Decode(finallink);
            // urldecode
            if (finallink != null && new Regex(finallink, "%[0-9A-Fa-f]{2}").matches()) {
                finallink = Encoding.urlDecode(finallink, false);
            }
            // whitespace cleanup
            finallink = StringUtils.trim(finallink);
        }
        // determine multi or single result?
        final String[] multi = new Regex(finallink, "(?:https?|ftp)://").getColumn(-1);
        if (multi != null && multi.length > 1) {
            // because links might not be separated or deliminated, its best to use htmlparser
            final String[] links = HTMLParser.getHttpLinks(finallink, "");
            if (links != null) {
                for (final String link : links) {
                    results.add(link);
                }
            }
        } else {
            results.add(finallink);
        }
        return results;
    }

    /* NO OVERRIDE!! */
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}