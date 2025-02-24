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
import java.util.regex.Matcher;

import org.jdownloader.plugins.components.antiDDoSForDecrypt;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 49596 $", interfaceVersion = 3, names = { "hentaidude.com" }, urls = { "https?://(?:www\\.)?hentaidude\\.com/page/.*|https?://(?:www\\.)?hentaidude.com/\\?tid=.*|^https?://(?:www\\.)?hentaidude\\.com/$" })
public class HentaiDudeComCrawler extends antiDDoSForDecrypt {
    private final String FASTER_NODLSIZE = "1";
    private final String SLOW_ALLPAGES   = "1";

    public HentaiDudeComCrawler(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    protected DownloadLink createDownloadlink(String url, String title) {
        final String ext = ".mp4";
        final DownloadLink dl = super.createDownloadlink(url, true);
        dl.setName(title + ext);
        if (getPluginConfig().getBooleanProperty(FASTER_NODLSIZE, true) == true) {
            dl.setAvailable(true);
        }
        dl.setProperty("mainlink", br.getURL());
        return dl;
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        String parameter = param.toString();
        ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        getPage(parameter);
        String page = br.toString();
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*404 - Sorry, nothing found")) {
            /* 2020-11-10: Without http response 404 */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String nextpage = br.getRegex("<a href=\"([^\"]+)\" class=\"styled-button\">Next").getMatch(0);
        /* Grab all pages if possible and enabled by user. */
        if (getPluginConfig().getBooleanProperty(SLOW_ALLPAGES, true) && nextpage != null) {
            int highest = 0;
            int minsize = 1;
            String newnextpage = "";
            Matcher nextpage2 = br.getRegex("<a href=\"([^\"]+)\">([0-9]+)").getMatcher();
            while (nextpage2.find()) {
                if (Integer.parseInt(nextpage2.group(2)) > highest) {
                    highest = Integer.parseInt(nextpage2.group(2));
                    newnextpage = nextpage2.group(1);
                }
            }
            newnextpage.replace("/" + highest + "/", "/1/");
            if (new Regex(parameter, "page/([0-9]+)/").getMatch(0) != null) {
                minsize = Integer.parseInt(new Regex(parameter, "page/([0-9]+)/").getMatch(0));
            }
            for (int i = minsize; i <= highest; i++) {
                if (i > minsize) {
                    getPage(nextpage);
                    page = br.toString();
                }
                final String[] results = HTMLParser.getHttpLinks(page, null);
                for (String result : results) {
                    if (result.matches("https?://hentaidude.com/.*([0-9]+|ova)/")) {
                        String fpName = br.getRegex("title=\"([^\"]+)\" href=\"" + result + "\"").getMatch(0);
                        if (fpName != null) {
                            if (fpName.length() > 4) {
                                ret.add(this.createDownloadlink(Encoding.htmlOnlyDecode(result), Encoding.htmlOnlyDecode(fpName)));
                            }
                        }
                    }
                }
                nextpage = br.getRegex("<a href=\"([^\"]+)\" class=\"styled-button\">Next").getMatch(0);
            }
        } else {
            final String[] results = HTMLParser.getHttpLinks(page, null);
            for (String result : results) {
                if (result.matches("https?://hentaidude.com/.*([0-9]+|ova)/")) {
                    String fpName = br.getRegex("title=\"([^\"]+)\" href=\"" + result + "\"").getMatch(0);
                    if (fpName != null) {
                        if (fpName.length() > 4) {
                            ret.add(this.createDownloadlink(Encoding.htmlOnlyDecode(result), Encoding.htmlOnlyDecode(fpName)));
                        }
                    }
                }
            }
        }
        if (ret.size() == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), FASTER_NODLSIZE, "Enable faster crawling (filesize won't be displayed until downloads are started)?").setDefaultValue(false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SLOW_ALLPAGES, "Grab all pages (very slow!)").setDefaultValue(false));
    }
}
