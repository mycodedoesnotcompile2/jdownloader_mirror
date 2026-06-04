//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52878 $", interfaceVersion = 3, names = {}, urls = {})
public class Gta5ModsCom extends PluginForDecrypt {
    public Gta5ModsCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    // always in english, also I haven't seen anything but "1 minute" but account for others cause why not
    private final Pattern WAIT_TIME = Pattern.compile("(?i)^Due to a high number of requests we have temporarily blocked traffic from your ip address for (\\d+) (minute|second|hour)s?\\b");
    private final Pattern DL_URL    = Pattern.compile("<a class\\s*=\\s*\"btn btn-primary btn-download\"\\s*href\\s*=\\s*\"(https?://files\\.gta5\\-mods\\.com/uploads/[^\"]+)\"");

    private static List<String[]> getPluginDomains() {
        List<String> doms = new ArrayList<String>();
        doms.add("gta5-mods.com");
        for (String loc : new String[] { "www", "id", "ms", "bg", "ca", "cs", "da", "de", "el", "es", "fr", "gl", "ko", "hi", "it", "hu", "mk", "nl", "no", "pl", "pt", "ro", "ru", "sl", "fi", "sv", "vi", "tr", "uk", "zh" }) {
            doms.add(loc + ".gta5-mods.com");
        }
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(doms.toArray(new String[0]));
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
            ret.add("https?://" + buildHostsPatternPart(domains) + "/(tools|vehicles|paintjobs|weapons|scripts|player|maps|misc)/[a-zA-Z0-9\\-]{4,}(/download/\\d+)?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        String url = param.getCryptedUrl();
        br.setHeader("referer", url);
        getPage(url, param);
        url = br.getRegex(DL_URL).getMatch(0);
        if (url == null) {
            final String downloadButton = br.getRegex("<a href=\"([^\"]+)\" class=\"btn btn-primary btn-download\"").getMatch(0);
            if (downloadButton == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            getPage(downloadButton, param);
            url = br.getRegex(DL_URL).getMatch(0);
            if (url == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        url = Encoding.htmlOnlyDecode(url);
        final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
        final String[] urlsplit = url.split("/");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        dl.setFinalFileName(urlsplit[urlsplit.length - 1].split("-", 2)[1]);
        ret.add(dl);
        return ret;
    }

    private void getPage(String url, CryptedLink param) throws Exception {
        br.getPage(url);
        int i = 0;
        for (; i < 10; i++) {
            Matcher match = WAIT_TIME.matcher(br.getRequest().getHtmlCode());
            if (!match.find()) {
                break;
            }
            long mult = 1000l;
            final String number = match.group(0);
            final String unit = match.group(1).toLowerCase();
            if ("hour".equals(unit)) {
                mult *= 60 * 60;
            } else if ("minute".equals(unit)) {
                mult *= 60;
            }
            sleep(Integer.parseInt(number) * mult - br.getRequest().getReadTime(), param);
            br.getPage(url);
        }
        if (i == 10) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE);
        }
    }
}
