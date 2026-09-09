//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 53356 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComV2FapnadoXxx extends KernelVideoSharingComV2 {
    public KernelVideoSharingComV2FapnadoXxx(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "fapnado.xxx", "fapnado.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    private final Map<String, String> cryptedMapping = new HashMap<String, String>();

    @Override
    public void clean() {
        cryptedMapping.clear();
        super.clean();
    }

    @Override
    protected String decryptDirectURLIfRequired(DownloadLink link, Browser br, String url) throws PluginException {
        if (br.containsHTML("unfurl\\(\"" + Pattern.quote(url))) {
            final String decrypted = decrypt_url(br, url);
            cryptedMapping.put(decrypted, url);
            return decrypted;
        }
        return super.decryptDirectURLIfRequired(link, br, url);
    }

    private String decrypt_url(final Browser br, String url) {
        String magic = br.getRegex("var a = '(\\d+)'").getMatch(0);
        if (magic == null) {
            /* Use static fallback */
            if (br.getHost().equals("fapnado.xxx")) {
                magic = "59230349905716806800799377149365";
            } else {
                /* fapnado.com */
                magic = "57498501723701598260159359313752";
            }
        }
        Pattern pattern = Pattern.compile("/[0-9]+/([^/]+)/");
        Matcher matcher = pattern.matcher(url);
        if (!matcher.find()) {
            return url;
        }
        String encoded = matcher.group(1);
        // unfash inline
        char[] chars = encoded.toCharArray();
        for (int c = chars.length - 1; c >= 0; c--) {
            int b = c;
            for (int d = c; d < 32; d++) {
                int digit = Character.getNumericValue(magic.charAt(d));
                b += digit;
            }
            while (b >= chars.length) {
                b -= chars.length;
            }
            // Swap characters at positions c and b
            char temp = chars[c];
            chars[c] = chars[b];
            chars[b] = temp;
        }
        String decoded = new String(chars);
        return url.replace(encoded, decoded);
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultVideosPattern(getPluginDomains());
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlTitle) {
        return generateContentURLDefaultVideosPattern(host, fuid, urlTitle);
    }

    @Override
    protected boolean isOfflineWebsite(final Browser br) {
        final String videoidFromURL = new Regex(br.getURL(), "/videos?/(\\d+)").getMatch(0);
        if (videoidFromURL != null && !br.containsHTML("/embed/" + videoidFromURL)) {
            /* Invalid link without error message e.g. /videos/6707/rachel-starr-gets- */
            return true;
        } else {
            return super.isOfflineWebsite(br);
        }
    }

    @Override
    protected int addQualityURL(Browser br, DownloadLink link, Map<Integer, String> qualityMap, String url) {
        final int ret = super.addQualityURL(br, link, qualityMap, url);
        if (ret != -1 || !cryptedMapping.containsKey(url)) {
            return ret;
        }
        final String orgUrl = cryptedMapping.get(url);
        final String label = br.getRegex(Pattern.quote(orgUrl) + "\"\\)\\);o.setAttribute\\(\"type\",\"video/mp4\"\\);o.setAttribute\\('title',\"(.*?)\"\\)").getMatch(0);
        final Integer height = labelToHeight(label);
        if (height == null) {
            return -1;
        }
        qualityMap.put(height, url);
        return height.intValue();
    }
}