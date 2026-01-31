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

import java.io.IOException;
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

@HostPlugin(revision = "$Revision: 52221 $", interfaceVersion = 3, names = {}, urls = {})
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

    @Override
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        final Map<Integer, String> qualityMap = new HashMap<Integer, String>();
        String superDllink = super.getDllink(link, br);
        if (superDllink != null) {
            superDllink = decrypt_url(br, superDllink);
            final String qualityHeightStr = new Regex(superDllink, "_(\\d+)p\\.mp4").getMatch(0);
            int qualityHeight = 360;
            if (qualityHeightStr != null) {
                qualityHeight = Integer.parseInt(qualityHeightStr);
            }
            qualityMap.put(qualityHeight, superDllink);
        }
        String dllinkHD = br.getRegex("unfurl\\(\"(https://[^\"]+)\"\\)\\);o\\.setAttribute\\(\"type\",\"video/mp4\"\\);o.setAttribute\\('title',\"HD").getMatch(0);
        if (dllinkHD != null) {
            dllinkHD = decrypt_url(br, dllinkHD);
            if (superDllink == null || !dllinkHD.equals(superDllink)) {
                qualityMap.put(720, dllinkHD);
            }
        }
        return this.handleQualitySelection(br, link, qualityMap);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
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

    private static String decrypt_url(final Browser br, String url) {
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
}