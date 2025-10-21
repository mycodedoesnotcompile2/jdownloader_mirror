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

import org.appwork.utils.Regex;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;

@DecrypterPlugin(revision = "$Revision: 51695 $", interfaceVersion = 3, names = {}, urls = {})
public class ShrinkeMe extends MightyScriptAdLinkFly {
    public ShrinkeMe(PluginWrapper wrapper) {
        super(wrapper);
    }

    private final String internalMainDomain = "shrinkme.click";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "shrinkme.click", "shrinke.me", "shrinkme.io", "shrinkme.info", "shrinkme.site", "shrinkme.us", "shrinkme.dev", "shrinke.us" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("shrinkme.io");
        return deadDomains;
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
            ret.add("https?://(?:(?:www|en)\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    protected void handleLandingRedirect(final CryptedLink param, Browser br) throws Exception {
        super.handleLandingRedirect(param, br);
        if (br.containsHTML("getRandomLink\\(\\)\\+\\\"\\?link")) {
            final String id = new Regex(param.getCryptedUrl(), ".+/([A-Za-z0-9]+)").getMatch(0);
            final Browser brc = br;
            brc.setCookie("themezon.net", "tp", id);
            brc.getPage("https://themezon.net/link.php?link=" + id);
            // google search redirect
            String location_href = brc.getRegex("window.location.href\\s*=\\s*\".*?url=(.*?)&.*?\";").getMatch(0);
            if (location_href == null) {
                // normal redirect
                location_href = brc.getRegex("window.location.href\\s*=\\s*\"(.*?)\";").getMatch(0);
            }
            if (location_href != null) {
                brc.getPage(location_href);
                Form form = brc.getFormByInputFieldKeyValue("newwpsafelink", id);
                brc.submitForm(form);
                brc.followRedirect(true);
                final String goToUrl = brc.getRegex("<a href\\s*=\\s*\"([^\"]*/" + id + ")\"").getMatch(0);
                brc.getPage(goToUrl);
            }
        }
    }

    @Override
    protected String getContentURL(final CryptedLink param) {
        final String contenturlOld = super.getContentURL(param);
        final String domainWithSubdomain = Browser.getHost(contenturlOld, true);
        final String contenturlNew = contenturlOld.replaceFirst(Pattern.quote(domainWithSubdomain) + "/", internalMainDomain + "/");
        if (!contenturlNew.equals(contenturlOld)) {
            logger.info("Changed URL: Old: " + contenturlOld + " | New: " + contenturlNew);
            return contenturlNew;
        } else {
            return contenturlOld;
        }
    }

    @Override
    protected String getSpecialReferer() {
        return "https://themezon.net/";
    }
}
