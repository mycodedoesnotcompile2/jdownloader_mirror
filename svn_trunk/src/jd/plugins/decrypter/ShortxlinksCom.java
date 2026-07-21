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
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

@DecrypterPlugin(revision = "$Revision: 53018 $", interfaceVersion = 3, names = {}, urls = {})
public class ShortxlinksCom extends PluginForDecrypt {
    public ShortxlinksCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "shortxlinks.com", "shortxlinks.in" });
        ret.add(new String[] { "arlinks.in" });
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
            /* 2026-07-16: go. subdomain is used for arlinks.in */
            ret.add("https?://(?:(?:www|go)\\.)?" + buildHostsPatternPart(domains) + "/(?!register|contact)([a-zA-Z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        // final String content_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* 2026-07-16: Two runs is the normal amount */
        int attempt = 1;
        do {
            final Form landing = br.getFormbyProperty("id", "landing");
            if (landing != null) {
                br.submitForm(landing);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
            final Form landing2 = br.getFormbyProperty("id", "landing");
            if (landing2 != null) {
                br.submitForm(landing2);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
            final Form landing3 = br.getFormbyKey("newwpsafelink");
            if (landing3 != null) {
                br.submitForm(landing3);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
            final String nextlink = br.getRegex("onclick=\"window\\.open\\('(https?://[^/\\?]+\\?safelink_redirect=[^']+)").getMatch(0);
            if (nextlink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* 2026-07-20: Looks like this delay is checked server side now. */
            final String waitStr = br.getRegex("let wpsafelinkCount = (\\d{1,2});").getMatch(0);
            if (waitStr != null) {
                logger.info("Found wait time seconds: " + waitStr);
                this.sleep(Long.parseLong(waitStr) * 1000, param);
            }
            br.getPage(nextlink);
            this.appVars = br.getRegex("var (app_vars.*?)</script>").getMatch(0);
            if (this.appVars != null) {
                break;
            }
            logger.info("Going into round: " + (attempt + 1));
            attempt++;
        } while (!this.isAbort() && attempt <= 3);
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        final Form goForm = getContinueForm(br);
        if (goForm == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String finallink = null;
        br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
        br.getHeaders().put("Origin", "https://" + br.getHost());
        if (goForm.hasInputFieldByName("_csrfToken")) {
            final InputField csrfField = goForm.getInputField("_csrfToken");
            final String csrftoken = csrfField != null ? csrfField.getValue() : null;
            if (!StringUtils.isEmpty(csrftoken)) {
                br.getHeaders().put("X-CSRF-Token", csrftoken);
            }
        }
        String waitSecondsStr = getAppVarsResult("counter_value");
        if (StringUtils.isEmpty(waitSecondsStr)) {
            waitSecondsStr = br.getRegex(">\\s*Please Wait\\s*(\\d+)s\\s*<").getMatch(0);
        }
        if (!StringUtils.isEmpty(waitSecondsStr)) {
            final int waitSeconds = Integer.parseInt(waitSecondsStr) + 1;
            logger.info("Waiting seconds: " + waitSeconds);
            this.sleep(waitSeconds * 1000, param);
        }
        br.setFollowRedirects(false);
        try {
            br.submitForm(goForm);
            finallink = br.getRedirectLocation();
        } finally {
            br.setFollowRedirects(true);
        }
        if (finallink == null) {
            finallink = findFinallink(br);
        }
        if (finallink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        ret.add(createDownloadlink(finallink));
        return ret;
    }

    private String appVars = null;

    /** Returns the Form that triggers the "/links/go" request which resolves the final link. */
    protected Form getContinueForm(final Browser br) {
        Form goForm = br.getFormbyKey("_Token[fields]");
        if (goForm == null) {
            goForm = br.getFormbyKey("_Token%5Bfields%5D");
        }
        if (goForm == null) {
            goForm = br.getFormbyAction("/links/go");
        }
        return goForm;
    }

    protected String getAppVarsResult(final String input) {
        String result = new Regex(this.appVars, "app_vars\\['" + Pattern.quote(input) + "'\\]\\s*=\\s*'([^']*)'").getMatch(0);
        if (result == null) {
            result = PluginJSonUtils.getJson(this.appVars, input);
        }
        if (StringUtils.isEmpty(result)) {
            return null;
        } else {
            return result;
        }
    }

    /** Returns final link that shall be returned by the crawler. Expected to come either as redirect or inside a json response. */
    protected String findFinallink(final Browser br) {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String finallink = (String) entries.get("url");
        logger.info("finallink from json: " + finallink);
        if (finallink != null && finallink.startsWith("/")) {
            logger.warning("finallink looks to be a relative URL --> Looks like something went wrong");
        }
        return finallink;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.MightyScript_AdLinkFly;
    }
}
