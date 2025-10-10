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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperCrawlerPluginHCaptcha;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51635 $", interfaceVersion = 3, names = {}, urls = {})
public class AdultdbIo extends PluginForDecrypt {
    public AdultdbIo(PluginWrapper wrapper) {
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
        ret.add(new String[] { "adultdb.io" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([a-z0-9\\-]+)/");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String urlSlug = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        /* Check for some invalid URLs */
        if (new Regex(urlSlug, "(?i)(wp-admin|wp-includes|wp-json|wp-content|about-us|feed)").patternFind()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Form captchaform = null;
        boolean hCaptcha = false;
        for (final Form form : br.getForms()) {
            if (form.containsHTML("recaptcha-form")) {
                captchaform = form;
                break;
            } else if (form.containsHTML("hcaptcha-form")) {
                // 2025-10-09
                captchaform = form;
                hCaptcha = true;
                break;
            }
        }
        if (captchaform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String title = br.getRegex("class=\"file_title\"[^>]*>([^<]+)</h1>").getMatch(0);
        if (title == null) {
            /* Fallback */
            title = urlSlug.replace("-", " ").trim();
        }
        final String[][] keyValuePairs = new Regex(captchaform.getHtmlCode(), "data-([\\w-]+)=\"([^\"]+)\"").getMatches();
        if (keyValuePairs == null || keyValuePairs.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Find reCaptchaKey */
        String rcKey = null;
        String hcaptchaKey = null;
        if (hCaptcha) {
            hcaptchaKey = br.getRegex("data-sitekey=\"([^\"]+)").getMatch(0);
            if (hcaptchaKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else {
            logger.info("Searching for reCaptchaKey");
            final String[] jsurls = br.getRegex("(/wp-content/litespeed/js/[^\"]+)\"").getColumn(0);
            if (jsurls != null && jsurls.length > 0) {
                final HashSet<String> dupes = new HashSet<String>();
                final Browser brx = br.cloneBrowser();
                for (final String jsurl : jsurls) {
                    if (!dupes.add(jsurl)) {
                        /* Skip duplicates */
                        continue;
                    }
                    brx.getPage(jsurl);
                    rcKey = brx.getRegex("\"recaptcha_key\":\\s*\"([^\"]+)\"").getMatch(0);
                    if (rcKey != null) {
                        break;
                    }
                    if (this.isAbort()) {
                        throw new InterruptedException();
                    }
                }
            }
            if (rcKey == null) {
                logger.warning("Failed to find reCaptchaKey -> Using static fallback");
                rcKey = "6Lc12LYZAAAAAHrmiB-FozY-KoqQYLFxEj6xoiAm";
            }
        }
        /* Fill form */
        final Map<String, String> mappings = new HashMap<String, String>();
        mappings.put("ptid", "captcha_id");
        mappings.put("show_duration", "duration");
        for (final String[] kvlist : keyValuePairs) {
            /* Use different key according to mapping if needed. */
            final String key_raw = kvlist[0];
            if (key_raw.equals("sitekey")) {
                /* Ignore invalid/unneeded items */
                continue;
            }
            final String key;
            if (mappings.containsKey(key_raw)) {
                key = mappings.get(key_raw);
            } else {
                key = key_raw;
            }
            captchaform.put(key, kvlist[1]);
        }
        captchaform.put("action", "recaptcha_content");
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        brc.getHeaders().put("Origin", "https://www." + getHost());
        brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
        if (hCaptcha) {
            final String hcaptchaResponse = new CaptchaHelperCrawlerPluginHCaptcha(this, br, hcaptchaKey).getToken();
            captchaform.put("token", Encoding.urlEncode(hcaptchaResponse));
        } else {
            final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, rcKey).getToken();
            captchaform.put("token", Encoding.urlEncode(recaptchaV2Response));
        }
        captchaform.setAction("/wp-admin/admin-ajax.php");
        brc.submitForm(captchaform);
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!Boolean.TRUE.equals(entries.get("success"))) {
            /* Assume that it failed due to wrong captcha. */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        final String html = entries.get("content").toString();
        final String[] links = new Regex(html, "<a href=\"(https?://[^\"]+)").getColumn(0);
        if (links == null || links.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String singleLink : links) {
            ret.add(createDownloadlink(singleLink));
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(title).trim());
        fp.addLinks(ret);
        return ret;
    }
}
