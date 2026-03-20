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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.AbstractCloudflareTurnstileCaptcha;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;

@DecrypterPlugin(revision = "$Revision: 52535 $", interfaceVersion = 3, names = {}, urls = {})
public class KeepshieldOrg extends PluginForDecrypt {
    public KeepshieldOrg(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "keepshield.org" });
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

    private static final Pattern TYPE_1 = Pattern.compile("/safe/([a-f0-9]{8,})");
    private static final Pattern TYPE_2 = Pattern.compile("/katf/([a-f0-9]{8,})");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + TYPE_1.pattern().substring(1) + "|" + TYPE_2.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /**
         * TODO: Implement captcha once they've implemented it server side 2025-02-02: Captcha can be chosen when creating a link but link
         * won't have a captcha then -> Website is buggy <br>
         * 2026-03-18: implemented Turnstile captcha. Creating links with other captcha types didn't work for me (again server-side
         * problems).
         */
        /* Some items are password protected */
        final Form pwform = this.getPasswordForm(br);
        if (pwform != null) {
            final String passCode = getUserInput("Password?", param);
            pwform.put("password", Encoding.urlEncode(passCode));
            br.submitForm(pwform);
            if (this.getPasswordForm(br) != null) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        /* Handle captcha */
        final Form captchaform = this.getCaptchaForm(br);
        if (captchaform != null) {
            if (AbstractCloudflareTurnstileCaptcha.containsCloudflareTurnstileClass(captchaform)) {
                final String response = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br).getToken();
                captchaform.put("cf-turnstile-response", Encoding.urlEncode(response));
            } else if (AbstractRecaptchaV2.containsRecaptchaV2Class(captchaform)) {
                final String response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                captchaform.put("g-recaptcha-response", Encoding.urlEncode(response));
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.submitForm(captchaform);
            if (this.getCaptchaForm(br) != null) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
        }
        /* Some items need an additional step/form with a wait time in beforehand. */
        final Form waitform = br.getFormbyProperty("id", "timer-form");
        if (waitform != null) {
            /* 2026-02-02: Wait is skippable */
            final boolean skipWait = true;
            if (!skipWait) {
                final String waitSecondsStr = br.getRegex("seconds = (\\d{1,2})").getMatch(0);
                final int waitSeconds = Integer.parseInt(waitSecondsStr);
                this.sleep(waitSeconds * 1001l, param);
            }
            br.submitForm(waitform);
        }
        String title = br.getRegex("<title>([^<]+) - KeepShield</title>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            title = title.replace("Protected Links", "");
        }
        final String links_as_json_array = br.getRegex("var allLinks = (\\[[^\\]]+\\]);").getMatch(0);
        if (links_as_json_array != null) {
            /** 2026-03-18: e.g. "/katf/..." links aka {@link #TYPE_2} */
            final List<Object> urls = restoreFromString(links_as_json_array, TypeRef.LIST);
            if (urls.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final Object url : urls) {
                final DownloadLink link = createDownloadlink(url.toString());
                ret.add(link);
            }
        } else {
            final String[] urls = br.getRegex("data-check-url=\"(https?://[^\"]+)").getColumn(0);
            if (urls == null || urls.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final String url : urls) {
                final DownloadLink link = createDownloadlink(url);
                ret.add(link);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        if (!StringUtils.isEmpty(title)) {
            fp.setName(title);
        }
        fp.addLinks(ret);
        return ret;
    }

    private Form getPasswordForm(final Browser br) {
        return br.getFormbyKey("password_submit");
    }

    private Form getCaptchaForm(final Browser br) {
        Form ret = br.getFormbyProperty("id", "captcha-form");
        if (ret == null) {
            ret = br.getFormbyKey("captcha_submit");
        }
        return ret;
    }
}
