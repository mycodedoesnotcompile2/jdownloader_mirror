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
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
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
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

@DecrypterPlugin(revision = "$Revision: 51797 $", interfaceVersion = 2, names = {}, urls = {})
public class FcLc extends PluginForDecrypt {
    private static final String[] domains = { "fc.lc", "fcc.lc", "short.fc-lc.com", "short.articlix.com", "fc-lc.com", "fc-lc.xyz" };

    /**
     * returns the annotation pattern array
     *
     */
    public static String[] getAnnotationUrls() {
        // construct pattern
        final String host = getHostsPattern();
        return new String[] { host + "/[a-zA-Z0-9]{2,}" };
    }

    private static String getHostsPattern() {
        final StringBuilder pattern = new StringBuilder();
        for (final String name : domains) {
            pattern.append((pattern.length() > 0 ? "|" : "") + Pattern.quote(name));
        }
        final String hosts = "https?://(?:www\\.)?" + "(?:" + pattern.toString() + ")";
        return hosts;
    }

    @Override
    public String[] siteSupportedNames() {
        return domains;
    }

    /**
     * Returns the annotations names array
     *
     * @return
     */
    public static String[] getAnnotationNames() {
        return new String[] { "fc.lc" };
    }

    public enum CaptchaType {
        reCaptchaV2,
        reCaptchaV2_invisible,
        hCaptcha,
        WTF
    };

    public FcLc(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    private String appVars = null;

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contentURL = param.getCryptedUrl();
        final String source_host = Browser.getHost(contentURL);
        br.setFollowRedirects(false);
        br.getPage(contentURL);
        // 2019-11-13: http->https->different domain(https)
        // 2019-11-13: http->https->different domain(http)->different domain(https)
        while (true) {
            String redirect = br.getRedirectLocation();
            if (redirect == null) {
                /* 2019-01-29: E.g. cuon.io */
                redirect = br.getRegex("<meta http\\-equiv=\"refresh\" content=\"\\d+;url=(https?://[^\"]+)\">").getMatch(0);
            }
            if (redirect == null) {
                /* 2019-04-25: E.g. clkfly.pw */
                redirect = br.getHttpConnection().getHeaderField("Refresh");
                if (redirect != null && redirect.matches("^\\d+, http.+")) {
                    redirect = new Regex(redirect, "^\\d+, (http.+)").getMatch(0);
                }
            }
            if (redirect == null) {
                logger.info("Exit redirect loop at URL: " + br.getURL());
                break;
            }
            if (!redirect.contains(source_host + "/")) {
                /*
                 * 2018-07-18: Direct redirect without captcha or any Form e.g. vivads.net OR redirect to other domain of same service e.g.
                 * wi.cr --> wicr.me
                 */
                ret.add(this.createDownloadlink(redirect));
                return ret;
            } else {
                br.getPage(redirect);
                continue;
            }
        }
        br.setFollowRedirects(true);
        if (br.getHttpConnection().getResponseCode() == 403 || br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.toString().length() < 100) {
            /* 2020-05-29: E.g. https://uii.io/full */
            logger.info("Invalid HTML - probably offline content");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form beforeCaptcha = br.getFormbyProperty("id", "before-captcha");
        if (beforeCaptcha != null) {
            /* 2019-10-30: E.g. exe.io, redirecto.link */
            logger.info("Found pre-captcha Form");
            br.submitForm(beforeCaptcha);
        }
        /* 2020-07-06: E.g. fc.lc, fcc.lc */
        Form beforeCaptcha2 = br.getFormbyKey("ad_form_data");
        if ((br.getHost().equalsIgnoreCase("fc.lc") || br.getHost().equalsIgnoreCase("fc-lc.com")) && beforeCaptcha2 != null) {
            logger.info("Sending Form beforeCaptcha2");
            if (beforeCaptcha2.containsHTML("recaptcha") && br.getHost().equalsIgnoreCase("fc.lc")) {
                /* 2021-05-07: Can contain invisible reCaptchaV2 but not every domain has recaptcha js! */
                final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                beforeCaptcha2.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            } else if (beforeCaptcha2.containsHTML("g-recaptcha btn btn-primary")) {
                /* 2021-10-12 */
                final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                beforeCaptcha2.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
            }
            br.submitForm(beforeCaptcha2);
            beforeCaptcha2 = br.getFormbyKey("ad_form_data");
            if (beforeCaptcha2 != null && beforeCaptcha2.getAction() != null) {
                logger.info("Submitting second beforeCaptcha2");
                br.submitForm(beforeCaptcha2);
            }
        }
        appVars = br.getRegex("var (app_vars.*?)</script>").getMatch(0);
        Form form = getCaptchaForm();
        if (form == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!this.evalulateCaptcha(CaptchaType.reCaptchaV2, contentURL)) {
            /* 2020-10-26: short.fc-lc.com */
            br.submitForm(form);
        }
        final CaptchaType captchaType = getCaptchaType();
        if (evalulateCaptcha(captchaType, contentURL)) {
            logger.info("Captcha required");
            if (captchaType == CaptchaType.reCaptchaV2 || captchaType == CaptchaType.reCaptchaV2_invisible) {
                final String key;
                if (captchaType == CaptchaType.reCaptchaV2) {
                    key = getAppVarsResult("reCAPTCHA_site_key");
                } else {
                    key = getAppVarsResult("invisible_reCAPTCHA_site_key");
                }
                if (StringUtils.isEmpty(key)) {
                    logger.warning("Failed to find reCaptchaV2 key --> Fallback to default handling");
                    final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br) {
                        @Override
                        public TYPE getType() {
                            if (captchaType == CaptchaType.reCaptchaV2_invisible) {
                                return TYPE.INVISIBLE;
                            } else {
                                return TYPE.NORMAL;
                            }
                        }
                    }.getToken();
                    form.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                } else {
                    final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, key) {
                        @Override
                        public TYPE getType() {
                            if (captchaType == CaptchaType.reCaptchaV2_invisible) {
                                return TYPE.INVISIBLE;
                            } else {
                                return TYPE.NORMAL;
                            }
                        }
                    }.getToken();
                    form.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                }
            } else if (captchaType == CaptchaType.hCaptcha) {
                final String hcaptchaResponse = new CaptchaHelperCrawlerPluginHCaptcha(this, br).getToken();
                /* Browser puts hCaptcha token token 3 times into form: 2x "h-captcha-response", 1x "g-recaptcha-response" */
                form.put("h-captcha-response", Encoding.urlEncode(hcaptchaResponse));
                form.put("g-recaptcha-response", Encoding.urlEncode(hcaptchaResponse));
            } else {
                /* Unsupported captchaType -> Developer mistake */
                logger.warning("Unsupported captcha type!");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.submitForm(form);
            if (br.containsHTML("The CAPTCHA was incorrect")) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
        } else if (form != null) {
            logger.info("No captcha required");
            br.submitForm(form);
        }
        /*
         * 2025-11-05: Page that wants user to show activity like moving mouse, pressing mouse buttons and keys -> There are no deeper
         * checks to we can simply access the next URL.
         */
        String anotherRedirect = br.getRequest().getHTMLRefresh();
        if (anotherRedirect == null) {
            anotherRedirect = br.getRegex("const REDIRECT_URL = \"(https?://[^\"]+)\";").getMatch(0);
        }
        if (anotherRedirect != null) {
            logger.info("Found 'anotherRedirect': " + anotherRedirect);
            /* This may redirect to a t.co short-url, then to a location.replace page */
            br.getPage(anotherRedirect);
            String locationReplace = br.getRegex("location\\.replace\\(\"([^\"+])\"").getMatch(0);
            if (locationReplace == null) {
                locationReplace = br.getRequest().getHTMLRefresh();
            }
            if (locationReplace != null) {
                logger.info("Found locationReplace");
                locationReplace = PluginJSonUtils.unescape(locationReplace);
                br.getPage(locationReplace);
            } else {
                logger.info("Failed to find locationReplace");
            }
        }
        int counter = -1;
        handle_verify_steps: do {
            /* 2025-11-05: In between sending these forms there might be a wait time of about 10 seconds which is skippable. */
            counter++;
            final Form[] forms = br.getForms();
            if (forms == null || forms.length == 0) {
                logger.info("Loop " + counter + ": Failed to find any forms");
                break handle_verify_steps;
            }
            Form verifyform = null;
            for (final Form aform : forms) {
                if (aform.containsHTML("token_unlocked_list")) {
                    verifyform = aform;
                    break;
                }
            }
            if (verifyform == null) {
                logger.info("Loop " + counter + ": Failed to find verifyform");
                break;
            }
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("", "");
            brc.getPage(br.getURL() + "?start_countdown=1");
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final String fdata = entries.get("rand").toString();
            verifyform.put("fdata", fdata);
            br.submitForm(verifyform);
        } while (counter <= 3);
        /* The following logger is just an indicator for development */
        if (br.containsHTML("id=\"linkrdy\"")) {
            logger.info("Looks like success: We are on the page that should contain the final form");
        } else {
            logger.info("Looks like failure");
        }
        Form finalform = br.getFormbyKey("ad_form_data");
        // Form finalform = br.getFormbyKey("_Token[fields]");
        // if (finalform == null) {
        // finalform = br.getFormbyKey("_Token[unlocked]");
        // }
        if (finalform != null) {
            /* 2025-11-05: Wait is skippable */
            final boolean skipWait = true;
            if (finalform.getAction() == null || (!finalform.getAction().startsWith("/") && !finalform.getAction().startsWith("http"))) {
                /* 2020-10-26: They are replacing the action inside the form so that the POST request will go to another domain. */
                final String action_url = br.getRegex("var domainUrl = \"(https?://[^\"]+)\";").getMatch(0);
                if (action_url != null) {
                    logger.info("Found special action: " + action_url);
                    finalform.setAction(action_url);
                } else {
                    logger.warning("Failed to find special action");
                    finalform.setAction("/links/go");
                }
            }
            br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            br.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            br.getHeaders().put("Origin", "https://" + br.getHost());
            String waitStr = this.getAppVarsResult("counter_value");
            if (!skipWait) {
                int wait = 15;
                if (waitStr != null && waitStr.matches("\\d+")) {
                    logger.info("Found waittime in html, waiting (seconds): " + waitStr);
                    wait = Integer.parseInt(waitStr) * +1;
                    this.sleep(wait * 1000, param);
                } else {
                    logger.info("Failed to find waittime in html");
                }
            } else {
                logger.info("Skipping waittime (seconds): " + waitStr);
            }
            br.submitForm(finalform);
        } else {
            logger.warning("Failed to find finalform -> This will most likely result in a plugin failure");
        }
        final String finallink = getFinallink();
        if (finallink == null) {
            if (br.containsHTML("<h1>\\s*Whoops, looks like something went wrong\\.\\s*</h1>")) {
                throw new DecrypterRetryException(RetryReason.HOST);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        ret.add(createDownloadlink(finallink));
        return ret;
    }

    private Form getCaptchaForm() {
        return br.getForm(0);
    }

    /**
     * true = captcha required, false = no captcha required <br />
     *
     * @param captchatype
     *            : Type of the captcha found in js/html - required in some rare cases.
     */
    private boolean evalulateCaptcha(final CaptchaType captchatype, final String url_source) {
        boolean hasCaptcha;
        // String captchaIndicatorValue = getAppVarsResult("enable_captcha");
        final String captchaIndicator = getAppVarsResult("captcha_shortlink");
        if (captchaIndicator != null) {
            /* Most website will contain this boolean-like value telling us whether we need to solve a captcha or not. */
            logger.info("Found captchaIndicator");
            if ("yes".equals(captchaIndicator)) {
                logger.info("Positive captchaIndicator --> Captcha required(?)");
                hasCaptcha = true;
            } else {
                logger.info("Negative captchaIndicator --> No captcha required(?)");
                hasCaptcha = false;
            }
        } else {
            /*
             * In some cases, we have to check for the type of the captcha to find out whether there is a captcha or not (unsafe method,
             * only needed in rare cases, see example websites in header of this class!)
             */
            if (captchatype != null) {
                hasCaptcha = true;
            } else {
                hasCaptcha = false;
            }
        }
        return hasCaptcha;
    }

    private CaptchaType getCaptchaType() {
        final String captchaTypeStr = getAppVarsResult("captcha_type");
        if (StringUtils.isEmpty(captchaTypeStr)) {
            /* 2018-12-11: Special case e.g. linkdrop.net */
            final String reCaptchaV2Key = getAppVarsResult("reCAPTCHA_site_key");
            if (reCaptchaV2Key != null) {
                return CaptchaType.reCaptchaV2;
            }
            /* No captcha or plugin broken */
            return null;
        }
        /* 2025-11-04: "captcha_type" says "invisible-recaptcha" but what they actually use is hCaptcha. */
        if (br.containsHTML("\"h-captcha-response\"")) {
            return CaptchaType.hCaptcha;
        }
        if (captchaTypeStr.equalsIgnoreCase("recaptcha")) {
            /*
             * 2018-07-18: For 'recaptcha', key is in "reCAPTCHA_site_key"; for 'invisible-recaptcha', key is in
             * "invisible_reCAPTCHA_site_key" --> We can usually use "reCAPTCHA_site_key" as well (tested with urle.co) ... but it is better
             * to use the correct one instead - especially because sometimes only that one is available.
             */
            return CaptchaType.reCaptchaV2;
        } else if (captchaTypeStr.equalsIgnoreCase("invisible-recaptcha")) {
            return CaptchaType.reCaptchaV2_invisible;
        } else {
            return CaptchaType.WTF;
        }
    }

    private String getAppVarsResult(final String input) {
        String result = new Regex(this.appVars, "app_vars\\['" + Pattern.quote(input) + "'\\]\\s*=\\s*'([^']*)'").getMatch(0);
        if (result == null) {
            /* 2018-07-18: json e.g. adbilty.me */
            result = PluginJSonUtils.getJson(this.appVars, input);
        }
        return result;
    }

    private String getFinallink() throws Exception {
        String finallink = null;
        if (br.getRequest().getHtmlCode().startsWith("{")) {
            /* For >90%, this json-attempt should work! */
            finallink = PluginJSonUtils.getJsonValue(br, "url");
        }
        if (StringUtils.isEmpty(finallink) || !finallink.startsWith("http")) {
            finallink = br.getRegex("<a href=(\"|')(.*?)\\1[^>]+>\\s*Get\\s+Link\\s*</a>").getMatch(1);
            if (StringUtils.isEmpty(finallink)) {
                finallink = br.getRegex("<a\\s+[^>]*href=(\"|')(.*?)\\1[^>]*>Continue[^<]*</a>").getMatch(1);
            }
        }
        /* 2020-02-03: clk.in: p.clk.in/?n=bla */
        if (!StringUtils.isEmpty(finallink) && finallink.matches("https?://p\\.[^/]+/\\?n=.+")) {
            logger.info("Special case: Finallink seems to lead to another step");
            br.getPage(finallink);
            finallink = br.getRegex("<div class=\"button\">\\s*?<center>\\s*?<a name=\"a\"\\s*?href=\"(http[^\"]+)\">").getMatch(0);
        }
        return finallink;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        /* Most of all providers will require the user to solve a reCaptchaV2. */
        return true;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.MightyScript_AdLinkFly;
    }
}