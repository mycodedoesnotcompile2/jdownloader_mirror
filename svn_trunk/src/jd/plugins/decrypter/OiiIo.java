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
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperCrawlerPluginHCaptcha;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.InputField;
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

@DecrypterPlugin(revision = "$Revision: 52919 $", interfaceVersion = 2, names = {}, urls = {})
public class OiiIo extends PluginForDecrypt {
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "oii.io" });
        return ret;
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([a-zA-Z0-9]{2,})");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    public enum CaptchaType {
        reCaptchaV2,
        reCaptchaV2_invisible,
        hCaptcha,
        WTF
    };

    public OiiIo(PluginWrapper wrapper) {
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
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        br.setFollowRedirects(true);
        if (br.getHttpConnection().getResponseCode() == 403 || br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.toString().length() < 100) {
            /* 2020-05-29: E.g. https://uii.io/full */
            logger.info("Invalid HTML - probably offline content");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form viewform = br.getFormbyProperty("id", "link-view");
        if (viewform != null) {
            final String hcaptchaResponse = new CaptchaHelperCrawlerPluginHCaptcha(this, br).getToken();
            viewform.put("g-recaptcha-response", Encoding.urlEncode(hcaptchaResponse));
            viewform.put("h-captcha-response", Encoding.urlEncode(hcaptchaResponse));
            br.submitForm(viewform);
        }
        appVars = br.getRegex("var (app_vars.*?)</script>").getMatch(0);
        final Form verify0 = br.getFormbyProperty("id", "verificationFormm");
        if (verify0 != null) {
            logger.info("Found verify0 form");
            final String cfSiteKey = br.getRegex("data-sitekey=\"([^\"]+)\"").getMatch(0);
            final Browser brc = br.cloneBrowser();
            if (cfSiteKey != null) {
                logger.info("Cloudflare Turnstile captcha #1 required");
                final InputField in_csrftoken = verify0.getInputFieldByName("_csrfToken");
                if (in_csrftoken != null) {
                    final String csrftoken = in_csrftoken.getValue();
                    brc.getHeaders().put("X-CSRF-Token", csrftoken);
                } else {
                    logger.warning("Failed to find csrftoken -> Subsequent request might fail");
                }
                /* Add a few extra form values that are added via js in browser */
                /* This is typically the id of the link e.g. oii.io/<alias> */
                final String extraval_alias = br.getRegex("formData\\.push\\(\\{ name: 'alias', value: \"([^\"]+)").getMatch(0);
                if (extraval_alias != null) {
                    verify0.put("alias", Encoding.urlEncode(extraval_alias));
                }
                final String extraval_session_token = br.getRegex("formData\\.push\\(\\{ name: 'session_token', value: \"([^\"]+)").getMatch(0);
                if (extraval_session_token != null) {
                    verify0.put("session_token", Encoding.urlEncode(extraval_session_token));
                }
                /* We know that the last captcha, ione had been requested this run, has been successful. */
                final String cfTurnstileResponse = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br, cfSiteKey).getToken();
                verify0.put("cf-turnstile-response", Encoding.urlEncode(cfTurnstileResponse));
                verify0.put("token", Encoding.urlEncode(cfTurnstileResponse));
            } else {
                logger.info("Cloudflare Turnstile captcha #1 NOT required");
            }
            brc.submitForm(verify0);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(entries.get("success"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            // TODO: Continue development here
            /**
             * Redirect to external fake blog site that displays ads e.g. <br>
             * aknewz.xyz (aknewz.xyz/?p_id=1234567 -> t.co -> aknews.xyz/
             */
            final String redirectUrl = entries.get("redirectUrl").toString();
            br.getPage(redirectUrl);
        } else {
            logger.info("Failed to find verify0 form");
        }
        String locationReplace = br.getRegex("location\\.replace\\(\"([^\"+])\"").getMatch(0);
        if (locationReplace == null) {
            locationReplace = br.getRequest().getHTMLRefresh();
        }
        if (locationReplace != null) {
            logger.info("Found locationReplace: " + locationReplace);
            locationReplace = PluginJSonUtils.unescape(locationReplace);
            br.getPage(locationReplace);
        } else {
            logger.info("Failed to find locationReplace");
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
            brc.getPage(br.getURL() + "?start_countdown=1");
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final String rand = entries.get("rand").toString();
            verifyform.put("fdata", rand);
            br.submitForm(verifyform);
        } while (counter <= 3);
        /* The following logger is just an indicator for development */
        if (br.containsHTML("id=\"linkrdy\"")) {
            logger.info("Looks like success: We are on the page that should contain the final form");
        } else {
            logger.info("Looks like failure");
        }
        final Form finalform = br.getFormbyKey("ad_form_data");
        if (finalform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (finalform.getAction() == null || (!finalform.getAction().startsWith("/") && !finalform.getAction().startsWith("http"))) {
            /* 2020-10-26: They are replacing the action inside the form so that the POST request will go to another domain. */
            final String action_url = br.getRegex("var domainUrl = \"(https?://[^\"]+)\";").getMatch(0);
            if (action_url != null) {
                logger.info("Found special action: " + action_url);
                finalform.setAction(action_url);
            } else {
                final String fallback = "/links/go1";
                logger.warning("Failed to find special action -> Using fallback: " + fallback);
                finalform.setAction(fallback);
            }
        }
        long passedTimeMillis = 0;
        final String cfSiteKey = br.getRegex("class=\"cf-turnstile\"[^<]*data-sitekey=\"([^\"]+)\"[^<]*data-callback=\"onTurnstileSuccess\"").getMatch(0);
        String waitSecondsStr = br.getRegex("<span id=\"timer\" class=\"timer\"[^>]*>(\\d{1,2})</span>\\s*<br>Seconds").getMatch(0);
        if (cfSiteKey != null) {
            logger.info("Cloudflare Turnstile captcha #2 required");
            /* We know that the last captcha, ione had been requested this run, has been successful. */
            this.validateLastChallengeResponse();
            final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
            final String cfTurnstileResponse = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br, cfSiteKey).getToken();
            finalform.put("token", Encoding.urlEncode(cfTurnstileResponse));
            passedTimeMillis = Time.systemIndependentCurrentJVMTimeMillis() - timeBefore;
        } else {
            logger.info("Cloudflare Turnstile captcha #2 NOT required");
        }
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        brc.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
        brc.getHeaders().put("Origin", "https://" + br.getHost());
        final String csrftoken = br.getCookie(br.getHost(), "csrfToken");
        if (csrftoken != null) {
            brc.getHeaders().put("X-CSRF-Token", csrftoken);
        } else {
            logger.warning("Failed to find csrftoken -> Subsequent request might fail");
        }
        /**
         * 2026-06-17: I'm unsure whether or not wait time is skippable but it pretty much doesn't matter since solving that Cloudflare
         * Turnstile captcha will often take around 15 seconds anyways.
         */
        final boolean skipWait = false;
        if (!skipWait) {
            int waitSeconds = 15;
            if (waitSecondsStr != null) {
                logger.info("Found waittime in html, waiting (seconds): " + waitSecondsStr);
                waitSeconds = Integer.parseInt(waitSecondsStr) * +1;
                long waitMillis = waitSeconds * 1000;
                waitMillis = waitMillis - passedTimeMillis;
                if (waitMillis > 0) {
                    this.sleep(waitMillis, param);
                } else {
                    logger.info("Skip wait as captcha solving took long enough to do so");
                }
            } else {
                logger.info("Failed to find waittime in html");
            }
        } else {
            logger.info("Skipping waittime (seconds): " + waitSecondsStr);
        }
        brc.submitForm(finalform);
        /*
         * e.g. error response: {"status":"success","message":"Access Denied.","url":""} <br> If this happens in browser, it will do
         * infinite looping on pre-ad pages.
         */
        /* example success: {"status":"success","message":"Go without Earn because anonymous user","url":"https..."} */
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final String finallink = entries.get("url").toString();
        final String message = (String) entries.get("message");
        if ("Access Denied.".equalsIgnoreCase(message)) {
            /* e.g. {"status":"success","message":"Access Denied.","url":""} */
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
        }
        if (!StringUtils.startsWithCaseInsensitive(finallink, "http")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        ret.add(createDownloadlink(finallink));
        return ret;
    }

    private String getAppVarsResult(final String input) {
        String result = new Regex(this.appVars, "app_vars\\['" + Pattern.quote(input) + "'\\]\\s*=\\s*'([^']*)'").getMatch(0);
        if (result == null) {
            result = PluginJSonUtils.getJson(this.appVars, input);
        }
        return result;
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