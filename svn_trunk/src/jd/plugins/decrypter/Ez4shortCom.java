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

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperCrawlerPluginHCaptcha;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 49962 $", interfaceVersion = 3, names = {}, urls = {})
public class Ez4shortCom extends PluginForDecrypt {
    public Ez4shortCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "ez4short.com" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* Without this Cloudflare will kick in. */
        // br.getHeaders().put("Referer", "https://" + getHost() + "/");
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2024-10-10: Plugin and/or website broken */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String adSessionInitLink = br.getRegex("fetch\\('(https?://[^']+\\?sessionId=[a-f0-9]+)'").getMatch(0);
        if (adSessionInitLink != null) {
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Origin", "https://" + br.getHost()); // optional
            brc.getPage(adSessionInitLink);
        }
        final String htmlRefresh = br.getRegex("<meta http-equiv=\"refresh\" content=\"\\d+;url=(https?://[^\"]+)").getMatch(0);
        br.getPage(htmlRefresh);
        /* Redirect to the next fake blog page */
        final String nextRedirect = br.getRegex("window\\.location\\.href = \"(https?://[^\"]+)\"").getMatch(0);
        if (nextRedirect != null) {
            br.getPage(nextRedirect);
        }
        final String validatorName = br.getRegex("el\\.name = \"(validator\\d+)\";").getMatch(0);
        if (validatorName == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        String finalresult = null;
        steploop: for (int stepExpected = 1; stepExpected <= 10; stepExpected++) {
            logger.info("Processing step loop: " + stepExpected);
            final Regex stepRegex = br.getRegex(">\\s*Step: (\\d+)/(\\d+)");
            if (!stepRegex.patternFind()) {
                logger.warning("Failed to find step pattern");
                break;
            }
            final int stepCurrent = Integer.parseInt(stepRegex.getMatch(0));
            final int stepMax = Integer.parseInt(stepRegex.getMatch(1));
            logger.info("Processing: expected step: " + stepExpected + " | Real: " + stepCurrent + "/" + stepMax);
            if (stepCurrent >= stepMax) {
                logger.info("Stopping because: Reached stepMax: " + stepCurrent + "/" + stepMax);
                break steploop;
            } else if (stepCurrent != stepExpected) {
                logger.info("Stopping because: Step mismatch: Expected: " + stepExpected + " | Got: " + stepCurrent);
                break steploop;
            }
            final Form continueform = new Form();
            continueform.setMethod(MethodType.POST);
            continueform.setAction(br.getURL());
            continueform.put(Encoding.urlEncode(validatorName), "true");
            if (stepExpected == 1 && CaptchaHelperCrawlerPluginHCaptcha.containsHCaptcha(br)) {
                final String hcaptchaResponse = new CaptchaHelperCrawlerPluginHCaptcha(this, br).getToken();
                continueform.put("h-captcha-response", Encoding.urlEncode(hcaptchaResponse));
                continueform.put("g-recaptcha-response", Encoding.urlEncode(hcaptchaResponse));
            } else {
                continueform.put("no-recaptcha-noresponse", "true");
            }
            final String secondsWaitStr = br.getRegex("startingSeconds = (\\d+)").getMatch(0);
            if (secondsWaitStr != null) {
                final int secondsWait = (int) ((Integer.parseInt(secondsWaitStr) * 0.33) + 5);
                logger.info("Found wait time: " + secondsWaitStr + " -> Final wait: " + secondsWait);
                this.sleep(secondsWait * 1000l, param);
            } else {
                logger.info("Did not find wait time");
            }
            br.submitForm(continueform);
            finalresult = br.getRegex("window\\.location\\.href = \"(https?://[^\"]+)\"").getMatch(0);
            if (finalresult != null) {
                logger.info("Stopping because: Found final result");
                break steploop;
            } else if (this.isAbort()) {
                break steploop;
            }
        }
        if (finalresult == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /*
         * This should get us back to psa.btcut.io but now with a "token" parameter inside the URL which allows us to get to the final URL.
         */
        br.getPage(finalresult);
        /* TODO: Final step back to btcut.io URL with token is missing. */
        /* This btcut.io link can be offline */
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form goform = br.getFormbyProperty("id", "go-link");
        if (goform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final boolean skipWait = false;
        final String waitSecondsStr = br.getRegex("class=\"timer\"[^>]*>\\s*(\\d+)\\s*</span>").getMatch(0);
        if (waitSecondsStr != null && !skipWait) {
            this.sleep(Integer.parseInt(waitSecondsStr) * 1000l, param);
        }
        br.submitForm(goform);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String finallink = entries.get("url").toString();
        ret.add(this.createDownloadlink(finallink));
        return ret;
    }
}
