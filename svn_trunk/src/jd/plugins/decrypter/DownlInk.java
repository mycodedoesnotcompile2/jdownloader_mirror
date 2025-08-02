//    jDownloader - Downloadmanager
//    Copyright (C) 2015  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicLong;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.FunctionObject;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 51296 $", interfaceVersion = 3, names = { "downl.ink" }, urls = { "https?://(?:www\\.)?downl\\.ink/(?-i)[a-f0-9]{6,}" })
@SuppressWarnings("deprecation")
public class DownlInk extends antiDDoSForDecrypt {
    public DownlInk(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected Browser prepBrowser(Browser prepBr, String host) {
        super.prepBrowser(prepBr, host);
        prepBr.addAllowedResponseCodes(new int[] { 401, 429 });
        return prepBr;
    }

    private String                  contenturl                         = null;
    private static final AtomicLong timestampLastCrawlProcessCompleted = new AtomicLong(0);

    public static String atob(String input) {
        final String ret = Encoding.Base64Decode(input);
        return ret;
    }

    private String eval(Browser br, String js) throws Exception {
        if (js == null) {
            js = br.getRegex("<script[^>]*>(.*?)</script>").getMatch(0);
        }
        if (js != null) {
            final ScriptEngineManager manager = JavaScriptEngineFactory.getScriptEngineManager(this);
            final ScriptEngine engine = manager.getEngineByName("javascript");
            final Context jsContext = Context.enter();
            try {
                final Method atob = getClass().getMethod("atob", new Class[] { String.class });
                engine.put("atob", new FunctionObject("atob", atob, jsContext.initStandardObjects()));
                engine.eval("var result3=\"NA\";");
                engine.eval("var result2=\"NA\";");
                engine.eval("var result1=\"NA\";");
                js = js.replace("eval(atob", "result1=(atob");
                engine.eval(js);
                js = (String) engine.get("result1");
                js = js.replace("eval(atob", "result2=(atob");
                js = js.replaceFirst("window\\.location\\.replace\\((.*?)\\);", "result3=$1;");
                engine.eval(js);
                return (String) engine.get("result3");
            } catch (final Exception e) {
                e.printStackTrace();
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, e);
            } finally {
                Context.exit();
            }
        }
        return null;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        contenturl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        // http can redirect to https
        br.setFollowRedirects(true);
        try {
            synchronized (timestampLastCrawlProcessCompleted) {
                /* Wait between crawl processes to try to avoid hitting their rate-limits. */
                final long timePassedSinceLastCrawlProcessMillis = Time.systemIndependentCurrentJVMTimeMillis() - timestampLastCrawlProcessCompleted.get();
                final long waitBetweenCrawlTasksMillis = 6000;
                if (timePassedSinceLastCrawlProcessMillis < waitBetweenCrawlTasksMillis) {
                    final long timeToWaitMillis = waitBetweenCrawlTasksMillis - timePassedSinceLastCrawlProcessMillis;
                    logger.info("Waiting until next crawl task is allowed [millis]: " + timeToWaitMillis);
                    this.sleep(timeToWaitMillis, param);
                }
                getPage(contenturl);
                // recaptchav2 can be here, they monitor based ip ? or maybe cloudflare cookie
                final Form captcha = br.getForm(0);
                if (captcha != null && containsRecaptchaV2Class(br)) {
                    final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                    captcha.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                    // no need for runPostRequestTask. usually cloudflare event is on FIRST request, so lets bypass.
                    br.submitForm(captcha);
                    // they will respond with 401 here which can throw exception without response code adding.
                    // then another get here, here comes the JS we need
                    getPage(br.getURL());
                    if (containsRecaptchaV2Class(br)) {
                        /* This shall be a rare case */
                        throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                    }
                }
                String result = eval(br, null);
                if (result != null) {
                    ret.add(createDownloadlink(result));
                    return ret;
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } finally {
            timestampLastCrawlProcessCompleted.set(Time.systemIndependentCurrentJVMTimeMillis());
        }
    }

    @Override
    protected void getPage(String page) throws Exception {
        super.getPage(page);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 429) {
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
        } else if (!canHandle(br.getURL()) || StringUtils.contains(br._getURL().getHost(), "google")) {
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2023-10-19: Set this to 1 to avoid running into "Flood detection" when user is processing a lot of items */
        return 1;
    }

    @Override
    public boolean hasCaptcha(final CryptedLink link, final jd.plugins.Account acc) {
        /* Captcha is sometimes needed. */
        return true;
    }
}