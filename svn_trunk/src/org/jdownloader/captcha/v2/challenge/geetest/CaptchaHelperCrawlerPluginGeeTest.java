package org.jdownloader.captcha.v2.challenge.geetest;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperCrawlerPluginGeeTest extends AbstractCaptchaHelperGeeTest<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginGeeTest(PluginForDecrypt plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperCrawlerPluginGeeTest(PluginForDecrypt plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        String apiKey = siteKey;
        if (apiKey == null) {
            apiKey = getGeeTestApiKey();
            if (apiKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "AreYouAHuman API Key can not be found");
            }
        }
        final PluginForDecrypt plugin = getPlugin();
        final GeeTestChallenge c = new GeeTestChallenge(apiKey, plugin) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, java.awt.Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(c);
    }
}
