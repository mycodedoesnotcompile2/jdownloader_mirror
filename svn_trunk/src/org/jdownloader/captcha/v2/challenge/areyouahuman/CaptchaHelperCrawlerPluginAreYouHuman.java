package org.jdownloader.captcha.v2.challenge.areyouahuman;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;

public class CaptchaHelperCrawlerPluginAreYouHuman extends AbstractCaptchaHelperAreYouHuman<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginAreYouHuman(PluginForDecrypt plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperCrawlerPluginAreYouHuman(PluginForDecrypt plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        String apiKey = siteKey;
        if (apiKey == null) {
            apiKey = getAreYouAHumanApiKey();
            if (apiKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "AreYouAHuman API Key can not be found");
            }
        }
        final PluginForDecrypt plugin = getPlugin();
        final AreYouAHumanChallenge c = new AreYouAHumanChallenge(apiKey, plugin) {
            @Override
            public BrowserViewport getBrowserViewport(BrowserWindow screenResource, java.awt.Rectangle elementBounds) {
                return null;
            }
        };
        return plugin.handleCaptchaChallenge(c);
    }
}
