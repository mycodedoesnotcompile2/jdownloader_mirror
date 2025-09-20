package org.jdownloader.captcha.v2.challenge.cutcaptcha;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;

public class CaptchaHelperCrawlerPluginCutCaptcha extends AbstractCaptchaHelperCutCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginCutCaptcha(PluginForDecrypt plugin, Browser br) {
        super(plugin, br, null);
    }

    public CaptchaHelperCrawlerPluginCutCaptcha(PluginForDecrypt plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        logger.info("SiteKey:" + getSiteKey());
        final CutCaptchaChallenge challenge = createChallenge();
        final PluginForDecrypt plugin = getPlugin();
        return plugin.handleCaptchaChallenge(challenge);
    }

    @Override
    public int getSolutionTimeout() {
        return -1;
    }
}
