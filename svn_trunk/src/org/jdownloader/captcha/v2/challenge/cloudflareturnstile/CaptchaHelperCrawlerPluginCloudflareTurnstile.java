package org.jdownloader.captcha.v2.challenge.cloudflareturnstile;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

public class CaptchaHelperCrawlerPluginCloudflareTurnstile extends AbstractCloudflareTurnstileCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginCloudflareTurnstile(PluginForDecrypt plugin, Browser br) {
        super(plugin, br, null);
    }

    public CaptchaHelperCrawlerPluginCloudflareTurnstile(PluginForDecrypt plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        logger.info("SiteKey:" + getSiteKey());
        final CloudflareTurnstileChallenge challenge = createChallenge();
        final PluginForDecrypt plugin = getPlugin();
        return plugin.handleCaptchaChallenge(challenge);
    }
}
