package org.jdownloader.captcha.v2.challenge.powcaptcha;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;

public class CaptchaHelperCrawlerPluginPowCaptcha extends AbstractPowCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginPowCaptcha(PluginForDecrypt plugin, Browser br) {
        super(plugin, br);
    }

    public CaptchaHelperCrawlerPluginPowCaptcha(PluginForDecrypt plugin, Browser br, String sessionUrl) {
        super(plugin, br, sessionUrl);
    }

    /**
     * @return Solution as returned by the captcha solver. Usually a JSON object containing the values for the form fields 'pow_*'.
     */
    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        logger.info("SessionUrl:" + getSessionUrl());
        final PowCaptchaChallenge challenge = createChallenge();
        final PluginForDecrypt plugin = getPlugin();
        return plugin.handleCaptchaChallenge(challenge);
    }
}
