package org.jdownloader.captcha.v2.challenge.altcha;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;

public class CaptchaHelperCrawlerPluginAltcha extends AbstractAltchaCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginAltcha(PluginForDecrypt plugin, Browser br) {
        super(plugin, br);
    }

    public CaptchaHelperCrawlerPluginAltcha(PluginForDecrypt plugin, Browser br, String challengeUrl, String challengeJson) {
        super(plugin, br, challengeUrl, challengeJson);
    }

    /**
     * @return Base64 encoded solution (payload), usually the value of the form field 'altcha'.
     */
    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        final AltchaChallenge challenge = createChallenge();
        logger.info("ChallengeUrl:" + challenge.getChallengeUrl() + "|ChallengeJson:" + challenge.getChallengeJson());
        final PluginForDecrypt plugin = getPlugin();
        return plugin.handleCaptchaChallenge(challenge);
    }
}
