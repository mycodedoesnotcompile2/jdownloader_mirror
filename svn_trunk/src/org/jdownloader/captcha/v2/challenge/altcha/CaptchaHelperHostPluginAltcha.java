package org.jdownloader.captcha.v2.challenge.altcha;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;

public class CaptchaHelperHostPluginAltcha extends AbstractAltchaCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginAltcha(PluginForHost plugin, Browser br) {
        super(plugin, br);
    }

    public CaptchaHelperHostPluginAltcha(PluginForHost plugin, Browser br, String challengeUrl, String challengeJson) {
        super(plugin, br, challengeUrl, challengeJson);
    }

    /**
     * @return Base64 encoded solution (payload), usually the value of the form field 'altcha'.
     */
    public String getToken() throws PluginException, InterruptedException {
        final AltchaChallenge challenge = createChallenge();
        logger.info("ChallengeUrl:" + challenge.getChallengeUrl() + "|ChallengeJson:" + challenge.getChallengeJson());
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        return plugin.handleCaptchaChallenge(link, challenge);
    }
}
