package org.jdownloader.captcha.v2.challenge.powcaptcha;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;

public class CaptchaHelperHostPluginPowCaptcha extends AbstractPowCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginPowCaptcha(PluginForHost plugin, Browser br) {
        super(plugin, br);
    }

    public CaptchaHelperHostPluginPowCaptcha(PluginForHost plugin, Browser br, String sessionUrl) {
        super(plugin, br, sessionUrl);
    }

    /**
     * @return Solution as returned by the captcha solver. Usually a JSON object containing the values for the form fields 'pow_*'.
     */
    public String getToken() throws PluginException, InterruptedException {
        logger.info("SessionUrl:" + getSessionUrl());
        final PowCaptchaChallenge challenge = createChallenge();
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        return plugin.handleCaptchaChallenge(link, challenge);
    }
}
