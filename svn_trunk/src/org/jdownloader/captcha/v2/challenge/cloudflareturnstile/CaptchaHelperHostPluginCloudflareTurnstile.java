package org.jdownloader.captcha.v2.challenge.cloudflareturnstile;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

public class CaptchaHelperHostPluginCloudflareTurnstile extends AbstractCloudflareTurnstileCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginCloudflareTurnstile(PluginForHost plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginCloudflareTurnstile(PluginForHost plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        logger.info("SiteKey:" + getSiteKey());
        final CloudflareTurnstileChallenge challenge = createChallenge();
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        return plugin.handleCaptchaChallenge(link, challenge);
    }
}
