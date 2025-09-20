package org.jdownloader.captcha.v2.challenge.cutcaptcha;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;

public class CaptchaHelperHostPluginCutCaptcha extends AbstractCaptchaHelperCutCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginCutCaptcha(PluginForHost plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginCutCaptcha(PluginForHost plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        logger.info("SiteKey:" + getSiteKey());
        final CutCaptchaChallenge challenge = createChallenge();
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        return plugin.handleCaptchaChallenge(link, challenge);
    }

    @Override
    public int getSolutionTimeout() {
        return -1;
    }
}
