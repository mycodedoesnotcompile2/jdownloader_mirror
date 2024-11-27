package org.jdownloader.captcha.v2.challenge.hcaptcha;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;

public class CaptchaHelperHostPluginHCaptcha extends AbstractHCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginHCaptcha(final PluginForHost plugin, final Browser br, final String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginHCaptcha(final PluginForHost plugin, final Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        logger.info("SiteDomain:" + getSiteDomain() + "|SiteKey:" + getSiteKey());
        runDdosPrevention();
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        if (siteKey == null) {
            siteKey = getSiteKey();
            if (siteKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "HCaptcha sitekey can not be found");
            }
        }
        final HCaptchaChallenge challenge = createChallenge();
        return plugin.handleCaptchaChallenge(link, challenge);
    }
}
