package org.jdownloader.captcha.v2.challenge.hcaptcha;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;

public class CaptchaHelperCrawlerPluginHCaptcha extends AbstractHCaptcha<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginHCaptcha(final PluginForDecrypt plugin, final Browser br, final String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperCrawlerPluginHCaptcha(final PluginForDecrypt plugin, final Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        logger.info("SiteDomain:" + getSiteDomain() + "|SiteKey:" + getSiteKey());
        runDdosPrevention();
        if (siteKey == null) {
            siteKey = getSiteKey();
            if (siteKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "HCaptcha sitekey can not be found");
            }
        }
        final PluginForDecrypt plugin = getPlugin();
        final HCaptchaChallenge c = createChallenge();
        return plugin.handleCaptchaChallenge(c);
    }
}
