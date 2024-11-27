package org.jdownloader.captcha.v2.challenge.recaptcha.v2;

import jd.http.Browser;
import jd.plugins.DecrypterException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.CaptchaCrawlerHelperInterface;

public class CaptchaHelperCrawlerPluginRecaptchaV2 extends AbstractRecaptchaV2<PluginForDecrypt> implements CaptchaCrawlerHelperInterface {
    public CaptchaHelperCrawlerPluginRecaptchaV2(final PluginForDecrypt plugin, final Browser br, final String siteKey, final String secureToken, boolean boundToDomain) {
        super(plugin, br, siteKey, secureToken, boundToDomain);
    }

    public CaptchaHelperCrawlerPluginRecaptchaV2(final PluginForDecrypt plugin, final Browser br, final String siteKey) {
        this(plugin, br, siteKey, null, false);
    }

    public CaptchaHelperCrawlerPluginRecaptchaV2(final PluginForDecrypt plugin, final Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException, DecrypterException {
        logger.info((isEnterprise() ? "Enterprise|" : "Free|") + "SiteDomain:" + getSiteDomain() + "|SiteKey:" + getSiteKey() + "|Type:" + getType() + "|V3Action:" + (getV3Action() != null));
        runDdosPrevention();
        if (siteKey == null) {
            siteKey = getSiteKey();
            if (siteKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "RecaptchaV2 API Key can not be found");
            }
        }
        if (secureToken == null) {
            secureToken = getSecureToken();
            // non fatal if secureToken is null.
        }
        final PluginForDecrypt plugin = getPlugin();
        final RecaptchaV2Challenge c = createChallenge();
        return plugin.handleCaptchaChallenge(c);

    }
}
