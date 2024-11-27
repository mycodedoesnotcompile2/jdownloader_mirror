package org.jdownloader.captcha.v2.challenge.recaptcha.v2;

import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;

public class CaptchaHelperHostPluginRecaptchaV2 extends AbstractRecaptchaV2<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginRecaptchaV2(final PluginForHost plugin, final Browser br, final String siteKey, final String secureToken, boolean boundToDomain) {
        super(plugin, br, siteKey, secureToken, boundToDomain);
    }

    /* Most likely used for login captchas. */
    public CaptchaHelperHostPluginRecaptchaV2(final PluginForHost plugin, final Browser br, final String siteKey) {
        this(plugin, br, siteKey, null, false);
    }

    public CaptchaHelperHostPluginRecaptchaV2(final PluginForHost plugin, final Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        logger.info((isEnterprise() ? "Enterprise|" : "Free|") + "SiteDomain:" + getSiteDomain() + "|SiteKey:" + getSiteKey() + "|Type:" + getType() + "|V3Action:" + (getV3Action() != null));
        runDdosPrevention();
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
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
        final RecaptchaV2Challenge challenge = createChallenge();
        return plugin.handleCaptchaChallenge(link, challenge);
    }
}
