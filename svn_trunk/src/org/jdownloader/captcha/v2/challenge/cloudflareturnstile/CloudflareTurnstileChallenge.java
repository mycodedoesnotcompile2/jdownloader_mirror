package org.jdownloader.captcha.v2.challenge.cloudflareturnstile;

import jd.plugins.Plugin;

import org.appwork.exceptions.WTFException;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;

public abstract class CloudflareTurnstileChallenge extends AbstractBrowserChallenge {
    private final String siteKey;

    public String getSiteKey() {
        return siteKey;
    }

    public String getSiteUrl() {
        return this.getPluginBrowser().getURL();
    }

    public CloudflareTurnstileChallenge(final Plugin plugin, final String siteKey) {
        super("cloudflareturnstile", plugin);
        this.siteKey = siteKey;
        if (!AbstractCloudflareTurnstileCaptcha.isValidSiteKey(siteKey)) {
            throw new WTFException("Bad SiteKey:" + siteKey);
        }
    }

    public boolean isCaptchaResponseValid() {
        if (super.isCaptchaResponseValid() && AbstractCloudflareTurnstileCaptcha.looksLikeValidToken(getResult().getValue())) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        return "turn";
    }

    @Override
    public boolean validateResponse(AbstractResponse<String> response) {
        return super.validateResponse(response) && AbstractCloudflareTurnstileCaptcha.looksLikeValidToken(response.getValue());
    }
}
