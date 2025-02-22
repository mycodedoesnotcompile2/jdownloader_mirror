package org.jdownloader.captcha.v2.challenge.cloudflareturnstile;

import org.appwork.exceptions.WTFException;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;

import jd.plugins.Plugin;

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
        if (!looksLikeValidSiteKey(siteKey)) {
            // default: SAs61IAI
            throw new WTFException("Bad SiteKey:" + siteKey);
        }
        this.siteKey = siteKey;
    }

    private static boolean looksLikeValidSiteKey(final String siteKey) {
        if (siteKey == null) {
            return false;
        } else if (siteKey.matches("^0x[a-zA-Z0-9\\-]{22}$")) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isCaptchaResponseValid() {
        if (super.isCaptchaResponseValid() && looksLikeValidToken(getResult().getValue())) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        return "turn";
    }

    public static boolean looksLikeValidToken(final String str) {
        /* E.g. 0.zTSnTXO0X0XwSjSCU8oyzbjEtD8p.d62306d4ee00c00dda690f959ebbd0bd90 */
        return str != null && str.matches("0\\.[a-zA-Z0-9_\\-\\.]{60,}");
    }

    @Override
    public boolean validateResponse(AbstractResponse<String> response) {
        return super.validateResponse(response) && looksLikeValidToken(response.getValue());
    }
}
