package org.jdownloader.captcha.v2.challenge.powcaptcha;

import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.ValidationResult;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;

/**
 * Proof-of-work captcha ("powCAPTCHA") as used by filecrypt.cc: The website embeds a widget which fetches a challenge from a "captcha
 * session" JSON URL and computes the proof of work in a web worker.
 */
public abstract class PowCaptchaChallenge extends AbstractBrowserChallenge {
    private final String sessionUrl;
    private final String userAgent;

    /** Absolute URL of the captcha session JSON (value of the attribute 'data-session'). */
    public String getSessionUrl() {
        return sessionUrl;
    }

    /** User-Agent used by the plugin browser. External solvers need to use the same one. */
    public String getUserAgent() {
        return userAgent;
    }

    public PowCaptchaChallenge(final Plugin plugin, final String sessionUrl, final String userAgent) throws PluginException {
        super("powcaptcha", plugin);
        if (!AbstractPowCaptcha.looksLikeValidSessionUrl(sessionUrl)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Bad SessionUrl:" + sessionUrl);
        }
        this.sessionUrl = sessionUrl;
        this.userAgent = userAgent;
    }

    /** The challenge (session) is bound to the IP address that loads it, so the solution must be created from the same IP address. */
    @Override
    public boolean isSameIPRequired() {
        return true;
    }

    public boolean isCaptchaResponseValid() {
        if (super.isCaptchaResponseValid() && AbstractPowCaptcha.looksLikeValidToken(getResult().getValue())) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        return "pow";
    }

    @Override
    public ValidationResult validateResponse(AbstractResponse<String> response) {
        final ValidationResult result = super.validateResponse(response);
        if (result != ValidationResult.VALID) {
            return result;
        }
        return AbstractPowCaptcha.looksLikeValidToken(response.getValue()) ? ValidationResult.VALID : ValidationResult.INVALID_TOKEN;
    }
}
