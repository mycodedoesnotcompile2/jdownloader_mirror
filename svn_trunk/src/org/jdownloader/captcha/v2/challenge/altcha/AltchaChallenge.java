package org.jdownloader.captcha.v2.challenge.altcha;

import java.util.Map;

import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;

import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.ValidationResult;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;

/**
 * ALTCHA (https://altcha.org/): Open source proof-of-work captcha. The widget ('altcha-widget') fetches a challenge JSON from the
 * "challenge URL" of the website, computes the proof of work and puts the base64 encoded solution ("payload") into the form field 'altcha'.
 * <br>
 * The challenge is never solved locally but always passed to the captcha solvers (via challenge JSON and/or challenge URL).
 */
public abstract class AltchaChallenge extends AbstractBrowserChallenge {
    private final String challengeUrl;
    private final String challengeJson;

    /**
     * @param challengeUrl
     *            Absolute URL the widget fetches the challenge from. Can be null if challengeJson is given.
     * @param challengeJson
     *            Challenge JSON as returned by the challenge URL. Can be null if challengeUrl is given.
     */
    public AltchaChallenge(final Plugin plugin, final String challengeUrl, final String challengeJson) throws PluginException {
        super("altcha", plugin);
        if (!AbstractAltchaCaptcha.looksLikeValidChallengeUrl(challengeUrl) && !AbstractAltchaCaptcha.looksLikeValidChallengeJson(challengeJson)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Bad ChallengeUrl:" + challengeUrl + "|ChallengeJson:" + challengeJson);
        }
        this.challengeUrl = challengeUrl;
        this.challengeJson = challengeJson;
    }

    /** Absolute URL of the challenge JSON, can be null. */
    public String getChallengeUrl() {
        return challengeUrl;
    }

    /** Challenge JSON, can be null. If available, solvers should prefer it over {@link #getChallengeUrl()}. */
    public String getChallengeJson() {
        return challengeJson;
    }

    /**
     * Returns the parsed challenge if it is a v1 challenge (fields: algorithm, challenge, maxnumber, salt, signature), otherwise null. v2
     * challenges contain the object "parameters" instead (KDF based: PBKDF2, Argon2id, Scrypt, SHA).
     */
    public Map<String, Object> getChallengeV1() {
        return AbstractAltchaCaptcha.parseChallengeV1(challengeJson);
    }

    public boolean isCaptchaResponseValid() {
        if (super.isCaptchaResponseValid() && AbstractAltchaCaptcha.looksLikeValidToken(getResult().getValue())) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getCaptchaNameSpace() {
        return "altcha";
    }

    @Override
    public ValidationResult validateResponse(AbstractResponse<String> response) {
        final ValidationResult result = super.validateResponse(response);
        if (result != ValidationResult.VALID) {
            return result;
        }
        return AbstractAltchaCaptcha.looksLikeValidToken(response.getValue()) ? ValidationResult.VALID : ValidationResult.INVALID_TOKEN;
    }
}
