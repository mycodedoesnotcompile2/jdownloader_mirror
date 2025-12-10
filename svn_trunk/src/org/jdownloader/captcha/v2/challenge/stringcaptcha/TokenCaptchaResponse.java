package org.jdownloader.captcha.v2.challenge.stringcaptcha;

import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;

public class TokenCaptchaResponse extends AbstractResponse<String> {
    public TokenCaptchaResponse(Challenge<String> challenge, Object solver, String token, int priority) {
        super(challenge, solver, priority, token);
    }
}
