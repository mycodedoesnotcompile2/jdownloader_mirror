package org.jdownloader.captcha.v2.solver.solver9kw;

import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;

public class Captcha9kwResponse extends CaptchaResponse implements Captcha9KWResponseInterface {
    private final String Captcha9kwID;

    public Captcha9kwResponse(Challenge<String> challenge, Object solver, String captchaCode, final String captcha9kwID) {
        super(challenge, solver, captchaCode);
        this.Captcha9kwID = captcha9kwID;
    }

    @Override
    public String getCaptcha9kwID() {
        return Captcha9kwID;
    }
}
