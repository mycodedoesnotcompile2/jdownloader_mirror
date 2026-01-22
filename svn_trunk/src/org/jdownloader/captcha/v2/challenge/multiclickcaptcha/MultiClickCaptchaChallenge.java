package org.jdownloader.captcha.v2.challenge.multiclickcaptcha;

import java.io.File;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.MultiClickCaptchaResponse;

import jd.plugins.Plugin;

public class MultiClickCaptchaChallenge extends ImageCaptchaChallenge<MultiClickedPoint> {
    private int minClicks = 1;
    private int maxClicks = -1; // -1 for unlimited

    public MultiClickCaptchaChallenge(File imagefile, String explain, Plugin plugin) {
        this(imagefile, explain, plugin, -1);
    }

    public MultiClickCaptchaChallenge(File imagefile, String explain, Plugin plugin, int maxClicks) {
        super(imagefile, plugin.getHost(), explain, plugin);
        this.maxClicks = maxClicks;
    }

    public int getMinClicks() {
        return minClicks;
    }

    public int getMaxClicks() {
        // TODO: Evaluate this aka close captcha dialog once user has clicked the expected amount of times.
        return maxClicks;
    }

    @Override
    public AbstractResponse<MultiClickedPoint> parseAPIAnswer(String result, String resultFormat, ChallengeSolver<?> solver) {
        MultiClickedPoint res = JSonStorage.restoreFromString(result, new TypeRef<MultiClickedPoint>() {
        });
        return new MultiClickCaptchaResponse(this, solver, res);
    }
}