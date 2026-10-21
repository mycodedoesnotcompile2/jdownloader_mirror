package org.jdownloader.captcha.v2.challenge.multiclickcaptcha;

import java.io.File;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.ValidationResult;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;

import jd.plugins.Plugin;

/**
 * Click captcha: the user/solver has to click on one or more positions of an image. A "single click captcha" is simply an instance with
 * {@code maxClicks == 1} (see {@link #isSingleClick()}).
 */
public class MultiClickCaptchaChallenge extends ImageCaptchaChallenge<MultiClickedPoint> {
    private final int minClicks;
    /** -1 for unlimited */
    private final int maxClicks;

    public MultiClickCaptchaChallenge(File imagefile, String explain, Plugin plugin) {
        this(imagefile, explain, plugin, -1);
    }

    public MultiClickCaptchaChallenge(File imagefile, String explain, Plugin plugin, int maxClicks) {
        this(imagefile, explain, plugin, 1, maxClicks);
    }

    public MultiClickCaptchaChallenge(File imagefile, String explain, Plugin plugin, int minClicks, int maxClicks) {
        super(imagefile, plugin.getHost(), explain, plugin);
        this.minClicks = minClicks;
        this.maxClicks = maxClicks;
    }

    public int getMinClicks() {
        return minClicks;
    }

    public int getMaxClicks() {
        return maxClicks;
    }

    /** True if exactly one click is expected, see CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA. */
    public boolean isSingleClick() {
        return maxClicks == 1;
    }

    /**
     * Rejects answers whose amount of clicks does not fit the expectation: fewer than minClicks, or - if min and max are equal - any
     * amount other than exactly that number. If only a max is set, answers with more clicks are rejected as well.
     */
    @Override
    public ValidationResult validateResponse(AbstractResponse<MultiClickedPoint> response) {
        final ValidationResult result = super.validateResponse(response);
        if (result != ValidationResult.VALID) {
            return result;
        }
        final MultiClickedPoint value = response.getValue();
        if (value == null || value.getX() == null || value.getY() == null) {
            return ValidationResult.EMPTY_RESPONSE;
        }
        final int clicks = value.getX().length;
        if (clicks != value.getY().length) {
            return ValidationResult.COORDINATES_MISMATCH;
        } else if (minClicks > 0 && minClicks == maxClicks && clicks != minClicks) {
            return ValidationResult.WRONG_CLICK_COUNT;
        } else if (clicks < minClicks) {
            return ValidationResult.TOO_FEW_CLICKS;
        } else if (maxClicks > 0 && clicks > maxClicks) {
            return ValidationResult.TOO_MANY_CLICKS;
        }
        return ValidationResult.VALID;
    }

    @Override
    public AbstractResponse<MultiClickedPoint> parseAPIAnswer(String result, String resultFormat, ChallengeSolver<?> solver) {
        MultiClickedPoint res = null;
        try {
            res = JSonStorage.restoreFromString(result, new TypeRef<MultiClickedPoint>() {
            });
        } catch (final Throwable ignore) {
            /* Not the multi click format, see below. */
        }
        if (res == null || res.getX() == null) {
            /* Single click captchas were answered as {"x":1,"y":2} -> keep accepting this format. */
            final ClickedPoint point = JSonStorage.restoreFromString(result, new TypeRef<ClickedPoint>() {
            });
            res = new MultiClickedPoint(new int[] { point.getX() }, new int[] { point.getY() });
        }
        return new AbstractResponse<MultiClickedPoint>(this, solver, res);
    }
}
