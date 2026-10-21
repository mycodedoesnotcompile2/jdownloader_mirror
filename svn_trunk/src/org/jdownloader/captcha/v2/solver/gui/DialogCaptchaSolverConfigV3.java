package org.jdownloader.captcha.v2.solver.gui;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.plugins.config.Order;

public interface DialogCaptchaSolverConfigV3 extends CaptchaSolverConfigV3 {
    @DefaultIntValue(300)
    @SpinnerValidator(min = 10, max = 600, step = 1)
    @DescriptionForConfigEntry("Countdown in seconds of the captcha dialog. The dialog closes automatically when the countdown runs out.")
    @AboutConfig
    @Order(2000)
    int getDialogCountdownSeconds();

    void setDialogCountdownSeconds(int seconds);
}