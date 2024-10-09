package jd.controlling.captcha;

import java.awt.Image;

import jd.gui.swing.dialog.ClickCaptchaDialog;
import jd.gui.swing.dialog.DialogType;

import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;

public class ClickCaptchaDialogHandler extends ChallengeDialogHandler<ClickCaptchaDialog, ClickCaptchaChallenge, ClickedPoint> {

    public ClickCaptchaDialogHandler(ClickCaptchaChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);
    }

    @Override
    protected ClickCaptchaDialog createDialog(DialogType dialogType, int flag) {
        final Image[] images = getImages(captchaChallenge);
        if (images == null) {
            return null;
        }
        final ClickCaptchaDialog dialog = new ClickCaptchaDialog(captchaChallenge, flag, dialogType, getHost(), images, captchaChallenge.getExplain()) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (ClickCaptchaDialogHandler.this) {
                        ClickCaptchaDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        return dialog;
    }

}