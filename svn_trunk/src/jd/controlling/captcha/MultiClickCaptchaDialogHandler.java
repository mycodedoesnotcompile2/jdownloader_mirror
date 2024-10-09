package jd.controlling.captcha;

import java.awt.Image;

import jd.gui.swing.dialog.DialogType;
import jd.gui.swing.dialog.MultiClickCaptchaDialog;

import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;

public class MultiClickCaptchaDialogHandler extends ChallengeDialogHandler<MultiClickCaptchaDialog, MultiClickCaptchaChallenge, MultiClickedPoint> {

    public MultiClickCaptchaDialogHandler(MultiClickCaptchaChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);
    }

    @Override
    protected MultiClickCaptchaDialog createDialog(DialogType dialogType, int flag) {
        final Image[] images = getImages(captchaChallenge);
        if (images == null) {
            return null;
        }
        final MultiClickCaptchaDialog dialog = new MultiClickCaptchaDialog(captchaChallenge, flag, dialogType, getHost(), images, captchaChallenge.getExplain()) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (MultiClickCaptchaDialogHandler.this) {
                        MultiClickCaptchaDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        return dialog;
    }
}