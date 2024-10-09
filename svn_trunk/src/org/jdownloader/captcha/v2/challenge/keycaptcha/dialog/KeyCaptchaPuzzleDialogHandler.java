package org.jdownloader.captcha.v2.challenge.keycaptcha.dialog;

import jd.controlling.captcha.ChallengeDialogHandler;
import jd.gui.swing.dialog.DialogType;

import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaPuzzleChallenge;

public class KeyCaptchaPuzzleDialogHandler extends ChallengeDialogHandler<KeyCaptchaPuzzleDialog, KeyCaptchaPuzzleChallenge, String> {

    public KeyCaptchaPuzzleDialogHandler(KeyCaptchaPuzzleChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);

    }

    @Override
    protected KeyCaptchaPuzzleDialog createDialog(DialogType dialogType, int flag) {
        final KeyCaptchaPuzzleDialog dialog = new KeyCaptchaPuzzleDialog(captchaChallenge, flag, dialogType, getHost()) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (KeyCaptchaPuzzleDialogHandler.this) {
                        KeyCaptchaPuzzleDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        return dialog;
    }

}