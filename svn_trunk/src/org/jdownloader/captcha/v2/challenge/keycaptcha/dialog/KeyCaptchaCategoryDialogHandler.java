package org.jdownloader.captcha.v2.challenge.keycaptcha.dialog;

import jd.controlling.captcha.ChallengeDialogHandler;
import jd.gui.swing.dialog.DialogType;

import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaCategoryChallenge;

public class KeyCaptchaCategoryDialogHandler extends ChallengeDialogHandler<KeyCaptchaCategoryDialog, KeyCaptchaCategoryChallenge, String> {

    public KeyCaptchaCategoryDialogHandler(KeyCaptchaCategoryChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);

    }

    @Override
    protected KeyCaptchaCategoryDialog createDialog(DialogType dialogType, int flag) {
        final KeyCaptchaCategoryDialog dialog = new KeyCaptchaCategoryDialog(captchaChallenge, flag, dialogType, getHost()) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (KeyCaptchaCategoryDialogHandler.this) {
                        KeyCaptchaCategoryDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        return dialog;
    }

}