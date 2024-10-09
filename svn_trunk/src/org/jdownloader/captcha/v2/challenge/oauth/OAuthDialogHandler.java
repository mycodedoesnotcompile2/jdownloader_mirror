package org.jdownloader.captcha.v2.challenge.oauth;

import jd.controlling.captcha.ChallengeDialogHandler;
import jd.gui.swing.dialog.DialogType;

public class OAuthDialogHandler extends ChallengeDialogHandler<OAuthDialog, OAuthChallenge, Boolean> {

    public OAuthDialogHandler(OAuthChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);
    }

    @Override
    protected int getTimeoutInMS() {
        return captchaChallenge.getTimeout();
    }

    @Override
    protected OAuthDialog createDialog(DialogType dialogType, int flag) {
        final OAuthDialog dialog = new OAuthDialog(flag, dialogType, getHost(), captchaChallenge) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (OAuthDialogHandler.this) {
                        OAuthDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        return dialog;
    }

}