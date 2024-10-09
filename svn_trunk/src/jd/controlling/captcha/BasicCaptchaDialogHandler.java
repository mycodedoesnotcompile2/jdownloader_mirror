package jd.controlling.captcha;

import java.awt.Image;

import jd.gui.swing.dialog.CaptchaDialog;
import jd.gui.swing.dialog.DialogType;

import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.AbstractCaptchaDialog;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;

public class BasicCaptchaDialogHandler extends ChallengeDialogHandler<CaptchaDialog, BasicCaptchaChallenge, String> {

    public BasicCaptchaDialogHandler(BasicCaptchaChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);
    }

    @Override
    protected CaptchaDialog createDialog(DialogType dialogType, int flag) {
        final Image[] images = getImages(captchaChallenge);
        if (images == null) {
            return null;
        }
        final CaptchaDialog dialog = new CaptchaDialog(captchaChallenge, flag, dialogType, getHost(), images, captchaChallenge.getExplain()) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (BasicCaptchaDialogHandler.this) {
                        BasicCaptchaDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        if (suggest != null) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    dialog.suggest(suggest);
                }
            }.waitForEDT();
        }
        return dialog;
    }

    @Override
    public void setSuggest(final String value) {
        super.setSuggest(value);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final AbstractCaptchaDialog<String> dialog = BasicCaptchaDialogHandler.this.dialog;
                if (dialog != null) {
                    ((CaptchaDialog) dialog).suggest(value);
                }
            }
        };
    }

}