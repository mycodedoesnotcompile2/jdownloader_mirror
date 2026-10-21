package org.jdownloader.captcha.v2.solver.browser;

import jd.controlling.captcha.ChallengeDialogHandler;
import jd.gui.swing.dialog.DialogType;

import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;

public class BrowserDialogHandler extends ChallengeDialogHandler<BrowserCaptchaDialog, AbstractBrowserChallenge, String> {

    public BrowserDialogHandler(AbstractBrowserChallenge captchaChallenge) {
        super(captchaChallenge.getDomainInfo(), captchaChallenge);
    }

    @Override
    protected int getDialogCountdownInMS() {
        return BrowserSolverService.getInstance().getConfig().getDialogCountdownSeconds() * 1000;
    }

    @Override
    protected BrowserCaptchaDialog createDialog(DialogType dialogType, int flag) {
        final BrowserCaptchaDialog dialog = new BrowserCaptchaDialog(flag, dialogType, getHost(), captchaChallenge) {
            public void dispose() {
                try {
                    super.dispose();
                } finally {
                    synchronized (BrowserDialogHandler.this) {
                        BrowserDialogHandler.this.notifyAll();
                    }
                }
            }
        };
        dialog.setTimeout(getTimeoutInMS());
        return dialog;
    }

}