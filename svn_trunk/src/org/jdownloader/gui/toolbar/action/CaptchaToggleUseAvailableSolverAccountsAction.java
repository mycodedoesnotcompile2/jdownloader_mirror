package org.jdownloader.gui.toolbar.action;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

/**
 * Global on/off switch for external (plugin based) captcha solver accounts, mirroring the existing "Use available Accounts" toggle
 * ({@link jd.gui.swing.jdgui.components.toolbar.actions.GlobalPremiumSwitchToggleAction}) but scoped to captcha solving. Backed by
 * {@link CFG_GENERAL#USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS}. Local solvers (dialog, browser, JAC) and the MyJDownloader remote solver are
 * not affected by this setting.
 */
public class CaptchaToggleUseAvailableSolverAccountsAction extends AbstractToolbarToggleAction {
    public CaptchaToggleUseAvailableSolverAccountsAction() {
        super(CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS);
        setIconKey(IconKey.ICON_OCR);
    }

    @Override
    protected String createTooltip() {
        if (getKeyHandler().isEnabled()) {
            return _GUI.T.CaptchaToggleUseAvailableSolverAccountsAction_tooltip_enabled();
        } else {
            return _GUI.T.CaptchaToggleUseAvailableSolverAccountsAction_tooltip_disabled();
        }
    }

    @Override
    protected String getNameWhenDisabled() {
        return _GUI.T.CaptchaToggleUseAvailableSolverAccountsAction_name();
    }

    @Override
    protected String getNameWhenEnabled() {
        return _GUI.T.CaptchaToggleUseAvailableSolverAccountsAction_name();
    }
}
