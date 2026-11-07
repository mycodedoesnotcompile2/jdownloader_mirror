package org.jdownloader.captcha.v2.solver.service;

import java.util.Set;

import jd.SecondLevelLaunch;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanel;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.Application;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.SilentModeController;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

public abstract class AbstractSolverService implements SolverService {
    public AbstractSolverService() {
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Override
    public Double getBalance() {
        return null;
    }

    @Override
    public java.util.Currency getBalanceCurrency() {
        return null;
    }

    @Override
    public String getHelpArticleURL() {
        return null;
    }

    @Override
    public String getBuyURL() {
        return null;
    }

    /**
     * A problem which makes a solver that is otherwise ready unusable (e.g. every captcha type it supports is disabled), as status text,
     * or null if there is none. Such a solver is not ready and shows a warning. Subclasses add their own problems and call super first.
     */
    protected String getBlockingStatusIssue() {
        if (isAllCaptchaTypesDisabled()) {
            return _GUI.T.CaptchaSolverService_status_allCaptchaTypesDisabled();
        }
        return null;
    }

    /**
     * A temporary circumstance the user should know about, but which does not make the solver unusable (e.g. silent mode for solvers that
     * need the user), as status text, or null if there is none. Shown with a warning, the solver stays ready.
     */
    protected String getInfoStatusIssue() {
        return null;
    }

    /** True if the solver supports captcha types and the user has disabled every one of them for it. */
    protected boolean isAllCaptchaTypesDisabled() {
        final Set<CAPTCHA_TYPE> supported = getSupportedCaptchaTypes();
        final Set<CAPTCHA_TYPE> disabled = getConfigV3().getDisabledCaptchaTypes();
        return supported != null && !supported.isEmpty() && disabled != null && disabled.containsAll(supported);
    }

    /**
     * True if the silent mode is active (see {@link SilentModeController#isActive()}) and the user could be asked on this computer otherwise. Never
     * in headless mode: There the dialogs are answered remotely (MyJDownloader), so manual solving is not blocked by the silent mode.
     */
    public static boolean isSilentModeActive() {
        return !Application.isHeadless() && SilentModeController.getInstance().isActive();
    }

    /**
     * True if the silent mode is active and set so that captchas must not be solved by the user. Solvers which need the user (dialog,
     * browser) must not be used then, but the other solvers still can solve the captcha, only the dialog is skipped. This is the case for
     * "Skip the Captcha" and "Do not show a dialog, but try auto solver", and for "Wait in Background ..." if dialogs during the silent
     * mode are set to "Cancel the dialog".
     */
    public static boolean isManualSolvingBlockedBySilentMode() {
        if (!isSilentModeActive()) {
            return false;
        }
        switch (CFG_SILENTMODE.CFG.getOnCaptchaDuringSilentModeAction()) {
        case SKIP_LINK:
        case DISABLE_DIALOG_SOLVER:
            return true;
        case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT:
        default:
            return isDialogCancelledBySilentMode();
        }
    }

    /**
     * True if the silent mode is active and set to "Wait in Background until window gets focus or timeout is reached" (and dialogs are not
     * cancelled): Solvers which need the user (dialog, browser) are used, but only after the user is back, so the other solvers should go
     * first.
     */
    public static boolean isManualSolvingDelayedBySilentMode() {
        return isSilentModeActive() && !isManualSolvingBlockedBySilentMode();
    }

    /** True if dialogs during the silent mode are set to "Cancel the dialog" (SilentModeSettings#getOnDialogDuringSilentModeAction). */
    private static boolean isDialogCancelledBySilentMode() {
        switch (CFG_SILENTMODE.CFG.getOnDialogDuringSilentModeAction()) {
        case CANCEL_DIALOG:
            return true;
        case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS:
        case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT:
        default:
            return false;
        }
    }

    /**
     * For solvers which need the user to solve the captcha on this computer: the status text if the silent mode is switched on (manually
     * or automatically) or active right now, else null. It is shown as soon as the silent mode is set, not only while it is effectively
     * active: Effectively it is not while a window of JDownloader has the focus (e.g. when the user switches it on), but then the user
     * should know why captcha dialogs might not be displayed later. One text for all silent mode settings, what exactly happens to the
     * captchas depends on the settings of the silent mode.
     */
    protected String getSilentModeStatusText() {
        if (Application.isHeadless()) {
            /* The dialogs are answered remotely (MyJDownloader), the silent mode does not matter. */
            return null;
        }
        final SilentModeController silentMode = SilentModeController.getInstance();
        if (!silentMode.isEnabledInSettings() && !silentMode.isActive()) {
            return null;
        }
        return _GUI.T.CaptchaSolverService_status_silentMode();
    }

    @Override
    public String getStatusText() {
        final String blocking = getBlockingStatusIssue();
        if (blocking != null) {
            return blocking;
        }
        final String info = getInfoStatusIssue();
        if (info != null) {
            return info;
        }
        return "Ready";
    }

    @Override
    public String getStatusActionName() {
        return null;
    }

    @Override
    public void onStatusAction() {
    }

    @Override
    public boolean isStatusActionWarning() {
        return false;
    }

    @Override
    public boolean isStatusTextWarning() {
        return getBlockingStatusIssue() != null || getInfoStatusIssue() != null;
    }

    /* Default: no warning button. */
    @Override
    public String getStatusWarningButtonTooltip() {
        return null;
    }

    @Override
    public void onStatusWarningButton() {
    }

    /**
     * Default: a solver is ready when it does not require a status action (e.g. "Add Account"/"Configure") and has no blocking problem
     * (see {@link #getBlockingStatusIssue()}). Overridable.
     */
    @Override
    public boolean isReady() {
        return getStatusActionName() == null && getBlockingStatusIssue() == null;
    }

    /** Enabled by the user and ready, see {@link SolverService#isUsable()}. */
    @Override
    public boolean isUsable() {
        return getConfigV3().isEnabled() && isReady();
    }

    protected void initServicePanel(final KeyHandler... handlers) {
        if (org.appwork.utils.Application.isHeadless()) {
            return;
        }
        SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
            @SuppressWarnings("unchecked")
            public void run() {
                for (KeyHandler k : handlers) {
                    k.getEventSender().addListener(new GenericConfigEventListener<Object>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                            ServicePanel.getInstance().requestUpdate(true);
                        }
                    });
                }
            }
        });
    }
}
