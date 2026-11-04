package org.jdownloader.settings;

import org.appwork.storage.config.handler.EnumKeyHandler;
import org.appwork.utils.Application;
import org.appwork.utils.swing.EDTHelper;
import org.jdownloader.crosssystem.idlegetter.IdleGetter;
import org.jdownloader.settings.SilentModeSettings.SilentModeSetting;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

import jd.gui.swing.jdgui.JDGui;

/**
 * Central access to the silent mode: Asks whether the silent mode is effectively active right now (the setting in
 * {@link SilentModeSettings#getSilentMode()} is only the wish of the user, e.g. it does not count while a window of JDownloader has the
 * focus) and why. Everything that has to behave differently during the silent mode should ask here and nowhere else.
 */
public final class SilentModeController {
    /** Whether the silent mode is effectively active right now, and why. */
    public enum State {
        /** The silent mode is not active. */
        INACTIVE,
        /**
         * The user switched the silent mode on (see {@link SilentModeSettings.SilentModeSetting#ENABLED}), and no window of JDownloader is
         * in use.
         */
        ACTIVE_MANUAL,
        /**
         * The silent mode was switched on automatically: JDownloader is in the tray/taskbar (see
         * {@link SilentModeSettings.SilentModeSetting}), the user has been idle for too long, or JDownloader runs without a graphical user
         * interface (headless), where nobody sits in front of it.
         */
        ACTIVE_AUTO
    }

    private static final SilentModeController INSTANCE = new SilentModeController();

    public static SilentModeController getInstance() {
        return INSTANCE;
    }

    private SilentModeController() {
    }

    /** The last reason that was logged for the state, see {@link #logReason(String)}. */
    private volatile String lastLoggedReason = null;

    /**
     * Logs why the silent mode is active or not, but only if the reason changed since the last call: {@link #getState()} is asked very
     * often (e.g. for every captcha and by the solver table), a log line per call would flood the log.
     */
    private void logReason(final String reason) {
        if (!reason.equals(lastLoggedReason)) {
            lastLoggedReason = reason;
            JDGui.getInstance().getLogger().info("SilentMode: " + reason);
        }
    }

    /**
     * Whether the silent mode is effectively active right now. In headless mode we assume that there is no user in front of the computer:
     * {@link State#ACTIVE_AUTO}.
     */
    public State getState() {
        if (Application.isHeadless()) {
            return State.ACTIVE_AUTO;
        }
        final long maxIdleTime = CFG_SILENTMODE.CFG.getAutoSilentModeInIdleState();
        if (maxIdleTime > 0 && IdleGetter.getInstance().getIdleTimeSinceLastUserInput() >= maxIdleTime) {
            logReason("Active: Idle state reached");
            return State.ACTIVE_AUTO;
        }
        final JDGui gui = JDGui.getInstance();
        final State state = new EDTHelper<State>() {
            @Override
            public State edtRun() {
                /* Nothing is blocked while the user works with JDownloader, no matter what is set. */
                final String inUseReason = gui.getInUseReason();
                if (inUseReason != null) {
                    logReason("Not active: " + inUseReason);
                    return State.INACTIVE;
                }
                switch (getSetting()) {
                case ENABLED:
                    logReason("Active: Enabled manually");
                    return State.ACTIVE_MANUAL;
                case AUTO_JD_IN_TASKBAR:
                    if (gui.isMinimizedToTaskbar()) {
                        logReason("Active: Automatically because JDownloader is in the taskbar");
                        return State.ACTIVE_AUTO;
                    }
                    logReason("Not active: JDownloader is not in the taskbar");
                    return State.INACTIVE;
                case AUTO_JD_IN_TRAY:
                    if (gui.isHiddenInTray()) {
                        logReason("Active: Automatically because JDownloader is in the tray");
                        return State.ACTIVE_AUTO;
                    }
                    logReason("Not active: JDownloader is not in the tray");
                    return State.INACTIVE;
                case DISABLED:
                default:
                    logReason("Not active: Silent mode is disabled");
                    return State.INACTIVE;
                }
            }
        }.getReturnValue();
        /* The EDT call can return null, e.g. if the gui is not initialized (yet): Not active then. */
        return state == null ? State.INACTIVE : state;
    }

    /** The silent mode which is set (the wish of the user), see {@link #getState()} for whether it is effectively active. */
    public SilentModeSetting getSetting() {
        return CFG_SILENTMODE.CFG.getSilentMode();
    }

    /** Sets the silent mode. */
    public void setSetting(final SilentModeSetting setting) {
        CFG_SILENTMODE.CFG.setSilentMode(setting);
    }

    /** The key handler of the silent mode setting, e.g. for listeners and settings components. */
    public EnumKeyHandler getSettingKeyHandler() {
        return CFG_SILENTMODE.SILENT_MODE;
    }

    /**
     * True if the user switched the silent mode on in the settings (manually or automatically). Unlike {@link #isActive()} this does not
     * depend on the current state, e.g. it is also true while a window of JDownloader has the focus.
     */
    public boolean isEnabledInSettings() {
        switch (getSetting()) {
        case ENABLED:
        case AUTO_JD_IN_TASKBAR:
        case AUTO_JD_IN_TRAY:
            return true;
        case DISABLED:
        default:
            return false;
        }
    }

    /** True if the silent mode is effectively active right now, see {@link #getState()}. */
    public boolean isActive() {
        return getState() != State.INACTIVE;
    }
}
