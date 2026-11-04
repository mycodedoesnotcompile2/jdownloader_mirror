package org.jdownloader.settings;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultLongValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.gui.translate._GUI;

public interface SilentModeSettings extends ConfigInterface {
    /** The silent mode setting, see {@link SilentModeController#getState()} for whether it is effectively active. */
    public static enum SilentModeSetting implements LabelInterface {
        DISABLED {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_disabled();
            }
        },
        ENABLED {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_enabled();
            }
        },
        AUTO_JD_IN_TRAY {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_autoTray();
            }
        },
        AUTO_JD_IN_TASKBAR {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_autoTaskbar();
            }
        };
    }

    void setSilentMode(SilentModeSetting mode);

    @DefaultEnumValue("DISABLED")
    @DescriptionForConfigEntry("Silent Mode: Disabled, enabled or enabled automatically depending on the state of the JDownloader window")
    @AboutConfig
    SilentModeSetting getSilentMode();

    public static enum DialogDuringSilentModeAction implements LabelInterface {
        WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS {
            @Override
            public String getLabel() {
                return _GUI.T.SilentModeSettings_dialogAction_waitUntilFocus();
            }
        },
        WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT {
            @Override
            public String getLabel() {
                return _GUI.T.SilentModeSettings_dialogAction_waitUntilFocusOrTimeout();
            }
        },
        CANCEL_DIALOG {
            @Override
            public String getLabel() {
                return _GUI.T.SilentModeSettings_dialogAction_cancel();
            }
        };
    }

    void setOnDialogDuringSilentModeAction(DialogDuringSilentModeAction action);

    @DefaultEnumValue("WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT")
    @AboutConfig
    DialogDuringSilentModeAction getOnDialogDuringSilentModeAction();

    void setAutoResetOnStartupEnabled(boolean b);

    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Auto Disable the Silent mode on each startup")
    @AboutConfig
    boolean isAutoResetOnStartupEnabled();

    void setOnDialogDuringSilentModeActionTimeout(long b);

    @DefaultLongValue(30 * 1000l)
    @AboutConfig
    long isOnDialogDuringSilentModeActionTimeout();

    public static enum CaptchaDuringSilentModeAction implements LabelInterface {
        SKIP_LINK {
            @Override
            public String getLabel() {
                return _GUI.T.SilentModeSettings_captchaAction_skip();
            }
        },
        WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT {
            @Override
            public String getLabel() {
                return _GUI.T.SilentModeSettings_captchaAction_waitUntilFocusOrTimeout();
            }
        },
        DISABLE_DIALOG_SOLVER {
            @Override
            public String getLabel() {
                return _GUI.T.SilentModeSettings_captchaAction_autoSolversOnly();
            }
        };
    }

    void setOnCaptchaDuringSilentModeAction(CaptchaDuringSilentModeAction action);

    @DefaultEnumValue("DISABLE_DIALOG_SOLVER")
    @AboutConfig
    CaptchaDuringSilentModeAction getOnCaptchaDuringSilentModeAction();

    @DescriptionForConfigEntry("[ms] After the given time without user interaction, JDownloader will autoactivate the silent mode, and disable it if there is user interaction again.")
    @DefaultLongValue(0)
    @AboutConfig
    long getAutoSilentModeInIdleState();

    void setAutoSilentModeInIdleState(long ms);

}
