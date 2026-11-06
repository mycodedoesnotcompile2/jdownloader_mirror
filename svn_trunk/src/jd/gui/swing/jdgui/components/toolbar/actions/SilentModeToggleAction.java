package jd.gui.swing.jdgui.components.toolbar.actions;

import java.awt.event.ActionEvent;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.settings.SilentModeController;
import org.jdownloader.settings.SilentModeSettings.SilentModeSetting;

/**
 * Switches the silent mode on and off. Which silent mode is set when the action switches it on (enabled, or one of the automatic ones) is a
 * setting of the action, see {@link #getSwitchedOnMode()}. Every silent mode except {@link SilentModeSetting#DISABLED} counts as "on",
 * clicking the action then switches the silent mode off.
 */
public class SilentModeToggleAction extends AbstractToolBarAction implements GenericConfigEventListener<Object> {
    /** The silent mode which is set when the action switches the silent mode on. */
    public static enum SwitchedOnMode implements LabelInterface {
        ENABLED {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_enabled();
            }

            @Override
            public SilentModeSetting getSetting() {
                return SilentModeSetting.ENABLED;
            }
        },
        AUTO_JD_IN_TRAY {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_autoTray();
            }

            @Override
            public SilentModeSetting getSetting() {
                return SilentModeSetting.AUTO_JD_IN_TRAY;
            }
        },
        AUTO_JD_IN_TASKBAR {
            @Override
            public String getLabel() {
                return _GUI.T.GUISettings_GUISettings_sielntMode_autoTaskbar();
            }

            @Override
            public SilentModeSetting getSetting() {
                return SilentModeSetting.AUTO_JD_IN_TASKBAR;
            }
        };

        public abstract SilentModeSetting getSetting();
    }

    /** Per instance of the action, see the customization of the toolbar and menus. */
    private SwitchedOnMode switchedOnMode = SwitchedOnMode.ENABLED;

    public static String getTranslationForSwitchedOnMode() {
        return _GUI.T.SilentModeToggleAction_setting_switchedOnMode();
    }

    @Customizer(link = "#getTranslationForSwitchedOnMode")
    @Order(100)
    public SwitchedOnMode getSwitchedOnMode() {
        return switchedOnMode;
    }

    public void setSwitchedOnMode(final SwitchedOnMode switchedOnMode) {
        this.switchedOnMode = switchedOnMode == null ? SwitchedOnMode.ENABLED : switchedOnMode;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public SilentModeToggleAction() {
        setIconKey(IconKey.ICON_SILENTMODE);
        /* Static like the name of all actions (shown e.g. in the menu manager), only the tooltip tells the current silent mode. */
        setName(_GUI.T.SilentModeToggleAction_name());
        final KeyHandler keyHandler = SilentModeController.getInstance().getSettingKeyHandler();
        if (!AppAction.isValidationMode()) {
            keyHandler.getEventSender().addListener(this, true);
            update(SilentModeController.getInstance().getSetting());
        }
    }

    /** Short description of the given silent mode for the name and the tooltip. */
    private static String getModeText(final SilentModeSetting setting) {
        switch (setting) {
        case ENABLED:
            return _GUI.T.SilentModeToggleAction_mode_on();
        case AUTO_JD_IN_TRAY:
            return _GUI.T.SilentModeToggleAction_mode_autoTray();
        case AUTO_JD_IN_TASKBAR:
            return _GUI.T.SilentModeToggleAction_mode_autoTaskbar();
        case DISABLED:
        default:
            return _GUI.T.SilentModeToggleAction_mode_off();
        }
    }

    /** The action is selected as soon as any silent mode is set. */
    private void update(final SilentModeSetting setting) {
        final boolean selected;
        switch (setting) {
        case DISABLED:
            selected = false;
            break;
        case ENABLED:
        case AUTO_JD_IN_TASKBAR:
        case AUTO_JD_IN_TRAY:
        default:
            selected = true;
            break;
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                SilentModeToggleAction.super.setSelected(selected);
                /*
                 * The selected state does not change between the enabled and the auto modes and Swing buttons only refresh the tooltip on a
                 * "ShortDescription" property change event, so it has to be fired explicitly.
                 */
                fireToolTipUpdate();
            }
        };
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final SilentModeController controller = SilentModeController.getInstance();
        switch (controller.getSetting()) {
        case DISABLED:
            controller.setSetting(switchedOnMode.getSetting());
            break;
        case ENABLED:
        case AUTO_JD_IN_TASKBAR:
        case AUTO_JD_IN_TRAY:
        default:
            controller.setSetting(SilentModeSetting.DISABLED);
            break;
        }
    }

    @Override
    public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
        update((SilentModeSetting) newValue);
    }

    @Override
    public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
    }

    /**
     * Read whenever the tooltip is shown, so it always tells the silent mode which is set. Whether it is effectively active is not
     * mentioned on purpose: The tooltip is only visible while the user works with a window of JDownloader, which means that the silent mode
     * is not effective at that moment anyway.
     */
    @Override
    protected String createTooltip() {
        return _GUI.T.SilentModeToggleAction_tooltip(getModeText(SilentModeController.getInstance().getSetting()));
    }
}
