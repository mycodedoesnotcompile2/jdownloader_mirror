package org.jdownloader.extensions;

import java.awt.event.ActionEvent;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.jdownloader.actions.AppAction;

public abstract class AbstractExtensionGuiEnableAction<T extends AbstractExtension<?, ?>> extends AbstractExtensionAction<T> implements GenericConfigEventListener<Boolean> {
    /**
     *
     */
    private static final long   serialVersionUID = 1L;
    protected BooleanKeyHandler keyHandler;

    public AbstractExtensionGuiEnableAction(BooleanKeyHandler guiEnabled) {
        super();
        keyHandler = guiEnabled;
        if (!AppAction.isValidationMode()) {
            setSelected(keyHandler.isEnabled());
            keyHandler.getEventSender().addListener(this, true);
        }
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        // activate panel
        if (_getExtension().getGUI().isActive()) {
            _getExtension().getGUI().setActive(false);
        } else {
            _getExtension().getGUI().setActive(true);
            // bring panel to front
            _getExtension().getGUI().toFront();
        }
        setSelected(_getExtension().getGUI().isActive());
    }

    @Override
    public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
        setSelected(newValue);
    }

}
