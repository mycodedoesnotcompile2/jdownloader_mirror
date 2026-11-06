package org.jdownloader.gui.toolbar.action;

import java.awt.event.ActionEvent;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.actions.AppAction;

public abstract class AbstractToolbarToggleAction extends AbstractToolBarAction implements GenericConfigEventListener<Boolean> {
    private final BooleanKeyHandler keyHandler;

    protected BooleanKeyHandler getKeyHandler() {
        return keyHandler;
    }

    public AbstractToolbarToggleAction(BooleanKeyHandler keyHandler) {
        this.keyHandler = keyHandler;
        if (!AppAction.isValidationMode()) {
            keyHandler.getEventSender().addListener(this, true);
            setSelected(keyHandler.isEnabled());
        }
    }

    public void setSelected(final boolean selected) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                AbstractToolbarToggleAction.super.setSelected(selected);
                fireNameUpdate();
                fireIconUpdate();
                fireToolTipUpdate();
            }
        };
    }

    @Override
    public String getName() {
        if (isSelected()) {
            return getNameWhenEnabled();
        } else {
            return getNameWhenDisabled();
        }
    }

    abstract protected String getNameWhenDisabled();

    abstract protected String getNameWhenEnabled();

    public void actionPerformed(ActionEvent e) {
        boolean sel = !keyHandler.isEnabled();
        keyHandler.setValue(sel);
    }

    public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
    }

    public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
        if (keyHandler == this.keyHandler) {
            setSelected(newValue);
        }
    }
}
