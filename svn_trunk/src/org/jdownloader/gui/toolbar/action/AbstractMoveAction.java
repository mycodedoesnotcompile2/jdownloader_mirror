package org.jdownloader.gui.toolbar.action;

import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import jd.gui.swing.jdgui.MainTabbedPane;

import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.event.GUIListener;

public abstract class AbstractMoveAction extends CustomizableAppAction implements GUIListener, PropertyChangeListener {

    protected AppAction delegate;

    public AbstractMoveAction() {
        GUIEventSender.getInstance().addListener(this, true);
        onGuiMainTabSwitch(null, MainTabbedPane.getInstance().getSelectedView());
    }

    @Override
    public void onKeyModifier(int parameter) {
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        delegate.actionPerformed(e);
    }

    protected void setDelegateAction(AppAction moveTopAction) {
        if (delegate != null) {
            delegate.removePropertyChangeListener(this);
        }
        this.delegate = moveTopAction;
        if (delegate != null) {
            setTooltipText(delegate.getTooltipText());
            delegate.addPropertyChangeListener(this);
            setEnabled(delegate.isEnabled());
        } else {
            setEnabled(false);
        }
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getPropertyName() == "enabled") {
            setEnabled((Boolean) evt.getNewValue());
        }
    }

}
