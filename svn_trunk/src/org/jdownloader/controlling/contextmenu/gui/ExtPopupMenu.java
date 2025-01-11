package org.jdownloader.controlling.contextmenu.gui;

import java.awt.Component;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Action;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import org.jdownloader.actions.AppAction;

public class ExtPopupMenu extends JPopupMenu implements ExtMenuInterface {
    protected final PropertyChangeListener propertyChangeListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            if (evt.getPropertyName() == "enabled") {
                refreshEnabledState(Boolean.TRUE.equals(evt.getNewValue()));
            }
            if (AppAction.VISIBLE == evt.getPropertyName()) {
                refreshJSeparatorVisibility();
            }
        }
    };
    protected final ContainerListener      containerListener      = new ContainerListener() {
        @Override
        public void componentRemoved(ContainerEvent e) {
            final Component child = e.getChild();
            child.removePropertyChangeListener(propertyChangeListener);
            if (isEnabled()) {
                refreshEnabledState(false);
            }
            refreshJSeparatorVisibility();
        }

        @Override
        public void componentAdded(ContainerEvent e) {
            final Component child = e.getChild();
            child.addPropertyChangeListener(propertyChangeListener);
            if (!isEnabled() && child.isEnabled()) {
                refreshEnabledState(true);
            }
            if (touchesJSeparator(child)) {
                refreshJSeparatorVisibility();
            }
        }
    };

    protected boolean touchesJSeparator(Component com) {
        if (com instanceof JSeparator) {
            return true;
        }
        final int index = getComponentIndex(com);
        if (index == -1) {
            return false;
        }
        final Component before = getComponent(Math.max(0, index - 1));
        if (before instanceof JSeparator) {
            return true;
        }
        final Component after = getComponent(Math.min(getComponentCount() - 1, index + 1));
        if (after instanceof JSeparator) {
            return true;
        }
        return false;
    }

    public ExtPopupMenu() {
        addContainerListener(containerListener);
        setEnabled(false);
    }

    protected void refreshEnabledState(boolean enabled) {
        if (enabled) {
            setEnabled(true);
        } else {
            boolean isEnabled = false;
            for (final Component elem : getComponents()) {
                if (elem.isEnabled()) {
                    isEnabled = true;
                    break;
                }
            }
            setEnabled(isEnabled);
        }
    }

    protected void refreshJSeparatorVisibility() {
        Component lastVisibleElem = null;
        boolean lastVisible = false;
        boolean revalidate = false;
        for (final Component elem : getComponents()) {
            final boolean isElemVisible = elem.isVisible();
            if (elem instanceof JSeparator) {
                if (lastVisibleElem instanceof JSeparator) {
                    elem.setVisible(false);
                    revalidate |= isElemVisible != false;
                    lastVisible = false;
                } else {
                    elem.setVisible(lastVisible);
                    revalidate |= isElemVisible != lastVisible;
                }
            } else {
                lastVisible = isElemVisible;
            }
            if (lastVisible) {
                lastVisibleElem = elem;
            }
        }
        if (revalidate) {
            revalidate();
        }
    }

    protected JMenuItem createActionComponent(final Action a) {
        final JMenuItem ret;
        if (((AppAction) a).isToggle()) {
            ret = new JCheckBoxMenuItem(a);
        } else {
            ret = super.createActionComponent(a);
        }
        if (a instanceof AppAction) {
            ((AppAction) a).addVisibilityPropertyChangeListener(ret);
        }
        return ret;
    }

    @Override
    public void cleanup() {
        while (getComponentCount() > 0 && (getComponent(getComponentCount() - 1)) instanceof JSeparator) {
            remove(getComponentCount() - 1);
        }
        if (getComponentCount() == 0) {
            setEnabled(false);
        }
    }
}
