package org.jdownloader.controlling.contextmenu.gui;

import java.awt.Component;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import org.jdownloader.actions.AppAction;

public class ExtMenuImpl extends JMenu implements ExtMenuInterface {
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

    public ExtMenuImpl(String name) {
        super(name);
        setEnabled(false);
    }

    protected boolean touchesJSeparator(Component com) {
        if (com instanceof JSeparator) {
            return true;
        }
        final JPopupMenu popup = getPopupMenu();
        if (popup == null) {
            return false;
        }
        final int index = popup.getComponentIndex(com);
        if (index == -1) {
            return false;
        }
        final Component before = getMenuComponent(Math.max(0, index - 1));
        if (before instanceof JSeparator) {
            return true;
        }
        final Component after = getMenuComponent(Math.min(popup.getComponentCount() - 1, index + 2));
        if (after instanceof JSeparator) {
            return true;
        }
        return false;
    }

    protected void refreshEnabledState(boolean enabled) {
        if (enabled) {
            setEnabled(true);
        } else {
            boolean isEnabled = false;
            for (final Component elem : getMenuComponents()) {
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
        for (final Component elem : getMenuComponents()) {
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

    private static final long serialVersionUID = 1L;

    @Override
    protected WinListener createWinListener(JPopupMenu p) {
        p.addContainerListener(containerListener);
        return super.createWinListener(p);
    }

    @Override
    public void cleanup() {
        while (getMenuComponentCount() > 0 && (getMenuComponent(getMenuComponentCount() - 1)) instanceof JSeparator) {
            remove(getMenuComponentCount() - 1);
        }
        if (getMenuComponentCount() == 0) {
            setEnabled(false);
        }
    }
};
