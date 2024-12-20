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
                                                                              if (Boolean.TRUE.equals(evt.getNewValue())) {
                                                                                  setEnabled(true);
                                                                              } else if (Boolean.FALSE.equals(evt.getNewValue())) {
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
                                                                          if (AppAction.VISIBLE == evt.getPropertyName()) {
                                                                              boolean lastVisible = false;
                                                                              for (final Component elem : getComponents()) {
                                                                                  if (elem instanceof JSeparator) {
                                                                                      elem.setVisible(lastVisible);
                                                                                  } else {
                                                                                      lastVisible = elem.isVisible();
                                                                                  }
                                                                              }
                                                                              revalidate();
                                                                          }
                                                                      }
                                                                  };

    protected final ContainerListener      containerListener      = new ContainerListener() {

                                                                      @Override
                                                                      public void componentRemoved(ContainerEvent e) {
                                                                          final Component child = e.getChild();
                                                                          child.removePropertyChangeListener(propertyChangeListener);
                                                                          propertyChangeListener.propertyChange(new PropertyChangeEvent(child, "enabled", true, false));
                                                                      }

                                                                      @Override
                                                                      public void componentAdded(ContainerEvent e) {
                                                                          final Component child = e.getChild();
                                                                          child.addPropertyChangeListener(propertyChangeListener);
                                                                          final boolean enabled = child.isEnabled();
                                                                          propertyChangeListener.propertyChange(new PropertyChangeEvent(child, "enabled", !enabled, enabled));
                                                                          if (child instanceof JSeparator) {
                                                                              final int componentCount = getComponentCount();
                                                                              if (componentCount == 1) {
                                                                                  remove(child);
                                                                              } else if (componentCount > 1 && getComponent(componentCount - 2) instanceof JSeparator) {
                                                                                  remove(child);
                                                                              }
                                                                          }
                                                                      }
                                                                  };

    public ExtPopupMenu() {
        addContainerListener(containerListener);
        setEnabled(false);
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
