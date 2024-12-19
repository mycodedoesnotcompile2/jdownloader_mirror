package org.jdownloader.actions;

import java.awt.Toolkit;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.WeakReference;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.KeyStroke;

import org.appwork.swing.action.BasicAction;
import org.appwork.utils.images.IconIO;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;

/**
 * This abstract class is the parent class for all actions in JDownloader
 *
 * @author thomas
 *
 */
public abstract class AppAction extends BasicAction {

    protected String           iconKey;

    protected int              size;

    private boolean            visible = true;

    public static final String VISIBLE = "visible";

    public AppAction() {
        super();

    }

    /**
     * e.g. KeyEvent.VK_Q, InputEvent.CTRL_DOWN_MASK
     *
     * @param vkQ
     * @param ctrlDownMask
     */
    public void setAccelerator(int vkQ, int ctrlDownMask) {
        setAccelerator(KeyStroke.getKeyStroke(vkQ, ctrlDownMask));
    }

    /**
     * KeyEvent.VK_Q
     *
     * @param vkQ
     */
    public void setAccelerator(int vkQ) {
        setAccelerator(vkQ, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
    }

    public void setIconKey(String iconKey) {
        this.iconKey = iconKey;
        setIconSizes(18);

    }

    public String getIconKey() {
        return iconKey;
    }

    public AppAction setIconSizes(int size) {
        this.size = size;
        return this;
    }

    public Object getValue(String key) {
        if (iconKey != null && LARGE_ICON_KEY.equalsIgnoreCase(key)) {
            return NewTheme.I().getIcon(iconKey, size);
        } else if (iconKey != null && SMALL_ICON.equalsIgnoreCase(key)) {
            //
            return NewTheme.I().getIcon(iconKey, size);
        }
        return super.getValue(key);
    }

    /**
     * invalid actions should not be used.
     *
     * @return
     */
    public boolean isVisible() {
        return visible;
    }

    public void addVisibilityPropertyChangeListener(final JComponent component) {
        final WeakReference<JComponent> weakComponent = new WeakReference<JComponent>(component);
        addPropertyChangeListener(new PropertyChangeListener() {

            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                final JComponent component = weakComponent.get();
                if (component == null) {
                    removePropertyChangeListener(this);
                    return;
                }
                if (evt.getPropertyName() == AppAction.VISIBLE) {
                    final boolean newVisible = Boolean.TRUE.equals(evt.getNewValue());
                    if (newVisible != component.isVisible()) {
                        component.setVisible(newVisible);
                        component.firePropertyChange(AppAction.VISIBLE, !newVisible, newVisible);
                    }
                }
            }
        });
    }

    public void setVisible(final boolean newValue) {
        if (visible == newValue) {
            return;
        }
        final boolean oldValue = visible;
        this.visible = newValue;
        firePropertyChange(VISIBLE, Boolean.valueOf(oldValue), Boolean.valueOf(newValue));
    }

    public Icon getIcon(int size) {
        Icon actionIcon = null;
        String iconKey = getIconKey();
        if (NewTheme.I().hasIcon(iconKey)) {
            actionIcon = (new AbstractIcon(iconKey, size));
        } else {
            Icon icon = (Icon) getValue(Action.LARGE_ICON_KEY);
            if (icon == null) {
                icon = getSmallIcon();
                if (icon != null) {
                    actionIcon = (IconIO.getScaledInstance(icon, size, size));
                }
            }
        }
        return actionIcon;

    }
}
