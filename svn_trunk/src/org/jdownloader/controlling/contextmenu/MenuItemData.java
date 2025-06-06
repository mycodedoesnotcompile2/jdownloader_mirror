package org.jdownloader.controlling.contextmenu;

import java.awt.AWTEvent;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.lang.ref.WeakReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.Timer;

import jd.controlling.TaskQueue;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.Storable;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.actions.AppAction;
import org.jdownloader.actions.ComponentProviderInterface;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.AbstractExtension;
import org.jdownloader.extensions.ExtensionController;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.images.NewTheme;

public class MenuItemData implements Storable {
    public static final String      EMPTY      = "<EMPTY>";
    /* needed for old values */
    private static final String     EMPTY_NAME = "<NO NAME>";
    private ArrayList<MenuItemData> items;
    private String                  name;
    private String                  iconKey;
    private String                  className;
    private ActionData              actionData = new ActionData();

    public String _getIdentifier() {
        ActionData actionData = getActionData();
        if (actionData._isValidDataForCreatingAnAction()) {
            if (actionData.getData() != null) {
                return actionData.getClazzName() + ":" + actionData.getData() + ":" + actionData.getSetup();
            }
            return actionData.getClazzName() + ":" + actionData.getSetup();
        }
        if (getClass() != MenuContainer.class && getClass() != MenuItemData.class) {
            return getClass().getName();
        }
        if (StringUtils.isNotEmpty(className)) {
            return className;
        }
        return getIconKey() + ":" + getName();
    }

    public static boolean isEmptyValue(String value) {
        if (StringUtils.equals(EMPTY, value)) {
            return true;
        } else if (StringUtils.equals(EMPTY_NAME, value)) {
            return true;
        } else {
            return false;
        }
    }

    public String toString() {
        return _getIdentifier() + "";
    }

    public ActionData getActionData() {
        return actionData;
    }

    public void setActionData(ActionData actionData) {
        if (actionData == null) {
            actionData = new ActionData();
        }
        this.actionData = actionData;
    }

    public String getClassName() {
        if (StringUtils.isNotEmpty(className)) {
            return className;
        }
        if (getClass() == MenuItemData.class) {
            return null;
        }
        return getClass().getName();
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public static enum Type {
        ACTION,
        CONTAINER;
    }

    private Type              type    = Type.ACTION;
    private boolean           validated;
    private Exception         validateException;
    private MenuContainerRoot root;
    private String            mnemonic;
    private String            shortcut;
    private boolean           visible = true;

    public void setMnemonic(String mnemonic) {
        this.mnemonic = mnemonic;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public MenuItemData(/* Storable */) {
        items = new ArrayList<MenuItemData>();
    }

    public ArrayList<MenuItemData> getItems() {
        return items;
    }

    public void setItems(ArrayList<MenuItemData> items) {
        this.items = items;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getIconKey() {
        return iconKey;
    }

    public void setIconKey(String iconKey) {
        this.iconKey = iconKey;
    }

    public void add(MenuItemData child) {
        items.add(child);
    }

    public MenuItemData(ActionData actionData) {
        this();
        setActionData(actionData);
    }

    public MenuItemData(Class<? extends AppAction> class1) {
        this(new ActionData(class1));
    }

    public MenuItemData createValidatedItem() throws InstantiationException, IllegalAccessException, ClassNotFoundException, ExtensionNotLoadedException {
        if (className == null || getClass().getName().equals(className)) {
            this._setValidated(true);
            return this;
        }
        MenuItemData ret = createInstance(this);
        ret._setValidated(true);
        ret._setRoot(_getRoot());
        return ret;
    }

    protected MenuItemData createInstance(MenuItemData menuItemData) throws InstantiationException, IllegalAccessException, ClassNotFoundException, ExtensionNotLoadedException {
        if (menuItemData.getClassName() == null) {
            return menuItemData;
        }
        MenuItemData ret = null;
        String packageName = AbstractExtension.class.getPackage().getName();
        if (menuItemData.getClassName().startsWith(packageName)) {
            ret = (MenuItemData) ExtensionController.getInstance().loadClass(menuItemData.getClassName()).newInstance();
        } else {
            // we renamed SeparatorData.class. and we have to fix the stored name here.
            // we probably can remove this workaround in a few weeks/months
            // 29th august 2014
            if ("org.jdownloader.controlling.contextmenu.SeperatorData".equals(menuItemData.getClassName())) {
                menuItemData.setClassName(SeparatorData.class.getName());
                menuItemData.setName("Separator");
            }
            ret = (MenuItemData) Class.forName(menuItemData.getClassName()).newInstance();
        }
        ret.setVisible(menuItemData.isVisible());
        if (getActionData() != null) {
            ret.setActionData(getActionData());
        }
        ret.setIconKey(menuItemData.getIconKey());
        ret.setName(menuItemData.getName());
        ret.setItems(menuItemData.getItems());
        ret.setType(menuItemData.getType());
        return ret;
    }

    public JComponent createItem(MenuBuilder menuBuilder) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, SecurityException, ExtensionNotLoadedException {
        if (getActionData() == null) {
            throw new WTFException("No action:" + this);
        } else if (!getActionData()._isValidDataForCreatingAnAction()) {
            throw new WTFException("No valid action:" + this);
        }
        final CustomizableAppAction action = createAction();
        action.requestUpdate(this);
        if (!isVisible()) {
            return null;
        } else if (!action.isVisible()) {
            return null;
        } else if (action instanceof ComponentProviderInterface) {
            final JComponent ret = ((ComponentProviderInterface) action).createComponent(this);
            if (ret != null) {
                action.addVisibilityPropertyChangeListener(ret);
            }
            return ret;
        }
        final JMenuItem ret = createMenuItem(action, menuBuilder);
        action.addVisibilityPropertyChangeListener(ret);
        if (StringUtils.isNotEmpty(name)) {
            ret.setText(name);
        }
        if (StringUtils.isNotEmpty(iconKey)) {
            ret.setIcon(getIcon(iconKey, 20));
        }
        return ret;
    }

    protected boolean processHideOnClickMouseEvent(final JMenuItem item, final boolean hideOnClick, final MouseEvent ev) {
        if (!hideOnClick && ev.getID() == MouseEvent.MOUSE_RELEASED) {
            item.setSelected(!item.isSelected());
            for (final ActionListener al : item.getActionListeners()) {
                int modifiers = 0;
                final AWTEvent currentEvent = EventQueue.getCurrentEvent();
                if (currentEvent instanceof InputEvent) {
                    modifiers = ((InputEvent) currentEvent).getModifiers();
                } else if (currentEvent instanceof ActionEvent) {
                    modifiers = ((ActionEvent) currentEvent).getModifiers();
                }
                al.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, item.getActionCommand(), EventQueue.getMostRecentEventTime(), modifiers));
                final Action action = item.getAction();
                if (action instanceof CustomizableAppAction) {
                    TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
                        @Override
                        protected Void run() throws RuntimeException {
                            new Timer(1000, new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    if (item.isVisible()) {
                                        ((CustomizableAppAction) action).requestUpdate(ev);
                                    }
                                }
                            }).start();
                            return null;
                        }
                    });
                }
            }
            return true;
        } else {
            return false;
        }
    }

    protected JMenuItem createMenuItem(final CustomizableAppAction action, final MenuBuilder menuBuilder) {
        // see ExtMenuItem to avoid popup close
        final JMenuItem ret;
        if (action.isToggle()) {
            ret = new JCheckBoxMenuItem(action) {

                @Override
                protected void processMouseEvent(final MouseEvent e) {
                    if (!MenuItemData.this.processHideOnClickMouseEvent(this, menuBuilder == null || menuBuilder.isHideOnClick(), e)) {
                        super.processMouseEvent(e);
                    }
                }
            };
        } else {
            ret = new JMenuItem(action) {

                @Override
                protected void processMouseEvent(final MouseEvent e) {
                    if (!MenuItemData.this.processHideOnClickMouseEvent(this, menuBuilder == null || menuBuilder.isHideOnClick(), e)) {
                        super.processMouseEvent(e);
                    }
                }

            };
        }
        ret.getAccessibleContext().setAccessibleName(action.getName());
        ret.getAccessibleContext().setAccessibleDescription(action.getTooltipText());
        return ret;
    }

    protected WeakReference<Constructor<?>> weakConstructor = new WeakReference<Constructor<?>>(null);

    public CustomizableAppAction createAction() throws ClassNotFoundException, NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ExtensionNotLoadedException {
        if (!validated) {
            //
            throw new WTFException();
        }
        if (actionData == null) {
            //
            throw new WTFException("No ACTION");
        }
        Constructor<?> c = weakConstructor.get();
        if (c == null) {
            final Class<?> clazz = actionData._getClazz();
            c = clazz.getConstructor();
            weakConstructor = new WeakReference<Constructor<?>>(c);
        }
        final CustomizableAppAction ret = (CustomizableAppAction) c.newInstance();
        ret.setMenuItemData(this);
        ret.applyMenuItemData();
        ret.initContextDefaults();
        ret.loadContextSetups();
        return (ret);
    }

    public JComponent addTo(JComponent root, MenuBuilder menuBuilder) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, SecurityException, ExtensionNotLoadedException {
        if (!isVisible()) {
            return null;
        } else {
            final JComponent it = createItem(menuBuilder);
            if (it == null) {
                return null;
            } else {
                root.add(it);
                return it;
            }
        }
    }

    public List<MenuItemData> list() {
        List<MenuItemData> set = new ArrayList<MenuItemData>();
        set.add(this);
        if (getItems() != null) {
            for (MenuItemData d : getItems()) {
                set.addAll(d.list());
            }
        }
        return set;
    }

    public List<List<MenuItemData>> listPaths() {
        List<List<MenuItemData>> set = new ArrayList<List<MenuItemData>>();
        ArrayList<MenuItemData> newPath = new ArrayList<MenuItemData>();
        newPath.add(this);
        set.add(newPath);
        if (getItems() != null) {
            for (MenuItemData d : getItems()) {
                for (List<MenuItemData> p : d.listPaths()) {
                    newPath = new ArrayList<MenuItemData>();
                    newPath.add(this);
                    newPath.addAll(p);
                    set.add(newPath);
                }
            }
        }
        return set;
    }

    public String _getDescription() {
        if (getActionData()._isValidDataForCreatingAnAction()) {
            try {
                return createAction().getTooltipText();
            } catch (Exception e) {
            }
        }
        return null;
    }

    public String getMnemonic() {
        return mnemonic;
    }

    public Collection<String> _getItemIdentifiers() {
        HashSet<String> ret = new HashSet<String>();
        for (MenuItemData mid : getItems()) {
            ret.add(mid._getIdentifier());
        }
        return ret;
    }

    public void _setValidated(boolean b) {
        validated = true;
    }

    public boolean _isValidated() {
        return validated;
    }

    public void _setValidateException(Exception e) {
        this.validateException = e;
    }

    public Exception _getValidateException() {
        return validateException;
    }

    public MenuContainerRoot _getRoot() {
        return root;
    }

    public void _setRoot(MenuContainerRoot root) {
        this.root = root;
    }

    public String getShortcut() {
        return shortcut;
    }

    public void setShortcut(String shortcut) {
        this.shortcut = shortcut;
    }

    // public String _getShortcut() {
    // if (StringUtils.isNotEmpty(shortcut)) { return shortcut; }
    // if (getActionData() != null) {
    // try {
    // return createAction(null).getShortCutString();
    // } catch (Exception e) {
    //
    // }
    // }
    // return null;
    // }
    public boolean isVisible() {
        return visible;
    }

    public void setVisible(boolean b) {
        visible = b;
    }

    public static Icon getIcon(String key, int size) {
        if (StringUtils.isEmpty(key)) {
            return null;
        }
        if (MenuItemData.isEmptyValue(key)) {
            return null;
        }
        return NewTheme.I().getIcon(key, size);
    }
}
