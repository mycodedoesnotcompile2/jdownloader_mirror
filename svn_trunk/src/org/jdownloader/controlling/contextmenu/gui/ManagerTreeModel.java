package org.jdownloader.controlling.contextmenu.gui;

import java.util.ArrayList;
import java.util.List;

import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuItemData.Type;
import org.jdownloader.controlling.contextmenu.SeparatorData;

public class ManagerTreeModel extends DefaultTreeModel implements TreeModel {

    private MenuContainerRoot data;
    private MenuManagerTree   tree;

    public ManagerTreeModel() {
        super(null, false);

    }

    public void set(final MenuContainerRoot menuContainerRoot) {
        final MenuContainerRoot data = menuContainerRoot.clone();
        ManagerTreeModel.this.data = data;
        fireTreeStructureChanged(this, new Object[] { data }, null, null);

    }

    @Override
    public Object getRoot() {
        return data;
    }

    @Override
    public Object getChild(Object parent, int index) {
        return (((MenuItemData) parent).getItems().get(index));
    }

    @Override
    public int getChildCount(Object parent) {
        return ((MenuItemData) parent).getItems() == null ? 0 : ((MenuItemData) parent).getItems().size();
    }

    @Override
    public boolean isLeaf(Object node) {
        return ((MenuItemData) node).getType() != Type.CONTAINER;
    }

    @Override
    public void valueForPathChanged(TreePath path, Object newValue) {

    }

    @Override
    public int getIndexOfChild(Object parent, Object child) {
        return ((MenuItemData) parent).getItems().indexOf(child);
    }

    public void moveTo(TreePath obj, MenuItemData parent, int childIndex) {

        MenuItemData itemToMove = (MenuItemData) obj.getLastPathComponent();
        MenuItemData oldParent = ((MenuItemData) obj.getPathComponent(obj.getPathCount() - 2));
        // oldParent.getItems().remove(itemToMove);
        // int oldIndex = oldParent.getItems().indexOf(itemToMove);
        // if(oldIndex>=0)oldParent.getItems().set(oldIndex, element)
        if (childIndex < 0) {
            // dropped on
            oldParent.getItems().remove(itemToMove);
            parent.getItems().add(itemToMove);
        } else {
            List<MenuItemData> a = new ArrayList<MenuItemData>(parent.getItems().subList(0, childIndex));
            List<MenuItemData> b = new ArrayList<MenuItemData>(parent.getItems().subList(childIndex, parent.getItems().size()));

            if (!a.remove(itemToMove)) {
                b.remove(itemToMove);
            }
            ArrayList<MenuItemData> newlist = new ArrayList<MenuItemData>();

            newlist.addAll(a);
            newlist.add(itemToMove);
            newlist.addAll(b);
            parent.setItems(newlist);
            if (oldParent != parent) {
                oldParent.getItems().remove(itemToMove);
            }
        }
        // data
        fireTreeStructureChanged(this, new Object[] { data }, null, null);

    }

    public void remove(TreePath treePath) {
        if (treePath != null) {
            MenuItemData parent = (MenuItemData) treePath.getPathComponent(treePath.getPathCount() - 2);
            parent.getItems().remove(treePath.getLastPathComponent());
            fireTreeStructureChanged(this, new Object[] { data }, null, null);

        }

    }

    /**
     * Creates a deep copy of the given item (including its settings and, for containers, all sub items) and inserts it directly below the
     * original.
     *
     * @return The path of the copy or null if the item cannot be duplicated (e.g. the root).
     */
    public TreePath duplicate(final TreePath treePath) {
        if (treePath == null || treePath.getPathCount() < 2) {
            return null;
        }
        final MenuItemData original = (MenuItemData) treePath.getLastPathComponent();
        final MenuItemData parent = (MenuItemData) treePath.getPathComponent(treePath.getPathCount() - 2);
        final int index = parent.getItems().indexOf(original);
        if (index < 0) {
            return null;
        }
        /* Serialize and restore: The copy shares no objects (e.g. ActionData setups) with the original */
        final MenuItemData copy = JSonStorage.restoreFromString(JSonStorage.serializeToJson(original), new TypeRef<MenuItemData>() {
        });
        copy._setValidated(true);
        final String title = getTitle(original);
        if (title != null) {
            copy.setName(title + " (copy)");
        }
        parent.getItems().add(index + 1, copy);
        /* Validation replaces generic entries by their actual class instances, so the copy has to be fetched again */
        fireTreeStructureChanged(this, new Object[] { data }, null, null);
        return treePath.getParentPath().pathByAddingChild(parent.getItems().get(index + 1));
    }

    /**
     * Returns the title that is shown for the item in the menu tree, or null for items without a title (separators). Resolved the same way as
     * in the tree renderer: the custom name of the item, otherwise the name of its action.
     */
    private String getTitle(final MenuItemData item) {
        if (item instanceof SeparatorData) {
            return null;
        }
        String name = item.getName();
        if (MenuItemData.isEmptyValue(name)) {
            /* The name is intentionally hidden */
            return null;
        }
        if (StringUtils.isEmpty(name) && !(item instanceof MenuContainer)) {
            try {
                final AppAction action = item.createAction();
                if (action instanceof CustomizableAppAction) {
                    ((CustomizableAppAction) action).requestUpdate(this);
                }
                name = action.getName();
            } catch (final Throwable ignore) {
                /* Fall back to the stored name of the action */
            }
            if (StringUtils.isEmpty(name)) {
                name = item.getActionData().getName();
            }
            if (StringUtils.isEmpty(name) && StringUtils.isNotEmpty(item.getActionData().getClazzName())) {
                final String className = item.getActionData().getClazzName();
                name = className.substring(className.lastIndexOf(".") + 1);
            }
        }
        return StringUtils.isEmpty(name) ? null : name;
    }

    /**
     * "Top" and "bottom" always mean the very top or the very bottom of the whole menu, i.e. the first or last position of the root. Items
     * of submenus are moved out of their submenu. Separators of submenus are not relevant.
     *
     * @return true if the item is not already at that position. The root and separators cannot be moved to the edges, because a menu must
     *         neither start nor end with a separator.
     */
    public boolean canMoveToEdge(final TreePath treePath, final boolean top) {
        if (treePath == null || treePath.getPathCount() < 2) {
            return false;
        }
        final MenuItemData item = (MenuItemData) treePath.getLastPathComponent();
        if (item instanceof SeparatorData) {
            return false;
        }
        final MenuItemData parent = (MenuItemData) treePath.getPathComponent(treePath.getPathCount() - 2);
        if (parent != data) {
            /* Within a submenu: Always can be moved to the top or bottom of the whole menu */
            return true;
        }
        final int index = data.getItems().indexOf(item);
        if (index < 0) {
            return false;
        } else if (top) {
            return index > 0;
        } else {
            return index < data.getItems().size() - 1;
        }
    }

    /**
     * Moves the item to the very top or the very bottom of the whole menu.
     *
     * @return The new path of the item or null if it was not moved.
     */
    public TreePath moveToEdge(final TreePath treePath, final boolean top) {
        if (!canMoveToEdge(treePath, top)) {
            return null;
        }
        final MenuItemData item = (MenuItemData) treePath.getLastPathComponent();
        final MenuItemData parent = (MenuItemData) treePath.getPathComponent(treePath.getPathCount() - 2);
        parent.getItems().remove(item);
        data.getItems().add(top ? 0 : data.getItems().size(), item);
        fireTreeStructureChanged(this, new Object[] { data }, null, null);
        return new TreePath(new Object[] { data, item });
    }

    public TreePath addAction(TreePath treePath, MenuItemData menuItemData) {
        try {
            menuItemData._setValidated(true);
            if (treePath != null && treePath.getLastPathComponent() != data) {
                if (((MenuItemData) treePath.getLastPathComponent()).getType() == Type.CONTAINER) {

                    ((MenuItemData) treePath.getLastPathComponent()).getItems().add(menuItemData);

                    return treePath.pathByAddingChild(menuItemData);
                } else {
                    MenuItemData parent = (MenuItemData) treePath.getPathComponent(treePath.getPathCount() - 2);

                    int index = parent.getItems().indexOf(treePath.getLastPathComponent());

                    parent.getItems().add(index + 1, menuItemData);

                    return treePath.getParentPath().pathByAddingChild(menuItemData);

                }

            } else {
                data.getItems().add(menuItemData);
                return new TreePath(new Object[] { data, menuItemData });
            }
        } finally {

            fireTreeStructureChanged(this, new Object[] { data }, null, null);
        }

    }

    protected void fireTreeStructureChanged(Object source, Object[] path, int[] childIndices, Object[] children) {
        // String before = JSonStorage.toString(data);
        // make sure that we have valid menu structure
        data.validateFull();
        // String after = JSonStorage.toString(data);
        // if (!before.equals(after)) {
        // Dialog.getInstance().showMessageDialog("Menu Structure not allowed!");
        //
        // }
        if (tree != null) {
            TreePath[] paths = tree.getSelectionPaths();

            super.fireTreeStructureChanged(source, path, childIndices, children);

            if (paths != null) {
                tree.setSelectionPaths(paths);
                // tree.expandAll();
            }
            tree.expandAll();
        }
    }

    public void fireUpdate() {
        fireTreeStructureChanged(this, new Object[] { data }, null, null);
    }

    public void setTree(MenuManagerTree extTree) {
        this.tree = extTree;
    }

}
