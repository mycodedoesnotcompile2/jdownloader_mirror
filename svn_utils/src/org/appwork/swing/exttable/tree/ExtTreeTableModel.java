/**
 * 
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany   
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 * 	
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header. 	
 * 	
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the 
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 * 	
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.exttable.tree;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTableModel;

/**
 * @author Thomas
 * 
 */
public abstract class ExtTreeTableModel<T extends TreeNodeInterface> extends ExtTableModel<T> {

    private List<T> tree;

    public List<T> getTree() {
        return tree;
    }

    /**
     * @param id
     */
    public ExtTreeTableModel(final String id) {
        super(id);

    }

    public void setTreeData(final ArrayList<T> newtableData, final boolean refreshSOrt) {
        final ArrayList<T> list = new ArrayList<T>();
        for (final T node : newtableData) {
            unpack(list, node);
        }
        this.tree = newtableData;
        _fireTableStructureChanged(list, refreshSOrt);
    }

    @Override
    public void addAllElements(final Collection<T> entries) {
        throw new WTFException("Not Implemented");
    }

    @Override
    public void addAllElements(final T... files) {
        throw new WTFException("Not Implemented");
    }

    @Override
    public void addElement(final T at) {
        throw new WTFException("Not Implemented");
    }

    @Override
    public List<T> getElements() {
        // TODO Auto-generated method stub
        return super.getElements();
    }

    @Override
    public void refreshSort() {
        // TODO Auto-generated method stub
        super.refreshSort();
    }

    public boolean move(final T group, int i, final List<T> imports) {

        try {

            final List<T> children = (List<T>) (group == null ? tree : group.getChildren());

            if (i < 0) {
                i = children.size();
            }

            final java.util.List<T> newdata = new ArrayList<T>(children.size());
            final List<T> before = new ArrayList<T>(children.subList(0, i));
            final List<T> after = new ArrayList<T>(children.subList(i, children.size()));
            internalRemove(before, new HashSet<T>(imports));
            internalRemove(after, new HashSet<T>(imports));
            internalRemove(tree, new HashSet<T>(imports));
            newdata.addAll(before);
            newdata.addAll(imports);
            newdata.addAll(after);
            for (final T t : imports) {
                t.setParent(group);
            }
            children.clear();
            children.addAll(newdata);

            this.setTreeData((ArrayList<T>) tree, true);
            return true;
        } catch (final Throwable t) {
            t.printStackTrace();

        }
        return false;
    }

    public T getParentByRow(final int dropRow) {
        for (int i = dropRow - 1; i >= 0; i--) {
            final T rowObject = getObjectbyRow(i);
            if (!rowObject.isLeaf()) { return rowObject; }
        }
        return null;
    }

    public TreePosition<T> getTreePositionByRow(final int dropRow) {
        final T beforeElement = getObjectbyRow(dropRow - 1);
        final T afterElement = getObjectbyRow(dropRow);
        if (beforeElement == null) { return new TreePosition<T>(null, 0); }
        T parent = (T) beforeElement.getParent();
        if (afterElement != null && afterElement.getParent() == beforeElement) {
            parent = beforeElement;
        }
        int index = 0;
        for (int i = dropRow - 1; i >= 0; i--) {
            final T rowObject = getObjectbyRow(i);
            if (rowObject.getParent() == parent) {
                index++;
                // if (!rowObject.isLeaf()) { return new
                // TreePosition<T>(rowObject,index); }
            } else {
                break;
            }

        }
        return new TreePosition<T>(parent, index);

    }

    /**
     * @param ai
     */
    public void remove(final T ai) {
        final HashSet<T> del = new HashSet<T>();
        del.add(ai);
        internalRemove(tree, del);
        setTreeData((ArrayList<T>) tree, true);
    }

    public TreePosition<T> getTreePositionByObject(final T parent) {
        int counter = -1;
        for (int i = getTableData().size() - 1; i >= 0; i--) {
            final T obj = getTableData().get(i);
            if (parent == obj) {
                counter = 0;
                // final TreePosition<T> ret = getTreePositionByRow(i);
                //
                // return new TreePosition<T>(ret.getParent(), ret.getIndex());
            } else {
                if (parent.getParent() == obj.getParent() && counter >= 0) {
                    counter++;
                }
            }
        }
        if (counter < 0) { return null; }
        return new TreePosition<T>((T) parent.getParent(), counter);

    }

    public int getIndexByRow(final int dropRow) {
        int index = 0;
        for (int i = dropRow - 1; i >= 0; i--) {
            final T rowObject = getObjectbyRow(i);
            if (!rowObject.isLeaf()) { return index; }
            index++;
        }
        return -1;
    }

    /**
     * @param children
     * @param hashSet
     */
    @SuppressWarnings("unchecked")
    private void internalRemove(final List<T> children, final HashSet<T> hashSet) {
        for (final Iterator<T> it = children.iterator(); it.hasNext();) {

            final T node = it.next();

            if (hashSet.contains(node)) {
                it.remove();
                continue;
            }
            if (!node.isLeaf()) {
                internalRemove((List<T>) node.getChildren(), hashSet);

            }
        }

    }

    @Override
    public List<T> refreshSort(final List<T> data) {
        // TODO Auto-generated method stub
        return super.refreshSort(data);
    }

    public boolean move(final java.util.List<T> transferData, final int dropRow) {

        throw new WTFException("Not Implemented");

    }

    @Override
    public void removeAll(final List<T> selectedObjects) {

        internalRemove(tree, new HashSet<T>(selectedObjects));
    }

    @Override
    public List<T> sort(final List<T> data, final ExtColumn<T> column) {
        return data;
    }

    public void _fireTableStructureChanged(final List<T> newtableData, final boolean refreshSort) {

        super._fireTableStructureChanged(newtableData, refreshSort);

    }

    protected void unpack(final ArrayList<T> list, final T node) {
        list.add(node);
        if (!node.isLeaf()) {
            for (final TreeNodeInterface t : node.getChildren()) {
                unpack(list, (T) t);
            }
        }
    }

    /**
     * @param imports
     * @return
     * @return
     */
    public static <T extends TreeNodeInterface> List<T> getAllChildren(final List<T> imports) {
        final ArrayList<T> ret = new ArrayList<T>();
        fillWithChildren(ret, imports);
        return ret;
    }

    private static <T extends TreeNodeInterface> void fillWithChildren(final List<T> ret, final List<T> imports) {
        for (final T t : imports) {
            if (t.isLeaf()) {
                ret.add(t);
            } else {
                fillWithChildren(ret, (List<T>) t.getChildren());
            }
        }
    }

    public static <T extends TreeNodeInterface> List<T> getAllParents(final List<T> imports) {
        final ArrayList<T> ret = new ArrayList<T>();
        fillWithParents(ret, imports);
        return ret;
    }

    private static <T extends TreeNodeInterface> void fillWithParents(final List<T> ret, final List<T> imports) {
        for (final T t : imports) {
            if (!t.isLeaf()) {
                ret.add(t);
                fillWithParents(ret, (List<T>) t.getChildren());
            }
        }
    }

}
