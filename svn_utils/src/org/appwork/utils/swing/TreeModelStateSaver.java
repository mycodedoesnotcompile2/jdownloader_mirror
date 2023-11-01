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
package org.appwork.utils.swing;

import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.JTree;
import javax.swing.tree.TreePath;



/**
 * This class is used to save the selection states and expanded states of each
 * tree node before rebuilding the tree. After rebuilding, the model is able to
 * restore these states.
 * 
 * @author $Author: unknown$
 * 
 */
public class TreeModelStateSaver {

    protected JTree                        tree;
    /**
     * Stores for each node the expanded state
     */
    private final HashMap<Object, Boolean> expandCache;

    /**
     * treePath for internal use
     */
    private TreePath                       treePath;

    /**
     * Stores all selected Paths
     */
    protected TreePath[]                   selectedPaths;

    /**
     * @param tree
     */
    public TreeModelStateSaver(final JTree tree) {
        this.tree = tree;
        this.expandCache = new HashMap<Object, Boolean>();
    }

    /**
     * @return the expandCache
     */
    public HashMap<Object, Boolean> getExpandCache() {
        return this.expandCache;
    }

    /**
     * @return the {@link TreeModelStateSaver#selectedPaths}
     * @see TreeModelStateSaver#selectedPaths
     */
    public TreePath[] getSelectedPaths() {
        return this.selectedPaths;
    }

    /**
     * Restore the saved tree state
     */
    public void restore() {

        new EDTHelper<Object>() {

            @Override
            public Object edtRun() {
                try {
                    if (TreeModelStateSaver.this.tree.getModel() == null) {

                    return null; }
                    ;

                    TreeModelStateSaver.this.restoreState(TreeModelStateSaver.this.tree.getModel().getRoot(), new ArrayList<Object>());

                    final TreePath[] selectedPathes = TreeModelStateSaver.this.getSelectedPaths();
                    if (selectedPathes != null && selectedPathes.length > 0) {
                        TreeModelStateSaver.this.tree.getSelectionModel().clearSelection();
                        TreeModelStateSaver.this.tree.getSelectionModel().setSelectionPaths(selectedPathes);

                    }
                } catch (final Throwable e) {
                    org.appwork.loggingv3.LogV3.log(e);
                }
                return null;
            }

        }.start();
    }

    protected void restoreState(final Object node, final java.util.List<Object> path) {
        new EDTHelper<Object>() {

            @Override
            public Object edtRun() {
                if (node == null) { return null; }
                path.add(node);

                TreeModelStateSaver.this.treePath = new TreePath(path.toArray(new Object[] {}));
                final Boolean bo = TreeModelStateSaver.this.expandCache.get(node);
                try {
                    if (bo != null && bo.booleanValue()) {
                        TreeModelStateSaver.this.tree.expandPath(TreeModelStateSaver.this.treePath);
                    }
                } catch (final Throwable e) {
                    org.appwork.loggingv3.LogV3.log(e);
                }

                for (int i = 0; i < TreeModelStateSaver.this.tree.getModel().getChildCount(node); i++) {
                    try {
                        TreeModelStateSaver.this.restoreState(TreeModelStateSaver.this.tree.getModel().getChild(node, i), new ArrayList<Object>(path));
                    } catch (final Throwable e) {
                        org.appwork.loggingv3.LogV3.log(e);
                    }
                }
                return null;
            }

        }.start();

    }

    /**
     * Save the current state of the tree
     */
    public void save() {
        if (this.tree.getModel() != null) {
            this.saveState(this.tree.getModel().getRoot(), new ArrayList<Object>());
        }
        this.selectedPaths = this.tree.getSelectionPaths();
    }

    /**
     * Saves the expaned states of each node to cacheMap runs rekursive
     * 
     * @param root
     */
    private void saveState(final Object node, final java.util.List<Object> path) {
        path.add(node);
        try {
            this.treePath = new TreePath(path.toArray(new Object[] {}));
            this.expandCache.put(node, this.tree.isExpanded(this.treePath));
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);

        }

        final int max = this.tree.getModel().isLeaf(node) ? 0 : this.tree.getModel().getChildCount(node);
        for (int i = 0; i < max; i++) {
            try {
                this.saveState(this.tree.getModel().getChild(node, i), new ArrayList<Object>(path));
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
            this.tree.getModel().getChildCount(node);
        }
    }

    /**
     * @param selectedPathes
     *            the selectedPathes to set
     */
    public void setSelectedPaths(final TreePath[] selectedPathes) {
        this.selectedPaths = selectedPathes;
    }

}
