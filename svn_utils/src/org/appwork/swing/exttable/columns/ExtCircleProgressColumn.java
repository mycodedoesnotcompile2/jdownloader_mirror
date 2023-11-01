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
package org.appwork.swing.exttable.columns;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.border.CompoundBorder;
import javax.swing.event.TableModelEvent;

import org.appwork.swing.components.circlebar.CircledProgressBar;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;

abstract public class ExtCircleProgressColumn<E> extends ExtColumn<E> {
    public class IndeterminatedCircledProgressBar extends CircledProgressBar {

        /**
         * 
         */
        private static final long serialVersionUID = 1L;
        /**
         * 
         */

        private long              timer            = 0;
        private long              cleanupTimer     = 0;

        @Override
        public boolean isDisplayable() {
            return true;
        }

        @Override
        public boolean isVisible() {
            return false;
        }

        @Override
        public void repaint() {
            final ExtTableModel<E> mod = ExtCircleProgressColumn.this.getModel();
            if (mod != null && mod.getTable() != null && mod.getTable().isShowing()) {

                // cleanup map in case we removed a indeterminated value
                if (System.currentTimeMillis() - this.cleanupTimer > 30000) {
                    Entry<E, Long> next;
                    for (final Iterator<Entry<E, Long>> it = ExtCircleProgressColumn.this.map.entrySet().iterator(); it.hasNext();) {
                        next = it.next();
                        final long lastUpdate = System.currentTimeMillis() - next.getValue();
                        if (lastUpdate > 5000) {
                            it.remove();
                        }
                    }

                    this.cleanupTimer = System.currentTimeMillis();
                    if (ExtCircleProgressColumn.this.map.size() == 0) {
                        ExtCircleProgressColumn.this.indeterminatedRenderer.setIndeterminate(false);
                        return;
                    }

                }
                if (System.currentTimeMillis() - this.timer > 1000 / ExtCircleProgressColumn.this.getFps() && ExtCircleProgressColumn.this.columnIndex >= 0) {
                    final List<E> selection = mod.getSelectedObjects();

                    mod.fireTableChanged(new TableModelEvent(mod, 0, Integer.MAX_VALUE, ExtCircleProgressColumn.this.columnIndex, TableModelEvent.UPDATE));
                    mod.setSelectedObjects(selection);
                    this.timer = System.currentTimeMillis();
                }

            }

        }

    }

    private static final long    serialVersionUID = -2473320164484034664L;
    protected CircledProgressBar determinatedRenderer;
    protected CompoundBorder     defaultBorder;
    private CircledProgressBar   indeterminatedRenderer;
    protected CircledProgressBar renderer;
    private HashMap<E, Long>     map;

    private int                  columnIndex      = -1;

    /**
     * 
     */
    public ExtCircleProgressColumn(final String title) {
        this(title, null);
    }

    public ExtCircleProgressColumn(final String name, final ExtTableModel<E> extModel) {
        super(name, extModel);
        this.determinatedRenderer = new CircledProgressBar() {

            /**
             * 
             */
            private static final long serialVersionUID = -5119741526005648827L;

            @Override
            public boolean isVisible() {
                return false;
            }
        };

        this.indeterminatedRenderer = new IndeterminatedCircledProgressBar();
        this.map = new HashMap<E, Long>();

        // this.getModel().addTableModelListener(new TableModelListener() {
        //
        // @Override
        // public void tableChanged(final TableModelEvent e) {
        // switch (e.getType()) {
        // case TableModelEvent.DELETE:
        // case TableModelEvent.UPDATE:
        // System.out.println(e);
        // if (ExtProgressColumn.this.map.size() == 0) {
        // ExtProgressColumn.this.indeterminatedRenderer.setIndeterminate(false);
        // }
        // }
        //
        // }
        // });
        this.renderer = this.determinatedRenderer;
        this.defaultBorder = BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1), this.determinatedRenderer.getBorder());
        this.setRowSorter(new ExtDefaultRowSorter<E>() {

            @Override
            public int compare(final E o1, final E o2) {
                final double v1 = (double) ExtCircleProgressColumn.this.getValue(o1) / ExtCircleProgressColumn.this.getMax(o1);
                final double v2 = (double) ExtCircleProgressColumn.this.getValue(o2) / ExtCircleProgressColumn.this.getMax(o2);

                if (v1 == v2) { return 0; }
                if (this.getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                    return v1 > v2 ? -1 : 1;
                } else {
                    return v2 > v1 ? -1 : 1;
                }
            }

        });
    }

    @Override
    public void configureEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        // TODO Auto-generated method stub

    }

    @Override
    public void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        if (this.renderer == this.determinatedRenderer) {

            // Normalize value and maxvalue to fit in the integer range
            long v = this.getValue(value);
            long m = this.getMax(value);
            final double factor = Math.max(v / (double) Integer.MAX_VALUE, m / (double) Integer.MAX_VALUE);

            if (factor >= 1.0) {
                v /= factor;
                m /= factor;
            }
            // take care to set the maximum before the value!!
            this.renderer.setMaximum((int) m);
            this.renderer.setValue((int) v);

            this.renderer.setString(this.getString(value));

        } else {
            this.renderer.setString(this.getString(value));
            if (!this.renderer.isIndeterminate()) {
                this.renderer.setIndeterminate(true);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.rapidshare.rsmanager.gui.components.table.ExtColumn#getCellEditorValue
     * ()
     */
    @Override
    public Object getCellEditorValue() {

        return null;
    }

    /**
     * @return
     */
    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return null;
    }

    /**
     * @return
     */
    protected int getFps() {
        return 20;
    }

    protected long getMax(final E value) {
        return 100;
    }

    /**
     * @return
     */
    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.columnIndex = column;
        if (this.isIndeterminated(value, isSelected, hasFocus, row, column)) {
            this.renderer = this.indeterminatedRenderer;
            if (this.map.size() == 0) {
                this.indeterminatedRenderer.setIndeterminate(true);
            }
            this.map.put(value, System.currentTimeMillis());

        } else {
            this.renderer = this.determinatedRenderer;
            this.map.remove(value);
            if (this.map.size() == 0) {
                this.indeterminatedRenderer.setIndeterminate(false);
            }
        }
        return this.renderer;
    }

    abstract protected String getString(E value);

    @Override
    protected String getTooltipText(final E value) {

        return this.getString(value);
    }

    abstract protected long getValue(E value);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.rapidshare.rsmanager.gui.components.table.ExtColumn#isEditable(java
     * .lang.Object)
     */
    @Override
    public boolean isEditable(final E obj) {

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.rapidshare.rsmanager.gui.components.table.ExtColumn#isEnabled(java
     * .lang.Object)
     */
    @Override
    public boolean isEnabled(final E obj) {

        return true;
    }

    /**
     * @param column
     * @param row
     * @param hasFocus
     * @param isSelected
     * @param value
     * @return
     */
    protected boolean isIndeterminated(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.rapidshare.rsmanager.gui.components.table.ExtColumn#isSortable(java
     * .lang.Object)
     */
    @Override
    public boolean isSortable(final E obj) {

        return true;
    }

    @Override
    public void resetEditor() {
        // TODO Auto-generated method stub

    }

    @Override
    public void resetRenderer() {
        this.renderer.setOpaque(false);
        this.renderer.setStringPainted(true);
        this.renderer.setBorder(this.defaultBorder);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.rapidshare.rsmanager.gui.components.table.ExtColumn#setValue(java
     * .lang.Object, java.lang.Object)
     */
    @Override
    public void setValue(final Object value, final E object) {

    }

}
