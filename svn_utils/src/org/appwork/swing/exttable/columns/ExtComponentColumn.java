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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.JComponent;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.utils.swing.EDTHelper;

public abstract class ExtComponentColumn<T> extends ExtColumn<T> {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private MouseAdapter      listener;

    /**
     * @param name
     * @param table
     */
    public ExtComponentColumn(final String name) {
        super(name, null);

        this.listener = new MouseAdapter() {

            private int col = -1;
            private int row = -1;

            @Override
            public void mouseMoved(final MouseEvent e) {

                final ExtTable<T> table = ExtComponentColumn.this.getModel().getTable();
                if (table.getColumnModel().getColumnCount() > 0) {
                    final int col = table.columnAtPoint(e.getPoint());
                    final int row = table.getRowIndexByPoint(e.getPoint());

                    int modelIndex = table.getColumnModel().getColumn(col).getModelIndex();
                    final int editing = table.getEditingColumn();
                    if (col != this.col || row != this.row) {
                        if (ExtComponentColumn.this.getModel().getExtColumnByModelIndex(modelIndex) == ExtComponentColumn.this) {
                            if (editing == col && table.getEditingRow() == row) {
                                /*
                                 * we are still in same cell, no need to change
                                 * anything
                                 */
                            } else {
                                modelIndex = table.getColumnModel().getColumn(editing).getModelIndex();
                                if (ExtComponentColumn.this.getModel().getExtColumnByModelIndex(modelIndex) == ExtComponentColumn.this) {
                                    /*
                                     * we are no longer in our editing column,
                                     * stop cell editing
                                     */
                                    ExtComponentColumn.this.stopCellEditing();
                                } else if (editing > 0) {
                                    /* stop another column from editing */
                                    ExtComponentColumn.this.getModel().getExtColumnByModelIndex(modelIndex).stopCellEditing();
                                }
                                /*
                                 * invoke later is important as we first have to
                                 * stopCellEditing and then put new cell into
                                 * editing mode
                                 */
                                new EDTHelper<Void>() {

                                    @Override
                                    public Void edtRun() {
                                        ExtComponentColumn.this.onCellUpdate(col, row);
                                        return null;
                                    }
                                }.start(true);
                            }
                        } else {
                            modelIndex = table.getColumnModel().getColumn(editing).getModelIndex();
                            if (ExtComponentColumn.this.getModel().getExtColumnByModelIndex(modelIndex) == ExtComponentColumn.this) {
                                /*
                                 * we are no longer in our editing column, stop
                                 * cell editing
                                 */
                                ExtComponentColumn.this.stopCellEditing();
                            }
                        }
                        this.col = col;
                        this.row = row;
                    }
                }
            }
        };
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.table.ExtColumn#getCellEditorValue()
     */
    @Override
    public Object getCellEditorValue() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public JComponent getEditorComponent(final T value, final boolean isSelected, final int row, final int column) {
        return this.getInternalEditorComponent(value, isSelected, row, column);
    }

    /**
     * @param value
     * @param isSelected
     * @param row
     * @param column
     * @return
     */
    abstract protected JComponent getInternalEditorComponent(T value, boolean isSelected, int row, int column);

    /**
     * @param value
     * @param isSelected
     * @param hasFocus
     * @param row
     * @param column
     * @return
     */
    /**
     * @param value
     * @param isSelected
     * @param row
     * @param column
     * @return
     */

    abstract protected JComponent getInternalRendererComponent(T value, boolean isSelected, boolean hasFocus, int row, int column);

    @Override
    public final JComponent getRendererComponent(final T value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        if (this.listener != null) {
            this.getModel().getTable().addMouseMotionListener(this.listener);
            this.listener = null;
        }
        return this.getInternalRendererComponent(value, isSelected, hasFocus, row, column);
    }

    @Override
    public boolean isCellEditable(final EventObject evt) {
        if (evt instanceof MouseEvent) { return false; }
        return true;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.table.ExtColumn#isEditable(java.lang.Object)
     */
    @Override
    public boolean isEditable(final T obj) {
        // TODO Auto-generated method stub
        return true;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.table.ExtColumn#isEnabled(java.lang.Object)
     */
    @Override
    public boolean isEnabled(final T obj) {
        // TODO Auto-generated method stub
        return true;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.table.ExtColumn#isSortable(java.lang.Object)
     */
    @Override
    public boolean isSortable(final T obj) {
        // TODO Auto-generated method stub
        return false;
    }

    /**
     * @param col
     * @param row
     */
    protected void onCellUpdate(final int col, final int row) {
        this.getModel().getTable().editCellAt(row, col);

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.table.ExtColumn#setValue(java.lang.Object,
     * java.lang.Object)
     */
    @Override
    public void setValue(final Object value, final T object) {
        // TODO Auto-generated method stub

    }

}
