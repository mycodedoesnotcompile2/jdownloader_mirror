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

import java.awt.event.MouseEvent;
import java.util.EventObject;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.swing.exttable.ExtTableModel;

public abstract class ExtCompoundColumn<T> extends ExtColumn<T> implements CellEditorListener {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private ExtColumn<T>      editor;
    private T                 editing;
    private ExtColumn<T>      renderer;

    public ExtCompoundColumn(final String name) {
        this(name, null);
    }

    /**
     * @param name
     * @param table
     */
    public ExtCompoundColumn(final String name, final ExtTableModel<T> table) {
        super(name, table);
        setRowSorter(new ExtDefaultRowSorter<T>() {
            @Override
            public int compare(final T o1, final T o2) {
                String o1s = ExtCompoundColumn.this.getSortString(o1);
                String o2s = ExtCompoundColumn.this.getSortString(o2);
                if (o1s == null) {
                    o1s = "";
                }
                if (o2s == null) {
                    o2s = "";
                }
                if (getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return o1s.compareTo(o2s);
                } else {
                    return o2s.compareTo(o1s);
                }
            }
        });
    }

    @Override
    public void configureEditorComponent(final T value, final boolean isSelected, final int row, final int column) {
        this.editor.configureEditorComponent(value, isSelected, row, column);
    }

    @Override
    public void configureRendererComponent(final T value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.renderer.configureRendererComponent(value, isSelected, hasFocus, row, column);
    }

    @Override
    public void editingCanceled(final ChangeEvent e) {
        cancelCellEditing();
    }

    @Override
    public void editingStopped(final ChangeEvent e) {
        stopCellEditing();
    }

    @Override
    public Object getCellEditorValue() {
        return this.editor.getCellEditorValue();
    }

    /**
     * return true if you dont want to forward to following onDoubleClick listener
     *
     * This method will be called when a doubleclick is performed on the object <code>obj</code>
     *
     * @param obj
     */
    public boolean onDoubleClick(final MouseEvent e, final T obj) {
        return selectColumn(obj).onDoubleClick(e, obj);
    }

    /**
     * This method will be called if the user does a windows typic rename click order. Means: click on a already selected single row
     *
     * @param e
     * @param obj
     * @return
     */
    public boolean onRenameClick(final MouseEvent e, final T obj) {
        return selectColumn(obj).onRenameClick(e, obj);
    }

    public boolean onSingleClick(final MouseEvent e, final T obj) {
        return selectColumn(obj).onSingleClick(e, obj);
    }

    @Override
    public JComponent getEditorComponent(final T value, final boolean isSelected, final int row, final int column) {
        this.editing = value;
        this.editor = this.selectColumn(this.editing);
        if (this.editor.getModel() != getModel()) {
            this.editor.setModel(getModel());
            editor.setTableColumn(getTableColumn(), false);
        }
        this.editor.removeCellEditorListener(this);
        this.editor.addCellEditorListener(this);
        return this.editor.getEditorComponent(value, isSelected, row, column);
    }

    @Override
    public JComponent getRendererComponent(final T value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.renderer = this.selectColumn(value);
        if (this.renderer.getModel() != getModel()) {
            this.renderer.setModel(getModel());
            renderer.setTableColumn(getTableColumn(), false);
        }
        return this.renderer.getRendererComponent(value, isSelected, hasFocus, row, column);
    }

    @Override
    public ExtDefaultRowSorter<T> getRowSorter() {
        return super.getRowSorter();
    }

    /**
     * @param o1
     * @return
     */
    public abstract String getSortString(T o1);

    @Override
    protected String getTooltipText(final T obj) {
        // TODO Auto-generated method stub
        return super.getTooltipText(obj);
    }

    @Override
    public boolean isCellEditable(final EventObject evt) {
        if (evt instanceof MouseEvent) {
            final ExtTable<T> table = getModel().getTable();
            // final int col =
            // table.columnAtPoint(((MouseEvent)evt).getPoint());
            final int row = table.getRowIndexByPoint(((MouseEvent) evt).getPoint());
            // final int modelIndex =
            // table.getColumnModel().getColumn(col).getModelIndex();
            // JComponent edit =
            // this.getEditorComponent(getModel().getElementAt(row), true, row,
            // modelIndex);
            final ExtColumn<T> edit = this.selectColumn(getModel().getElementAt(row));
            return ((MouseEvent) evt).getClickCount() >= edit.getClickcount() && edit.getClickcount() > 0;
        }
        return true;
    }

    @Override
    public boolean isEditable(final T obj) {
        return this.selectColumn(obj).isEditable(obj);
    }

    @Override
    public boolean isEnabled(final T obj) {
        return this.selectColumn(obj).isEnabled(obj);
    }

    @Override
    public boolean isSortable(final T obj) {
        return false;
    }

    @Override
    public boolean matchSearch(final T object, final Pattern pattern) {
        return this.selectColumn(object).matchSearch(object, pattern);
    }

    @Override
    public void resetEditor() {
        this.editor.resetEditor();
    }

    @Override
    public void resetRenderer() {
        this.renderer.resetRenderer();
    }

    /**
     * @param object
     * @return
     */
    abstract public ExtColumn<T> selectColumn(T object);

    // @Override
    // public void setModel(final ExtTableModel<T> model) {
    // super.setModel(model);
    // }
    @Override
    public void setValue(final Object value, final T object) {
        this.editor.setValue(value, object);
    }
}
