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

import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JComponent;
import javax.swing.JSpinner;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.JSpinner.NumberEditor;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.reflection.Clazz;

public abstract class ExtSpinnerColumn<E> extends ExtTextColumn<E> {
    /**
     *
     */
    private static final long        serialVersionUID = 1L;
    private final JSpinner           editor;
    private final NumberEditor       intEditor;
    private final NumberEditor       longEditor;
    private final NumberEditor       byteEditor;
    private final NumberEditor       shortEditor;
    private final NumberEditor       doubleEditor;
    private final NumberEditor       floatEditor;
    private final SpinnerNumberModel intModel;
    private final SpinnerNumberModel longModel;
    private final SpinnerNumberModel byteModel;
    private final SpinnerNumberModel shortModel;
    private final SpinnerNumberModel doubleModel;
    private final SpinnerNumberModel floatModel;
    private FocusListener            tableFocusLost;

    public ExtSpinnerColumn(final String name) {
        this(name, null);
    }

    public ExtSpinnerColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);
        this.intModel = new SpinnerNumberModel(0, Integer.MIN_VALUE, Integer.MAX_VALUE, 1);
        this.longModel = new SpinnerNumberModel(Long.valueOf(0l), Long.valueOf(Long.MIN_VALUE), Long.valueOf(Long.MAX_VALUE), Long.valueOf(1l));
        this.byteModel = new SpinnerNumberModel(Byte.valueOf((byte) 0), Byte.valueOf(Byte.MIN_VALUE), Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 1));
        this.shortModel = new SpinnerNumberModel(Short.valueOf((short) 0), Short.valueOf(Short.MIN_VALUE), Short.valueOf(Short.MAX_VALUE), Short.valueOf((short) 1));
        this.doubleModel = new SpinnerNumberModel(Double.valueOf(0.0d), Double.valueOf(-Double.MAX_VALUE), Double.valueOf(Double.MAX_VALUE), Double.valueOf(1.0d));
        this.floatModel = new SpinnerNumberModel(Float.valueOf(0.0f), Float.valueOf(-Float.MAX_VALUE), Float.valueOf(Float.MAX_VALUE), Float.valueOf(1.0f));
        this.editor = new JSpinner();
        this.intEditor = new JSpinner.NumberEditor(new JSpinner(intModel), "#");
        this.longEditor = new JSpinner.NumberEditor(new JSpinner(longModel), "#");
        this.byteEditor = new JSpinner.NumberEditor(new JSpinner(byteModel), "#");
        this.shortEditor = new JSpinner.NumberEditor(new JSpinner(shortModel), "#");
        this.doubleEditor = new JSpinner.NumberEditor(new JSpinner(doubleModel), "###.##");
        this.floatEditor = new JSpinner.NumberEditor(new JSpinner(floatModel), "###.##");
        this.intEditor.getTextField().addActionListener(this);
        this.longEditor.getTextField().addActionListener(this);
        this.byteEditor.getTextField().addActionListener(this);
        this.shortEditor.getTextField().addActionListener(this);
        this.doubleEditor.getTextField().addActionListener(this);
        this.floatEditor.getTextField().addActionListener(this);
        tableFocusLost = new FocusListener() {
            @Override
            public void focusLost(FocusEvent e) {
                stopCellEditing();
            }

            @Override
            public void focusGained(FocusEvent e) {
                // TODO Auto-generated method stub
            }
        };
        this.setRowSorter(new ExtDefaultRowSorter<E>() {
            @Override
            public int compare(final E o1, final E o2) {
                final float _1 = ExtSpinnerColumn.this.getNumber(o1).floatValue();
                final float _2 = ExtSpinnerColumn.this.getNumber(o2).floatValue();
                if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return _1 == _2 ? 0 : _1 < _2 ? -1 : 1;
                } else {
                    return _1 == _2 ? 0 : _1 > _2 ? -1 : 1;
                }
            }
        });
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        this.stopCellEditing();
        super.actionPerformed(e);
    }

    @Override
    public void configureEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        final Number n = this.getNumber(value);
        this.editor.setModel(this.getModel(value, n));
        this.editor.setEditor(this.getEditor(value, n, this.editor));
        this.editor.setValue(n);
    }

    public NumberEditor getByteEditor() {
        return this.byteEditor;
    }

    public SpinnerNumberModel getByteModel() {
        return this.byteModel;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.table.ExtColumn#getCellEditorValue()
     */
    @Override
    public Object getCellEditorValue() {
        // TODO Auto-generated method stub
        return this.editor.getValue();
    }

    public NumberEditor getDoubleEditor() {
        return this.doubleEditor;
    }

    public SpinnerNumberModel getDoubleModel() {
        return this.doubleModel;
    }

    /**
     * @param value
     * @param n
     * @param editor2
     * @return
     */
    protected DefaultEditor getEditor(final E value, final Number n, final JSpinner editor2) {
        if (Clazz.isDouble(n.getClass())) {
            return this.getDoubleEditor();
        } else if (Clazz.isFloat(n.getClass())) {
            return this.getFloatEditor();
        } else if (Clazz.isLong(n.getClass())) {
            return this.getLongEditor();
        } else if (Clazz.isInteger(n.getClass())) {
            return this.getIntEditor();
        } else if (Clazz.isShort(n.getClass())) {
            return this.getShortEditor();
        } else if (Clazz.isByte(n.getClass())) {
            return this.getByteEditor();
        } else {
            return this.getLongEditor();
        }
    }

    /**
     * @return
     */
    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return this.editor;
    }

    public NumberEditor getFloatEditor() {
        return this.floatEditor;
    }

    public SpinnerNumberModel getFloatModel() {
        return this.floatModel;
    }

    public NumberEditor getIntEditor() {
        return this.intEditor;
    }

    public SpinnerNumberModel getIntModel() {
        return this.intModel;
    }

    public NumberEditor getLongEditor() {
        return this.longEditor;
    }

    public SpinnerNumberModel getLongModel() {
        return this.longModel;
    }

    /**
     * @param value
     * @param b
     *            TODO
     * @return
     */
    protected SpinnerNumberModel getModel(final E value, final Number n) {
        if (Clazz.isDouble(n.getClass())) {
            return this.getDoubleModel();
        } else if (Clazz.isFloat(n.getClass())) {
            return this.getFloatModel();
        } else if (Clazz.isLong(n.getClass())) {
            return this.getLongModel();
        } else if (Clazz.isInteger(n.getClass())) {
            return this.getIntModel();
        } else if (Clazz.isShort(n.getClass())) {
            return this.getShortModel();
        } else if (Clazz.isByte(n.getClass())) {
            return this.getByteModel();
        } else {
            return this.getLongModel();
        }
    }

    /**
     * @param value
     * @return
     */
    abstract protected Number getNumber(final E value);

    /**
     * @return
     */
    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.rendererField;
    }

    public NumberEditor getShortEditor() {
        return this.shortEditor;
    }

    public SpinnerNumberModel getShortModel() {
        return this.shortModel;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.table.ExtColumn#isEditable(java.lang.Object)
     */
    @Override
    public boolean isEditable(final E obj) {
        if (tableFocusLost != null) {
            // spinner buttons to not throw a focus lost event, so we stop
            // ediing if table los focus
            getModel().getTable().addFocusListener(tableFocusLost);
            tableFocusLost = null;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.table.ExtColumn#isEnabled(java.lang.Object)
     */
    @Override
    public boolean isEnabled(final E obj) {
        // TODO Auto-generated method stub
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.table.ExtColumn#isSortable(java.lang.Object)
     */
    @Override
    public boolean isSortable(final E obj) {
        // TODO Auto-generated method stub
        return true;
    }

    @Override
    public void resetEditor() {
        this.editor.setBorder(null);
    }

    @Override
    public void resetRenderer() {
        super.resetRenderer();
        this.rendererField.setHorizontalAlignment(SwingConstants.RIGHT);
    }

    /**
     * @param value
     * @param object
     */
    abstract protected void setNumberValue(Number value, E object);

    @Override
    final public void setValue(final Object value, final E object) {
        // TODO Auto-generated method stub
        this.setNumberValue((Number) value, object);
    }
}
