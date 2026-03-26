package org.appwork.swing.exttable.columns;

import java.text.NumberFormat;
import java.util.Currency;

import javax.swing.JComponent;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.DebugMode;
import org.appwork.utils.swing.renderer.RenderLabel;

public abstract class ExtCurrencyColumn<E> extends ExtColumn<E> {
    private static final long serialVersionUID = 3468695684952592990L;
    private final RenderLabel renderer;
    private NumberFormat      formatter;

    public ExtCurrencyColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);
        this.renderer = new RenderLabel();
        formatter = updateNumberFormat();
        this.setRowSorter(new ExtDefaultRowSorter<E>() {
            @Override
            public int compare(final Object o1, final Object o2) {
                if (ExtCurrencyColumn.this.getValue(o1) == ExtCurrencyColumn.this.getValue(o2)) {
                    return 0;
                }
                if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return ExtCurrencyColumn.this.getValue(o1) > ExtCurrencyColumn.this.getValue(o2) ? -1 : 1;
                } else {
                    return ExtCurrencyColumn.this.getValue(o1) < ExtCurrencyColumn.this.getValue(o2) ? -1 : 1;
                }
            }
        });
    }

    @Override
    protected NumberFormat getDefaultNumberFormat() {
        return NumberFormat.getCurrencyInstance();
    }

    @Override
    protected NumberFormat updateNumberFormat() {
        return formatter = super.updateNumberFormat();
    }

    abstract protected Currency getCurrency(E value);

    protected String getText(final E value) {
        try {
            final Currency currency = getCurrency(value);
            final NumberFormat fmt = (NumberFormat) this.formatter.clone();
            if (currency != null) {
                fmt.setCurrency(currency);
            }
            return fmt.format(this.getValue(value) / 100.0f);
        } catch (final Exception e) {
            DebugMode.debugger();
            return e.getMessage();
        }
    }

    @Override
    public void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.renderer.setText(getText(value));
    }

    @Override
    public Object getCellEditorValue() {
        return null;
    }

    abstract protected long getValue(Object o);

    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return null;
    }

    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.renderer;
    }

    @Override
    public boolean isEditable(final Object obj) {
        return false;
    }

    @Override
    public boolean isEnabled(final Object obj) {
        return true;
    }

    @Override
    public boolean isSortable(final Object obj) {
        return true;
    }

    @Override
    public void resetEditor() {
    }

    @Override
    public void resetRenderer() {
        this.renderer.setOpaque(false);
        this.renderer.setBorder(ExtColumn.DEFAULT_BORDER);
    }

    @Override
    public void setValue(final Object value, final Object object) {
    }
}