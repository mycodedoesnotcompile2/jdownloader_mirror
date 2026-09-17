package org.appwork.swing.exttable.columns;

import java.util.Currency;

import javax.swing.JComponent;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.DebugMode;
import org.appwork.utils.formatter.CurrencyFormatter;
import org.appwork.utils.swing.renderer.RenderLabel;

public abstract class ExtCurrencyColumn<E> extends ExtColumn<E> {
    private static final long serialVersionUID = 3468695684952592990L;
    private final RenderLabel renderer;

    public ExtCurrencyColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);
        this.renderer = new RenderLabel();
        this.setRowSorter(new ExtDefaultRowSorter<E>() {
            @Override
            public int compare(final E o1, final E o2) {
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

    abstract protected Currency getCurrency(E value);

    protected String getText(final E value) {
        try {
            /* getValue returns the amount in cents, so divide by 100 to get the actual monetary amount. */
            return CurrencyFormatter.format(this.getValue(value) / 100.0d, getCurrency(value));
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

    abstract protected long getValue(E o);

    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return null;
    }

    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.renderer;
    }

    @Override
    public boolean isEditable(final E obj) {
        return false;
    }

    @Override
    public boolean isEnabled(final E obj) {
        return true;
    }

    @Override
    public boolean isSortable(final E obj) {
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
    public void setValue(final Object value, final E object) {
    }
}