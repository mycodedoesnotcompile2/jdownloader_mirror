package org.appwork.console.table;

public class ToStringRenderer extends AbstractRenderer<Object[]> {
    @Override
    public Object getString(Object[] d, int row, Column<Object[]> column) {
        return String.valueOf(d[column.index]);
    }

    @Override
    public int compare(Object[] o1, Object[] o2, Column<Object[]> c) {
        return String.valueOf(this.getString(o1, 0, c)).compareTo(String.valueOf(this.getString(o2, 0, c)));
    }
}
