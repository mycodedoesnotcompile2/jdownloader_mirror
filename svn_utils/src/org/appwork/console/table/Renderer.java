package org.appwork.console.table;

import java.util.List;

public interface Renderer<Data> {
    Object getString(Data d, int row, Column<Data> column);

    Object getSubHeader(List<Data> rows, Column<Data> column);

    Object getFooter(List<Data> rows, Column<Data> column);

    int compare(Data o1, Data o2, Column<Data> c);
}