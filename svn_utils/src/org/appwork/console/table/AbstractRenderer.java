package org.appwork.console.table;

import java.util.List;

public abstract class AbstractRenderer<Data> implements Renderer<Data> {
    @Override
    public int compare(Data o1, Data o2, Column<Data> c) {
        return 0;
    }

    @Override
    public Object getSubHeader(List<Data> rows, Column<Data> c) {
        return "";
    }

    @Override
    public Object getFooter(List<Data> rows, Column<Data> c) {
        return "";
    }
}