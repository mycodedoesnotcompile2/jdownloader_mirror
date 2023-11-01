package org.appwork.console.table;

public class Column<Data> {
    public final String         header;
    public final boolean        visible;
    public final Renderer<Data> renderer;
    public int                  width;
    public boolean              right;
    public int                  minWidth = 0;
    public int                  index;

    public Column(String string, boolean right, Renderer<Data> renderer) {
        this.header = string;
        this.visible = true;
        this.right = right;
        this.renderer = renderer;
    }

    public Column<Data> minWidth(int i) {
        this.minWidth = i;
        return this;
    }
}