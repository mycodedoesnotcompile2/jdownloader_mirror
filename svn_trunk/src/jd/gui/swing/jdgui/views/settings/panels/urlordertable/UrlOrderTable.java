package jd.gui.swing.jdgui.views.settings.panels.urlordertable;

import javax.swing.DropMode;

import jd.gui.swing.jdgui.BasicJDTable;

import org.appwork.swing.exttable.ExtTransferHandler;
import org.jdownloader.settings.UrlDisplayEntry;

public class UrlOrderTable extends BasicJDTable<UrlDisplayEntry> {

    public UrlOrderTable() {
        super(new UrlOrderTableModel());
        setTransferHandler(new ExtTransferHandler<UrlDisplayEntry>());
        this.setDragEnabled(true);
        this.setDropMode(DropMode.ON_OR_INSERT_ROWS);
    }

    @Override
    protected boolean isDisplayFullsizeAlways() {
        return true;
    }
}
