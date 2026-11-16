package org.jdownloader.gui.views.components.packagetable.columns;

import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JPopupMenu;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

import org.appwork.swing.exttable.ExtTable;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.packagetable.ExtractToResolver;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModel;
import org.jdownloader.gui.views.downloads.columns.FileColumn;

/**
 * Read only column that shows where the archives of a link or package are extracted to.<br>
 * The values are calculated in the background by {@link ExtractToResolver}, the column only reads the cache and is therefore safe to be
 * painted and sorted in the EDT.
 */
public class ExtractToColumn extends ExtTextColumn<AbstractNode> {
    private static final long   serialVersionUID = 1L;
    private final AtomicBoolean updateScheduled  = new AtomicBoolean(false);
    private final Runnable      onUpdated        = new Runnable() {
                                                     @Override
                                                     public void run() {
                                                         onResultsUpdated();
                                                     }
                                                 };

    public ExtractToColumn() {
        super(_GUI.T.ExtractToColumn_name());
    }

    @Override
    public JPopupMenu createHeaderPopup() {
        return FileColumn.createColumnPopup(this, getMinWidth() == getMaxWidth() && getMaxWidth() > 0);
    }

    @Override
    public boolean isDefaultVisible() {
        return false;
    }

    @Override
    public boolean isEditable(final AbstractNode obj) {
        return false;
    }

    @Override
    public boolean isSortable(final AbstractNode obj) {
        return true;
    }

    @Override
    public boolean isEnabled(final AbstractNode obj) {
        if (obj instanceof AbstractPackageNode) {
            return ((AbstractPackageNode) obj).getView().isEnabled();
        }
        return obj.isEnabled();
    }

    @Override
    public String getStringValue(final AbstractNode value) {
        final ExtractToResolver.Result result = ExtractToResolver.getInstance().get(value, isColumnVisible(), onUpdated);
        if (result == null) {
            return null;
        }
        return result.getText();
    }

    /**
     * Values are only calculated while the column is visible (also sorting or searching must not start calculations for a hidden column).
     */
    private boolean isColumnVisible() {
        final ExtTableModel<AbstractNode> model = getModel();
        return model != null && model.isColumnVisible(this);
    }

    @Override
    protected String getTooltipText(final AbstractNode obj) {
        final ExtractToResolver.Result result = ExtractToResolver.getInstance().get(obj, isColumnVisible(), onUpdated);
        if (result == null) {
            return null;
        }
        return result.getTooltip();
    }

    /**
     * Called (in a background thread) when new values are available. Repaints the table. If the table is sorted by this column, the sort
     * order is refreshed, because rows were sorted with empty values as long as the results were missing.
     */
    private void onResultsUpdated() {
        if (!updateScheduled.compareAndSet(false, true)) {
            return;
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                updateScheduled.set(false);
                final ExtTableModel<AbstractNode> model = getModel();
                if (model == null) {
                    return;
                }
                final ExtTable<AbstractNode> table = model.getTable();
                if (table != null) {
                    table.repaint();
                }
                if (model.getSortColumn() == ExtractToColumn.this && model instanceof PackageControllerTableModel) {
                    ((PackageControllerTableModel) model).recreateModel(true);
                }
            }
        };
    }
}
