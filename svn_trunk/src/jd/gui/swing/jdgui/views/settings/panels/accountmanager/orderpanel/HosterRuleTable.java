package jd.gui.swing.jdgui.views.settings.panels.accountmanager.orderpanel;

import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DropMode;
import javax.swing.JPopupMenu;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.ToggleEnabledSelectionAction;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.ExtTransferHandler;
import org.jdownloader.controlling.hosterrule.AccountUsageRule;
import org.jdownloader.controlling.hosterrule.HosterRuleController;

public class HosterRuleTable extends BasicJDTable<AccountUsageRule> {

    public HosterRuleTable(ExtTableModel<AccountUsageRule> tableModel) {
        super(tableModel);
        setSearchEnabled(true);
        this.setDragEnabled(true);
        this.setDropMode(DropMode.INSERT_ROWS);
        setTransferHandler(new ExtTransferHandler<AccountUsageRule>() {

            @Override
            public boolean canImport(TransferSupport support) {
                return isOriginalOrder() && super.canImport(support);
            }

        });

    }

    @Override
    protected boolean onShortcutDelete(List<AccountUsageRule> selectedObjects, KeyEvent evt, boolean direct) {
        new RemoveAction(selectedObjects, direct).actionPerformed(null);
        return true;
    }

    /**
     * Context menu: enable/disable and remove for the whole selection. Remove asks for confirmation, same as the Delete key.
     */
    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final AccountUsageRule contextObject, final List<AccountUsageRule> selection, final ExtColumn<AccountUsageRule> column, final MouseEvent mouseEvent) {
        if (selection == null || selection.isEmpty()) {
            return null;
        }
        popup.add(new ToggleEnabledSelectionAction<AccountUsageRule>(contextObject, selection) {
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean isEntryEnabled(final AccountUsageRule entry) {
                return entry.isEnabled();
            }

            @Override
            protected void setEntryEnabled(final AccountUsageRule entry, final boolean enabled) {
                entry.setEnabled(enabled);
            }

            @Override
            protected void onFinished() {
                HosterRuleTable.this.repaint();
            }
        });
        popup.add(new RemoveAction(new ArrayList<AccountUsageRule>(selection), false));
        return popup;
    }

    @Override
    protected boolean onDoubleClick(MouseEvent e, AccountUsageRule obj) {
        if (obj != null) {
            HosterRuleController.getInstance().showEditPanel(obj);
            return true;
        }
        return false;
    }

}
