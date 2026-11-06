package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DropMode;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.ListSelectionModel;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTransferHandler;
import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.TimingTableModel.TimingRow;

/**
 * Simulation of how a captcha runs through the solvers, see {@link TimingTableModel}. Rows can be dragged (the drop position is shown),
 * but this has no effect: The order of the solvers can only be changed in the solver order table, the user is told so.
 */
public class TimingTable extends BasicJDTable<TimingRow> {
    private static final long serialVersionUID = 1L;

    public TimingTable(final TimingTableModel model) {
        super(model);
        setSearchEnabled(true);
        setShowHorizontalLineBelowLastEntry(false);
        setShowHorizontalLines(true);
        setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        /* Focusable (click on a row): Without focus the table never receives keyboard shortcuts like Ctrl+A (select all). */
        setFocusable(true);
        setDragEnabled(true);
        setTransferHandler(new ExtTransferHandler<TimingRow>());
        setDropMode(DropMode.INSERT_ROWS);
    }

    @Override
    public TimingTableModel getModel() {
        return (TimingTableModel) super.getModel();
    }

    /** Removes the values of the selected captcha type of all shown solvers, see {@link TimingTableModel#resetAll()}. */
    public AppAction getResetCaptchaTypeAction() {
        return new AppAction() {
            private static final long serialVersionUID = 1L;
            {
                setName(_GUI.T.TimingTable_reset_captchaType());
                setIconKey(IconKey.ICON_RESET);
                setTooltipText(_GUI.T.TimingTable_reset_tooltip_captchaType());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                TimingTable.this.getModel().resetAll();
            }
        };
    }

    /** Sets the value of the solver of all shown solvers back to the default, see {@link TimingTableModel#resetAllSolverValues()}. */
    public AppAction getResetSolverAction() {
        return new AppAction() {
            private static final long serialVersionUID = 1L;
            {
                setName(_GUI.T.TimingTable_reset_solver());
                setIconKey(IconKey.ICON_RESET);
                setTooltipText(_GUI.T.TimingTable_reset_tooltip_solver());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                TimingTable.this.getModel().resetAllSolverValues();
            }
        };
    }

    /** Removes all values of all shown solvers, see {@link TimingTableModel#resetAllValues()}. */
    public AppAction getResetAllAction() {
        return new AppAction() {
            private static final long serialVersionUID = 1L;
            {
                setName(_GUI.T.TimingTable_reset_all());
                setIconKey(IconKey.ICON_RESET);
                setTooltipText(_GUI.T.TimingTable_reset_tooltip_all());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                TimingTable.this.getModel().resetAllValues();
            }
        };
    }

    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final TimingRow contextObject, final List<TimingRow> selection, final ExtColumn<TimingRow> column, final MouseEvent mouseEvent) {
        final TimingTableModel model = getModel();
        if (selection != null && !selection.isEmpty()) {
            final List<TimingRow> rows = new ArrayList<TimingRow>(selection);
            final JMenuItem reset = new JMenuItem(new AppAction() {
                private static final long serialVersionUID = 1L;
                {
                    setName(model.getCaptchaType() == null ? _GUI.T.TimingTable_context_reset_solver() : _GUI.T.TimingTable_context_reset_captchaType());
                    setIconKey(IconKey.ICON_RESET);
                    /* Greyed out if the selected solvers already have their default for the solver, or for the selected captcha type. */
                    setEnabled(model.isAnyNotDefault(rows));
                }

                @Override
                public void actionPerformed(final ActionEvent e) {
                    model.reset(rows);
                }
            });
            popup.add(reset);
            popup.addSeparator();
        }
        /* The same reset options as the buttons below the table, greyed out in the same cases (see SolverTimingDialog#updateResetEnabled). */
        final AppAction resetCaptchaType = getResetCaptchaTypeAction();
        resetCaptchaType.setEnabled(model.getCaptchaType() != null && !model.isAllDefault());
        popup.add(new JMenuItem(resetCaptchaType));
        final AppAction resetSolver = getResetSolverAction();
        resetSolver.setEnabled(model.isAnySolverValueSet());
        popup.add(new JMenuItem(resetSolver));
        final AppAction resetAll = getResetAllAction();
        resetAll.setEnabled(model.isAnyValueSet());
        popup.add(new JMenuItem(resetAll));
        return popup;
    }

    @Override
    protected boolean isDisplayFullsizeAlways() {
        return false;
    }
}
