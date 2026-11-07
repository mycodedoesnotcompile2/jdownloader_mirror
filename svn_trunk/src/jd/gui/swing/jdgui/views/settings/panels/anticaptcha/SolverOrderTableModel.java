package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Set;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Currency;
import java.util.List;

import javax.swing.Icon;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtCurrencyColumn;
import org.appwork.swing.exttable.columns.ExtLongColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.renderer.RenderLabel;
import org.appwork.utils.swing.renderer.RendererMigPanel;
import org.jdownloader.api.myjdownloader.MyJDownloaderConnectionStatus;
import org.jdownloader.api.myjdownloader.MyJDownloaderController;
import org.jdownloader.api.myjdownloader.event.MyJDownloaderListener;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_MYJD;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.controlling.AccountController;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.settings.SilentModeController;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.gui.swing.jdgui.TriStateSorterTableModel;
import jd.plugins.AccountInfo;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class SolverOrderTableModel extends ExtTableModel<SolverService> implements TriStateSorterTableModel {
    /* Third state of the tri-state sorting: back to the real order of the solvers. */
    private static final String SORT_ORIGINAL = "ORIGINAL";

    /* Assigned in initColumns() (called by the super constructor), so it must not have an initializer. */
    private ExtColumn<SolverService> activeJobsColumn;

    public SolverOrderTableModel() {
        super("SolverOrderTableModel");
        update();
        /*
         * The number of active solver jobs changes all the time: While the column is visible, redraw the rows (without re-sorting) every
         * second.
         */
        final javax.swing.Timer activeJobsTimer = new javax.swing.Timer(1000, new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(final java.awt.event.ActionEvent e) {
                final javax.swing.JTable table = getTable();
                if (table != null && table.isShowing() && activeJobsColumn != null && isColumnVisible(activeJobsColumn) && getRowCount() > 0) {
                    fireTableRowsUpdated(0, getRowCount() - 1);
                }
            }
        });
        activeJobsTimer.setRepeats(true);
        activeJobsTimer.start();
        /*
         * Keep the status/balance/configure columns up to date: redraw the rows (without re-sorting) whenever an account is added, updated
         * or removed, or when the MyJDownloader connection status changes. The cell contents are computed live from the solver services, so
         * a plain redraw is enough.
         */
        AccountController.getInstance().getEventSender().addListener(new AccountControllerListener() {
            @Override
            public void onAccountControllerEvent(final AccountControllerEvent event) {
                refreshRows();
            }
        });
        MyJDownloaderController.getInstance().getEventSender().addListener(new MyJDownloaderListener() {
            @Override
            public void onMyJDownloaderConnectionStatusChanged(final MyJDownloaderConnectionStatus status, final int connections) {
                refreshRows();
            }
        });
        /*
         * The "Enable solver accounts" warning (Status column) and the sort order (ready solvers first) both depend on this global
         * setting, so toggling it via CaptchaToggleUseAvailableSolverAccountsAction must redraw the table immediately.
         */
        CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(final KeyHandler<Boolean> keyHandler, final Boolean newValue) {
                refreshRows();
            }

            @Override
            public void onConfigValidatorError(final KeyHandler<Boolean> keyHandler, final Boolean invalidValue, final ValidationException validateException) {
            }
        });
        addMyJDownloaderListeners();
        addSilentModeListeners();
        /*
         * The enabled state of a solver can be changed outside of this table (e.g. via CaptchaToggleSolverServicesAction), so every
         * solver's "enabled" setting must redraw the rows, too. Weak listener: enabledListener is a field, so it lives as long as this model.
         */
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            final BooleanKeyHandler enabledKey = service.getConfigV3()._getStorageHandler().getKeyHandler("enabled", BooleanKeyHandler.class);
            if (enabledKey != null) {
                enabledKey.getEventSender().addListener(enabledListener, true);
            }
        }
    }

    /**
     * The status of the dialog and browser solver tells what the silent mode does to captchas, which depends on the silent mode settings,
     * so changes of them must redraw the rows, too. Whether the silent mode is active right now cannot be observed (it depends on the
     * window focus and the idle time), it is read whenever the rows are painted.
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void addSilentModeListeners() {
        final GenericConfigEventListener<Object> listener = new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
                refreshRows();
            }

            @Override
            public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
            }
        };
        final KeyHandler[] keyHandlers = new KeyHandler[] { SilentModeController.getInstance().getSettingKeyHandler(), CFG_SILENTMODE.ON_CAPTCHA_DURING_SILENT_MODE_ACTION, CFG_SILENTMODE.ON_DIALOG_DURING_SILENT_MODE_ACTION };
        for (final KeyHandler keyHandler : keyHandlers) {
            keyHandler.getEventSender().addListener(listener);
        }
    }

    private final GenericConfigEventListener<Boolean> enabledListener = new GenericConfigEventListener<Boolean>() {
        @Override
        public void onConfigValueModified(final KeyHandler<Boolean> keyHandler, final Boolean newValue) {
            refreshRows();
        }

        @Override
        public void onConfigValidatorError(final KeyHandler<Boolean> keyHandler, final Boolean invalidValue, final ValidationException validateException) {
        }
    };

    /**
     * The MyJDownloader solver's Status column depends on whether a MyJD login is configured (email/password) and on the latest MyJD
     * error, so changes of those settings must redraw the rows, too. Connection status changes are already covered by the
     * MyJDownloaderListener above.
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void addMyJDownloaderListeners() {
        final GenericConfigEventListener<Object> listener = new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
                refreshRows();
            }

            @Override
            public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
            }
        };
        final KeyHandler[] keyHandlers = new KeyHandler[] { CFG_MYJD.EMAIL, CFG_MYJD.PASSWORD, CFG_MYJD.LATEST_ERROR };
        for (final KeyHandler keyHandler : keyHandlers) {
            keyHandler.getEventSender().addListener(listener);
        }
    }

    /** True while the rows are being reloaded: the selection is cleared and restored then, which is not a user action. */
    private volatile boolean reloading = false;

    public boolean isReloading() {
        return reloading;
    }

    /**
     * Runs a reload of the rows (EDT only) and keeps all selected solvers selected afterwards. Reloading clears the selection, which would
     * make the detail panels below the table disappear. The selection events fired in between are suppressed (see {@link #isReloading()}).
     * Only if the selected solver is gone afterwards, the listeners are informed about the lost selection.
     */
    private void reloadPreservingSelection(final Runnable reload) {
        final ExtTable<SolverService> table = getTable();
        /* All selected solvers, not only the first one: The user can select several solvers (e.g. for the context menu). */
        final List<SolverService> selectedSolvers = table == null ? new ArrayList<SolverService>() : getSelectedObjects();
        reloading = true;
        try {
            reload.run();
            for (final SolverService selectedSolver : selectedSolvers) {
                final int row = getRowforObject(selectedSolver);
                if (row >= 0) {
                    table.getSelectionModel().addSelectionInterval(row, row);
                }
            }
        } finally {
            reloading = false;
        }
        if (!selectedSolvers.isEmpty() && table.getSelectedRow() < 0 && table instanceof SolverOrderTable) {
            ((SolverOrderTable) table).fireSelectionChanged();
        }
    }

    /** Redraws all rows on the EDT without changing their order. */
    private void refreshRows() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                reloadPreservingSelection(new Runnable() {
                    @Override
                    public void run() {
                        fireTableDataChanged();
                    }
                });
            }
        };
    }

    private void update() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                // make sure that this class is loaded. it contains the logic to restore old settings.
                /* The order in which the solvers are started for a captcha, the user can change it by drag and drop, see move(). */
                final List<SolverService> visible = ChallengeResponseController.getInstance().getOrderedServices();
                reloadPreservingSelection(new Runnable() {
                    @Override
                    public void run() {
                        _fireTableStructureChanged(visible, true);
                    }
                });
            }
        };
    }

    /**
     * Drag and drop: Moves the dragged solvers to the given drop row (an index into the displayed rows) and stores the new order. The order
     * is the order in which the solvers are started for a captcha, see {@link ChallengeResponseController#getOrderedServices()}. <br>
     * Only possible while the rows are displayed in the real order of the solvers, see {@link #sort(List, ExtColumn)}.
     */
    @Override
    public boolean move(final List<SolverService> transferData, final int dropRow) {
        if (transferData == null || transferData.isEmpty()) {
            return false;
        }
        if (getSortColumn() != null) {
            /* The rows are only sorted visually: Their positions are not the positions in the real order. */
            return false;
        }
        final List<SolverService> displayed = new ArrayList<SolverService>(getTableData());
        final int targetIndex = Math.max(0, Math.min(dropRow, displayed.size()));
        final List<SolverService> newOrder = new ArrayList<SolverService>(displayed.subList(0, targetIndex));
        final List<SolverService> after = new ArrayList<SolverService>(displayed.subList(targetIndex, displayed.size()));
        newOrder.removeAll(transferData);
        after.removeAll(transferData);
        newOrder.addAll(transferData);
        newOrder.addAll(after);
        ChallengeResponseController.getInstance().setServiceOrder(newOrder);
        update();
        return true;
    }

    /**
     * No default sorting: The rows are displayed in the order of the solvers (see {@link #move(List, int)}). Sorting by a column stores the
     * result as the new solver order, see {@link #sort(List, ExtColumn)}.
     */
    @Override
    protected ExtColumn<SolverService> getDefaultSortColumn() {
        return null;
    }

    /**
     * Sorting by a column (click on a column header) is a visual sort only: It does not change the order of the solvers. The table shows
     * the highlighted sort column then, and a click on the header until the highlighting disappears (third state, {@link #SORT_ORIGINAL})
     * shows the real order again. A permanent sort (Ctrl + click) is done by {@link #sortPermanently(ExtColumn)}.
     */
    @Override
    public List<SolverService> sort(final List<SolverService> data, final ExtColumn<SolverService> column) {
        if (column == null || SORT_ORIGINAL.equals(column.getSortOrderIdentifier())) {
            super.sort(data, null);
            /* Back to the real order: The rows may be in the order of a previous visual sort. */
            final List<SolverService> ordered = ChallengeResponseController.getInstance().getOrderedServices();
            Collections.sort(data, new Comparator<SolverService>() {
                @Override
                public int compare(final SolverService a, final SolverService b) {
                    return ordered.indexOf(a) - ordered.indexOf(b);
                }
            });
            return data;
        }
        return super.sort(data, column);
    }

    /**
     * Permanent sort (Ctrl + click on a column header): The resulting row order is stored as the new order of the solvers. This overwrites
     * the order the user has set before.
     */
    public void sortPermanently(final ExtColumn<SolverService> column) {
        final String current = column.getSortOrderIdentifier();
        final String primary = getPrimarySortIdentifier();
        /* Toggles between ascending and descending. */
        column.setSortOrderIdentifier(primary.equals(current) ? getSecondarySortIdentifier() : primary);
        final List<SolverService> sorted = new ArrayList<SolverService>(ChallengeResponseController.getInstance().getOrderedServices());
        Collections.sort(sorted, column.getRowSorter());
        ChallengeResponseController.getInstance().setServiceOrder(sorted);
        /* The rows are in the real order now: No visual sort is active any more. */
        column.setSortOrderIdentifier(SORT_ORIGINAL);
        setSortColumn(null);
        update();
    }

    private static String getPrimarySortIdentifier() {
        return CFG_GUI.CFG.isPrimaryTableSorterDesc() ? ExtColumn.SORT_DESC : ExtColumn.SORT_ASC;
    }

    private static String getSecondarySortIdentifier() {
        return CFG_GUI.CFG.isPrimaryTableSorterDesc() ? ExtColumn.SORT_ASC : ExtColumn.SORT_DESC;
    }

    /** Tri-state: primary sort order, secondary sort order, then back to the real order. */
    @Override
    public String getNextSortIdentifier(final String sortOrderIdentifier) {
        if (sortOrderIdentifier == null || SORT_ORIGINAL.equals(sortOrderIdentifier)) {
            return getPrimarySortIdentifier();
        } else if (getPrimarySortIdentifier().equals(sortOrderIdentifier)) {
            return getSecondarySortIdentifier();
        } else {
            return SORT_ORIGINAL;
        }
    }

    @Override
    public Icon getSortIcon(final String sortOrderIdentifier) {
        if (SORT_ORIGINAL.equals(sortOrderIdentifier)) {
            return null;
        }
        return super.getSortIcon(sortOrderIdentifier);
    }

    /**
     * Do not restore the sort state of a previous session (a visual sort only): The table has to start in the order of the solvers (see
     * {@link ChallengeResponseController#getOrderedServices()}), which is also the order of the timing table and the order in which the
     * solvers are used for captchas. A restored sort column would display the rows in a different order.
     */
    @Override
    protected boolean isSortStateSaverEnabled() {
        return false;
    }

    @Override
    protected void autoColumnWidth() {
        super.autoColumnWidth();
    }

    @Override
    protected void initColumns() {
        this.addColumn(new ExtCheckColumn<SolverService>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
            private final JComponent empty = new RendererMigPanel("ins 0", "[]", "[]");

            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    private final Icon ok = NewTheme.I().getIcon(IconKey.ICON_OK, 14);

                    @Override
                    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                        setIcon(ok);
                        setHorizontalAlignment(CENTER);
                        setText(null);
                        return this;
                    }
                };
                return ret;
            }

            @Override
            public int getMaxWidth() {
                return 30;
            }

            @Override
            public JComponent getRendererComponent(SolverService value, boolean isSelected, boolean hasFocus, int row, int column) {
                JComponent ret = super.getRendererComponent(value, isSelected, hasFocus, row, column);
                return ret;
            }

            @Override
            public boolean isHidable() {
                return false;
            }

            @Override
            protected boolean getBooleanValue(SolverService value) {
                return value.getConfigV3().isEnabled();
            }

            @Override
            public boolean isEditable(SolverService obj) {
                return true;
            }

            @Override
            protected void setBooleanValue(boolean value, final SolverService object) {
                object.getConfigV3().setEnabled(!object.getConfigV3().isEnabled());
            }
        });
        addColumn(new ExtTextColumn<SolverService>(_GUI.T.CaptchaSolverComparison_column_solver()) {
            @Override
            protected Icon getIcon(SolverService value) {
                return value.getIcon(18);
            }

            @Override
            public int getDefaultWidth() {
                return 100;
            }

            @Override
            public boolean isEnabled(SolverService obj) {
                return obj.getConfigV3().isEnabled();
            }

            @Override
            public String getStringValue(SolverService value) {
                return value.getName();
            }

            /* The description of the solver (the comparison table shows the same). */
            @Override
            protected String getTooltipText(final SolverService value) {
                return value.getDescription();
            }

            /* Double click opens the page where an account for this solver can be bought. */
            @Override
            public boolean onDoubleClick(final MouseEvent e, final SolverService value) {
                return SolverComparisonTableModel.openBuyPage(value);
            }

            /* Like the Enabled column, the solver name must always be visible. */
            @Override
            public boolean isHidable() {
                return false;
            }
        });
        addColumn(new ExtTextColumn<SolverService>(_GUI.T.SolverOrderTableModel_initColumns_type_()) {
            @Override
            public boolean isEnabled(SolverService obj) {
                return obj.getConfigV3().isEnabled();
            }

            /* Hidden by default, can be shown via the column menu. */
            @Override
            public boolean isDefaultVisible() {
                return false;
            }

            @Override
            public int getDefaultWidth() {
                return 300;
            }

            /* Same icon as in the solver type selection of the solver comparison table. */
            @Override
            protected Icon getIcon(SolverService value) {
                return value.getType().getIcon(18);
            }

            @Override
            public String getStringValue(SolverService value) {
                return value.getType().getLabel();
            }

            @Override
            protected String getTooltipText(SolverService value) {
                return value.getDescription();
            }
        });
        this.addColumn(new ExtComponentColumn<SolverService>("Status") {
            private SolverService          editing;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;
            private final JButton          rendererBtn;
            private final MigPanel         editor;
            private final JButton          editorBtn;
            private final JButton          rendererWarnBtn;
            private final JButton          editorWarnBtn;
            private final RenderLabel      editorLabel;
            private final Icon             addIcon     = new AbstractIcon(IconKey.ICON_ADD, 14);
            private final Icon             warningIcon = new AbstractIcon(IconKey.ICON_WARNING, 14);
            private final Color            defaultStatusForeground;
            {
                rendererLabel = new RenderLabel();
                defaultStatusForeground = rendererLabel.getForeground();
                rendererBtn = new JButton();
                rendererBtn.setIcon(addIcon);
                rendererBtn.setHorizontalAlignment(SwingConstants.LEFT);
                rendererBtn.setMargin(new Insets(0, 4, 0, 4));
                /*
                 * Icon-only warning button in front of the status (e.g. "solver accounts are globally disabled"); its explanation is
                 * shown as tooltip (see getTooltipText). Status label and action button share the second cell.
                 */
                rendererWarnBtn = new JButton();
                rendererWarnBtn.setIcon(warningIcon);
                rendererWarnBtn.setMargin(new Insets(0, 2, 0, 2));
                /*
                 * No gap between the columns: The gap belongs to the warning button (gapright), so rows without it start flush left instead
                 * of being indented by the gap of an empty first column.
                 */
                renderer = new RendererMigPanel("ins 0, gap 0", "[][grow,fill]", "[grow,fill]");
                renderer.add(rendererWarnBtn, "cell 0 0, hidemode 3, height 18!, gapright 2");
                renderer.add(rendererLabel, "cell 1 0, hidemode 3");
                renderer.add(rendererBtn, "cell 1 0, hidemode 3, height 18!");
                editorLabel = new RenderLabel();
                editorWarnBtn = new JButton();
                editorWarnBtn.setIcon(warningIcon);
                editorWarnBtn.setMargin(new Insets(0, 2, 0, 2));
                editorWarnBtn.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (editing != null) {
                            editing.onStatusWarningButton();
                        }
                    }
                });
                editorBtn = new JButton();
                editorBtn.setIcon(addIcon);
                editorBtn.setHorizontalAlignment(SwingConstants.LEFT);
                editorBtn.setMargin(new Insets(0, 4, 0, 4));
                editorBtn.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (editing != null) {
                            final SolverService target = editing;
                            /*
                             * onStatusAction() (e.g. "Add Account") opens a modal dialog; while it is open (or right after it closes,
                             * successfully or not), an account/status change re-sorts/redraws the table (refreshRows()/update()), which
                             * clears the row selection. Restore it afterwards so the row the user was just looking at stays selected.
                             */
                            target.onStatusAction();
                            if (getModel() != null && getModel().getTable() != null) {
                                final int row = getModel().getRowforObject(target);
                                if (row >= 0) {
                                    getModel().getTable().getSelectionModel().setSelectionInterval(row, row);
                                }
                            }
                        }
                    }
                });
                editor = new MigPanel("ins 0, gap 0", "[][grow,fill]", "[grow,fill]");
                editor.add(editorWarnBtn, "cell 0 0, hidemode 3, height 18!, gapright 2");
                editor.add(editorBtn, "cell 1 0, hidemode 3, height 18!");
                editor.add(editorLabel, "cell 1 0, hidemode 3");
                setClickcount(1);
                /*
                 * No single scalar backs this column (it shows either a status text or an action button), so sort by readiness first
                 * (ready solvers before those needing an action, warnings before routine actions like "Add Account"), then alphabetically
                 * by the displayed text within each group.
                 */
                this.setRowSorter(new ExtDefaultRowSorter<SolverService>() {
                    @Override
                    public int compare(final SolverService o1, final SolverService o2) {
                        final String s1 = getStatusSortKey(o1);
                        final String s2 = getStatusSortKey(o2);
                        if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                            return s1.compareToIgnoreCase(s2);
                        } else {
                            return s2.compareToIgnoreCase(s1);
                        }
                    }
                });
            }

            private String getStatusSortKey(final SolverService value) {
                final int rank = value.isReady() ? 0 : (value.isStatusActionWarning() ? 1 : 2);
                final String text = value.getStatusText() != null ? value.getStatusText() : value.getStatusActionName();
                return rank + "_" + (text != null ? text : "");
            }

            @Override
            public boolean isSortable(final SolverService obj) {
                return true;
            }

            @Override
            public int getDefaultWidth() {
                return 180;
            }

            @Override
            public boolean isEditable(final SolverService service) {
                /* Only rows that show an action button or a warning button are editable (clickable). */
                return service.getStatusActionName() != null || service.getStatusWarningButtonTooltip() != null;
            }

            /* The warning button has no text: its explanation is shown as tooltip. */
            @Override
            protected String getTooltipText(final SolverService value) {
                return value.getStatusWarningButtonTooltip();
            }

            @Override
            protected JComponent getInternalEditorComponent(final SolverService value, final boolean isSelected, final int row, final int column) {
                return editor;
            }

            @Override
            protected JComponent getInternalRendererComponent(final SolverService value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return renderer;
            }

            @Override
            public void configureEditorComponent(final SolverService value, final boolean isSelected, final int row, final int column) {
                editing = value;
                final String warningTooltip = value.getStatusWarningButtonTooltip();
                editorWarnBtn.setVisible(warningTooltip != null);
                editorWarnBtn.setToolTipText(warningTooltip);
                final String action = value.getStatusActionName();
                if (action != null) {
                    editorBtn.setText(action);
                    editorBtn.setIcon(value.isStatusActionWarning() ? warningIcon : addIcon);
                    editorBtn.setVisible(true);
                    editorLabel.setVisible(false);
                } else {
                    editorLabel.setText(value.getStatusText());
                    editorLabel.setIcon(null);
                    editorLabel.setVisible(true);
                    editorBtn.setVisible(false);
                }
            }

            @Override
            public void configureRendererComponent(final SolverService value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                /* Like all other columns, the status is greyed out for disabled solvers (also the green check of a ready solver). */
                final boolean solverEnabled = value.getConfigV3().isEnabled();
                rendererLabel.setEnabled(solverEnabled);
                rendererBtn.setEnabled(solverEnabled);
                rendererWarnBtn.setEnabled(solverEnabled);
                rendererWarnBtn.setVisible(value.getStatusWarningButtonTooltip() != null);
                final String action = value.getStatusActionName();
                if (action != null) {
                    rendererBtn.setText(action);
                    rendererBtn.setIcon(value.isStatusActionWarning() ? warningIcon : addIcon);
                    rendererBtn.setVisible(true);
                    rendererLabel.setVisible(false);
                } else {
                    rendererLabel.setText(value.getStatusText());
                    /* Ready solvers get a green check; solvers that need an action (button rows) never get one. */
                    /* Status texts that are warnings get a warning triangle. */
                    rendererLabel.setIcon(value.isStatusTextWarning() ? warningIcon : (value.isReady() ? NewTheme.I().getIcon(IconKey.ICON_OK, 14) : null));
                    /* The renderer is reused for all rows, so the foreground has to be set (or reset) for every row. */
                    rendererLabel.setForeground(value.isStatusTextWarning() ? LAFOptions.getInstance().getColorForErrorForeground() : defaultStatusForeground);
                    rendererLabel.setVisible(true);
                    rendererBtn.setVisible(false);
                }
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
        addColumn(new ExtTextColumn<SolverService>("Description") {
            @Override
            public boolean isEnabled(SolverService obj) {
                return obj.getConfigV3().isEnabled();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }

            @Override
            public String getStringValue(SolverService value) {
                final String description = value.getDescription();
                return description != null ? description : "";
            }
        });
        this.addColumn(new ExtCurrencyColumn<SolverService>("Balance", this) {
            @Override
            protected long getValue(final SolverService o) {
                final Double b = o.getBalance();
                return b == null ? 0L : Math.round(b.doubleValue() * 100.0d);
            }

            @Override
            protected Currency getCurrency(final SolverService value) {
                return value.getBalanceCurrency();
            }

            @Override
            protected String getText(final SolverService value) {
                /* Empty for solvers without a balance (local solvers, or paid solvers without a valid account). */
                final Double balance = value.getBalance();
                if (balance == null) {
                    return "";
                }
                /*
                 * Formatted the same way as the Status column's "Balance: ..." text (AccountInfo#formatCaptchaSolverBalance), regardless of
                 * how many accounts of this solver exist, instead of ExtCurrencyColumn's own locale-dependent formatting.
                 */
                return AccountInfo.formatCaptchaSolverBalance(balance.doubleValue(), value.getBalanceCurrency());
            }

            @Override
            public boolean isEnabled(final SolverService obj) {
                return obj.getConfigV3().isEnabled();
            }

            @Override
            public void configureEditorComponent(final SolverService value, final boolean isSelected, final int row, final int column) {
                /* Not editable, nothing to configure. */
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        });
        /* Number of captcha types each solver supports (hidden by default, sortable). */
        this.addColumn(new ExtLongColumn<SolverService>(_GUI.T.SolverOrderTableModel_column_supportedCaptchaTypes(), this) {
            @Override
            protected long getLong(final SolverService value) {
                final Set<CAPTCHA_TYPE> supportedTypes = value.getSupportedCaptchaTypes();
                if (supportedTypes == null) {
                    /* A solver without a list of supported captcha types is a bug (see SolverService#getSupportedCaptchaTypes). */
                    return 0;
                }
                return supportedTypes.size();
            }

            @Override
            public boolean isEnabled(final SolverService obj) {
                return obj.getConfigV3().isEnabled();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        });
        /* Number of captchas the solver (all accounts of the service together) is working on right now (hidden by default, sortable). */
        activeJobsColumn = new ExtLongColumn<SolverService>("Number of active captcha challenges", this) {
            @Override
            protected long getLong(final SolverService value) {
                return ChallengeResponseController.getInstance().getActiveCaptchaCount(value.getID());
            }

            @Override
            public boolean isEnabled(final SolverService obj) {
                return obj.getConfigV3().isEnabled();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
        this.addColumn(activeJobsColumn);
        /* Help column: a question-mark icon linking to a knowledgebase article. Always visible, far right. */
        this.addColumn(new ExtComponentColumn<SolverService>("Help") {
            private SolverService          editing;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;
            private final MigPanel         editor;
            private final JButton          editorBtn;
            private final Icon             helpIcon = NewTheme.I().getIcon(IconKey.ICON_HELP, 16);
            {
                rendererLabel = new RenderLabel();
                rendererLabel.setHorizontalAlignment(SwingConstants.CENTER);
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel);
                editorBtn = new JButton();
                editorBtn.setIcon(helpIcon);
                editorBtn.setMargin(new Insets(0, 2, 0, 2));
                editorBtn.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (editing != null) {
                            final String url = editing.getHelpArticleURL();
                            if (url != null && CrossSystem.isOpenBrowserSupported()) {
                                CrossSystem.openURL(url);
                            }
                        }
                    }
                });
                editor = new MigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                editor.add(editorBtn, "height 18!");
                setClickcount(1);
                /* Sort by whether a help article exists (icon shown or not); no other value is displayed in this column. */
                this.setRowSorter(new ExtDefaultRowSorter<SolverService>() {
                    @Override
                    public int compare(final SolverService o1, final SolverService o2) {
                        final int b1 = o1.getHelpArticleURL() != null ? 1 : 0;
                        final int b2 = o2.getHelpArticleURL() != null ? 1 : 0;
                        if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                            return b2 - b1;
                        } else {
                            return b1 - b2;
                        }
                    }
                });
            }

            @Override
            public boolean isSortable(final SolverService obj) {
                return true;
            }

            @Override
            public int getDefaultWidth() {
                return 34;
            }

            @Override
            public int getMaxWidth() {
                return 44;
            }

            @Override
            public boolean isEditable(final SolverService service) {
                return service.getHelpArticleURL() != null;
            }

            @Override
            protected JComponent getInternalEditorComponent(final SolverService value, final boolean isSelected, final int row, final int column) {
                return editor;
            }

            @Override
            protected JComponent getInternalRendererComponent(final SolverService value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return renderer;
            }

            @Override
            public void configureEditorComponent(final SolverService value, final boolean isSelected, final int row, final int column) {
                editing = value;
            }

            @Override
            public void configureRendererComponent(final SolverService value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                rendererLabel.setEnabled(value.getConfigV3().isEnabled());
                /* Only show the help icon for solvers that actually have a help article. */
                rendererLabel.setIcon(value.getHelpArticleURL() != null ? helpIcon : null);
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
    }
}
