package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Currency;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtCurrencyColumn;
import org.appwork.swing.exttable.columns.ExtLongColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.uio.UIOManager;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.renderer.RenderLabel;
import org.appwork.utils.swing.renderer.RendererMigPanel;
import org.jdownloader.api.myjdownloader.MyJDownloaderConnectionStatus;
import org.jdownloader.api.myjdownloader.MyJDownloaderController;
import org.jdownloader.api.myjdownloader.event.MyJDownloaderListener;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.JobRunnable;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_MYJD;

import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.plugins.AccountInfo;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class SolverOrderTableModel extends ExtTableModel<SolverService> {
    public SolverOrderTableModel() {
        super("SolverOrderTableModel");
        update();
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
    }

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

    /** Redraws all rows on the EDT without changing their order. */
    private void refreshRows() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                fireTableDataChanged();
            }
        };
    }

    private void update() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                // make sure that this class is loaded. it contains the logic to restore old settings.
                final List<SolverService> visible = new ArrayList<SolverService>(ChallengeResponseController.getInstance().listServices());
                /* Default order: ready (usable) solvers first. Collections.sort is stable, so the relative order is otherwise kept. */
                Collections.sort(visible, new Comparator<SolverService>() {
                    @Override
                    public int compare(final SolverService a, final SolverService b) {
                        final int ra = a.isReady() ? 0 : 1;
                        final int rb = b.isReady() ? 0 : 1;
                        return ra - rb;
                    }
                });
                _fireTableStructureChanged(visible, true);
            }
        };
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

            /* Same tooltip as in the solver comparison table. */
            @Override
            protected String getTooltipText(final SolverService value) {
                return SolverComparisonTableModel.getSolverTooltip(value);
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

            @Override
            public int getDefaultWidth() {
                return 300;
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
            private final Icon             addIcon     = new AbstractIcon(IconKey.ICON_ADD, 14);
            private final Icon             warningIcon = new AbstractIcon(IconKey.ICON_WARNING, 14);
            {
                rendererLabel = new RenderLabel();
                rendererBtn = new JButton();
                rendererBtn.setIcon(addIcon);
                rendererBtn.setHorizontalAlignment(SwingConstants.LEFT);
                rendererBtn.setMargin(new Insets(0, 4, 0, 4));
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel, "hidemode 3");
                renderer.add(rendererBtn, "hidemode 3, height 18!");
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
                editor = new MigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                editor.add(editorBtn, "height 18!");
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
                /* Only rows that show an action button are editable (clickable). */
                return service.getStatusActionName() != null;
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
                editorBtn.setText(value.getStatusActionName());
                editorBtn.setIcon(value.isStatusActionWarning() ? warningIcon : addIcon);
            }

            @Override
            public void configureRendererComponent(final SolverService value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final String action = value.getStatusActionName();
                if (action != null) {
                    rendererBtn.setText(action);
                    rendererBtn.setIcon(value.isStatusActionWarning() ? warningIcon : addIcon);
                    rendererBtn.setVisible(true);
                    rendererLabel.setVisible(false);
                } else {
                    rendererLabel.setText(value.getStatusText());
                    /* Ready solvers get a green check; solvers that need an action (button rows) never get one. */
                    rendererLabel.setIcon(value.isReady() ? NewTheme.I().getIcon(IconKey.ICON_OK, 14) : null);
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
                final List<CAPTCHA_TYPE> supportedTypes = value.getSupportedCaptchaTypes();
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
        this.addColumn(new ExtComponentColumn<SolverService>(_GUI.T.SolverOrderTableModel_initColumns_timeout()) {
            private JButton            editorBtn;
            private JButton            rendererBtn;
            private SolverService      editing;
            protected MigPanel         editor;
            protected RendererMigPanel renderer;
            private RenderLabel        label;
            {
                editorBtn = new JButton("");
                editorBtn.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (editing != null) {
                            SolverTimingDialog d = new SolverTimingDialog(editing);
                            UIOManager.I().show(null, d);
                        }
                    }
                });
                label = new RenderLabel();
                rendererBtn = new JButton("");
                this.editor = new MigPanel("ins 1", "[grow,fill]", "[grow]") {
                    @Override
                    public void requestFocus() {
                    }
                };
                editor.add(editorBtn);
                this.renderer = new RendererMigPanel("ins 1", "[grow,fill]", "[grow]");
                renderer.add(rendererBtn);
                setClickcount(1);
                /* The cell always shows "Edit", so there is no visible per-row value; sort by the number of configured wait-for rules instead. */
                this.setRowSorter(new ExtDefaultRowSorter<SolverService>() {
                    @Override
                    public int compare(final SolverService o1, final SolverService o2) {
                        final int c1 = JobRunnable.getWaitForOverrideCount(o1);
                        final int c2 = JobRunnable.getWaitForOverrideCount(o2);
                        if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                            return c1 - c2;
                        } else {
                            return c2 - c1;
                        }
                    }
                });
            }

            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    @Override
                    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                        setIcon(new AbstractIcon(IconKey.ICON_WAIT, 14));
                        setHorizontalAlignment(CENTER);
                        setText(_GUI.T.SolverOrderTableModel_initColumns_timeout());
                        return this;
                    }
                };
                return ret;
            }

            @Override
            public boolean isAutoWidthEnabled() {
                return true;
            }

            @Override
            protected boolean isDefaultResizable() {
                return true;
            }

            protected String generateID() {
                return "CaptchaOrderTable.timeoutbutton3";
            }

            @Override
            public boolean isHidable() {
                return false;
            }

            @Override
            public int getDefaultWidth() {
                return 3;
            }

            public Dimension getCellSizeEstimation(SolverService element, int row) {
                Component c = getTableCellRendererComponent(getModel().getTable(), element, false, false, row, 1);
                rendererBtn.setMaximumSize(null);
                return c.getPreferredSize();
            }

            @Override
            public boolean isSortable(final SolverService obj) {
                return true;
            }

            @Override
            protected JComponent getInternalEditorComponent(SolverService value, boolean isSelected, int row, int column) {
                return editor;
            }

            @Override
            public boolean onSingleClick(MouseEvent e, SolverService obj) {
                return super.onSingleClick(e, obj);
            }

            @Override
            protected JComponent getInternalRendererComponent(SolverService value, boolean isSelected, boolean hasFocus, int row, int column) {
                return renderer;
            }

            @Override
            public boolean isEnabled(SolverService obj) {
                return true;
            }

            @Override
            public void configureRendererComponent(SolverService value, boolean isSelected, boolean hasFocus, int row, int column) {
                // rendererBtn.setIcon(new AbstractIcon(IconKey.ICON_WAIT, 16));
                rendererBtn.setText(_GUI.T.lit_edit());
                rendererBtn.setMaximumSize(new Dimension(getWidth(), getTable().getRowHeight(row) - 2));
            }

            @Override
            public void configureEditorComponent(SolverService value, boolean isSelected, int row, int column) {
                editing = value;
                editorBtn.setText(_GUI.T.lit_edit());
                editorBtn.setMaximumSize(new Dimension(getWidth(), getTable().getRowHeight(row) - 2));
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
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
            public boolean isHidable() {
                return false;
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
