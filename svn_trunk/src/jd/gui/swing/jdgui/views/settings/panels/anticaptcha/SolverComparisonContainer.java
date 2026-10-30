package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.accessibility.Accessible;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.plaf.basic.ComboPopup;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtCheckBoxMenuItem;
import org.appwork.swing.exttable.ExtColumn;
import org.jdownloader.captcha.v2.CaptchaHistoryEntry;
import org.jdownloader.captcha.v2.CaptchaHistoryManager;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.MultiComboBox;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * "Solver comparison table" tab: shows which captcha solver supports which captcha type (rows = solvers, columns =
 * captcha types). Above the table the user picks which captcha types are shown: a dropdown preselects a set of types (manual
 * selection, all types at the start / the types the user ever needed according to the {@link CaptchaHistoryManager}) and a multi-select popup allows changing
 * that selection freely afterwards.
 */
public class SolverComparisonContainer extends MigPanel implements SettingsComponent {
    /** Sets of captcha types the "preselected captcha types" dropdown can select. */
    private enum Preselect {
        /** The manual selection of the user, all captcha types as long as the user never changed the selection. */
        SELECTION,
        /** The captcha types the user ever needed (captcha history). Greyed out if the history contains no entry. */
        USED
    }

    /** Groups of solver types the "preselected solver groups" dropdown can select. */
    private enum SolverTypePreselect {
        /** All solver types. */
        ALL,
        /** The automatic local solver (JAC). */
        JAC,
        /** Manual captcha solving: the captcha dialog, the browser solver and the MyJDownloader remote solver. */
        MANUAL,
        /** External (plugin based) solvers only. The default. */
        EXTERNAL,
        /** The manual selection of the user, external solvers as long as the user never changed the selection. */
        CUSTOM
    }

    /** Sets of solvers the "preselected captcha solvers" dropdown can select. */
    private enum SolverPreselect {
        /** All solvers of the selected solver types. Greyed out if no solver is enabled. */
        ALL,
        /** The manual selection of the user, all solvers as long as the user never changed the selection. */
        CUSTOM,
        /** All solvers which are enabled. */
        ENABLED,
        /** All solvers which are enabled and ready to be used, see {@link SolverService#isReady()}. Greyed out if there is none. */
        USABLE
    }

    private final SolverComparisonTableModel     model;
    private final BasicJDTable<SolverService>    table;
    private final JComboBox                      presetCombo;
    private final JCheckBox                      onlyFullSupportCheckBox;
    private final JLabel                         emptyLabel;
    /**
     * Hint below the "only full support" check box, shown while the check box is greyed out: red and bold if the check box is checked
     * (the filter the user wants cannot be applied), otherwise only an information.
     */
    private final JLabel                         fullSupportHintLabel;
    private final Font                           hintFont;
    private final Font                           hintBoldFont;
    private final Color                          hintColor;
    /**
     * Multi-select "[x/y] [Type1, Type2, ...]" control (same widget/label format as the multi-select ENUM columns in the Advanced
     * Settings, see {@code AdvancedValueColumn}), doubling as its own button and popup.
     */
    private final MultiComboBox<CAPTCHA_TYPE>    typesCombo;
    /** All captcha types JDownloader can process, i.e. all possible columns. */
    private final Set<CAPTCHA_TYPE>              allTypes;
    /** Captcha types whose columns are currently shown. */
    private final Set<CAPTCHA_TYPE>              selectedTypes = new LinkedHashSet<CAPTCHA_TYPE>();
    /** True while the dropdown/multi-select is modified programmatically so that its listener does not reset the user's selection. */
    private boolean                              adjusting     = false;
    /** True if the captcha history contains captcha types, i.e. the "Used by me" preset can be selected (otherwise it is greyed out). */
    private boolean                              usedAvailable = false;
    private final JScrollPane                    scrollPane;
    /** Hint below the table about the self-declared captcha types of external solvers, only shown if the table contains one. */
    private final JLabel                         disclaimerLabel;
    private final JComboBox                      solverTypePresetCombo;
    /** Multi-select "[x/y] [Type1, Type2, ...]" control of the solver types. */
    private final MultiComboBox<SolverType>      solverTypesCombo;
    /** The solver types whose solvers are shown, see {@link #allSolvers}. */
    private final Set<SolverType>                selectedSolverTypes = new LinkedHashSet<SolverType>();
    /** True while the solver type dropdown/multi-select is modified programmatically so that its listener does not reset the selection. */
    private boolean                              adjustingSolverTypes = false;
    /** The solver types of the "Custom" preset, remembered for the running session only. null = the user never populated it. */
    private Set<SolverType>                      rememberedSolverTypeSelection = null;
    /** All solvers of all types, before the solver type selection is applied. */
    private final List<SolverService>            allServices         = new ArrayList<SolverService>();
    private final JComboBox                      solverPresetCombo;
    /** Multi-select "[x/y] [Solver1, Solver2, ...]" control of the solver selection. */
    private final MultiComboBox<SolverService>   solversCombo;
    /** All solvers of the selected solver types, i.e. all possible rows. */
    private final List<SolverService>            allSolvers          = new ArrayList<SolverService>();
    /** Solvers which are currently shown as rows. */
    private final Set<SolverService>             selectedSolvers     = new LinkedHashSet<SolverService>();
    /** True while the solver dropdown/multi-select is modified programmatically so that its listener does not reset the selection. */
    private boolean                              adjustingSolvers    = false;
    /** True if at least one solver is enabled and usable, i.e. the "usable" solver preset can be selected (otherwise it is greyed out). */
    private boolean                              solverUsableAvailable = false;
    /** The solvers of the "Custom" preset, remembered for the running session only. null = the user never populated it, then all solvers. */
    private Set<SolverService>                   rememberedSolverSelection = null;
    /**
     * The captcha types of the "Selection" preset, remembered for the running session only (not persisted). null = the user never
     * populated it, the preset then selects all types.
     */
    private Set<CAPTCHA_TYPE>                    rememberedSelection = null;

    public SolverComparisonContainer() {
        super("ins 0, wrap 1", "[grow,fill]", "[][][grow,fill][]");
        allTypes = CaptchaType.getProcessableCaptchaTypes();
        model = new SolverComparisonTableModel();
        emptyLabel = new JLabel();
        emptyLabel.setForeground(Color.RED);
        emptyLabel.setFont(emptyLabel.getFont().deriveFont(Font.BOLD, 20f));
        table = new BasicJDTable<SolverService>(model) {
            private static final long serialVersionUID = 1L;

            /* Large red hint inside the table if no captcha type is selected. */
            @Override
            public void paintComponent(final Graphics g) {
                super.paintComponent(g);
                final String hint = getEmptyHint();
                if (hint != null) {
                    final int width = Math.max(100, getWidth() - 40);
                    emptyLabel.setText("<html><body style='width:" + width + "px;text-align:center'>" + hint + "</body></html>");
                    final Dimension size = emptyLabel.getPreferredSize();
                    emptyLabel.setSize(size);
                    final Graphics2D g2 = (Graphics2D) g.create();
                    try {
                        g2.translate((getWidth() - size.width) / 2, getTableHeader().getHeight() + 30);
                        emptyLabel.paint(g2);
                    } finally {
                        g2.dispose();
                    }
                }
            }

            /*
             * "Hide from selection" first (deselects the chosen solvers; greyed out if it would hide every row, the table would be empty),
             * then the same solver actions as in the solver table: open the account manager, buy credits and Test & Debug. Those refer to
             * the solver that was right-clicked, independent of the selection.
             */
            @Override
            protected JPopupMenu onContextMenu(final JPopupMenu popup, final SolverService contextObject, final List<SolverService> selection, final ExtColumn<SolverService> column, final MouseEvent mouseEvent) {
                if (selection != null && !selection.isEmpty()) {
                    final JMenuItem hide = new JMenuItem(_GUI.T.CaptchaSolverComparison_hideFromSelection(), NewTheme.I().getIcon(IconKey.ICON_REMOVE, 16));
                    /* Greyed out if every row is selected: hiding all of them would leave an empty table. */
                    hide.setEnabled(selection.size() != SolverComparisonContainer.this.model.getRowCount());
                    hide.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            hideFromSelection(new ArrayList<SolverService>(selection));
                        }
                    });
                    popup.add(hide);
                    /* Deselects every solver except the selected ones. Greyed out for the same reason as "Hide". */
                    final JMenuItem hideOthers = new JMenuItem(_GUI.T.CaptchaSolverComparison_hideAllOthers(), NewTheme.I().getIcon(IconKey.ICON_REMOVE, 16));
                    hideOthers.setEnabled(selection.size() != SolverComparisonContainer.this.model.getRowCount());
                    hideOthers.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            hideAllOthers(new ArrayList<SolverService>(selection));
                        }
                    });
                    popup.add(hideOthers);
                }
                if (contextObject instanceof PluginForCaptchaSolverSolverService) {
                    final PluginForCaptchaSolverSolverService external = (PluginForCaptchaSolverSolverService) contextObject;
                    if (popup.getComponentCount() > 0) {
                        popup.addSeparator();
                    }
                    /* Greyed out if the solver has no account the account manager could show. */
                    final JMenuItem openAccountManager = new JMenuItem(_GUI.T.SolverOrderTable_context_openAccountManager(), new AbstractIcon(IconKey.ICON_PREMIUM, 18));
                    openAccountManager.setEnabled(external.hasAccount());
                    openAccountManager.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            external.openAccountManagerSelectingFirstMatchingAccount();
                        }
                    });
                    popup.add(openAccountManager);
                    if (external.getBuyURL() != null) {
                        final JMenuItem buyCredits = new JMenuItem(_GUI.T.SolverOrderTable_context_buyCredits(), new AbstractIcon(IconKey.ICON_BUY, 18));
                        buyCredits.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                external.openBuyPage("captchasolver/comparisontable/context");
                            }
                        });
                        popup.add(buyCredits);
                    }
                    if (CaptchaConfigPanel.isDebugTabShown()) {
                        /*
                         * Only shown if the "Test & Debug" tab exists. Refers to all selected solvers if the right-clicked one is part of the
                         * selection, otherwise only to the right-clicked one. Only enabled if at least one of them can be tested there.
                         */
                        final List<SolverService> testSolvers = selection != null && selection.contains(external) ? new ArrayList<SolverService>(selection) : new ArrayList<SolverService>(java.util.Collections.singletonList((SolverService) external));
                        boolean anyTestable = false;
                        for (final SolverService testSolver : testSolvers) {
                            anyTestable |= CaptchaTestPanel.isSelectable(testSolver);
                        }
                        final JMenuItem testAndDebug = new JMenuItem(testSolvers.size() > 1 ? "Test & Debug (" + testSolvers.size() + " solvers)" : "Test & Debug", new AbstractIcon(IconKey.ICON_BUG, 18));
                        testAndDebug.setEnabled(anyTestable);
                        testAndDebug.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                CaptchaConfigPanel.showTestAndDebug(testSolvers, null);
                            }
                        });
                        popup.add(testAndDebug);
                    }
                }
                return popup.getComponentCount() == 0 ? null : popup;
            }

            /*
             * Own column menu: the captcha type columns are controlled by the captcha type selection above the table, so the default menu
             * (which would hide columns on its own) is replaced, see createColumnMenu.
             */
            @Override
            protected JPopupMenu columnControlMenu(final ExtColumn<SolverService> extColumn) {
                return createColumnMenu(extColumn);
            }

            @Override
            public boolean isColumnButtonVisible() {
                return true;
            }
        };
        /* Many captcha type columns: keep their width and scroll horizontally instead of squeezing them. */
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        /* Multi-select "[x/y] [Type1, Type2]" control; CAPTCHA_TYPE has no LabelInterface, so the per-item label comes from getDisplayName(). */
        typesCombo = new MultiComboBox<CAPTCHA_TYPE>(new ArrayList<CAPTCHA_TYPE>(allTypes)) {
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean isSelectAllEnabled() {
                return true;
            }

            @Override
            protected String getLabel(final CAPTCHA_TYPE type) {
                return type.getDisplayName();
            }

            /* Same icons as in the debug panel's captcha type selection. */
            @Override
            protected Icon getIcon(final CAPTCHA_TYPE type) {
                return type.getIcon();
            }

            @Override
            protected String getLabel(final List<CAPTCHA_TYPE> list) {
                final List<String> labels = new ArrayList<String>();
                for (final CAPTCHA_TYPE type : list) {
                    labels.add(getLabel(type));
                }
                return "[" + list.size() + "/" + getValues().size() + "] " + labels.toString();
            }

            @Override
            public void onChanged() {
                super.onChanged();
                if (adjusting) {
                    return;
                }
                selectedTypes.clear();
                selectedTypes.addAll(getSelectedItems());
                onManualSelectionChange();
            }
        };
        presetCombo = new JComboBox() {
            private static final long serialVersionUID = 1L;

            @Override
            public void setSelectedItem(final Object anObject) {
                if (anObject == Preselect.USED && !usedAvailable) {
                    /* Greyed out: there are no used captcha types. */
                    return;
                }
                super.setSelectedItem(anObject);
            }

            /* The popup list is recreated whenever the look and feel changes, so the guard has to be installed again then. */
            @Override
            public void updateUI() {
                super.updateUI();
                installDisabledItemGuard(this);
            }
        };
        installDisabledItemGuard(presetCombo);
        presetCombo.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                final JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof Preselect) {
                    label.setText(getPresetName((Preselect) value));
                    if (value == Preselect.USED && !usedAvailable) {
                        label.setEnabled(false);
                        label.setForeground(Color.GRAY);
                    }
                }
                return label;
            }
        });
        presetCombo.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (!adjusting && presetCombo.getSelectedItem() instanceof Preselect) {
                    applyPreset((Preselect) presetCombo.getSelectedItem());
                }
            }
        });
        solversCombo = new MultiComboBox<SolverService>(new ArrayList<SolverService>()) {
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean isSelectAllEnabled() {
                return true;
            }

            @Override
            protected String getLabel(final SolverService solver) {
                return solver.getName();
            }

            @Override
            protected Icon getIcon(final SolverService solver) {
                return solver.getIcon(16);
            }

            @Override
            protected String getLabel(final List<SolverService> list) {
                final List<String> labels = new ArrayList<String>();
                for (final SolverService solver : list) {
                    labels.add(getLabel(solver));
                }
                return "[" + list.size() + "/" + getValues().size() + "] " + labels.toString();
            }

            @Override
            public void onChanged() {
                super.onChanged();
                if (adjustingSolvers) {
                    return;
                }
                selectedSolvers.clear();
                selectedSolvers.addAll(getSelectedItems());
                onManualSolverSelectionChange();
            }
        };
        solverPresetCombo = new JComboBox() {
            private static final long serialVersionUID = 1L;

            @Override
            public void setSelectedItem(final Object anObject) {
                if (isPresetDisabled(anObject)) {
                    /* Greyed out entries cannot be selected. */
                    return;
                }
                super.setSelectedItem(anObject);
            }

            /* The popup list is recreated whenever the look and feel changes, so the guard has to be installed again then. */
            @Override
            public void updateUI() {
                super.updateUI();
                installDisabledItemGuard(this);
            }
        };
        installDisabledItemGuard(solverPresetCombo);
        solverPresetCombo.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                final JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof SolverPreselect) {
                    label.setText(getSolverPresetName((SolverPreselect) value));
                    if (isPresetDisabled(value)) {
                        label.setEnabled(false);
                        label.setForeground(Color.GRAY);
                    }
                }
                return label;
            }
        });
        solverPresetCombo.addItem(SolverPreselect.ALL);
        solverPresetCombo.addItem(SolverPreselect.CUSTOM);
        solverPresetCombo.addItem(SolverPreselect.ENABLED);
        solverPresetCombo.addItem(SolverPreselect.USABLE);
        solverPresetCombo.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (!adjustingSolvers && solverPresetCombo.getSelectedItem() instanceof SolverPreselect) {
                    applySolverPreset((SolverPreselect) solverPresetCombo.getSelectedItem());
                }
            }
        });
        /*
         * Both selection rows (captcha types and solvers) share one grid: the labels, the preselect dropdowns and the multi-select
         * controls line up, the multi-select controls take the remaining width.
         */
        solverTypesCombo = new MultiComboBox<SolverType>(SolverType.values()) {
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean isSelectAllEnabled() {
                return true;
            }

            @Override
            protected String getLabel(final SolverType type) {
                return type.getLabel();
            }

            @Override
            protected Icon getIcon(final SolverType type) {
                return type.getIcon(16);
            }

            @Override
            protected String getLabel(final List<SolverType> list) {
                final List<String> labels = new ArrayList<String>();
                for (final SolverType type : list) {
                    labels.add(getLabel(type));
                }
                return "[" + list.size() + "/" + getValues().size() + "] " + labels.toString();
            }

            @Override
            public void onChanged() {
                super.onChanged();
                if (adjustingSolverTypes) {
                    return;
                }
                selectedSolverTypes.clear();
                selectedSolverTypes.addAll(getSelectedItems());
                onManualSolverTypeSelectionChange();
            }
        };
        solverTypePresetCombo = new JComboBox();
        solverTypePresetCombo.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                final JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof SolverTypePreselect) {
                    label.setText(getSolverTypePresetName((SolverTypePreselect) value));
                }
                return label;
            }
        });
        solverTypePresetCombo.addItem(SolverTypePreselect.ALL);
        solverTypePresetCombo.addItem(SolverTypePreselect.JAC);
        solverTypePresetCombo.addItem(SolverTypePreselect.MANUAL);
        solverTypePresetCombo.addItem(SolverTypePreselect.EXTERNAL);
        solverTypePresetCombo.addItem(SolverTypePreselect.CUSTOM);
        /* Default: external solvers only. */
        solverTypePresetCombo.setSelectedItem(SolverTypePreselect.EXTERNAL);
        selectedSolverTypes.addAll(getSolverTypePresetTypes(SolverTypePreselect.EXTERNAL));
        solverTypePresetCombo.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (!adjustingSolverTypes && solverTypePresetCombo.getSelectedItem() instanceof SolverTypePreselect) {
                    applySolverTypePreset((SolverTypePreselect) solverTypePresetCombo.getSelectedItem());
                }
            }
        });
        final MigPanel selectionBar = new MigPanel("ins 0, wrap 3, gapy 5", "[][fill][grow,fill]", "[][][]");
        selectionBar.add(new JLabel(_GUI.T.CaptchaSolverComparison_preselect_label()));
        selectionBar.add(presetCombo, "height 26!");
        selectionBar.add(typesCombo, "height 26!");
        selectionBar.add(new JLabel(_GUI.T.CaptchaSolverComparison_preselectSolverTypes_label()));
        selectionBar.add(solverTypePresetCombo, "height 26!");
        selectionBar.add(solverTypesCombo, "height 26!");
        selectionBar.add(new JLabel(_GUI.T.CaptchaSolverComparison_preselectSolvers_label()));
        selectionBar.add(solverPresetCombo, "height 26!");
        selectionBar.add(solversCombo, "height 26!");
        add(selectionBar, "growx");
        onlyFullSupportCheckBox = new JCheckBox(_GUI.T.CaptchaSolverComparison_onlyFullSupport());
        onlyFullSupportCheckBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                model.setOnlyFullSupport(onlyFullSupportCheckBox.isSelected());
            }
        });
        fullSupportHintLabel = new JLabel(_GUI.T.CaptchaSolverComparison_fullSupportImpossible());
        fullSupportHintLabel.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18));
        hintFont = fullSupportHintLabel.getFont();
        hintBoldFont = hintFont.deriveFont(Font.BOLD);
        hintColor = fullSupportHintLabel.getForeground();
        fullSupportHintLabel.setVisible(false);
        /* The warning is shown below the check box. */
        final MigPanel fullSupportBar = new MigPanel("ins 0, wrap 1", "[grow,fill]", "[][]");
        fullSupportBar.add(onlyFullSupportCheckBox);
        fullSupportBar.add(fullSupportHintLabel, "hidemode 3, wmin 0");
        add(fullSupportBar, "growx");
        scrollPane = table.createScrollPane();
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        /* Re-evaluate whether the columns fit whenever the available width changes. */
        scrollPane.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(final ComponentEvent e) {
                updateResizeMode();
            }
        });
        add(scrollPane, "grow");
        /* Only about external solvers: shown as long as at least one of them is in the table. */
        disclaimerLabel = new JLabel(_GUI.T.CaptchaSolverComparison_disclaimer(), NewTheme.I().getIcon(IconKey.ICON_INFO, 18), SwingConstants.LEADING);
        disclaimerLabel.setVisible(false);
        add(disclaimerLabel, "growx, wmin 10, hidemode 3");
        model.addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(final TableModelEvent e) {
                updateDisclaimerVisibility();
            }
        });
        /* Default: all captcha types selected. */
        selectedTypes.addAll(allTypes);
        syncTypesCombo();
        onSelectionChanged();
        refresh();
    }

    /**
     * Menu of the column header / the column button: show or hide the column with the number of supported captcha types, hide the captcha
     * type of the clicked column and select the captcha types. Hiding or showing a captcha type changes the captcha type selection above
     * the table (and with it the preset, like a manual change there).
     */
    private JPopupMenu createColumnMenu(final ExtColumn<SolverService> extColumn) {
        final JPopupMenu popup = new JPopupMenu();
        final CAPTCHA_TYPE clickedType = extColumn == null ? null : model.getCaptchaType(extColumn);
        if (clickedType != null) {
            final JMenuItem hideType = new JMenuItem(_GUI.T.CaptchaSolverComparison_columnMenu_hideType(clickedType.getDisplayName()), clickedType.getIcon());
            hideType.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    setTypeSelected(clickedType, false);
                }
            });
            popup.add(hideType);
            popup.addSeparator();
        }
        final ExtColumn<SolverService> countColumn = model.getSupportedCountColumn();
        final ExtCheckBoxMenuItem countItem = new ExtCheckBoxMenuItem(countColumn.getName());
        countItem.setHideOnClick(false);
        countItem.setSelected(model.isColumnVisible(countColumn));
        countItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                model.setColumnVisible(countColumn, countItem.isSelected());
            }
        });
        popup.add(countItem);
        popup.addSeparator();
        final JMenu typesMenu = new JMenu(_GUI.T.CaptchaSolverComparison_columnMenu_types());
        final ExtCheckBoxMenuItem selectAllItem = new ExtCheckBoxMenuItem(_GUI.T.CaptchaSolverComparison_columnMenu_selectAll());
        selectAllItem.setHideOnClick(false);
        selectAllItem.setSelected(selectedTypes.size() == allTypes.size());
        selectAllItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                selectedTypes.clear();
                if (selectAllItem.isSelected()) {
                    selectedTypes.addAll(allTypes);
                }
                syncTypesCombo();
                onManualSelectionChange();
            }
        });
        typesMenu.add(selectAllItem);
        typesMenu.addSeparator();
        for (final CAPTCHA_TYPE type : allTypes) {
            final ExtCheckBoxMenuItem typeItem = new ExtCheckBoxMenuItem(type.getDisplayName());
            typeItem.setIcon(type.getIcon());
            typeItem.setHideOnClick(false);
            typeItem.setSelected(selectedTypes.contains(type));
            typeItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    setTypeSelected(type, typeItem.isSelected());
                }
            });
            typesMenu.add(typeItem);
        }
        popup.add(typesMenu);
        return popup;
    }

    /** Selects or deselects one captcha type like the multi-select above the table does. */
    private void setTypeSelected(final CAPTCHA_TYPE type, final boolean selected) {
        if (selected) {
            selectedTypes.add(type);
        } else {
            selectedTypes.remove(type);
        }
        syncTypesCombo();
        onManualSelectionChange();
    }

    /**
     * Selects the given solvers in the table and scrolls to the first one. If one of them is not in the table because of the selections
     * and filters above it, they are reset first: all solver groups and solvers, all captcha types and the filter "only solvers that
     * support all selected captcha types" is switched off.
     */
    public void showSolvers(final java.util.Collection<SolverService> solvers) {
        refresh();
        boolean allShown = true;
        for (final SolverService solver : solvers) {
            allShown &= model.isShown(solver);
        }
        if (!allShown) {
            /* Solver groups and solvers: everything. */
            adjustingSolverTypes = true;
            try {
                solverTypePresetCombo.setSelectedItem(SolverTypePreselect.ALL);
            } finally {
                adjustingSolverTypes = false;
            }
            applySolverTypePreset(SolverTypePreselect.ALL);
            /* All captcha types. */
            rememberedSelection = new LinkedHashSet<CAPTCHA_TYPE>(allTypes);
            adjusting = true;
            try {
                presetCombo.setSelectedItem(Preselect.SELECTION);
            } finally {
                adjusting = false;
            }
            applyPreset(Preselect.SELECTION);
            onlyFullSupportCheckBox.setSelected(false);
            model.setOnlyFullSupport(false);
        }
        /* The rows are rebuilt on the EDT: select once that is done. */
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                table.getSelectionModel().clearSelection();
                int firstRow = -1;
                for (final SolverService solver : solvers) {
                    final int row = model.getRowforObject(solver);
                    if (row < 0) {
                        continue;
                    }
                    table.getSelectionModel().addSelectionInterval(row, row);
                    firstRow = firstRow < 0 ? row : Math.min(firstRow, row);
                }
                if (firstRow >= 0) {
                    table.scrollRectToVisible(table.getCellRect(firstRow, 0, true));
                }
            }
        });
    }

    /** Shows the hint below the table only if at least one external solver is in the table. */
    private void updateDisclaimerVisibility() {
        boolean externalSolverShown = false;
        for (final SolverService solver : model.getTableData()) {
            if (solver.getType() == SolverType.EXTERNAL) {
                externalSolverShown = true;
                break;
            }
        }
        disclaimerLabel.setVisible(externalSolverShown);
    }

    /** True if the given dropdown entry is greyed out, i.e. it cannot be selected: "Used by me" and the solver presets "All" / "usable". */
    private boolean isPresetDisabled(final Object preset) {
        if (preset == Preselect.USED) {
            return !usedAvailable;
        } else if (preset == SolverPreselect.USABLE) {
            return !solverUsableAvailable;
        }
        return false;
    }

    /** True if the mouse event is on a greyed out entry of the given popup list. */
    private boolean isDisabledPreset(final JList list, final MouseEvent e) {
        final int index = list.locationToIndex(e.getPoint());
        final Rectangle bounds = index >= 0 ? list.getCellBounds(index, index) : null;
        return bounds != null && bounds.contains(e.getPoint()) && isPresetDisabled(list.getModel().getElementAt(index));
    }

    /**
     * Makes clicks on the greyed out entry of the combo box's popup list have no effect at all: the popup stays open instead of closing and
     * jumping back to the previous entry. This is done by wrapping the popup list's own mouse listeners, which would otherwise select the
     * entry and close the popup.
     */
    private void installDisabledItemGuard(final JComboBox combo) {
        if (combo.getUI() == null) {
            return;
        }
        final Accessible accessible = combo.getUI().getAccessibleChild(combo, 0);
        if (!(accessible instanceof ComboPopup)) {
            return;
        }
        final JList list = ((ComboPopup) accessible).getList();
        if (list == null || list.getClientProperty("disabledItemGuard") != null) {
            return;
        }
        list.putClientProperty("disabledItemGuard", Boolean.TRUE);
        for (final MouseListener original : list.getMouseListeners()) {
            list.removeMouseListener(original);
            list.addMouseListener(new MouseListener() {
                @Override
                public void mouseClicked(final MouseEvent e) {
                    if (!isDisabledPreset(list, e)) {
                        original.mouseClicked(e);
                    }
                }

                @Override
                public void mousePressed(final MouseEvent e) {
                    if (!isDisabledPreset(list, e)) {
                        original.mousePressed(e);
                    }
                }

                @Override
                public void mouseReleased(final MouseEvent e) {
                    if (!isDisabledPreset(list, e)) {
                        original.mouseReleased(e);
                    }
                }

                @Override
                public void mouseEntered(final MouseEvent e) {
                    original.mouseEntered(e);
                }

                @Override
                public void mouseExited(final MouseEvent e) {
                    original.mouseExited(e);
                }
            });
        }
    }

    private static String getSolverPresetName(final SolverPreselect preset) {
        switch (preset) {
        case CUSTOM:
            return _GUI.T.CaptchaSolverComparison_preselectSolvers_custom();
        case ENABLED:
            return _GUI.T.CaptchaSolverComparison_preselectSolvers_enabled();
        case USABLE:
            return _GUI.T.CaptchaSolverComparison_preselectSolvers_usable();
        case ALL:
        default:
            return _GUI.T.CaptchaSolverComparison_preselectSolvers_all();
        }
    }

    /** The "usable" preset needs at least one enabled and usable solver; otherwise it is greyed out. */
    private void updateSolverPresetAvailability() {
        boolean anyUsable = false;
        for (final SolverService solver : allSolvers) {
            if (solver.getConfigV3().isEnabled() && solver.isReady()) {
                anyUsable = true;
            }
        }
        solverUsableAvailable = anyUsable;
    }

    /** Returns the solvers the given preset selects. */
    private Set<SolverService> getSolverPresetSolvers(final SolverPreselect preset) {
        final Set<SolverService> ret = new LinkedHashSet<SolverService>();
        for (final SolverService solver : allSolvers) {
            final boolean selected;
            switch (preset) {
            case CUSTOM:
                selected = rememberedSolverSelection == null || rememberedSolverSelection.contains(solver);
                break;
            case ENABLED:
                selected = solver.getConfigV3().isEnabled();
                break;
            case USABLE:
                selected = solver.getConfigV3().isEnabled() && solver.isReady();
                break;
            case ALL:
            default:
                selected = true;
                break;
            }
            if (selected) {
                ret.add(solver);
            }
        }
        return ret;
    }

    private void applySolverPreset(final SolverPreselect preset) {
        selectedSolvers.clear();
        selectedSolvers.addAll(getSolverPresetSolvers(preset));
        syncSolversCombo();
        onSolverSelectionChanged();
    }

    /**
     * Called when the user changed the solver selection manually: as soon as it differs from what the current preset selects, the
     * preselect dropdown switches to "Custom".
     */
    private void onManualSolverSelectionChange() {
        final Object currentPreset = solverPresetCombo.getSelectedItem();
        if (currentPreset == SolverPreselect.CUSTOM) {
            /* The user edits the selection itself -> remember it. */
            rememberedSolverSelection = new LinkedHashSet<SolverService>(selectedSolvers);
        } else if (currentPreset instanceof SolverPreselect && !selectedSolvers.equals(getSolverPresetSolvers((SolverPreselect) currentPreset))) {
            switchToCustomSolverSelection();
        }
        onSolverSelectionChanged();
    }

    /** Remembers the current solver selection as the "Custom" preset and switches the dropdown to it. */
    private void switchToCustomSolverSelection() {
        rememberedSolverSelection = new LinkedHashSet<SolverService>(selectedSolvers);
        adjustingSolvers = true;
        try {
            solverPresetCombo.setSelectedItem(SolverPreselect.CUSTOM);
        } finally {
            adjustingSolvers = false;
        }
    }

    /** Context menu "Hide from selection": deselects the given solvers, which turns the preset into "Custom". */
    private void hideFromSelection(final List<SolverService> solvers) {
        selectedSolvers.removeAll(solvers);
        switchToCustomSolverSelection();
        syncSolversCombo();
        onSolverSelectionChanged();
    }

    /** Context menu "Hide all others": keeps only the given solvers selected, which turns the preset into "Custom". */
    private void hideAllOthers(final List<SolverService> solvers) {
        selectedSolvers.retainAll(solvers);
        switchToCustomSolverSelection();
        syncSolversCombo();
        onSolverSelectionChanged();
    }

    /** Reflects {@link #selectedSolvers} in the multi-select control without triggering its own change handling. */
    private void syncSolversCombo() {
        adjustingSolvers = true;
        try {
            solversCombo.setSelectedItems(new ArrayList<SolverService>(selectedSolvers));
        } finally {
            adjustingSolvers = false;
        }
    }

    private void onSolverSelectionChanged() {
        model.setVisibleSolvers(selectedSolvers);
        updateFullSupportCheckBox();
        table.repaint();
    }

    /** The red hint shown inside the table if the selection leaves no row/column, or null if there is nothing to complain about. */
    private String getEmptyHint() {
        if (model.isEmptyDueToNoTypes()) {
            return _GUI.T.CaptchaSolverComparison_noTypeSelected();
        }
        if (selectedSolverTypes.isEmpty()) {
            return _GUI.T.CaptchaSolverComparison_noSolverTypeSelected();
        }
        if (model.isEmptyDueToNoSolvers()) {
            final Object preset = solverPresetCombo.getSelectedItem();
            if (preset == SolverPreselect.ENABLED) {
                return _GUI.T.CaptchaSolverComparison_noSolverEnabled();
            } else if (preset == SolverPreselect.USABLE) {
                return _GUI.T.CaptchaSolverComparison_noSolverUsable();
            }
            return _GUI.T.CaptchaSolverComparison_noSolverSelected();
        }
        return null;
    }

    private static String getPresetName(final Preselect preset) {
        switch (preset) {
        case SELECTION:
            return _GUI.T.CaptchaSolverComparison_preselect_selection();
        case USED:
            return _GUI.T.CaptchaSolverComparison_preselect_used();
        default:
            return _GUI.T.CaptchaSolverComparison_preselect_selection();
        }
    }

    /**
     * Reloads the rows (solvers of the selected solver types) and the offered presets. Call this whenever the tab becomes visible again, since solvers and the
     * captcha history change while the application is running.
     */
    public void refresh() {
        allServices.clear();
        allServices.addAll(ChallengeResponseController.getInstance().listServices());
        syncSolverTypesCombo();
        reloadSolvers(false);
        rebuildPresetCombo();
        updateFullSupportCheckBox();
    }

    /** Rebuilds the rows and the solver selection from all solvers of the selected solver types. */
    /**
     * @param resetToAll
     *            true if the solver types changed: then all solvers of the selected types are selected ("All by selected types"),
     *            whatever the solver preset was before. The remembered "Custom" selection is not touched. false: the current solver
     *            preset is applied again (enabled/usable solvers change while the application is running).
     */
    private void reloadSolvers(final boolean resetToAll) {
        final List<SolverService> typedSolvers = new ArrayList<SolverService>();
        for (final SolverService service : allServices) {
            if (selectedSolverTypes.contains(service.getType())) {
                typedSolvers.add(service);
            }
        }
        allSolvers.clear();
        allSolvers.addAll(typedSolvers);
        adjustingSolvers = true;
        try {
            solversCombo.setValues(new ArrayList<SolverService>(allSolvers));
        } finally {
            adjustingSolvers = false;
        }
        model.setSolvers(typedSolvers);
        updateSolverPresetAvailability();
        final Object currentSolverPreset = solverPresetCombo.getSelectedItem();
        if (!resetToAll && currentSolverPreset instanceof SolverPreselect && !isPresetDisabled(currentSolverPreset)) {
            applySolverPreset((SolverPreselect) currentSolverPreset);
            return;
        }
        /* Solver types changed or the chosen preset is not available anymore: all solvers of the selected types. */
        adjustingSolvers = true;
        try {
            solverPresetCombo.setSelectedItem(SolverPreselect.ALL);
        } finally {
            adjustingSolvers = false;
        }
        applySolverPreset(SolverPreselect.ALL);
    }

    private static String getSolverTypePresetName(final SolverTypePreselect preset) {
        switch (preset) {
        case JAC:
            /* Same text as the name of the solver itself. */
            return _GUI.T.JACSolver_getName_();
        case MANUAL:
            return _GUI.T.CaptchaSolverComparison_preselectSolverTypes_manual();
        case EXTERNAL:
            /* Same text as the type of the external solvers. */
            return _GUI.T.CaptchaSolverService_type();
        case CUSTOM:
            return _GUI.T.CaptchaSolverComparison_preselectSolverTypes_custom();
        case ALL:
        default:
            return _GUI.T.CaptchaSolverComparison_preselectSolverTypes_all();
        }
    }

    /** Returns the solver types the given preset selects. */
    private Set<SolverType> getSolverTypePresetTypes(final SolverTypePreselect preset) {
        final Set<SolverType> ret = new LinkedHashSet<SolverType>();
        switch (preset) {
        case JAC:
            ret.add(SolverType.JD_LOCAL);
            break;
        case MANUAL:
            ret.add(SolverType.JD_LOCAL_DIALOG);
            ret.add(SolverType.JD_LOCAL_BROWSER);
            ret.add(SolverType.JD_REMOTE_MYJD);
            break;
        case CUSTOM:
            if (rememberedSolverTypeSelection != null) {
                ret.addAll(rememberedSolverTypeSelection);
                break;
            }
            /* Never populated: external solvers, like the default. */
            ret.add(SolverType.EXTERNAL);
            break;
        case ALL:
            for (final SolverType type : SolverType.values()) {
                ret.add(type);
            }
            break;
        case EXTERNAL:
        default:
            ret.add(SolverType.EXTERNAL);
            break;
        }
        return ret;
    }

    private void applySolverTypePreset(final SolverTypePreselect preset) {
        selectedSolverTypes.clear();
        selectedSolverTypes.addAll(getSolverTypePresetTypes(preset));
        syncSolverTypesCombo();
        onSolverTypeSelectionChanged();
    }

    /**
     * Called when the user changed the solver type selection manually: as soon as it differs from what the current preset selects, the
     * preselect dropdown switches to "Custom".
     */
    private void onManualSolverTypeSelectionChange() {
        final Object currentPreset = solverTypePresetCombo.getSelectedItem();
        if (currentPreset == SolverTypePreselect.CUSTOM) {
            /* The user edits the selection itself -> remember it. */
            rememberedSolverTypeSelection = new LinkedHashSet<SolverType>(selectedSolverTypes);
        } else if (currentPreset instanceof SolverTypePreselect && !selectedSolverTypes.equals(getSolverTypePresetTypes((SolverTypePreselect) currentPreset))) {
            rememberedSolverTypeSelection = new LinkedHashSet<SolverType>(selectedSolverTypes);
            adjustingSolverTypes = true;
            try {
                solverTypePresetCombo.setSelectedItem(SolverTypePreselect.CUSTOM);
            } finally {
                adjustingSolverTypes = false;
            }
        }
        onSolverTypeSelectionChanged();
    }

    /** Reflects {@link #selectedSolverTypes} in the multi-select control without triggering its own change handling. */
    private void syncSolverTypesCombo() {
        adjustingSolverTypes = true;
        try {
            solverTypesCombo.setSelectedItems(new ArrayList<SolverType>(selectedSolverTypes));
        } finally {
            adjustingSolverTypes = false;
        }
    }

    private void onSolverTypeSelectionChanged() {
        reloadSolvers(true);
        updateFullSupportCheckBox();
        table.repaint();
    }

    /** "Used by me" is always listed, but greyed out and not selectable if the captcha history contains no captcha type. */
    private void rebuildPresetCombo() {
        final Object current = presetCombo.getSelectedItem();
        usedAvailable = !getUsedTypes().isEmpty();
        Preselect newSelection = Preselect.SELECTION;
        boolean presetNotAvailableAnymore = false;
        if (current instanceof Preselect) {
            switch ((Preselect) current) {
            case USED:
                if (usedAvailable) {
                    newSelection = Preselect.USED;
                } else {
                    /* The previously chosen preset is not available anymore (history got cleared). */
                    presetNotAvailableAnymore = true;
                }
                break;
            case SELECTION:
            default:
                newSelection = (Preselect) current;
                break;
            }
        }
        adjusting = true;
        try {
            presetCombo.removeAllItems();
            presetCombo.addItem(Preselect.SELECTION);
            presetCombo.addItem(Preselect.USED);
            presetCombo.setSelectedItem(newSelection);
        } finally {
            adjusting = false;
        }
        if (presetNotAvailableAnymore) {
            applyPreset(newSelection);
        }
    }

    /** Returns all captcha types the user ever needed according to the captcha history (only types JDownloader can process). */
    private Set<CAPTCHA_TYPE> getUsedTypes() {
        final Set<CAPTCHA_TYPE> usedTypes = new LinkedHashSet<CAPTCHA_TYPE>();
        for (final CaptchaHistoryEntry entry : CaptchaHistoryManager.getInstance().getAllEntries()) {
            final CAPTCHA_TYPE type = entry.getCaptcha_type();
            if (type != null && allTypes.contains(type)) {
                usedTypes.add(type);
            }
        }
        return usedTypes;
    }

    /** Returns the captcha types the given preset selects. */
    private Set<CAPTCHA_TYPE> getPresetTypes(final Preselect preset) {
        switch (preset) {
        case USED:
            return getUsedTypes();
        case SELECTION:
            if (rememberedSelection != null) {
                return new LinkedHashSet<CAPTCHA_TYPE>(rememberedSelection);
            }
            return new LinkedHashSet<CAPTCHA_TYPE>(allTypes);
        default:
            return new LinkedHashSet<CAPTCHA_TYPE>(allTypes);
        }
    }

    private void applyPreset(final Preselect preset) {
        selectedTypes.clear();
        selectedTypes.addAll(getPresetTypes(preset));
        syncTypesCombo();
        onSelectionChanged();
    }

    /**
     * Called when the user changed the captcha type selection manually: as soon as it differs from what the current preset selects, the
     * preselect dropdown switches to "Selection".
     */
    private void onManualSelectionChange() {
        final Object currentPreset = presetCombo.getSelectedItem();
        if (currentPreset instanceof Preselect) {
            switch ((Preselect) currentPreset) {
            case SELECTION:
                /* The user edits the selection itself -> remember it. */
                rememberedSelection = new LinkedHashSet<CAPTCHA_TYPE>(selectedTypes);
                break;
            case USED:
            default:
                if (!selectedTypes.equals(getPresetTypes((Preselect) currentPreset))) {
                    /* Jumping to "Selection" always overwrites the remembered selection, whether there was one before or not. */
                    rememberedSelection = new LinkedHashSet<CAPTCHA_TYPE>(selectedTypes);
                    adjusting = true;
                    try {
                        presetCombo.setSelectedItem(Preselect.SELECTION);
                    } finally {
                        adjusting = false;
                    }
                }
                break;
            }
        }
        onSelectionChanged();
    }

    /** Reflects {@link #selectedTypes} in the multi-select control without triggering its own change handling (see {@link #adjusting}). */
    private void syncTypesCombo() {
        adjusting = true;
        try {
            typesCombo.setSelectedItems(new ArrayList<CAPTCHA_TYPE>(selectedTypes));
        } finally {
            adjusting = false;
        }
    }

    private void onSelectionChanged() {
        /* "Used by me": list solvers first that support the most (ideally all) of the captcha types the user needs. */
        model.setVisibleTypes(selectedTypes, presetCombo.getSelectedItem() == Preselect.USED);
        updateFullSupportCheckBox();
        updateResizeMode();
    }

    /**
     * Greys out the "only full support" check box (and tells why) if no solver supports all selected captcha types: Applying it would
     * hide every solver. The check box keeps its state, the filter applies again as soon as the selection allows it.
     */
    private void updateFullSupportCheckBox() {
        final boolean impossible = model.isFullSupportImpossible();
        onlyFullSupportCheckBox.setEnabled(!impossible);
        fullSupportHintLabel.setVisible(impossible);
        final boolean warning = impossible && onlyFullSupportCheckBox.isSelected();
        fullSupportHintLabel.setForeground(warning ? Color.RED : hintColor);
        fullSupportHintLabel.setFont(warning ? hintBoldFont : hintFont);
    }

    /**
     * Few columns: stretch them over the full table width. So many columns that they do not fit with their default width: keep the
     * default width and scroll horizontally.
     */
    private void updateResizeMode() {
        int neededWidth = model.getSolverColumnWidth();
        for (final CAPTCHA_TYPE type : selectedTypes) {
            neededWidth += SolverComparisonTableModel.getTypeColumnDefaultWidth(type);
        }
        final int available = scrollPane.getViewport().getWidth();
        final int mode = neededWidth <= available ? JTable.AUTO_RESIZE_ALL_COLUMNS : JTable.AUTO_RESIZE_OFF;
        if (table.getAutoResizeMode() != mode) {
            table.setAutoResizeMode(mode);
        }
    }

    @Override
    public String getConstraints() {
        return "height 60:n:n,pushy,growy";
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}
