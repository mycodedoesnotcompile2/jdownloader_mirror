package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.MigPanel;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.CaptchaSolverCaptchaTypesSettingsPanelBuilder;
import org.jdownloader.captcha.v2.CaptchaSolverCaptchaTypesSettingsPanelBuilder.SolverServiceCaptchaTypeAccessor;
import org.jdownloader.captcha.v2.CaptchaSolverLimitRule;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.SettingsButton;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.PluginConfigPanelNG;

public class SolverOrderContainer extends org.appwork.swing.MigPanel implements SettingsComponent {
    private final SolverOrderTable     solverOrder;
    private BasicJDTable<CAPTCHA_TYPE> detailTable;
    private JScrollPane                detailScrollPane;
    private JLabel                     detailLabel;
    private JLabel                     descriptionLabel;
    private JLabel                     limitsLabel;
    private JLabel                     settingsLabel;
    private JScrollPane                configScrollPane;
    private MigPanel                   displayModePanel;
    /** "Enable custom limits" switch below the solver's settings, only shown for external (plugin based) solvers. */
    private JCheckBox                  customLimitsCheckBox;
    /** Holds the custom limits table, only filled and visible while the "Enable custom limits" switch is on. */
    private MigPanel                   customLimitsContainer;
    /** The solver currently shown below the solver table, or null if none is selected. */
    private SolverService              selectedSolver;
    /** The generic config panel of the selected solver (knows which config to reset), or null if no solver is selected. */
    private PluginConfigPanelNG        configPanel;
    /** Resets all settings of the selected solver. */
    private SettingsButton             resetButton;
    private SolverOrderTable.SelectionListener selectionListener;
    /** The config keys of the selected solver which are observed to keep the reset button's enabled state up to date. */
    @SuppressWarnings("rawtypes")
    private final List<KeyHandler>             observedKeys         = new ArrayList<KeyHandler>();
    private final GenericConfigEventListener<Object> defaultStateListener = new GenericConfigEventListener<Object>() {
        @Override
        public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
            updateResetButtonState();
        }

        @Override
        public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
        }
    };
    /**
     * Display mode of the captcha types table: true = only the captcha types the solver supports, false = all captcha types. Global, i.e.
     * not stored per solver, so it stays as the user last chose it when another solver gets selected. Not persisted: after a restart the
     * default (supported types only) applies again.
     */
    private boolean                    showOnlySupportedCaptchaTypes = true;

    public SolverOrderContainer(SolverOrderTable urlOrder) {
        /* gapy 0: no implicit inter-row gaps, so the height reserved in getConstraints() stays exact (all gaps are set explicitly). */
        super("ins 0, gapy 0", "[grow,fill]", "[][][][][][][][][][][]");
        this.solverOrder = urlOrder;
        /* Main solver order table. Slightly narrower on the right; never shows any scrollbar. */
        final JScrollPane sp = new JScrollPane(urlOrder);
        sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        passMouseWheelToParent(sp);
        SwingUtils.setOpaque(this, false);
        add(sp, "gapright 40, wrap");
        /* Header label shown above the detail table; separated from the solver table above by a gap. */
        detailLabel = new JLabel();
        detailLabel.setVisible(false);
        add(detailLabel, "gaptop 15, wrap");
        // Optional per-solver description shown above the detail table
        descriptionLabel = new JLabel();
        descriptionLabel.setVisible(false);
        add(descriptionLabel, "growx, wmin 10, gapright 40, wrap");
        /* "Captcha types to display" switch above the captcha types table: supported captcha types only vs. all captcha types. */
        displayModePanel = new MigPanel("ins 0", "[][][]", "[]");
        SwingUtils.setOpaque(displayModePanel, false);
        final JRadioButton supportedOnlyRadio = new JRadioButton(_GUI.T.CaptchaTypesTable_displayMode_supportedOnly(), showOnlySupportedCaptchaTypes);
        final JRadioButton allRadio = new JRadioButton(_GUI.T.CaptchaTypesTable_displayMode_all(), !showOnlySupportedCaptchaTypes);
        supportedOnlyRadio.setOpaque(false);
        allRadio.setOpaque(false);
        final ButtonGroup displayModeGroup = new ButtonGroup();
        displayModeGroup.add(supportedOnlyRadio);
        displayModeGroup.add(allRadio);
        supportedOnlyRadio.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                setShowOnlySupportedCaptchaTypes(true);
            }
        });
        allRadio.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                setShowOnlySupportedCaptchaTypes(false);
            }
        });
        displayModePanel.add(new JLabel(_GUI.T.CaptchaTypesTable_displayMode()));
        displayModePanel.add(supportedOnlyRadio);
        displayModePanel.add(allRadio);
        displayModePanel.setVisible(false);
        add(displayModePanel, "gaptop 6, wrap");
        // Detail table, initially hidden
        detailScrollPane = new JScrollPane();
        /* Never scroll: the detail table is always shown at full size (all rows). */
        detailScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        detailScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        detailScrollPane.setVisible(false);
        passMouseWheelToParent(detailScrollPane);
        add(detailScrollPane, "gaptop 6, growx, gapright 40, wrap");
        /* Server-side limits info line, shown below the captcha-types table for plugin-based (account) solvers only. */
        limitsLabel = new JLabel();
        limitsLabel.setVisible(false);
        add(limitsLabel, "gaptop 6, wrap");
        /* "<solver> Settings" section header (gear icon) shown BELOW the captcha-types table, above the config panel. */
        settingsLabel = new JLabel("Settings", NewTheme.I().getIcon(IconKey.ICON_SETTINGS, 18), JLabel.LEADING);
        settingsLabel.setVisible(false);
        add(settingsLabel, "gaptop 12, wrap");
        /* Config panel of the selected solver, shown below the "Settings" header. */
        configScrollPane = new JScrollPane();
        configScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        /*
         * No border/background: the settings are meant to be on the same level (left edge) as the items around them, like the "Enable
         * custom limit rules" switch below, and not look like they lie in an extra panel.
         */
        configScrollPane.setBorder(null);
        configScrollPane.setViewportBorder(null);
        configScrollPane.setOpaque(false);
        configScrollPane.getViewport().setOpaque(false);
        /*
         * Like the two scroll panes above, this one is sized to show its content in full, so it never needs its own scrollbar; forward the
         * mouse wheel to the surrounding scroll pane so scrolling keeps working while the mouse is over the solver's settings panel.
         */
        passMouseWheelToParent(configScrollPane);
        configScrollPane.setVisible(false);
        add(configScrollPane, "gaptop 6, growx, wrap");
        /* Last item of the solver's settings: switch for the custom limits, only shown for external solvers. */
        customLimitsCheckBox = new JCheckBox(_GUI.T.CaptchaSolverLimits_enable());
        customLimitsCheckBox.setToolTipText(_GUI.T.CaptchaSolverLimits_enable_tooltip());
        customLimitsCheckBox.setOpaque(false);
        customLimitsCheckBox.setVisible(false);
        customLimitsCheckBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                onCustomLimitsToggled();
            }
        });
        add(customLimitsCheckBox, "gaptop 6, wrap");
        /* The limits table below the switch, only shown while the switch is on. */
        customLimitsContainer = new MigPanel("ins 0", "[grow,fill]", "[]");
        SwingUtils.setOpaque(customLimitsContainer, false);
        customLimitsContainer.setVisible(false);
        add(customLimitsContainer, "gaptop 6, growx, gapright 40, wrap");
        /* Resets all settings of the selected solver (similar to the reset button of the plugin settings). */
        resetButton = new SettingsButton(new AppAction() {
            {
                setIconKey(IconKey.ICON_RESET);
                setName(_GUI.T.lit_reset());
                setTooltipText(_GUI.T.SolverOrderContainer_reset_tooltip());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                resetSelectedSolver();
            }
        });
        resetButton.setVisible(false);
        add(resetButton, "gaptop 12, alignx right, gapright 40");
        selectionListener = new SolverOrderTable.SelectionListener() {
            @Override
            public void onSolverSelected(SolverService solver) {
                selectedSolver = solver;
                if (solver == null) {
                    detailLabel.setVisible(false);
                    descriptionLabel.setVisible(false);
                    displayModePanel.setVisible(false);
                    limitsLabel.setVisible(false);
                    settingsLabel.setVisible(false);
                    detailScrollPane.setVisible(false);
                    detailTable = null;
                    configScrollPane.setViewportView(null);
                    configPanel = null;
                    configScrollPane.setVisible(false);
                    updateCustomLimits(null);
                } else {
                    // Header label, prefixed with the selected solver's icon
                    detailLabel.setIcon(solver.getIcon(18));
                    detailLabel.setText(solver.getName() + ": Supported captcha types overview and settings");
                    detailLabel.setVisible(true);
                    /*
                     * Description above the table (no icon; the icon is shown on the header above): the solver's own description plus a
                     * hint that unchecking a captcha type disables it globally for this solver. Always shown when a solver is selected.
                     */
                    final StringBuilder description = new StringBuilder("<html>");
                    final String descriptionText = solver.getDescription();
                    if (descriptionText != null && descriptionText.length() > 0) {
                        description.append(descriptionText).append("<br>");
                    }
                    description.append("Unchecking a captcha type will disable it globally for this solver.");
                    description.append("</html>");
                    descriptionLabel.setText(description.toString());
                    descriptionLabel.setVisible(true);
                    /* Settings header below the table names the selected solver, e.g. "9kw.eu Settings". */
                    settingsLabel.setText(solver.getName() + " Settings");
                    settingsLabel.setVisible(true);
                    displayModePanel.setVisible(true);
                    buildDetailTable(solver);
                    /*
                     * Server-side limits info line: only meaningful for plugin-based (account) solvers, which are the only ones that
                     * implement getServerSideMaxSimultaneousCaptchaThreadsLimit()/getServerSideMaxPollingTimeoutMillis().
                     */
                    if (solver instanceof PluginForCaptchaSolverSolverService) {
                        final PluginForCaptchaSolverSolverService pluginSolver = (PluginForCaptchaSolverSolverService) solver;
                        final int maxThreads = pluginSolver.getServerSideMaxSimultaneousCaptchaThreadsLimit();
                        final long maxPollingMillis = pluginSolver.getServerSideMaxPollingTimeoutMillis();
                        final String maxThreadsText = maxThreads == Integer.MAX_VALUE ? "~" : String.valueOf(maxThreads);
                        final String maxPollingText = maxPollingMillis == Long.MAX_VALUE ? "~" : String.valueOf(maxPollingMillis / 1000L);
                        final long minPollingIntervalMillis = pluginSolver.getServerSideMinPollingIntervalMillis();
                        final String minPollingIntervalText = minPollingIntervalMillis <= 0 ? "~" : (minPollingIntervalMillis % 1000 == 0 ? String.valueOf(minPollingIntervalMillis / 1000) : String.valueOf(minPollingIntervalMillis / 1000.0d));
                        limitsLabel.setText("<html>Max concurrent captcha threads per account: " + maxThreadsText + "<br>Server side max polling time: " + maxPollingText + "s<br>Server side min polling interval: " + minPollingIntervalText + (minPollingIntervalMillis <= 0 ? "" : "s") + "</html>");
                        limitsLabel.setVisible(true);
                    } else {
                        limitsLabel.setVisible(false);
                    }
                    /* Show the selected solver's config below the table (plugin config if available, else the local solver's config). */
                    /* Built generically from the solver's V3 config interface, just like plugin config panels. */
                    final PluginConfigPanelNG configComponent = new PluginConfigPanelNG() {
                        @Override
                        public void updateContents() {
                        }

                        @Override
                        public void save() {
                        }
                    };
                    configComponent.build(solver.getConfigV3());
                    configPanel = configComponent;
                    /* Width tracking wrapper: long setting descriptions wrap instead of pushing the input fields out of the visible area. */
                    final WidthTrackingPanel configWrapper = new WidthTrackingPanel("ins 0", "[grow,fill]", "[]");
                    configWrapper.add(configComponent, "growx, wmin 10");
                    if (solver instanceof BrowserSolverService) {
                        /* The browser command line has no generic editor (hidden config entry), so offer the same dialog as the table does. */
                        final BrowserSolverService browserSolver = (BrowserSolverService) solver;
                        final JButton chooseBrowser = new JButton(_GUI.T.SolverOrderContainer_chooseBrowser(), new AbstractIcon(IconKey.ICON_BROWSE, 16));
                        chooseBrowser.addActionListener(new ActionListener() {
                            @Override
                            public void actionPerformed(final ActionEvent e) {
                                solverOrder.showSelectBrowserDialog(browserSolver);
                            }
                        });
                        configWrapper.add(chooseBrowser, "newline, gaptop 5, growx 0, alignx left");
                    }
                    configScrollPane.setViewportView(configWrapper);
                    configScrollPane.setVisible(true);
                    updateCustomLimits(solver);
                }
                resetButton.setVisible(solver != null);
                observeConfig(solver);
                refreshLayout();
                /* Second pass: texts which got wrapped by the first layout pass have a different height now. */
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        refreshLayout();
                    }
                });
            }
        };
        urlOrder.setSelectionListener(selectionListener);
    }

    /**
     * Makes the reset button follow the state of the given solver's config: it is only enabled while at least one setting differs from its
     * default. Listens to all keys of the solver's config (and stops listening to the ones of the previously selected solver).
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    private void observeConfig(final SolverService solver) {
        for (final KeyHandler key : observedKeys) {
            key.getEventSender().removeListener(defaultStateListener);
        }
        observedKeys.clear();
        if (solver != null) {
            for (final KeyHandler key : solver.getConfigV3()._getStorageHandler().getKeyHandler()) {
                /* Weak listener: defaultStateListener is a field, so it stays alive as long as this container. */
                key.getEventSender().addListener(defaultStateListener, true);
                observedKeys.add(key);
            }
        }
        updateResetButtonState();
    }

    /** Enables the reset button only if the selected solver has at least one setting which differs from its default. */
    private void updateResetButtonState() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final SolverService solver = selectedSolver;
                resetButton.setEnabled(solver != null && !isAllDefault(solver.getConfigV3()));
            }
        };
    }

    /** True if every key of the given config has its default value. An empty collection/map counts as the same as no value. */
    @SuppressWarnings({ "rawtypes" })
    private static boolean isAllDefault(final ConfigInterface cfg) {
        for (final KeyHandler key : cfg._getStorageHandler().getKeyHandler()) {
            final Object value = emptyToNull(key.getValue());
            final Object defaultValue = emptyToNull(key.getDefaultValue());
            if (value == null ? defaultValue != null : !value.equals(defaultValue)) {
                return false;
            }
        }
        return true;
    }

    private static Object emptyToNull(final Object value) {
        if (value instanceof Collection && ((Collection<?>) value).isEmpty()) {
            return null;
        } else if (value instanceof Map && ((Map<?, ?>) value).isEmpty()) {
            return null;
        }
        return value;
    }

    /**
     * Resets all settings of the selected solver to their defaults (all keys of its config, e.g. enabled state, disabled captcha types,
     * wait times and custom limits) after the user confirmed it, and rebuilds the solver's section so it shows the defaults.
     */
    private void resetSelectedSolver() {
        final SolverService solver = selectedSolver;
        if (solver == null || configPanel == null) {
            return;
        }
        if (!UIOManager.I().showConfirmDialog(0, _GUI.T.lit_are_you_sure(), _GUI.T.SolverOrderContainer_reset_are_you_sure(solver.getName()), new AbstractIcon(IconKey.ICON_QUESTION, 32), _GUI.T.lit_yes(), null)) {
            return;
        }
        configPanel.reset();
        /* Rebuild everything which depends on the config (captcha types table, config panel, custom limits). */
        selectionListener.onSolverSelected(solver);
    }

    /**
     * Builds the captcha types table of the given solver for the current display mode (see {@link #showOnlySupportedCaptchaTypes}) and
     * shows it, sized to fit.
     */
    private void buildDetailTable(final SolverService solver) {
        // Build detail table from a no-account builder
        final CaptchaSolverCaptchaTypesSettingsPanelBuilder builder = new CaptchaSolverCaptchaTypesSettingsPanelBuilder(new SolverServiceCaptchaTypeAccessor(solver), showOnlySupportedCaptchaTypes);
        detailTable = builder.getCaptchaTypesTable();
        /* Size the scroll pane to fit header plus all rows so every row is shown without a scrollbar. */
        final int fullHeight = detailTableFullHeight();
        final Dimension fullSize = new Dimension(detailTable.getPreferredSize().width, fullHeight);
        detailTable.setPreferredScrollableViewportSize(new Dimension(detailTable.getPreferredSize().width, detailTable.getPreferredSize().height));
        detailScrollPane.setViewportView(detailTable);
        detailScrollPane.setPreferredSize(fullSize);
        detailScrollPane.setMinimumSize(fullSize);
        detailScrollPane.setVisible(true);
    }

    /**
     * Switches the display mode of the captcha types table. The mode is global: it stays the same when another solver gets selected. The
     * table of the currently selected solver (if any) is rebuilt, since its row count and with it the height reserved in
     * {@link #getConstraints()} change.
     */
    private void setShowOnlySupportedCaptchaTypes(final boolean showOnlySupported) {
        if (this.showOnlySupportedCaptchaTypes == showOnlySupported) {
            return;
        }
        this.showOnlySupportedCaptchaTypes = showOnlySupported;
        if (selectedSolver != null) {
            buildDetailTable(selectedSolver);
            refreshLayout();
        }
    }

    private void refreshLayout() {
        revalidate();
        repaint();
        if (getParent() != null) {
            getParent().revalidate();
            getParent().repaint();
        }
    }

    /**
     * Shows the "Enable custom limits" switch for the given solver (reflecting its stored state) and, if the switch is on, the table with
     * its limits. External (plugin based) solvers are the only ones with custom limits, for every other solver (or none) both are hidden.
     */
    private void updateCustomLimits(final SolverService solver) {
        if (!(solver instanceof PluginForCaptchaSolverSolverService)) {
            customLimitsCheckBox.setVisible(false);
            showCustomLimitsTable(null);
            return;
        }
        final CaptchaSolverPluginConfig cfg = ((PluginForCaptchaSolverSolverService) solver).getPluginConfig();
        customLimitsCheckBox.setSelected(cfg.isCustomLimitsEnabled());
        customLimitsCheckBox.setVisible(true);
        showCustomLimitsTable(cfg.isCustomLimitsEnabled() ? cfg : null);
    }

    /** Fills the limits container with the limits table of the given config, or hides it if the config is null. */
    private void showCustomLimitsTable(final CaptchaSolverPluginConfig cfg) {
        customLimitsContainer.removeAll();
        if (cfg != null) {
            customLimitsContainer.add(new CaptchaSolverLimitsPanel(cfg, new Runnable() {
                @Override
                public void run() {
                    refreshLayout();
                }
            }), "growx");
        }
        customLimitsContainer.setVisible(cfg != null);
    }

    /** The user toggled "Enable custom limits" for the selected solver. */
    private void onCustomLimitsToggled() {
        if (!(selectedSolver instanceof PluginForCaptchaSolverSolverService)) {
            return;
        }
        final CaptchaSolverPluginConfig cfg = ((PluginForCaptchaSolverSolverService) selectedSolver).getPluginConfig();
        final boolean enabled = customLimitsCheckBox.isSelected();
        if (enabled && cfg.getLimitRules() == null) {
            /* First time the limits get enabled: offer the (disabled) example rules. An existing empty list means "deliberately cleared". */
            cfg.setLimitRules(CaptchaSolverLimitRule.createExampleRules());
        }
        cfg.setCustomLimitsEnabled(enabled);
        showCustomLimitsTable(enabled ? cfg : null);
        refreshLayout();
    }

    /**
     * A scroll pane without scrollbars still consumes mouse wheel events, so scrolling over such a table would do nothing. Replaces the
     * scroll pane's own wheel handling by one that forwards the event to the next scroll pane above it (the panel that actually scrolls).
     */
    static void passMouseWheelToParent(final JScrollPane scrollPane) {
        for (final MouseWheelListener listener : scrollPane.getMouseWheelListeners()) {
            scrollPane.removeMouseWheelListener(listener);
        }
        scrollPane.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(final MouseWheelEvent e) {
                final Container parent = SwingUtilities.getAncestorOfClass(JScrollPane.class, scrollPane);
                if (parent != null) {
                    parent.dispatchEvent(SwingUtilities.convertMouseEvent(scrollPane, e, parent));
                }
            }
        });
    }

    /**
     * Full pixel height of a table (header + all rows) plus a few pixels for the scroll pane border, so the last row is never clipped and
     * no scrollbar is needed. getPreferredSize() already accounts for row height and inter-cell spacing of all rows. <br>
     * 2026-09-21: Please don't ask me why but without adding these extra 10px, also e.g. with a value lower than 10px extra, the last table
     * item will be partly cut and there will be a scrollbar which we don't want.
     */
    static int tableFullHeight(final BasicJDTable<?> table) {
        if (table == null) {
            return 0;
        }
        final int headerHeight = table.getTableHeader() != null ? table.getTableHeader().getPreferredSize().height : 0;
        return headerHeight + table.getPreferredSize().height + 10;
    }

    private int detailTableFullHeight() {
        return tableFullHeight(detailTable);
    }

    /*
     * The total height is summed from the exact preferred heights of the visible components plus the explicit gaptop gaps used in the
     * layout (the layout sets gapy 0, so there are no implicit inter-row gaps to account for). This keeps the reserved height exact: the
     * tables are shown in full (no clipping) and there is no leftover space that would make the surrounding panel scrollable.
     */
    @Override
    public String getConstraints() {
        int height = tableFullHeight(solverOrder);
        if (detailScrollPane.isVisible() && detailTable != null) {
            /* gaptop 15 above the header label. */
            height += 15 + detailLabel.getPreferredSize().height;
            if (descriptionLabel.isVisible()) {
                height += descriptionLabel.getPreferredSize().height;
            }
            if (displayModePanel.isVisible()) {
                /* gaptop 6 above the "Captcha types to display" switch. */
                height += 6 + displayModePanel.getPreferredSize().height;
            }
            /* gaptop 6 above the captcha-types table. */
            height += 6 + detailTableFullHeight();
            if (limitsLabel.isVisible()) {
                /* gaptop 6 above the limits info line. */
                height += 6 + limitsLabel.getPreferredSize().height;
            }
            if (settingsLabel.isVisible()) {
                /* gaptop 12 above the "Settings" header. */
                height += 12 + settingsLabel.getPreferredSize().height;
            }
        }
        if (configScrollPane.isVisible() && configScrollPane.getViewport() != null && configScrollPane.getViewport().getView() != null) {
            /* gaptop 6 above the config panel. */
            height += 6 + configScrollPane.getViewport().getView().getPreferredSize().height;
        }
        if (customLimitsCheckBox.isVisible()) {
            /* gaptop 6 above the "Enable custom limits" switch. */
            height += 6 + customLimitsCheckBox.getPreferredSize().height;
        }
        if (customLimitsContainer.isVisible()) {
            /* gaptop 6 above the custom limits table. */
            height += 6 + customLimitsContainer.getPreferredSize().height;
        }
        if (resetButton.isVisible()) {
            /* gaptop 12 above the reset button. */
            height += 12 + resetButton.getPreferredSize().height;
        }
        return "height " + height + "!, wmin 10";
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}