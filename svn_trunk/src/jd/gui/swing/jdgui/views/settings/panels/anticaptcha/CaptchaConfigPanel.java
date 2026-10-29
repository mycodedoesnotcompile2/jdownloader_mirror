package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.settings.Pair;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;
import jd.gui.swing.jdgui.views.settings.components.Checkbox;

public class CaptchaConfigPanel extends AbstractConfigPanel {
    private static final long         serialVersionUID = 1L;
    // private CESSettingsPanel psp;
    private SolverOrderTable          solverOrderTable;
    private SolverComparisonContainer solverComparisonContainer;
    private CaptchaSettingsTabbedPane tabs;
    private JScrollPane               solversScrollPane;
    private CaptchaTestPanel          testPanel;
    private JScrollPane               testScrollPane;

    public String getTitle() {
        return _GUI.T.AntiCaptchaConfigPanel_getTitle();
    }

    @Override
    public String getSidebarTooltip() {
        return _GUI.T.SettingsSidebar_tooltip_Captcha();
    }

    public CaptchaConfigPanel() {
        super();
        debugTabShown = false;
        this.addHeader(getTitle(), new AbstractIcon(IconKey.ICON_OCR, 32));
        this.addDescriptionPlain(_GUI.T.AntiCaptchaConfigPanel_onShow_description());
        final Pair<Checkbox> useExternalSolverAccounts = addPair(_GUI.T.CaptchaConfigPanel_useExternalSolverAccounts(), null, new Checkbox(CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS));
        final Pair<Checkbox> avoidAutoSolverForLoginCaptchas = addPair(_GUI.T.CaptchaConfigPanel_avoidAutoSolverForLoginCaptchas(), null, new Checkbox(CFG_CAPTCHA.AVOID_AUTO_SOLVER_FOR_LOGIN_CAPTCHAS));
        avoidAutoSolverForLoginCaptchas.setToolTipText(_GUI.T.CaptchaConfigPanel_avoidAutoSolverForLoginCaptchas_tooltip());
        avoidAutoSolverForLoginCaptchas.setConditionPair(useExternalSolverAccounts);
        /* Tabbed area at the top: solver overview and captcha rules (similar to the Linkgrabber Filter panel). */
        final SolverOrderTable table = this.solverOrderTable = new SolverOrderTable();
        final SolverOrderContainer container = new SolverOrderContainer(table);
        /* Width tracking: long texts in the solver settings must not make the panel wider than the visible area. */
        final MigPanel solversTab = new WidthTrackingPanel("ins 0, wrap 1", "[grow,fill]", "[grow,fill]");
        solversTab.add(container, "grow");
        final CaptchaSettingsTabbedPane tabs = this.tabs = new CaptchaSettingsTabbedPane();
        final JScrollPane solversScrollPane = this.solversScrollPane = new JScrollPane(solversTab);
        /* No horizontal scrollbar, but a vertical one: the settings of the selected solver can be taller than the available space. */
        solversScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        solversScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        solversScrollPane.getVerticalScrollBar().setUnitIncrement(20);
        /* No frame around the whole tab: the tables inside have their own, a second one would be a double border (like in the rules tab). */
        solversScrollPane.setBorder(null);
        tabs.addTab(_GUI.T.CaptchaConfigPanel_solverOverviewAndSettings(), solversScrollPane);
        tabs.addTab("Captcha Rules", new CaptchaRulesContainer());
        this.solverComparisonContainer = new SolverComparisonContainer();
        tabs.addTab(_GUI.T.CaptchaSolverComparison_tab_title(), solverComparisonContainer);
        /* Always shown in the IDE; in the stable only via the (advanced settings only) setting "ShowCaptchaDebugPanel". */
        if (isDebugPanelAvailable()) {
            /* The panel is not Scrollable, so without an explicit unit increment one wheel step only scrolls a few pixels. */
            final CaptchaTestPanel testPanel = this.testPanel = new CaptchaTestPanel();
            final JScrollPane testScrollPane = this.testScrollPane = new JScrollPane(testPanel);
            testScrollPane.getVerticalScrollBar().setUnitIncrement(20);
            tabs.addTab("Test & Debug", testScrollPane);
            debugTabShown = true;
        }
        add(tabs);
        // this.addHeader(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_solver(), new AbstractIcon(IconKey.ICON_share", 32));
        // this.addDescriptionPlain(_GUI.T.AntiCaptchaConfigPanel_onShow_description_solver());
        // add(psp = new CESSettingsPanel());
    }

    /**
     * True if the "Test & Debug" tab is available: always in the IDE, otherwise only if the (advanced settings only) setting
     * "ShowCaptchaDebugPanel" is enabled.
     */
    private static volatile boolean debugTabShown = false;

    /**
     * True if the "Test & Debug" tab really exists in the (already built) captcha settings. The tab is created together with the panel, so
     * this can differ from {@link #isDebugPanelAvailable()} if the setting was changed afterwards.
     */
    public static boolean isDebugTabShown() {
        return debugTabShown;
    }

    public static boolean isDebugPanelAvailable() {
        return DebugMode.TRUE_IN_IDE_ELSE_FALSE || CFG_CAPTCHA.CFG.isShowCaptchaDebugPanel();
    }

    /** Opens the captcha settings, shows the "Test & Debug" tab and preselects the given solver for the multi test. */
    public static void showTestAndDebug(final SolverService solver) {
        showTestAndDebug(solver, null);
    }

    /**
     * Opens the captcha settings, shows the "Test & Debug" tab, preselects the given solver and the given captcha types (if not null):
     * the first one at the top, all of them in the multi test selection.
     */
    public static void showTestAndDebug(final SolverService solver, final java.util.Collection<CAPTCHA_TYPE> types) {
        showTestAndDebug(java.util.Collections.singletonList(solver), types);
    }

    /**
     * Like {@link #showTestAndDebug(SolverService, java.util.Collection)} for the captcha types table of one solver: If that solver is part
     * of the selection in the solver table (several solvers can be selected there), all selected solvers are preselected, otherwise only
     * the given one (e.g. if the captcha types table is not shown below the solver table). The captcha types are the ones given.
     */
    public static void showTestAndDebugWithSelectedSolvers(final SolverService solver, final java.util.Collection<CAPTCHA_TYPE> types) {
        java.util.List<SolverService> solvers = java.util.Collections.singletonList(solver);
        final CaptchaConfigPanel captchaConfigPanel = ConfigurationView.getInstance().getSubPanel(CaptchaConfigPanel.class);
        if (captchaConfigPanel != null && captchaConfigPanel.solverOrderTable != null) {
            final java.util.List<SolverService> selected = captchaConfigPanel.solverOrderTable.getModel().getSelectedObjects();
            if (selected.contains(solver)) {
                solvers = selected;
            }
        }
        showTestAndDebug(solvers, types);
    }

    /**
     * Opens the captcha settings, shows the "Test & Debug" tab, preselects the given solvers (only the ones which can be tested) and the
     * given captcha types (if not null): the first one at the top, all of them in the multi test selection.
     */
    public static void showTestAndDebug(final java.util.Collection<SolverService> solvers, final java.util.Collection<CAPTCHA_TYPE> types) {
        JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
        JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
        ConfigurationView.getInstance().setSelectedSubPanel(CaptchaConfigPanel.class);
        final CaptchaConfigPanel captchaConfigPanel = ConfigurationView.getInstance().getSubPanel(CaptchaConfigPanel.class);
        if (captchaConfigPanel != null) {
            captchaConfigPanel.selectTestAndDebug(solvers, types);
        }
    }

    private void selectTestAndDebug(final java.util.Collection<SolverService> solvers, final java.util.Collection<CAPTCHA_TYPE> types) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (testScrollPane == null) {
                    return;
                }
                tabs.setSelectedComponent(testScrollPane);
                testPanel.selectMultiTestSolvers(solvers, types);
            }
        });
    }

    /** Opens the captcha settings (can be called from any thread). */
    public static void showCaptchaSettings() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
                JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
                ConfigurationView.getInstance().setSelectedSubPanel(CaptchaConfigPanel.class);
            }
        });
    }

    /**
     * Opens the captcha settings with the solver of the given ID (the host of a captcha solver plugin) preselected. Captcha solver plugins
     * always link here instead of to the plugin settings.
     */
    public static void showSolver(final String solverID) {
        JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
        JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
        ConfigurationView.getInstance().setSelectedSubPanel(CaptchaConfigPanel.class);
        final CaptchaConfigPanel captchaConfigPanel = ConfigurationView.getInstance().getSubPanel(CaptchaConfigPanel.class);
        if (captchaConfigPanel != null) {
            captchaConfigPanel.selectSolver(solverID);
        }
    }

    /**
     * Shows the "Solver overview & settings" tab and preselects the solver with the given ID in its table. Deferred, so it happens after
     * the panel got shown (onShow re-sorts the table, which would move the rows).
     */
    public void selectSolver(final String solverID) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                tabs.setSelectedComponent(solversScrollPane);
                final ExtTableModel<SolverService> model = solverOrderTable.getModel();
                for (int row = 0; row < model.getRowCount(); row++) {
                    final SolverService solver = model.getObjectbyRow(row);
                    if (solver != null && solverID.equals(solver.getID())) {
                        solverOrderTable.getSelectionModel().setSelectionInterval(row, row);
                        solverOrderTable.scrollRectToVisible(solverOrderTable.getCellRect(row, 0, true));
                        return;
                    }
                }
            }
        });
    }

    private Component label(String lbl) {
        JLabel ret = new JLabel(lbl);
        ret.setEnabled(true);
        return ret;
    }

    @Override
    public Icon getIcon() {
        return new AbstractIcon(IconKey.ICON_OCR, 32);
    }

    @Override
    protected void onShow() {
        /*
         * Re-apply the currently active column sort (e.g. "Status") against the solvers' current values. Live updates while this panel is
         * visible only repaint cells (see SolverOrderTableModel.refreshRows()) so rows don't jump around under the user's cursor; returning
         * to this panel is the point where rows get regrouped, matching the same behavior as the account manager's account table.
         */
        solverOrderTable.getModel().refreshSort();
        /* Solvers and the captcha history change while JDownloader is running. */
        solverComparisonContainer.refresh();
        super.onShow();
    }

    @Override
    public void save() {
    }

    @Override
    public void updateContents() {
    }
}