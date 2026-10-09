package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import jd.gui.swing.jdgui.views.settings.components.Checkbox;
import jd.gui.swing.jdgui.views.settings.components.Spinner;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;

import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.notify.captcha.CESBubbleSupport;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.settings.Pair;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_SOUND;

public class CaptchaConfigPanel extends AbstractConfigPanel {
    private static final long serialVersionUID = 1L;

    // private CESSettingsPanel psp;
    private SolverOrderTable  solverOrderTable;
    private SolverComparisonContainer solverComparisonContainer;
    private Pair<Spinner>             skipBubbleTimeoutPair;
    private Pair<Checkbox>            useExternalSolverAccountsPair;
    private CaptchaSettingsTabbedPane tabs;
    private JScrollPane               solversScrollPane;

    public String getTitle() {
        return _GUI.T.AntiCaptchaConfigPanel_getTitle();
    }

    public CaptchaConfigPanel() {
        super();
        this.addHeader(getTitle(), new AbstractIcon(IconKey.ICON_OCR, 32));
        this.addDescriptionPlain(_GUI.T.AntiCaptchaConfigPanel_onShow_description());

        addPair(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_sounds(), null, new Checkbox(CFG_SOUND.CAPTCHA_SOUND_ENABLED));
        addPair(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_countdown_download(), null, new Checkbox(CFG_CAPTCHA.DIALOG_COUNTDOWN_FOR_DOWNLOADS_ENABLED));
        final Pair<Checkbox> useExternalSolverAccounts = this.useExternalSolverAccountsPair = addPair(_GUI.T.CaptchaConfigPanel_useExternalSolverAccounts(), null, new Checkbox(CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS));
        final Pair<Checkbox> avoidAutoSolverForLoginCaptchas = addPair(_GUI.T.CaptchaConfigPanel_avoidAutoSolverForLoginCaptchas(), null, new Checkbox(CFG_CAPTCHA.AVOID_AUTO_SOLVER_FOR_LOGIN_CAPTCHAS));
        avoidAutoSolverForLoginCaptchas.setToolTipText(_GUI.T.CaptchaConfigPanel_avoidAutoSolverForLoginCaptchas_tooltip());
        avoidAutoSolverForLoginCaptchas.setConditionPair(useExternalSolverAccounts);
        skipBubbleTimeoutPair = addPair(_GUI.T.CaptchaExchangeSpinnerAction_skipbubbletimeout_(), null, new Spinner(CFG_CAPTCHA.EXTERNAL_CAPTCHA_SOLVER_CHANCE_TO_ABORT_EXTERNAL_CAPTCHA_SOLVER));
        /* Not setConditionPair: the enabled state also depends on the bubble settings, see updateSkipBubbleTimeoutEnabled(). */
        useExternalSolverAccounts.getComponent().addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                updateSkipBubbleTimeoutEnabled();
            }
        });
        updateSkipBubbleTimeoutEnabled();

        /* Tabbed area at the top: solver overview and captcha rules (similar to the Linkgrabber Filter panel). */
        final SolverOrderTable table = this.solverOrderTable = new SolverOrderTable();
        final SolverOrderContainer container = new SolverOrderContainer(table);
        /* Width tracking: long texts in the solver settings must not make the panel wider than the visible area. */
        final MigPanel solversTab = new WidthTrackingPanel("ins 5, wrap 1", "[grow,fill]", "[grow,fill]");
        solversTab.add(container, "grow");
        final CaptchaSettingsTabbedPane tabs = this.tabs = new CaptchaSettingsTabbedPane();
        final JScrollPane solversScrollPane = this.solversScrollPane = new JScrollPane(solversTab);
        /* No horizontal scrollbar, but a vertical one: the settings of the selected solver can be taller than the available space. */
        solversScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        solversScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        solversScrollPane.getVerticalScrollBar().setUnitIncrement(20);
        tabs.addTab(_GUI.T.CaptchaConfigPanel_solverOverviewAndSettings(), solversScrollPane);
        tabs.addTab("Captcha Rules", new CaptchaRulesContainer());
        this.solverComparisonContainer = new SolverComparisonContainer();
        tabs.addTab(_GUI.T.CaptchaSolverComparison_tab_title(), solverComparisonContainer);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            tabs.addTab("Test & Debug", new JScrollPane(new CaptchaTestPanel()));
        }
        add(tabs);
        // this.addHeader(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_solver(), new AbstractIcon(IconKey.ICON_share", 32));
        // this.addDescriptionPlain(_GUI.T.AntiCaptchaConfigPanel_onShow_description_solver());
        // add(psp = new CESSettingsPanel());
    }

    /**
     * The "chance to abort" timeout is the duration of the captcha solver bubble (see CESSolverJob#showBubble), so it has no effect if that
     * bubble is disabled (either all bubble notifications are switched off or this bubble type is) or if external solver accounts are not
     * used at all.
     */
    private void updateSkipBubbleTimeoutEnabled() {
        skipBubbleTimeoutPair.setEnabled(CESBubbleSupport.getInstance().isEnabled() && useExternalSolverAccountsPair.getComponent().isSelected());
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
        /* The bubble settings may have been changed in another settings panel in the meantime. */
        updateSkipBubbleTimeoutEnabled();
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