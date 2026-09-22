package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.event.ActionEvent;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JScrollPane;

import jd.gui.swing.jdgui.views.settings.components.Checkbox;
import jd.gui.swing.jdgui.views.settings.components.SettingsButton;
import jd.gui.swing.jdgui.views.settings.components.Spinner;

import org.appwork.swing.MigPanel;
import org.appwork.uio.UIOManager;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_SOUND;

public class CaptchaConfigPanel extends AbstractConfigPanel {
    private static final long serialVersionUID = 1L;

    // private CESSettingsPanel psp;
    private SolverOrderTable  solverOrderTable;
    private SolverComparisonContainer solverComparisonContainer;

    public String getTitle() {
        return _GUI.T.AntiCaptchaConfigPanel_getTitle();
    }

    public CaptchaConfigPanel() {
        super();
        this.addHeader(getTitle(), new AbstractIcon(IconKey.ICON_OCR, 32));
        this.addDescriptionPlain(_GUI.T.AntiCaptchaConfigPanel_onShow_description());

        addPair(_GUI.T.CaptchaConfigPanel_useExternalSolverAccounts(), null, new Checkbox(CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS));
        addPair(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_sounds(), null, new Checkbox(CFG_SOUND.CAPTCHA_SOUND_ENABLED));
        addPair(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_countdown_download(), null, new Checkbox(CFG_CAPTCHA.DIALOG_COUNTDOWN_FOR_DOWNLOADS_ENABLED));
        addPair(_GUI.T.CaptchaExchangeSpinnerAction_skipbubbletimeout_(), null, new Spinner(CFG_CAPTCHA.CAPTCHA_EXCHANGE_CHANCE_TO_SKIP_BUBBLE_TIMEOUT));

        /* Tabbed area at the top: solver overview and captcha rules (similar to the Linkgrabber Filter panel). */
        final SolverOrderTable table = this.solverOrderTable = new SolverOrderTable();
        final SolverOrderContainer container = new SolverOrderContainer(table);
        final MigPanel solversTab = new MigPanel("ins 5, wrap 1", "[grow,fill]", "[grow,fill][]");
        solversTab.add(container, "grow");
        solversTab.add(new SettingsButton(new AppAction() {
            {
                setIconKey(IconKey.ICON_RESET);
                setName(_GUI.T.lit_reset());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                if (UIOManager.I().showConfirmDialog(0, _GUI.T.lit_are_you_sure(), _GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_reset_lit_are_you_sure(), new AbstractIcon(IconKey.ICON_QUESTION, 32), _GUI.T.lit_yes(), null)) {
                    ChallengeResponseController.getInstance().resetTiming();
                }
            }
        }), "align right");
        final CaptchaSettingsTabbedPane tabs = new CaptchaSettingsTabbedPane();
        final JScrollPane solversScrollPane = new JScrollPane(solversTab);
        /* No horizontal scrollbar, but a vertical one: the settings of the selected solver can be taller than the available space. */
        solversScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        solversScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        solversScrollPane.getVerticalScrollBar().setUnitIncrement(20);
        tabs.addTab(_GUI.T.CaptchaConfigPanel_solverOverviewAndSettings(), solversScrollPane);
        tabs.addTab("Captcha Rules", new CaptchaRulesContainer());
        this.solverComparisonContainer = new SolverComparisonContainer();
        tabs.addTab(_GUI.T.CaptchaSolverComparison_tab_title(), solverComparisonContainer);
        add(tabs);
        // this.addHeader(_GUI.T.AntiCaptchaConfigPanel_AntiCaptchaConfigPanel_solver(), new AbstractIcon(IconKey.ICON_share", 32));
        // this.addDescriptionPlain(_GUI.T.AntiCaptchaConfigPanel_onShow_description_solver());
        // add(psp = new CESSettingsPanel());
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