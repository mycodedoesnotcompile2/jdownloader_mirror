package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.CardLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JRadioButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JSeparator;
import javax.swing.JScrollPane;
import javax.swing.Scrollable;
import javax.swing.SwingUtilities;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberAbsoluteDialogLocator;
import org.jdownloader.captcha.v2.CaptchaHistoryEntry;
import org.jdownloader.captcha.v2.CaptchaHistoryManager;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.LazyHostPluginFilter;
import org.jdownloader.settings.SilentModeSettings.SilentModeSetting;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

import jd.controlling.captcha.CaptchaSettings;
import jd.plugins.Account;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import net.miginfocom.swing.MigLayout;

/**
 * Wizard which helps the user to find the captcha solvers which fit the needs: <br>
 * 1. How does the user want to solve captchas (manually and/or with paid solvers)? <br>
 * 2. For which domains? (preselected from the captcha history and the entries of the last wizard run, the user can add more) <br>
 * 3. Which solvers can solve the captcha types needed for these domains? <br>
 * At the end the captcha comparison table is opened with the needed captcha types selected, see
 * {@link CaptchaConfigPanel#showCaptchaWizardResult(Set, boolean, boolean, boolean)}. <br>
 * The selected entries are stored in {@link CaptchaSettings#getCaptchaWizardEntries()} when the wizard is finished and are preselected the
 * next time.
 */
public class CaptchaWizardDialog extends AbstractDialog<Integer> {
    /* The number of steps follows this array, the progress bar and the buttons are derived from it. */
    private static final int    STEP_METHOD  = 0;
    private static final int    STEP_DOMAINS = 1;
    private static final int    STEP_TYPES   = 2;
    private static final int    STEP_SOLVERS = 3;
    private static final int    STEPS        = 4;
    private static final String[] CARDS      = new String[] { "method", "domains", "types", "solvers" };
    private int                 step         = STEP_METHOD;
    /* Step 1 also contains the solver pre-check: the offered fixes. A checkbox is null if there is nothing to fix. */
    private JPanel              precheckPanel;
    private JCheckBox           enableGlobalCheckBox;
    private JCheckBox           enableSolversCheckBox;
    private JCheckBox           enableAccountsCheckBox;
    private JCheckBox           disableSilentCheckBox;
    private List<SolverService> disabledSolvers = new ArrayList<SolverService>();
    /* The accounts of external solvers which the user has disabled. */
    private List<Account>       disabledAccounts = new ArrayList<Account>();
    /* True as soon as the pre-check changed a setting. */
    private boolean             precheckChanged = false;
    /* The solving methods the user can choose in step 1. */
    private enum Method {
        MANUAL,
        PAID,
        BOTH
    }
    private JRadioButton        manualRadio;
    private JRadioButton        paidRadio;
    private JRadioButton        bothRadio;
    /* State of step 1 and 2 when the wizard was opened, used to find out whether the user changed anything. */
    private Method              initialMethod;
    private Map<String, Boolean> initialEntryStates = new HashMap<String, Boolean>();
    /* All hoster plugins the user can add, collected once when needed. */
    private List<LazyHostPlugin> addableHosters;
    private CaptchaWizardDomainTable domainTable;
    private JLabel              mixedInfoLabel;
    private JLabel              domainHintLabel;
    private JPanel              typesPanel;
    private CaptchaWizardSolverTable solverTable;
    private JLabel              deselectedInfoLabel;
    private JPanel              infoPanel;
    private JPanel              cardPanel;
    private CardLayout          cardLayout;
    private JProgressBar        progressBar;
    private JButton             backButton;
    /* The result of the last time step 3 was entered, used to open the comparison table. */
    private Set<CAPTCHA_TYPE>   requiredTypes = new LinkedHashSet<CAPTCHA_TYPE>();
    private boolean             anySolverSolvesAll = false;
    /* The solvers the table of step 3 showed, they are marked in the comparison table. */
    private List<SolverService> shownSolvers       = new ArrayList<SolverService>();

    /** Shows the wizard, and opens the captcha comparison table if the user finishes it. */
    public static void showWizard() {
        final CaptchaWizardDialog dialog = new CaptchaWizardDialog();
        try {
            Dialog.getInstance().showDialog(dialog);
        } catch (final DialogNoAnswerException e) {
            /* Canceled or closed. */
            return;
        }
        dialog.saveEntries();
        /* Finish is always possible, also with an empty table or if the user deselected all solvers. */
        CaptchaConfigPanel.showCaptchaWizardResult(dialog.requiredTypes, dialog.solverTable.getRelevantSolvers());
    }

    private CaptchaWizardDialog() {
        super(Dialog.STYLE_HIDE_ICON, _GUI.T.CaptchaWizard_title(), null, _GUI.T.CaptchaWizard_next(), _GUI.T.lit_cancel());
        setLocator(new RememberAbsoluteDialogLocator(getClass().getSimpleName()));
        setDimensor(new RememberLastDialogDimension(getClass().getSimpleName()));
    }

    @Override
    protected Integer createReturnValue() {
        return getReturnmask();
    }

    @Override
    protected int getPreferredWidth() {
        return 800;
    }

    @Override
    public JComponent layoutDialogContent() {
        cardLayout = new CardLayout();
        cardPanel = new JPanel(cardLayout);
        cardPanel.add(createMethodPanel(), CARDS[STEP_METHOD]);
        cardPanel.add(createDomainsPanel(), CARDS[STEP_DOMAINS]);
        cardPanel.add(createTypesPanel(), CARDS[STEP_TYPES]);
        cardPanel.add(createSolversPanel(), CARDS[STEP_SOLVERS]);
        /* The progress at the bottom shows in which step we are. */
        progressBar = new JProgressBar(0, STEPS);
        /* The "Step X/Y" text is part of the progress bar. */
        progressBar.setStringPainted(true);
        final JPanel panel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[grow,fill][]"));
        panel.add(cardPanel, "grow, push");
        panel.add(progressBar, "gaptop 10");
        updateStepState();
        return panel;
    }

    @Override
    protected void addButtons(final JPanel buttonBar) {
        backButton = new JButton(_GUI.T.CaptchaWizard_back(), NewTheme.I().getIcon(IconKey.ICON_UNDO, 18));
        backButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (step > 0) {
                    step--;
                    updateStepState();
                }
            }
        });
        getCancelButton().setIcon(NewTheme.I().getIcon(IconKey.ICON_CANCEL, 18));
        /*
         * The order of tagged buttons depends on the platform, so the buttons are added again in a fixed order without tags: Back, Next,
         * Cancel. Cancel is always the right most button.
         */
        buttonBar.removeAll();
        buttonBar.add(backButton, "alignx right, sizegroup confirms");
        buttonBar.add(getOkButton(), "alignx right, sizegroup confirms");
        buttonBar.add(getCancelButton(), "alignx right, sizegroup confirms");
        /* The button bar is built before the content is shown, so the state of the buttons is set again. */
        updateStepState();
    }

    /** "Next" goes to the next step, only the last step ("Finish") closes the dialog. */
    @Override
    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == getCancelButton() && !confirmDiscard()) {
            return;
        }
        if (e.getSource() == getOkButton() && step < STEPS - 1) {
            if (step == STEP_METHOD) {
                applyPrecheck();
            }
            step++;
            updateStepState();
            if (step == STEP_TYPES) {
                onTypesStepEntered();
            } else if (step == STEP_SOLVERS) {
                onSolverStepEntered();
            }
            return;
        }
        super.actionPerformed(e);
    }

    /** Part of step 1: Checks whether external solvers are globally enabled and whether all solvers are enabled, and offers to fix it. */
    private JComponent createPrecheckPanel() {
        precheckPanel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[]10[]5[]"));
        buildPrecheckContent();
        return precheckPanel;
    }

    /**
     * (Re)builds the content of the pre-check: A problem is shown as a checkbox (ticked by default, the fix is applied on "Next"), no
     * problem as a text with a green check icon.
     */
    private void buildPrecheckContent() {
        disabledSolvers = new ArrayList<SolverService>();
        disabledAccounts = new ArrayList<Account>();
        for (final SolverService service : ChallengeResponseController.getInstance().getOrderedServices()) {
            if (!service.getConfigV3().isEnabled()) {
                disabledSolvers.add(service);
            }
            if (service instanceof PluginForCaptchaSolverSolverService) {
                disabledAccounts.addAll(((PluginForCaptchaSolverSolverService) service).getDisabledAccounts());
            }
        }
        precheckPanel.removeAll();
        precheckPanel.add(new JSeparator());
        precheckPanel.add(createHeader(_GUI.T.CaptchaWizard_precheck_header()));
        /* The findings: a warning triangle for a problem, a green check mark if everything is fine. */
        final boolean globalOk = CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS.isEnabled();
        final boolean solversOk = disabledSolvers.isEmpty();
        if (globalOk) {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_globalEnabled(), NewTheme.I().getIcon(IconKey.ICON_OK, 18), JLabel.LEADING));
        } else {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_globalDisabled(), NewTheme.I().getIcon(IconKey.ICON_WARNING, 18), JLabel.LEADING));
        }
        if (solversOk) {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_allSolversEnabled(), NewTheme.I().getIcon(IconKey.ICON_OK, 18), JLabel.LEADING));
        } else {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_solversDisabled(disabledSolvers.size()), NewTheme.I().getIcon(IconKey.ICON_WARNING, 18), JLabel.LEADING));
        }
        final boolean accountsOk = disabledAccounts.isEmpty();
        if (accountsOk) {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_allAccountsEnabled(), NewTheme.I().getIcon(IconKey.ICON_OK, 18), JLabel.LEADING));
        } else {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_accountsDisabled(disabledAccounts.size()), NewTheme.I().getIcon(IconKey.ICON_WARNING, 18), JLabel.LEADING));
        }
        /* Silent mode (also its automatic variants) suppresses captcha dialogs, which defeats manual solving. */
        final boolean silentOk = CFG_SILENTMODE.CFG.getSilentMode() == SilentModeSetting.DISABLED;
        if (silentOk) {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_silentDisabled(), NewTheme.I().getIcon(IconKey.ICON_OK, 18), JLabel.LEADING));
        } else {
            precheckPanel.add(new JLabel(_GUI.T.CaptchaWizard_precheck_silentEnabled(), NewTheme.I().getIcon(IconKey.ICON_WARNING, 18), JLabel.LEADING));
        }
        /* The fixes are offered separately at the bottom, only if there is a problem. They are applied when the user clicks "Next". */
        disableSilentCheckBox = silentOk ? null : new JCheckBox(_GUI.T.CaptchaWizard_precheck_disableSilent(), true);
        enableGlobalCheckBox = globalOk ? null : new JCheckBox(_GUI.T.CaptchaWizard_precheck_enableGlobal(), true);
        enableSolversCheckBox = solversOk ? null : new JCheckBox(_GUI.T.CaptchaWizard_precheck_enableSolvers(disabledSolvers.size()), true);
        enableAccountsCheckBox = accountsOk ? null : new JCheckBox(_GUI.T.CaptchaWizard_precheck_enableAccounts(disabledAccounts.size()), true);
        if (enableGlobalCheckBox != null || enableSolversCheckBox != null || enableAccountsCheckBox != null || disableSilentCheckBox != null) {
            precheckPanel.add(new JSeparator(), "gaptop 15, pushy, aligny bottom");
            precheckPanel.add(createHeader(_GUI.T.CaptchaWizard_precheck_fixHeader()), "wmin 0");
            if (disableSilentCheckBox != null) {
                precheckPanel.add(disableSilentCheckBox);
            }
            if (enableGlobalCheckBox != null) {
                precheckPanel.add(enableGlobalCheckBox);
            }
            if (enableSolversCheckBox != null) {
                precheckPanel.add(enableSolversCheckBox);
            }
            if (enableAccountsCheckBox != null) {
                precheckPanel.add(enableAccountsCheckBox);
            }
            /* Shown as soon as the user unticks one of the suggested fixes. */
            final JLabel warning = new JLabel("<html>" + _GUI.T.CaptchaWizard_precheck_fixDisabledWarning() + "</html>", NewTheme.I().getIcon(IconKey.ICON_WARNING, 18), JLabel.LEADING);
            precheckPanel.add(warning, "wmin 0, hidemode 3");
            final ItemListener listener = new ItemListener() {
                @Override
                public void itemStateChanged(final ItemEvent e) {
                    warning.setVisible(isAnyFixDisabled());
                }
            };
            for (final JCheckBox box : new JCheckBox[] { disableSilentCheckBox, enableGlobalCheckBox, enableSolversCheckBox, enableAccountsCheckBox }) {
                if (box != null) {
                    box.addItemListener(listener);
                }
            }
            warning.setVisible(isAnyFixDisabled());
        }
        precheckPanel.revalidate();
        precheckPanel.repaint();
    }

    /** True if a fix is offered but the user has unticked it. */
    private boolean isAnyFixDisabled() {
        for (final JCheckBox box : new JCheckBox[] { disableSilentCheckBox, enableGlobalCheckBox, enableSolversCheckBox, enableAccountsCheckBox }) {
            if (box != null && !box.isSelected()) {
                return true;
            }
        }
        return false;
    }

    /** Applies the ticked fixes of the pre-check. This counts as a change of the user, see {@link #hasChanges()}. */
    private void applyPrecheck() {
        if (enableGlobalCheckBox != null && enableGlobalCheckBox.isSelected()) {
            try {
                CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS.setValue(true);
            } catch (final ValidationException e) {
                /* Should never happen for a plain boolean key. */
                throw new WTFException(e);
            }
            precheckChanged = true;
        }
        if (enableSolversCheckBox != null && enableSolversCheckBox.isSelected()) {
            for (final SolverService service : disabledSolvers) {
                service.getConfigV3().setEnabled(true);
            }
            precheckChanged = true;
        }
        if (disableSilentCheckBox != null && disableSilentCheckBox.isSelected()) {
            CFG_SILENTMODE.CFG.setSilentMode(SilentModeSetting.DISABLED);
            precheckChanged = true;
        }
        if (enableAccountsCheckBox != null && enableAccountsCheckBox.isSelected()) {
            for (final Account account : disabledAccounts) {
                account.setEnabled(true);
            }
            precheckChanged = true;
        }
        /* The page now shows the new state, also when the user comes back to it. */
        buildPrecheckContent();
    }

    private JComponent createMethodPanel() {
        final JPanel panel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[]15[]5[]10[][][]15[grow,fill]"));
        panel.add(new JLabel("<html>" + _GUI.T.CaptchaWizard_step1_intro() + "</html>"), "wmin 0");
        panel.add(createHeader(_GUI.T.CaptchaWizard_step1_header()));
        panel.add(new JLabel("<html>" + _GUI.T.CaptchaWizard_step1_text() + "</html>"), "wmin 0");
        /* Exactly one method can be chosen, "both" is the default. */
        manualRadio = new JRadioButton(_GUI.T.CaptchaWizard_step1_manual());
        paidRadio = new JRadioButton(_GUI.T.CaptchaWizard_step1_paid());
        bothRadio = new JRadioButton(_GUI.T.CaptchaWizard_step1_both(), true);
        final ButtonGroup group = new ButtonGroup();
        group.add(manualRadio);
        group.add(paidRadio);
        group.add(bothRadio);
        panel.add(manualRadio);
        panel.add(paidRadio);
        panel.add(bothRadio);
        /* The solver pre-check is part of this step: findings and the offered fixes (at the bottom). */
        panel.add(createPrecheckPanel(), "grow, push, gaptop 10");
        initialMethod = getMethod();
        return panel;
    }

    /** The solving method chosen in step 1. */
    private Method getMethod() {
        if (manualRadio.isSelected()) {
            return Method.MANUAL;
        } else if (paidRadio.isSelected()) {
            return Method.PAID;
        }
        return Method.BOTH;
    }

    /** True if solvers of JDownloader which need a human are wanted. */
    private boolean isManualWanted() {
        switch (getMethod()) {
        case PAID:
            return false;
        case MANUAL:
        case BOTH:
        default:
            return true;
        }
    }

    /** True if paid external solvers are wanted. */
    private boolean isPaidWanted() {
        switch (getMethod()) {
        case MANUAL:
            return false;
        case PAID:
        case BOTH:
        default:
            return true;
        }
    }

    /**
     * True if the user changed anything: the solving method of step 1, or the entries of step 2 (added, removed, (de)selected). Only
     * without changes ESC may close the wizard, so a few mouse clicks are not lost by accident.
     */
    private boolean hasChanges() {
        if (precheckChanged || getMethod() != initialMethod) {
            return true;
        }
        return !getEntryStates().equals(initialEntryStates);
    }

    /** Storage string of every entry in the domain table, mapped to its enabled state. */
    private Map<String, Boolean> getEntryStates() {
        final Map<String, Boolean> ret = new HashMap<String, Boolean>();
        for (final CaptchaWizardEntry entry : domainTable.getAllEntries()) {
            ret.put(entry.getKey(), entry.isEnabled());
        }
        return ret;
    }

    @Override
    protected void registerEscape() {
        super.registerEscape();
        final JComponent focus = getFocusButton();
        if (focus == null) {
            return;
        }
        final Action original = focus.getActionMap().get("ESCAPE");
        if (original == null) {
            return;
        }
        focus.getActionMap().put("ESCAPE", new AbstractAction() {
            private static final long serialVersionUID = 1L;

            @Override
            public void actionPerformed(final ActionEvent e) {
                if (confirmDiscard()) {
                    original.actionPerformed(e);
                }
            }
        });
    }

    /**
     * Without changes closing is always fine. With changes the user has to confirm first (ESC, the close button of the window and the
     * Cancel button). OK is preselected, so Enter confirms.
     *
     * @return true if the wizard may be closed
     */
    private boolean confirmDiscard() {
        if (!hasChanges()) {
            return true;
        }
        try {
            Dialog.getInstance().showConfirmDialog(0, _GUI.T.CaptchaWizard_confirmClose_title(), _GUI.T.CaptchaWizard_confirmClose_message(), NewTheme.I().getIcon(IconKey.ICON_QUESTION, 32), _GUI.T.lit_yes(), _GUI.T.lit_no());
            return true;
        } catch (final DialogNoAnswerException e) {
            return false;
        }
    }

    @Override
    public boolean closeAllowed() {
        return confirmDiscard();
    }

    private JComponent createDomainsPanel() {
        final JPanel panel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[][][][][grow,fill][]"));
        panel.add(createHeader(_GUI.T.CaptchaWizard_step2_header()));
        panel.add(new JLabel("<html>" + _GUI.T.CaptchaWizard_step2_text() + "</html>"), "wmin 0");
        panel.add(new JLabel("<html>" + _GUI.T.CaptchaWizard_step2_addHint() + "</html>"), "wmin 0");
        mixedInfoLabel = new JLabel("<html>" + _GUI.T.CaptchaWizard_step2_mixedInfo() + "</html>", NewTheme.I().getIcon(IconKey.ICON_INFO, 18), JLabel.LEADING);
        panel.add(mixedInfoLabel, "wmin 0, hidemode 3");
        domainTable = new CaptchaWizardDomainTable(new Runnable() {
            @Override
            public void run() {
                updateStepState();
            }
        });
        /* The table scrolls itself, the dialog has a fixed height for it. */
        final JScrollPane scrollPane = domainTable.createScrollPane();
        panel.add(scrollPane, "grow, height 250:300:, push");
        final JPanel bottom = new JPanel(new MigLayout("ins 0", "[][grow,fill]", "[]"));
        final JButton addButton = new JButton(_GUI.T.CaptchaWizard_step2_add(), NewTheme.I().getIcon(IconKey.ICON_ADD, 16));
        addButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                onAddDomain();
            }
        });
        domainHintLabel = new JLabel(_GUI.T.CaptchaWizard_step2_nothingSelected(), NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18), JLabel.LEADING);
        bottom.add(addButton);
        bottom.add(domainHintLabel, "hidemode 3");
        panel.add(bottom);
        final List<CaptchaWizardEntry> initial = loadInitialEntries();
        domainTable.setEntries(initial);
        initialEntryStates = getEntryStates();
        /*
         * All initial entries come from the local captcha history or from the last wizard run, and all of them are preselected: Tell the
         * user, but only if there is at least one.
         */
        mixedInfoLabel.setVisible(!initial.isEmpty());
        return panel;
    }

    /** Step 3: The captcha types the user needs and the hint that the next step suggests captcha solvers. */
    private JComponent createTypesPanel() {
        final JPanel panel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[][][]"));
        panel.add(createHeader(_GUI.T.CaptchaWizard_step3_header()));
        typesPanel = new JPanel(new MigLayout("ins 0, wrap 4", "[]20[]20[]20[]", "[]"));
        panel.add(typesPanel, "gaptop 5, gapbottom 10");
        panel.add(new JLabel("<html>" + _GUI.T.CaptchaWizard_step3_nextInfo() + "</html>", NewTheme.I().getIcon(IconKey.ICON_INFO, 18), JLabel.LEADING), "wmin 0");
        return panel;
    }

    /** A panel which is always as wide as the viewport of its scroll pane, so the text in it wraps instead of being cut off. */
    private static class ViewportWidthPanel extends JPanel implements Scrollable {
        private static final long serialVersionUID = 1L;

        private ViewportWidthPanel(final LayoutManager layout) {
            super(layout);
        }

        @Override
        public Dimension getPreferredScrollableViewportSize() {
            return getPreferredSize();
        }

        @Override
        public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
            return 16;
        }

        @Override
        public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
            return Math.max(16, visibleRect.height - 16);
        }

        @Override
        public boolean getScrollableTracksViewportWidth() {
            return true;
        }

        @Override
        public boolean getScrollableTracksViewportHeight() {
            return false;
        }
    }

    /** Step 4: The solvers. The content scrolls, as the notes below the table can take much space. */
    private JComponent createSolversPanel() {
        final JPanel panel = new ViewportWidthPanel(new MigLayout("ins 0 0 0 5, wrap 1", "[grow,fill]", "[][][]"));
        panel.add(createHeader(_GUI.T.CaptchaWizard_step4_header()));
        solverTable = new CaptchaWizardSolverTable();
        panel.add(solverTable.createScrollPane(), "grow");
        deselectedInfoLabel = new JLabel("", NewTheme.I().getIcon(IconKey.ICON_INFO, 18), JLabel.LEADING);
        deselectedInfoLabel.setVisible(false);
        panel.add(deselectedInfoLabel, "wmin 0, hidemode 3");
        /* The informations which are also shown as dialogs, see onSolverStepEntered. */
        infoPanel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[]"));
        panel.add(infoPanel, "wmin 0");
        final JScrollPane scroll = new JScrollPane(panel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroll.setBorder(null);
        scroll.getVerticalScrollBar().setUnitIncrement(16);
        return scroll;
    }

    private static JLabel createHeader(final String text) {
        final JLabel label = new JLabel(text);
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        return label;
    }

    /** Shows the current step and enables the buttons which make sense in it. */
    private void updateStepState() {
        /* Not everything is built yet while the content is created (the domain table already reports changes then). */
        if (progressBar == null || domainHintLabel == null) {
            return;
        }
        cardLayout.show(cardPanel, CARDS[step]);
        progressBar.setValue(step + 1);
        progressBar.setString(_GUI.T.CaptchaWizard_progress(step + 1, STEPS));
        final boolean lastStep = step == STEPS - 1;
        getOkButton().setText(lastStep ? _GUI.T.CaptchaWizard_finish() : _GUI.T.CaptchaWizard_next());
        /* Finish opens the captcha comparison table: same icon as the other buttons which lead to it. */
        getOkButton().setIcon(NewTheme.I().getIcon(lastStep ? IconKey.ICON_LIST : IconKey.ICON_GO_NEXT, 18));
        if (backButton != null) {
            backButton.setEnabled(step > 0);
        }
        final boolean anyDomain = !domainTable.getEnabledEntries().isEmpty();
        /* An empty list asks to add an entry, a list without any ticked entry asks to select one. */
        domainHintLabel.setText(domainTable.getAllEntries().isEmpty() ? _GUI.T.CaptchaWizard_step2_nothingAdded() : _GUI.T.CaptchaWizard_step2_nothingSelected());
        domainHintLabel.setVisible(!anyDomain);
        switch (step) {
        case STEP_DOMAINS:
            getOkButton().setEnabled(anyDomain);
            break;
        case STEP_METHOD:
        case STEP_TYPES:
        case STEP_SOLVERS:
        default:
            getOkButton().setEnabled(true);
            break;
        }
    }

    /** The entries the table starts with: the local captcha history merged with the entries stored by the last run of the wizard. */
    private List<CaptchaWizardEntry> loadInitialEntries() {
        final Set<CaptchaWizardEntry> ret = new LinkedHashSet<CaptchaWizardEntry>();
        final Set<CAPTCHA_TYPE> processable = CaptchaType.getProcessableCaptchaTypes();
        for (final CaptchaHistoryEntry history : CaptchaHistoryManager.getInstance().getAllEntries()) {
            final CAPTCHA_TYPE type = history.getCaptcha_type();
            if (type != null && processable.contains(type) && StringUtils.isNotEmpty(history.getDomain())) {
                ret.add(new CaptchaWizardEntry(history.getDomain(), type, history.getCaptchaRequestType(), true));
            }
        }
        /* Only domains are stored. Domains which are already in the list because of the history keep the captcha types of the history. */
        final Set<String> knownDomains = new LinkedHashSet<String>();
        for (final CaptchaWizardEntry entry : ret) {
            knownDomains.add(entry.getDomain().toLowerCase(Locale.ENGLISH));
        }
        final Set<String> stored = JsonConfig.create(CaptchaSettings.class).getCaptchaWizardEntries();
        if (stored != null) {
            for (final String domain : stored) {
                if (StringUtils.isNotEmpty(domain) && knownDomains.add(domain.toLowerCase(Locale.ENGLISH))) {
                    /* TODO: Placeholder captcha type, see getAddableDomains(). */
                    ret.add(new CaptchaWizardEntry(domain, getRandomCaptchaType(), null, true));
                }
            }
        }
        return new ArrayList<CaptchaWizardEntry>(ret);
    }

    /** Stores the domains of the selected entries, they are preselected the next time the wizard is opened. */
    private void saveEntries() {
        final Set<String> domains = new LinkedHashSet<String>();
        for (final CaptchaWizardEntry entry : domainTable.getEnabledEntries()) {
            domains.add(entry.getDomain().toLowerCase(Locale.ENGLISH));
        }
        JsonConfig.create(CaptchaSettings.class).setCaptchaWizardEntries(domains);
    }

    /**
     * All hoster plugins the user can add, except multihosters and captcha solver plugins (neither of them has captchas of its own). The
     * list is collected only once, domains which are already in the table are offered as well.<br>
     * TODO: Plugins do not know yet which captcha types they can request. As soon as they do (see the planned
     * {@code getExpectedCaptchaTypes()} of the plugins), only domains with at least one expected captcha type must be offered here, and
     * the captcha types of the added domain must be the expected ones instead of {@link #getRandomCaptchaType()}.
     */
    private List<LazyHostPlugin> getAddableDomains() {
        if (addableHosters != null) {
            return addableHosters;
        }
        final Set<String> excluded = new TreeSet<String>();
        for (final LazyHostPlugin plugin : HostPluginController.getInstance().list(LazyHostPluginFilter.ALL_MULTIHOSTERS)) {
            excluded.add(plugin.getHost().toLowerCase(Locale.ENGLISH));
        }
        for (final LazyHostPlugin plugin : HostPluginController.getInstance().list(LazyHostPluginFilter.ALL_CAPTCHA_SOLVERS)) {
            excluded.add(plugin.getHost().toLowerCase(Locale.ENGLISH));
        }
        final List<LazyHostPlugin> ret = new ArrayList<LazyHostPlugin>();
        for (final LazyHostPlugin plugin : HostPluginController.getInstance().list()) {
            if (plugin.isFallbackPlugin() || StringUtils.isEmpty(plugin.getHost())) {
                continue;
            }
            if (!excluded.contains(plugin.getHost().toLowerCase(Locale.ENGLISH))) {
                ret.add(plugin);
            }
        }
        addableHosters = ret;
        return ret;
    }

    /**
     * TODO: Placeholder until plugins know their expected captcha types (see {@link #getAddableDomains()}): A random captcha type is
     * chosen for the added domain.
     */
    private CAPTCHA_TYPE getRandomCaptchaType() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>(CaptchaType.getProcessableCaptchaTypes());
        return types.get(new Random().nextInt(types.size()));
    }

    private void onAddDomain() {
        final String domain = CaptchaWizardAddDomainDialog.showDialog(getAddableDomains());
        if (domain == null) {
            return;
        }
        /* A domain which is already in the table (also a disabled one) is enabled and selected instead of being added again. */
        final List<CaptchaWizardEntry> existing = new ArrayList<CaptchaWizardEntry>();
        for (final CaptchaWizardEntry entry : domainTable.getAllEntries()) {
            if (entry.getDomain().equalsIgnoreCase(domain)) {
                existing.add(entry);
            }
        }
        if (!existing.isEmpty()) {
            domainTable.enableAndSelect(existing);
            return;
        }
        /* Added entries are sorted in by their captcha type and selected. */
        domainTable.addEntries(Collections.singletonList(new CaptchaWizardEntry(domain, getRandomCaptchaType(), null, true)));
    }

    /** Step 3: Collects the captcha types of the selected entries and shows them. */
    private void onTypesStepEntered() {
        requiredTypes = new LinkedHashSet<CAPTCHA_TYPE>();
        for (final CaptchaWizardEntry entry : domainTable.getEnabledEntries()) {
            requiredTypes.add(entry.getCaptchaType());
        }
        typesPanel.removeAll();
        for (final CAPTCHA_TYPE type : requiredTypes) {
            typesPanel.add(new JLabel(type.getDisplayName(), type.getIcon(), JLabel.LEADING));
        }
        typesPanel.revalidate();
        typesPanel.repaint();
    }

    /**
     * Step 4: Shows the solvers which can solve all captcha types of step 3 (all solvers are taken into account, the
     * solving method of step 1 is only a preference). Tells the user if manual solving is not possible (only if manual solving is all the
     * user wants) or if a combination of several solvers is needed.
     */
    private void onSolverStepEntered() {
        /*
         * The choice of step 1 is only a preference, not a restriction: All solvers (manual and paid ones) are taken into account for the
         * result.
         */
        /* Solvers which solve all required captcha types on their own. */
        final List<SolverService> solvers = new ArrayList<SolverService>();
        /* Solvers which solve at least one of the required captcha types, the base for a combination. */
        final List<SolverService> candidates = new ArrayList<SolverService>();
        /* The captcha types a manual solver of JDownloader can solve. */
        final Set<CAPTCHA_TYPE> manuallySolvable = new LinkedHashSet<CAPTCHA_TYPE>();
        anySolverSolvesAll = false;
        for (final SolverService service : ChallengeResponseController.getInstance().getOrderedServices()) {
            final int solved = CaptchaWizardSolverTable.getSolvedCount(service, requiredTypes);
            if (service.getType().requiresHuman() && service.getSupportedCaptchaTypes() != null) {
                manuallySolvable.addAll(service.getSupportedCaptchaTypes());
            }
            if (solved == 0) {
                continue;
            }
            candidates.add(service);
            if (solved == requiredTypes.size()) {
                solvers.add(service);
            }
        }
        anySolverSolvesAll = !solvers.isEmpty();
        /* The required captcha types which cannot be solved manually. */
        final Set<CAPTCHA_TYPE> manuallyUnsolvable = new LinkedHashSet<CAPTCHA_TYPE>(requiredTypes);
        manuallyUnsolvable.removeAll(manuallySolvable);
        /* The warning is only for users who prefer manual solving exclusively. */
        final boolean manualImpossible = isManualWanted() && !isPaidWanted() && !manuallyUnsolvable.isEmpty();
        final List<SolverService> combination;
        final Set<CAPTCHA_TYPE> unsolvable = new LinkedHashSet<CAPTCHA_TYPE>(requiredTypes);
        if (anySolverSolvesAll) {
            combination = null;
        } else {
            combination = findSmallestCombination(candidates, requiredTypes);
            for (final SolverService service : combination) {
                unsolvable.removeAll(service.getSupportedCaptchaTypes());
            }
        }
        /* What the user can already use right now. */
        final Set<CAPTCHA_TYPE> usableAutomaticTypes = new LinkedHashSet<CAPTCHA_TYPE>();
        final Set<CAPTCHA_TYPE> usableManualTypes = new LinkedHashSet<CAPTCHA_TYPE>();
        for (final SolverService service : candidates) {
            if (!service.isUsable()) {
                continue;
            }
            if (service.getType().requiresHuman()) {
                usableManualTypes.addAll(service.getSupportedCaptchaTypes());
            } else {
                usableAutomaticTypes.addAll(service.getSupportedCaptchaTypes());
            }
        }
        final Set<CAPTCHA_TYPE> usableTypes = new LinkedHashSet<CAPTCHA_TYPE>(usableAutomaticTypes);
        usableTypes.addAll(usableManualTypes);
        /* If the user can already solve all captcha types with usable solvers, the warnings below are pointless and no solver is preselected. */
        final boolean alreadyUsable = usableTypes.containsAll(requiredTypes);
        /* The table only lists solvers which solve everything, or the combination if there is no such solver. */
        shownSolvers = new ArrayList<SolverService>(combination == null ? solvers : combination);
        final int deselected = solverTable.setSolvers(shownSolvers, requiredTypes, alreadyUsable);
        deselectedInfoLabel.setText("<html>" + _GUI.T.CaptchaWizard_info_deselectedUsable(deselected) + "</html>");
        deselectedInfoLabel.setVisible(deselected > 0);
        /* Every information is one text which is shown in a dialog and in the list below the table. */
        final List<CaptchaWizardInfo> infos = new ArrayList<CaptchaWizardInfo>();
        if (manualImpossible && !alreadyUsable) {
            infos.add(new CaptchaWizardInfo(CaptchaWizardInfo.Level.WARNING, _GUI.T.CaptchaWizard_info_manualNotPossible(), manuallyUnsolvable));
        }
        if (combination != null && !combination.isEmpty() && !alreadyUsable) {
            infos.add(new CaptchaWizardInfo(CaptchaWizardInfo.Level.INFO, _GUI.T.CaptchaWizard_info_multipleSolvers(joinNames(combination)), null));
        }
        if (!unsolvable.isEmpty() && !alreadyUsable) {
            /* Some of the captcha types are not supported by any solver at all. */
            infos.add(new CaptchaWizardInfo(CaptchaWizardInfo.Level.WARNING, _GUI.T.CaptchaWizard_info_unsolvable(), unsolvable));
        }
        if (alreadyUsable) {
            infos.add(new CaptchaWizardInfo(CaptchaWizardInfo.Level.INFO, _GUI.T.CaptchaWizard_info_alreadyUsable(), null));
        }
        /* The user prefers automatic solving, but for some types only a manual solver is usable at the moment. */
        final Set<CAPTCHA_TYPE> automaticButManual = new LinkedHashSet<CAPTCHA_TYPE>(requiredTypes);
        automaticButManual.retainAll(usableManualTypes);
        automaticButManual.removeAll(usableAutomaticTypes);
        if (isPaidWanted() && !isManualWanted() && !automaticButManual.isEmpty()) {
            infos.add(new CaptchaWizardInfo(CaptchaWizardInfo.Level.WARNING, _GUI.T.CaptchaWizard_info_automaticButManual(), automaticButManual));
        }
        /* The same content is shown below the table, with a heading. */
        infoPanel.removeAll();
        if (!infos.isEmpty()) {
            infoPanel.add(createHeader(_GUI.T.CaptchaWizard_notes_header()));
            infoPanel.add(CaptchaWizardInfo.createListComponent(infos));
        }
        infoPanel.revalidate();
        infoPanel.repaint();
        if (infos.isEmpty()) {
            return;
        }
        /* One dialog for all informations, shown after this step is visible, so the user sees the table behind the message. */
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                CaptchaWizardInfo.showDialog(infos);
            }
        });
    }

    private static String joinNames(final List<SolverService> services) {
        final StringBuilder sb = new StringBuilder();
        for (final SolverService service : services) {
            if (sb.length() > 0) {
                sb.append(", ");
            }
            sb.append(service.getName());
        }
        return sb.toString();
    }

    /**
     * Finds the smallest set of solvers which together solve all required captcha types (a minimum set cover). If some required types are
     * not solved by any candidate, the smallest set which solves all the solvable ones is returned. Small candidate lists are searched
     * exhaustively by increasing size, larger ones use the greedy approximation.
     */
    static List<SolverService> findSmallestCombination(final List<SolverService> candidates, final Set<CAPTCHA_TYPE> required) {
        final Set<CAPTCHA_TYPE> target = new LinkedHashSet<CAPTCHA_TYPE>();
        for (final SolverService service : candidates) {
            for (final CAPTCHA_TYPE type : required) {
                if (service.getSupportedCaptchaTypes().contains(type)) {
                    target.add(type);
                }
            }
        }
        if (candidates.size() <= 20) {
            for (int size = 1; size <= candidates.size(); size++) {
                final List<SolverService> found = searchCombination(candidates, 0, size, new ArrayList<SolverService>(), new LinkedHashSet<CAPTCHA_TYPE>(), target);
                if (found != null) {
                    return found;
                }
            }
        }
        /* Greedy: always take the solver which solves most of the types which are still missing. */
        final List<SolverService> ret = new ArrayList<SolverService>();
        final Set<CAPTCHA_TYPE> missing = new LinkedHashSet<CAPTCHA_TYPE>(target);
        while (!missing.isEmpty()) {
            SolverService best = null;
            int bestCount = 0;
            for (final SolverService service : candidates) {
                int count = 0;
                for (final CAPTCHA_TYPE type : missing) {
                    if (service.getSupportedCaptchaTypes().contains(type)) {
                        count++;
                    }
                }
                if (count > bestCount) {
                    best = service;
                    bestCount = count;
                }
            }
            ret.add(best);
            missing.removeAll(best.getSupportedCaptchaTypes());
        }
        return ret;
    }

    private static List<SolverService> searchCombination(final List<SolverService> candidates, final int from, final int remaining, final List<SolverService> current, final Set<CAPTCHA_TYPE> covered, final Set<CAPTCHA_TYPE> target) {
        if (remaining == 0) {
            return covered.containsAll(target) ? new ArrayList<SolverService>(current) : null;
        }
        for (int i = from; i <= candidates.size() - remaining; i++) {
            final SolverService service = candidates.get(i);
            final Set<CAPTCHA_TYPE> next = new LinkedHashSet<CAPTCHA_TYPE>(covered);
            next.addAll(service.getSupportedCaptchaTypes());
            current.add(service);
            final List<SolverService> found = searchCombination(candidates, i + 1, remaining - 1, current, next, target);
            current.remove(current.size() - 1);
            if (found != null) {
                return found;
            }
        }
        return null;
    }
}
