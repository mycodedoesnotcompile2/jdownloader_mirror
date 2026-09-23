package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter.CaptchaFilterType;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.AbstractAddAction;

/** Adds a new {@link CaptchaChallengeFilter} rule, after letting the user pick which solver it applies to. */
public class CaptchaRulesAddAction extends AbstractAddAction {
    private static final long   serialVersionUID = 1L;
    private final CaptchaRulesTable table;

    public CaptchaRulesAddAction(final CaptchaRulesTable table) {
        super();
        setName(_GUI.T.CaptchaRules_add_button());
        this.table = table;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final List<SolverService> services = new ArrayList<SolverService>(ChallengeResponseController.getInstance().listServices());
        if (services.isEmpty()) {
            Dialog.getInstance().showErrorDialog(_GUI.T.CaptchaRules_add_no_solver());
            return;
        }
        final SolverService[] options = services.toArray(new SolverService[services.size()]);
        final int selectedIndex;
        try {
            selectedIndex = Dialog.getInstance().showComboDialog(0, _GUI.T.CaptchaRules_add_title(), _GUI.T.CaptchaRules_add_message(), options, 0, null, null, null, new SolverListCellRenderer());
        } catch (final DialogNoAnswerException ex) {
            /* User cancelled or closed the dialog */
            return;
        }
        if (selectedIndex < 0) {
            return;
        }
        final SolverService chosen = options[selectedIndex];
        final CaptchaChallengeFilter rule = new CaptchaChallengeFilter();
        rule.setSolver(chosen.getID());
        rule.setName(_GUI.T.CaptchaRules_new_rule_name(chosen.getName()));
        rule.setFilterType(CaptchaFilterType.BLACKLIST);
        rule.setDomain("example.com");
        rule.setEnabled(true);
        rule.setRegex(false);
        /* No exclusions by default, i.e. the new rule applies to all request types (see CompiledCaptchaChallengeFilter#matchesRequestType). */
        rule.setPosition(CaptchaChallengeFilterController.getInstance().list().size());
        CaptchaChallengeFilterController.getInstance().add(rule);
        if (table != null) {
            table.getModel().refresh();
            /* Land in the "Name" cell with its (default) text pre-selected, ready to be overwritten. */
            table.getModel().startEditingName(rule);
        }
    }
}
