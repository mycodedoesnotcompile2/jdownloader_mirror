package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;

import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

/** Context-menu action duplicating the rule that was right-clicked, like {@code DuplicateAction} in the packagizer filter table. */
public class CaptchaRulesDuplicateAction extends AppAction {
    private static final long          serialVersionUID = 1L;
    private final CaptchaChallengeFilter contextObject;
    private final CaptchaRulesTable      table;

    public CaptchaRulesDuplicateAction(final CaptchaChallengeFilter contextObject, final CaptchaRulesTable table) {
        setName(_GUI.T.CaptchaRules_duplicate_button());
        setIconKey(IconKey.ICON_COPY);
        this.contextObject = contextObject;
        this.table = table;
    }

    @Override
    public boolean isEnabled() {
        return contextObject != null && !contextObject.isStaticRule();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final CaptchaChallengeFilter newRule = contextObject.duplicate();
        newRule.setPosition(CaptchaChallengeFilterController.getInstance().list().size());
        CaptchaChallengeFilterController.getInstance().add(newRule);
        if (table != null) {
            table.getModel().refresh();
        }
    }
}
