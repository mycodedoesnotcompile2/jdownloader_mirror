package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.util.List;

import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

/**
 * Removes captcha rules. Modelled after
 * {@link jd.gui.swing.jdgui.views.settings.panels.packagizer.RemoveAction}: the toolbar instance ({@link #CaptchaRulesRemoveAction(CaptchaRulesTable)})
 * always acts on the table's current selection and is enabled/disabled from the outside; the context-menu instance carries a fixed
 * selection snapshot instead.
 */
public class CaptchaRulesRemoveAction extends AppAction {
    private static final long                serialVersionUID = 1L;
    private final CaptchaRulesTable           table;
    private List<CaptchaChallengeFilter>      selected;
    private boolean                           ignoreSelection  = false;

    public CaptchaRulesRemoveAction(final CaptchaRulesTable table) {
        this.table = table;
        this.ignoreSelection = true;
        setName(_GUI.T.CaptchaRules_remove_button());
        setIconKey(IconKey.ICON_REMOVE);
    }

    public CaptchaRulesRemoveAction(final CaptchaRulesTable table, final List<CaptchaChallengeFilter> selected, final boolean direct) {
        this.table = table;
        this.selected = selected;
        setName(_GUI.T.CaptchaRules_remove_button());
        setIconKey(IconKey.ICON_REMOVE);
    }

    private boolean rly() {
        try {
            Dialog.getInstance().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _GUI.T.literall_are_you_sure(), _GUI.T.CaptchaRules_remove_confirm(), null, null, null);
            return true;
        } catch (final DialogClosedException e) {
        } catch (final DialogCanceledException e) {
        }
        return false;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final List<CaptchaChallengeFilter> remove = selected != null ? selected : (table != null ? table.getModel().getSelectedObjects() : null);
        if (remove == null || remove.isEmpty()) {
            return;
        }
        if (!rly()) {
            return;
        }
        for (final CaptchaChallengeFilter rule : remove) {
            if (!rule.isStaticRule()) {
                CaptchaChallengeFilterController.getInstance().remove(rule);
            }
        }
        if (table != null) {
            table.getModel().refresh();
        }
    }

    @Override
    public boolean isEnabled() {
        if (ignoreSelection) {
            return super.isEnabled();
        } else if (selected != null) {
            for (final CaptchaChallengeFilter rule : selected) {
                if (!rule.isStaticRule()) {
                    return true;
                }
            }
        }
        return false;
    }
}
