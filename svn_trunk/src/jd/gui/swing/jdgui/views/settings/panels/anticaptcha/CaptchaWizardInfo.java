package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import net.miginfocom.swing.MigLayout;

/**
 * One information of the last step of the {@link CaptchaWizardDialog}: a text (one translation) and optionally the captcha types the text
 * is about, one per line with their icon. The same information is shown in an info dialog and in the list below the solver table, see
 * {@link #createComponent()}.
 */
public class CaptchaWizardInfo {
    public enum Level {
        INFO,
        /** Everything is fine: shown with a green check mark. */
        SUCCESS,
        WARNING
    }

    private final Level                level;
    private final String               text;
    private final List<CAPTCHA_TYPE>   types;

    public CaptchaWizardInfo(final Level level, final String text, final Collection<CAPTCHA_TYPE> types) {
        this.level = level;
        this.text = text;
        this.types = types == null ? new ArrayList<CAPTCHA_TYPE>() : new ArrayList<CAPTCHA_TYPE>(types);
    }

    /** Moves the warnings in front of the infos. The order within the warnings and within the infos stays the same. */
    public static void sortWarningsFirst(final List<CaptchaWizardInfo> infos) {
        final List<CaptchaWizardInfo> warnings = new ArrayList<CaptchaWizardInfo>();
        final List<CaptchaWizardInfo> others = new ArrayList<CaptchaWizardInfo>();
        for (final CaptchaWizardInfo info : infos) {
            switch (info.level) {
            case WARNING:
                warnings.add(info);
                break;
            case SUCCESS:
            case INFO:
            default:
                others.add(info);
                break;
            }
        }
        infos.clear();
        infos.addAll(warnings);
        infos.addAll(others);
    }

    private Icon getIcon() {
        switch (level) {
        case WARNING:
            return NewTheme.I().getIcon(IconKey.ICON_WARNING, 18);
        case SUCCESS:
            return NewTheme.I().getIcon(IconKey.ICON_OK, 18);
        case INFO:
        default:
            return NewTheme.I().getIcon(IconKey.ICON_INFO, 18);
        }
    }

    /** Icon and text, below the text the captcha types (if any). */
    public JComponent createComponent() {
        final JPanel panel = new JPanel(new MigLayout("ins 0, wrap 2", "[][grow,fill]", "[]"));
        panel.setOpaque(false);
        panel.add(new JLabel(getIcon()), "aligny top");
        panel.add(new JLabel("<html>" + text + "</html>"), "wmin 0");
        for (final CAPTCHA_TYPE type : types) {
            panel.add(new JLabel(type.getDisplayName(), type.getIcon(), JLabel.LEADING), "skip 1, gapleft 10");
        }
        return panel;
    }

    /** All given informations below each other. The dialog and the list below the solver table both use this, so they look the same. */
    public static JComponent createListComponent(final List<CaptchaWizardInfo> infos) {
        final JPanel panel = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[]"));
        panel.setOpaque(false);
        for (final CaptchaWizardInfo info : infos) {
            panel.add(info.createComponent(), "wmin 0");
        }
        return panel;
    }

    /** Shows all given informations in one dialog (with a Close button only). */
    public static void showDialog(final List<CaptchaWizardInfo> infos) {
        try {
            Dialog.getInstance().showDialog(new InfoDialog(infos));
        } catch (final DialogNoAnswerException e) {
            /* Closed: nothing to do, it is only an information. */
        }
    }

    private static class InfoDialog extends AbstractDialog<Integer> {
        private final List<CaptchaWizardInfo> infos;

        private InfoDialog(final List<CaptchaWizardInfo> infos) {
            super(UIOManager.BUTTONS_HIDE_CANCEL | Dialog.STYLE_HIDE_ICON, _GUI.T.CaptchaWizard_title(), null, _GUI.T.lit_close(), null);
            this.infos = infos;
        }

        @Override
        protected Integer createReturnValue() {
            return getReturnmask();
        }

        @Override
        protected int getPreferredWidth() {
            return 500;
        }

        @Override
        public JComponent layoutDialogContent() {
            return createListComponent(infos);
        }
    }
}
