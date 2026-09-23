package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.filechooser.FileFilter;

import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;

/** Exports captcha rules to a JSON file, modelled after the packagizer filter table's {@code ExportAction}. */
public class CaptchaRulesExportAction extends AppAction {
    private static final long                  serialVersionUID = 1L;
    public static final String                 EXT              = ".captchafilter";
    private final List<CaptchaChallengeFilter> rules;
    private final CaptchaRulesTable            table;

    /** @param selection null exports all (non-static) rules, otherwise only the given selection. */
    public CaptchaRulesExportAction(final CaptchaRulesTable table, final List<CaptchaChallengeFilter> selection) {
        setName(_GUI.T.CaptchaRules_export_button());
        setIconKey(IconKey.ICON_EXPORT);
        this.rules = selection;
        this.table = table;
    }

    @Override
    public boolean isEnabled() {
        return rules == null || rules.size() > 0;
    }

    /** Removes the non-persisted static example rule from a list before exporting/counting. */
    private static List<CaptchaChallengeFilter> withoutStaticRules(final List<CaptchaChallengeFilter> rules) {
        final List<CaptchaChallengeFilter> ret = new ArrayList<CaptchaChallengeFilter>();
        for (final CaptchaChallengeFilter rule : rules) {
            if (!rule.isStaticRule()) {
                ret.add(rule);
            }
        }
        return ret;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        try {
            List<CaptchaChallengeFilter> export = rules;
            if (export == null) {
                export = CaptchaChallengeFilterController.getInstance().list();
            } else {
                export = withoutStaticRules(export);
            }
            if (export == null || export.isEmpty()) {
                return;
            }
            final ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.CaptchaRules_export_dialog_title(), null, null);
            d.setFileSelectionMode(FileChooserSelectionMode.FILES_ONLY);
            d.setFileFilter(new FileFilter() {
                @Override
                public String getDescription() {
                    return "*" + EXT;
                }

                @Override
                public boolean accept(final File f) {
                    return f.isDirectory() || StringUtils.endsWithCaseInsensitive(f.getName(), EXT);
                }
            });
            d.setType(FileChooserType.SAVE_DIALOG);
            d.setMultiSelection(false);
            Dialog.I().showDialog(d);
            File saveto = d.getSelectedFile();
            if (saveto == null) {
                return;
            }
            if (!StringUtils.endsWithCaseInsensitive(saveto.getName(), EXT)) {
                saveto = new File(saveto.getAbsolutePath() + EXT);
            }
            final List<CaptchaChallengeFilter> exportList = new ArrayList<CaptchaChallengeFilter>(export);
            final File target = saveto;
            final Thread thread = new Thread("Export captcha rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    CaptchaChallengeFilterController.getInstance().exportList(target, exportList);
                }
            };
            thread.start();
        } catch (final DialogNoAnswerException e1) {
            LogController.CL().log(e1);
        }
    }
}
