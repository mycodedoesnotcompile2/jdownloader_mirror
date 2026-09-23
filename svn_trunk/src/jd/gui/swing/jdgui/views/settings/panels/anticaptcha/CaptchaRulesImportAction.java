package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.filechooser.FileFilter;

import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;

/** Imports captcha rules from a JSON file, modelled after the packagizer filter table's {@code ImportAction}. */
public class CaptchaRulesImportAction extends AppAction {
    private static final long       serialVersionUID = 1L;
    private final CaptchaRulesTable table;

    public CaptchaRulesImportAction(final CaptchaRulesTable table) {
        setIconKey(IconKey.ICON_IMPORT);
        setName(_GUI.T.CaptchaRules_import_button());
        this.table = table;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        try {
            final ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.CaptchaRules_import_dialog_title(), null, null);
            d.setFileSelectionMode(FileChooserSelectionMode.FILES_ONLY);
            d.setFileFilter(new FileFilter() {
                @Override
                public String getDescription() {
                    return "*" + CaptchaRulesExportAction.EXT;
                }

                @Override
                public boolean accept(final File f) {
                    return f.isDirectory() || StringUtils.endsWithCaseInsensitive(f.getName(), CaptchaRulesExportAction.EXT);
                }
            });
            d.setType(FileChooserType.OPEN_DIALOG);
            d.setMultiSelection(true);
            Dialog.I().showDialog(d);
            final File[] files = d.getSelection();
            if (files == null || files.length == 0) {
                return;
            }
            /* File IO (and the JSON parsing) is done off the EDT, then the table is refreshed back on the EDT. */
            final Thread thread = new Thread("Import captcha rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    for (final File file : files) {
                        if (file != null && file.isFile()) {
                            CaptchaChallengeFilterController.getInstance().importList(file);
                        }
                    }
                    if (table != null) {
                        new EDTRunner() {
                            @Override
                            protected void runInEDT() {
                                table.getModel().refresh();
                            }
                        };
                    }
                }
            };
            thread.start();
        } catch (final DialogNoAnswerException e1) {
            LogController.CL().log(e1);
        }
    }
}
