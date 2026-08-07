package jd.gui.swing.jdgui.views.settings.panels.packagizer;

import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.filechooser.FileFilter;

import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;
import org.jdownloader.translate._JDT;

public class ImportAction extends AppAction {
    public static final String EXT              = ".packagizer";
    /**
     *
     */
    private static final long  serialVersionUID = 1L;

    public ImportAction(PackagizerFilter packagizer) {
        setIconKey(IconKey.ICON_IMPORT);
        setName(_GUI.T.LinkgrabberFilter_LinkgrabberFilter_import());
        setTooltipText(_JDT.T.ImportAction_tt());
    }

    public void actionPerformed(ActionEvent e) {
        try {
            final ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.Packagizer_import_dialog_title(), null, null);
            d.setFileSelectionMode(FileChooserSelectionMode.FILES_ONLY);
            d.setFileFilter(new FileFilter() {
                @Override
                public String getDescription() {
                    return "*" + EXT;
                }

                @Override
                public boolean accept(File f) {
                    return f.isDirectory() || StringUtils.endsWithCaseInsensitive(f.getName(), EXT);
                }
            });
            d.setType(FileChooserType.OPEN_DIALOG);
            d.setMultiSelection(true);
            Dialog.I().showDialog(d);
            final File[] files = d.getSelection();
            if (files == null || files.length == 0) {
                return;
            }
            // Read/parse/import off the EDT: importList does file IO and may show a modal dialog, which would block the GUI.
            final Thread thread = new Thread("Import packagizer rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    for (final File file : files) {
                        if (!file.isFile()) {
                            continue;
                        }
                        PackagizerController.getInstance().importList(file, true);
                    }
                }
            };
            thread.start();
        } catch (DialogNoAnswerException e1) {
            LogController.CL().log(e1);
        }
    }
}