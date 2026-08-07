package jd.gui.swing.jdgui.views.settings.panels.packagizer;

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
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.controlling.packagizer.PackagizerRule;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;
import org.jdownloader.translate._JDT;

public class ExportAction extends AppAction {
    /**
     *
     */
    private static final long          serialVersionUID = 1L;
    private final List<PackagizerRule> rules;
    private final PackagizerFilter     packagizer;

    public ExportAction(PackagizerFilter packagizer, List<PackagizerRule> selection) {
        setName(_GUI.T.LinkgrabberFilter_LinkgrabberFilter_export());
        setIconKey(IconKey.ICON_EXPORT);
        setTooltipText(_JDT.T.ExportAction_ExportAction_tt());
        this.rules = selection;
        this.packagizer = packagizer;
    }

    public boolean isEnabled() {
        return rules == null || rules.size() > 0;
    }

    public void actionPerformed(ActionEvent e) {
        try {
            List<PackagizerRule> export = rules;
            if (export == null) {
                export = packagizer.getTable().getModel().getTableData();
            }
            if (export == null || export.size() == 0) {
                return;
            }
            final String ext = ImportAction.EXT;
            ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.LinkgrabberFilter_export_dialog_title(), null, null);
            d.setFileSelectionMode(FileChooserSelectionMode.FILES_ONLY);
            d.setFileFilter(new FileFilter() {
                @Override
                public String getDescription() {
                    return "*" + ext;
                }

                @Override
                public boolean accept(File f) {
                    return f.isDirectory() || StringUtils.endsWithCaseInsensitive(f.getName(), ext);
                }
            });
            d.setType(FileChooserType.SAVE_DIALOG);
            d.setMultiSelection(false);
            Dialog.I().showDialog(d);
            File saveto = d.getSelectedFile();
            if (saveto == null) {
                return;
            }
            if (!saveto.getName().endsWith(ext)) {
                saveto = new File(saveto.getAbsolutePath() + ext);
            }
            // Snapshot the rules on the EDT (getTableData() may be a live list), then write off the EDT: the file IO (and the
            // error dialog) would otherwise block the GUI.
            final List<PackagizerRule> exportList = new ArrayList<PackagizerRule>(export);
            final File target = saveto;
            final Thread thread = new Thread("Export packagizer rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    PackagizerController.getInstance().exportList(target, exportList, true);
                }
            };
            thread.start();
        } catch (DialogNoAnswerException e1) {
            LogController.CL().log(e1);
        }
    }
}