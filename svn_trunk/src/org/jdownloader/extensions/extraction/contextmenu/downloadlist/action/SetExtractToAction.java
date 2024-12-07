package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.List;

import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.DownloadFolderChooserDialog;
import org.jdownloader.gui.views.SelectionInfo;

public class SetExtractToAction extends AbstractExtractionContextAction {
    public SetExtractToAction() {
        super();
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_extract_to());
        setIconKey(IconKey.ICON_FOLDER);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        File extractto = _getExtension().getFinalExtractToFolder(archives.get(0), true);
        while (extractto != null && !extractto.isDirectory() && !isTag(extractto.getName())) {
            extractto = extractto.getParentFile();
        }
        try {
            File path = DownloadFolderChooserDialog.open(extractto, true, org.jdownloader.extensions.extraction.translate.T.T.extract_to2());
            if (path == null) {
                return;
            }
            for (Archive archive : archives) {
                archive.getSettings().setExtractPath(path.getAbsolutePath());
            }
        } catch (DialogClosedException e1) {
            e1.printStackTrace();
        } catch (DialogCanceledException e1) {
            e1.printStackTrace();
        }
    }

    private boolean isTag(String name) {
        return name.matches(".*\\<.+\\>.*");
    }
}
