package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.util.List;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;

import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.BooleanStatus;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;

public class CleanupAutoDeleteFilesEnabledToggleAction extends AbstractExtractionContextAction {

    public CleanupAutoDeleteFilesEnabledToggleAction() {
        super();
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_autodeletefiles());
        setIconKey(IconKey.ICON_DELETE);
        setSelected(false);
    }

    @Override
    protected void onAsyncInitDone(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        super.onAsyncInitDone(archives, selectionInfo);
        if (archives == null || archives.size() == 0) {
            return;
        }
        setSelected(_getExtension().getRemoveFilesAfterExtractAction(archives.get(0)) != FileCreationManager.DeleteOption.NO_DELETE);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        for (Archive archive : archives) {
            archive.getSettings().setRemoveFilesAfterExtraction(isSelected() ? BooleanStatus.TRUE : BooleanStatus.FALSE);
        }
        if (JDGui.bugme(WarnLevel.NORMAL)) {
            Dialog.getInstance().showMessageDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, isSelected() ? org.jdownloader.extensions.extraction.translate.T.T.set_autoremovefiles_true() : org.jdownloader.extensions.extraction.translate.T.T.set_autoremovefiles_false());
        }
    }

}
