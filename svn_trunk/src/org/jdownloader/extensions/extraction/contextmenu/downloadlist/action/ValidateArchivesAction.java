package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.util.List;

import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.DummyArchive;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.extensions.extraction.gui.DummyArchiveDialog;
import org.jdownloader.extensions.extraction.multi.CheckException;
import org.jdownloader.extensions.extraction.translate.T;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;

public class ValidateArchivesAction extends AbstractExtractionContextAction {

    public ValidateArchivesAction() {
        super();
        setName(T.T.ValidateArchiveAction_ValidateArchiveAction_object_());
        setIconKey(IconKey.ICON_HASHSUM);
    }

    @Override
    protected void onAsyncInitDone(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        if (archives != null && archives.size() > 0) {
            if (archives.size() > 1) {
                setName(T.T.ValidateArchiveAction_ValidateArchiveAction_multi());
            } else {
                setName(T.T.ValidateArchiveAction_ValidateArchiveAction(archives.get(0).getName()));
            }
        }
        super.onAsyncInitDone(archives, selectionInfo);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, final List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        new Thread("ValidateArchivesAction") {
            {
                setDaemon(true);
            }

            public void run() {
                try {
                    for (Archive archive : archives) {
                        try {
                            DummyArchive da = ArchiveValidator.EXTENSION.createDummyArchive(archive);
                            DummyArchiveDialog d = new DummyArchiveDialog(da);
                            try {
                                Dialog.getInstance().showDialog(d);
                            } catch (DialogCanceledException e) {
                            }
                        } catch (CheckException e1) {
                            Dialog.getInstance().showExceptionDialog("Error", "Cannot Check Archive", e1);
                        }
                    }
                } catch (DialogClosedException e1) {
                }
            };
        }.start();
    }
}
