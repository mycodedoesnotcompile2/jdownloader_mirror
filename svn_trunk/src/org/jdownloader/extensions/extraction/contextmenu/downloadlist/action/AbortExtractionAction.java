package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.util.List;

import jd.controlling.TaskQueue;

import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ExtractionController;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;

public class AbortExtractionAction extends AbstractExtractionContextAction {
    public AbortExtractionAction() {
        super();
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_abort());
        setIconKey(IconKey.ICON_CANCEL);
    }

    @Override
    protected void onAsyncInitDone(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        if (archives == null || archives.size() == 0) {
            /* Do nothing */
            return;
        }
        for (final Archive lArchive : archives) {
            final ExtractionController extractionController = lArchive.getExtractionController();
            if (extractionController != null && !extractionController.isFinished() && !extractionController.gotKilled()) {
                setEnabled(true);
                break;
            }
        }
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final List<Archive> archives, final SelectionInfo<?, ?> selectionInfo) {
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                for (final Archive lArchive : archives) {
                    final ExtractionController extractionController = lArchive.getExtractionController();
                    if (extractionController != null && !extractionController.isFinished() && !extractionController.gotKilled()) {
                        _getExtension().cancel(extractionController);
                    }
                }
                return null;
            }
        });
    }

}
