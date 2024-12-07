package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.List;

import jd.plugins.DownloadLink;

import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.plugins.FinalLinkState;

public class ExtractArchiveNowAction extends AbstractExtractionContextAction {
    /**
     *
     */
    public ExtractArchiveNowAction() {
        super();
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_extract());
        setIconKey(IconKey.ICON_RUN);
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        final Thread thread = new Thread() {
            @Override
            public void run() {
                for (final Archive archive : archives) {
                    if (_getExtension().isComplete(archive)) {
                        _getExtension().addToQueue(archive, true);
                    } else {
                        Dialog.getInstance().showMessageDialog(org.jdownloader.extensions.extraction.translate.T.T.cannot_extract_incomplete(archive.getName()));
                    }
                }
            }
        };
        thread.setName("Extract Context: extract");
        thread.setDaemon(true);
        thread.start();
    }

    @Override
    protected void onAsyncInitDone(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        setEnabled(isEnabled(archives, selectionInfo));
    }

    private boolean isEnabled(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        if (selectionInfo == null || selectionInfo.isEmpty()) {
            return false;
        }
        if (archives == null || archives.isEmpty()) {
            return false;
        }
        /**
         * Check if at least one selected item is a finished download or a while which exists. </br> This is just a very simple check to
         * provide visual feedback (grey-out action on non allowed items). </br> Believe it or not but some people are using this feature to
         * extract files that they've never downloaded via JDownloader.
         */
        for (final Object o : selectionInfo.getChildren()) {
            if (!(o instanceof DownloadLink)) {
                continue;
            }
            final DownloadLink dl = (DownloadLink) o;
            if (!dl.isEnabled()) {
                continue;
            }
            if (FinalLinkState.CheckFinished(dl.getFinalLinkState())) {
                return true;
            } else if (new File(dl.getFileOutput()).exists()) {
                /* Target file exists so we can extract it. */
                return true;
            }
        }
        return false;
    }
}