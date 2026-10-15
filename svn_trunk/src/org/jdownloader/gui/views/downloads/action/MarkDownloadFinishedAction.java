package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;

import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.plugins.FinalLinkState;

public class MarkDownloadFinishedAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> {
    private static final long   serialVersionUID = 8087143123808363305L;
    private final static String NAME             = _GUI.T.gui_table_contextmenu_markfinished();

    public MarkDownloadFinishedAction() {
        setIconKey(IconKey.ICON_TRUE);
        setName(NAME);
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (SelectionInfo.isEmpty(selectionInfo)) {
            return;
        }
        DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
            @Override
            public boolean isHighPriority() {
                return false;
            }

            @Override
            public void interrupt() {
            }

            private void setFinished(DownloadLink link) {
                link.setFinalLinkState(FinalLinkState.FINISHED);
                final long knownSize = link.getKnownDownloadSize();
                if (knownSize >= 0) {
                    link.setDownloadCurrent(knownSize);
                }
            }

            @Override
            public void execute(DownloadSession currentSession) {
                for (final DownloadLink link : selectionInfo.getChildren()) {
                    final DownloadWatchDogJob setFinishedJob = new DownloadWatchDogJob() {
                        @Override
                        public boolean isHighPriority() {
                            return false;
                        }

                        @Override
                        public void interrupt() {
                        }

                        @Override
                        public void execute(DownloadSession currentSession) {
                            setFinished(link);
                        }
                    };
                    final SingleDownloadController con = link.getDownloadLinkController();
                    if (con == null || !con.isAlive()) {
                        setFinishedJob.execute(currentSession);
                    } else {
                        con.getJobsAfterDetach().add(setFinishedJob);
                    }
                }
            }
        });
    }
}