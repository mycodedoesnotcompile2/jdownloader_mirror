package org.jdownloader.extensions.extraction.contextmenu.downloadlist;

import java.awt.event.ActionEvent;
import java.lang.ref.WeakReference;
import java.util.List;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.extensions.AbstractExtensionAction;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ExtractionExtension;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator.ArchiveValidation;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberView;

public abstract class AbstractExtractionContextAction extends AbstractExtensionAction<ExtractionExtension> {

    protected interface ArchivesCallback {
        public void onArchives(List<Archive> archives, SelectionInfo<?, ?> selectionInfo);

        public boolean isCancellable();
    }

    public AbstractExtractionContextAction() {
        super();
    }

    @Override
    public void requestUpdate(Object requestor) {
        super.requestUpdate(requestor);
        requestUpdateSelection();
    }

    private volatile WeakReference<SelectionInfoCallback<?, ?>> lastCallBack = new WeakReference<SelectionInfoCallback<?, ?>>(null);

    protected void getSelectedArchives(final ArchivesCallback callback) {
        final View view = MainTabbedPane.getInstance().getSelectedView();
        if (view instanceof DownloadsView) {
            DownloadsTable.getInstance().getSelectionInfo(new SelectionInfoCallback<FilePackage, DownloadLink>() {
                {
                    lastCallBack = new WeakReference<SelectionInfoCallback<?, ?>>(this);
                }

                @Override
                public void onSelectionInfo(final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                    final ArchiveValidation result = ArchiveValidator.validate(selectionInfo, true);
                    result.executeWhenReached(new Runnable() {

                        @Override
                        public void run() {
                            callback.onArchives(result.getArchives(), selectionInfo);
                        }

                    });
                }

                @Override
                public boolean isCancelled() {
                    if (!callback.isCancellable()) {
                        return false;
                    }
                    final WeakReference<SelectionInfoCallback<?, ?>> lastCallBack = AbstractExtractionContextAction.this.lastCallBack;
                    return lastCallBack.get() != this;
                }
            }, SelectionType.SELECTED);
        } else if (view instanceof LinkGrabberView) {
            LinkGrabberTable.getInstance().getSelectionInfo(new SelectionInfoCallback<CrawledPackage, CrawledLink>() {
                {
                    lastCallBack = new WeakReference<SelectionInfoCallback<?, ?>>(this);
                }

                @Override
                public void onSelectionInfo(final SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
                    final ArchiveValidation result = ArchiveValidator.validate(selectionInfo, true);
                    result.executeWhenReached(new Runnable() {

                        @Override
                        public void run() {
                            callback.onArchives(result.getArchives(), selectionInfo);
                        }

                    });
                }

                @Override
                public boolean isCancelled() {
                    if (!callback.isCancellable()) {
                        return false;
                    }
                    final WeakReference<SelectionInfoCallback<?, ?>> lastCallBack = AbstractExtractionContextAction.this.lastCallBack;
                    return lastCallBack.get() != this;
                }
            }, SelectionType.SELECTED);

        }
    }

    protected void onAsyncInitDone(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        if (selectionInfo != null && !selectionInfo.isEmpty()) {
            setVisible(true);
            if (archives != null && archives.size() > 0) {
                super.setEnabled(true);
            } else {
                super.setEnabled(false);
            }
        } else {
            setVisible(false);
            setEnabled(false);
        }
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        getSelectedArchives(new ArchivesCallback() {

            @Override
            public void onArchives(final List<Archive> archives, final SelectionInfo<?, ?> selectionInfo) {
                new EDTRunner() {
                    protected void runInEDT() {
                        if (archives == null) {
                            return;
                        }
                        if (!isEnabled()) {
                            return;
                        }
                        onActionPerformed(e, archives, selectionInfo);
                    };
                };

            }

            @Override
            public boolean isCancellable() {
                return false;
            }
        });
    }

    protected abstract void onActionPerformed(final ActionEvent e, final List<Archive> archives, SelectionInfo<?, ?> selectionInfo);

    protected void requestUpdateSelection() {
        setEnabled(false);
        getSelectedArchives(new ArchivesCallback() {

            @Override
            public void onArchives(final List<Archive> archives, final SelectionInfo<?, ?> selectionInfo) {
                new EDTRunner() {

                    @Override
                    protected void runInEDT() {
                        onAsyncInitDone(archives, selectionInfo);
                    }
                };

            }

            @Override
            public boolean isCancellable() {
                return true;
            }
        });

    }

}
