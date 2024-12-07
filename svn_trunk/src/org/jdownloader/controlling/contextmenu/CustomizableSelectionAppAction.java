package org.jdownloader.controlling.contextmenu;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberView;

public abstract class CustomizableSelectionAppAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableAppAction {

    @SuppressWarnings("unchecked")
    @Deprecated
    protected SelectionInfo<PackageType, ChildrenType> getSelection() {
        final View view = MainTabbedPane.getInstance().getSelectedView();
        if (view instanceof DownloadsView) {
            return (SelectionInfo<PackageType, ChildrenType>) DownloadsTable.getInstance().getSelectionInfo(true, true);
        } else if (view instanceof LinkGrabberView) {
            return (SelectionInfo<PackageType, ChildrenType>) LinkGrabberTable.getInstance().getSelectionInfo(true, true);
        } else {
            return null;
        }
    }

    protected boolean hasSelection(SelectionInfo<?, ?> selection) {
        return selection != null && !selection.isEmpty();
    }

    protected void getSelection(final SelectionInfoCallback<PackageType, ChildrenType> callback, final SelectionType selectionType) {
        getViewSelection(callback, selectionType);
    }

    public static void getViewSelection(final SelectionInfoCallback callback, final SelectionType selectionType) {
        final View view = MainTabbedPane.getInstance().getSelectedView();
        if (view instanceof DownloadsView) {
            DownloadsTable.getInstance().getSelectionInfo(new SelectionInfoCallback<FilePackage, DownloadLink>() {

                @Override
                public void onSelectionInfo(final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                    callback.onSelectionInfo(selectionInfo);
                }

                @Override
                public boolean isCancelled() {
                    return callback.isCancelled();
                }
            }, selectionType);
        } else if (view instanceof LinkGrabberView) {
            LinkGrabberTable.getInstance().getSelectionInfo(new SelectionInfoCallback<CrawledPackage, CrawledLink>() {

                @Override
                public void onSelectionInfo(final SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
                    callback.onSelectionInfo(selectionInfo);
                }

                @Override
                public boolean isCancelled() {
                    return callback.isCancelled();
                }
            }, selectionType);
        }
    }

    @Override
    public void requestUpdate(Object requestor) {
        super.requestUpdate(requestor);
        setEnabled(hasSelection());
    }

    protected boolean hasSelection() {
        final SelectionInfo<?, ?> selectionInfo = getSelection();
        return hasSelection(selectionInfo);
    }

}
