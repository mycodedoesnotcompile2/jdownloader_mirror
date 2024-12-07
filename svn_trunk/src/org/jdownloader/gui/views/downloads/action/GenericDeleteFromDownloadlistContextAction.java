package org.jdownloader.gui.views.downloads.action;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.controlling.contextmenu.TableContext;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.bottombar.IncludedSelectionSetup;

public class GenericDeleteFromDownloadlistContextAction extends GenericDeleteFromDownloadlistAction {
    private final TableContext tableContext;

    public GenericDeleteFromDownloadlistContextAction() {
        super();
        addContextSetup(0, tableContext = new TableContext(false, true));
    }

    protected void initIncludeSelectionSupport() {
        addContextSetup(1, includedSelection = new IncludedSelectionSetup(DownloadsTable.getInstance(), this, this) {
            @Override
            public void updateListeners() {
            }
        });
    }

    @Override
    protected void onUpdate(final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        super.onUpdate(selectionType, selectionInfo);
        final boolean hasSelection = selectionInfo != null && !selectionInfo.isEmpty();
        if (hasSelection) {
            if (tableContext.isItemVisibleForSelections()) {
                setVisible(true);
            } else {
                setVisible(false);
                setEnabled(false);
            }
        } else {
            if (tableContext.isItemVisibleForEmptySelection()) {
                setVisible(true);
            } else {
                setVisible(false);
                setEnabled(false);
            }
        }
    }

    @Override
    public void initContextDefaults() {
        includedSelection.setIncludeSelectedLinks(true);
        includedSelection.setIncludeUnselectedLinks(false);
    }
}
