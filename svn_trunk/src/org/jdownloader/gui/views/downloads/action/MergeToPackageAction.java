package org.jdownloader.gui.views.downloads.action;

import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModel;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class MergeToPackageAction extends org.jdownloader.gui.views.linkgrabber.contextmenu.AbstractMergeToPackageAction<FilePackage, DownloadLink> {
    private static final long serialVersionUID = -4468197802870765463L;

    public MergeToPackageAction() {
        super();
    }

    @Override
    protected FilePackage createNewPackage(final String name, final String downloadFolder) {
        final FilePackage newPackage = FilePackage.getInstance();
        newPackage.setName(name);
        newPackage.setDownloadDirectory(PackagizerController.replaceDynamicTags(downloadFolder, name, newPackage));
        return newPackage;
    }

    @Override
    protected PackageControllerTableModel<FilePackage, DownloadLink> getTableModel() {
        return DownloadsTableModel.getInstance();
    }
}
