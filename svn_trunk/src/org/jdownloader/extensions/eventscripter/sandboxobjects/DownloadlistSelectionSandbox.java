package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.ArrayList;
import java.util.List;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;

public class DownloadlistSelectionSandbox {
    private final SelectionInfo<FilePackage, DownloadLink> selectionInfo;

    public DownloadlistSelectionSandbox(SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        this.selectionInfo = selectionInfo;
    }

    @Override
    public int hashCode() {
        if (selectionInfo != null) {
            return selectionInfo.hashCode();
        } else {
            return super.hashCode();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DownloadlistSelectionSandbox) {
            return ((DownloadlistSelectionSandbox) obj).selectionInfo == selectionInfo;
        } else {
            return super.equals(obj);
        }
    }

    public DownloadlistSelectionSandbox() {
        this(null);
    }

    public DownloadLinkSandBox[] getLinks() {
        if (selectionInfo == null) {
            return null;
        }
        final List<DownloadLink> childs = selectionInfo.getChildren();
        final DownloadLinkSandBox[] ret = new DownloadLinkSandBox[childs.size()];
        for (int i = 0; i < ret.length; i++) {
            ret[i] = new DownloadLinkSandBox(childs.get(i));
        }
        return ret;
    }

    public DownloadLinkSandBox[] getSelectedLinks(final FilePackageSandBox filePackageSandbox) {
        if (selectionInfo == null || filePackageSandbox == null || filePackageSandbox.filePackage == null) {
            return null;
        }
        for (PackageView<FilePackage, DownloadLink> packageView : selectionInfo.getPackageViews()) {
            if (packageView.getPackage() == filePackageSandbox.filePackage) {
                final List<DownloadLinkSandBox> ret = new ArrayList<DownloadLinkSandBox>();
                final List<DownloadLink> children = packageView.getSelectedChildren();
                if (children != null) {
                    for (DownloadLink child : children) {
                        ret.add(new DownloadLinkSandBox(child));
                    }
                }
                return ret.toArray(new DownloadLinkSandBox[0]);
            }
        }
        return null;
    }

    @Deprecated
    public DownloadLinkSandBox[] getDownloadLinks() {
        return getLinks();
    }

    public boolean isLinkContext() {
        if (selectionInfo != null) {
            return selectionInfo.isLinkContext();
        } else {
            return false;
        }
    }

    public boolean isPackageContext() {
        if (selectionInfo != null) {
            return selectionInfo.isPackageContext();
        } else {
            return false;
        }
    }

    public boolean isPackageSelected(final FilePackageSandBox filePackageSandbox) {
        if (selectionInfo == null || filePackageSandbox == null || filePackageSandbox.filePackage == null) {
            return false;
        }
        for (PackageView<FilePackage, DownloadLink> packageView : selectionInfo.getPackageViews()) {
            if (packageView.getPackage() == filePackageSandbox.filePackage) {
                return packageView.isPackageSelected();
            }
        }
        return false;
    }

    public FilePackageSandBox[] getPackages() {
        return getPackages(false);
    }

    public FilePackageSandBox[] getPackages(final boolean includeSelectedOnly) {
        if (selectionInfo == null) {
            return null;
        }
        final List<PackageView<FilePackage, DownloadLink>> packageViews = selectionInfo.getPackageViews();
        final List<FilePackageSandBox> ret = new ArrayList<FilePackageSandBox>(packageViews.size());
        for (PackageView<FilePackage, DownloadLink> packageView : packageViews) {
            if (!includeSelectedOnly || packageView.isPackageSelected()) {
                ret.add(new FilePackageSandBox(packageView.getPackage()));
            }
        }
        return ret.toArray(new FilePackageSandBox[0]);
    }

    public FilePackageSandBox getContextPackage() {
        if (selectionInfo == null) {
            return new FilePackageSandBox();
        } else if (isPackageContext()) {
            final FilePackage cl = selectionInfo.getContextPackage();
            return cl == null ? null : new FilePackageSandBox(cl);
        } else {
            return null;
        }
    }

    public DownloadLinkSandBox getContextLink() {
        if (selectionInfo == null) {
            return new DownloadLinkSandBox();
        } else if (isLinkContext()) {
            final DownloadLink cl = selectionInfo.getContextLink();
            return cl == null ? null : new DownloadLinkSandBox(cl);
        } else {
            return null;
        }
    }
}
