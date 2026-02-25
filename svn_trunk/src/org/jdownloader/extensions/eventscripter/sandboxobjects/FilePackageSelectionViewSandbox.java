package org.jdownloader.extensions.eventscripter.sandboxobjects;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.gui.views.SelectionInfo.PackageView;

public class FilePackageSelectionViewSandbox {

    private final PackageView<FilePackage, DownloadLink> view;

    public FilePackageSelectionViewSandbox(PackageView<FilePackage, DownloadLink> view) {
        this.view = view;
    }

    private DownloadLinkSandBox[] children;

    public DownloadLinkSandBox[] getChildren() {
        DownloadLinkSandBox[] children = this.children;
        if (children == null) {
            this.children = children = DownloadLinkSandBox.wrapSandBox(view.getChildren());
        }
        return children;
    }

    private FilePackageSandBox fp;

    public FilePackageSandBox getPackage() {
        FilePackageSandBox fp = this.fp;
        if (fp == null) {
            this.fp = fp = new FilePackageSandBox(view.getPackage());
        }
        return fp;
    }

    public boolean isPackageSelected() {
        return view.isPackageSelected();
    }

    private DownloadLinkSandBox[] selected;

    public DownloadLinkSandBox[] getSelectedChildren() {
        DownloadLinkSandBox[] selected = this.selected;
        if (selected == null) {
            this.selected = selected = DownloadLinkSandBox.wrapSandBox(view.getSelectedChildren());
        }
        return selected;
    }

    public boolean isExpanded() {
        return view.isExpanded();
    }
}
