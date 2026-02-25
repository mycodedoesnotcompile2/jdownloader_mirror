package org.jdownloader.extensions.eventscripter.sandboxobjects;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;

import org.jdownloader.gui.views.SelectionInfo.PackageView;

public class CrawledPackageSelectionViewSandbox {

    private final PackageView<CrawledPackage, CrawledLink> view;

    public CrawledPackageSelectionViewSandbox(PackageView<CrawledPackage, CrawledLink> view) {
        this.view = view;
    }

    private CrawledLinkSandbox[] children;

    public CrawledLinkSandbox[] getChildren() {
        CrawledLinkSandbox[] children = this.children;
        if (children == null) {
            this.children = children = CrawledLinkSandbox.wrapSandBox(view.getChildren());
        }
        return children;
    }

    private CrawledPackageSandbox fp;

    public CrawledPackageSandbox getPackage() {
        CrawledPackageSandbox fp = this.fp;
        if (fp == null) {
            this.fp = fp = new CrawledPackageSandbox(view.getPackage());
        }
        return fp;
    }

    public boolean isPackageSelected() {
        return view.isPackageSelected();
    }

    private CrawledLinkSandbox[] selected;

    public CrawledLinkSandbox[] getSelectedChildren() {
        CrawledLinkSandbox[] selected = this.selected;
        if (selected == null) {
            this.selected = selected = CrawledLinkSandbox.wrapSandBox(view.getSelectedChildren());
        }
        return selected;
    }

    public boolean isExpanded() {
        return view.isExpanded();
    }
}
