package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.ArrayList;
import java.util.List;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;

import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;

public class LinkgrabberSelectionSandbox {
    private final SelectionInfo<CrawledPackage, CrawledLink> selectionInfo;

    public LinkgrabberSelectionSandbox(SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
        this.selectionInfo = selectionInfo;
    }

    public LinkgrabberSelectionSandbox() {
        this(null);
    }

    @Override
    public int hashCode() {
        if (selectionInfo != null) {
            return selectionInfo.hashCode();
        } else {
            return super.hashCode();
        }
    }

    public boolean isPackageSelected(final CrawledPackageSandbox crawledPackageSandbox) {
        if (selectionInfo == null || crawledPackageSandbox == null || crawledPackageSandbox.filePackage == null) {
            return false;
        }
        for (final PackageView<CrawledPackage, CrawledLink> packageView : selectionInfo.getPackageViews()) {
            if (packageView.getPackage() == crawledPackageSandbox.filePackage) {
                return packageView.isPackageSelected();
            }
        }
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LinkgrabberSelectionSandbox) {
            return ((LinkgrabberSelectionSandbox) obj).selectionInfo == selectionInfo;
        } else {
            return super.equals(obj);
        }
    }

    public CrawledLinkSandbox[] getLinks() {
        if (selectionInfo == null) {
            return null;
        }
        final List<CrawledLink> childs = selectionInfo.getChildren();
        final CrawledLinkSandbox[] ret = new CrawledLinkSandbox[childs.size()];
        for (int i = 0; i < ret.length; i++) {
            ret[i] = new CrawledLinkSandbox(childs.get(i));
        }
        return ret;
    }

    public CrawledLinkSandbox[] getSelectedLinks(final CrawledPackageSandbox filePackageSandbox) {
        if (selectionInfo == null || filePackageSandbox == null || filePackageSandbox.filePackage == null) {
            return null;
        }
        for (PackageView<CrawledPackage, CrawledLink> packageView : selectionInfo.getPackageViews()) {
            if (packageView.getPackage() == filePackageSandbox.filePackage) {
                final List<CrawledLinkSandbox> ret = new ArrayList<CrawledLinkSandbox>();
                final List<CrawledLink> children = packageView.getSelectedChildren();
                if (children != null) {
                    for (CrawledLink child : children) {
                        ret.add(new CrawledLinkSandbox(child));
                    }
                }
                return ret.toArray(new CrawledLinkSandbox[0]);
            }
        }
        return null;
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

    public CrawledPackageSandbox[] getPackages() {
        return getPackages(false);
    }

    public CrawledPackageSandbox[] getPackages(final boolean includeSelectedOnly) {
        if (selectionInfo == null) {
            return null;
        }
        final List<PackageView<CrawledPackage, CrawledLink>> packageViews = selectionInfo.getPackageViews();
        final List<CrawledPackageSandbox> ret = new ArrayList<CrawledPackageSandbox>(packageViews.size());
        for (PackageView<CrawledPackage, CrawledLink> packageView : packageViews) {
            if (!includeSelectedOnly || packageView.isPackageSelected()) {
                ret.add(new CrawledPackageSandbox(packageView.getPackage()));
            }
        }
        return ret.toArray(new CrawledPackageSandbox[0]);
    }

    public CrawledPackageSandbox getContextPackage() {
        if (selectionInfo == null) {
            return new CrawledPackageSandbox();
        } else if (isPackageContext()) {
            final CrawledPackage cl = selectionInfo.getContextPackage();
            return cl == null ? null : new CrawledPackageSandbox(cl);
        } else {
            return null;
        }
    }

    public CrawledLinkSandbox getContextLink() {
        if (selectionInfo == null) {
            return new CrawledLinkSandbox();
        } else if (isLinkContext()) {
            final CrawledLink cl = selectionInfo.getContextLink();
            return cl == null ? null : new CrawledLinkSandbox(cl);
        } else {
            return null;
        }
    }
}
