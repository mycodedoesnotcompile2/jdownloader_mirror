package org.jdownloader.gui.views.linkgrabber.contextmenu;

import org.jdownloader.gui.translate._GUI;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;

public class SplitPackagesByHostLinkgrabber extends org.jdownloader.gui.views.linkgrabber.contextmenu.AbstractSplitPackagesByHostAction<CrawledPackage, CrawledLink> {
    private static final long serialVersionUID = -4468197802870765463L;

    public SplitPackagesByHostLinkgrabber() {
        super();
        setName(_GUI.T.SplitPackagesByHost_SplitPackagesByHost_object_() + "__TEST__");
    }
    // @Override
    // protected CrawledPackage createNewPackage(final String name, String downloadFolder) {
    // final CrawledPackage newPackage = new CrawledPackage();
    // newPackage.setName(name);
    // newPackage.setDownloadFolder(downloadFolder);
    // return newPackage;
    // }
}
