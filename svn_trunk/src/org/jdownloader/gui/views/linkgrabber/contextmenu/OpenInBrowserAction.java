package org.jdownloader.gui.views.linkgrabber.contextmenu;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;

/**
 * LinkGrabber variant of the "Open in Browser" action.
 *
 * All functionality lives in the DownloadTable variant
 * ({@link org.jdownloader.gui.views.downloads.action.OpenInBrowserAction}); this class only binds it to the CrawledPackage / CrawledLink
 * types.
 */
public class OpenInBrowserAction extends org.jdownloader.gui.views.downloads.action.OpenInBrowserAction<CrawledPackage, CrawledLink> {
    private static final long serialVersionUID = 7911375550836173693L;

    public OpenInBrowserAction() {
        super();
    }
}
