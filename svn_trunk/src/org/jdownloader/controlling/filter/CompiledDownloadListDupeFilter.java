package org.jdownloader.controlling.filter;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.DownloadListDupeFilter;

public class CompiledDownloadListDupeFilter extends DownloadListDupeFilter implements Storable {
    @StorableAllowPrivateAccessModifier
    private CompiledDownloadListDupeFilter() {
    }

    public CompiledDownloadListDupeFilter(DownloadListDupeFilter filter) {
        super(filter.getMatchType(), filter.isEnabled());
    }

    public boolean matches(CrawledLink link) {
        if (link == null) {
            return false;
        }
        final boolean isDupe = DownloadController.getInstance().hasDownloadLinkByID(link.getLinkID());
        switch (getMatchType()) {
        case IS_TRUE:
            return isDupe;
        case IS_FALSE:
            return !isDupe;
        }
        return false;
    }
}
