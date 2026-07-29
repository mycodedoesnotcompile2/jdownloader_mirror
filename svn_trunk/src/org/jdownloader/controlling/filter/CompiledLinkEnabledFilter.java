package org.jdownloader.controlling.filter;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;

import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.LinkEnabledFilter;

public class CompiledLinkEnabledFilter extends LinkEnabledFilter implements Storable {
    @StorableAllowPrivateAccessModifier
    private CompiledLinkEnabledFilter() {
    }

    public CompiledLinkEnabledFilter(LinkEnabledFilter filter) {
        super(filter.getMatchType(), filter.isEnabled());
    }

    public boolean matches(CrawledLink link) {
        if (link == null) {
            return false;
        }
        switch (getMatchType()) {
        case IS_TRUE:
            return link.isEnabled();
        case IS_FALSE:
            return !link.isEnabled();
        }
        return false;
    }
}
