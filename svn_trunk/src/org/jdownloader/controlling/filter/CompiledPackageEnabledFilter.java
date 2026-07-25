package org.jdownloader.controlling.filter;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.PackageEnabledFilter;

public class CompiledPackageEnabledFilter extends PackageEnabledFilter implements Storable {
    @StorableAllowPrivateAccessModifier
    private CompiledPackageEnabledFilter() {
    }

    public CompiledPackageEnabledFilter(PackageEnabledFilter filter) {
        super(filter.getMatchType(), filter.isEnabled());
    }

    public boolean matches(CrawledLink link) {
        final CrawledPackage pkg = link.getParentNode();
        if (pkg == null) {
            return false;
        }
        switch (getMatchType()) {
        case IS_TRUE:
            return pkg.isEnabled();
        case IS_FALSE:
            return !pkg.isEnabled();
        }
        return false;
    }
}
