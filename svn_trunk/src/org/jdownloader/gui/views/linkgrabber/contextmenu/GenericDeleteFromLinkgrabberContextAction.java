package org.jdownloader.gui.views.linkgrabber.contextmenu;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;

import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.controlling.contextmenu.TableContext;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.bottombar.GenericDeleteFromLinkgrabberAction;
import org.jdownloader.gui.views.linkgrabber.bottombar.IncludedSelectionSetup;

public class GenericDeleteFromLinkgrabberContextAction extends GenericDeleteFromLinkgrabberAction {
    private final TableContext tableContext;

    public GenericDeleteFromLinkgrabberContextAction() {
        super();
        addContextSetup(0, tableContext = new TableContext(false, true));
    }

    @Override
    protected void initIncludeSelectionSupport() {
        addContextSetup(1, includedSelection = new IncludedSelectionSetup(LinkGrabberTable.getInstance(), this, this) {
            @Override
            public SelectionType getSelectionType() {
                final SelectionType ret = super.getSelectionType();
                if (SelectionType.ALL.equals(ret) && !isIgnoreFiltered()) {
                    return SelectionType.BACKEND;
                }
                return ret;
            }

            @Override
            public void updateListeners() {
            }
        });
    }

    @Override
    protected void onUpdate(final SelectionType selectionType, final SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
        super.onUpdate(selectionType, selectionInfo);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final boolean hasSelection = selectionInfo != null && !selectionInfo.isEmpty();
                if (hasSelection) {
                    if (tableContext.isItemVisibleForSelections()) {
                        setVisible(true);
                    } else {
                        setVisible(false);
                        setEnabled(false);
                    }
                } else {
                    if (tableContext.isItemVisibleForEmptySelection()) {
                        setVisible(true);
                    } else {
                        setVisible(false);
                        setEnabled(false);
                    }
                }
            }
        };
    }

    @Override
    public void initContextDefaults() {
        includedSelection.setIncludeSelectedLinks(true);
        includedSelection.setIncludeUnselectedLinks(false);
    }
}
