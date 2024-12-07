package org.jdownloader.extensions.extraction.contextmenu;

import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import javax.swing.JComponent;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator.ArchiveValidation;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberView;

public class ArchivesSubMenu extends MenuContainer {
    public ArchivesSubMenu() {
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_main());
        setIconKey(org.jdownloader.gui.IconKey.ICON_EXTRACT);
    }

    private volatile WeakReference<SelectionInfoCallback<?, ?>> lastCallBack = new WeakReference<SelectionInfoCallback<?, ?>>(null);

    private void validate(final JComponent ret) {
        final View view = MainTabbedPane.getInstance().getSelectedView();
        if (view instanceof DownloadsView) {
            DownloadsTable.getInstance().getSelectionInfo(new SelectionInfoCallback<FilePackage, DownloadLink>() {
                {
                    lastCallBack = new WeakReference<SelectionInfoCallback<?, ?>>(this);
                }

                @Override
                public void onSelectionInfo(SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                    validate(ret, selectionInfo);
                }

                @Override
                public boolean isCancelled() {
                    final WeakReference<SelectionInfoCallback<?, ?>> lastCallBack = ArchivesSubMenu.this.lastCallBack;
                    return lastCallBack.get() != this;
                }
            }, SelectionType.SELECTED);
        } else if (view instanceof LinkGrabberView) {
            LinkGrabberTable.getInstance().getSelectionInfo(new SelectionInfoCallback<CrawledPackage, CrawledLink>() {
                {
                    lastCallBack = new WeakReference<SelectionInfoCallback<?, ?>>(this);
                }

                @Override
                public void onSelectionInfo(SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
                    validate(ret, selectionInfo);
                }

                @Override
                public boolean isCancelled() {
                    final WeakReference<SelectionInfoCallback<?, ?>> lastCallBack = ArchivesSubMenu.this.lastCallBack;
                    return lastCallBack.get() != this;
                }
            }, SelectionType.SELECTED);
        }
    }

    private void validate(final JComponent ret, final SelectionInfo<?, ?> selectionInfo) {
        final ArchiveValidation result = ArchiveValidator.validate(selectionInfo, true);
        result.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                final List<Archive> archives = result.getArchives();
                if (archives != null && archives.size() > 0) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            ret.setEnabled(true);
                        }
                    };
                }
            }
        });
    }

    @Override
    public JComponent addTo(JComponent root, MenuBuilder menuBuilder) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, SecurityException, ExtensionNotLoadedException {
        final JComponent ret = super.addTo(root, menuBuilder);
        ret.setEnabled(false);
        validate(ret);
        return ret;
    }
}
