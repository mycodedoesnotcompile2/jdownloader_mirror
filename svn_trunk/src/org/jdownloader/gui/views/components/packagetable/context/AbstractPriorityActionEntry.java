package org.jdownloader.gui.views.components.packagetable.context;

import java.awt.event.ActionEvent;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.KeyObserver;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.event.GUIListener;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTableModel;
import org.jdownloader.translate._JDT;

public abstract class AbstractPriorityActionEntry<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements GUIListener, ActionContext {

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private final Priority    priority;
    private volatile boolean  metaCtrl         = false;

    public AbstractPriorityActionEntry(Priority priority) {
        super();
        this.priority = priority;
        GUIEventSender.getInstance().addListener(this, true);
        metaCtrl = KeyObserver.getInstance().isMetaDown(true) || KeyObserver.getInstance().isControlDown(true);
        updateStateAndLabelAndIcon();
        setSmallIcon(priority.loadIcon(18));
    }

    @Override
    public void onKeyModifier(int parameter) {
        final boolean before = metaCtrl;
        if (KeyObserver.getInstance().isControlDown(false) || KeyObserver.getInstance().isMetaDown(false)) {
            metaCtrl = true;
        } else {
            metaCtrl = false;
        }
        if (before != metaCtrl) {
            updateStateAndLabelAndIcon();
        }
    }

    private void updateStateAndLabelAndIcon() {
        if (isForceMode() && !metaCtrl || metaCtrl) {
            setName(priority.T() + " " + _GUI.T.system_download_triggerfileexists_overwrite());
        } else {
            setName(priority.T());
        }
    }

    private boolean forceMode = false;

    public static String getTranslationForForceMode() {
        return _JDT.T.PriorityAction_getTranslationForForceMode();
    }

    @Customizer(link = "#getTranslationForForceMode")
    public boolean isForceMode() {
        return forceMode;
    }

    public void setForceMode(boolean forceMode) {
        this.forceMode = forceMode;
    }

    @Override
    public void onGuiMainTabSwitch(View oldView, View newView) {
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<PackageType, ChildrenType> selectionInfo) {
        final boolean finalMetaCtrl = forceMode ? !metaCtrl : metaCtrl;
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {

            private final void setPriorityEnum(AbstractNode node, Priority priority) {
                if (node instanceof CrawledLink) {
                    ((CrawledLink) node).setPriority(priority);
                } else if (node instanceof DownloadLink) {
                    ((DownloadLink) node).setPriorityEnum(priority);
                } else if (node instanceof CrawledPackage) {
                    ((CrawledPackage) node).setPriorityEnum(priority);
                } else if (node instanceof FilePackage) {
                    ((FilePackage) node).setPriorityEnum(priority);
                }
            }

            @Override
            protected Void run() throws RuntimeException {
                final Boolean downloadList;
                if (selectionInfo.getController() instanceof DownloadController) {
                    downloadList = true;
                } else if (selectionInfo.getController() instanceof LinkCollector) {
                    downloadList = false;
                } else {
                    downloadList = null;
                }
                packageView: for (PackageView<PackageType, ChildrenType> packageView : selectionInfo.getPackageViews()) {
                    final PackageType pkg = packageView.getPackage();
                    if (packageView.isPackageSelected()) {
                        setPriorityEnum(pkg, priority);
                        if (finalMetaCtrl) {
                            for (ChildrenType child : packageView.getChildren()) {
                                setPriorityEnum(child, priority);
                            }
                            continue packageView;
                        }
                    }
                    for (ChildrenType child : packageView.getSelectedChildren()) {
                        setPriorityEnum(child, priority);
                    }
                }
                if (Boolean.FALSE.equals(downloadList)) {
                    LinkGrabberTableModel.getInstance().setPriorityColumnVisible(true);
                } else if (Boolean.TRUE.equals(downloadList)) {
                    DownloadsTableModel.getInstance().setPriorityColumnVisible(true);
                }
                return null;
            }
        });
    }

}
