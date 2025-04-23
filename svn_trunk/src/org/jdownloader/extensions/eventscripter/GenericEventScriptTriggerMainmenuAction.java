package org.jdownloader.extensions.eventscripter;

import java.awt.event.ActionEvent;

import javax.swing.SwingUtilities;

import org.appwork.utils.event.queue.Queue;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.extensions.ExtensionController;
import org.jdownloader.extensions.LazyExtension;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.jdtrayicon.MenuManagerTrayIcon;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.EDTSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.QueueSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.MenuManagerDownloadTabBottomBar;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberView;
import org.jdownloader.gui.views.linkgrabber.bottombar.MenuManagerLinkgrabberTabBottombar;

import jd.controlling.packagecontroller.PackageControllerQueue.ReadOnlyQueueAction;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;

public class GenericEventScriptTriggerMainmenuAction extends CustomizableAppAction {
    public GenericEventScriptTriggerMainmenuAction() {
        setName("EventScripter Trigger");
        setIconKey(IconKey.ICON_EVENT);
    }

    protected static void getViewSelection(final SelectionInfoCallback callback, final SelectionType selectionType) {
        final View view = MainTabbedPane.getInstance().getSelectedView();
        if (view instanceof DownloadsView) {
            DownloadsTable.getInstance().getSelectionInfo(callback, selectionType);
        } else if (view instanceof LinkGrabberView) {
            LinkGrabberTable.getInstance().getSelectionInfo(callback, selectionType);
        } else {
            if (callback instanceof QueueSelectionInfoCallback) {
                final QueueSelectionInfoCallback qcallback = (QueueSelectionInfoCallback) callback;
                final Queue queue = qcallback.getQueue();
                if (queue != null && Thread.currentThread() != queue.getQueueThread()) {
                    queue.add(new ReadOnlyQueueAction<Void, RuntimeException>(Queue.QueuePriority.HIGH) {
                        @Override
                        protected Void run() throws RuntimeException {
                            callback.onSelectionInfo(null);
                            return null;
                        }
                    });
                    return;
                }
            }
            if (callback instanceof EDTSelectionInfoCallback && !SwingUtilities.isEventDispatchThread()) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        callback.onSelectionInfo(null);
                    }
                });
                return;
            }
            callback.onSelectionInfo(null);
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final LazyExtension extension = ExtensionController.getInstance().getExtension(EventScripterExtension.class);
        if (extension != null && extension._isEnabled()) {
            getViewSelection(new EDTSelectionInfoCallback() {
                @Override
                public void onSelectionInfo(SelectionInfo selectionInfo) {
                    if (getMenuItemData()._getRoot() == MenuManagerMainmenu.getInstance().getMenuData()) {
                        ((EventScripterExtension) extension._getExtension()).triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.MAIN_MENU_BUTTON, selectionInfo);
                    } else if (getMenuItemData()._getRoot() == MenuManagerLinkgrabberTabBottombar.getInstance().getMenuData()) {
                        ((EventScripterExtension) extension._getExtension()).triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.LINKGRABBER_BOTTOM_BAR_BUTTON, selectionInfo);
                    } else if (getMenuItemData()._getRoot() == MenuManagerDownloadTabBottomBar.getInstance().getMenuData()) {
                        ((EventScripterExtension) extension._getExtension()).triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.DOWNLOAD_TABLE_BOTTOM_BAR_BUTTON, selectionInfo);
                    } else if (getMenuItemData()._getRoot() == MenuManagerTrayIcon.getInstance().getMenuData()) {
                        ((EventScripterExtension) extension._getExtension()).triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.TRAY_BUTTON, selectionInfo);
                    }
                }

                @Override
                public boolean isCancelled() {
                    return false;
                }
            }, SelectionType.SELECTED);
        }
    }
}
