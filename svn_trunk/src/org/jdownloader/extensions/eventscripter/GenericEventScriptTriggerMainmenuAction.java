package org.jdownloader.extensions.eventscripter;

import java.awt.event.ActionEvent;

import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.CustomizableSelectionAppAction;
import org.jdownloader.extensions.ExtensionController;
import org.jdownloader.extensions.LazyExtension;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.jdtrayicon.MenuManagerTrayIcon;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.MenuManagerDownloadTabBottomBar;
import org.jdownloader.gui.views.linkgrabber.bottombar.MenuManagerLinkgrabberTabBottombar;

public class GenericEventScriptTriggerMainmenuAction extends CustomizableAppAction {

    public GenericEventScriptTriggerMainmenuAction() {
        setName("EventScripter Trigger");
        setIconKey(IconKey.ICON_EVENT);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final LazyExtension extension = ExtensionController.getInstance().getExtension(EventScripterExtension.class);
        if (extension != null && extension._isEnabled()) {
            CustomizableSelectionAppAction.getViewSelection(new SelectionInfoCallback() {
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
