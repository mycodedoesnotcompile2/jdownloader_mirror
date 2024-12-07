package org.jdownloader.extensions.eventscripter;

import java.awt.event.ActionEvent;

import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.extensions.ExtensionController;
import org.jdownloader.extensions.LazyExtension;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.contextmenumanager.MenuManagerDownloadTableContext;
import org.jdownloader.gui.views.linkgrabber.contextmenu.MenuManagerLinkgrabberTableContext;

public class GenericEventScriptTriggerContextMenuAction extends CustomizableTableContextAppAction {
    public GenericEventScriptTriggerContextMenuAction() {
        setName("EventScripter Trigger");
        setIconKey(IconKey.ICON_EVENT);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, SelectionType selectionType, SelectionInfo selectionInfo) {
        final LazyExtension extension = ExtensionController.getInstance().getExtension(EventScripterExtension.class);
        if (extension != null && extension._isEnabled()) {
            final EventScripterExtension ext = ((EventScripterExtension) extension._getExtension());
            if (getMenuItemData()._getRoot() == MenuManagerDownloadTableContext.getInstance().getMenuData()) {
                ext.triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.DOWNLOAD_TABLE_CONTEXT_MENU_BUTTON, selectionInfo);
            } else if (getMenuItemData()._getRoot() == MenuManagerLinkgrabberTableContext.getInstance().getMenuData()) {
                ext.triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.LINKGRABBER_TABLE_CONTEXT_MENU_BUTTON, selectionInfo);
            }
        }
    }

}
