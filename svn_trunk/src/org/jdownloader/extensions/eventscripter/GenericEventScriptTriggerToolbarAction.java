package org.jdownloader.extensions.eventscripter;

import java.awt.event.ActionEvent;

import org.jdownloader.controlling.contextmenu.CustomizableSelectionAppAction;
import org.jdownloader.extensions.ExtensionController;
import org.jdownloader.extensions.LazyExtension;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

public class GenericEventScriptTriggerToolbarAction extends AbstractToolBarAction {

    public GenericEventScriptTriggerToolbarAction() {
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
                    ((EventScripterExtension) extension._getExtension()).triggerAction(getName(), getIconKey(), getShortCutString(), EventTrigger.TOOLBAR_BUTTON, selectionInfo);
                }

                @Override
                public boolean isCancelled() {
                    return false;
                }
            }, SelectionType.SELECTED);
        }
    }

    @Override
    protected String createTooltip() {
        return null;
    }

}
