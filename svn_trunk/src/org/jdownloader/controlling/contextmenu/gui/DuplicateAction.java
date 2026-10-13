package org.jdownloader.controlling.contextmenu.gui;

import java.awt.event.ActionEvent;

import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.IconKey;

/**
 * Duplicates the selected entries of the menu tree. The copy is inserted directly below the original.
 */
public class DuplicateAction extends AppAction {
    private final MenuManagerDialog managerFrame;

    public DuplicateAction(final MenuManagerDialog managerFrame) {
        this.managerFrame = managerFrame;
        setTooltipText("Duplicate Action");
        setName("Duplicate Action");
        setIconKey(IconKey.ICON_COPY);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        managerFrame.duplicateSelection();
    }
}
