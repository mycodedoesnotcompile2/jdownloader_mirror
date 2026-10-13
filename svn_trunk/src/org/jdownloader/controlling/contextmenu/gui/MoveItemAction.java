package org.jdownloader.controlling.contextmenu.gui;

import java.awt.event.ActionEvent;

import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

/**
 * Moves the selected entry of the menu tree to the very top or to the very bottom of the whole menu. Disabled if the entry is already
 * there.
 */
public class MoveItemAction extends AppAction {
    private final MenuManagerDialog managerFrame;
    private final boolean           top;

    public MoveItemAction(final MenuManagerDialog managerFrame, final boolean top) {
        this.managerFrame = managerFrame;
        this.top = top;
        if (top) {
            setName(_GUI.T.MoveToTopAction_MoveToTopAction());
            setIconKey(IconKey.ICON_GO_TOP);
        } else {
            setName(_GUI.T.MoveToBottomAction_MoveToBottomAction());
            setIconKey(IconKey.ICON_GO_BOTTOM);
        }
        setTooltipText(getName());
        update();
    }

    /**
     * Enables or disables the action depending on the current selection.
     */
    public void update() {
        setEnabled(top ? managerFrame.canMoveToTop() : managerFrame.canMoveToBottom());
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        managerFrame.moveSelectionToEdge(top);
    }
}
