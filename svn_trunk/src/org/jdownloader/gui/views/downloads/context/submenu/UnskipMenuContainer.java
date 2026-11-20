package org.jdownloader.gui.views.downloads.context.submenu;

import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

public class UnskipMenuContainer extends MenuContainer {
    public UnskipMenuContainer() {
        setName(_GUI.T.UnskipMenuContainer());
        setIconKey(IconKey.ICON_SKIPPED);
    }
}
