//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.gui.swing.jdgui.menu.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

public class AddContainerAction extends CustomizableAppAction {
    private static final long serialVersionUID = 4713690050852393405L;

    public AddContainerAction() {
        setName(_GUI.T.action_addcontainer());
        setTooltipText(_GUI.T.action_addcontainer_tooltip());
        setIconKey(IconKey.ICON_LOAD);
        setAccelerator(KeyEvent.VK_O);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        new org.jdownloader.gui.views.linkgrabber.actions.AddContainerAction().actionPerformed(e);
    }
}