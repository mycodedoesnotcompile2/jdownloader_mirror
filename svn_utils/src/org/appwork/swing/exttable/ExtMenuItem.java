/**
 * 
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany   
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 * 	
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header. 	
 * 	
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the 
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 * 	
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.exttable;

import java.awt.AWTEvent;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.JMenuItem;

/**
 * This is an extended JCheckBoxMenuItem
 */
public class ExtMenuItem extends JMenuItem {

    /**
     * 
     */
    private static final long serialVersionUID = -2308936338542479539L;

 
    /**
     * Creates a new Menuitem By action
     * 
     * @param action
     */
    public ExtMenuItem(final AbstractAction action) {
        super(action);
    }

    /**
     * Creates a new MenuItem with name
     * 
     * @param name
     */
    public ExtMenuItem(final String name) {
        super(name);
    }
    /**
     * parameter that says if the underlaying popupmenu closes on click
     */
    private boolean hideOnClick = true;

    /**
     * @return the {@link ExtMenuItem#hideOnClick}
     * @see ExtMenuItem#hideOnClick
     */
    public boolean isHideOnClick() {
        return hideOnClick;
    }

    protected void processMouseEvent(final MouseEvent e) {
        if (!hideOnClick && e.getID() == MouseEvent.MOUSE_RELEASED) {
            setSelected(!isSelected());
            for (final ActionListener al : getActionListeners()) {
                
                int modifiers = 0;
                final AWTEvent currentEvent = EventQueue.getCurrentEvent();
                if (currentEvent instanceof InputEvent) {
                    modifiers = ((InputEvent) currentEvent).getModifiers();
                } else if (currentEvent instanceof ActionEvent) {
                    modifiers = ((ActionEvent) currentEvent).getModifiers();
                }
                al.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, getActionCommand(), EventQueue.getMostRecentEventTime(), modifiers));
                
              
            }
//            doClick(0);
        } else {
            super.processMouseEvent(e);
        }
    }

    /**
     * @param hideOnClick
     *            the {@link ExtMenuItem#hideOnClick} to set
     * @see ExtMenuItem#hideOnClick
     */
    public void setHideOnClick(final boolean hideOnClick) {
        this.hideOnClick = hideOnClick;
    }

}
