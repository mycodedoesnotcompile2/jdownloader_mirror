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
package org.appwork.app.gui.copycutpaste;

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.AWTEventListener;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;

/**
 * @author thomas
 *
 */
public class CopyPasteSupport implements AWTEventListener {
    private long startupTime = System.currentTimeMillis();

    public long getStartupTime() {
        return startupTime;
    }

    private long lastMouseEvent = -1;

    public long getLastMouseEvent() {
        return lastMouseEvent;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.event.EDTBasicListener#onAWTEventAfterDispatch(java .awt.AWTEvent)
     */
    @Override
    public void eventDispatched(final AWTEvent event) {
        if (!(event instanceof MouseEvent)) {
            return;
        }
        lastMouseEvent = System.currentTimeMillis();
        final MouseEvent mouseEvent = (MouseEvent) event;

        if (!mouseEvent.isPopupTrigger() && mouseEvent.getButton() != MouseEvent.BUTTON3) {
            return;
        }
        if (mouseEvent.getComponent() == null) {
            return;
        }
        // get deepest component
        // Component c = null;
        final Point point = mouseEvent.getPoint();
        final Component c;
        if (mouseEvent.getSource() instanceof JDialog) {
            c = SwingUtilities.getDeepestComponentAt((JDialog) mouseEvent.getSource(), (int) point.getX(), (int) point.getY());
        } else if (mouseEvent.getSource() instanceof JFrame) {
            final Component source = ((JFrame) mouseEvent.getSource()).getContentPane();
            point.x -= (source.getLocationOnScreen().x - ((JFrame) mouseEvent.getSource()).getLocationOnScreen().x);
            point.y -= (source.getLocationOnScreen().y - ((JFrame) mouseEvent.getSource()).getLocationOnScreen().y);
            c = SwingUtilities.getDeepestComponentAt(source, (int) point.getX(), (int) point.getY());
        } else if (mouseEvent.getSource() instanceof Component) {
            c = (Component) mouseEvent.getSource();
        } else {
            return;
        }
        // Check if deepest component is a textcomponent
        if (!(c instanceof JTextComponent) && !(c instanceof ContextMenuAdapter)) {
            return;
        }
        if (MenuSelectionManager.defaultManager().getSelectedPath().length > 0) {
            return;
        }
        final JTextComponent t = (JTextComponent) c;
        t.requestFocus();
        final JPopupMenu menu;
        if (c instanceof ContextMenuAdapter) {
            menu = ((ContextMenuAdapter) c).getPopupMenu(mouseEvent, createCutAction(t), createCopyAction(t), createPasteAction(t), createDeleteAction(t), createSelectAction(t));
            if (menu == null) {
                return;
            }
        } else {
            // create menu
            menu = new JPopupMenu();
            menu.add(createCutAction(t));
            menu.add(createCopyAction(t));
            menu.add(createPasteAction(t));
            menu.add(createDeleteAction(t));
            menu.add(createSelectAction(t));
        }
        final Point pt = SwingUtilities.convertPoint(mouseEvent.getComponent(), mouseEvent.getPoint(), c);
        menu.show(c, pt.x, pt.y);
    }

    protected AbstractAction createSelectAction(final JTextComponent t) {
        return new SelectAction(t);
    }

    protected AbstractAction createDeleteAction(final JTextComponent t) {
        return new DeleteAction(t);
    }

    protected AbstractAction createPasteAction(final JTextComponent t) {
        return new PasteAction(t);
    }

    protected AbstractAction createCopyAction(final JTextComponent t) {
        return new CopyAction(t);
    }

    protected AbstractAction createCutAction(final JTextComponent t) {
        return new CutAction(t);
    }

    /**
     *
     */
    public static void ensure() {
        for (AWTEventListener l : Toolkit.getDefaultToolkit().getAWTEventListeners()) {
            if (l instanceof CopyPasteSupport) {
                return;
            }
        }
        Toolkit.getDefaultToolkit().addAWTEventListener(new CopyPasteSupport(), AWTEvent.MOUSE_EVENT_MASK);

    }

}
