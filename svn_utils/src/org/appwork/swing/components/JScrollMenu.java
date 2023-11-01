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
package org.appwork.swing.components;

import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.MenuElement;
import javax.swing.UIManager;
import javax.swing.plaf.MenuItemUI;
import javax.swing.plaf.PopupMenuUI;

import org.appwork.utils.Application;

/**
 * Source: http://stackoverflow.com/questions/9288350/adding-vertical-scroll-to-a-jpopupmenu
 *
 * @author thomas
 *
 */

public class JScrollMenu extends JMenu {
    // Covers the one in the JMenu because the method that creates it in JMenu is private
    /** The popup menu portion of the menu. */
    private JPopupMenu popupMenu;

    /**
     * Constructs a new <code>JMenu</code> with no text.
     */
    public JScrollMenu() {
        this("");
    }

    /**
     * Constructs a new <code>JMenu</code> with the supplied string as its text.
     *
     * @param s
     *            the text for the menu label
     */
    public JScrollMenu(String s) {
        super(s);
    }

    /**
     * Constructs a menu whose properties are taken from the <code>Action</code> supplied.
     *
     * @param a
     *            an <code>Action</code>
     */
    public JScrollMenu(Action a) {
        this();
        setAction(a);
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComponent#getPreferredSize()
     */

    @Override
    public Dimension getPreferredSize() {
        Dimension ret = super.getPreferredSize();
        if (Application.isSyntheticaLookAndFeel()) {
            if (getMenuComponentCount() > 0) {
                // seems like somehow synthetica does not respect the arrow for this class
                ret.width += 10;
            }
        }
        return ret;
    }

    /**
     * Lazily creates the popup menu. This method will create the popup using the <code>JScrollPopupMenu</code> class.
     */
    protected void ensurePopupMenuCreated() {
        if (popupMenu == null) {
            this.popupMenu = new JScrollPopupMenu();
            popupMenu.setInvoker(this);
            popupListener = createWinListener(popupMenu);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.AbstractButton#getText()
     */
    @Override
    public String getText() {
        // TODO Auto-generated method stub
        // if (StringUtils.equals(super.getText(), "Video")) {
        // new Exception().printStackTrace();
        // }
        return super.getText();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
     */
    @Override
    protected void paintComponent(Graphics g) {
        // TODO Auto-generated method stub
        super.paintComponent(g);
    }

    //////////////////////////////
    //// All of these methods are necessary because ensurePopupMenuCreated() is private in JMenu
    //////////////////////////////
    @Override
    public void updateUI() {
        MenuItemUI u = (MenuItemUI) UIManager.getUI(this);
        setUI(u);

        if (popupMenu != null) {
            popupMenu.setUI((PopupMenuUI) UIManager.getUI(popupMenu));
        }
    }

    @Override
    public boolean isPopupMenuVisible() {
        ensurePopupMenuCreated();
        return popupMenu.isVisible();
    }

    @Override
    public void setMenuLocation(int x, int y) {
        super.setMenuLocation(x, y);
        if (popupMenu != null) {
            popupMenu.setLocation(x, y);
        }
    }

    @Override
    public JMenuItem add(JMenuItem menuItem) {
        ensurePopupMenuCreated();
        return popupMenu.add(menuItem);
    }

    @Override
    public Component add(Component c) {
        ensurePopupMenuCreated();
        popupMenu.add(c);
        return c;
    }

    @Override
    public Component add(Component c, int index) {
        ensurePopupMenuCreated();
        popupMenu.add(c, index);
        return c;
    }

    @Override
    public void addSeparator() {
        ensurePopupMenuCreated();
        popupMenu.addSeparator();
    }

    @Override
    public void insert(String s, int pos) {
        if (pos < 0) {
            throw new IllegalArgumentException("index less than zero.");
        }

        ensurePopupMenuCreated();
        popupMenu.insert(new JMenuItem(s), pos);
    }

    @Override
    public JMenuItem insert(JMenuItem mi, int pos) {
        if (pos < 0) {
            throw new IllegalArgumentException("index less than zero.");
        }
        ensurePopupMenuCreated();
        popupMenu.insert(mi, pos);
        return mi;
    }

    @Override
    public JMenuItem insert(Action a, int pos) {
        if (pos < 0) {
            throw new IllegalArgumentException("index less than zero.");
        }

        ensurePopupMenuCreated();
        JMenuItem mi = new JMenuItem(a);
        mi.setHorizontalTextPosition(JButton.TRAILING);
        mi.setVerticalTextPosition(JButton.CENTER);
        popupMenu.insert(mi, pos);
        return mi;
    }

    @Override
    public void insertSeparator(int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index less than zero.");
        }

        ensurePopupMenuCreated();
        popupMenu.insert(new JPopupMenu.Separator(), index);
    }

    @Override
    public void remove(JMenuItem item) {
        if (popupMenu != null) {
            popupMenu.remove(item);
        }
    }

    @Override
    public void remove(int pos) {
        if (pos < 0) {
            throw new IllegalArgumentException("index less than zero.");
        }
        if (pos > getItemCount()) {
            throw new IllegalArgumentException("index greater than the number of items.");
        }
        if (popupMenu != null) {
            popupMenu.remove(pos);
        }
    }

    @Override
    public void remove(Component c) {
        if (popupMenu != null) {
            popupMenu.remove(c);
        }
    }

    @Override
    public void removeAll() {
        if (popupMenu != null) {
            popupMenu.removeAll();
        }
    }

    @Override
    public int getMenuComponentCount() {
        return (popupMenu == null) ? 0 : popupMenu.getComponentCount();
    }

    @Override
    public Component getMenuComponent(int n) {
        return (popupMenu == null) ? null : popupMenu.getComponent(n);
    }

    @Override
    public Component[] getMenuComponents() {
        return (popupMenu == null) ? new Component[0] : popupMenu.getComponents();
    }

    @Override
    public JPopupMenu getPopupMenu() {
        ensurePopupMenuCreated();
        return popupMenu;
    }

    @Override
    public MenuElement[] getSubElements() {
        return popupMenu == null ? new MenuElement[0] : new MenuElement[] { popupMenu };
    }

    @Override
    public void applyComponentOrientation(ComponentOrientation o) {
        super.applyComponentOrientation(o);

        if (popupMenu != null) {
            int ncomponents = getMenuComponentCount();
            for (int i = 0; i < ncomponents; ++i) {
                getMenuComponent(i).applyComponentOrientation(o);
            }
            popupMenu.setComponentOrientation(o);
        }
    }

    @Override
    public void setComponentOrientation(ComponentOrientation o) {
        super.setComponentOrientation(o);
        if (popupMenu != null) {
            popupMenu.setComponentOrientation(o);
        }
    }
}