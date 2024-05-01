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
package org.appwork.swing.components.tooltips;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseListener;

import javax.swing.JComponent;
import javax.swing.JToolTip;
import javax.swing.UIManager;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import org.appwork.swing.MigPanel;

import net.miginfocom.swing.MigLayout;

/**
 * @author thomas
 *
 */
public abstract class ExtTooltip extends JToolTip implements AncestorListener {
    /**
     *
     */
    public static final String APPWORK_TOOLTIP_FOREGROUND = "Appwork.Tooltip.Foreground";
    private static final long  serialVersionUID           = -2212735987320956801L;
    protected MigPanel         panel;
    private int                w                          = 0;
    private int                h                          = 0;
    private long               lastResize;
    private long               lastResizeH;

    /**
     * @param title
     */
    public ExtTooltip() {
        super();
        setLayout(new MigLayout("ins 0", "[grow,fill]", "[grow,fill]"));
        panel = createContent();
        // this.add(con);
        setTipText("");
        // this.setUI(null);
        // panel.setOpaque(true);
        // panel.setBackground(Color.BLUE);
        if (panel != null) {
            this.add(panel);
        }
        addAncestorListener(this);
    }

    @Deprecated
    public static void setForgroundColor(final Color black) {
        UIManager.put(ExtTooltip.APPWORK_TOOLTIP_FOREGROUND, black);
    }

    @Override
    public synchronized void addMouseListener(final MouseListener l) {
        /*
         * Make sure that we have only one listener
         */
        super.removeMouseListener(l);
        super.addMouseListener(l);
    }

    @Override
    public void ancestorAdded(final AncestorEvent event) {
        h = 0;
        w = 0;
        lastResize = 0;
        lastResizeH = 0;
    }

    @Override
    public void ancestorMoved(final AncestorEvent event) {
    }

    @Override
    public void ancestorRemoved(final AncestorEvent event) {
    }

    /**
     * @return
     */
    public abstract TooltipPanel createContent();

    @Override
    public Rectangle getBounds() {
        return super.getBounds();
    }

    @Override
    public Rectangle getBounds(final Rectangle rv) {
        return super.getBounds(rv);
    }

    @Override
    public int getHeight() {
        if (panel == null) {
            return 0;
        }
        final Insets insets = this.getInsets();
        final int th = panel.getPreferredSize().height + insets.top + insets.bottom;
        if (th > h) {
            h = th;
            final Container parent = getParent();
            if (parent != null) {
                final Rectangle b = parent.getBounds();
                parent.setBounds(b.x, b.y, w, h);
            }
        } else if (th < h) {
            if (System.currentTimeMillis() - lastResizeH > 1000) {
                h -= (h - th) * (System.currentTimeMillis() - lastResizeH - 1000) / 10000;
                final Container parent = getParent();
                if (parent != null) {
                    final Rectangle b = parent.getBounds();
                    parent.setBounds(b.x, b.y, w, h);
                }
            }
        }
        return h;
    }

    @Override
    public Dimension getPreferredSize() {
        if (panel == null) {
            return new Dimension(0, 0);
        }
        final Dimension dim = panel.getPreferredSize();
        final Insets insets = this.getInsets();
        dim.width += insets.left + insets.right;
        dim.height += insets.top + insets.bottom;
        return dim;
    }

    @Override
    public int getWidth() {
        if (panel == null) {
            return 0;
        }
        final Insets insets = this.getInsets();
        final int tw = panel.getPreferredSize().width + insets.left + insets.right;
        if (tw > w) {
            w = tw;
            final Container parent = getParent();
            if (parent != null) {
                final Rectangle b = parent.getBounds();
                parent.setBounds(b.x, b.y, w, h);
            }
            lastResize = System.currentTimeMillis();
        } else if (tw < w) {
            if (System.currentTimeMillis() - lastResize > 1000) {
                w -= (w - tw) * (System.currentTimeMillis() - lastResize - 1000) / 10000;
                final Container parent = getParent();
                if (parent != null) {
                    final Rectangle b = parent.getBounds();
                    parent.setBounds(b.x, b.y, w, h);
                }
            }
        }
        return w;
    }

    /**
     * normal behaviour is, that a new tooltip will be shown immediately if we move the mouse to a new tooltip component within a short
     * time. if this method is false, this behaviour will not be active after this tooltip.
     *
     * @return
     */
    public boolean isLastHiddenEnabled() {
        return true;
    }

    /**
     *
     */
    public void onHide() {
    }

    /**
     * @param ttPosition
     *
     */
    public void onShow() {
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComponent#setMaximumSize(java.awt.Dimension)
     */
    @Override
    public void setMaximumSize(final Dimension maximumSize) {
        panel.setMaximumSize(maximumSize);
    }

    @Override
    public void paint(final Graphics g) {
        // this.getLayout().layoutContainer(this);
        if (panel != null) {
            panel.setSize(panel.getPreferredSize());
            final Insets insets = this.getInsets();
            panel.setLocation(insets.left, insets.top);
        }
        super.paint(g);
    }

    @Override
    protected void paintChildren(final Graphics g) {
        super.paintChildren(g);
    }

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        // final Insets insets = this.getInsets();
        // g.translate(insets.left, insets.top);
        // this.panel.setSize(this.panel.getPreferredSize());
        // this.panel.repaint();
        // if (this.test++ < 5) {
        // this.panel.paintComponents(g);
        // }
    }

    @Override
    public void paintComponents(final Graphics g) {
        super.paintComponents(g);
    }

    /**
     * @return
     */
    abstract public String toText();

    /**
     * @param activeComponent
     * @param ttPosition
     * @return
     */
    public Point getDesiredLocation(final JComponent activeComponent, final Point ttPosition) {
        return ttPosition;
    }
}
