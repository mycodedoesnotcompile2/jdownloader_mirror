/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.images;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;

import javax.swing.Icon;

/**
 * @author thomas
 * @date Feb 5, 2025
 *
 */
public class BorderedIcon implements Icon, IconPipe {
    private Icon  delegate;
    private int   borderWidth;
    private Color borderColor;

    /**
     * @param checkBox
     * @param red
     * @param i
     */
    public BorderedIcon(Icon delegate, Color color, int borderWidth) {
        this.delegate = delegate;
        this.borderWidth = borderWidth;
        this.borderColor = color;
    }

    /**
     * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        AffineTransform originalTransform = g2d.getTransform();
        double scaleX = originalTransform.getScaleX();
        double scaleY = originalTransform.getScaleY();
        int scaledBorderVertical = IconIO.clipScale(borderWidth, scaleX);
        int scaledBorderHorizontal = IconIO.clipScale(borderWidth, scaleY);
        int scaledWidthIconDelegate = IconIO.clipScale(delegate.getIconWidth(), scaleX);
        int scaledheightIconDelegate = IconIO.clipScale(delegate.getIconHeight(), scaleY);
        if (borderColor != null) {
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
            g2d.setColor(borderColor);
            g2d.scale(1.0 / scaleX, 1.0 / scaleY);
            // int totalWidth = getIconWidth();
            // int totalHeight = getIconHeight();
            int xDev = IconIO.clipScale(x, scaleX);
            int yDev = IconIO.clipScale(y, scaleY);
            int widthDev = scaledWidthIconDelegate + scaledBorderVertical + scaledBorderHorizontal;
            int heightDev = scaledheightIconDelegate + scaledBorderVertical + scaledBorderHorizontal;
            g2d.setColor(borderColor);
            // TODO:paint border vert and horizontal individual
            for (int i = 0; i < scaledBorderVertical; i++) {
                g2d.drawRect(xDev + i, yDev + i, widthDev - 1 - 2 * i, heightDev - 1 - 2 * i);
            }
            g2d.setTransform(originalTransform);
        }
        g2d.setColor(Color.green);
        delegate.paintIcon(c, g, x + borderWidth, y + borderWidth);
    }

    /**
     * @see javax.swing.Icon#getIconWidth()
     */
    @Override
    public int getIconWidth() {
        int dw = delegate.getIconWidth();
        return delegate.getIconWidth() + borderWidth * 2;
    }

    /**
     * @see javax.swing.Icon#getIconHeight()
     */
    @Override
    public int getIconHeight() {
        return delegate.getIconHeight() + borderWidth * 2;
    }

    /**
     * @see org.appwork.utils.images.IconPipe#getDelegate()
     */
    @Override
    public Icon getDelegate() {
        return delegate;
    }
}
