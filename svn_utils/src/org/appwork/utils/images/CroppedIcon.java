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

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;

import org.appwork.exceptions.NotSupportedException;
import org.appwork.utils.DebugMode;

/**
 * @author thomas
 * @date Jan 30, 2025
 *
 */
public class CroppedIcon implements Icon {
    private Icon            icon;
    private int             width;
    private int             height;
    private AlignHorizontal alignHorizontal;
    private AlignVertical   alignVertical;

    /**
     * @param iconEnd
     */
    public CroppedIcon(Icon icon) {
        this(icon, Math.max(icon.getIconHeight(), icon.getIconWidth()));
    }

    /**
     * @param icon
     * @param max
     * @param object
     */
    public CroppedIcon(Icon icon, int widthAndHeight) {
        this(icon, widthAndHeight, widthAndHeight, null, null);
    }

    /**
     * @param icon
     * @param size
     * @param size2
     * @param background
     */
    public CroppedIcon(Icon icon, int width, int height, AlignHorizontal horizonal, AlignVertical vertical) {
        this.icon = icon;
        crop(width, height);
        align(horizonal, vertical);
    }

    /**
     * @param width
     * @param height
     */
    public CroppedIcon crop(int width, int height) {
        this.width = width;
        this.height = height;
        return this;
    }

    /**
     * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        int xOffset = 0;
        int yOffset = 0;
        if (alignHorizontal != null) {
            switch (alignHorizontal) {
            case CENTER:
                yOffset = (width - icon.getIconWidth()) / 2;
                break;
            case LEFT:
                xOffset = 0;
                break;
            case RIGHT:
                xOffset = width - icon.getIconWidth();
                break;
            default:
                DebugMode.debugger();
                throw new NotSupportedException();
            }
        }
        if (alignVertical != null) {
            switch (alignVertical) {
            case CENTER:
                yOffset = (height - icon.getIconHeight()) / 2;
                break;
            case TOP:
                yOffset = 0;
                break;
            case BOTTOM:
                yOffset = height - icon.getIconHeight();
                break;
            default:
                DebugMode.debugger();
                throw new NotSupportedException();
            }
        }
        icon.paintIcon(c, g, x + xOffset, y + yOffset);
    }

    /**
     * @see javax.swing.Icon#getIconWidth()
     */
    @Override
    public int getIconWidth() {
        return width;
    }

    /**
     * @see javax.swing.Icon#getIconHeight()
     */
    @Override
    public int getIconHeight() {
        return height;
    }

    /**
     * @param center
     * @param bottom
     * @return
     */
    public Icon align(AlignHorizontal horizonal, AlignVertical vertical) {
        if (horizonal == null) {
            horizonal = AlignHorizontal.CENTER;
        }
        if (vertical == null) {
            vertical = AlignVertical.CENTER;
        }
        this.alignHorizontal = horizonal;
        this.alignVertical = vertical;
        return this;
    }
}
