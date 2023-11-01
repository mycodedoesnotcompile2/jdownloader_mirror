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
package org.appwork.utils.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.border.AbstractBorder;

public class ShadowBorder extends AbstractBorder {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private int               shadowWidth      = 3;

    private Color             color            = Color.GRAY;

    public ShadowBorder() {
        this(3);
    }

    public ShadowBorder(final int width) {
        this(width, Color.GRAY);
    }

    public ShadowBorder(final int width, final Color color) {
        this.shadowWidth = width;
        this.color = color;

    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new Insets(0, 0, this.shadowWidth + 1, this.shadowWidth + 1);
    }

    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        insets.top = 1;
        insets.left = 1;
        insets.bottom = this.shadowWidth + 1;
        insets.right = this.shadowWidth + 1;
        return insets;
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width, final int height) {
        final Color oldColor = g.getColor();
        int x1, y1, x2, y2;
        g.setColor(this.color);

        g.drawRect(x, y, width - this.shadowWidth - 1, height - this.shadowWidth - 1);
        final int alphaSteps = this.color.getAlpha() / (this.shadowWidth + 1);

        for (int i = 0; i <= this.shadowWidth; i++) {
            // bottom shadow
            g.setColor(new Color(g.getColor().getRed(), g.getColor().getGreen(), g.getColor().getBlue(), g.getColor().getAlpha() - alphaSteps));
            x1 = x + i + this.shadowWidth;
            y1 = y + height - this.shadowWidth + i;
            x2 = x + width + i - this.shadowWidth;
            y2 = y1;
            g.drawLine(x1, y1, x2, y2);

            // right shadow
            x1 = x + width - this.shadowWidth + i;
            y1 = y + i + this.shadowWidth;
            x2 = x1;
            y2 = y + height + i - this.shadowWidth - 1;
            g.drawLine(x1, y1, x2, y2);

        }

        g.setColor(oldColor);
    }
}
