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

import java.awt.Color;
import java.awt.Graphics2D;

public abstract class ExtOverlayRowHighlighter {

    private Color borderColor;

    private Color contentColor;

    public ExtOverlayRowHighlighter(final Color borderColor, final Color contentColor) {
        this.borderColor = borderColor;
        this.contentColor = contentColor;
    }

    abstract public boolean doHighlight(ExtTable<?> extTable, int row);

    /**
     * @return the {@link ExtOverlayRowHighlighter#borderColor}
     * @see ExtOverlayRowHighlighter#borderColor
     */
    public Color getBorderColor() {
        return this.borderColor;
    }

    /**
     * @return the {@link ExtOverlayRowHighlighter#contentColor}
     * @see ExtOverlayRowHighlighter#contentColor
     */
    public Color getContentColor() {
        return this.contentColor;
    }

    /**
     * Overwrite this method for custom highlighters
     * 
     * @param g
     * @param x
     * @param y
     * @param width
     * @param height
     */
    public void paint(final Graphics2D g, final int x, final int y, final int width, final int height) {
        if (this.getBorderColor() != null) {
            g.setColor(this.getBorderColor());
            g.drawRect(0, y, width, height);

        }

        if (this.getContentColor() != null) {
            g.setColor(this.getContentColor());
            g.fillRect(0, y, width, height);
        }
    }

    /**
     * @param borderColor
     *            the {@link ExtOverlayRowHighlighter#borderColor} to set
     * @see ExtOverlayRowHighlighter#borderColor
     */
    public void setBorderColor(final Color borderColor) {
        this.borderColor = borderColor;
    }

    /**
     * @param contentColor
     *            the {@link ExtOverlayRowHighlighter#contentColor} to set
     * @see ExtOverlayRowHighlighter#contentColor
     */
    public void setContentColor(final Color contentColor) {
        this.contentColor = contentColor;
    }
}
