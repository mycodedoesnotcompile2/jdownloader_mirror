/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;

import javax.swing.Icon;

import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.swing.EDT;

/**
 * @author thomas
 * @date Jan 30, 2025
 *
 */
public class DebugIcon extends AbstractIconPipe {
    private static boolean CTRL_PRESSED = false;
    static {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !Application.isHeadless()) {
            KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher() {
                @Override
                public boolean dispatchKeyEvent(KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
                        // hide size on ctrl.
                        boolean old = CTRL_PRESSED;
                        CTRL_PRESSED = (e.getID() == KeyEvent.KEY_PRESSED);
                        if (old != CTRL_PRESSED) {
                            new EDT<Void, RuntimeException>() {
                                @Override
                                protected Void runInEDT() throws RuntimeException {
                                    for (Window window : Window.getWindows()) {
                                        window.invalidate();
                                        window.validate();
                                        window.repaint();
                                    }
                                    System.out.println("Redraw");
                                    return null;
                                }
                            }.start();
                        }
                    }
                    return false;
                }
            });
        }
    }

    /**
     * @param iconEnd
     */
    public DebugIcon(Icon icon) {
        super(icon);
    }

    /**
     * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y, Icon parent) {
        paintDelegate(c, g, x, y);
        if (!CTRL_PRESSED) {
            Graphics2D g2d = (Graphics2D) g.create();
            String text = "" + (getIconWidth() * g2d.getTransform().getScaleX());
            text = text.replaceAll("\\.0$", "");
            g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, Interpolation.BILINEAR.getHint());
            g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            Font baseFont = new Font("SansSerif", Font.PLAIN, getIconWidth());
            g2d.setFont(baseFont);
            FontMetrics metrics = g2d.getFontMetrics();
            int textWidth = metrics.stringWidth(text);
            int textHeight = metrics.getAscent() + metrics.getDescent();
            double scaleX = (double) getIconWidth() / textWidth;
            double scaleY = (double) getIconHeight() / textHeight;
            double scale = Math.min(scaleX, scaleY);
            double scaledTextWidth = textWidth * scale;
            double scaledTextHeight = textHeight * scale;
            double offsetX = (getIconWidth() - scaledTextWidth) / 2;
            double offsetY = (getIconHeight() - scaledTextHeight) / 2;
            g2d.translate(x + offsetX, y + offsetY);
            g2d.scale(scale, scale);
            FontRenderContext frc = g2d.getFontRenderContext();
            GlyphVector gv = baseFont.createGlyphVector(frc, text);
            Shape textShape = gv.getOutline(0, metrics.getAscent());
            g2d.setStroke(new BasicStroke(3.0f / (float) scale, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            g2d.setColor(Color.WHITE);
            g2d.draw(textShape);
            g2d.setColor(Color.RED);
            g2d.fill(textShape);
            g2d.dispose();
        }
    }
}
