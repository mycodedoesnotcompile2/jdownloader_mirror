/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Paint;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map.Entry;

import javax.swing.Icon;

import org.appwork.utils.swing.Graphics2DProxy;

/**
 * @author thomas
 * @date 13.12.2023
 *
 */
public class ColoredIcon extends AbstractIconPipe {
    private HashMap<ColorLookup, Color> colorMap;

    public ColoredIcon(Icon icon) {
        super(icon);
        this.colorMap = new HashMap<ColorLookup, Color>();
    }

    /**
     * @param icon
     * @param width
     * @param height
     */
    public ColoredIcon(Icon icon, Color color) {
        super(icon);
        this.colorMap = new HashMap<ColorLookup, Color>();
        replace(new ColorLookup(null, 0, true), color);
    }

    public static class ColorLookup {
        private boolean keepBrightness = true;

        /**
         * @param search
         * @param tollerance2
         */
        public ColorLookup(Color search, int tollerance2, boolean keepBrightness) {
            this.color = search;
            this.tollerance = tollerance2;
            this.keepBrightness = keepBrightness;
        }

        private Color color;
        private int   tollerance = 0;
    }

    public ColoredIcon(Icon icon, ColorLookup from, Color to) {
        super(icon);
        this.colorMap = new HashMap<ColorLookup, Color>();
        replace(from, to);
    }

    /**
     * @param from
     * @param to
     */
    public ColoredIcon replace(ColorLookup from, Color to) {
        colorMap.put(from, to);
        return this;
    }

    /**
     * @param icon
     * @param black
     * @param textForeGroundForComponent
     */
    public ColoredIcon(Icon icon, Color from, Color to) {
        super(icon);
        this.colorMap = new HashMap<ColorLookup, Color>();
        replace(new ColorLookup(from, 50, true), to);
    }

    private class Graphics2DProxyImpl extends Graphics2DProxy {
        private ColoredIcon icon;

        /**
         * @param g
         * @param coloredIcon
         */
        public Graphics2DProxyImpl(Graphics2D g, ColoredIcon coloredIcon) {
            super(g);
            this.icon = coloredIcon;
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#beforeBufferedImageDraw(java.awt.image.BufferedImage)
         */
        @Override
        protected BufferedImage beforeBufferedImageDraw(BufferedImage img) {
            return IconIO.toBufferedImage(beforeImageDraw(img));
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#beforeImageDraw(java.awt.Image)
         */
        @Override
        protected Image beforeImageDraw(Image img) {
            Image ret = img;
            for (Entry<ColorLookup, Color> es : colorMap.entrySet()) {
                ret = IconIO.replaceColor(IconIO.toBufferedImage(ret), es.getKey().color, es.getKey().tollerance, es.getValue(), es.getKey().keepBrightness);
            }
            return ret;
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#setComposite(java.awt.Composite)
         */
        @Override
        public void setComposite(Composite comp) {
            super.setComposite(comp);
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#setPaint(java.awt.Paint)
         */
        @Override
        public void setPaint(Paint paint) {
            super.setPaint(icon.modifyPaint(paint));
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#setColor(java.awt.Color)
         */
        @Override
        public void setColor(Color c) {
            super.setColor(icon.modifyColor(c));
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#create()
         */
        @Override
        public Graphics create() {
            return new Graphics2DProxyImpl((Graphics2D) this.delegate.create(), icon);
        }

        /**
         * @see org.appwork.utils.swing.Graphics2DProxy#create(int, int, int, int)
         */
        @Override
        public Graphics create(int x, int y, int width, int height) {
            return new Graphics2DProxyImpl((Graphics2D) super.create(x, y, width, height), icon);
        }
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
        this.paintIcon(c, g, x, y, null);
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y, Icon parent) {
        paintDelegate(c, new Graphics2DProxyImpl((Graphics2D) g, this), x, y);
    }

    /**
     * @param c
     * @return
     */
    public Color modifyColor(Color c) {
        Color replacement = null;
        final int a1 = c == null ? 0 : c.getAlpha();
        final int r1 = c == null ? 0 : c.getRed();
        final int g1 = c == null ? 0 : c.getGreen();
        final int b1 = c == null ? 0 : c.getBlue();
        for (Entry<ColorLookup, Color> es : colorMap.entrySet()) {
            if (es.getKey().color == null) {
                if (c == null || c.getAlpha() == 0) {
                    return es.getValue();
                }
                continue;
            }
            int rgb = es.getKey().color.getRGB();
            final int a = (rgb >> 24) & 0xff;
            final int r = (rgb >> 16) & 0xff;
            final int g = (rgb >> 8) & 0xff;
            final int b = (rgb >> 0) & 0xff;
            if (Math.abs(r - r1) <= es.getKey().tollerance && Math.abs(g - g1) <= es.getKey().tollerance && Math.abs(b - b1) <= es.getKey().tollerance && Math.abs(a - a1) <= es.getKey().tollerance) {
                replacement = es.getValue();
            }
            if (replacement == null) {
                replacement = colorMap.get(null);
            }
            if (replacement != null) {
                return replacement;
            }
        }
        return c;
    }

    /**
     * @param paint
     * @return
     */
    public Paint modifyPaint(Paint paint) {
        if (paint instanceof Color) {
            return modifyColor((Color) paint);
        }
        return paint;
    }
}
