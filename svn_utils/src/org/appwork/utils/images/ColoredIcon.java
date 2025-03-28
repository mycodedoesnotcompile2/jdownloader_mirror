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
import java.awt.Transparency;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.resources.HighDPIIcon;
import org.appwork.utils.swing.Graphics2DProxy;

/**
 * @author thomas
 * @date 13.12.2023
 *
 */
public class ColoredIcon extends AbstractIconPipe {
    private HashMap<ColorLookup, Color> colorMap;
    private BufferedImage               cache;
    private double                      cachedScaleY;
    private double                      cachedScaleX;

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
         *
         *
         * @param search
         * @param tollerance2
         * @param keepBrightness
         *            if set to true, black will stay black, and white will stay white. all colors in between will have a brigtness variant
         *            of the target color
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
            // TODO:
            // if there is no inner HighDPI Icon in the Pipe, this might be a MultiResImage.
            // in this case we have options:
            // 1. do NOT convert to toBufferedImage image here, and add multiresimage support to replaceColor
            // 2. search and use the best variant here, and applay replace Color only to this variant.
            // DebugMode.breakIf(MultiResolutionImageHelper.isInstanceOf(img));
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
    public void paintIcon(Component c, Graphics g, int x, int y, List<Icon> parents) {
        AffineTransform transform = ((Graphics2D) g).getTransform();
        if (cache == null || cachedScaleX != transform.getScaleX() || cachedScaleY != transform.getScaleY()) {
            cache = IconIO.createEmptyImage((int) Math.round(getIconWidth() * transform.getScaleX()), (int) Math.round(getIconHeight() * transform.getScaleY()), BufferedImage.TYPE_INT_ARGB, Transparency.TRANSLUCENT);
            cachedScaleX = transform.getScaleX();
            cachedScaleY = transform.getScaleY();
            Graphics2D g2 = cache.createGraphics();
            g2.scale(cachedScaleX, cachedScaleY);
            paintDelegate(c, new Graphics2DProxyImpl(g2, this), 0, 0, parents);
            g2.dispose();
        }
        Graphics2D g2 = (Graphics2D) g.create();
        g2.scale(1d / transform.getScaleX(), 1d / transform.getScaleY());
        new HighDPIIcon(cache).paintIcon(c, g2, (int) (x * cachedScaleX), (int) (y * cachedScaleY), parents);
        g2.dispose();
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "Colored Icon: " + getDelegate() + " " + getIconWidth() + ":" + getIconHeight();
    }

    /**
     * @param c
     * @return
     */
    public Color modifyColor(Color c) {
        Color replacement = null;
        int rgb = c.getRGB();
        final int a = (rgb >> 24) & 0xff;
        final int r = (rgb >> 16) & 0xff;
        final int g = (rgb >> 8) & 0xff;
        final int b = (rgb >> 0) & 0xff;
        for (Entry<ColorLookup, Color> es : colorMap.entrySet()) {
            if (es.getKey().color == null) {
                if (c == null || c.getAlpha() == 0) {
                    return es.getValue();
                }
                continue;
            }
            Color replace = es.getValue();
            Color search = es.getKey().color;
            final int a1 = search.getAlpha();
            final int r1 = search.getRed();
            final int g1 = search.getGreen();
            final int b1 = search.getBlue();
            int tollerance = es.getKey().tollerance;
            boolean keepBrightness = es.getKey().keepBrightness;
            if (Math.abs(r - r1) <= tollerance && Math.abs(g - g1) <= tollerance && Math.abs(b - b1) <= tollerance && Math.abs(a - a1) <= tollerance) {
                if (!keepBrightness) {
                    return replace;
                }
                final double brightness = (0.299 * r + 0.578 * g + 0.114 * b) / 255d;
                Color nc = new Color((int) (replace.getRed() * brightness), (int) (replace.getGreen() * brightness), (int) (replace.getBlue() * brightness), replace.getAlpha() * a / 255);
                return nc;
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

    private static final Set<ModificationType> MODIFICATIONS = Collections.unmodifiableSet(new HashSet<ModificationType>(Arrays.asList(ModificationType.COLOR)));

    /**
     * @see org.appwork.utils.images.IconPipe#getModifications()
     */
    @Override
    public Set<ModificationType> getModifications() {
        return MODIFICATIONS;
    }
}
