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
package org.appwork.utils.images.svg;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.images.MultiResIcon;

import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGElementException;
import com.kitfox.svg.SVGException;
import com.kitfox.svg.SVGUniverse;

/**
 * @author daniel
 * @date 04.10.2018
 *
 */
public class SVGIO {
    public static Image getImageFromSVG(URL url, int w, int h) throws IOException {
        return getImageFromSVG(url, w, h, null);
    }

    public static Image getImageFromSVG(URL url, int w, int h, Color color) throws IOException {
        try {
            InputStream is = null;
            try {
                is = url.openStream();
                if (is != null) {
                    return getImageFromSVG(is, w, h, color);
                } else {
                    throw new IOException("Not found:" + url);
                }
            } finally {
                if (is != null) {
                    is.close();
                }
            }
        } catch (IOException e) {
            throw e;
        } catch (Throwable e) {
            throw new IOException("URL:" + url, e);
        }
    }

    public static Image getImageFromSVG(InputStream is, int w, int h, Color color) throws IOException {
        try {
            final SVGUniverse universe = new SVGUniverse();
            final URI uri = universe.loadSVG(is, "dummy.svg");
            final SVGDiagram diagram = universe.getDiagram(uri);
            if (diagram == null) {
                return null;
            }
            diagram.updateTime(0d);
            diagram.setIgnoringClipHeuristic(true);
            if (w <= 0) {
                w = (int) diagram.getWidth();
            }
            if (h <= 0) {
                h = (int) diagram.getHeight();
            }
            final double faktor = 1d / Math.max((double) diagram.getWidth() / w, (double) diagram.getHeight() / h);
            final int width = Math.max((int) (diagram.getWidth() * faktor), 1);
            final int height = Math.max((int) (diagram.getHeight() * faktor), 1);
            final BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
            final Graphics2D g = bi.createGraphics();
            try {
                g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
                g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                int x = 0;
                int y = 0;
                g.translate(x, y);
                final Rectangle2D.Double rect = new Rectangle2D.Double();
                diagram.getViewRect(rect);
                final AffineTransform scaleXform = new AffineTransform();
                scaleXform.setToScale(width / rect.width, height / rect.height);
                g.transform(scaleXform);
                diagram.render(g);
            } finally {
                g.dispose();
            }
            return bi;
        } catch (IOException e) {
            throw e;
        } catch (Throwable e) {
            throw new IOException(e);
        }
    }

    /**
     * @param stream
     * @param width
     * @param height
     * @param color
     * @return
     * @throws IOException
     */
    public static MultiResIcon getIconFromSVG(InputStream stream, int width, int height, Color color) throws IOException {
        final SVGUniverse universe = new SVGUniverse();
        URI uri;
        try {
            uri = universe.loadSVG(stream, "dummy.svg");
            final SVGDiagram diagram = universe.getDiagram(uri);
            if (diagram == null) {
                return null;
            }
            diagram.updateTime(0d);
            diagram.setIgnoringClipHeuristic(true);
            return new SVGIcon(width, height) {
                /**
                 * @see org.appwork.utils.images.svg.SVGIcon#getIconHeight()
                 */
                @Override
                public int getIconHeight() {
                    ensureDimensions();
                    return super.getIconHeight();
                }

                private void ensureDimensions() {
                    if (super.getIconWidth() <= 0) {
                        setIconWidth((int) diagram.getWidth());
                    }
                    if (super.getIconHeight() <= 0) {
                        setIconHeight((int) diagram.getHeight());
                    }
                }

                /**
                 * @see org.appwork.utils.images.svg.SVGIcon#getIconWidth()
                 */
                @Override
                public int getIconWidth() {
                    ensureDimensions();
                    return super.getIconWidth();
                }

                /**
                 * @see org.appwork.utils.images.svg.SVGIcon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
                 */
                @Override
                public void paintIcon(Component c, Graphics g1D, int x, int y, int width, int height) {
                    if (width <= 0) {
                        width = (int) diagram.getWidth();
                    }
                    if (height <= 0) {
                        height = (int) diagram.getHeight();
                    }
                    final double faktor = 1d / Math.max((double) diagram.getWidth() / width, (double) diagram.getHeight() / height);
                    width = Math.max((int) (diagram.getWidth() * faktor), 1);
                    height = Math.max((int) (diagram.getHeight() * faktor), 1);
                    final Graphics2D g = (Graphics2D) g1D;
                    RenderingHints restoreHints = g.getRenderingHints();
                    AffineTransform restoreTransform = g.getTransform();
                    try {
                        g.translate(x, y);
                        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
                        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                        final Rectangle2D.Double rect = new Rectangle2D.Double();
                        diagram.getViewRect(rect);
                        g.scale(width / rect.width, height / rect.height);
                        try {
                            diagram.render(g);
                        } catch (SVGException e) {
                            throw new WTFException(e);
                        }
                    } finally {
                        g.setRenderingHints(restoreHints);
                        g.setTransform(restoreTransform);
                    }
                }
            };
        } catch (SVGElementException e) {
            throw new IOException(e);
        } catch (SVGException e1) {
            throw new IOException(e1);
        }
    }
}
