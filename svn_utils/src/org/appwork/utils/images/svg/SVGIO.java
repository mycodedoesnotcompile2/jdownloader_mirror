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

import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGElement;
import com.kitfox.svg.SVGElementException;
import com.kitfox.svg.SVGRoot;
import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.animation.AnimationElement;

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
            // Rectangle dp = diagram.getDeviceViewport();
            // Rectangle2D vr = diagram.getViewRect();
            // Rectangle2D bb = diagram.getRoot().getBoundingBox();
            if (color != null) {
                final SVGRoot root = diagram.getRoot();
                // set color
                final float alpha = color.getAlpha() / 255f;
                final String hex = "#" + String.format("%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
                // root.setAttribute("fill", AnimationElement.AT_CSS, hex);
                setColor(root, alpha, hex);
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
                final AffineTransform oldXform = g.getTransform();
                g.transform(scaleXform);
                diagram.render(g);
                g.setTransform(oldXform);
                g.translate(-x, -y);
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

    private static void setColor(SVGElement root, float alpha, String hex) throws SVGElementException {
        if (root.hasAttribute("fill", AnimationElement.AT_CSS)) {
            root.setAttribute("fill", AnimationElement.AT_CSS, hex);
        } else {
            root.addAttribute("fill", AnimationElement.AT_CSS, hex);
        }
        if (root.hasAttribute("fill-opacity", AnimationElement.AT_CSS)) {
            root.setAttribute("fill-opacity", AnimationElement.AT_CSS, alpha + "");
        } else {
            root.addAttribute("fill-opacity", AnimationElement.AT_CSS, alpha + "");
        }
        for (int i = 0; i < root.getNumChildren(); i++) {
            SVGElement child = root.getChild(i);
            setColor(child, alpha, hex);
        }
    }
}
