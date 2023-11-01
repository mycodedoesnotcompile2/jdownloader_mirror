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

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author daniel
 * @date Aug 11, 2023
 *
 */
public class WeisjJSVG {
    public static Image getImageFromSVG(InputStream inputStream, int w, int h, final Color backgroundColor) throws IOException {
        try {
            com.github.weisj.jsvg.parser.SVGLoader loader = new com.github.weisj.jsvg.parser.SVGLoader();
            final com.github.weisj.jsvg.SVGDocument svgDocument = loader.load(inputStream);
            if (svgDocument != null) {
                final com.github.weisj.jsvg.geometry.size.FloatSize size = svgDocument.size();
                if (w <= 0) {
                    w = (int) size.getWidth();
                }
                if (h <= 0) {
                    h = (int) size.getHeight();
                }
                final double scaleWidth;
                final double scaleHeight;
                if (false) {
                    scaleWidth = 1d / Math.max(size.getWidth() / w, 1);
                    scaleHeight = 1d / Math.max(size.getHeight() / h, 1);
                } else {
                    scaleWidth = 1d / Math.max(size.getWidth() / w, size.getHeight() / h);
                    scaleHeight = scaleWidth;
                }
                final int width = Math.max((int) (size.getWidth() * scaleWidth), 1);
                final int height = Math.max((int) (size.getHeight() * scaleHeight), 1);
                final BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
                final Graphics2D g = bi.createGraphics();
                try {
                    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
                    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                    if (backgroundColor != null) {
                        g.setComposite(AlphaComposite.Src);
                        g.setColor(backgroundColor);
                        g.fillRect(0, 0, width, height);
                        g.setComposite(AlphaComposite.SrcAtop);
                    }
                    if (scaleWidth != 1.0d || scaleHeight != 1.0d) {
                        g.scale(scaleWidth, scaleHeight);
                    }
                    svgDocument.render(null, g);
                } finally {
                    g.dispose();
                }
                return bi;
            }
            return null;
        } catch (Throwable e) {
            throw new IOException(e);
        }
    }
}
