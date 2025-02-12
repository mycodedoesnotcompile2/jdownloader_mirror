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
package org.appwork.swing.components;

import java.awt.Color;
import java.awt.Composite;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class RecordingGraphics2D extends Graphics2D {
    // The underlying Graphics2D delegate.
    private final Graphics2D                delegate;
    // Recorded area for this graphics context (in device coordinates).
    private Rectangle2D                     drawnArea = null;
    // List of child RecordingGraphics2D objects created via create().
    private final List<RecordingGraphics2D> children  = new ArrayList<RecordingGraphics2D>();

    /**
     * Constructs a new RecordingGraphics2D that wraps the given delegate.
     *
     * @param delegate
     *            the underlying Graphics2D instance; must not be null
     * @throws NullPointerException
     *             if delegate is null
     */
    public RecordingGraphics2D(Graphics2D delegate) {
        if (delegate == null) {
            throw new NullPointerException("delegate must not be null");
        }
        this.delegate = delegate;
    }

    /**
     * Returns the area (as a Rectangle2D) that has been drawn by this Graphics2D instance, after applying the current transform.
     *
     * @return the recorded drawing area, or null if nothing has been drawn yet.
     */
    public Rectangle2D getDrawnArea() {
        return drawnArea;
    }

    /**
     * Returns the complete recorded area for this Graphics2D instance and all child Graphics (created via create()), in device coordinates.
     *
     * @return the union of the drawing areas of this instance and its children
     */
    public Rectangle2D getCompleteDrawnArea() {
        Rectangle2D complete = drawnArea;
        for (RecordingGraphics2D child : children) {
            Rectangle2D childArea = child.getCompleteDrawnArea();
            if (childArea != null) {
                if (complete == null) {
                    complete = childArea;
                } else {
                    complete = complete.createUnion(childArea);
                }
            }
        }
        return complete;
    }

    /**
     * Applies the current transform to the given shape and then records its bounds.
     *
     * @param s
     *            the shape in user space to be transformed and recorded
     */
    private void recordTransformed(Shape s) {
        if (s == null) {
            return;
        }
        // Get the current transform (delegated)
        AffineTransform currentTransform = getTransform();
        Shape transformedShape = currentTransform.createTransformedShape(s);
        Rectangle2D bounds = transformedShape.getBounds2D();
        if (drawnArea == null) {
            drawnArea = bounds;
        } else {
            drawnArea = drawnArea.createUnion(bounds);
        }
        System.out.println(currentTransform + "+ " + s + "(" + transformedShape + ")  = " + drawnArea);
    }

    /**
     * Convenience method to record a Rectangle (which is a Shape).
     *
     * @param rect
     *            the rectangle to record
     */
    private void recordTransformed(Rectangle rect) {
        recordTransformed((Shape) rect);
    }
    // === Drawing Methods (these methods do not actually draw but only record the transformed area) ===

    @Override
    public void draw(Shape s) {
        recordTransformed(s);
    }

    @Override
    public void fill(Shape s) {
        recordTransformed(s);
    }

    @Override
    public void drawImage(BufferedImage img, BufferedImageOp op, int x, int y) {
        if (img != null) {
            recordTransformed(new Rectangle(x, y, img.getWidth(), img.getHeight()));
        }
    }

    @Override
    public boolean drawImage(Image img, AffineTransform xform, ImageObserver obs) {
        if (img != null && xform != null) {
            int w = img.getWidth(obs);
            int h = img.getHeight(obs);
            if (w > 0 && h > 0) {
                // The image is defined in user space (0,0,w,h); then apply the given transform.
                Shape imageShape = xform.createTransformedShape(new Rectangle2D.Double(0, 0, w, h));
                recordTransformed(imageShape);
            }
        }
        return true;
    }

    @Override
    public void drawString(String str, int x, int y) {
        Font font = getFont();
        if (font != null && str != null) {
            FontRenderContext frc = getFontRenderContext();
            TextLayout layout = new TextLayout(str, font, frc);
            Rectangle2D bounds = layout.getBounds();
            // drawString uses the baseline point (x,y), so shift the bounds.
            Rectangle2D shifted = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(), bounds.getWidth(), bounds.getHeight());
            recordTransformed(shifted);
        }
    }

    @Override
    public void drawString(String str, float x, float y) {
        Font font = getFont();
        if (font != null && str != null) {
            FontRenderContext frc = getFontRenderContext();
            TextLayout layout = new TextLayout(str, font, frc);
            Rectangle2D bounds = layout.getBounds();
            Rectangle2D shifted = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(), bounds.getWidth(), bounds.getHeight());
            recordTransformed(shifted);
        }
    }

    @Override
    public void drawString(AttributedCharacterIterator iterator, int x, int y) {
        if (iterator != null) {
            FontRenderContext frc = getFontRenderContext();
            TextLayout layout = new TextLayout(iterator, frc);
            Rectangle2D bounds = layout.getBounds();
            Rectangle2D shifted = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(), bounds.getWidth(), bounds.getHeight());
            recordTransformed(shifted);
        }
    }

    @Override
    public void drawString(AttributedCharacterIterator iterator, float x, float y) {
        if (iterator != null) {
            FontRenderContext frc = getFontRenderContext();
            TextLayout layout = new TextLayout(iterator, frc);
            Rectangle2D bounds = layout.getBounds();
            Rectangle2D shifted = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(), bounds.getWidth(), bounds.getHeight());
            recordTransformed(shifted);
        }
    }

    @Override
    public void drawLine(int x1, int y1, int x2, int y2) {
        // Create a line shape in user space.
        Line2D line = new Line2D.Double(x1, y1, x2, y2);
        recordTransformed(line);
    }

    @Override
    public void fillRect(int x, int y, int width, int height) {
        recordTransformed(new Rectangle(x, y, width, height));
    }

    @Override
    public void drawRect(int x, int y, int width, int height) {
        recordTransformed(new Rectangle(x, y, width, height));
    }

    @Override
    public void clearRect(int x, int y, int width, int height) {
        recordTransformed(new Rectangle(x, y, width, height));
    }

    @Override
    public void drawRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight) {
        RoundRectangle2D roundRect = new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight);
        recordTransformed(roundRect);
    }

    @Override
    public void fillRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight) {
        RoundRectangle2D roundRect = new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight);
        recordTransformed(roundRect);
    }

    @Override
    public void drawOval(int x, int y, int width, int height) {
        Ellipse2D oval = new Ellipse2D.Double(x, y, width, height);
        recordTransformed(oval);
    }

    @Override
    public void fillOval(int x, int y, int width, int height) {
        Ellipse2D oval = new Ellipse2D.Double(x, y, width, height);
        recordTransformed(oval);
    }

    @Override
    public void drawArc(int x, int y, int width, int height, int startAngle, int arcAngle) {
        Arc2D arc = new Arc2D.Double(x, y, width, height, startAngle, arcAngle, Arc2D.OPEN);
        recordTransformed(arc);
    }

    @Override
    public void fillArc(int x, int y, int width, int height, int startAngle, int arcAngle) {
        Arc2D arc = new Arc2D.Double(x, y, width, height, startAngle, arcAngle, Arc2D.PIE);
        recordTransformed(arc);
    }

    @Override
    public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints) {
        if (xPoints == null || yPoints == null || nPoints <= 0) {
            return;
        }
        Polygon polyline = new Polygon(xPoints, yPoints, nPoints);
        recordTransformed(polyline.getBounds());
    }

    @Override
    public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints) {
        if (xPoints == null || yPoints == null || nPoints <= 0) {
            return;
        }
        Polygon polygon = new Polygon(xPoints, yPoints, nPoints);
        recordTransformed(polygon.getBounds());
    }

    @Override
    public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints) {
        if (xPoints == null || yPoints == null || nPoints <= 0) {
            return;
        }
        Polygon polygon = new Polygon(xPoints, yPoints, nPoints);
        recordTransformed(polygon.getBounds());
    }

    @Override
    public void drawRenderedImage(RenderedImage img, AffineTransform xform) {
        if (img != null && xform != null) {
            int width = img.getWidth();
            int height = img.getHeight();
            Shape imageShape = xform.createTransformedShape(new Rectangle2D.Double(0, 0, width, height));
            recordTransformed(imageShape);
        }
    }

    @Override
    public void drawRenderableImage(RenderableImage img, AffineTransform xform) {
        if (img != null && xform != null) {
            RenderedImage ri = img.createDefaultRendering();
            if (ri != null) {
                int width = ri.getWidth();
                int height = ri.getHeight();
                Shape imageShape = xform.createTransformedShape(new Rectangle2D.Double(0, 0, width, height));
                recordTransformed(imageShape);
            }
        }
    }

    @Override
    public void drawGlyphVector(GlyphVector g, float x, float y) {
        if (g != null) {
            Shape glyphOutline = g.getOutline(x, y);
            recordTransformed(glyphOutline);
        }
    }

    @Override
    public boolean hit(Rectangle rect, Shape s, boolean onStroke) {
        // No hit detection is performed.
        return false;
    }

    @Override
    public GraphicsConfiguration getDeviceConfiguration() {
        return delegate.getDeviceConfiguration();
    }

    @Override
    public void copyArea(int x, int y, int width, int height, int dx, int dy) {
        recordTransformed(new Rectangle(x, y, width, height));
    }

    @Override
    public boolean drawImage(Image img, int x, int y, ImageObserver observer) {
        if (img != null) {
            int w = img.getWidth(observer);
            int h = img.getHeight(observer);
            if (w > 0 && h > 0) {
                recordTransformed(new Rectangle(x, y, w, h));
            }
        }
        return true;
    }

    @Override
    public boolean drawImage(Image img, int x, int y, int width, int height, ImageObserver observer) {
        if (img != null) {
            recordTransformed(new Rectangle(x, y, width, height));
        }
        return true;
    }

    @Override
    public boolean drawImage(Image img, int x, int y, Color bgcolor, ImageObserver observer) {
        if (img != null) {
            int w = img.getWidth(observer);
            int h = img.getHeight(observer);
            if (w > 0 && h > 0) {
                recordTransformed(new Rectangle(x, y, w, h));
            }
        }
        return true;
    }

    @Override
    public boolean drawImage(Image img, int x, int y, int width, int height, Color bgcolor, ImageObserver observer) {
        if (img != null) {
            recordTransformed(new Rectangle(x, y, width, height));
        }
        return true;
    }

    @Override
    public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, ImageObserver observer) {
        if (img != null) {
            recordTransformed(new Rectangle(dx1, dy1, dx2 - dx1, dy2 - dy1));
        }
        return true;
    }

    @Override
    public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, Color bgcolor, ImageObserver observer) {
        if (img != null) {
            recordTransformed(new Rectangle(dx1, dy1, dx2 - dx1, dy2 - dy1));
        }
        return true;
    }
    // === Non-Drawing Methods (forwarded to the delegate) ===

    @Override
    public void clip(Shape s) {
        delegate.clip(s);
    }

    @Override
    public void setPaint(Paint paint) {
        delegate.setPaint(paint);
    }

    @Override
    public void setComposite(Composite comp) {
        delegate.setComposite(comp);
    }

    @Override
    public void setStroke(Stroke s) {
        delegate.setStroke(s);
    }

    @Override
    public void setRenderingHint(RenderingHints.Key hintKey, Object hintValue) {
        delegate.setRenderingHint(hintKey, hintValue);
    }

    @Override
    public Object getRenderingHint(RenderingHints.Key hintKey) {
        return delegate.getRenderingHint(hintKey);
    }

    @Override
    public void setRenderingHints(Map<?, ?> hints) {
        delegate.setRenderingHints(hints);
    }

    @Override
    public void addRenderingHints(Map<?, ?> hints) {
        delegate.addRenderingHints(hints);
    }

    @Override
    public RenderingHints getRenderingHints() {
        return delegate.getRenderingHints();
    }

    @Override
    public void translate(int x, int y) {
        delegate.translate(x, y);
    }

    @Override
    public void translate(double tx, double ty) {
        delegate.translate(tx, ty);
    }

    @Override
    public void rotate(double theta) {
        delegate.rotate(theta);
    }

    @Override
    public void rotate(double theta, double x, double y) {
        delegate.rotate(theta, x, y);
    }

    @Override
    public void scale(double sx, double sy) {
        delegate.scale(sx, sy);
    }

    @Override
    public void shear(double shx, double shy) {
        delegate.shear(shx, shy);
    }

    @Override
    public void transform(AffineTransform Tx) {
        delegate.transform(Tx);
    }

    @Override
    public void setTransform(AffineTransform Tx) {
        delegate.setTransform(Tx);
    }

    @Override
    public AffineTransform getTransform() {
        return delegate.getTransform();
    }

    @Override
    public Paint getPaint() {
        return delegate.getPaint();
    }

    @Override
    public Composite getComposite() {
        return delegate.getComposite();
    }

    @Override
    public void setBackground(Color color) {
        delegate.setBackground(color);
    }

    @Override
    public Color getBackground() {
        return delegate.getBackground();
    }

    @Override
    public Stroke getStroke() {
        return delegate.getStroke();
    }

    @Override
    public void clipRect(int x, int y, int width, int height) {
        delegate.clipRect(x, y, width, height);
    }

    @Override
    public void setClip(Shape clip) {
        delegate.setClip(clip);
    }

    @Override
    public Shape getClip() {
        return delegate.getClip();
    }

    @Override
    public void setClip(int x, int y, int width, int height) {
        delegate.setClip(x, y, width, height);
    }

    @Override
    public Rectangle getClipBounds() {
        return delegate.getClipBounds();
    }

    @Override
    public FontRenderContext getFontRenderContext() {
        return delegate.getFontRenderContext();
    }

    /**
     * Overrides create() to store the created child graphics and return a new RecordingGraphics2D.
     */
    @Override
    public Graphics create() {
        Graphics g = delegate.create();
        RecordingGraphics2D child = new RecordingGraphics2D((Graphics2D) g);
        children.add(child);
        return child;
    }

    @Override
    public Color getColor() {
        return delegate.getColor();
    }

    @Override
    public void setColor(Color c) {
        delegate.setColor(c);
    }

    @Override
    public void setPaintMode() {
        delegate.setPaintMode();
    }

    @Override
    public void setXORMode(Color c1) {
        delegate.setXORMode(c1);
    }

    @Override
    public Font getFont() {
        return delegate.getFont();
    }

    @Override
    public void setFont(Font font) {
        delegate.setFont(font);
    }

    @Override
    public FontMetrics getFontMetrics(Font f) {
        return delegate.getFontMetrics(f);
    }

    @Override
    public void dispose() {
        delegate.dispose();
    }
}
