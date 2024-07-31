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

import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.font.FontRenderContext;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.text.JTextComponent;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.StringUtils;

public class SwingUtils {
    /**
     * Calculates the position of a frame to be in the center of an other frame.
     *
     * @param parentFrame
     * @param frame
     * @return
     */
    public static Point getCenter(final Component parentFrame, final Window frame) {
        final Point point = new Point();
        if (parentFrame == null || frame == null) {
            point.setLocation(0, 0);
        } else {
            final int x = parentFrame.getLocation().x + parentFrame.getSize().width / 2 - frame.getSize().width / 2;
            final int y = parentFrame.getLocation().y + parentFrame.getSize().height / 2 - frame.getSize().height / 2;
            point.setLocation(x, y);
        }
        return point;
    }

    /**
     * @param frame
     * @param string
     */
    public static JComponent getComponentByName(final JComponent frame, final String name) {
        JComponent ret = null;
        for (final Component c : frame.getComponents()) {
            if (c instanceof JComponent) {
                if (c.getName() != null && c.getName().equals(name)) {
                    return (JComponent) c;
                } else {
                    ret = SwingUtils.getComponentByName((JComponent) c, name);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
        }
        return null;
    }

    public static Window getWindowForComponent(final Component parentComponent) {
        if (parentComponent == null) {
            return JOptionPane.getRootFrame();
        } else if (parentComponent instanceof Frame || parentComponent instanceof java.awt.Dialog) {
            return (Window) parentComponent;
        } else {
            return SwingUtils.getWindowForComponent(parentComponent.getParent());
        }
    }

    /**
     * Sets a component's opaque status
     *
     * @param descriptionField
     * @param b
     */
    public static JComponent setOpaque(final JComponent descriptionField, final boolean b) {
        descriptionField.setOpaque(b);
        descriptionField.putClientProperty("Synthetica.opaque", b ? Boolean.TRUE : Boolean.FALSE);
        return descriptionField;
    }

    /**
     * @param btnDetails
     */
    public static <T extends AbstractButton> T toBold(final T button) {
        final Font f = button.getFont();
        button.setFont(f.deriveFont(f.getStyle() ^ Font.BOLD));
        return button;
    }

    /**
     * @param ret
     * @return
     * @return
     */
    public static <T extends JLabel> T toBold(final T label) {
        final Font f = label.getFont();
        label.setFont(f.deriveFont(f.getStyle() ^ Font.BOLD));
        return label;
    }

    /**
     * @param label
     */
    public static <T extends JTextComponent> T toBold(final T label) {
        final Font f = label.getFont();
        label.setFont(f.deriveFont(f.getStyle() ^ Font.BOLD));
        return label;
    }

    /**
     * @param fc
     */
    public static void printComponentTree(final JComponent fc) {
        printComponentTree(fc, "");
    }

    /**
     * @param fc
     * @param string
     */
    private static void printComponentTree(final JComponent fc, final String string) {
        // c.setVisible(false);
        for (int i = 0; i < fc.getComponentCount(); i++) {
            final Component cc = fc.getComponent(i);
            System.out.println(string + "[" + i + "]" + cc.getClass().getSuperclass().getSimpleName() + ":" + cc + " Opaque: " + cc.isOpaque());
            if (cc instanceof JComponent) {
                printComponentTree((JComponent) cc, string + "[" + i + "]");
            }
        }
    }

    public static <T> Map<T, List<Integer>> find(final Component c, Map<T, List<Integer>> map, final Class<T> search) {
        List<Integer> path = null;
        if (map == null) {
            map = new HashMap<T, List<Integer>>();
        }
        path = map.get(null);
        if (path == null) {
            path = new ArrayList<Integer>();
        }
        if (search.isAssignableFrom(c.getClass())) {
            map.put((T) c, path);
        } else if (c instanceof Container) {
            final Container co = (Container) c;
            for (int index = 0; index < co.getComponentCount(); index++) {
                final ArrayList<Integer> nextPath = new ArrayList<Integer>(path);
                nextPath.add(index);
                final Map<T, List<Integer>> nextMap = new HashMap<T, List<Integer>>(map);
                nextMap.put(null, nextPath);
                final Map<T, List<Integer>> ret = find(co.getComponent(index), nextMap, search);
                if (ret != null) {
                    map = ret;
                }
            }
        }
        map.remove(null);
        if (map.size() > 0) {
            return map;
        } else {
            return null;
        }
    }

    /**
     * @param fc
     * @param i
     * @param j
     * @param k
     */
    public static JComponent getParent(JComponent parent, final int... path) {
        for (final int i : path) {
            parent = (JComponent) parent.getComponent(i);
        }
        return parent;
    }

    /**
     * May return null!
     *
     * @param p
     * @return
     */
    public static GraphicsDevice getScreenByLocation(final Point location) {
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        for (final GraphicsDevice screen : screens) {
            final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
            if (bounds.contains(location)) {
                return screen;
            }
        }
        return null;
    }

    /**
     * @return
     */
    public static GraphicsDevice getScreenByLocation(int x, int y) {
        return getScreenByLocation(new Point(x, y));
    }

    /**
     * @param screen
     * @return
     */
    public static Rectangle getUsableScreenBounds(GraphicsDevice screen) {
        final GraphicsConfiguration configuration = screen.getDefaultConfiguration();
        final Rectangle bounds = configuration.getBounds();
        final Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(configuration);
        bounds.x += insets.left;
        bounds.y += insets.top;
        bounds.width -= insets.left + insets.right;
        bounds.height -= insets.top + insets.bottom;
        return bounds;
    }

    /**
     * Returns true if the rectangle can be fully shown on the current screen setup. If any part of the rectangle is offscreen, or behind
     * insets like taskbars, this method will return false;
     *
     * @param dialogBounds
     * @return
     */
    public static boolean isRectangleFullyDisplayableOnScreens(Rectangle bounds) {
        Area p = new Area(bounds);
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        for (final GraphicsDevice screen : screens) {
            Rectangle usableBounds = SwingUtils.getUsableScreenBounds(screen);
            p.subtract(new Area(usableBounds));
            if (p.isEmpty()) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param bounds
     * @return
     */
    public static GraphicsDevice getScreenByBounds(final Rectangle bounds) {
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        Rectangle biggestIntersection = null;
        GraphicsDevice bestScreenByBounds = null;
        for (final GraphicsDevice screen : screens) {
            final Rectangle screenBounds = screen.getDefaultConfiguration().getBounds();
            final Rectangle intersection = screenBounds.intersection(bounds);
            if (intersection != null && intersection.width > 0 && intersection.height > 0) {
                // intersection can be negative width/height
                if (biggestIntersection == null || (intersection.width * intersection.height > biggestIntersection.width * biggestIntersection.height)) {
                    biggestIntersection = intersection;
                    bestScreenByBounds = screen;
                    if (intersection.equals(bounds)) {
                        // it will not get better
                        break;
                    }
                }
            }
        }
        return bestScreenByBounds;
    }

    public static Rectangle2D getStringSizeForFont(final String string, final Font font) {
        if (font == null) {
            return null;
        } else if (string == null || string.length() == 0) {
            return new Rectangle2D.Double(0, 0, 0, 0);
        } else {
            final FontRenderContext fontRenderContext = new FontRenderContext(new AffineTransform(), true, true);
            final double width = font.getStringBounds(string, fontRenderContext).getWidth();
            final double height = font.getStringBounds(string, fontRenderContext).getHeight();
            return new Rectangle2D.Double(0, 0, width, height);
        }
    }

    /**
     * @param screenID
     * @return
     */
    public static GraphicsDevice getScreenByID(final String screenID) {
        if (StringUtils.isEmpty(screenID)) {
            return null;
        } else {
            final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            final GraphicsDevice[] screens = ge.getScreenDevices();
            for (final GraphicsDevice screen : screens) {
                if (StringUtils.equals(screen.getIDstring(), screenID)) {
                    return screen;
                }
            }
            return null;
        }
    }

    /**
     * @param runnable
     * @param i
     */
    public static void invokeLater(final Runnable runnable, int delayInMS) {
        if (delayInMS <= 0) {
            SwingUtilities.invokeLater(runnable);
        } else {
            final Timer timer = new Timer(delayInMS, new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    runnable.run();
                }
            });
            timer.setRepeats(false);
            timer.start();
        }
    }

    /**
     * @param device
     * @param point
     * @return
     */
    public static Point convertToUnscaled(GraphicsDevice device, Point target) {
        GraphicsDevice[] screens = device != null ? new GraphicsDevice[] { device } : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices();
        for (final GraphicsDevice screen : screens) {
            Point point = target;
            final GraphicsConfiguration screenConfiguration = screen.getDefaultConfiguration();
            // OS DPI Scaling support.
            final AffineTransform transformation = screenConfiguration.getDefaultTransform();
            final Point2D p2d = transformation.transform(target, null);
            point = new Point((int) p2d.getX(), (int) p2d.getY());
            if (device != null) {
                return point;
            }
            final Rectangle bounds = screenConfiguration.getBounds();
            if (bounds.contains(point)) {
                return point;
            }
        }
        return null;
    }

    public static Point convertToScaled(Point target, GraphicsDevice device) {
        GraphicsDevice[] screens = device != null ? new GraphicsDevice[] { device } : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices();
        for (final GraphicsDevice screen : screens) {
            Point point = target;
            final GraphicsConfiguration screenConfiguration = screen.getDefaultConfiguration();
            try {
                // OS DPI Scaling support.
                final AffineTransform transformation = screenConfiguration.getDefaultTransform();
                final Point2D p2d = transformation.inverseTransform(target, null);
                point = new Point((int) p2d.getX(), (int) p2d.getY());
            } catch (NoninvertibleTransformException e1) {
                LogV3.log(e1);
            }
            if (device != null) {
                return point;
            }
            final Rectangle bounds = screenConfiguration.getBounds();
            if (bounds.contains(point)) {
                return point;
            }
        }
        return null;
    }
}
