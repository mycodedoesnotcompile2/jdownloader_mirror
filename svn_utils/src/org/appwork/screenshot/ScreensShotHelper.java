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
package org.appwork.screenshot;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.DisplayMode;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.image.BufferedImage;

/**
 * @author thomas
 * 
 */
public class ScreensShotHelper {

    /**
     * @return
     * @throws AWTException
     */
    public static Image getFullScreenShot() throws AWTException {
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();

        // for (final GraphicsDevice screen : screens) {
        int xMax = 0;
        int xMin = 0;
        int yMax = 0;
        int yMin = 0;
        for (final GraphicsDevice screen : screens) {
            final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
            xMax = Math.max(xMax, bounds.x + bounds.width);
            yMax = Math.max(bounds.y + bounds.height, yMax);
            yMin = Math.min(yMin, bounds.y);
            xMin = Math.min(xMin, bounds.x);
        }
        // final BufferedImage complete = new BufferedImage(xMax - xMin, yMax -
        // yMin, Transparency.TRANSLUCENT);
        Image complete = null;
        Graphics2D g2 = null;

        final BufferedImage img = new BufferedImage(xMax - xMin, yMax - yMin, BufferedImage.TYPE_INT_RGB);
        g2 = img.createGraphics();
        complete = img;

        for (final GraphicsDevice screen : screens) {
            final DisplayMode dm = screen.getDisplayMode();
            // bounds are used to gete the position and size of this screen in
            // the complete screen configuration
            final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
            final int screenWidth = dm.getWidth();
            final int screenHeight = dm.getHeight();
            final Rectangle rect = new Rectangle(screenWidth, screenHeight);
            final Robot robot = new Robot(screen);
            final BufferedImage image = robot.createScreenCapture(rect);
            g2.drawImage(image, bounds.x - xMin, bounds.y - yMin, null);

        }
        g2.dispose();

        g2 = null;

        return img;
    }

    /**
     * @param x
     * @param y
     * @param width
     * @param height
     * @return
     */
    public static Image getScreenShot(final int x, final int y, final int width, final int height) {

        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();
        final Rectangle rect = new Rectangle(width, height);
        rect.x = x;
        rect.y = y;

        try {

            final BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            final Graphics2D g2 = img.createGraphics();
            g2.setColor(Color.ORANGE);
            g2.fillRect(0, 0, width, height);
            for (final GraphicsDevice screen : screens) {
                final DisplayMode dm = screen.getDisplayMode();
                // bounds are used to gete the position and size of this screen
                // in
                // the complete screen configuration
                final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
                if (!bounds.intersects(rect)) {
                    continue;
                }

                final Rectangle part = bounds.intersection(rect);
                part.x -= bounds.x;
                part.y -= bounds.y;
                Robot robot;

                robot = new Robot(screen);

                final BufferedImage image = robot.createScreenCapture(part);
                final Rectangle intersections = bounds.intersection(rect);
                g2.drawImage(image, intersections.x - x, intersections.y - y, null);

            }
            g2.dispose();
            return img;
        } catch (final AWTException e) {            
            e.printStackTrace();
        }
        return null;
    }
}
