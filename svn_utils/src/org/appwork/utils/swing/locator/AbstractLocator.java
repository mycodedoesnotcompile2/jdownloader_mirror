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
package org.appwork.utils.swing.locator;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.SwingUtils;

/**
 * @author Thomas
 *
 */
public abstract class AbstractLocator implements Locator {
    protected String id;

    /**
     *
     */
    public AbstractLocator() {
        id = null;
    }

    public AbstractLocator(String id) {
        this.id = id;
    }

    /**
     * @param frame
     * @return
     */
    protected String getStorageID(Window frame) {
        return getClass().getName().replaceAll("(^.*\\.|[\\d\\$]+$)", "") + "-" + getID(frame);
    }

    public String getID() {
        return id;
    }

    protected String getID(final Window window) {
        if (id == null) {
            String title = "";
            if (window instanceof Dialog) {
                title = ((Dialog) window).getTitle();
            }
            if (StringUtils.isEmpty(title) && window instanceof Frame) {
                title = ((Frame) window).getTitle();
            }
            id = window.getClass().getName().replaceAll("[\\d\\$]+$", "") + "-" + title;
        }
        return id;
    }

    public static Point correct(final Point point, final Window d) {
        final Dimension prefSize = d.getSize();
        return correct(point, prefSize);
    }

    public static Point correct(final Point point, final Dimension prefSize) {
        final Rectangle preferedRect = new Rectangle(point.x, point.y, prefSize.width, prefSize.height);
        GraphicsDevice biggestInteresctionScreen = SwingUtils.getScreenByBounds(preferedRect);
        if (biggestInteresctionScreen == null) {
            final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            biggestInteresctionScreen = ge.getDefaultScreenDevice();
            if (biggestInteresctionScreen == null) {
                // maybe just a java 10 ea bug
                System.out.println("GraphicsEnvironment.getDefaultScreenDevice returned null?!");
                return preferedRect.getLocation();
            }
        }
        final Rectangle bounds = SwingUtils.getUsableScreenBounds(biggestInteresctionScreen);
        switch (CrossSystem.getOS()) {
        case WINDOWS_10:
        case WINDOWS_8:
            /*
             * IT seems that for windows 10 (i guess 8 as well) windows have a huge border (Probably the shadow).<br> That results in a
             * strange behaviour:<br> screen specs: with bounds 0 0 1920 1080 the window is in the top left corner. The window's content
             * panes x location is 0, but the windows x location is -8 <br> this workaround increases the usably space for 16 pixel to each
             * size. this is not correct, and windows might not be correced 100%, but ar least correctly positioned windows will not get
             * corrected badly
             */
            bounds.x -= 16;
            bounds.y -= 16;
            bounds.width += 32;
            bounds.height += 32;
            break;
        default:
            break;
        }
        if (preferedRect.x + preferedRect.width > bounds.x + bounds.width) {
            preferedRect.x = bounds.x + bounds.width - preferedRect.width;
        }
        if (preferedRect.y + preferedRect.height > bounds.y + bounds.height) {
            preferedRect.y = bounds.y + bounds.height - preferedRect.height;
        }
        if (preferedRect.x < bounds.x) {
            preferedRect.x = bounds.x;
        }
        if (preferedRect.y < bounds.y) {
            preferedRect.y = bounds.y;
        }
        return preferedRect.getLocation();
    }

    /**
     * @param point
     * @param dialog
     * @return
     */
    public static Point validate(Point point, final Window dialog) {
        point = AbstractLocator.correct(point, dialog);
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice[] screens = ge.getScreenDevices();       
        for (final GraphicsDevice screen : screens) {
            final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
            if (bounds.contains(point)) {
                return point;     
            }
        }
        return new CenterOfScreenLocator().getLocationOnScreen(dialog);
    }
}
