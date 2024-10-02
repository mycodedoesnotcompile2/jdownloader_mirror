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

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ComponentEvent;

import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.utils.swing.SwingUtils;

/**
 * @author Thomas
 *
 */
public class CenterOfScreenLocator extends AbstractLocator {
    /**
     * @param id
     */
    public CenterOfScreenLocator() {
        super(null);
    }

    @Override
    public Point getLocationOnScreen(final Window dialog) {
        final Point mouse = ToolTipController.getMouseLocation();
        if (dialog.getParent() == null || !dialog.getParent().isDisplayable() || !dialog.getParent().isVisible()) {
            Rectangle windowBounds = dialog.getBounds();
            return getCenterLocationByWindowBounds(windowBounds);
        } else if (dialog.getParent() instanceof Frame && ((Frame) dialog.getParent()).getExtendedState() == Frame.ICONIFIED && mouse != null) {
            // dock dialog at bottom right if mainframe is not visible
            final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            final GraphicsDevice[] screens = ge.getScreenDevices();
            for (final GraphicsDevice screen : screens) {
                final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
                screen.getDefaultConfiguration().getDevice();
                final Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(screen.getDefaultConfiguration());
                if (bounds.contains(mouse.getLocation())) {
                    return correct(new Point((int) (bounds.x + bounds.getWidth() - dialog.getWidth() - 20 - insets.right), (int) (bounds.y + bounds.getHeight() - dialog.getHeight() - 20 - insets.bottom)), dialog);
                }
            }
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            return correct(new Point((int) (screenSize.getWidth() - dialog.getWidth() - 20), (int) (screenSize.getHeight() - dialog.getHeight() - 60)), dialog);
        } else {
            final Point ret = SwingUtils.getCenter(dialog.getParent(), dialog);
            return correct(ret, dialog);
        }
    }

    public Point getCenterLocationByWindowBounds(Rectangle windowBounds) {
        GraphicsDevice screen = SwingUtils.getScreenByBounds(windowBounds);
        if (screen != null) {
            Rectangle bounds = SwingUtils.getUsableScreenBounds(screen);
            return correct(new Point((int) (bounds.getWidth() - windowBounds.getWidth()) / 2 + bounds.x, (int) (bounds.getHeight() - windowBounds.getHeight()) / 2 + bounds.y), new Dimension(windowBounds.width, windowBounds.height));
        }
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        return correct(new Point((int) (screenSize.getWidth() - windowBounds.getWidth()) / 2, (int) (screenSize.getHeight() - windowBounds.getHeight()) / 2), new Dimension(windowBounds.width, windowBounds.height));
    }

    @Override
    public void onClose(final Window window, final ComponentEvent event) {
    }
}
