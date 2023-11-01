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

import java.awt.Container;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowEvent;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.locationstore.LocationStorage;
import org.appwork.utils.swing.locationstore.LocationStorageManager;

public class RememberRelativeLocator extends AbstractLocator {

    public static final String TYPE = "relative";

    private final Window       parent;
    protected AbstractLocator  fallbackLocator;

    /**
     * @param jFrame
     * @param string
     */
    public RememberRelativeLocator(final String id, final Window jFrame) {
        super(id);
        if (id == null) {
            throw new IllegalArgumentException("id ==null");
        }
        parent = jFrame;
        setFallbackLocator(new CenterOfScreenLocator());
    }

    /**
     * @return
     */
    protected AbstractLocator getFallbackLocator() {
        return fallbackLocator;
    }

    @Override
    public Point getLocationOnScreen(final Window frame) {
        try {
            final String storageID = getStorageID(frame);
            final LocationStorage cfg = LocationStorageManager.INSTANCE.get(frame, storageID);
            if (cfg.isValid()) {
                final Point location = new Point(cfg.getX(), cfg.getY());
                Point parentLocationOnScreen = null;
                final Point ret;
                if (RememberAbsoluteLocator.TYPE.equalsIgnoreCase(cfg.getType())) {
                    ret = AbstractLocator.validate(location, frame);
                } else if (RememberRelativeLocator.TYPE.equalsIgnoreCase(cfg.getType())) {
                    // Do a "is on screen check" here
                    final Container actualParent = frame.getParent();
                    if (actualParent != null && actualParent.isShowing()) {
                        parentLocationOnScreen = actualParent.getLocationOnScreen();
                    }
                    final Window parent = getParent();
                    if (parentLocationOnScreen == null && parent != null && parent.isShowing()) {
                        parentLocationOnScreen = parent.getLocationOnScreen();
                    }
                    if (parentLocationOnScreen == null) {
                        ret = getFallbackLocator().getLocationOnScreen(frame);
                    } else {
                        ret = AbstractLocator.validate(new Point(location.x + parentLocationOnScreen.x, location.y + parentLocationOnScreen.y), frame);
                    }
                } else {
                    ret = getFallbackLocator().getLocationOnScreen(frame);
                }
                if (false && CrossSystem.isLinux()) {
                    LogV3.log(new Exception(storageID + "|location:" + location + "|type:" + cfg.getType() + "|parentLocation:" + parentLocationOnScreen + "|ret:" + ret));
                }
                return ret;
            }
        } catch (final Throwable e) {
            if (CrossSystem.isLinux()) {
                LogV3.log(e);
            }
            // frame.getParent() might be null or invisble
            // e.printStackTrace();
        }
        return getFallbackLocator().getLocationOnScreen(frame);
    }

    protected Window getParent() {
        return parent;
    }

    @Override
    public void onClose(final Window window, final ComponentEvent event) {
        try {
            if (window.isShowing()) {
                final String storageID = getStorageID(window);
                final Point locationOnScreen = window.getLocationOnScreen();
                Point parentLocationOnScreen = null;
                Container parentLocationSource = null;
                final Container actualParent = window.getParent();
                if (actualParent != null && actualParent.isShowing()) {
                    parentLocationOnScreen = actualParent.getLocationOnScreen();
                    parentLocationSource = actualParent;
                }
                final Window parent = getParent();
                if (parentLocationOnScreen == null && parent != null && parent.isShowing()) {
                    parentLocationSource = parent;
                    parentLocationOnScreen = parent.getLocationOnScreen();
                }
                if (false && CrossSystem.isLinux()) {
                    if (event != null && event instanceof WindowEvent && ((WindowEvent) event).getID() == WindowEvent.WINDOW_CLOSING) {
                        LogV3.log(new Exception(storageID + "|Parent:" + parentLocationSource + "=" + parentLocationOnScreen + "|Location:" + locationOnScreen));
                    }
                }
                final LocationStorage cfg = LocationStorageManager.INSTANCE.get(window, storageID);
                if (parentLocationOnScreen == null) {
                    // no parent. save absolute
                    LocationStorageManager.INSTANCE.onUpdate(window, locationOnScreen, null, cfg, RememberAbsoluteLocator.TYPE);
                } else {
                    LocationStorageManager.INSTANCE.onUpdate(window, new Point(locationOnScreen.x - parentLocationOnScreen.x, locationOnScreen.y - parentLocationOnScreen.y), null, cfg, RememberRelativeLocator.TYPE);
                }
            }
        } catch (final Throwable e) {
            LogV3.logger(this).exception("Swallowed", e);
            // nothing.... frame.getParent or parent might be invisible
        }
    }

    public void setFallbackLocator(final AbstractLocator fallbackLocator) {
        if (fallbackLocator == null) {
            this.fallbackLocator = new CenterOfScreenLocator();
        } else {
            this.fallbackLocator = fallbackLocator;
        }
    }
}
