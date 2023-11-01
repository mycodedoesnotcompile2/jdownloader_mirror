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
package org.appwork.utils.swing.dimensor;

import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowEvent;
import java.util.WeakHashMap;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.locationstore.LocationStorage;
import org.appwork.utils.swing.locationstore.LocationStorageManager;

/**
 * @author Thomas
 *
 */
public class RememberLastDimensor extends AbstractDimensor {

    protected static final WeakHashMap<Window, Dimension> DIFFERENCE = new WeakHashMap<Window, Dimension>();
    protected static final WeakHashMap<Window, Dimension> LAST       = new WeakHashMap<Window, Dimension>();

    public RememberLastDimensor(final String id) {
        super(id);
    }

    public Dimension getDimension(final Window window) {
        final LocationStorage cfg = LocationStorageManager.INSTANCE.get(window, getStorageID(window));
        final Dimension ret;
        if (cfg.isValid()) {
            ret = validate(new Dimension(cfg.getX(), cfg.getY()), window);
        } else {
            ret = null;
        }
        if (CrossSystem.isLinux() && ret != null) {
            LAST.put(window, ret);
        }
        return ret;
    }

    public void onClose(final Window window, final ComponentEvent event) {
        if (window.isShowing()) {
            final String storageID = getStorageID(window);
            final LocationStorage cfg = LocationStorageManager.INSTANCE.get(window, storageID);
            if (CrossSystem.isLinux()) {
                final Dimension preferredSize = window.getPreferredSize();
                final Dimension windowSize = new Dimension(window.getWidth(), window.getHeight());
                final int differenceWidth = preferredSize.width - windowSize.width;
                final int differenceWidthAbs = Math.abs(differenceWidth);
                final int differenceHeight = preferredSize.height - windowSize.height;
                final int differenceHeightAbs = Math.abs(differenceHeight);
                if ((differenceWidthAbs > 0 && differenceWidthAbs <= 16) || (differenceHeightAbs > 0 && differenceHeightAbs <= 16)) {
                    final Dimension difference = new Dimension(differenceWidth, differenceHeight);
                    if (!DIFFERENCE.containsKey(window)) {
                        DIFFERENCE.put(window, difference);
                    }
                    if (false) {
                        LogV3.log(new Exception("difference:" + storageID + "|Stored:" + new Dimension(cfg.getX(), cfg.getY()) + "|Last:" + LAST.get(window) + "|Preferred:" + preferredSize + "|Window:" + windowSize + "|Difference:" + difference));
                    }
                }
                final Dimension difference = DIFFERENCE.get(window);
                if (difference != null) {
                    final Dimension correctedWindowSize = new Dimension(windowSize.width + difference.width, windowSize.height + difference.height);
                    if (false && event != null && event instanceof WindowEvent && ((WindowEvent) event).getID() == WindowEvent.WINDOW_CLOSING) {
                        LogV3.log(new Exception("corrected:" + storageID + "|Stored:" + new Dimension(cfg.getX(), cfg.getY()) + "|Last:" + LAST.get(window) + "|Preferred:" + preferredSize + "|Window:" + windowSize + "|Difference:" + difference + "|Corrected:" + correctedWindowSize));
                    }
                    LocationStorageManager.INSTANCE.onUpdate(window, null, correctedWindowSize, cfg, null);
                    return;
                }
            }
            LocationStorageManager.INSTANCE.onUpdate(window, null, null, cfg, null);
        }
    }
}
