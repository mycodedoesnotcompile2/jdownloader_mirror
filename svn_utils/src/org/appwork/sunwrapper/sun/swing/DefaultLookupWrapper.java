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
package org.appwork.sunwrapper.sun.swing;

import java.awt.Color;
import java.lang.reflect.Method;

import javax.swing.JComponent;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;

import org.appwork.utils.ReflectionUtils;

public class DefaultLookupWrapper {

    private static Method  getColorMethod             = null;
    private static boolean preferNativeGetColorMethod = true;

    private static Method findGetColorMethod(final JComponent comp, final ComponentUI ui, final String key) {
        final Object[] params = new Object[] { comp, ui, key };
        try {
            return ReflectionUtils.findMatchingMethod("sun.swing.DefaultLookup", "getColor", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        return null;
    }

    /**
     * @param extTableHeaderRenderer
     * @param ui
     * @param string
     * @return
     */
    public static Color getColor(JComponent comp, ComponentUI ui, String key) {
        try {
            Method method = getColorMethod;
            if (method == null && preferNativeGetColorMethod) {
                getColorMethod = method = findGetColorMethod(comp, ui, key);
            }
            if (method != null) {
                return (Color) method.invoke(null, new Object[] { comp, ui, key });
            } else {
                preferNativeGetColorMethod = false;
            }
        } catch (final NoClassDefFoundError e) {
            preferNativeGetColorMethod = false;
        } catch (final IllegalAccessError e) {
            preferNativeGetColorMethod = false;
        } catch (final Exception ignore) {
            preferNativeGetColorMethod = false;
        }
        return (Color) UIManager.get(key, comp.getLocale());

    }

    private static Method  getBorderMethod             = null;
    private static boolean preferNativeGetBorderMethod = true;

    private static Method findGetBorderMethod(final JComponent comp, final ComponentUI ui, final String key) {
        final Object[] params = new Object[] { comp, ui, key };
        try {
            return ReflectionUtils.findMatchingMethod("sun.swing.DefaultLookup", "getBorder", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        return null;
    }

    /**
     * @param extTableHeaderRenderer
     * @param ui
     * @param string
     * @return
     */
    public static Border getBorder(JComponent comp, ComponentUI ui, String key) {
        try {
            Method method = getBorderMethod;
            if (method == null && preferNativeGetBorderMethod) {
                getBorderMethod = method = findGetBorderMethod(comp, ui, key);
            }
            if (method != null) {
                return (Border) method.invoke(null, new Object[] { comp, ui, key });
            } else {
                preferNativeGetBorderMethod = false;
            }
        } catch (final NoClassDefFoundError e) {
            preferNativeGetBorderMethod = false;
        } catch (final IllegalAccessError e) {
            preferNativeGetBorderMethod = false;
        } catch (final Exception ignore) {
            preferNativeGetBorderMethod = false;
        }
        return (Border) UIManager.get(key, comp.getLocale());
    }
}
