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

import java.awt.Component;
import java.awt.FontMetrics;
import java.lang.reflect.Method;

import javax.swing.JComponent;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;

import org.appwork.utils.ReflectionUtils;

public class SwingUtilities2Wrapper {
    private static String internalClipStringIfNecessary(final JComponent component, final FontMetrics fontMetrics, final String str, final int availableWidth) {
        if (str == null || str.length() == 0 || str.trim().length() == 0) {
            return str;
        } else if (availableWidth == 0) {
            return "";
        }
        boolean normalized = false;
        final char[] charArray = str.toCharArray();
        int length = charArray.length;
        while (length > 0) {
            final int stringWidth = fontMetrics.charsWidth(charArray, 0, length);
            if (stringWidth > availableWidth) {
                if (!normalized) {
                    normalized = true;
                    float percent = 100f / availableWidth * stringWidth;
                    length = Math.min(length, ((int) ((length / percent) * 100f) + 5));
                } else {
                    length--;
                }
            } else {
                if (length == charArray.length) {
                    return str;
                } else {
                    return new String(charArray, 0, length);
                }
            }
        }
        return "";
    }

    private static Method  clipStringIfNecessaryMethod   = null;
    private static boolean preferNativeClipStringMethod  = true;
    private static Method  stringWithMethod;
    private static boolean preferNativeStringWidthMethod = true;

    private static Method findStringWidthMethod(final JComponent component, final FontMetrics fontMetrics, final String str) {
        final Object[] params = new Object[] { component, fontMetrics, str };
        try {
            return ReflectionUtils.findMatchingMethod("sun.swing.SwingUtilities2", "stringWidth", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        try {
            // since JDK9
            return ReflectionUtils.findMatchingMethod("javax.swing.plaf.basic.BasicGraphicsUtils", "getStringWidth", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        return null;
    }

    /**
     * @return
     */
    private static Method findClipStringMethod(final JComponent component, final FontMetrics fontMetrics, final String str, final int availableWidth) {
        final Object[] params = new Object[] { component, fontMetrics, str, availableWidth };
        try {
            return ReflectionUtils.findMatchingMethod("sun.swing.SwingUtilities2", "clipStringIfNecessary", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        try {
            // since JDK9
            return ReflectionUtils.findMatchingMethod("javax.swing.plaf.basic.BasicGraphicsUtils", "getClippedString", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        return null;
    }

    /**
     * @param rendererField
     * @param fontMetrics
     * @param str
     * @param i
     * @return
     */
    public static String clipStringIfNecessary(final JComponent component, final FontMetrics fontMetrics, final String str, final int availableWidth) {
        try {
            if (fontMetrics == null) {
                throw new IllegalArgumentException("Fontmetrics may not be null");
            }
            Method method = clipStringIfNecessaryMethod;
            if (method == null && preferNativeClipStringMethod) {
                clipStringIfNecessaryMethod = method = findClipStringMethod(component, fontMetrics, str, availableWidth);
            }
            if (method != null) {
                String ret = (String) method.invoke(null, new Object[] { component, fontMetrics, str, availableWidth });
                // if there is not enought space for ...
                while (ret.length() > 0 && ret.endsWith(".") && stringWidth(component, fontMetrics, ret) > availableWidth) {
                    ret = ret.substring(0, ret.length() - 1);
                }
                return ret;
            } else {
                preferNativeClipStringMethod = false;
                return internalClipStringIfNecessary(component, fontMetrics, str, availableWidth);
            }
        } catch (final NoClassDefFoundError e) {
            preferNativeClipStringMethod = false;
        } catch (final IllegalAccessError e) {
            preferNativeClipStringMethod = false;
        } catch (final Exception ignore) {
            preferNativeClipStringMethod = false;
        }
        return str;
    }

    /**
     * @param component
     * @param fontMetrics
     * @param ret
     * @return
     */
    public static int stringWidth(JComponent component, FontMetrics fontMetrics, String str) {
        try {
            Method method = stringWithMethod;
            if (method == null && preferNativeStringWidthMethod) {
                stringWithMethod = method = findStringWidthMethod(component, fontMetrics, str);
            }
            if (method != null) {
                return ((Number) method.invoke(null, new Object[] { component, fontMetrics, str })).intValue();
                // if there is not enought space for ...
            } else {
                preferNativeStringWidthMethod = false;
            }
        } catch (final NoClassDefFoundError e) {
            preferNativeStringWidthMethod = false;
        } catch (final IllegalAccessError e) {
            preferNativeStringWidthMethod = false;
        } catch (final Exception ignore) {
            preferNativeStringWidthMethod = false;
        }
        return fontMetrics.stringWidth(str);
    }

    private static Method  skipClickCountMethodMethod       = null;
    private static boolean preferNativeSkipClickCountMethod = true;

    private static Method findSkipClickCountMethod(final Component dispatchComponent, final int count) {
        final Object[] params = new Object[] { dispatchComponent, count };
        try {
            return ReflectionUtils.findMatchingMethod("sun.swing.SwingUtilities2", "setSkipClickCount", params);
        } catch (NoSuchMethodException ignore) {
        } catch (ClassNotFoundException ignore) {
        }
        return null;
    }

    // must be instanceof StringBuilder! see SwingUtilities2
    private static final StringBuilder SKIP_CLICK_COUNT = new StringBuilder("skipClickCount");

    /**
     * @param dispatchComponent
     * @param i
     */
    public static void setSkipClickCount(final Component dispatchComponent, final int count) {
        try {
            Method method = skipClickCountMethodMethod;
            if (method == null && preferNativeSkipClickCountMethod) {
                skipClickCountMethodMethod = method = findSkipClickCountMethod(dispatchComponent, count);
            }
            if (method != null) {
                method.invoke(null, new Object[] { dispatchComponent, count });
            } else {
                preferNativeSkipClickCountMethod = false;
                if (dispatchComponent instanceof JTextComponent && ((JTextComponent) dispatchComponent).getCaret() instanceof DefaultCaret) {
                    ((JTextComponent) dispatchComponent).putClientProperty(SKIP_CLICK_COUNT, count);
                }
            }
        } catch (final NoClassDefFoundError e) {
            preferNativeSkipClickCountMethod = false;
        } catch (final IllegalAccessError e) {
            preferNativeSkipClickCountMethod = false;
        } catch (final Exception ignore) {
            preferNativeSkipClickCountMethod = false;
        }
    }
}
