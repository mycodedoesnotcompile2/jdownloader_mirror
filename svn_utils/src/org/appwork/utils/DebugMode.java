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
package org.appwork.utils;

import java.net.URL;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.logging2.LogInterface;

/**
 * @author daniel
 * @date 11.12.2018
 *
 */
public class DebugMode {
    public static boolean TRUE_IN_IDE_ELSE_FALSE = !isJared();

    public static final <T> T TRIGGER_BUILD_ERROR(T... forward) {
        logInIDEOnly(new Exception("REMOVE THIS CODE BEFORE BUILD!"));
        if (forward == null || forward.length == 0) {
            return null;
        }
        return forward[0];
    }

    private static final boolean isJared() {
        final String name = Application.class.getName().replaceAll("\\.", "/") + ".class";
        final ClassLoader cll = Application.class.getClassLoader();
        if (cll == null) {
            return true;
        }
        final URL caller = cll.getResource(name);
        /*
         * caller is null in case the resource is not found or not enough rights, in that case we assume its not jared
         */
        if (caller == null) {
            return false;
        } else {
            final boolean ret = caller.toString().matches("jar\\:.*\\.(jar|exe)\\!.*");
            return ret;
        }
    }

    /**
     * @param <T>
     * @param logger
     * @param e
     */
    public static <T extends Throwable> T logInIDEOnly(final LogInterface logger, final T e) {
        if (TRUE_IN_IDE_ELSE_FALSE && logger != null) {
            logger.log(e);
        }
        return e;
    }

    /**
     * @param e
     * @return
     */
    public static <T extends Throwable> boolean throwInIDEElse(T e) throws T {
        if (TRUE_IN_IDE_ELSE_FALSE && e != null) {
            throw e;
        } else {
            return true;
        }
    }

    public static <T extends Throwable> T logInIDEOnly(T e) {
        return logInIDEOnly(LogV3.defaultLogger(), e);
    }

    public static void logInfoInIDEOnly(String e) {
        if (TRUE_IN_IDE_ELSE_FALSE && e != null) {
            LogV3.info(e);
        }
    }

    /**
     * IDE Only! call this method whenever you want to stop. The method throws an exception if no breakpoint was found. "SPECIAL"
     *
     */
    public static void debugger(boolean breakIf, Object... context) {
        if (!TRUE_IN_IDE_ELSE_FALSE || !breakIf || "true".equalsIgnoreCase(System.getProperty("NO_DEBUG"))) {
            return;
        }
        long t = Time.systemIndependentCurrentJVMTimeMillis();
        System.err.println("BREAKPOINT");
        if (Time.systemIndependentCurrentJVMTimeMillis() - t < 20) {
            throw new WTFException("A Breakpoint 2 lines before was expected. Please set a breakpoint");
        }
    }

    /**
     * @param string
     */
    public static void debugger(String reason) {
        debugger(true, reason);
    }

    /**
     *
     */
    public static void debugger() {
        debugger(true);
    }

    /**
     * @param e
     */
    public static void debugger(Exception e) {
        debugger(e != null, e);
    }

    /**
     * @param b
     */
    public static void breakIf(boolean breakIf, Object... context) {
        debugger(breakIf, context);
    }

    /**
     * @param b
     * @param types
     */
    public static void breakIfNot(boolean breakIfNot, Object... context) {
        breakIf(!breakIfNot, context);
    }
}
