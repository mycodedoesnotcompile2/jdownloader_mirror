/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import org.appwork.storage.TypeRef;
import org.appwork.testframework.executer.AdminExecuter;

/**
 * Static methods used as targets for {@link AdminExecuter#runAsAdmin(Class, String, TypeRef, Object...)} and
 * {@link AdminExecuter#runAsLocalSystem(Class, String, TypeRef, Object...)}. Return values must be serializable (Integer, String, etc.).
 */
public final class RunAsStaticMethodTarget {
    /**
     * Adds two ints. Used for runAsAdmin(Class, String, Object...) with varargs (Integer, Integer).
     */
    public static Integer add(int a, int b) {
        return Integer.valueOf(a + b);
    }

    /**
     * Returns the string. Used for runAsAdmin(Class, String, Class[], Object[]) with explicit types.
     */
    public static String echo(String s) {
        return s == null ? "" : s;
    }

    /**
     * Returns a marker string. Used to verify the static method ran in the elevated/SYSTEM process.
     */
    public static String marker() {
        return "RUN_AS_STATIC_METHOD_MARKER";
    }

    private RunAsStaticMethodTarget() {
    }
}
