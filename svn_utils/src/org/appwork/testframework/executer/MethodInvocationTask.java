/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.testframework.executer;

import java.io.Serializable;
import java.lang.reflect.Method;

/**
 * Used by {@link AdminExecuter#runAsAdmin(Class, String, Class[], Object[], org.appwork.storage.TypeRef)}
 * and {@link AdminExecuter#runAsLocalSystem(Class, String, Class[], Object[], org.appwork.storage.TypeRef)}. The method must be static and its parameters and return
 * value must be serializable (or null).
 */
public final class MethodInvocationTask implements ElevatedTestTask {
    private static final long serialVersionUID = 1L;

    private final String   className;
    private final String   methodName;
    private final String[] paramTypeNames;
    private final Object[] args;

    /**
     * @param className
     *            fully qualified class name
     * @param methodName
     *            static method name
     * @param paramTypeNames
     *            parameter type names (e.g. "int", "java.lang.String")
     * @param args
     *            arguments (must match param types; serializable)
     */
    public MethodInvocationTask(String className, String methodName, String[] paramTypeNames, Object[] args) {
        this.className = className;
        this.methodName = methodName;
        this.paramTypeNames = paramTypeNames != null ? paramTypeNames : new String[0];
        this.args = args != null ? args : new Object[0];
    }

    @Override
    public Serializable run() throws Exception {
        Class<?> clazz = Class.forName(className);
        Class<?>[] paramTypes = new Class<?>[paramTypeNames.length];
        for (int i = 0; i < paramTypeNames.length; i++) {
            paramTypes[i] = loadClass(paramTypeNames[i]);
        }
        Method method = clazz.getMethod(methodName, paramTypes);
        Object result = method.invoke(null, args);
        return (result instanceof Serializable) ? (Serializable) result : null;
    }

    private static Class<?> loadClass(String name) throws ClassNotFoundException {
        if ("int".equals(name)) {
            return Integer.TYPE;
        }
        if ("long".equals(name)) {
            return Long.TYPE;
        }
        if ("boolean".equals(name)) {
            return Boolean.TYPE;
        }
        if ("byte".equals(name)) {
            return Byte.TYPE;
        }
        if ("short".equals(name)) {
            return Short.TYPE;
        }
        if ("char".equals(name)) {
            return Character.TYPE;
        }
        if ("double".equals(name)) {
            return Double.TYPE;
        }
        if ("float".equals(name)) {
            return Float.TYPE;
        }
        return Class.forName(name);
    }
}
