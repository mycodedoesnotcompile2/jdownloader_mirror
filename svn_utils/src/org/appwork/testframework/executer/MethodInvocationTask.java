/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 * ====================================================================================================================================================
 *         (License header abbreviated; see project license.)
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
