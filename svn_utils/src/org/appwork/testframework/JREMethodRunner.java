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
package org.appwork.testframework;

import java.io.File;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.IO;

/**
 * Runner class that executes a method on a class in a child JRE process. This class is used internally by
 * {@link TestJREProvider#runInJRE(org.appwork.utils.JavaVersion, Class, String, Object...)}.
 * <p>
 * The runner first looks for a static method. If not found, it looks for an instance method and creates an instance of the class using the
 * default constructor.
 * <p>
 * Arguments:
 * <ul>
 * <li>args[0] = Fully qualified class name</li>
 * <li>args[1] = Method name (static or instance)</li>
 * <li>args[2] = Path to params file (JSON serialized Object[]), optional</li>
 * <li>args[3] = Path to exception output file</li>
 * </ul>
 * <p>
 * Exit codes:
 * <ul>
 * <li>0 = Success</li>
 * <li>1 = Method threw an exception</li>
 * <li>2 = Failed to load class or find method</li>
 * <li>3 = Invalid arguments</li>
 * <li>4 = Failed to deserialize parameters</li>
 * <li>5 = Failed to instantiate class</li>
 * </ul>
 *
 * @author Thomas
 * @date 20.02.2026
 */
public class JREMethodRunner {
    public static final int EXIT_SUCCESS            = 0;
    public static final int EXIT_METHOD_EXCEPTION   = 1;
    public static final int EXIT_CLASS_NOT_FOUND    = 2;
    public static final int EXIT_INVALID_ARGS       = 3;
    public static final int EXIT_PARAM_DESER_FAIL   = 4;
    public static final int EXIT_INSTANTIATION_FAIL = 5;

    public static void main(final String[] args) {
        if (args.length < 4) {
            System.err.println("JREMethodRunner: Missing arguments. Expected: className methodName paramsFile exceptionFile");
            System.exit(EXIT_INVALID_ARGS);
            return;
        }
        Application.setApplication(".JREMethodRunner");
        final String className = args[0];
        final String methodName = args[1];
        final String paramsFilePath = args[2];
        final String exceptionFilePath = args[3];
        final File exceptionFile = new File(exceptionFilePath);
        Class<?> clazz;
        try {
            clazz = Class.forName(className);
        } catch (final ClassNotFoundException e) {
            System.err.println("Class not found: " + className);
            e.printStackTrace(System.err);
            writeExceptionToFile(exceptionFile, e);
            System.exit(EXIT_CLASS_NOT_FOUND);
            return;
        }
        Object[] params = new Object[0];
        if (paramsFilePath != null && !paramsFilePath.isEmpty() && !"null".equals(paramsFilePath)) {
            final File paramsFile = new File(paramsFilePath);
            if (paramsFile.exists()) {
                try {
                    final String json = IO.readFileToString(paramsFile);
                    params = Deser.fromString(json, new TypeRef<Object[]>() {
                    });
                    if (params == null) {
                        params = new Object[0];
                    }
                } catch (final Exception e) {
                    System.err.println("Failed to deserialize parameters");
                    e.printStackTrace(System.err);
                    writeExceptionToFile(exceptionFile, e);
                    System.exit(EXIT_PARAM_DESER_FAIL);
                    return;
                }
            }
        }
        MethodInfo methodInfo = null;
        try {
            methodInfo = findMethod(clazz, methodName, params.length);
        } catch (final NoSuchMethodException e) {
            System.err.println("Method not found: " + methodName + " with " + params.length + " parameters");
            e.printStackTrace(System.err);
            writeExceptionToFile(exceptionFile, e);
            System.exit(EXIT_CLASS_NOT_FOUND);
            return;
        }
        Object instance = null;
        if (!methodInfo.isStatic) {
            try {
                instance = clazz.getDeclaredConstructor().newInstance();
            } catch (final Exception e) {
                System.err.println("Failed to instantiate class: " + className);
                e.printStackTrace(System.err);
                writeExceptionToFile(exceptionFile, e);
                System.exit(EXIT_INSTANTIATION_FAIL);
                return;
            }
        }
        try {
            methodInfo.method.setAccessible(true);
            final Object[] convertedParams = convertParams(params, methodInfo.method.getParameterTypes());
            methodInfo.method.invoke(instance, convertedParams);
            System.exit(EXIT_SUCCESS);
        } catch (final InvocationTargetException e) {
            final Throwable cause = e.getCause() != null ? e.getCause() : e;
            cause.printStackTrace(System.err);
            writeExceptionToFile(exceptionFile, cause);
            System.exit(EXIT_METHOD_EXCEPTION);
        } catch (final Throwable e) {
            e.printStackTrace(System.err);
            writeExceptionToFile(exceptionFile, e);
            System.exit(EXIT_METHOD_EXCEPTION);
        }
    }

    /**
     * Holds method info including whether it's static.
     */
    private static class MethodInfo {
        final Method  method;
        final boolean isStatic;

        MethodInfo(final Method method, final boolean isStatic) {
            this.method = method;
            this.isStatic = isStatic;
        }
    }

    /**
     * Finds a method with the given name and parameter count. First searches for static methods, then instance methods.
     */
    private static MethodInfo findMethod(final Class<?> clazz, final String methodName, final int paramCount) throws NoSuchMethodException {
        final Method foundStaticMethod = findMethodByType(clazz, methodName, paramCount, true);
        if (foundStaticMethod != null) {
            return new MethodInfo(foundStaticMethod, true);
        }
        final Method foundInstanceMethod = findMethodByType(clazz, methodName, paramCount, false);
        if (foundInstanceMethod != null) {
            return new MethodInfo(foundInstanceMethod, false);
        }
        throw new NoSuchMethodException(clazz.getName() + "." + methodName + " with " + paramCount + " parameters");
    }

    /**
     * Finds a method matching the given name and parameter count.
     */
    private static Method findMethodByType(final Class<?> clazz, final String methodName, final int paramCount, final boolean mustBeStatic) {
        for (final Method method : clazz.getDeclaredMethods()) {
            if (!method.getName().equals(methodName)) {
                continue;
            }
            final boolean isStatic = Modifier.isStatic(method.getModifiers());
            if (mustBeStatic != isStatic) {
                continue;
            }
            if (method.getParameterTypes().length != paramCount) {
                continue;
            }
            return method;
        }
        return null;
    }

    /**
     * Converts parameters to match the expected method parameter types using Deser.convert().
     */
    private static Object[] convertParams(final Object[] params, final Class<?>[] targetTypes) {
        if (params.length == 0) {
            return params;
        }
        final Object[] converted = new Object[params.length];
        for (int i = 0; i < params.length; i++) {
            converted[i] = convertParam(params[i], targetTypes[i]);
        }
        return converted;
    }

    /**
     * Converts a single parameter to the target type using Deser.convert().
     */
    private static Object convertParam(final Object value, final Class<?> targetType) {
        if (value == null) {
            return null;
        }
        if (targetType.isAssignableFrom(value.getClass())) {
            return value;
        }
        try {
            return Deser.convert(value, new TypeRef<Object>(targetType) {
            });
        } catch (final Exception e) {
            return value;
        }
    }

    /**
     * Writes exception information to a file for retrieval by the parent process.
     */
    private static void writeExceptionToFile(final File exceptionFile, final Throwable e) {
        try {
            final FileOutputStream fos = new FileOutputStream(exceptionFile);
            try {
                final ObjectOutputStream oos = new ObjectOutputStream(fos);
                try {
                    oos.writeObject(new JRETaskRunner.SerializedException(e));
                } finally {
                    oos.close();
                }
            } finally {
                fos.close();
            }
        } catch (final Exception writeError) {
            System.err.println("JREMethodRunner: Failed to write exception file: " + writeError.getMessage());
        }
    }
}
