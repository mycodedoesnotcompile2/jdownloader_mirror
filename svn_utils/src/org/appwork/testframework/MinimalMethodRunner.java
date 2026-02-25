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
import java.io.FileWriter;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.appwork.utils.Application;

/**
 * Minimal runner that executes a no-arg method on a class via reflection. Has no external dependencies and is compiled for Java 1.6 so it
 * can run in older JREs when {@link JREMethodRunner} is not compatible.
 * <p>
 * Arguments:
 * <ul>
 * <li>args[0] = Fully qualified class name</li>
 * <li>args[1] = Method name (static or instance, no parameters)</li>
 * <li>args[2] = Path to exception output file</li>
 * </ul>
 * <p>
 * Exit codes:
 * <ul>
 * <li>0 = Success</li>
 * <li>1 = Method threw an exception (InvocationTargetException)</li>
 * <li>2 = Class/method not found or other error</li>
 * <li>3 = Invalid arguments (args.length &lt; 3)</li>
 * </ul>
 *
 * @author Thomas
 * @date 24.02.2026
 */
public class MinimalMethodRunner {
    public static void main(final String[] args) {
        if (args == null || args.length < 3) {
            System.exit(3);
            return;
        }
        Application.setApplication(".MinimalMethodRunner");
        final String className = args[0];
        final String methodName = args[1];
        final String exceptionFilePath = args[2];
        final File exceptionFile = new File(exceptionFilePath);
        try {
            final Class<?> clazz = Class.forName(className);
            Method method = null;
            final Method[] methods = clazz.getDeclaredMethods();
            for (int i = 0; i < methods.length; i++) {
                final Method m = methods[i];
                if (m.getName().equals(methodName)) {
                    method = m;
                    break;
                }
            }
            if (method == null) {
                throw new NoSuchMethodException(methodName);
            }
            method.setAccessible(true);
            Object instance = null;
            if (!Modifier.isStatic(method.getModifiers())) {
                instance = clazz.getDeclaredConstructor().newInstance();
            }
            method.invoke(instance);
            System.exit(0);
        } catch (final InvocationTargetException e) {
            final Throwable cause = e.getCause() != null ? e.getCause() : e;
            cause.printStackTrace(System.err);
            writeException(exceptionFile, cause);
            System.exit(1);
        } catch (final Throwable e) {
            e.printStackTrace(System.err);
            writeException(exceptionFile, e);
            System.exit(2);
        }
    }

    private static void writeException(final File f, final Throwable e) {
        try {
            final PrintWriter pw = new PrintWriter(new FileWriter(f));
            pw.println(e.getClass().getName());
            pw.println(e.getMessage() != null ? e.getMessage() : "");
            e.printStackTrace(pw);
            pw.close();
        } catch (final Exception ignore) {
            // ignore
        }
    }
}
