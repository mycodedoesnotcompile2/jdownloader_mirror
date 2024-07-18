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
package org.appwork.loggingv3;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.appwork.utils.DebugMode;
import org.appwork.utils.logging2.LogInterface;

/**
 * @author Thomas
 * @date 21.09.2018
 *
 */
public abstract class AbstractLogger implements LogInterface {
    private static final Method TO_STACKTRACE = getToStackTraceMethod();

    protected StackTraceElement getThrownAt() {
        final Exception e = new Exception();
        StackTraceElement last = filterThrownAt(e);
        return last;
    }

    public StackTraceElement filterThrownAt(final Throwable e) {
        StackTraceElement last = null;
        if (e == null) {
            return null;
        }
        for (StackTraceElement es : e.getStackTrace()) {
            last = es;
            if (filterThrownAtEntries(es)) {
                continue;
            }
            return es;
        }
        return last;
    }

    /**
     * @return
     */
    private static Method getToStackTraceMethod() {
        try {
            final Method ret = Class.forName("org.appwork.utils.Exceptions").getMethod("getStackTrace", new Class[] { Throwable.class });
            if (ret.getReturnType() != String.class) {
                throw new Exception("org.appwork.utils.Exceptions.getStacktrace does not return String");
            } else {
                return ret;
            }
        } catch (Exception e) {
            LogV3.log(e);
        }
        return null;
    }

    /**
     * @param es
     * @return
     */
    protected boolean filterThrownAtEntries(StackTraceElement es) {
        if (es.getClassName().startsWith(DebugMode.class.getName())) {
            if ("logInfoInIDEOnly".equals(es.getMethodName())) {
                return true;
            } else if ("logInIDEOnly".equals(es.getMethodName())) {
                return true;
            } else if ("debugger".equals(es.getMethodName())) {
                return true;
            } else if ("breakIf".equals(es.getMethodName())) {
                return true;
            } else if ("TRIGGER_BUILD_ERROR".equals(es.getMethodName())) {
                return true;
            } else {
                new Exception("Extend Log Formater here to show the correct logging source").printStackTrace();
            }
        }
        if (es.getClassName().startsWith(LogV3.class.getPackage().getName()) || es.getClassName().startsWith("org.slf4j.") || es.getClassName().contains("Logger") || es.getClassName().contains("logging")) {
            return true;
        } else if (es.getMethodName().equals("log") || es.getMethodName().equals("info") || es.getMethodName().equals("messageLogged") || es.getMethodName().equals("fireMessageLoggedEvent") || es.getMethodName().equals("fireMessageLogged") || es.getMethodName().contains("Logger") || es.getMethodName().contains("logger")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void log(Throwable e) {
        info(getStackTrace(e));
    }

    public static String getStackTrace(final Throwable thrown) {
        // we do not use Exceptions.class to reduce coupling
        if (TO_STACKTRACE != null) {
            try {
                return (String) TO_STACKTRACE.invoke(null, thrown);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        }
        if (thrown == null) {
            return "";
        } else {
            final java.io.StringWriter sw = new java.io.StringWriter();
            final java.io.PrintWriter pw = new java.io.PrintWriter(sw);
            thrown.printStackTrace(pw);
            pw.close();
            return sw.toString();
        }
    }

    @Override
    public void fine(String msg) {
        info(msg);
    }

    @Override
    public void finest(String msg) {
        info(msg);
    }

    @Override
    public void severe(String msg) {
        info(msg);
    }

    @Override
    public void finer(String msg) {
        info(msg);
    }

    @Override
    public void warning(String msg) {
        info(msg);
    }

    public void exception(String msg, Throwable e) {
        info(msg + "\r\n" + getStackTrace(e));
    }
}
