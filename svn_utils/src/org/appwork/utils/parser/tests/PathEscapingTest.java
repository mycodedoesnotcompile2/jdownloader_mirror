package org.appwork.utils.parser.tests;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;

public class PathEscapingTest extends AWTest {
    public static void main(final String[] args) throws NoSuchMethodException, SecurityException, ClassNotFoundException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        run();
    }

    protected static void runCompareTest(final String[] cmd, final String expect) throws NoSuchMethodException, ClassNotFoundException, IllegalAccessException, InvocationTargetException {
        if (Application.getJavaVersion() < 18312007) {
            logInfoAnyway("WARNING: This Test requires at least JVM 1.8u312");
            return;
        }
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("SKIPPED: WINDOWS Only");
            return;
        }
        try {
            final String result = runProcessImpl(cmd);
            final String ourImpl = runOurImpl(cmd);
            if (StringUtils.equals(expect, result, ourImpl)) {
                System.out.println("SUCCESS " + ourImpl);
            } else {
                if (StringUtils.equals(expect, ourImpl)) {
                    logInfoAnyway("WARNING: Implementation for " + JVMVersion.getJVMVersion() + " differs from what we expect");
                    logInfoAnyway("Input: " + Arrays.asList(cmd));
                    logInfoAnyway("JavaImpl: " + result);
                    logInfoAnyway("ourImpl: " + ourImpl);
                    return;
                }
                System.err.println("JavaImpl: " + result);
                System.err.println("ourImpl: " + ourImpl);
                throw new WTFException("Failed on " + result);
            }
        } catch (RuntimeException e) {
            if (ReflectionUtils.isInstanceOf("java.lang.reflect.InaccessibleObjectException", e)) {
                e.printStackTrace();
            } else {
                throw e;
            }
        }
    }

    protected static String runOurImpl(final String[] cmd) {
        String ourImpl;
        try {
            ourImpl = ShellParser.createCommandLine(Style.WINDOWS, cmd);
        } catch (final IllegalArgumentException e) {
            ourImpl = e.getClass().getName();
        }
        return ourImpl;
    }

    private static String quoteString(final String arg) {
        final StringBuilder argbuf = new StringBuilder(arg.length() + 2);
        return argbuf.append('"').append(arg).append('"').toString();
    }

    protected static String runProcessImpl(String[] cmd) throws NoSuchMethodException, ClassNotFoundException, IllegalAccessException, InvocationTargetException {
        String result;
        try {
            final Method createCommandLine = Class.forName("java.lang.ProcessImpl").getDeclaredMethod("createCommandLine", new Class[] { int.class, String.class, String[].class });
            createCommandLine.setAccessible(true);
            final Method getExecutablePath = Class.forName("java.lang.ProcessImpl").getDeclaredMethod("getExecutablePath", new Class[] { String.class });
            getExecutablePath.setAccessible(true);
            final Method getTokensFromCommand = Class.forName("java.lang.ProcessImpl").getDeclaredMethod("getTokensFromCommand", new Class[] { String.class });
            getTokensFromCommand.setAccessible(true);
            String executablePath;
            try {
                executablePath = (String) getExecutablePath.invoke(null, new Object[] { cmd[0] });
            } catch (final InvocationTargetException e) {
                // Workaround for the calls like
                // Runtime.getRuntime().exec("\"C:\\Program Files\\foo\" bar")
                // No chance to avoid CMD/BAT injection, except to do the work
                // right from the beginning. Otherwise we have too many corner
                // cases from
                // Runtime.getRuntime().exec(String[] cmd [, ...])
                // calls with internal ["] and escape sequences.
                // Restore original command line.
                final StringBuilder join = new StringBuilder();
                // terminal space in command line is ok
                for (final String s : cmd) {
                    join.append(s).append(' ');
                }
                // Parse the command line again.
                cmd = (String[]) getTokensFromCommand.invoke(null, new Object[] { join.toString() });
                executablePath = (String) getExecutablePath.invoke(null, new Object[] { cmd[0] });
            }
            final Object quoted = quoteString(executablePath);
            result = (String) createCommandLine.invoke(null, new Object[] { executablePath.endsWith(".exe") ? 2 : 0, quoted, cmd });
        } catch (final InvocationTargetException e) {
            result = e.getTargetException().getClass().getName();
        }
        return result;
    }

    @Override
    public void runTest() throws Exception {
        // "java.exe" -version "c:\Pa th\\"
        // Maybe Crosscheck with
        // https://github.com/JetBrains/intellij-community/blob/44c3dddac8e82091b2241d85c2303fc938ce1cd5/platform/util/src/com/intellij/execution/CommandLineUtil.java#L19
        runCompareTest(new String[] { "\"java.exe\"", "-version", "\"c:\\Pa th\\\"" }, "\"java.exe\" -version \"\\\"c:\\Pa th\\\\\\\"\"");
        runCompareTest(new String[] { "\"java.exe\"", "-version", "c:\\Pa th\\" }, "\"java.exe\" -version \"c:\\Pa th\\\\\"");
        runCompareTest(new String[] { "\"java.exe\"", "-version", "\"c:\\Path\\\"" }, "\"java.exe\" -version \"\\\"c:\\Path\\\\\\\"\"");
        runCompareTest(new String[] { "\"java.exe\"", "-version", "c:\\Pa th\\" }, "\"java.exe\" -version \"c:\\Pa th\\\\\"");
        runCompareTest(new String[] { "c:\\path with\" spaces\\java.cmd", "-version", "c:\\Path with spaces\\" }, "\"c:\\path\" with spaces\\java.cmd -version c:\\Path with spaces\\");
        runCompareTest(new String[] { "java.exe", "-version", "param \" embedded" }, "\"java.exe\" -version \"param \\\" embedded\"");
        runCompareTest(new String[] { "batch.bat", "-version", "param \" embedded" }, "java.lang.IllegalArgumentException");
        runCompareTest(new String[] { "java.exe", "-version", "c:\\Path\\" }, "\"java.exe\" -version c:\\Path\\");
        runCompareTest(new String[] { "\"java.exe\"", "-version", "\"c:\\Path\\\"" }, "\"java.exe\" -version \"\\\"c:\\Path\\\\\\\"\"");
        runCompareTest(new String[] { "\"java.exe\"", "-version", "\"c:\\Pa th\"" }, "\"java.exe\" -version \"c:\\Pa th\"");
        runCompareTest(new String[] { "java.exe", "-version", "c:\\Path with spaces\\" }, "\"java.exe\" -version \"c:\\Path with spaces\\\\\"");
        runCompareTest(new String[] { "java.exe", "-version", "c:\\Path with\\\" spaces\\" }, "\"java.exe\" -version \"c:\\Path with\\\\\\\" spaces\\\\\"");
        runCompareTest(new String[] { "java.cmd", "-version", "c:\\Path\\" }, "\"java.cmd\" -version c:\\Path\\");
        runCompareTest(new String[] { "java.cmd", "-version", "c:\\Path with spaces\\" }, "\"java.cmd\" -version \"c:\\Path with spaces\\\"");
        runCompareTest(new String[] { "c:\\path with spaces\\java.cmd", "-version", "c:\\Path with spaces\\" }, "\"c:\\path with spaces\\java.cmd\" -version \"c:\\Path with spaces\\\"");
    }
}
