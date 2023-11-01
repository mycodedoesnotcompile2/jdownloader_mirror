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
package org.appwork.utils.processes;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.command.ProcessErrorStream;
import org.appwork.utils.processes.command.ProcessInputStream;
import org.appwork.utils.processes.command.ProcessStreamReader;

public class ProcessBuilderFactory {
    private static String CONSOLE_CODEPAGE = null;

    public static ProcessOutput runCommand(final java.util.List<String> commands) throws IOException, InterruptedException {
        return ProcessBuilderFactory.runCommand(ProcessBuilderFactory.create(commands));
    }

    public static ProcessOutput runCommand(String... commands) throws IOException, InterruptedException {
        return ProcessBuilderFactory.runCommand(ProcessBuilderFactory.create(commands));
    }

    public static ProcessOutput runCommand(final ProcessBuilder pb) throws IOException, InterruptedException {
        final ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        final ByteArrayOutputStream sdtStream = new ByteArrayOutputStream();
        final int exitCode = runCommand(pb, errorStream, sdtStream);
        return new ProcessOutput(exitCode, sdtStream, errorStream, getConsoleCodepage());
    }

    public static int runCommand(ProcessBuilder pb, final OutputStream errorStream, final OutputStream sdtStream) throws IOException, InterruptedException {
        return runCommand(pb, errorStream, sdtStream, null);
    }

    /**
     * s
     *
     * @param create
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    public static int runCommand(ProcessBuilder pb, final OutputStream errorStream, final OutputStream sdtStream, final ProcessHandler osHandler) throws IOException, InterruptedException {
        // System.out.println("Start Process " + pb.command());
        //
        ProcessStreamReader stdReader = null;
        ProcessStreamReader errorReader = null;
        LogV3.info("runCommand:Command=" + pb.command() + "|Directory=" + pb.directory());
        final Process process = pb.start();
        try {
            if (osHandler == null || !osHandler.setProcess(process)) {
                process.getOutputStream().close();
            }
            stdReader = new ProcessStreamReader("Process-Reader-Std:" + pb.command(), process, new ProcessInputStream(process), sdtStream);
            errorReader = new ProcessStreamReader("Process-Reader-Error:" + pb.command(), process, new ProcessErrorStream(process), errorStream);
            if (CrossSystem.isWindows()) {
                stdReader.setPriority(Thread.NORM_PRIORITY + 1);
                errorReader.setPriority(Thread.NORM_PRIORITY + 1);
            }
            stdReader.start();
            errorReader.start();
            // System.out.println("Wait for Process");
            final int returnCode = process.waitFor();
            stdReader.waitFor();
            errorReader.waitFor();
            return returnCode;
        } finally {
            try {
                errorStream.close();
            } catch (Throwable ignore) {
            }
            try {
                sdtStream.close();
            } catch (Throwable ignore) {

            }
            try {
                process.destroy();
            } catch (Throwable e) {
            }
            // make sure the readers end, even in case of an exception
            if (errorReader.isAlive()) {
                errorReader.notifyProcessExited();
                errorReader.interrupt();
            }
            if (stdReader.isAlive()) {
                stdReader.notifyProcessExited();
                stdReader.interrupt();
            }
        }
    }

    public static ProcessBuilder create(final java.util.List<String> splitCommandString) {
        return ProcessBuilderFactory.create(splitCommandString.toArray(new String[] {}));
    }

    public static ProcessBuilder create(final String... tiny) {
        return new ProcessBuilder(ProcessBuilderFactory.escape(tiny));
    }

    public static String[] escape(final String[] input) {
        return escape(input, false);
    }

    public static String[] escape(final String[] input, final boolean forceEscape) {
        if (input != null && (CrossSystem.isWindows() || CrossSystem.isOS2() || forceEscape)) {
            /* The windows processbuilder throws exceptions if a arguments starts with ", but does not end with " or vice versa */
            final String[] ret = new String[input.length];
            final String rawC = "\"";
            final String escapedC = "\\\"";
            for (int index = 0; index < ret.length; index++) {
                // only count non escaped quotations
                final String value = input[index];
                if (value == null) {
                    ret[index] = value;
                } else {
                    final int count = new Regex(value, "((?<!\\\\)" + rawC + ")").count();
                    final boolean rawC_Start = value.startsWith(rawC);
                    final boolean rawC_End = value.endsWith(rawC) && !value.endsWith(escapedC);
                    if (count == 0) {
                        // we have none!
                        ret[index] = value;
                    } else if (rawC_Start && rawC_End) {
                        // prefix and postfix are provided
                        if (count % 2 == 0) {
                            ret[index] = value;
                        } else {
                            // we have to accept our fate and trust the input to be valid :)
                            ret[index] = value;
                        }
                    } else if (count % 2 == 0) {
                        // even count
                        ret[index] = value;
                    } else {
                        // WTF: rest must be odd! corrections required?
                        // note: you can't use replace as you will nuke other valid quoted components.
                        if (rawC_Start && !rawC_End) {
                            ret[index] = value + rawC;
                        } else if (!rawC_Start && rawC_End) {
                            ret[index] = rawC + value;
                        } else {
                            // we have to accept our fate and trust the input to be valid :)
                            ret[index] = value;
                        }
                    }
                }
            }
            return ret;
        } else {
            return input;
        }
    }

    /**
     * @return
     * @throws InterruptedException
     */
    public static String getConsoleCodepage() throws InterruptedException {
        if (StringUtils.isEmpty(CONSOLE_CODEPAGE)) {
            switch (CrossSystem.getOSFamily()) {
            case LINUX:
                return System.getProperty("file.encoding");
            case WINDOWS:
                final ProcessBuilder pb = ProcessBuilderFactory.create("cmd", "/c", "chcp");
                final Process process;
                try {
                    LogV3.info("getConsoleCodepage:Command=" + pb.command() + "|Directory=" + pb.directory());
                    process = pb.start();
                    final Thread th = new Thread() {
                        public void run() {
                            try {
                                process.getOutputStream().close();
                                final BufferedReader f = new BufferedReader(new InputStreamReader(process.getInputStream(), "ASCII"));
                                String line;
                                final StringBuilder ret = new StringBuilder();
                                final String sep = System.getProperty("line.separator");
                                while ((line = f.readLine()) != null) {
                                    if (Thread.interrupted()) {
                                        return;
                                    }
                                    if (ret.length() > 0) {
                                        ret.append(sep);
                                    } else if (line.startsWith("\uFEFF")) {
                                        /*
                                         * Workaround for this bug: http://bugs.sun.com/view_bug.do?bug_id=4508058
                                         * http://bugs.sun.com/view_bug.do?bug_id=6378911
                                         */
                                        line = line.substring(1);
                                    }
                                    ret.append(line);
                                }
                                process.destroy();
                                String result = ret.toString();
                                // /
                                result = new Regex(result, ":\\s*(\\d+)").getMatch(0);
                                if (StringUtils.isNotEmpty(result)) {
                                    final String cp = "cp" + result.trim();
                                    // https://msdn.microsoft.com/en-us/library/dd317756%28VS.85%29.aspx
                                    if ("CP65001".equalsIgnoreCase(cp)) {
                                        CONSOLE_CODEPAGE = "UTF-8";
                                    } else {
                                        CONSOLE_CODEPAGE = cp;
                                    }
                                }
                            } catch (Throwable e) {
                                LogV3.log(e);
                            } finally {
                                try {
                                    process.destroy();
                                } catch (Throwable e) {
                                }
                            }
                        };
                    };
                    th.start();
                    try {
                        th.join();
                    } catch (InterruptedException e) {
                        try {
                            process.destroy();
                        } catch (Throwable e1) {
                        }
                        throw e;
                    }
                } catch (IOException e1) {
                    LogV3.log(e1);
                }
                break;
            default:
                break;
            }
            LogV3.info("Console Codepage: " + CONSOLE_CODEPAGE + "(" + Charset.defaultCharset().displayName() + ")");
            if (StringUtils.isEmpty(CONSOLE_CODEPAGE)) {
                CONSOLE_CODEPAGE = Charset.defaultCharset().displayName();
            }
        }
        return CONSOLE_CODEPAGE;
    }
}
