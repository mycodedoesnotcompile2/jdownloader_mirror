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
package org.appwork.utils.processes.command;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.JNAHelper;
import org.appwork.exceptions.NotSupportedException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.processes.ProcessBuilderFactory;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinNT.HANDLE;

/**
 * @author Thomas
 * @date 18.10.2018
 *
 */
public class Command {
    protected final ProcessBuilder builder;
    private OutputHandler          lineHandler;
    private Process                process;
    private int                    exitCode = -1;
    public final String[]          commandline;

    public int getExitCode() {
        return exitCode;
    }

    /**
     * @param javaBinary
     * @param string
     * @param absolutePath
     * @param name
     */
    public Command(String... cmds) {
        this.commandline = cmds;
        builder = ProcessBuilderFactory.create(cmds);
        try {
            charset = Charset.forName(ProcessBuilderFactory.getConsoleCodepage());
        } catch (InterruptedException e) {
            charset = Charset.defaultCharset();
            Thread.currentThread().interrupt();
        }
    }

    public Command(List<String> cmds) {
        this(cmds.toArray(new String[] {}));
    }

    public Command(String cmdLine) {
        this(ShellParser.splitCommandString(cmdLine));
    }

    /**
     * @param lineHandler
     */
    public Command setOutputHandler(OutputHandler lineHandler) {
        checkRunning();
        this.lineHandler = lineHandler;
        return this;
    }

    protected final List<AsyncInputStreamHandler> asyncTasks = new ArrayList<AsyncInputStreamHandler>();
    protected Charset                             charset;

    public Command setCharset(Charset charset) {
        checkRunning();
        if (charset == null) {
            throw new IllegalArgumentException("charset is null!");
        } else {
            this.charset = charset;
            return this;
        }
    }

    /**
     * @return
     * @throws IOException
     * @throws InterruptedException
     *
     */
    public Command start(boolean closeOutputStream) throws IOException, InterruptedException {
        this.process = builder.start();
        if (closeOutputStream) {
            process.getOutputStream().close();
        }
        final OutputHandler lh = lineHandler;
        if (lh != null) {
            asyncTasks.add(lh.createAsyncStreamHandler(new ProcessInputStream(process), getCharset()));
            asyncTasks.add(lh.createAsyncStreamHandler(new ProcessErrorStream(process), getCharset()));
        }
        for (final AsyncInputStreamHandler task : asyncTasks) {
            task.start();
        }
        return this;
    }

    public Charset getCharset() {
        return charset;
    }

    /**
     * @return
     * @throws InterruptedException
     * @throws IOException
     *
     */
    public int waitFor() throws InterruptedException, IOException {
        final Process process = getProcess();
        if (process == null) {
            throw new IllegalStateException("Process not yet started!");
        }
        Integer exitCode = null;
        try {
            try {
                exitCode = process.waitFor();
                this.exitCode = exitCode;
                return exitCode;
            } catch (InterruptedException e) {
                for (final AsyncInputStreamHandler task : asyncTasks) {
                    task.interrupt();
                }
                throw e;
            }
        } finally {
            killProcessOnException(process);
            if (lineHandler != null && exitCode != null) {
                lineHandler.onExitCode(exitCode);
            }
            for (final AsyncInputStreamHandler task : asyncTasks) {
                if (exitCode != null) {
                    task.onExit(exitCode);
                }
                task.waitFor();
            }
        }
    }

    protected void killProcessOnException(final Process process) {
        try {
            process.destroy();
        } catch (Exception e) {
        }
    }

    /**
     * @param parentFile
     * @return
     */
    public Command setDirectory(File directory) {
        checkRunning();
        builder.directory(directory);
        return this;
    }

    public Process getProcess() {
        return process;
    }

    /**
     * Returns the process ID (PID) of the process.
     * <p>
     * Implementation strategy:
     * <ul>
     * <li>Java 9+: Uses Process.pid() via reflection (source 1.8 compatible)</li>
     * <li>Pre-Java 9 on Unix/Mac: Uses reflection to access the 'pid' field on UnixProcess</li>
     * <li>Pre-Java 9 on Windows: Uses reflection to get the process handle, then JNA Kernel32.GetProcessId()</li>
     * </ul>
     *
     * @return the process ID as long
     * @throws NotSupportedException
     *             if no method is available to retrieve the PID on the current platform/JVM
     * @throws IllegalStateException
     *             if the process has not been started yet
     */
    public long getPID() throws NotSupportedException {
        final Process proc = getProcess();
        if (proc == null) {
            throw new IllegalStateException("Process not yet started!");
        }
        // Java 9+: use Process.pid() via reflection (source 1.8 compatible)
        if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            try {
                final Long pid = ReflectionUtils.invoke(proc.getClass(), "pid", proc, Long.class);
                if (pid != null) {
                    return pid.longValue();
                }
            } catch (InvocationTargetException e) {
                LogV3.log(e);
            }
        }
        // Pre-Java 9: Use reflection on internal Process implementation
        if (CrossSystem.isUnix() || CrossSystem.isMac()) {
            // Unix/Mac: ProcessImpl has a 'pid' field
            try {
                final Number pid = ReflectionUtils.getFieldValue(proc.getClass(), "pid", proc, Number.class);
                if (pid != null) {
                    return pid.longValue();
                }
            } catch (InvocationTargetException e) {
                LogV3.log(e);
            } catch (NoSuchFieldException e) {
                LogV3.log(e);
            }
        } else if (CrossSystem.isWindows()) {
            // Windows: ProcessImpl has a 'handle' field, use JNA to get PID from handle
            try {
                final Long handle = ReflectionUtils.getFieldValue(proc.getClass(), "handle", proc, Long.class);
                if (handle != null && handle.longValue() != 0) {
                    return getPIDFromWindowsHandle(handle.longValue());
                }
            } catch (NotSupportedException e) {
                throw e;
            } catch (InvocationTargetException e) {
                LogV3.log(e);
            } catch (NoSuchFieldException e) {
                LogV3.log(e);
            }
        }
        throw new NotSupportedException("Cannot retrieve PID: no supported method available for this platform/JVM combination");
    }

    /**
     * Retrieves the PID from a Windows process handle using JNA.
     *
     * @param handle
     *            the Windows process handle
     * @return the process ID
     * @throws NotSupportedException
     *             if JNA is not available
     */
    protected long getPIDFromWindowsHandle(long handle) throws NotSupportedException {
        if (!JNAHelper.isJNAAvailable()) {
            throw new NotSupportedException("JNA is not available to retrieve PID from Windows handle");
        }
        final HANDLE hProcess = new HANDLE(Pointer.createConstant(handle));
        return Kernel32.INSTANCE.GetProcessId(hProcess);
    }

    /**
     *
     */
    protected void checkRunning() {
        if (getProcess() != null) {
            throw new IllegalStateException("Process already running. You have to do this  BEFORE calling #start()");
        }
    }

    /**
     * @param string
     * @param name
     * @return
     */
    public Command putEnvironMent(String key, String value) {
        if (key == null || value == null) {
            LogV3.warning(key + "=" + value + " NullPointer Protection!");
            return this;
        } else {
            checkRunning();
            builder.environment().put(key, value);
            return this;
        }
    }

    /**
     *
     */
    public void destroy() {
        final Process process = getProcess();
        if (process != null) {
            process.destroy();
        }
    }

    /**
     * @return true if the process is alive. If this method returns false, this does not mean, that stdout/err are read completly. You
     *         should call {@link #waitFor()} to wait for streams and ensure that they get closed
     */
    public boolean isAlive() {
        final Process p = getProcess();
        // WARNING: Process.isAlive is 1.8+
        if (p == null) {
            return false;
        } else {
            try {
                p.exitValue();
                return false;
            } catch (IllegalThreadStateException e) {
                return true;
            }
        }
    }

    /**
     * @param commandEnvironmentVariables
     */
    public void putAllEnvironment(Map<String, String> map) {
        for (Entry<String, String> es : map.entrySet()) {
            putEnvironMent(es.getKey(), es.getValue());
        }
    }
}
