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
package org.appwork.utils.os;

import org.appwork.processes.ProcessInfo;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinNT.HANDLE;

/**
 * ProcessInfo for a process started with a JNA handle (e.g. from {@link WindowsUtils#startElevatedProcess}). Holds the OS handle for stable
 * reference; use {@link #close()} when done or let ProcessHandler close it via terminateForced / waitForExit.
 *
 * @author thomas
 */
public class JNAProcessInfo extends ProcessInfo {
    private final HANDLE     handle;
    private volatile boolean closed;

    /**
     * Package-private: created by {@link WindowsUtils#startElevatedProcess}.
     *
     * @param h
     *            valid process handle (e.g. from ShellExecuteEx with SEE_MASK_NOCLOSEPROCESS)
     */
    public JNAProcessInfo(HANDLE h) {
        super(Kernel32.INSTANCE.GetProcessId(h));
        this.handle = h;
        this.closed = false;
    }

    /**
     * Returns the OS process handle for use by ProcessHandler (wait, terminate). Do not close the handle directly; use {@link #close()} or
     * let the handler close it.
     *
     * @return the process handle (never null)
     */
    public HANDLE getHandle() {
        return handle;
    }

    /**
     * Release the OS handle. Call when done if ProcessHandler did not already use it (e.g. terminateForced / waitForExit close it).
     */
    public void close() {
        if (closed) {
            return;
        }
        synchronized (this) {
            if (closed) {
                return;
            }
            if (handle != null && Pointer.nativeValue(handle.getPointer()) != 0) {
                Kernel32.INSTANCE.CloseHandle(handle);
            }
            closed = true;
        }
    }

    /**
     * @return true if {@link #close()} has been called
     */
    public boolean isClosed() {
        return closed;
    }
}
