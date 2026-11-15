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
package org.appwork.utils.os.windows.execute;

import java.io.Closeable;
import java.util.ArrayList;
import java.util.List;

import org.appwork.JNAHelper;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;

/**
 * Thin wrapper around a Windows Job Object. Does <b>not</b> set {@code JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE}: closing this handle must not
 * terminate member processes (Connect only tracks them temporarily).
 * <p>
 * Child processes of members stay in the job unless breakaway is allowed — which this wrapper does not enable.
 */
public class WindowsJobObject implements Closeable {
    private volatile HANDLE handle;

    private WindowsJobObject(final HANDLE handle) {
        this.handle = handle;
    }

    /**
     * Creates an anonymous job object.
     *
     * @throws IllegalStateException
     *             if not Windows / JNA unavailable
     * @throws Win32Exception
     *             if {@code CreateJobObject} fails
     */
    public static WindowsJobObject create() {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            throw new IllegalStateException("Windows Job Objects require Windows + JNA");
        }
        final HANDLE h = Kernel32Ext.INSTANCE.CreateJobObject(null, null);
        if (h == null || WinBase.INVALID_HANDLE_VALUE.equals(h)) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        LogV3.info("WindowsJobObject: created handle=0x" + Long.toHexString(Pointer.nativeValue(h.getPointer())));
        return new WindowsJobObject(h);
    }

    public HANDLE getHandle() {
        return this.handle;
    }

    public boolean isOpen() {
        final HANDLE h = this.handle;
        return h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h);
    }

    /**
     * Assigns an already-created process to this job. Typically used with a process created {@code CREATE_SUSPENDED} before
     * {@code ResumeThread}.
     */
    public void assignProcess(final HANDLE processHandle) {
        final HANDLE h = requireOpen();
        if (processHandle == null) {
            throw new IllegalArgumentException("processHandle is null");
        }
        if (!Kernel32Ext.INSTANCE.AssignProcessToJobObject(h, processHandle)) {
            final int err = Kernel32.INSTANCE.GetLastError();
            LogV3.info("WindowsJobObject: AssignProcessToJobObject FAILED err=" + err);
            throw new Win32Exception(err);
        }
        LogV3.info("WindowsJobObject: AssignProcessToJobObject OK");
    }

    /**
     * PIDs currently associated with this job (empty if closed or query fails). Never throws.
     */
    public List<Integer> listProcessIds() {
        final ArrayList<Integer> out = new ArrayList<Integer>();
        try {
            final HANDLE h = this.handle;
            if (h == null || WinBase.INVALID_HANDLE_VALUE.equals(h)) {
                return out;
            }
            // NumberOfAssignedProcesses + NumberOfProcessIdsInList + up to N ULONG_PTRs
            final int maxPids = 256;
            final int ptrSize = Native.POINTER_SIZE;
            final int bytes = 8 + maxPids * ptrSize;
            final Memory buf = new Memory(bytes);
            buf.clear();
            final IntByReference retLen = new IntByReference();
            if (!Kernel32Ext.INSTANCE.QueryInformationJobObject(h, Kernel32Ext.JobObjectBasicProcessIdList, buf, bytes, retLen)) {
                final int err = Kernel32.INSTANCE.GetLastError();
                // ERROR_MORE_DATA (234): list larger than buffer — still parse what we got if header present
                if (err != 234) {
                    LogV3.info("WindowsJobObject.listProcessIds failed: " + err);
                    return out;
                }
            }
            final int assigned = buf.getInt(0);
            final int inList = buf.getInt(4);
            final int count = Math.min(Math.min(assigned, inList), maxPids);
            for (int i = 0; i < count; i++) {
                final long pid;
                if (ptrSize == 8) {
                    pid = buf.getLong(8L + (long) i * ptrSize);
                } else {
                    pid = buf.getInt(8 + i * ptrSize) & 0xffffffffL;
                }
                if (pid > 0L && pid <= Integer.MAX_VALUE) {
                    out.add(Integer.valueOf((int) pid));
                }
            }
            if (assigned > maxPids || inList > maxPids) {
                LogV3.info("WindowsJobObject.listProcessIds truncated assigned=" + assigned + " inList=" + inList + " max=" + maxPids);
            }
        } catch (final Throwable e) {
            LogV3.info("WindowsJobObject.listProcessIds exception");
            LogV3.log(e);
        }
        return out;
    }

    private HANDLE requireOpen() {
        final HANDLE h = this.handle;
        if (h == null || WinBase.INVALID_HANDLE_VALUE.equals(h)) {
            throw new IllegalStateException("Job object already closed");
        }
        return h;
    }

    @Override
    public void close() {
        final HANDLE h = this.handle;
        this.handle = null;
        if (h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h)) {
            try {
                LogV3.info("WindowsJobObject: closing handle=0x" + Long.toHexString(Pointer.nativeValue(h.getPointer())) + " (members keep running)");
                Kernel32.INSTANCE.CloseHandle(h);
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }
}
