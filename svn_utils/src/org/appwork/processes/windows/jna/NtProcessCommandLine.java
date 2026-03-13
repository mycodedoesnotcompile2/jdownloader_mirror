/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.processes.windows.jna;

import java.io.UnsupportedEncodingException;

import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinNT;

/**
 * Gets the command line of a Windows process via NtQueryInformationProcess with ProcessCommandLineInformation (class 60).
 * Available on Windows 8.1 and later; requires only PROCESS_QUERY_LIMITED_INFORMATION (no WMI, no PROCESS_VM_READ).
 * Returns null on older Windows or on failure so callers can fall back to WMI.
 *
 * @author thomas
 * @date 08.03.2026
 */
public final class NtProcessCommandLine {

    /** ProcessCommandLineInformation – command line as UNICODE_STRING in the same buffer (Windows 8.1+). */
    private static final int PROCESS_COMMAND_LINE_INFORMATION = 60;

    private static final int BUFFER_SIZE = 32 * 1024;

    /** NTSTATUS: success. */
    private static final int STATUS_SUCCESS = 0;

    private NtProcessCommandLine() {
    }

    /**
     * Gets the command line of the process with the given PID using NtQueryInformationProcess(ProcessCommandLineInformation).
     * Uses only PROCESS_QUERY_LIMITED_INFORMATION. Returns null if not supported (e.g. Windows &lt; 8.1), process not found,
     * or on any error; callers should fall back to WMI in that case.
     *
     * @param pid
     *            process ID
     * @return command line string, or null
     */
    public static String getCommandLine(int pid) {
        if (!CrossSystem.getOS().isMinimum(CrossSystem.OperatingSystem.WINDOWS_8_1)) {
            return null;
        }
        WinNT.HANDLE handle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
        if (handle == null || WinBase.INVALID_HANDLE_VALUE.equals(handle)) {
            return null;
        }
        try {
            Memory buffer = new Memory(BUFFER_SIZE);
            IntByReference returnLength = new IntByReference();
            int status = NtDllProcessQuery.INSTANCE.NtQueryInformationProcess(handle, PROCESS_COMMAND_LINE_INFORMATION, buffer, BUFFER_SIZE, returnLength);
            if (status != STATUS_SUCCESS) {
                return null;
            }
            int lengthBytes = buffer.getShort(0) & 0xFFFF;
            if (lengthBytes <= 0 || lengthBytes > BUFFER_SIZE - 32) {
                return null;
            }
            int stringOffset = Native.POINTER_SIZE == 8 ? 16 : 8;
            byte[] raw = buffer.getByteArray(stringOffset, lengthBytes);
            try {
                return new String(raw, "UTF-16LE").trim();
            } catch (UnsupportedEncodingException e) {
                return null;
            }
        } finally {
            Kernel32.INSTANCE.CloseHandle(handle);
        }
    }
}
