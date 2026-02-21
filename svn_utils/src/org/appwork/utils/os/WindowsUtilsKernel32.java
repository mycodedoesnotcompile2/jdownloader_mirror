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
package org.appwork.utils.os;

import org.appwork.jna.windows.Kernel32Ext;

import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * @author thomas
 * @date 20.02.2026
 *
 */
public interface WindowsUtilsKernel32 extends Kernel32Ext {
    final static WindowsUtilsKernel32 INSTANCE         = Native.load("kernel32", WindowsUtilsKernel32.class, W32APIOptions.DEFAULT_OPTIONS);
    /** GetFinalPathNameByHandle: return path with drive letter (e.g. \\?\C:\path). Use this to get C:\ style paths. */
    int                               VOLUME_NAME_DOS  = 0x0;
    /** GetFinalPathNameByHandle: return path with volume GUID (e.g. \\?\Volume{...}). */
    int                               VOLUME_NAME_GUID = 0x1;

    /**
     * Retrieves the final path for a file or directory handle (Vista+).
     *
     * @param hFile
     *            handle to file or directory
     * @param lpszFilePath
     *            buffer for path (wide chars)
     * @param cchFilePath
     *            size of buffer in characters
     * @param dwFlags
     *            {@link #FILE_NAME_NORMALIZED} or {@link #VOLUME_NAME_DOS}
     * @return length in characters excluding null, or 0 on error
     */
    int GetFinalPathNameByHandleW(HANDLE hFile, char[] lpszFilePath, int cchFilePath, int dwFlags);

    /**
     * Retrieves the session ID for the process.
     *
     * @param dwProcessId
     *            process ID
     * @param pSessionId
     *            receives session ID
     * @return true on success
     */
    boolean ProcessIdToSessionId(int dwProcessId, IntByReference pSessionId);

    /**
     * Retrieves the full path of the executable for the process (Vista+).
     *
     * @param hProcess
     *            process handle with PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_QUERY_INFORMATION
     * @param dwFlags
     *            0 for full path
     * @param lpExeName
     *            buffer for path (wide chars)
     * @param lpdwSize
     *            in: buffer size in chars, out: required size
     * @return true on success
     */
    boolean QueryFullProcessImageNameW(HANDLE hProcess, int dwFlags, char[] lpExeName, IntByReference lpdwSize);

    /** File type: disk file (for GetFileType inherited from Kernel32). */
    int FILE_TYPE_DISK = 0x0001;
}
