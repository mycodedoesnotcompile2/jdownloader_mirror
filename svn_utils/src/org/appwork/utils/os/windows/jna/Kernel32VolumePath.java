/**
 * JNA interface for Windows volume path APIs (Kernel32). Used to map volume GUID paths to drive letters.
 */
package org.appwork.utils.os.windows.jna;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * GetVolumePathNamesForVolumeNameW: retrieves drive letters and mount paths for a volume GUID.
 * Volume name format: "\\?\Volume{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}\"
 * Output: multi-string (null-terminated strings, double-null at end), e.g. "C:\"
 */
public interface Kernel32VolumePath extends Kernel32 {
    Kernel32VolumePath INSTANCE = Native.load("kernel32", Kernel32VolumePath.class, W32APIOptions.DEFAULT_OPTIONS);

    /**
     * @param lpszVolumeName volume GUID path, e.g. "\\?\Volume{guid}\" (null-terminated wide string)
     * @param lpszVolumePathNames output buffer (multi-string: null-terminated strings, double-null at end)
     * @param cchBufferLength buffer size in chars
     * @param lpcchReturnLength in/out: required or copied length
     * @return true on success
     */
    boolean GetVolumePathNamesForVolumeNameW(char[] lpszVolumeName, char[] lpszVolumePathNames, int cchBufferLength, IntByReference lpcchReturnLength);
}
