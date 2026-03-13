/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.experimental.windowsexecuter;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.win32.W32APIOptions;

/**
 * Minimal JNA interface for CreateProcessAsUserW with writable lpCommandLine (Pointer to UTF-16 buffer). Does not extend Advapi32,
 * so there is no overload conflict with the String-based CreateProcessAsUser. Use this for
 * {@link org.appwork.experimental.windowsexecuter.WindowsExecuter#runAsNonElevatedUser}.
 */
public interface Advapi32CreateProcess extends Library {

    Advapi32CreateProcess INSTANCE = Native.load("advapi32", Advapi32CreateProcess.class, W32APIOptions.UNICODE_OPTIONS);

    /**
     * CreateProcessAsUserW. lpCommandLine must be a writable buffer (Pointer to null-terminated UTF-16 string);
     * passing String or char[] can cause ERROR_BAD_LENGTH because the API may modify the buffer.
     */
    boolean CreateProcessAsUser(HANDLE hToken, Pointer lpApplicationName, Pointer lpCommandLine,
            WinBase.SECURITY_ATTRIBUTES lpProcessAttributes, WinBase.SECURITY_ATTRIBUTES lpThreadAttributes,
            boolean bInheritHandles, int dwCreationFlags, Pointer lpEnvironment, String lpCurrentDirectory,
            WinBase.STARTUPINFO lpStartupInfo, WinBase.PROCESS_INFORMATION lpProcessInformation);
}
