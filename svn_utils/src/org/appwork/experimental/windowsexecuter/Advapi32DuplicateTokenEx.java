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
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * Minimal JNA interface for DuplicateTokenEx to create a primary token for CreateProcessAsUser.
 * Separate from Advapi32 to avoid overload ambiguity. Used when running under a specific SID (e.g. active console user).
 */
public interface Advapi32DuplicateTokenEx extends Library {

    Advapi32DuplicateTokenEx INSTANCE = Native.load("advapi32", Advapi32DuplicateTokenEx.class, W32APIOptions.UNICODE_OPTIONS);

    /**
     * Duplicates an access token into a primary token for CreateProcessAsUser.
     *
     * @param hExistingToken      handle with TOKEN_DUPLICATE
     * @param dwDesiredAccess     0 for same as source
     * @param lpTokenAttributes   null
     * @param impersonationLevel SecurityImpersonation (2)
     * @param tokenType          TokenPrimary (1) for CreateProcessAsUser
     * @param phNewToken         receives the new token; caller must close
     * @return true on success
     */
    boolean DuplicateTokenEx(HANDLE hExistingToken, int dwDesiredAccess, Pointer lpTokenAttributes,
            int impersonationLevel, int tokenType, HANDLEByReference phNewToken);
}
