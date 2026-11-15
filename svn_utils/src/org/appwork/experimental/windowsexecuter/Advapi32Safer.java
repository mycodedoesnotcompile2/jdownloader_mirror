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
package org.appwork.experimental.windowsexecuter;

import java.util.Arrays;
import java.util.List;

import org.appwork.jna.windows.JNAOptions;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.WinNT.SID_AND_ATTRIBUTES;

/**
 * Advapi32 extensions for running a process as non-elevated (WinSafer API and token integrity). Used by
 * {@link org.appwork.experimental.windowsexecuter.WindowsExecuter#runAsNonElevatedUser(ExecuteOptions)}. For CreateProcessAsUser with
 * writable lpCommandLine use {@link Advapi32CreateProcess} to avoid overload conflicts.
 */
public interface Advapi32Safer extends Advapi32 {
    Advapi32Safer INSTANCE                 = Native.load("advapi32", Advapi32Safer.class, JNAOptions.SYSTEM_DLLS_ONLY);
    /** User scope (restriction applies to current user). */
    int           SAFER_SCOPEID_USER       = 2;
    /** Normal user level (non-elevated, medium integrity). */
    int           SAFER_LEVELID_NORMALUSER = 0x20000;
    /** Open existing level. */
    int           SAFER_LEVEL_OPEN         = 1;
    /** Integrity level: medium (non-elevated). SID S-1-16-8192. */
    String        INTEGRITY_SID_MEDIUM     = "S-1-16-8192";
    /** Label attribute for integrity. */
    int           SE_GROUP_INTEGRITY       = 0x00000020;
    /** Token type: primary token (for CreateProcessAsUser). */
    int           TOKEN_TYPE_PRIMARY       = 1;
    /** Token type: impersonation token. */
    int           TOKEN_TYPE_IMPERSONATION = 2;

    /**
     * Creates a SAFER_LEVEL_HANDLE that defines a security restriction level.
     *
     * @param dwScopeId
     *            SAFER_SCOPEID_USER or SAFER_SCOPEID_MACHINE
     * @param dwLevelId
     *            SAFER_LEVELID_NORMALUSER etc.
     * @param openFlags
     *            SAFER_LEVEL_OPEN
     * @param pLevelHandle
     *            output level handle
     * @param pReserved
     *            reserved, pass null
     * @return true on success
     */
    boolean SaferCreateLevel(int dwScopeId, int dwLevelId, int openFlags, HANDLEByReference pLevelHandle, Pointer pReserved);

    /**
     * Creates a restricted token from a Safer level handle. If InAccessToken is null, uses the current process token.
     *
     * @param levelHandle
     *            handle from SaferCreateLevel
     * @param inAccessToken
     *            optional input token; null = use current process token
     * @param outAccessToken
     *            receives the restricted token (primary token for CreateProcessAsUser)
     * @param dwFlags
     *            reserved, 0
     * @param lpReserved
     *            reserved, null
     * @return true on success
     */
    boolean SaferComputeTokenFromLevel(HANDLE levelHandle, HANDLE inAccessToken, HANDLEByReference outAccessToken, int dwFlags, Pointer lpReserved);

    /**
     * Closes a SAFER_LEVEL_HANDLE from SaferCreateLevel.
     */
    boolean SaferCloseLevel(HANDLE levelHandle);

    /**
     * TOKEN_MANDATORY_LABEL for SetTokenInformation(TokenIntegrityLevel). Same layout as SID_AND_ATTRIBUTES.
     */
    class TOKEN_MANDATORY_LABEL extends Structure {
        public SID_AND_ATTRIBUTES Label;

        public TOKEN_MANDATORY_LABEL() {
            Label = new SID_AND_ATTRIBUTES();
        }

        public TOKEN_MANDATORY_LABEL(Pointer p) {
            super(p);
            Label = new SID_AND_ATTRIBUTES();
            Label.read();
        }

        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList("Label");
        }
    }

    /**
     * Sets token information (e.g. TokenIntegrityLevel). Use with TOKEN_MANDATORY_LABEL.getPointer() after write().
     *
     * @param tokenHandle
     *            token handle
     * @param tokenInfoClass
     *            e.g. TokenIntegrityLevel (25)
     * @param tokenInformation
     *            pointer to structure (e.g. TOKEN_MANDATORY_LABEL)
     * @param length
     *            size in bytes
     * @return true on success
     */
    boolean SetTokenInformation(HANDLE tokenHandle, int tokenInfoClass, Pointer tokenInformation, int length);
}
