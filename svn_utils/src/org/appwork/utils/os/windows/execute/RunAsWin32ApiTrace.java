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

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.NoLogSource;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinNT.HANDLE;

/**
 * Verbose, removable tracing for Win32/JNA calls on the run-as path. Prefix {@value #PREFIX}; grep logs for {@code RunAsApiTrace}.
 * <p>
 * Set system property {@value #SYSPROP_APITRACE} to {@code true} ({@code -Dappwork.runas.apitrace=true}) so code that checks {@link #isEnabled()}
 * emits additional detail lines (WaitForSingleObject return code, ReadFile byte counts, etc.). IN/OUT lines from {@code in}/{@code out} are not gated by this flag.
 */
public final class RunAsWin32ApiTrace {
    public static final String PREFIX       = "RunAsApiTrace";
    /** When {@code true} ({@link Boolean#getBoolean(String)}), supplemental {@code LogV3.info} diagnostics guarded by {@link #isEnabled()} are emitted. */
    public static final String SYSPROP_APITRACE = "appwork.runas.apitrace";

    private RunAsWin32ApiTrace() {
    }

    /**
     * @return {@link Boolean#getBoolean(String)} for {@value #SYSPROP_APITRACE} — pass {@code -Dappwork.runas.apitrace=true} to the JVM.
     */
    public static boolean isEnabled() {
        return Boolean.getBoolean(SYSPROP_APITRACE);
    }

    @NoLogSource
    public static void in(final String component, final String api, final String detail) {
        LogV3.info(PREFIX + " " + component + " IN  " + api + " " + (detail != null ? detail : ""));
    }

    /**
     * @param gle
     *            {@link Kernel32#GetLastError()} captured immediately after the native call (before further Win32 calls).
     */
    @NoLogSource
    public static void out(final String component, final String api, final boolean ok, final int gle) {
        LogV3.info(PREFIX + " " + component + " OUT " + api + " ok=" + ok + " gle=" + gle);
    }

    @NoLogSource
    public static String h(final HANDLE h) {
        if (h == null || h.getPointer() == null) {
            return "null";
        }
        return "0x" + Long.toHexString(Pointer.nativeValue(h.getPointer()));
    }
}
