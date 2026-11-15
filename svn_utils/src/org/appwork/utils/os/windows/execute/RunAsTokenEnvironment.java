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

import org.appwork.utils.os.windows.execute.jna.UserenvRunAsLib;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.PointerByReference;

/**
 * Builds / frees a Unicode environment block for token-based process creation ({@link RunAsProcessLauncher}).
 */
public final class RunAsTokenEnvironment {
    private RunAsTokenEnvironment() {
    }

    /**
     * @param primaryToken
     *            duplicated primary token for the target user
     * @param inheritFromCaller
     *            {@code false} so the child does not merge the current process environment (e.g. LocalSystem caller vars)
     * @return block for {@code CreateProcessAsUserW} / {@code CreateProcessWithTokenW}; call {@link #destroy(Pointer)} when done
     */
    public static Pointer createForPrimaryToken(final HANDLE primaryToken, final boolean inheritFromCaller) {
        if (primaryToken == null) {
            throw new IllegalArgumentException("primaryToken is required");
        }
        final PointerByReference envRef = new PointerByReference();
        RunAsWin32ApiTrace.in("RunAsTokenEnvironment", "CreateEnvironmentBlock", "token=" + RunAsWin32ApiTrace.h(primaryToken) + " inherit=" + inheritFromCaller);
        final boolean ok = UserenvRunAsLib.INSTANCE.CreateEnvironmentBlock(envRef, primaryToken, inheritFromCaller);
        final int gle = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("RunAsTokenEnvironment", "CreateEnvironmentBlock", ok, gle);
        if (!ok) {
            throw new Win32Exception(gle);
        }
        final Pointer env = envRef.getValue();
        if (env == null) {
            throw new Win32Exception(gle != 0 ? gle : 0x8); // ERROR_NOT_ENOUGH_MEMORY fallback
        }
        return env;
    }

    public static void destroy(final Pointer envBlock) {
        if (envBlock == null) {
            return;
        }
        RunAsWin32ApiTrace.in("RunAsTokenEnvironment", "DestroyEnvironmentBlock", "env=" + envBlock);
        final boolean ok = UserenvRunAsLib.INSTANCE.DestroyEnvironmentBlock(envBlock);
        RunAsWin32ApiTrace.out("RunAsTokenEnvironment", "DestroyEnvironmentBlock", ok, Kernel32.INSTANCE.GetLastError());
    }
}
