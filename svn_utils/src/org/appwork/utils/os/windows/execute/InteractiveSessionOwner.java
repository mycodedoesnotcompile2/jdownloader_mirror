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
import org.appwork.utils.os.WindowsUtils;

import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinNT;

/**
 * Result of resolving the interactive session owner (WTS session id, account SID, user token) for {@link RunAsHelper}. Step 2 in the run-as
 * pipeline: open the session user token via {@link SessionUserTokens#openUserTokenForSession(int)} and read the account. Call
 * {@link #close()} when done so the token handle is released.
 */
public final class InteractiveSessionOwner {
    private final int    sessionId;
    private final String ownerSid;
    private final String ownerAccountName;
    private WinNT.HANDLE userToken;

    private InteractiveSessionOwner(int sessionId, String ownerSid, String ownerAccountName, WinNT.HANDLE userToken) {
        this.sessionId = sessionId;
        this.ownerSid = ownerSid;
        this.ownerAccountName = ownerAccountName;
        this.userToken = userToken;
    }

    /**
     * Resolves the interactive owner for launching UI/user-context children from the current process.
     * <p>
     * Callers without an interactive desktop (typically LocalSystem in session 0) use
     * {@link SessionUserTokens#resolveInteractiveOwnerSession()} (Explorer / {@code WTSActive} / console fallback) instead of the process
     * session.
     */
    public static InteractiveSessionOwner openForCurrentProcess() throws Exception {
        return openForSession(resolveOwnerSessionIdForCurrentProcess());
    }

    /**
     * Owner WTS session for {@link #openForCurrentProcess()} / {@link RunAsHelper#resolveCurrentContext()}: normally the process session;
     * for LocalSystem in session 0 the interactive desktop session (not necessarily {@code WTSGetActiveConsoleSessionId}).
     *
     * @throws NoInteractiveOwnerSessionException
     *             when session 0 / LocalSystem and no usable interactive owner session can be determined
     */
    public static int resolveOwnerSessionIdForCurrentProcess() {
        int processSession = WindowsUtils.getCurrentProcessSessionId();
        if (processSession < 0) {
            throw new IllegalStateException("Cannot resolve WTS session id for current process (ProcessIdToSessionId failed).");
        }
        // Session 0 has no interactive desktop/Explorer — resolve via Explorer map + WTSActive (+ console fallback).
        if (processSession == 0 && WindowsUtils.isRunningAsLocalSystem()) {
            return SessionUserTokens.resolveInteractiveOwnerSession();
        }
        return processSession;
    }

    /**
     * Opens the interactive owner for the given WTS session id (e.g. when the caller is LocalSystem and must target a console/RDP session).
     * Token resolution correlates token and session id (WTSQueryUserToken, then Shell_TrayWnd only if same session).
     */
    public static InteractiveSessionOwner openForSession(int sessionId) throws Exception {
        RunAsWin32ApiTrace.in("InteractiveSessionOwner", "openForSession", "sessionId=" + sessionId);
        WinNT.HANDLE h = SessionUserTokens.openUserTokenForSession(sessionId);
        if (RunAsWin32ApiTrace.isEnabled()) {
            LogV3.info(RunAsWin32ApiTrace.PREFIX + " InteractiveSessionOwner INFO sessionUserToken=" + RunAsWin32ApiTrace.h(h));
        }
        try {
            Advapi32Util.Account a = Advapi32Util.getTokenAccount(h);
            if (a == null || a.sidString == null || a.sidString.trim().length() == 0) {
                throw new IllegalStateException("Cannot read account for session user token (session " + sessionId + ").");
            }
            String name = a.name != null ? a.name : "";
            return new InteractiveSessionOwner(sessionId, a.sidString.trim(), name, h);
        } catch (Throwable t) {
            closeHandleQuietly(h);
            if (t instanceof Exception) {
                throw (Exception) t;
            }
            throw new Exception(t);
        }
    }

    public int getSessionId() {
        return sessionId;
    }

    public String getOwnerSid() {
        return ownerSid;
    }

    /**
     * Account name from the token (may be empty on failure paths; SID is authoritative).
     */
    public String getOwnerAccountName() {
        return ownerAccountName;
    }

    /**
     * Session user token; owned by this instance until {@link #close()}. Do not close from outside.
     */
    public WinNT.HANDLE getUserTokenHandle() {
        return userToken;
    }

    public void close() {
        closeHandleQuietly(userToken);
        userToken = null;
    }

    private static void closeHandleQuietly(WinNT.HANDLE h) {
        if (h != null) {
            try {
                RunAsWin32ApiTrace.in("InteractiveSessionOwner", "CloseHandle", RunAsWin32ApiTrace.h(h));
                final boolean c = Kernel32.INSTANCE.CloseHandle(h);
                RunAsWin32ApiTrace.out("InteractiveSessionOwner", "CloseHandle", c, Kernel32.INSTANCE.GetLastError());
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }
}
