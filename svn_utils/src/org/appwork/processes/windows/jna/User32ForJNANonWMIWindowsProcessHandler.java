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

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

/**
 * Minimal User32 interface for {@link JNANonWMIWindowsProcessHandler} only. Exposes only the functions used by that
 * handler for toFront (AllowSetForegroundWindow, GetWindowThreadProcessId, AttachThreadInput, ShowWindow, SendMessage,
 * SetForegroundWindow). Keeps the process module independent from org.appwork.jna.windows.User32Ext.
 *
 * @author thomas
 * @date 08.03.2026
 */
public interface User32ForJNANonWMIWindowsProcessHandler extends StdCallLibrary {

    User32ForJNANonWMIWindowsProcessHandler INSTANCE = Native.load("user32", User32ForJNANonWMIWindowsProcessHandler.class, W32APIOptions.DEFAULT_OPTIONS);

    /**
     * Enables the process to set the foreground window using SetForegroundWindow.
     *
     * @param dwProcessId
     *            process ID to allow
     * @return true on success
     */
    boolean AllowSetForegroundWindow(int dwProcessId);

    /**
     * Retrieves the identifier of the thread that created the specified window. If lpdwProcessId is null, only the
     * thread id is returned.
     *
     * @param hWnd
     *            window handle
     * @param lpdwProcessId
     *            optional output for process ID, or null
     * @return thread identifier
     */
    int GetWindowThreadProcessId(WinDef.HWND hWnd, Pointer lpdwProcessId);

    /**
     * Attaches or detaches the input processing mechanism of one thread to that of another thread.
     *
     * @param idAttach
     *            thread to attach
     * @param idAttachTo
     *            thread to attach to
     * @param fAttach
     *            true to attach, false to detach
     * @return true on success
     */
    boolean AttachThreadInput(int idAttach, int idAttachTo, boolean fAttach);

    /**
     * Sets the specified window's show state.
     *
     * @param hWnd
     *            window handle
     * @param nCmdShow
     *            show state (e.g. SW_MINIMIZE, SW_RESTORE, SW_SHOW)
     * @return true if the window was previously visible
     */
    boolean ShowWindow(WinDef.HWND hWnd, int nCmdShow);

    /**
     * Sends a message to the specified window.
     *
     * @param hWnd
     *            window handle
     * @param Msg
     *            message (e.g. WM_SYSCOMMAND)
     * @param wParam
     *            first message parameter
     * @param lParam
     *            second message parameter
     * @return result of the window procedure
     */
    WinDef.LRESULT SendMessage(WinDef.HWND hWnd, int Msg, WinDef.WPARAM wParam, WinDef.LPARAM lParam);

    /**
     * Brings the thread that created the specified window into the foreground.
     *
     * @param hWnd
     *            window handle
     * @return true on success
     */
    boolean SetForegroundWindow(WinDef.HWND hWnd);
}
