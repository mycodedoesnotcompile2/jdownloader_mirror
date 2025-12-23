/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.jna.windows;

import com.sun.jna.Callback;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinGDI;
import com.sun.jna.win32.W32APIOptions;


/**
 * Extended User32 interface for additional Windows API functions not available in JNA Platform.
 * Extends {@link com.sun.jna.platform.win32.User32} with functions needed for System Tray and other advanced features.
 * 
 * <p>This interface adds the following functionality:</p>
 * <ul>
 *   <li>Window flashing (FlashWindowEx)</li>
 *   <li>System Tray support (SetWindowLong with Callback, custom MSG handling)</li>
 *   <li>Icon creation (CreateIconIndirect)</li>
 *   <li>Window text retrieval (GetWindowTextW)</li>
 *   <li>Thread input attachment (AttachThreadInput)</li>
 *   <li>Foreground window control (AllowSetForegroundWindow)</li>
 * </ul>
 * 
 * @author thomas
 * @date 11.03.2025
 */
public interface User32Ext extends com.sun.jna.platform.win32.User32 {
    /** Stop flashing the window. */
    public static final int FLASHW_STOP      = 0;
    /** Flash the window caption. */
    public static final int FLASHW_CAPTION   = 0x00000001;
    /** Flash the taskbar button. */
    public static final int FLASHW_TRAY      = 0x00000002;
    /** Flash both the window caption and taskbar button. */
    public static final int FLASHW_ALL       = (FLASHW_CAPTION | FLASHW_TRAY);
    /** Flash continuously until the window comes to the foreground. */
    public static final int FLASHW_TIMERNOFG = 0x0000000C;

    /**
     * Left mouse button up message (for System Tray click detection).
     * Sent when the user releases the left mouse button.
     */
    public static final int WM_LBUTTONUP = 0x0202;
    
    /**
     * Right mouse button up message (for System Tray click detection).
     * Sent when the user releases the right mouse button.
     */
    public static final int WM_RBUTTONUP = 0x0205;

    final static User32Ext  INSTANCE         = Native.load("user32", User32Ext.class, W32APIOptions.DEFAULT_OPTIONS);

    /**
     * Retrieves a handle to a window that has the specified relationship to the specified window.
     * 
     * @param hWnd A handle to a window.
     * @param uCmd The relationship between the specified window and the window whose handle is to be retrieved.
     * @return The return value is a handle to a window. If no window exists with the specified relationship to the specified window, the return value is NULL.
     */
    WinDef.HWND GetWindow(WinDef.HWND hWnd, int uCmd);
    
    // // Get process ID associated with a window
    // int GetWindowThreadProcessId(HWND hWnd, IntByReference lpdwProcessId);

    /**
     * Copies the text of the specified window's title bar (if it has one) into a buffer.
     * Unicode version of GetWindowText.
     * 
     * @param hwnd Handle to the window or control containing the text.
     * @param windowTitle Buffer that will receive the text.
     * @param length Maximum number of characters to copy to the buffer, including the NULL character.
     * @return The length, in characters, of the copied string, not including the terminating null character.
     */
    public int GetWindowTextW(HWND hwnd, char[] windowTitle, int length);

    /**
     * Attaches or detaches the input processing mechanism of one thread to that of another thread.
     * 
     * @param idAttach The identifier of the thread to be attached to another thread.
     * @param idAttachTo The identifier of the thread to which idAttach will be attached.
     * @param fAttach If this parameter is TRUE, the two threads are attached. If FALSE, the threads are detached.
     * @return If the function succeeds, the return value is nonzero. If the function fails, the return value is zero.
     */
    boolean AttachThreadInput(int idAttach, int idAttachTo, boolean fAttach);

    /**
     * Enables the process to set the foreground window using SetForegroundWindow.
     * 
     * @param dwProcessId The identifier of the process that will be enabled to set the foreground window.
     * @return If the function succeeds, the return value is nonzero. If the function fails, the return value is zero.
     */
    boolean AllowSetForegroundWindow(int dwProcessId);

    /**
     * FLASHWINFO structure for FlashWindowEx.
     * Contains the flash status for a window and the number of times the system should flash the window.
     */
    @Structure.FieldOrder({ "cbSize", "hwnd", "dwFlags", "uCount", "dwTimeout" })
    class FLASHWINFO extends Structure {
        /** The size of this structure, in bytes. */
        public int  cbSize;
        /** A handle to the window to be flashed. */
        public HWND hwnd;
        /** The flash status. This parameter can be one or more of the FLASHW_* constants. */
        public int  dwFlags;
        /** The number of times to flash the window. */
        public int  uCount;
        /** The rate at which the window is to be flashed, in milliseconds. If dwTimeout is zero, the function uses the default cursor blink rate. */
        public int  dwTimeout;
    }

    /**
     * Flashes the specified window. It does not change the active state of the window.
     * 
     * @param pwfi A pointer to a FLASHWINFO structure.
     * @return The return value specifies the window's state before the call to FlashWindowEx.
     *         If the window caption was drawn as active before the call, the return value is nonzero.
     *         Otherwise, the return value is zero.
     */
    boolean FlashWindowEx(FLASHWINFO pwfi);

    /**
     * Changes an attribute of the specified window, specifically for setting a window procedure (WNDPROC) callback.
     * This method accepts a {@link Callback} parameter directly, which is needed for System Tray implementations.
     * 
     * <p>On 64-bit Windows, this internally calls SetWindowLongPtr. On 32-bit Windows, SetWindowLong is used directly.</p>
     * 
     * <p>This is different from {@link com.sun.jna.platform.win32.User32#SetWindowLong(HWND, int, int)}
     * which only accepts integer values, not callbacks.</p>
     * 
     * @param hWnd A handle to the window and, indirectly, the class to which the window belongs.
     * @param nIndex The zero-based offset to the value to be set. Use {@link com.sun.jna.platform.win32.WinUser#GWL_WNDPROC} to set a window procedure.
     * @param procedure The replacement window procedure callback (WNDPROC).
     * @return The previous value of the specified offset indicates success. Zero indicates failure.
     * @see com.sun.jna.platform.win32.WinUser#GWL_WNDPROC
     * @see #SetWindowLongPtr(HWND, int, Callback)
     */
    int SetWindowLong(HWND hWnd, int nIndex, Callback procedure);
    
    /**
     * Changes an attribute of the specified window using SetWindowLongPtr (64-bit safe version).
     * This should be used on 64-bit Windows when setting a window procedure callback.
     * On 32-bit Windows, this is equivalent to SetWindowLong.
     * 
     * <p>This method is explicitly declared to ensure proper 64-bit handling.</p>
     * 
     * @param hWnd A handle to the window and, indirectly, the class to which the window belongs.
     * @param nIndex The zero-based offset to the value to be set. Use {@link com.sun.jna.platform.win32.WinUser#GWL_WNDPROC} to set a window procedure.
     * @param procedure The replacement window procedure callback (WNDPROC).
     * @return The previous value of the specified offset indicates success. Zero indicates failure.
     * @see com.sun.jna.platform.win32.WinUser#GWL_WNDPROC
     */
    int SetWindowLongPtr(HWND hWnd, int nIndex, Callback procedure);

    /**
     * Creates an icon or cursor from an {@link WinGDI.ICONINFO} structure.
     * Used for System Tray icon conversion from Java images to Windows HICON handles.
     * 
     * <p>This method uses the custom ICONINFO structure from {@link org.appwork.jna.windows.structs.ICONINFO}
     * which is compatible with the System Tray implementation.</p>
     * 
     * @param piconinfo A pointer to an ICONINFO structure that contains information about the icon or cursor.
     * @return If the function succeeds, the return value is a handle to the icon or cursor.
     *         If the function fails, the return value is NULL.
     * @see org.appwork.jna.windows.structs.ICONINFO
     */
    WinDef.HICON CreateIconIndirect(WinGDI.ICONINFO piconinfo);

    /**
     * Retrieves a message from the calling thread's message queue using a custom MSG structure.
     * This version uses {@link org.appwork.jna.windows.structs.MSG} instead of {@link com.sun.jna.platform.win32.WinUser.MSG},
     * which is required for the System Tray message loop implementation.
     * 
     * <p>The function dispatches incoming sent messages, checks the thread message queue for a posted message,
     * and retrieves the message (if any exist).</p>
     * 
     * @param lpMsg A pointer to an MSG structure that receives message information.
     * @param hWnd A handle to the window whose messages are to be retrieved. If NULL, retrieves messages for any window.
     * @param wMsgFilterMin The integer value of the lowest message value to be retrieved.
     * @param wMsgFilterMax The integer value of the highest message value to be retrieved.
     * @return If the function retrieves a message other than WM_QUIT, the return value is nonzero.
     *         If the function retrieves the WM_QUIT message, the return value is zero.
     *         If there is an error, the return value is -1.
     * @see org.appwork.jna.windows.structs.MSG
     */
    boolean GetMessage(org.appwork.jna.windows.structs.MSG lpMsg, Pointer hWnd, int wMsgFilterMin, int wMsgFilterMax);

    /**
     * Translates virtual-key messages into character messages using a custom MSG structure.
     * The character messages are posted to the calling thread's message queue, to be read the next time the thread calls GetMessage.
     * 
     * <p>This version uses {@link org.appwork.jna.windows.structs.MSG} instead of {@link com.sun.jna.platform.win32.WinUser.MSG},
     * which is required for the System Tray message loop implementation.</p>
     * 
     * @param lpMsg A pointer to an MSG structure that contains message information retrieved from the calling thread's message queue.
     * @return If the message is translated (that is, a character message is posted to the thread's message queue), the return value is nonzero.
     *         If the message is WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, or WM_SYSKEYUP, the return value is nonzero, regardless of the translation.
     *         If the message is not translated, the return value is zero.
     * @see org.appwork.jna.windows.structs.MSG
     */
    boolean TranslateMessage(org.appwork.jna.windows.structs.MSG lpMsg);

    /**
     * Dispatches a message to a window procedure using a custom MSG structure.
     * Typically used to dispatch a message retrieved by the GetMessage function.
     * 
     * <p>This version uses {@link org.appwork.jna.windows.structs.MSG} instead of {@link com.sun.jna.platform.win32.WinUser.MSG},
     * which is required for the System Tray message loop implementation.</p>
     * 
     * @param lpMsg A pointer to an MSG structure that contains the message.
     * @return The return value specifies the value returned by the window procedure.
     *         Although its meaning depends on the message being dispatched, the return value generally is ignored.
     * @see org.appwork.jna.windows.structs.MSG
     */
    WinDef.LRESULT DispatchMessage(org.appwork.jna.windows.structs.MSG lpMsg);

    /**
     * Defines a new window message that is guaranteed to be unique throughout the system using a WString parameter.
     * The message value can be used when sending or posting messages.
     * 
     * <p>This version uses {@link WString} instead of {@link String}, which is required for compatibility
     * with the System Tray implementation (e.g., registering "TaskbarCreated" message).</p>
     * 
     * <p>This is different from {@link com.sun.jna.platform.win32.User32#RegisterWindowMessage(String)}
     * which uses String instead of WString.</p>
     * 
     * @param lpString The message string to be registered. Maximum length is 255 characters.
     * @return If the message is successfully registered, the return value is a message identifier in the range 0xC000 through 0xFFFF.
     *         If the function fails, the return value is zero.
     * @see com.sun.jna.WString
     */
    int RegisterWindowMessage(WString lpString);
}
