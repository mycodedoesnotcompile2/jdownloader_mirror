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

import com.sun.jna.Native;
import com.sun.jna.Pointer;

import org.appwork.jna.windows.structs.NOTIFYICONDATA;
import org.appwork.jna.windows.structs.NOTIFYICONIDENTIFIER;

import com.sun.jna.platform.win32.WinNT.HRESULT;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Shell32 extension for System Tray functionality.
 * Extends JNA Platform Shell32 with Shell_NotifyIcon and Shell_NotifyIconGetRect.
 * 
 * @author thomas
 * @date 11.03.2025
 */
public interface Shell32Ext extends com.sun.jna.platform.win32.Shell32 {
    // Constants for Shell_NotifyIcon
    public static final int NIM_ADD = 0x0;
    public static final int NIM_MODIFY = 0x1;
    public static final int NIM_DELETE = 0x2;

    final static Shell32Ext INSTANCE = Native.load("shell32", Shell32Ext.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

    /**
     * Shell_NotifyIcon with custom NOTIFYICONDATA structure (for System Tray).
     */
    boolean Shell_NotifyIcon(int dwMessage, NOTIFYICONDATA lpdata);
    
    /**
     * Shell_NotifyIconGetRect - Retrieves the bounding rectangle of a notification icon (Windows 7+).
     * 
     * @param identifier The NOTIFYICONIDENTIFIER structure identifying the icon
     * @param iconLocation Pointer to a RECT structure that receives the icon's bounding rectangle
     * @return HRESULT - S_OK if successful, otherwise an error code
     */
    int Shell_NotifyIconGetRect(NOTIFYICONIDENTIFIER identifier, WinUser.RECT iconLocation);

    /**
     * Translates a file-system path (or other display name) into an absolute ITEMIDLIST (PIDL). The returned PIDL must be released with
     * {@code Ole32.INSTANCE.CoTaskMemFree(...)}. <br>
     * Unlike {@code explorer.exe /select}, this works for paths that exceed MAX_PATH (260 chars) and does not depend on an 8.3 short name
     * being available for the file.
     *
     * @param pszName
     *            the parsing name (e.g. the absolute file path); passed as {@link String} because this interface is loaded with the UNICODE
     *            {@code TypeMapper}, which marshals it as a wide string automatically - no {@link com.sun.jna.WString} needed
     * @param pbc
     *            bind context, may be {@code null}
     * @param ppidl
     *            receives the absolute PIDL on success
     * @param sfgaoIn
     *            attributes to query ({@code 0} if none)
     * @param psfgaoOut
     *            receives the queried attributes, may be {@code null}
     * @return {@code S_OK} (0) on success
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/shlobj_core/nf-shlobj_core-shparsedisplayname">SHParseDisplayName</a>
     */
    HRESULT SHParseDisplayName(String pszName, Pointer pbc, PointerByReference ppidl, int sfgaoIn, IntByReference psfgaoOut);

    /**
     * Opens a Windows Explorer window with the items in a specified folder selected. <br>
     * Special case: passing the item's own absolute PIDL as {@code pidlFolder} together with {@code cidl == 0} and {@code apidl == null}
     * opens the item's <b>parent</b> folder and selects the item itself. This is the MAX_PATH-safe replacement for
     * {@code explorer.exe /select,"<path>"}.
     *
     * @param pidlFolder
     *            absolute PIDL of the folder (or of the item itself, see above)
     * @param cidl
     *            number of elements in {@code apidl}
     * @param apidl
     *            array of child PIDLs to select, may be {@code null}
     * @param dwFlags
     *            open flags ({@code 0} for default)
     * @return {@code S_OK} (0) on success
     * @see <a href=
     *      "https://learn.microsoft.com/en-us/windows/win32/api/shlobj_core/nf-shlobj_core-shopenfolderandselectitems">SHOpenFolderAndSelectItems</a>
     */
    HRESULT SHOpenFolderAndSelectItems(Pointer pidlFolder, int cidl, Pointer[] apidl, int dwFlags);
}
