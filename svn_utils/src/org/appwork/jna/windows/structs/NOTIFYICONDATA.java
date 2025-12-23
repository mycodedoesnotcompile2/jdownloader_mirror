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
package org.appwork.jna.windows.structs;

import static com.sun.jna.platform.win32.WinDef.HWND;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinDef;

/**
 * NOTIFYICONDATA structure for Windows Shell_NotifyIcon API.
 * 
 * @author thomas
 * @date 11.03.2025
 */
public class NOTIFYICONDATA extends Structure {
    public static final int NIF_MESSAGE = 0x1;
    public static final int NIF_ICON = 0x2;
    public static final int NIF_TIP = 0x4;
    public static final int NIF_STATE = 0x8;
    public static final int NIF_INFO = 0x10;

    public static final int NIIF_NONE = 0x0;
    public static final int NIIF_INFO = 0x1;
    public static final int NIIF_WARNING = 0x2;
    public static final int NIIF_ERROR = 0x3;
    public static final int NIIF_USER = 0x4;

    public int cbSize;
    public HWND hWnd;
    public int uID;
    public int uFlags;
    public int uCallbackMessage;
    public WinDef.HICON hIcon;

    public char[] szTip = new char[128];

    public int dwState;
    public int dwStateMask;

    public char[] szInfo = new char[256];
    public int uTimeoutOrVersion; // {UINT uTimeout; UINT uVersion;};

    public char[] szInfoTitle = new char[64];
    public int dwInfoFlags;

    public NOTIFYICONDATA() {
        super();
        cbSize = size();
    }

    public void setTooltip(String s) {
        uFlags |= NIF_TIP;

        System.arraycopy(s.toCharArray(), 0, szTip, 0, Math.min(s.length(), szTip.length));
        szTip[s.length()] = '\0';
    }

    public void setBalloon(String title, String message, int millis, int niif) {
        uFlags |= NIF_INFO;

        System.arraycopy(message.toCharArray(), 0, szInfo, 0, Math.min(message.length(), szInfo.length));
        szInfo[message.length()] = '\0';

        uTimeoutOrVersion = millis;

        System.arraycopy(title.toCharArray(), 0, szInfoTitle, 0, Math.min(title.length(), szInfoTitle.length));
        szInfoTitle[title.length()] = '\0';

        dwInfoFlags = niif;
    }

    public void setIcon(WinDef.HICON hIcon) {
        uFlags |= NIF_ICON;
        this.hIcon = hIcon;
    }

    public void setCallback(int callback) {
        uFlags |= NIF_MESSAGE;
        uCallbackMessage = callback;
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("cbSize",
                             "hWnd",
                             "uID",
                             "uFlags",
                             "uCallbackMessage",
                             "hIcon",
                             "szTip",
                             "dwState",
                             "dwStateMask",
                             "szInfo",
                             "uTimeoutOrVersion",
                             "szInfoTitle",
                             "dwInfoFlags");
    }
}
