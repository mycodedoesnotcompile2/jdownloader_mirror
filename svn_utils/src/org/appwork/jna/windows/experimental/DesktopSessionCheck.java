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
package org.appwork.jna.windows.experimental;

import java.util.ArrayList;
import java.util.List;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase.PROCESS_INFORMATION;
import com.sun.jna.platform.win32.WinBase.STARTUPINFO;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.Wtsapi32;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

public class DesktopSessionCheck {
    public static void main(String[] args) throws Exception {
        // 1. Aktueller User, unter dem der Prozess läuft (z.B. der Admin)
        String currentUser = Advapi32Util.getUserName();
        // 2. Die Session ID des aktuellen Prozesses ermitteln
        int sessionId = Kernel32.INSTANCE.GetCurrentProcessId();
        IntByReference pSessionId = new IntByReference();
        if (Kernel32.INSTANCE.ProcessIdToSessionId(sessionId, pSessionId)) {
            sessionId = pSessionId.getValue();
        }
        // 3. Den registrierten User der Session ermitteln (der "Besitzer" des Desktops)
        String sessionUser = getSessionUsername(sessionId);
        System.out.println("--- Desktop/Session Analyse ---");
        System.out.println("Prozess läuft als User:  " + currentUser);
        System.out.println("Session ID:              " + sessionId);
        System.out.println("Besitzer der Session:    " + sessionUser);
        System.out.println("-------------------------------");
        if (currentUser.equalsIgnoreCase(sessionUser)) {
            System.out.println("Ergebnis: Du läufst in deiner eigenen Session.");
        } else {
            System.out.println("Ergebnis: IDENTITÄTSWECHSEL erkannt!");
            System.out.println("Du bist als '" + currentUser + "' auf dem Desktop von '" + sessionUser + "'.");
        }
        System.out.println(launchAsStandardUser("cmd.exe", null, "e:\\"));
    }

    // Erweitertes Interface für CreateProcessWithTokenW
    public static interface Advapi32Extended extends Advapi32 {
        static final Advapi32Extended INSTANCE = Native.load("advapi32", Advapi32Extended.class, W32APIOptions.DEFAULT_OPTIONS);

        boolean CreateProcessWithTokenW(HANDLE hToken, int dwLogonFlags, String lpApplicationName, String lpCommandLine, int dwCreationFlags, Pointer lpEnvironment, String lpCurrentDirectory, STARTUPINFO lpStartupInfo, PROCESS_INFORMATION lpProcessInformation);
    }

    private static int launchAsStandardUser(final String lpApplicationName, final String lpCommandLine, final String lpCurrentDirectory) throws Exception {
        // 1. Fenster der Taskleiste finden (gehört dem Explorer des interaktiven Users)
        final WinDef.HWND hwnd = User32.INSTANCE.FindWindow("Shell_TrayWnd", null);
        if (hwnd == null) {
            throw new RuntimeException("Explorer.exe (Shell_TrayWnd) nicht gefunden.");
        }
        final IntByReference pid = new IntByReference();
        if (User32.INSTANCE.GetWindowThreadProcessId(hwnd, pid) == 0) {
            throw new Win32Exception(Native.getLastError());
        }
        final List<HANDLE> closeHandles = new ArrayList<HANDLE>();
        try {
            // 2. Prozess öffnen
            final HANDLE hProcess = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_INFORMATION, false, pid.getValue());
            if (hProcess == null) {
                throw new Win32Exception(Native.getLastError());
            }
            closeHandles.add(0, hProcess);
            // 3. Token des Explorers abgreifen
            final HANDLEByReference hToken = new HANDLEByReference();
            if (!Advapi32.INSTANCE.OpenProcessToken(hProcess, WinNT.TOKEN_DUPLICATE | WinNT.TOKEN_QUERY, hToken)) {
                throw new Win32Exception(Native.getLastError());
            }
            closeHandles.add(0, hToken.getValue());
            // 4. Token duplizieren
            final HANDLEByReference hDupToken = new HANDLEByReference();
            if (!Advapi32.INSTANCE.DuplicateTokenEx(hToken.getValue(), WinNT.TOKEN_ALL_ACCESS, null, WinNT.SECURITY_IMPERSONATION_LEVEL.SecurityImpersonation, WinNT.TOKEN_TYPE.TokenPrimary, hDupToken)) {
                throw new Win32Exception(Native.getLastError());
            }
            closeHandles.add(0, hDupToken.getValue());
            // 5. Prozess starten mit CreateProcessWithTokenW
            final STARTUPINFO si = new STARTUPINFO();
            final PROCESS_INFORMATION pi = new PROCESS_INFORMATION();
            si.cb = new WinDef.DWORD(si.size());
            // dwLogonFlags: 1 = LOGON_WITH_PROFILE
            // https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createprocesswithtokenw
            if (!Advapi32Extended.INSTANCE.CreateProcessWithTokenW(hDupToken.getValue(), 1, lpApplicationName, lpCommandLine, 0, null, lpCurrentDirectory, si, pi)) {
                throw new Win32Exception(Native.getLastError());
            }
            closeHandles.add(0, pi.hThread);
            closeHandles.add(0, pi.hProcess);
            return pi.dwProcessId.intValue();
        } finally {
            // Cleanup
            for (HANDLE closeHandle : closeHandles) {
                Kernel32.INSTANCE.CloseHandle(closeHandle);
            }
        }
    }

    private static String getSessionUsername(int sessionId) {
        final PointerByReference ppBuffer = new PointerByReference();
        final IntByReference pBytesReturned = new IntByReference();
        // Abfrage der WTS-API nach dem Usernamen der Session
        final int WTS_USER_NAME = 5;
        if (Wtsapi32.INSTANCE.WTSQuerySessionInformation(Wtsapi32.WTS_CURRENT_SERVER_HANDLE, sessionId, WTS_USER_NAME, ppBuffer, pBytesReturned)) {
            String name = ppBuffer.getValue().getWideString(0);
            Wtsapi32.INSTANCE.WTSFreeMemory(ppBuffer.getValue());
            return name;
        }
        return "Unbekannt";
    }
}