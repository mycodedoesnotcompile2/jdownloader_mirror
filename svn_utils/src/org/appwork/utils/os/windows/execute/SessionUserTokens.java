/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.execute;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.jna.windows.Wtsapi32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.WindowsUtils;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Tlhelp32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.Wtsapi32;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Resolves interactive session user tokens (WTS / Shell_TrayWnd) for {@link RunAsHelper} session-owner launches.
 */
public final class SessionUserTokens {
    private static final int PROCESS_QUERY_INFORMATION = 0x0400;

    private SessionUserTokens() {
    }

    /**
     * Tries {@code WTSQueryUserToken} for the session, then the Explorer {@code Shell_TrayWnd} token only if that process belongs to the
     * same WTS session id.
     *
     * @param sessionId
     *            target session (not {@code 0xFFFFFFFF})
     * @return user token; caller must {@link Kernel32#CloseHandle}
     */
    public static WinNT.HANDLE openUserTokenForSession(int sessionId) {
        if (sessionId < 0 || sessionId == (int) 0xFFFFFFFFL) {
            throw new IllegalArgumentException("Invalid WTS session id: " + sessionId);
        }
        final WinNT.HANDLE wts = queryWtsUserToken(sessionId);
        if (wts != null) {
            return wts;
        }
        return openShellTrayUserTokenForSession(sessionId);
    }

    /**
     * Picks the interactive desktop WTS session: unique {@code explorer.exe} session and/or {@code WTSActive} with a non-null winStation
     * (same filter as ConnectService {@code HTTPHandler.handleSelfTest}), with physical console only as fallback.
     * <p>
     * Not LocalSystem-specific — used whenever the caller has no interactive desktop of its own (typically session 0). Do not use
     * {@code WTSGetActiveConsoleSessionId} alone: under RDP/Hyper-V the physical console can be empty while the user works in another
     * session.
     *
     * @return interactive WTS session id (&gt; 0)
     * @throws NoInteractiveOwnerSessionException
     *             when no usable interactive owner session can be determined
     */
    public static int resolveInteractiveOwnerSession() {
        final int activeConsole = safeActiveConsoleSessionId();
        final Map<Integer, List<Integer>> explorersBySession = collectExplorerPidsBySession();
        final List<Integer> explorerSessions = sessionsAboveZero(explorersBySession);
        final Integer explorerUnique = uniqueOrNull(explorerSessions);
        final List<Integer> wtsActiveWithStation = new ArrayList<Integer>();
        final Map<Integer, String> sessionUserLabels = new LinkedHashMap<Integer, String>();
        enumerateWtsActiveWithStation(wtsActiveWithStation, sessionUserLabels);
        final Integer wtsActiveUnique = uniqueOrNull(wtsActiveWithStation);
        // HTTPHandler overwrites on each match → last WTSActive+winStation wins when several exist.
        final Integer wtsActiveLast = wtsActiveWithStation.isEmpty() ? null : wtsActiveWithStation.get(wtsActiveWithStation.size() - 1);
        final boolean consoleValid = activeConsole > 0 && activeConsole != (int) 0xFFFFFFFFL;
        final boolean consoleHasUser = consoleValid && sessionUserLabels.containsKey(Integer.valueOf(activeConsole));
        Integer picked = null;
        String reason = null;
        if (explorerUnique != null && wtsActiveUnique != null) {
            if (explorerUnique.equals(wtsActiveUnique)) {
                picked = explorerUnique;
                reason = "explorer+WTSActive agree";
            } else {
                // Prefer currently active interactive session (HTTPHandler primary signal) when tokens disagree.
                if (canQueryUserToken(wtsActiveUnique.intValue())) {
                    picked = wtsActiveUnique;
                    reason = "conflict: WTSActive token OK";
                } else if (canQueryUserToken(explorerUnique.intValue())) {
                    picked = explorerUnique;
                    reason = "conflict: explorer token OK";
                } else {
                    picked = wtsActiveUnique;
                    reason = "conflict: prefer WTSActive";
                }
            }
        } else if (explorerUnique != null && wtsActiveLast != null && explorerUnique.equals(wtsActiveLast)) {
            picked = explorerUnique;
            reason = "explorer agrees with last WTSActive+winStation";
        } else if (explorerUnique != null && (wtsActiveWithStation.contains(explorerUnique) || canQueryUserToken(explorerUnique.intValue()) || sessionUserLabels.containsKey(explorerUnique))) {
            picked = explorerUnique;
            reason = "explorer unique";
        } else if (wtsActiveUnique != null) {
            picked = wtsActiveUnique;
            reason = "WTSActive+winStation unique";
        } else if (wtsActiveLast != null && explorerUnique == null) {
            picked = wtsActiveLast;
            reason = "last WTSActive+winStation";
        } else if (explorerUnique != null) {
            picked = explorerUnique;
            reason = "explorer unique only";
        } else {
            final List<Integer> intersection = new ArrayList<Integer>();
            for (final Integer sid : explorerSessions) {
                if (wtsActiveWithStation.contains(sid)) {
                    intersection.add(sid);
                }
            }
            if (intersection.size() == 1) {
                picked = intersection.get(0);
                reason = "explorer∩WTSActive unique";
            } else if (consoleValid && consoleHasUser && canQueryUserToken(activeConsole)) {
                picked = Integer.valueOf(activeConsole);
                reason = "fallback console token+user";
            } else if (consoleValid && consoleHasUser) {
                picked = Integer.valueOf(activeConsole);
                reason = "fallback console has user";
            } else if (consoleValid && canQueryUserToken(activeConsole)) {
                picked = Integer.valueOf(activeConsole);
                reason = "fallback console token";
            }
        }
        if (picked != null) {
            LogV3.info("SessionUserTokens: interactive owner session=" + picked + " (" + reason + ")");
            return picked.intValue();
        }
        final String detail = "explorerSessions=" + explorerSessions + ", WTSActiveWithStation=" + wtsActiveWithStation + ", activeConsole=" + activeConsole;
        LogV3.warning("SessionUserTokens: no interactive owner session (" + detail + ")");
        throw new NoInteractiveOwnerSessionException("No interactive owner WTS session (" + detail + ")");
    }

    /**
     * Short probe for tests / failure dumps (explorer map + whether {@code WTSQueryUserToken} works for {@code sessionId}).
     */
    public static void logTokenResolutionDiagnostics(final int sessionId) {
        LogV3.info("SessionUserTokens: probe session=" + sessionId + " token=" + (canQueryUserToken(sessionId) ? "OK" : "FAIL") + " explorersBySession=" + collectExplorerPidsBySession());
    }

    /**
     * Fills {@code WTSActive} sessions that have a non-null winStation (skips Services / session 0 where station is often null — same as
     * HTTPHandler.handleSelfTest).
     */
    private static void enumerateWtsActiveWithStation(final List<Integer> wtsActiveWithStation, final Map<Integer, String> sessionUserLabels) {
        final PointerByReference ppSessionInfo = new PointerByReference();
        final IntByReference pCount = new IntByReference();
        if (!Wtsapi32Ext.INSTANCE.WTSEnumerateSessions(Pointer.NULL, 0, 1, ppSessionInfo, pCount)) {
            return;
        }
        try {
            final int count = pCount.getValue();
            if (count <= 0 || ppSessionInfo.getValue() == null) {
                return;
            }
            final Wtsapi32.WTS_SESSION_INFO ref = new Wtsapi32.WTS_SESSION_INFO(ppSessionInfo.getValue());
            final Wtsapi32.WTS_SESSION_INFO[] arr = (Wtsapi32.WTS_SESSION_INFO[]) ref.toArray(count);
            for (int i = 0; i < arr.length; i++) {
                final int sid = arr[i].SessionId;
                final int state = arr[i].State;
                final String winStation = arr[i].pWinStationName;
                final String userName = queryWtsSessionString(sid, Wtsapi32.WTS_INFO_CLASS.WTSUserName);
                final String domainName = queryWtsSessionString(sid, Wtsapi32.WTS_INFO_CLASS.WTSDomainName);
                final boolean hasUser = userName != null && userName.length() > 0;
                if (hasUser) {
                    final String label = (domainName != null && domainName.length() > 0 ? domainName + "\\" : "") + userName;
                    sessionUserLabels.put(Integer.valueOf(sid), label);
                }
                // WTSActive == 0
                if (state == 0 && winStation != null && sid > 0) {
                    wtsActiveWithStation.add(Integer.valueOf(sid));
                }
            }
        } finally {
            if (ppSessionInfo.getValue() != null) {
                Wtsapi32Ext.INSTANCE.WTSFreeMemory(ppSessionInfo.getValue());
            }
        }
    }

    private static boolean canQueryUserToken(final int sessionId) {
        final PointerByReference token = new PointerByReference();
        if (!Wtsapi32Ext.INSTANCE.WTSQueryUserToken(sessionId, token)) {
            return false;
        }
        try {
            Kernel32.INSTANCE.CloseHandle(new WinNT.HANDLE(token.getValue()));
        } catch (final Throwable ignore) {
        }
        return true;
    }

    private static Map<Integer, List<Integer>> collectExplorerPidsBySession() {
        final Map<Integer, List<Integer>> map = new LinkedHashMap<Integer, List<Integer>>();
        WinNT.HANDLE snapshot = null;
        try {
            snapshot = Kernel32.INSTANCE.CreateToolhelp32Snapshot(Tlhelp32.TH32CS_SNAPPROCESS, new DWORD(0));
            if (snapshot == null || WinBase.INVALID_HANDLE_VALUE.equals(snapshot)) {
                return map;
            }
            final Tlhelp32.PROCESSENTRY32.ByReference pe = new Tlhelp32.PROCESSENTRY32.ByReference();
            if (!Kernel32.INSTANCE.Process32First(snapshot, pe)) {
                return map;
            }
            do {
                final String name = Native.toString(pe.szExeFile);
                if (name != null && name.equalsIgnoreCase("explorer.exe")) {
                    final IntByReference sessionRef = new IntByReference();
                    if (Kernel32Ext.INSTANCE.ProcessIdToSessionId(pe.th32ProcessID.intValue(), sessionRef)) {
                        final Integer sid = Integer.valueOf(sessionRef.getValue());
                        List<Integer> list = map.get(sid);
                        if (list == null) {
                            list = new ArrayList<Integer>();
                            map.put(sid, list);
                        }
                        list.add(Integer.valueOf(pe.th32ProcessID.intValue()));
                    }
                }
            } while (Kernel32.INSTANCE.Process32Next(snapshot, pe));
        } catch (final Throwable t) {
            LogV3.log(t);
        } finally {
            if (snapshot != null && !WinBase.INVALID_HANDLE_VALUE.equals(snapshot)) {
                Kernel32.INSTANCE.CloseHandle(snapshot);
            }
        }
        return map;
    }

    private static List<Integer> sessionsAboveZero(final Map<Integer, List<Integer>> explorersBySession) {
        final List<Integer> out = new ArrayList<Integer>();
        for (final Integer sid : explorersBySession.keySet()) {
            if (sid != null && sid.intValue() > 0) {
                out.add(sid);
            }
        }
        return out;
    }

    private static Integer uniqueOrNull(final List<Integer> sessions) {
        return sessions.size() == 1 ? sessions.get(0) : null;
    }

    private static int safeActiveConsoleSessionId() {
        try {
            return Kernel32Ext.INSTANCE.WTSGetActiveConsoleSessionId();
        } catch (final Throwable t) {
            return -1;
        }
    }

    private static WinNT.HANDLE queryWtsUserToken(int sessionId) {
        final PointerByReference token = new PointerByReference();
        RunAsWin32ApiTrace.in("SessionUserTokens", "WTSQueryUserToken", "sessionId=" + sessionId);
        final boolean ok = Wtsapi32Ext.INSTANCE.WTSQueryUserToken(sessionId, token);
        final int gle = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("SessionUserTokens", "WTSQueryUserToken", ok, gle);
        if (!ok) {
            return null;
        }
        return new WinNT.HANDLE(token.getValue());
    }

    private static String queryWtsSessionString(final int sessionId, final int infoClass) {
        final PointerByReference ppBuffer = new PointerByReference();
        final IntByReference pBytes = new IntByReference();
        try {
            if (!Wtsapi32Ext.INSTANCE.WTSQuerySessionInformation(Wtsapi32.WTS_CURRENT_SERVER_HANDLE, sessionId, infoClass, ppBuffer, pBytes)) {
                return "";
            }
            try {
                final Pointer p = ppBuffer.getValue();
                if (p == null) {
                    return "";
                }
                final String s = p.getWideString(0);
                return s != null ? s : "";
            } finally {
                if (ppBuffer.getValue() != null) {
                    Wtsapi32Ext.INSTANCE.WTSFreeMemory(ppBuffer.getValue());
                }
            }
        } catch (final Throwable t) {
            return "";
        }
    }

    private static WinNT.HANDLE openShellTrayUserTokenForSession(int expectedSessionId) {
        RunAsWin32ApiTrace.in("SessionUserTokens", "FindWindow", "className=Shell_TrayWnd");
        final HWND hwnd = User32.INSTANCE.FindWindow("Shell_TrayWnd", null);
        final int gleFw = Kernel32.INSTANCE.GetLastError();
        RunAsWin32ApiTrace.out("SessionUserTokens", "FindWindow", hwnd != null, gleFw);
        if (hwnd == null) {
            throw new IllegalStateException("Shell_TrayWnd not found for session " + expectedSessionId + " (Explorer not visible from this process)" + contextSuffix(expectedSessionId));
        }
        final IntByReference pid = new IntByReference();
        final int tid = User32.INSTANCE.GetWindowThreadProcessId(hwnd, pid);
        if (tid == 0) {
            throw new Win32Exception(Native.getLastError());
        }
        final IntByReference sessionRef = new IntByReference();
        if (!Kernel32Ext.INSTANCE.ProcessIdToSessionId(pid.getValue(), sessionRef)) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        final int traySession = sessionRef.getValue();
        if (traySession != expectedSessionId) {
            throw new IllegalStateException("Shell_TrayWnd process session " + traySession + " does not match target WTS session " + expectedSessionId);
        }
        final WinNT.HANDLE hProcess = Kernel32.INSTANCE.OpenProcess(PROCESS_QUERY_INFORMATION, false, pid.getValue());
        if (hProcess == null) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        try {
            final WinNT.HANDLEByReference hToken = new WinNT.HANDLEByReference();
            if (!Advapi32.INSTANCE.OpenProcessToken(hProcess, WinNT.TOKEN_DUPLICATE | WinNT.TOKEN_QUERY, hToken)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            return hToken.getValue();
        } finally {
            Kernel32.INSTANCE.CloseHandle(hProcess);
        }
    }

    private static String contextSuffix(int expectedSessionId) {
        String currentSession = "?";
        String localSystem = "?";
        try {
            currentSession = String.valueOf(WindowsUtils.getCurrentProcessSessionId());
        } catch (final Throwable ignore) {
        }
        try {
            localSystem = String.valueOf(WindowsUtils.isRunningAsLocalSystem());
        } catch (final Throwable ignore) {
        }
        return " [expectedSession=" + expectedSessionId + ", currentSession=" + currentSession + ", localSystem=" + localSystem + "]";
    }
}
