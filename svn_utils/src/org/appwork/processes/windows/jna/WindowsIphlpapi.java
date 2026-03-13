/**
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         See full license in project root.
 * ==================================================================================================================================================== */
package org.appwork.processes.windows.jna;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.platform.win32.IPHlpAPI;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * JNA interface to Windows IP Helper API (IPHlpAPI.dll). Used by {@link WindowsJNAProcessUtils} for GetTcpTable2 / GetTcp6Table2
 * to resolve peer PID from the TCP connection table.
 */
public interface WindowsIphlpapi extends IPHlpAPI {
    int AF_INET = 2;

    WindowsIphlpapi INSTANCE = Native.load("IPHlpAPI", WindowsIphlpapi.class, W32APIOptions.DEFAULT_OPTIONS);

    int GetExtendedTcpTable(Memory pTcpTable, IntByReference pdwSize, boolean bOrder, int ulAf, int table, int reserved);

    /** @see <a href="https://docs.microsoft.com/en-us/windows/win32/api/iphlpapi/nf-iphlpapi-getextendedtcptable">GetExtendedTcpTable</a> */
    int GetTcpTable2(Memory table, IntByReference psize, boolean sort);

    /** GetTcp6Table2 for IPv6 TCP connection table. Requires Vista+. */
    int GetTcp6Table2(Memory table, IntByReference psize, boolean sort);
}
