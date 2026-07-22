package org.appwork.jna.windows;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.platform.win32.IPHlpAPI;
import com.sun.jna.ptr.IntByReference;

public interface Iphlpapi extends IPHlpAPI {
    // https://docs.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-socket
    public static final int      AF_INET  = 2;
    public static final Iphlpapi INSTANCE = Native.load("IPHlpAPI", Iphlpapi.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

    // https://docs.microsoft.com/en-us/windows/win32/api/iphlpapi/nf-iphlpapi-getextendedtcptable
    int GetExtendedTcpTable(Memory pTcpTable, IntByReference pdwSize, boolean bOrder, int ulAf, int table, int reserved);

    int GetTcpTable2(Memory table, IntByReference psize, boolean sort);
}