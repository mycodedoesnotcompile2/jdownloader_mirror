package org.appwork.jna;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.platform.win32.IPHlpAPI;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.W32APIOptions;

public interface Iphlpapi extends IPHlpAPI {
    // https://docs.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-socket
    public static final int      AF_INET  = 2;
    public static final Iphlpapi INSTANCE = Native.load("IPHlpAPI", Iphlpapi.class, W32APIOptions.DEFAULT_OPTIONS);

    // https://docs.microsoft.com/en-us/windows/win32/api/iphlpapi/nf-iphlpapi-getextendedtcptable
    int GetExtendedTcpTable(Memory pTcpTable, IntByReference pdwSize, boolean bOrder, int ulAf, int table, int reserved);

    int GetTcpTable2(Memory table, IntByReference psize, boolean sort);
}