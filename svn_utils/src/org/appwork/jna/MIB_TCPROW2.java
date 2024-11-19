package org.appwork.jna;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinDef.DWORD;

public class MIB_TCPROW2 extends Structure {
    // typedef struct _MIB_TCPROW2 {
    // DWORD dwState;
    // DWORD dwLocalAddr;
    // DWORD dwLocalPort;
    // DWORD dwRemoteAddr;
    // DWORD dwRemotePort;
    // DWORD dwOwningPid;
    // TCP_CONNECTION_OFFLOAD_STATE dwOffloadState;
    // } MIB_TCPROW2, *PMIB_TCPROW2;
    public DWORD dwState;
    public DWORD dwLocalAddr;
    public DWORD dwLocalPort;
    public DWORD dwRemoteAddr;
    public DWORD dwRemotePort;
    public DWORD dwOwningPid;
    public int   dwOffloadState;

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "dwState", "dwLocalAddr", "dwLocalPort", "dwRemoteAddr", "dwRemotePort", "dwOwningPid", "dwOffloadState" });
    }
}