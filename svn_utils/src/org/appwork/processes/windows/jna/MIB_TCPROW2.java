/**
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         See full license in project root.
 * ==================================================================================================================================================== */
package org.appwork.processes.windows.jna;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinDef.DWORD;

/**
 * Windows MIB_TCPROW2 structure for IPv4 TCP connection table entries. Used by {@link WindowsJNAProcessUtils}.
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/tcpmib/ns-tcpmib-mib_tcprow2">MIB_TCPROW2</a>
 */
public class MIB_TCPROW2 extends Structure {
    public DWORD dwState;
    public DWORD dwLocalAddr;
    public DWORD dwLocalPort;
    public DWORD dwRemoteAddr;
    public DWORD dwRemotePort;
    public DWORD dwOwningPid;
    public int   dwOffloadState;

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList(new String[] { "dwState", "dwLocalAddr", "dwLocalPort", "dwRemoteAddr", "dwRemotePort", "dwOwningPid", "dwOffloadState" });
    }
}
