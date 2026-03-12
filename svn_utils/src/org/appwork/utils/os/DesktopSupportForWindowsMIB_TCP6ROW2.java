/**
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         See full license in project root.
 * ==================================================================================================================================================== */
package org.appwork.utils.os;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinDef.DWORD;

/**
 * Windows MIB_TCP6ROW2 structure for IPv6 TCP connection table entries. Used by {@link DesktopSupportWindowsViaJNA}.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/tcpmib/ns-tcpmib-mib_tcp6row2">MIB_TCP6ROW2</a>
 */
public class DesktopSupportForWindowsMIB_TCP6ROW2 extends Structure {
    // IN6_ADDR LocalAddr (16 bytes)
    public byte[] LocalAddr = new byte[16];
    public DWORD  dwLocalScopeId;
    public DWORD  dwLocalPort;
    // IN6_ADDR RemoteAddr (16 bytes)
    public byte[] RemoteAddr = new byte[16];
    public DWORD  dwRemoteScopeId;
    public DWORD  dwRemotePort;
    public DWORD  State;
    public DWORD  dwOwningPid;
    public int    dwOffloadState;

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList(new String[] { "LocalAddr", "dwLocalScopeId", "dwLocalPort", "RemoteAddr", "dwRemoteScopeId", "dwRemotePort", "State", "dwOwningPid", "dwOffloadState" });
    }
}
