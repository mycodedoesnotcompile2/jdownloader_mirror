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
import com.sun.jna.Structure;
import com.sun.jna.Structure.FieldOrder;
import com.sun.jna.platform.win32.WinDef.DWORD;

/**
 * Windows MIB_TCPTABLE2 structure for IPv4 TCP connection table. Used by {@link WindowsJNAProcessUtils}.
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/tcpmib/ns-tcpmib-mib_tcptable2">MIB_TCPTABLE2</a>
 */
@FieldOrder({ "dwNumEntries", "table" })
public class MIB_TCPTABLE2 extends Structure {
    public DWORD         dwNumEntries;
    public MIB_TCPROW2[] table = new MIB_TCPROW2[] { new MIB_TCPROW2() };

    public MIB_TCPTABLE2() {
    }

    public MIB_TCPTABLE2(final int size) {
        this.dwNumEntries = new DWORD(size);
        this.table = new MIB_TCPROW2[size];
    }

    public MIB_TCPTABLE2(final Memory mem) {
        super(mem);
        this.read();
    }

    @Override
    public void read() {
        super.read();
        if (this.dwNumEntries.intValue() > 0) {
            this.table = (MIB_TCPROW2[]) this.table[0].toArray(this.dwNumEntries.intValue());
        } else {
            this.table = new MIB_TCPROW2[] { new MIB_TCPROW2() };
        }
    }
}
