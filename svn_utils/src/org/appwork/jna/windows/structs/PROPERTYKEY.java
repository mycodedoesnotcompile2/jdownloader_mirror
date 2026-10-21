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
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.jna.windows.structs;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.Guid;

/**
 * Win32 {@code PROPERTYKEY} ({@code fmtid} + {@code pid}).
 */
public class PROPERTYKEY extends Structure {
    public Guid.GUID fmtid;
    public int       pid;

    public PROPERTYKEY() {
        super();
    }

    public PROPERTYKEY(final Pointer memory) {
        super(memory);
        this.read();
    }

    public PROPERTYKEY(final Guid.GUID fmtid, final int pid) {
        this.fmtid = fmtid;
        this.pid = pid;
        this.write();
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("fmtid", "pid");
    }
}
