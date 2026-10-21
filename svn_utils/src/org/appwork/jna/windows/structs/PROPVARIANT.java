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

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.Variant;

/**
 * Minimal {@code PROPVARIANT} layout for pointer-sized values (e.g. {@link Variant#VT_LPWSTR}).
 * <p>
 * Enough for {@code IPropertyStore.SetValue} of AppUserModel relaunch strings. Keep the returned instance reachable until
 * {@code SetValue}/{@code Commit} returns (holds the native wide-string {@link Memory}).
 */
public class PROPVARIANT extends Structure {
    public short   vt;
    public short   reserved1;
    public short   reserved2;
    public short   reserved3;
    public Pointer value;

    public PROPVARIANT() {
        super();
    }

    public PROPVARIANT(final Pointer memory) {
        super(memory);
        this.read();
    }

    /**
     * Builds a {@link Variant#VT_LPWSTR} PROPVARIANT. Keep the returned object reachable until the native SetValue/Commit call returns
     * (the wide string {@link Memory} is referenced via {@link #value}).
     */
    @SuppressWarnings("resource")
    public static PROPVARIANT ofLpWStr(final String str) {
        final PROPVARIANT pv = new PROPVARIANT();
        pv.vt = (short) Variant.VT_LPWSTR;
        if (str != null) {
            final Memory mem = new Memory(((long) str.length() + 1L) * Native.WCHAR_SIZE);
            mem.setWideString(0, str);
            pv.value = mem;
        } else {
            pv.value = Pointer.NULL;
        }
        pv.write();
        return pv;
    }

    /**
     * Best-effort read of a {@link Variant#VT_LPWSTR} value previously filled by {@code IPropertyStore.GetValue}.
     */
    public String readLpWStr() {
        this.read();
        if (this.vt != (short) Variant.VT_LPWSTR || this.value == null || Pointer.nativeValue(this.value) == 0L) {
            return null;
        }
        try {
            return this.value.getWideString(0);
        } catch (final Throwable e) {
            return null;
        }
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("vt", "reserved1", "reserved2", "reserved3", "value");
    }
}
