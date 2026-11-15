/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
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
