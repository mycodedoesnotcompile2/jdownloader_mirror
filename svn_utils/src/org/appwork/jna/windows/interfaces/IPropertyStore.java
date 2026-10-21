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
package org.appwork.jna.windows.interfaces;

import org.appwork.jna.windows.structs.PROPERTYKEY;
import org.appwork.jna.windows.structs.PROPVARIANT;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.WinNT.HRESULT;
import com.sun.jna.platform.win32.COM.Unknown;

/**
 * Minimal JNA binding for {@code IPropertyStore} (vtable indices after {@code IUnknown}).
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/propsys/nn-propsys-ipropertystore">IPropertyStore</a>
 */
public class IPropertyStore extends Unknown {
    public static final Guid.IID IID = new Guid.IID("{886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99}");

    public IPropertyStore() {
        super();
    }

    public IPropertyStore(final Pointer pvInstance) {
        super(pvInstance);
    }

    /** Vtable index 5 */
    public HRESULT GetValue(final PROPERTYKEY key, final PROPVARIANT pv) {
        return (HRESULT) this._invokeNativeObject(5, new Object[] { this.getPointer(), key, pv }, HRESULT.class);
    }

    /** Vtable index 6 */
    public HRESULT SetValue(final PROPERTYKEY key, final PROPVARIANT propvar) {
        return (HRESULT) this._invokeNativeObject(6, new Object[] { this.getPointer(), key, propvar }, HRESULT.class);
    }

    /** Vtable index 7 */
    public HRESULT Commit() {
        return (HRESULT) this._invokeNativeObject(7, new Object[] { this.getPointer() }, HRESULT.class);
    }
}
