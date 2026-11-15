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
