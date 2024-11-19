package org.appwork.jna.wmi;

import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.OaIdl.SAFEARRAY;
import com.sun.jna.platform.win32.OaIdlUtil;
import com.sun.jna.platform.win32.Variant.VARIANT;
import com.sun.jna.platform.win32.WinNT.HRESULT;
import com.sun.jna.platform.win32.COM.COMUtils;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemClassObject;
import com.sun.jna.ptr.PointerByReference;
//[in, string] LPCWSTR wszName,
//[in] long lFlags,
//[out, OPTIONAL] VARIANT* pVal,
//[out, OPTIONAL] CIMTYPE* pType,
//[out, OPTIONAL] long* plFlavor
//);
//
//HRESULT Put(
//[in, string] LPCWSTR wszName,
//[in] long lFlags,
//[in] VARIANT* pVal,
//[in] CIMTYPE Type
//);
//
//HRESULT Delete(
//[in, string] LPCWSTR wszName
//);
//
//HRESULT GetNames(
//[in, string] LPCWSTR wszQualifierName,
//[in] long lFlags,
//[in] VARIANT* pQualifierVal,
//[out] SAFEARRAY (BSTR)* pNames
//);

//https://github.com/java-native-access/jna/pull/1084/commits/545c3019cbb6136fe0b6ca5154b685b64545c6b6
public class ExtIWbemClassObject extends IWbemClassObject {
    public HRESULT GetNames(String wszQualifierName, int lFlags, VARIANT.ByReference pQualifierVal, PointerByReference pNames) {
        return this.GetNames(wszQualifierName == null ? null : new WString(wszQualifierName), lFlags, pQualifierVal, pNames);
    }

    public HRESULT GetNames(WString wszQualifierName, int lFlags, VARIANT.ByReference pQualifierVal, PointerByReference pNames) {
        // 8th method in IWbemClassObjectVtbl
        return (HRESULT) this._invokeNativeObject(7, new Object[] { this.getPointer(), wszQualifierName, lFlags, pQualifierVal, pNames }, HRESULT.class);
    }

    public String[] GetNames(String wszQualifierName, int lFlags, VARIANT.ByReference pQualifierVal) {
        PointerByReference pbr = new PointerByReference();
        COMUtils.checkRC(this.GetNames(wszQualifierName, lFlags, pQualifierVal, pbr));
        Object[] nameObjects = (Object[]) OaIdlUtil.toPrimitiveArray(new SAFEARRAY(pbr.getValue()), true);
        String[] names = new String[nameObjects.length];
        for (int i = 0; i < nameObjects.length; i++) {
            names[i] = (String) nameObjects[i];
        }
        return names;
    }

    public ExtIWbemClassObject() {
        super();
    }

    public ExtIWbemClassObject(Pointer pvInstance) {
        super(pvInstance);
    }
}
