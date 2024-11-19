package org.appwork.jna.wmi;

import java.io.Closeable;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;
import java.util.concurrent.TimeoutException;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.DebugMode;
import org.appwork.utils.NameInterface;
import org.appwork.utils.StringUtils;

import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.OaIdl.SAFEARRAY;
import com.sun.jna.platform.win32.OaIdlUtil;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.OleAuto;
import com.sun.jna.platform.win32.Variant;
import com.sun.jna.platform.win32.Variant.VARIANT;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HRESULT;
import com.sun.jna.platform.win32.COM.COMException;
import com.sun.jna.platform.win32.COM.COMUtils;
import com.sun.jna.platform.win32.COM.Wbemcli;
import com.sun.jna.platform.win32.COM.Wbemcli.IEnumWbemClassObject;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemLocator;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemServices;
import com.sun.jna.ptr.IntByReference;

public class WMIConnector implements Closeable {
    private IWbemServices svc;
    private static String OS_LANGUAGE_CODE;

    public WMIConnector() {
        this.initCOM();
        this.svc = connectServer();
        if (OS_LANGUAGE_CODE == null) {
            // no sync..risk multiple execution
            ArrayList<HashMap<String, Object>> wmiResult;
            try {
                wmiResult = this.executeQuery(null, "SELECT * from Win32_OperatingSystem");
                final int languageCode = ((Number) wmiResult.get(0).get("OSLanguage")).intValue();
                OS_LANGUAGE_CODE = "ms_" + StringUtils.fillPre(Integer.toHexString(languageCode), "0", 3);
                this.svc.Release();
                this.svc = connectServer();
            } catch (final TimeoutException e) {
                LogV3.log(e);
            } catch (final WMIQueryFailedException e) {
                LogV3.log(e);
            }
        }
    }

    public static IWbemServices connectServer() {
        // Step 3: ---------------------------------------------------
        // Obtain the initial locator to WMI -------------------------
        final IWbemLocator loc = IWbemLocator.create();
        if (loc == null) {
            throw new COMException("Failed to create WbemLocator object.");
        }
        // Step 4: -----------------------------------------------------
        // Connect to WMI through the IWbemLocator::ConnectServer method
        // Connect to the namespace with the current user and obtain pointer
        // pSvc to make IWbemServices calls.
        final IWbemServices services = loc.ConnectServer("ROOT\\CIMV2", null, null, OS_LANGUAGE_CODE, 0, null, null);
        // Release the locator. If successful, pSvc contains connection
        // information
        loc.Release();
        // Step 5: --------------------------------------------------
        // Set security levels on the proxy -------------------------
        final HRESULT hres = Ole32.INSTANCE.CoSetProxyBlanket(services, Ole32.RPC_C_AUTHN_WINNT, Ole32.RPC_C_AUTHZ_NONE, null, Ole32.RPC_C_AUTHN_LEVEL_CALL, Ole32.RPC_C_IMP_LEVEL_IMPERSONATE, null, Ole32.EOAC_NONE);
        if (COMUtils.FAILED(hres)) {
            services.Release();
            throw new COMException("Could not set proxy blanket.", hres);
        }
        return services;
    }

    private static final int WMITIMEOUT   = 60000;
    private int              comThreading = Ole32.COINIT_MULTITHREADED;
    private boolean          securityInitialized;

    private ArrayList<HashMap<String, Object>> readValues(final NameInterface[] fields, final IEnumWbemClassObject enumerator) throws TimeoutException {
        final ArrayList<HashMap<String, Object>> ret = new ArrayList<HashMap<String, Object>>();
        ;
        HashMap<String, NameInterface> all = null;
        if (fields != null) {
            all = new HashMap<String, NameInterface>();
            for (final Object a : fields) {
                all.put(((NameInterface) a).name().toLowerCase(Locale.ENGLISH), (NameInterface) a);
            }
        }
        while (enumerator.getPointer() != Pointer.NULL) {
            final Pointer[] pclsObj = new Pointer[1];
            // Enumerator will be released by calling method so no need to
            // release it here.
            final HRESULT hres = enumerator.Next(WMITIMEOUT, pclsObj.length, pclsObj, new IntByReference(0));
            // Enumeration complete or no more data; we're done, exit the loop
            if (hres.intValue() == Wbemcli.WBEM_S_FALSE || hres.intValue() == Wbemcli.WBEM_S_NO_MORE_DATA) {
                break;
            }
            // Throw exception to notify user of timeout
            if (hres.intValue() == Wbemcli.WBEM_S_TIMEDOUT) {
                DebugMode.debugger();
                throw new TimeoutException("No results after " + WMITIMEOUT + " ms.");
            }
            // Other exceptions here.
            if (COMUtils.FAILED(hres)) {
                throw new COMException("Failed to enumerate results.", hres);
            }
            HashMap<String, Object> map;
            ret.add(map = new HashMap<String, Object>());
            // Get the value of the properties
            final ExtIWbemClassObject clsObj = new ExtIWbemClassObject(pclsObj[0]);
            // System.out.println("Read 1");
            try {
                for (String property : clsObj.GetNames(null, 0, null)) {
                    final VARIANT.ByReference pVal = new VARIANT.ByReference();
                    final IntByReference pType = new IntByReference();
                    try {
                        if (all != null) {
                            final NameInterface NameInterface = all.get(property.toLowerCase(Locale.ENGLISH));
                            if (NameInterface == null) {
                                continue;
                            }
                            property = NameInterface.name();
                        }
                        // System.out.println("Read 2 " + property);
                        final HRESULT result = clsObj.Get(new WString(property), 0, pVal, pType, null);
                        if (!W32Errors.SUCCEEDED(result.intValue())) {
                            throw new Win32Exception(result);
                        }
                        // System.out.println("Read 3");
                        final int vtType = (pVal.getValue() == null ? Variant.VT_NULL : pVal.getVarType()).intValue();
                        // final int cimType = pType.getValue();
                        switch (vtType) {
                        case Variant.VT_BSTR:
                            map.put(property, pVal.stringValue());
                            break;
                        case Variant.VT_I4:
                            map.put(property, pVal.intValue());
                            break;
                        case Variant.VT_UI1:
                            map.put(property, pVal.byteValue());
                            break;
                        case Variant.VT_I2:
                            map.put(property, pVal.shortValue());
                            break;
                        case Variant.VT_BOOL:
                            map.put(property, pVal.booleanValue());
                            break;
                        case Variant.VT_R4:
                            map.put(property, pVal.floatValue());
                            break;
                        case Variant.VT_R8:
                            map.put(property, pVal.doubleValue());
                            break;
                        case Variant.VT_NULL:
                            map.put(property, null);
                            break;
                        default:
                            // System.out.println("Read 4");
                            final Object value = pVal.getValue();
                            if (value instanceof SAFEARRAY) {
                                map.put(property, OaIdlUtil.toPrimitiveArray((SAFEARRAY) value, false));
                                break;
                            }
                            throw new WTFException("Unsupported");
                        // map.put(property), pVal.getValue());
                        }
                    } catch (final Throwable e) {
                        LogV3.defaultLogger().exception("Failed to get Data for " + property, e);
                    } finally {
                        OleAuto.INSTANCE.VariantClear(pVal);
                    }
                }
            } finally {
                clsObj.Release();
            }
        }
        return ret;
    }

    protected boolean initCOM(final int coInitThreading) {
        final WinNT.HRESULT hres = Ole32.INSTANCE.CoInitializeEx(null, coInitThreading);
        switch (hres.intValue()) {
        // Successful local initialization (S_OK) or was already initialized
        // (S_FALSE) but still needs uninit
        case COMUtils.S_OK:
        case COMUtils.S_FALSE:
            return true;
        // COM was already initialized with a different threading model
        case WinError.RPC_E_CHANGED_MODE:
            return false;
        // Any other results is impossible
        default:
            throw new COMException("Failed to initialize COM library.", hres);
        }
    }

    /**
     * Initializes COM library and sets security to impersonate the local user
     *
     * @return True if COM was initialized and needs to be uninitialized, false otherwise
     */
    public boolean initCOM() {
        boolean comInit = false;
        // Step 1: --------------------------------------------------
        // Initialize COM. ------------------------------------------
        comInit = this.initCOM(this.getComThreading());
        if (!comInit) {
            comInit = this.initCOM(this.switchComThreading());
        }
        // Step 2: --------------------------------------------------
        // Set general COM security levels --------------------------
        if (comInit && !this.securityInitialized) {
            final WinNT.HRESULT hres = Ole32.INSTANCE.CoInitializeSecurity(null, -1, null, null, Ole32.RPC_C_AUTHN_LEVEL_DEFAULT, Ole32.RPC_C_IMP_LEVEL_IMPERSONATE, null, Ole32.EOAC_NONE, null);
            // If security already initialized we get RPC_E_TOO_LATE
            // This can be safely ignored
            if (COMUtils.FAILED(hres) && hres.intValue() != WinError.RPC_E_TOO_LATE) {
                Ole32.INSTANCE.CoUninitialize();
                throw new COMException("Failed to initialize security.", hres);
            }
            this.securityInitialized = true;
        }
        return comInit;
    }

    /**
     * Switches the current threading model for COM initialization, as OSHI is required to match if an external program has COM initialized
     * already.
     *
     * @return The new threading model after switching
     */
    public int switchComThreading() {
        if (this.comThreading == Ole32.COINIT_APARTMENTTHREADED) {
            this.comThreading = Ole32.COINIT_MULTITHREADED;
        } else {
            this.comThreading = Ole32.COINIT_APARTMENTTHREADED;
        }
        return this.comThreading;
    }

    /**
     * Returns the current threading model for COM initialization, as OSHI is required to match if an external program has COM initialized
     * already.
     *
     * @return The current threading model
     */
    public int getComThreading() {
        return this.comThreading;
    }

    public void dispose() {
        try {
            this.svc.Release();
        } catch (final Throwable e) {
            LogV3.log(e);
        }
        Ole32.INSTANCE.CoUninitialize();
    }

    public ArrayList<HashMap<String, Object>> executeQuery(final NameInterface[] fields, final String query) throws TimeoutException, WMIQueryFailedException {
        try {
            final IEnumWbemClassObject cursor = this.svc.ExecQuery("WQL", query, Wbemcli.WBEM_FLAG_FORWARD_ONLY | Wbemcli.WBEM_FLAG_RETURN_IMMEDIATELY | 0x20000, null);
            try {
                return this.readValues(fields, cursor);
            } finally {
                cursor.Release();
            }
        } catch (final RuntimeException e) {
            throw new WMIQueryFailedException(e);
        }
    }

    @Override
    public void close() throws IOException {
        // try {
        this.svc.Release();
        // } catch (final Throwable e) {
        // LogV3.log(e);
        // }
        Ole32.INSTANCE.CoUninitialize();
    }
}
