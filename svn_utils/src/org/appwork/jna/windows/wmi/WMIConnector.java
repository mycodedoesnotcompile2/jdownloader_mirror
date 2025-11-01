package org.appwork.jna.windows.wmi;

import java.io.Closeable;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.OaIdl.SAFEARRAY;
import com.sun.jna.platform.win32.OaIdl.VARIANT_BOOL;
import com.sun.jna.platform.win32.OaIdlUtil;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.OleAuto;
import com.sun.jna.platform.win32.Variant;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT.HRESULT;
import com.sun.jna.platform.win32.COM.COMException;
import com.sun.jna.platform.win32.COM.COMUtils;
import com.sun.jna.platform.win32.COM.Wbemcli;
import com.sun.jna.platform.win32.COM.Wbemcli.IEnumWbemClassObject;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemLocator;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemServices;
import com.sun.jna.ptr.IntByReference;

public class WMIConnector implements Closeable {
    /**
     *
     */
    public static final String                NAMESPACE_ROOT_CIMV2 = "ROOT\\CIMV2";
    /**
     *
     */
    public static final String                LOCALE_EN_MS_409     = "MS_409";
    private static final String               DEFAULT_LANGUAGE     = LOCALE_EN_MS_409;
    private static final String               DEFAULT_NAMESPACE    = NAMESPACE_ROOT_CIMV2;
    private static final Map<Integer, String> WMI_ERRORS           = JNAWMIUtils.WMI_ERRORS;
    private static volatile boolean           securityInitialized;
    private static final Object               SECURITY_LOCK        = new Object();
    private String                            namespace            = DEFAULT_NAMESPACE;
    private String                            language             = DEFAULT_LANGUAGE;
    private boolean                           comUnInitRequired;
    private Ole32                             ole32;
    private OleAuto                           oleAuto;
    private IWbemServices                     svc;
    private HRESULT                           hres;
    private final int                         timeout              = 60000;

    public void setNamespace(String namespace) {
        if (svc != null) {
            throw new IllegalStateException("Cannot change namespace after connection is established");
        }
        this.namespace = namespace;
    }

    public void setLanguage(String language) {
        if (svc != null) {
            throw new IllegalStateException("Cannot change language after connection is established");
        }
        this.language = language;
    }

    /**
     * @param string
     */
    public WMIConnector(String namespace) {
        this(namespace, DEFAULT_LANGUAGE);
    }

    /**
     *
     */
    public WMIConnector() {
        this(DEFAULT_NAMESPACE, DEFAULT_LANGUAGE);
    }

    /**
     * @param namespaceRootCimv2
     * @param localeEnMs409
     */
    public WMIConnector(String namespace, String locale) {
        this.namespace = namespace;
        this.language = locale;
        if (StringUtils.isEmpty(this.namespace)) {
            this.namespace = DEFAULT_NAMESPACE;
        }
        if (StringUtils.isEmpty(language)) {
            this.language = DEFAULT_LANGUAGE;
        }
    }

    public List<Map<String, Object>> query(String query, String... properties) throws TimeoutException, WMIException {
        if (svc == null) {
            open();
        }
        try {
            final ArrayList<Map<String, Object>> list = new ArrayList<Map<String, Object>>();
            final IntByReference pType = new IntByReference();
            // Send query
            final IEnumWbemClassObject enumRes = svc.ExecQuery("WQL", query, Wbemcli.WBEM_FLAG_FORWARD_ONLY | Wbemcli.WBEM_FLAG_RETURN_IMMEDIATELY, null);
            try {
                while (enumRes.getPointer() != Pointer.NULL) {
                    final Pointer[] pclsObj = new Pointer[1];
                    hres = enumRes.Next(timeout, pclsObj.length, pclsObj, new IntByReference(0));
                    if (hres.intValue() == Wbemcli.WBEM_S_FALSE || hres.intValue() == Wbemcli.WBEM_S_NO_MORE_DATA) {
                        break;
                    }
                    // Throw exception to notify user of timeout
                    if (hres.intValue() == Wbemcli.WBEM_S_TIMEDOUT) {
                        DebugMode.debugger();
                        throw new TimeoutException("No results after " + timeout + " ms: " + WMI_ERRORS.get(hres.intValue()));
                    }
                    // Other exceptions here.
                    if (COMUtils.FAILED(hres)) {
                        String message = WMI_ERRORS.get(hres.intValue());
                        throw new COMException(message == null ? "Unknown Error" : message, hres);
                    }
                    final Wbemcli.IWbemClassObject classObject = new ExtIWbemClassObject(pclsObj[0]);
                    try {
                        String propertiesList[];
                        if (properties == null || properties.length == 0) {
                            int retry = 0;
                            while (true) {
                                try {
                                    propertiesList = classObject.GetNames(null, JNAWMIUtils.WBEM_FLAG_NONSYSTEM_ONLY, null);
                                    break;
                                } catch (IllegalArgumentException e) {
                                    if (retry++ > 5) {
                                        throw e;
                                    }
                                }
                            }
                        } else {
                            propertiesList = properties;
                        }
                        final HashMap<String, Object> map = new HashMap<String, Object>();
                        int vtType = -1;
                        for (final String property : propertiesList) {
                            Variant.VARIANT.ByReference pVal = new Variant.VARIANT.ByReference();
                            try {
                                vtType = -1;
                                oleAuto.VariantInit(pVal);
                                COMUtils.checkRC(classObject.Get(property, 0, pVal, pType, null));
                                final Object pValValue = pVal.getValue();
                                vtType = (pValValue == null ? Variant.VT_NULL : pVal.getVarType()).intValue();
                                switch (vtType) {
                                case Variant.VT_I1:
                                case Variant.VT_UI1:
                                    map.put(property, ((Number) pValValue).byteValue());
                                    break;
                                case Variant.VT_I2:
                                case Variant.VT_UI2:
                                    map.put(property, ((Number) pValValue).shortValue());
                                    break;
                                case Variant.VT_I4:
                                case Variant.VT_UI4:
                                    map.put(property, ((Number) pValValue).intValue());
                                    break;
                                case Variant.VT_I8:
                                case Variant.VT_UI8:
                                    map.put(property, ((Number) pValValue).longValue());
                                    break;
                                case Variant.VT_BSTR:
                                    map.put(property, pVal.stringValue());
                                    break;
                                case Variant.VT_DATE:
                                    map.put(property, pVal.dateValue());
                                    break;
                                case Variant.VT_BOOL:
                                    map.put(property, ((VARIANT_BOOL) pValValue).booleanValue());
                                    break;
                                case Variant.VT_R4:
                                    map.put(property, ((Number) pValValue).floatValue());
                                    break;
                                case Variant.VT_R8:
                                    map.put(property, ((Number) pValValue).doubleValue());
                                    break;
                                case Variant.VT_NULL:
                                case Variant.VT_EMPTY:
                                    map.put(property, null);
                                    break;
                                default:
                                    boolean isarray = (vtType & Variant.VT_ARRAY) == Variant.VT_ARRAY;
                                    if (isarray) {
                                        // System.out.println("SafeArray");
                                        DebugMode.breakIf(!(pValValue instanceof SAFEARRAY));
                                        final Object array = OaIdlUtil.toPrimitiveArray((SAFEARRAY) pValValue, false);
                                        map.put(property, array);
                                    } else {
                                        DebugMode.breakIf(pValValue instanceof SAFEARRAY);
                                        map.put(property, null);
                                    }
                                    break;
                                }
                                try {
                                    oleAuto.VariantClear(pVal);
                                } catch (IllegalArgumentException e) {
                                    LogV3.log(e);
                                } finally {
                                    pVal = null;
                                }
                            } catch (final Throwable e) {
                                for (final Field f : Variant.class.getFields()) {
                                    try {
                                        if (f.getName().startsWith("VT_") && vtType == f.getInt(null)) {
                                            LogV3.info("Error Type: " + f.getName());
                                            break;
                                        }
                                    } catch (final Exception e1) {
                                        LogV3.log(e1);
                                    }
                                }
                                LogV3.info("Error Prop: " + property);
                                LogV3.log(e);
                                // if (manager != null) {
                                // manager.trackException(e);
                                // }
                            } finally {
                                if (pVal != null) {
                                    try {
                                        oleAuto.VariantClear(pVal);
                                    } catch (IllegalArgumentException e) {
                                        LogV3.log(e);
                                    }
                                }
                            }
                        }
                        list.add(map);
                    } finally {
                        classObject.Release();
                    }
                }
            } finally {
                enumRes.Release();
            }
            return list;
        } catch (final Exception org) {
            if (org instanceof COMException) {
                int code = ((COMException) org).getHresult().intValue();
                if (WMI_ERRORS.containsKey(code)) {
                    // Known error. no reason to try the fallback
                    LogV3.info("Known Error Code: " + code);
                    throw WMIException.wrap(org, "query:" + query + ";namespace:" + namespace + " Error: " + code);
                }
            }
            throw WMIException.wrap(org, "query:" + query + ";namespace:" + namespace);
        }
    }

    public void open() throws WMIException {
        try {
            oleAuto = OleAuto.INSTANCE;
            ole32 = Ole32.INSTANCE;
            comUnInitRequired = false;
            hres = ole32.CoInitializeEx(null, Ole32.COINIT_MULTITHREADED);
            // LogV3.info("COM CoInitializeEx - hres: " + hres.intValue() + " (0x" + Integer.toHexString(hres.intValue()) + ")");
            switch (hres.intValue()) {
            // Successful local initialization (S_OK) or was already initialized
            // (S_FALSE) but still needs uninit
            case COMUtils.S_OK:
            case COMUtils.S_FALSE:
                comUnInitRequired = true;
                break;
            // COM was already initialized with a different threading model
            case WinError.RPC_E_CHANGED_MODE:
                DebugMode.debugger();
                // LogV3.info("COM already initialized with different threading model, trying COINIT_APARTMENTTHREADED");
                hres = ole32.CoInitializeEx(null, Ole32.COINIT_APARTMENTTHREADED);
                // LogV3.info("COM CoInitializeEx#2 - hres: " + hres.intValue() + " (0x" + Integer.toHexString(hres.intValue()) +
                // ")");
                switch (hres.intValue()) {
                // Successful local initialization (S_OK) or was already initialized
                // (S_FALSE) but still needs uninit
                case COMUtils.S_OK:
                case COMUtils.S_FALSE:
                    comUnInitRequired = true;
                    break;
                default:
                    throw new COMException("Failed to initialize COM library #2. Error: " + hres.intValue(), hres);
                }
                break;
            // COM already initialized - this is fine, we can proceed
            case WinError.CO_E_ALREADYINITIALIZED: // CO_E_ALREADYINITIALIZED
                DebugMode.debugger();
                // LogV3.info("COM already initialized - hres: " + hres.intValue() + " (0x" + Integer.toHexString(hres.intValue()) +
                // ")");
                break;
            default:
                throw new COMException("Failed to initialize COM library #1. Error: " + hres.intValue(), hres);
            }
            if (true) {
                synchronized (SECURITY_LOCK) {
                    if (!securityInitialized) {
                        // Initializes COM security settings for the current thread to safely access the local WMI (Windows
                        // Management
                        // Instrumentation) database.
                        // This is important because WMI operations require secure communication between the client (your code) and
                        // the
                        // WMI
                        // service.
                        // - RPC_C_AUTHN_LEVEL_DEFAULT: Ensures that all WMI calls are authenticated, preventing unauthorized access
                        // to
                        // system management data.
                        // - RPC_C_IMP_LEVEL_IMPERSONATE: Allows the WMI service to perform operations on behalf of the client,
                        // which is
                        // necessary for tasks like querying system information.
                        // - EOAC_NONE: Specifies that no additional capabilities are required.
                        // Without these security settings, the COM infrastructure may deny access to the WMI service, or it could
                        // result in
                        // insecure operations.
                        hres = ole32.CoInitializeSecurity(null, -1, null, null, Ole32.RPC_C_AUTHN_LEVEL_DEFAULT, Ole32.RPC_C_IMP_LEVEL_IMPERSONATE, null, Ole32.EOAC_NONE, null);
                        // If security already initialized we get RPC_E_TOO_LATE
                        // This can be safely ignored
                        if (COMUtils.FAILED(hres) && hres.intValue() != WinError.RPC_E_TOO_LATE) {
                            throw new COMException("Failed to initialize security: " + JNAWMIUtils.WMI_ERRORS.get(hres.intValue()), hres);
                        }
                        // LogV3.info("CoInitializeSecurity - set COM security configuration for this process");
                        securityInitialized = true;
                    }
                }
            }
            // Obtain the initial locator to WMI -
            final IWbemLocator loc = IWbemLocator.create();
            if (loc == null) {
                throw new COMException("Failed to create WbemLocator object.");
            }
            try {
                // Connect to WMI through the IWbemLocator::ConnectServer method
                // Connect to the namespace with the current user and obtain pointer
                // pSvc to make IWbemServices calls.
                svc = loc.ConnectServer(namespace, null, null, language, 0, null, null);
                // Set security levels on the proxy -
                hres = ole32.CoSetProxyBlanket(svc, Ole32.RPC_C_AUTHN_WINNT, Ole32.RPC_C_AUTHZ_NONE, null, Ole32.RPC_C_AUTHN_LEVEL_CALL, Ole32.RPC_C_IMP_LEVEL_IMPERSONATE, null, Ole32.EOAC_NONE);
                if (COMUtils.FAILED(hres)) {
                    // Clean up svc before throwing exception
                    if (svc != null) {
                        svc.Release();
                        svc = null;
                    }
                    throw new COMException("Could not set proxy blanket: " + JNAWMIUtils.WMI_ERRORS.get(hres.intValue()), hres);
                }
            } finally {
                // Release the locator. If successful, pSvc contains connection
                // information
                loc.Release();
            }
        } catch (final Exception org) {
            throw WMIException.wrap(org, "Failed to open COM Connection");
        }
    }

    /**
     * @see java.io.Closeable#close()
     */
    @Override
    public void close() throws IOException {
        try {
            final IWbemServices svc = this.svc;
            this.svc = null;
            if (svc != null) {
                svc.Release();
            }
        } catch (Exception e) {
            LogV3.log(e);
        } finally {
            // Always cleanup COM resources
            try {
                final Ole32 ole32 = this.ole32;
                if (comUnInitRequired && ole32 != null) {
                    ole32.CoUninitialize();
                    comUnInitRequired = false;
                }
            } catch (Exception e) {
                LogV3.log(e);
            } finally {
                // Clear references
                ole32 = null;
                oleAuto = null;
            }
        }
    }
}
