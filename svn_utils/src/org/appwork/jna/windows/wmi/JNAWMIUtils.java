/**
 *
 */
package org.appwork.jna.windows.wmi;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.LogToStdOutSink;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Joiner;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.command.Command;
import org.appwork.utils.processes.command.ProcessOutputHandler;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.OaIdl.SAFEARRAY;
import com.sun.jna.platform.win32.OaIdlUtil;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.OleAuto;
import com.sun.jna.platform.win32.Variant;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.COM.COMException;
import com.sun.jna.platform.win32.COM.COMUtils;
import com.sun.jna.platform.win32.COM.Wbemcli;
import com.sun.jna.platform.win32.COM.Wbemcli.IEnumWbemClassObject;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemLocator;
import com.sun.jna.platform.win32.COM.Wbemcli.IWbemServices;
import com.sun.jna.ptr.IntByReference;

/**
 *
 */
public class JNAWMIUtils {
    /**
     *
     */
    private static final String         DEFAULT_LANGUAGE = "MS_409";
    /**
     *
     */
    private static Map<Integer, String> WMI_ERRORS       = new HashMap<Integer, String>();
    static {
        // via chatGPT from https://learn.microsoft.com/de-de/windows/win32/wmisdk/wmi-error-constants - MAY BE WRONG!
        WMI_ERRORS.put(0x80041001, "WBEM_E_FAILED");
        WMI_ERRORS.put(0x80041002, "WBEM_E_NOT_FOUND");
        WMI_ERRORS.put(0x80041003, "WBEM_E_ACCESS_DENIED");
        WMI_ERRORS.put(0x80041004, "WBEM_E_PROVIDER_FAILURE");
        WMI_ERRORS.put(0x80041005, "WBEM_E_TYPE_MISMATCH");
        WMI_ERRORS.put(0x80041006, "WBEM_E_OUT_OF_MEMORY");
        WMI_ERRORS.put(0x80041007, "WBEM_E_INVALID_CONTEXT");
        WMI_ERRORS.put(0x80041008, "WBEM_E_INVALID_PARAMETER");
        WMI_ERRORS.put(0x80041009, "WBEM_E_NOT_AVAILABLE");
        WMI_ERRORS.put(0x8004100A, "WBEM_E_CRITICAL_ERROR");
        WMI_ERRORS.put(0x8004100B, "WBEM_E_INVALID_STREAM");
        WMI_ERRORS.put(0x8004100C, "WBEM_E_NOT_SUPPORTED");
        WMI_ERRORS.put(0x8004100D, "WBEM_E_INVALID_SUPERCLASS");
        WMI_ERRORS.put(0x8004100E, "WBEM_E_INVALID_NAMESPACE");
        WMI_ERRORS.put(0x8004100F, "WBEM_E_INVALID_OBJECT");
        WMI_ERRORS.put(0x80041010, "WBEM_E_INVALID_CLASS");
        WMI_ERRORS.put(0x80041011, "WBEM_E_PROVIDER_NOT_FOUND");
        WMI_ERRORS.put(0x80041012, "WBEM_E_INVALID_PROVIDER_REGISTRATION");
        WMI_ERRORS.put(0x80041013, "WBEM_E_PROVIDER_LOAD_FAILURE");
        WMI_ERRORS.put(0x80041014, "WBEM_E_INITIALIZATION_FAILURE");
        WMI_ERRORS.put(0x80041015, "WBEM_E_TRANSPORT_FAILURE");
        WMI_ERRORS.put(0x80041016, "WBEM_E_INVALID_OPERATION");
        WMI_ERRORS.put(0x80041017, "WBEM_E_INVALID_QUERY");
        WMI_ERRORS.put(0x80041018, "WBEM_E_INVALID_QUERY_TYPE");
        WMI_ERRORS.put(0x80041019, "WBEM_E_ALREADY_EXISTS");
        WMI_ERRORS.put(0x8004101A, "WBEM_E_OVERRIDE_NOT_ALLOWED");
        WMI_ERRORS.put(0x8004101B, "WBEM_E_PROPAGATED_QUALIFIER");
        WMI_ERRORS.put(0x8004101C, "WBEM_E_PROPAGATED_PROPERTY");
        WMI_ERRORS.put(0x8004101D, "WBEM_E_UNEXPECTED");
        WMI_ERRORS.put(0x8004101E, "WBEM_E_ILLEGAL_OPERATION");
        WMI_ERRORS.put(0x8004101F, "WBEM_E_CANNOT_BE_KEY");
        WMI_ERRORS.put(0x80041020, "WBEM_E_INCOMPLETE_CLASS");
        WMI_ERRORS.put(0x80041021, "WBEM_E_INVALID_SYNTAX");
        WMI_ERRORS.put(0x80041022, "WBEM_E_NONDECORATED_OBJECT");
        WMI_ERRORS.put(0x80041023, "WBEM_E_READ_ONLY");
        WMI_ERRORS.put(0x80041024, "WBEM_E_PROVIDER_NOT_CAPABLE");
        WMI_ERRORS.put(0x80041025, "WBEM_E_CLASS_HAS_CHILDREN");
        WMI_ERRORS.put(0x80041026, "WBEM_E_CLASS_HAS_INSTANCES");
        WMI_ERRORS.put(0x80041027, "WBEM_E_QUERY_NOT_IMPLEMENTED");
        WMI_ERRORS.put(0x80041028, "WBEM_E_ILLEGAL_NULL");
        WMI_ERRORS.put(0x80041029, "WBEM_E_INVALID_QUALIFIER_TYPE");
        WMI_ERRORS.put(0x8004102A, "WBEM_E_INVALID_PROPERTY_TYPE");
        WMI_ERRORS.put(0x8004102B, "WBEM_E_VALUE_OUT_OF_RANGE");
        WMI_ERRORS.put(0x8004102C, "WBEM_E_CANNOT_BE_SINGLETON");
        WMI_ERRORS.put(0x8004102D, "WBEM_E_INVALID_CIM_TYPE");
        WMI_ERRORS.put(0x8004102E, "WBEM_E_INVALID_METHOD");
        WMI_ERRORS.put(0x8004102F, "WBEM_E_INVALID_METHOD_PARAMETERS");
        WMI_ERRORS.put(0x80041030, "WBEM_E_SYSTEM_PROPERTY");
        WMI_ERRORS.put(0x80041031, "WBEM_E_INVALID_PROPERTY");
        WMI_ERRORS.put(0x80041032, "WBEM_E_CALL_CANCELLED");
        WMI_ERRORS.put(0x80041033, "WBEM_E_SHUTTING_DOWN");
    }
    /**
     *
     */
    private static final String PRODUCT_STATE = "productState";
    /**
     *
     */
    private static final String DISPLAY_NAME  = "displayName";
    private static final int    TIMEOUT       = 60000;
    private static boolean      SECURITY_INITIALIZED;

    private static String decodeProductState(final int productState) {
        // SignatureStatus = 0x000000F0
        // ProductOwner = 0x00000F00
        // ProductState = 0x0000F000
        final String productStateStr = getProductStateDescription(productState & 0x0000F000);
        final String productOwnerStr = getProductOwnerDescription((productState & 0x00000F00));
        final String signatureStatusStr = getSignatureStatusDescription((productState & 0x000000F0));
        // System.out.println("Converted Big Endian Product State: " + Integer.toHexString(productState));
        // System.out.println("Product State: " + productStateStr);
        // System.out.println("Product Owner: " + productOwnerStr);
        // System.out.println("Signature Status: " + signatureStatusStr);
        // System.out.println("-");
        return StringUtils.join(",", productStateStr, signatureStatusStr, productOwnerStr);
    }

    private static String getProductStateDescription(final int productState) {
        switch (productState) {
        case 0x0000:
            return "off";
        case 0x1000:
            return "on";
        case 0x2000:
            return "snoozed";
        case 0x3000:
            return "expired";
        default:
            return "unknown_" + Integer.toHexString(productState);
        }
    }

    private static String getProductOwnerDescription(final int productOwner) {
        switch (productOwner) {
        case 0x000:
            return "non_ms";
        case 0x100:
            return "ms";
        default:
            return "unknown_" + Integer.toHexString(productOwner);
        }
    }

    private static String getSignatureStatusDescription(final int signatureStatus) {
        switch (signatureStatus) {
        case 0x00:
            return "up_to_date";
        case 0x10:
            return "out_of_date";
        default:
            return "unknown_" + Integer.toHexString(signatureStatus);
        }
    }

    public static ArrayList<Map<String, Object>> query(String namespace, final String query, String... properties) throws WMIException, InterruptedException {
        if (namespace == null) {
            namespace = "ROOT\\CIMV2";
        }
        // DebugMode.debugger();
        final ArrayList<Map<String, Object>> list = new ArrayList<Map<String, Object>>();
        try {
            // synchronized (JNAWMIUtils.class)
            {
                boolean comUnInitRequired = false;
                try {
                    WinNT.HRESULT hres = Ole32.INSTANCE.CoInitializeEx(null, Ole32.COINIT_MULTITHREADED);
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
                        hres = Ole32.INSTANCE.CoInitializeEx(null, Ole32.COINIT_APARTMENTTHREADED);
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
                        synchronized (JNAWMIUtils.class) {
                            if (!SECURITY_INITIALIZED) {
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
                                hres = Ole32.INSTANCE.CoInitializeSecurity(null, -1, null, null, Ole32.RPC_C_AUTHN_LEVEL_DEFAULT, Ole32.RPC_C_IMP_LEVEL_IMPERSONATE, null, Ole32.EOAC_NONE, null);
                                // If security already initialized we get RPC_E_TOO_LATE
                                // This can be safely ignored
                                if (COMUtils.FAILED(hres) && hres.intValue() != WinError.RPC_E_TOO_LATE) {
                                    throw new COMException("Failed to initialize security: " + WMI_ERRORS.get(hres.intValue()), hres);
                                }
                                // LogV3.info("CoInitializeSecurity - set COM security configuration for this process");
                                SECURITY_INITIALIZED = true;
                            }
                        }
                    }
                    // Obtain the initial locator to WMI -
                    final IWbemLocator loc = IWbemLocator.create();
                    if (loc == null) {
                        throw new COMException("Failed to create WbemLocator object.");
                    }
                    // Connect to WMI through the IWbemLocator::ConnectServer method
                    // Connect to the namespace with the current user and obtain pointer
                    // pSvc to make IWbemServices calls.
                    final IWbemServices svc = loc.ConnectServer(namespace, null, null, DEFAULT_LANGUAGE, 0, null, null);
                    final IntByReference pType = new IntByReference();
                    try {
                        // Release the locator. If successful, pSvc contains connection
                        // information
                        loc.Release();
                        // Set security levels on the proxy -
                        hres = Ole32.INSTANCE.CoSetProxyBlanket(svc, Ole32.RPC_C_AUTHN_WINNT, Ole32.RPC_C_AUTHZ_NONE, null, Ole32.RPC_C_AUTHN_LEVEL_CALL, Ole32.RPC_C_IMP_LEVEL_IMPERSONATE, null, Ole32.EOAC_NONE);
                        if (COMUtils.FAILED(hres)) {
                            throw new COMException("Could not set proxy blanket: " + WMI_ERRORS.get(hres.intValue()), hres);
                        }
                        // Send query
                        final IEnumWbemClassObject enumRes = svc.ExecQuery("WQL", query, Wbemcli.WBEM_FLAG_FORWARD_ONLY | Wbemcli.WBEM_FLAG_RETURN_IMMEDIATELY, null);
                        try {
                            while (enumRes.getPointer() != Pointer.NULL) {
                                final Pointer[] pclsObj = new Pointer[1];
                                hres = enumRes.Next(TIMEOUT, pclsObj.length, pclsObj, new IntByReference(0));
                                if (hres.intValue() == Wbemcli.WBEM_S_FALSE || hres.intValue() == Wbemcli.WBEM_S_NO_MORE_DATA) {
                                    break;
                                }
                                // Throw exception to notify user of timeout
                                if (hres.intValue() == Wbemcli.WBEM_S_TIMEDOUT) {
                                    DebugMode.debugger();
                                    throw new TimeoutException("No results after " + TIMEOUT + " ms: " + WMI_ERRORS.get(hres.intValue()));
                                }
                                // Other exceptions here.
                                if (COMUtils.FAILED(hres)) {
                                    String message = WMI_ERRORS.get(hres.intValue());
                                    throw new COMException(message == null ? "Unknown Error" : message, hres);
                                }
                                final Wbemcli.IWbemClassObject classObject = new ExtIWbemClassObject(pclsObj[0]);
                                ;
                                try {
                                    if (properties == null || properties.length == 0) {
                                        properties = classObject.GetNames(null, 0, null);
                                    }
                                    final HashMap<String, Object> map = new HashMap<String, Object>();
                                    int vtType = -1;
                                    for (final String property : properties) {
                                        try {

                                            Variant.VARIANT.ByReference pVal = new Variant.VARIANT.ByReference();
                                            try {
                                                vtType = -1;
                                                OleAuto.INSTANCE.VariantInit(pVal);
                                                COMUtils.checkRC(classObject.Get(property, 0, pVal, pType, null));
                                                // final Object value = pVal.getValue();
                                                vtType = (pVal.getValue() == null ? Variant.VT_NULL : pVal.getVarType()).intValue();
                                                // final int cimType = pType.getValue();
                                                switch (vtType) {
                                                case Variant.VT_I1:
                                                case Variant.VT_UI1:
                                                    map.put(property, pVal.byteValue());
                                                    break;
                                                case Variant.VT_I2:
                                                case Variant.VT_UI2:
                                                    map.put(property, pVal.shortValue());
                                                    break;
                                                case Variant.VT_I4:
                                                case Variant.VT_UI4:
                                                    map.put(property, pVal.intValue());
                                                    break;
                                                case Variant.VT_I8:
                                                case Variant.VT_UI8:
                                                    map.put(property, pVal.longValue());
                                                    break;
                                                case Variant.VT_BSTR:
                                                    map.put(property, pVal.stringValue());
                                                    break;
                                                case Variant.VT_DATE:
                                                    map.put(property, pVal.dateValue());
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
                                                case Variant.VT_EMPTY:
                                                    map.put(property, null);
                                                    break;

                                                default:

                                                    boolean isarray = (vtType & Variant.VT_ARRAY) == Variant.VT_ARRAY;

                                                    if (isarray) {
                                                        try {
                                                            final Object value = pVal.getValue();
                                                            // System.out.println("SafeArray");
                                                            DebugMode.breakIf(!(value instanceof SAFEARRAY));
                                                            final Object array = OaIdlUtil.toPrimitiveArray((SAFEARRAY) value, false);
                                                            // warning: Chat gpt warns that we should not use destroy but work with
                                                            // clearVariant only. We should evaluate if this is true
                                                            // clearVariant: COM cares about cleanup
                                                            // .destroy: Java/JNA cares about cleanup
                                                            ((SAFEARRAY) value).destroy();
                                                            // set value to empty to avoid that it gets freed another time.
                                                            pVal.setValue(Variant.VT_EMPTY, null);
                                                            pVal = null;
                                                            map.put(property, array);
                                                        } catch (RuntimeException e) {
                                                            e.printStackTrace();
                                                            DebugMode.debugger();
                                                            throw e;
                                                        } catch (Error e) {
                                                            e.printStackTrace();
                                                            DebugMode.debugger();
                                                            throw e;
                                                        }
                                                    } else {
                                                        final Object value = pVal.getValue();
                                                        DebugMode.breakIf(value instanceof SAFEARRAY);

                                                        map.put(property, null);
                                                    }
                                                }
                                            } finally {

                                                if (pVal != null) {
                                                    boolean isarray = (vtType & Variant.VT_ARRAY) == Variant.VT_ARRAY;
                                                    try {

                                                        DebugMode.breakIf(isarray);
                                                    } catch (Error e) {
                                                        e.printStackTrace();
                                                        DebugMode.debugger();
                                                        throw e;
                                                    }
                                                    OleAuto.INSTANCE.VariantClear(pVal);
                                                }

                                            }
                                        } catch (final Throwable e) {
                                            for (final Field f : Variant.class.getFields()) {
                                                try {
                                                    if (f.getName().startsWith("VT_") && vtType == f.getInt(null)) {
                                                        LogV3.info("Error Type: " + f.getName());
                                                        break;
                                                    }
                                                } catch (final Exception e1) {
                                                    e1.printStackTrace();
                                                }
                                            }
                                            LogV3.info("Error Prop: " + property);
                                            LogV3.log(e);
                                            // if (manager != null) {
                                            // manager.trackException(e);
                                            // }
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
                    } finally {
                        // Cleanup
                        svc.Release();
                    }
                } finally {
                    // Uninitialize COM components
                    if (comUnInitRequired) {
                        Ole32.INSTANCE.CoUninitialize();
                    }
                }
                return list;
            }
        } catch (final Exception org) {
            if (org instanceof COMException) {
                int code = ((COMException) org).getHresult().intValue();
                if (WMI_ERRORS.containsKey(code)) {
                    // Known error. no reason to try the fallback
                    LogV3.info("Known Error Code: " + code);
                    throw WMIException.wrap(org);
                }
            }
            LogV3.log(org);
            LogV3.info("Try Fallback via PowerShell");
            try {
                ArrayList<Map<String, Object>> ret = wmiQueryViaPowerShell(namespace, query, properties);
                LogV3.info("Try Fallback via PowerShell Result: " + ret.size() + " Entries");
                return ret;
            } catch (WMIException e1) {
                LogV3.log(e1);
            }
            throw WMIException.wrap(org);
        }
    }

    public static ArrayList<Map<String, Object>> wmiQueryViaPowerShell(String namespace, String query, String... properties) throws WMIException, InterruptedException {
        try {
            if (namespace == null) {
                namespace = "ROOT\\CIMV2";
            }
            String q = "Get-WmiObject -Locale " + DEFAULT_LANGUAGE + " -Namespace " + namespace + " -Query \\\"" + query + "\\\"";
            if (properties != null && properties.length > 0) {
                q += " | Select-Object " + new Joiner(", ").join(properties);
            }
            q += " | ConvertTo-Json";
            Command c = new Command("powershell.exe", "-Command", q);
            ProcessOutputHandler poh;
            c.setOutputHandler(poh = new ProcessOutputHandler());
            c.start(true);
            int exitCode = c.waitFor();
            if (exitCode != 0) {
                LogV3.info(poh.getResult().getErrOutString());
                throw new WMIException("PowerShell: " + poh.getResult().getErrOutString());
            }

            String json = poh.getResult().getStdOutString();
            LogV3.info("PowerShell :\r\n" + json);
            if ("".equals(json)) {
                json = "{}";
            }
            Object o = Deser.get().fromString(json, TypeRef.OBJECT);
            if (o instanceof List) {
                return Deser.get().convert(o, new TypeRef<ArrayList<Map<String, Object>>>() {
                });
            } else {
                ArrayList<Map<String, Object>> ret = new ArrayList<Map<String, Object>>();
                Map<String, Object> obj = Deser.get().convert(o, TypeRef.MAP);
                ret.add(obj);
                return ret;
            }
        } catch (IOException e) {
            throw WMIException.wrap(e);
        }
    }

    public static void main(final String[] args) throws WMIException, InterruptedException {
        final SimpleLoggerFactory f = new SimpleLoggerFactory();
        f.setSinkToConsole(new LogToStdOutSink());
        LogV3.setFactory(f);
        final HashSet<String> expected = new HashSet<String>();
        expected.add(";DisplayName: Windows Defender - on,up_to_date,ms;{\"__NAMESPACE\":\"ROOT\\\\SecurityCenter2\",\"pathToSignedReportingExe\":\"%ProgramFiles%\\\\Windows Defender\\\\MsMpeng.exe\",\"displayName\":\"Windows Defender\",\"pathToSignedProductExe\":\"windowsdefender://\",\"productState\":397568,\"__PROPERTY_COUNT\":6,\"__DYNASTY\":\"AntiVirusProduct\",\"__DERIVATION\":[],\"__SERVER\":\"DESKTOP-7KP0VLD\",\"__CLASS\":\"AntiVirusProduct\",\"__RELPATH\":\"AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__PATH\":\"\\\\\\\\DESKTOP-7KP0VLD\\\\ROOT\\\\SecurityCenter2:AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__GENUS\":2,\"instanceGuid\":\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\",\"__SUPERCLASS\":null,\"timestamp\":\"Tue, 30 Jul 2024 15:06:30 GMT\"}");
        expected.add(";DisplayName: Windows Defender - on,up_to_date,ms;{\"__NAMESPACE\":\"ROOT\\\\SecurityCenter2\",\"pathToSignedReportingExe\":\"%ProgramFiles%\\\\Windows Defender\\\\MsMpeng.exe\",\"displayName\":\"Windows Defender\",\"pathToSignedProductExe\":\"windowsdefender://\",\"productState\":397568,\"__PROPERTY_COUNT\":6,\"__DYNASTY\":\"AntiVirusProduct\",\"__SERVER\":\"DESKTOP-7KP0VLD\",\"__CLASS\":\"AntiVirusProduct\",\"__RELPATH\":\"AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__PATH\":\"\\\\\\\\DESKTOP-7KP0VLD\\\\ROOT\\\\SecurityCenter2:AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__GENUS\":2,\"instanceGuid\":\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\",\"__SUPERCLASS\":null,\"timestamp\":\"Tue, 30 Jul 2024 15:06:30 GMT\"}");
        final AtomicLong lastGC = new AtomicLong();
        for (int i = 0; i < 1; i++) {
            new Thread() {
                private int counter = 0;

                @Override
                public void run() {
                    while (true) {
                        // LogV3.info("AntiVirusProduct START");
                        try {
                            this.setName(new Date().toString() + " - " + this.counter);
                            String namespace;
                            switch (CrossSystem.getOS()) {
                            case WINDOWS_XP:
                                namespace = "ROOT\\SecurityCenter";
                                break;
                            default:
                                namespace = "ROOT\\SecurityCenter2";
                                break;
                            }
                            final String query = "SELECT * from AntiVirusProduct";
                            final ArrayList<Map<String, Object>> list = JNAWMIUtils.query(namespace, query);
                            final StringBuilder sb = new StringBuilder();
                            if (list.size() > 0) {
                                for (final Map<String, Object> map : list) {
                                    final String displayName = StringUtils.valueOfOrNull(map.get(DISPLAY_NAME));
                                    final int state = ((Number) map.get(PRODUCT_STATE)).intValue();
                                    final String stateString = decodeProductState(state);
                                    sb.append(";" + "DisplayName: " + displayName + " - " + stateString);
                                    sb.append(";" + Deser.get().toString(map, SC.LOG_SINGLELINE));
                                }
                            }
                            // Thread.sleep(100);
                            synchronized (lastGC) {
                                if (Time.now() - lastGC.get() > 1000) {
                                    lastGC.set(Time.now());
                                    LogV3.info("GC");
                                    System.gc();
                                    Thread.sleep(200);
                                }
                            }
                            this.counter++;
                            // if (!expected.contains(sb.toString())) {
                            // LogV3.info("Unexpected result:\r\n" + sb.toString());
                            // }
                        } catch (final Throwable e) {
                            LogV3.log(e);
                        } finally {
                            // LogV3.info("AntiVirusProduct END");
                        }
                    }
                };
            }.start();
        }
    }

    /**
     * @param path
     * @return
     */
    public static String escape(String input) {
        return input.replace("\\", "\\\\") // Backslash
                .replace("'", "\\'") // Einfaches Anführungszeichen
                .replace("\"", "\\\"") // Doppeltes Anführungszeichen
                .replace("%", "\\%"); // Prozentzeichen (LIKE-Klausel)
    }
}
