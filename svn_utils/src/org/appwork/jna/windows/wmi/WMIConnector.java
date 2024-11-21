package org.appwork.jna.windows.wmi;

import java.io.Closeable;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.LogToStdOutSink;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.utils.DebugMode;
import org.appwork.utils.NameInterface;
import org.appwork.utils.NonInterruptibleThread;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Pointer;
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

@Deprecated
/**
 * @deprecated maybe unstable
 * @author thomas
 * @date 20.11.2024
 *
 */
public class WMIConnector implements Closeable {
    /**
     *
     */
    public static final String LNG_EN           = "MS_409";
    private static String      OS_LANGUAGE_CODE = null;

    public static void main(final String[] args) {
        final SimpleLoggerFactory f = new SimpleLoggerFactory();
        f.setSinkToConsole(new LogToStdOutSink());
        LogV3.setFactory(f);
        final HashSet<String> expected = new HashSet<String>();
        expected.add("[{\"__CLASS\":\"AntiVirusProduct\",\"__DERIVATION\":[],\"__DYNASTY\":\"AntiVirusProduct\",\"__GENUS\":2,\"__NAMESPACE\":\"ROOT\\\\SecurityCenter2\",\"__PATH\":\"\\\\\\\\DESKTOP-7KP0VLD\\\\ROOT\\\\SecurityCenter2:AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__PROPERTY_COUNT\":6,\"__RELPATH\":\"AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__SERVER\":\"DESKTOP-7KP0VLD\",\"__SUPERCLASS\":null,\"displayName\":\"Windows Defender\",\"instanceGuid\":\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\",\"pathToSignedProductExe\":\"windowsdefender://\",\"pathToSignedReportingExe\":\"%ProgramFiles%\\\\Windows Defender\\\\MsMpeng.exe\",\"productState\":397568,\"timestamp\":\"Thu, 14 Nov 2024 07:28:55 GMT\"}]");
        final AtomicLong lastGC = new AtomicLong();
        final String namespace;
        switch (CrossSystem.getOS()) {
        case WINDOWS_XP:
            namespace = "ROOT\\SecurityCenter";
            break;
        default:
            namespace = "ROOT\\SecurityCenter2";
            break;
        }
        for (int i = 0; i < 10; i++) {
            new Thread() {
                private int counter = 0;

                @Override
                public void run() {
                    while (true) {
                        // LogV3.info("AntiVirusProduct START");
                        final WMIConnector con = new WMIConnector(namespace);
                        try {
                            this.setName(new Date().toString() + " - " + this.counter);
                            final String query = "SELECT * from AntiVirusProduct";
                            try {
                                final ArrayList<HashMap<String, Object>> list = con.executeQuery(null, query);
                                // Thread.sleep(100);
                                synchronized (lastGC) {
                                    if (Time.now() - lastGC.get() > 1000) {
                                        lastGC.set(Time.now());
                                        LogV3.info("GC " + counter);
                                        System.gc();
                                        Thread.sleep(200);
                                    }
                                }
                                boolean ok = true;
                                String co = Deser.get().toString(list, SC.HASH_CONTENT);
                                if (!expected.contains(co)) {
                                    System.out.println("UNEXPECTED:");
                                    System.out.println(Deser.get().toString(co));
                                }
                                this.counter++;
                            } finally {
                            }
                        } catch (final Throwable e) {
                            LogV3.log(e);
                        } finally {
                            // LogV3.info("AntiVirusProduct END");
                            try {
                                con.close();
                            } catch (IOException e) {
                                LogV3.log(e);
                            }
                        }
                    }
                };
            }.start();
        }
    }

    private IWbemServices svc;

    public WMIConnector() {
        this(null, LNG_EN);
    }

    public WMIConnector(String namespace) {
        this(namespace, LNG_EN);
    }

    public WMIConnector(String namespace, String lng) {
        if (namespace == null) {
            namespace = "ROOT\\CIMV2";
        }
        this.initCOM();
        if (lng == null) {
            if (OS_LANGUAGE_CODE == null) {
                NonInterruptibleThread.execute(new Runnable() {
                    @Override
                    public void run() {
                        int osCode;
                        try {
                            osCode = ((Number) JNAWMIUtils.query(null, "SELECT OSLanguage from Win32_OperatingSystem", "OSLanguage").get(0).get("OSLanguage")).intValue();
                            OS_LANGUAGE_CODE = "ms_" + StringUtils.fillPre(Integer.toHexString(osCode), "0", 3);
                        } catch (WMIException e) {
                            LogV3.log(e);
                        } catch (InterruptedException e) {
                            LogV3.log(e);
                        }
                    }
                });
            }
            lng = OS_LANGUAGE_CODE;
        }
        this.svc = connectServer(namespace, lng);
    }

    public static IWbemServices connectServer(String namespace, String lng) {
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
        final IWbemServices services = loc.ConnectServer(namespace, null, null, lng, 0, null, null);
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
                int vtType = -1;
                for (String property : clsObj.GetNames(null, 0, null)) {
                    boolean clear = true;
                    final VARIANT.ByReference pVal = new VARIANT.ByReference();
                    vtType = -1;
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
                        final HRESULT result = clsObj.Get(property, 0, pVal, pType, null);
                        if (!W32Errors.SUCCEEDED(result.intValue())) {
                            throw new Win32Exception(result);
                        }
                        // System.out.println("Read 3");
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
                        case Variant.VT_INT:
                        case Variant.VT_UINT:
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
                            final Object value = pVal.getValue();
                            if (value instanceof SAFEARRAY) {
                                final Object array = OaIdlUtil.toPrimitiveArray((SAFEARRAY) value, false);
                                ((SAFEARRAY) value).destroy();
                                clear = false;
                                map.put(property, array);
                            } else {
                                map.put(property, null);
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
                    } finally {
                        if (clear) {
                            if (pVal.getValue() instanceof SAFEARRAY) {
                                DebugMode.debugger();
                            }
                            OleAuto.INSTANCE.VariantClear(pVal);
                        }
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
