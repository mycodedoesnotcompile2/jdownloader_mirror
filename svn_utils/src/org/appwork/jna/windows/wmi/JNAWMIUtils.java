/**
 *
 */
package org.appwork.jna.windows.wmi;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.loggingv3.LogV3;
import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Joiner;
import org.appwork.utils.StringUtils;
import org.appwork.utils.processes.command.Command;
import org.appwork.utils.processes.command.ProcessOutputHandler;

import com.sun.jna.platform.win32.COM.COMException;

/**
 *
 */
public class JNAWMIUtils {
    /**
     *
     */
    private static final String DEFAULT_LANGUAGE         = "MS_409";
    /**
     *
     */
    static Map<Integer, String> WMI_ERRORS               = new HashMap<Integer, String>();
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
    public static final String  PRODUCT_STATE            = "productState";
    /**
     *
     */
    public static final String  DISPLAY_NAME             = "displayName";
    private static final int    TIMEOUT                  = 60000;
    static final int            WBEM_FLAG_NONSYSTEM_ONLY = 0x40;
    private static boolean      SECURITY_INITIALIZED;

    public static String decodeProductState(final int productState) {
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

    public static List<Map<String, Object>> query(String namespace, final String query, String... properties) throws WMIException, InterruptedException {
        if (namespace == null) {
            namespace = "ROOT\\CIMV2";
        }
        try {
            final WMIConnector con = new WMIConnector(namespace);
            con.open();
            try {
                return con.query(query, properties);
            } finally {
                con.close();
            }
        } catch (final Exception org) {
            final COMException com = Exceptions.getInstanceof(org, COMException.class);
            if (com != null) {
                final int code = com.getHresult().intValue();
                if (WMI_ERRORS.containsKey(code)) {
                    // Known error. no reason to try the fallback
                    LogV3.info("Known Error Code: " + code);
                    throw WMIException.wrap(org, "query:" + query + ";namespace:" + namespace);
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
            throw WMIException.wrap(org, "query:" + query + ";namespace:" + namespace);
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
            throw WMIException.wrap(e, "query:" + query + ";namespace:" + namespace);
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
