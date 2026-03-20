/**
 * Reflection bridge for sun.security.x509 / sun.security.util to support multiple Java versions.
 * Uses reflection for operations that changed or were removed between Java 8 and Java 21+
 * (e.g. X509CertInfo.set, CertificateExtensions.set, ObjectIdentifier constructor).
 * JDK 21 replaced X509CertInfo's generic set(String, Object) with individual setters (setVersion, setSerialNumber, etc.).
 *
 * Requires --add-exports java.base/sun.security.x509=ALL-UNNAMED and
 * --add-exports java.base/sun.security.util=ALL-UNNAMED at runtime.
 */
package org.appwork.utils.net.httpconnection.tests;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 * Bridge for internal JDK certificate APIs that differ across Java versions.
 */
public final class CertificateFactoryBridge {

    private static final String OID_CLASS = "sun.security.util.ObjectIdentifier";
    private static final String X509_CERT_INFO_CLASS = "sun.security.x509.X509CertInfo";
    private static final String CERTIFICATE_EXTENSIONS_CLASS = "sun.security.x509.CertificateExtensions";

    /** Attribute name to setter method name for X509CertInfo (JDK 21+ individual setters). */
    private static final String[][] X509CertInfo_NAME_TO_SETTER = new String[][] {
            { "version", "setVersion" },
            { "serialNumber", "setSerialNumber" },
            { "algorithmID", "setAlgorithmId" },
            { "algorithmId", "setAlgorithmId" },
            { "issuer", "setIssuer" },
            { "subject", "setSubject" },
            { "validity", "setValidity" },
            { "key", "setKey" },
            { "extensions", "setExtensions" },
    };

    /**
     * Creates an ObjectIdentifier compatible with the current JVM.
     * Java 8: constructor ObjectIdentifier(int[]).
     * Java 21+: int[] constructor removed; use String form or ObjectIdentifier.of(String).
     *
     * @param oid OID components (e.g. 1.3.6.1.5.5.7.3.1 -> int[] { 1, 3, 6, 1, 5, 5, 7, 3, 1 })
     * @return ObjectIdentifier instance (as Object to avoid compile-time dependency on sun.*)
     */
    public static Object createObjectIdentifier(final int[] oid) throws Exception {
        if (oid == null || oid.length == 0) {
            throw new IllegalArgumentException("oid must not be null or empty");
        }
        final Class<?> clazz = Class.forName(OID_CLASS);

        // Java 8: new ObjectIdentifier(int[])
        try {
            final Constructor<?> ctor = clazz.getConstructor(int[].class);
            return ctor.newInstance(oid);
        } catch (NoSuchMethodException e) {
            // fall through to String-based creation
        }

        final String oidStr = oidArrayToString(oid);

        // Java 9+: new ObjectIdentifier(String)
        try {
            final Constructor<?> ctor = clazz.getConstructor(String.class);
            return ctor.newInstance(oidStr);
        } catch (NoSuchMethodException e) {
            // fall through
        }

        // Java 21+: ObjectIdentifier.of(String)
        try {
            final Method of = clazz.getMethod("of", String.class);
            return of.invoke(null, oidStr);
        } catch (NoSuchMethodException e) {
            throw new UnsupportedOperationException("ObjectIdentifier creation not supported on this JVM. Tried (int[]), (String), of(String). OID=" + oidStr, e);
        }
    }

    /**
     * Converts OID int[] to dot-separated string (e.g. "1.3.6.1.5.5.7.3.1").
     */
    public static String oidArrayToString(final int[] oid) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < oid.length; i++) {
            if (i > 0) {
                sb.append('.');
            }
            sb.append(oid[i]);
        }
        return sb.toString();
    }

    /**
     * Invokes set(name, value) on the given object (X509CertInfo or CertificateExtensions).
     * Uses reflection so that different method signatures across JDK versions are supported.
     * JDK 21: X509CertInfo no longer has set(String, Object); use individual setters (setVersion, setSerialNumber, etc.).
     *
     * @param target instance of X509CertInfo or CertificateExtensions
     * @param name   attribute name (e.g. from X509CertInfo.VERSION or Extension.NAME)
     * @param value  attribute value
     */
    public static void invokeSet(final Object target, final Object name, final Object value) throws Exception {
        if (target == null) {
            throw new IllegalArgumentException("target must not be null");
        }
        final Class<?> targetClass = target.getClass();
        final String nameStr = name != null ? name.toString() : null;

        // Try set(String, Object) first (Java 8 style)
        try {
            final Method set = targetClass.getMethod("set", String.class, Object.class);
            set.invoke(target, nameStr, value);
            return;
        } catch (NoSuchMethodException e) {
            // try set(Object, Object)
        }

        try {
            final Method set = targetClass.getMethod("set", Object.class, Object.class);
            set.invoke(target, name, value);
            return;
        } catch (NoSuchMethodException e) {
            // JDK 21+: X509CertInfo has individual setters only
            if (X509_CERT_INFO_CLASS.equals(targetClass.getName())) {
                invokeX509CertInfoSetter(target, nameStr, value);
                return;
            }
            // JDK 21+: CertificateExtensions has setExtension(String, Extension) instead of set(String, Object)
            if (CERTIFICATE_EXTENSIONS_CLASS.equals(targetClass.getName())) {
                final Method setExt = targetClass.getMethod("setExtension", String.class, Class.forName("sun.security.x509.Extension"));
                setExt.invoke(target, nameStr, value);
                return;
            }
            throw new UnsupportedOperationException("set(String, Object) or set(Object, Object) not found on " + targetClass.getName(), e);
        }
    }

    /**
     * JDK 21+: call individual setter on X509CertInfo (setVersion, setSerialNumber, setSubject, etc.).
     * Handles compound name "algorithmID.algorithm" by wrapping AlgorithmId in CertificateAlgorithmId.
     */
    private static void invokeX509CertInfoSetter(final Object info, final String nameStr, final Object value) throws Exception {
        if (nameStr == null || nameStr.length() == 0) {
            throw new IllegalArgumentException("attribute name must not be null or empty");
        }
        String simpleName = nameStr;
        Object setValue = value;
        // Compound name e.g. "algorithmID.algorithm" with value AlgorithmId -> wrap in CertificateAlgorithmId and set
        if (nameStr.indexOf('.') >= 0 && nameStr.endsWith(".algorithm") && value != null) {
            setValue = wrapAlgorithmIdInCertificateAlgorithmId(value);
            simpleName = "algorithmID";
        }
        final String setterName = findSetterNameForAttribute(simpleName);
        if (setterName == null) {
            throw new UnsupportedOperationException("No setter mapping for X509CertInfo attribute: " + nameStr);
        }
        final Method setter = findSetterMethod(info.getClass(), setterName, setValue);
        if (setter == null) {
            throw new UnsupportedOperationException("Setter " + setterName + " not found on " + info.getClass().getName() + " for value type " + (setValue != null ? setValue.getClass().getName() : "null"));
        }
        setter.invoke(info, setValue);
    }

    private static String findSetterNameForAttribute(final String simpleName) {
        for (int i = 0; i < X509CertInfo_NAME_TO_SETTER.length; i++) {
            if (simpleName.equals(X509CertInfo_NAME_TO_SETTER[i][0])) {
                return X509CertInfo_NAME_TO_SETTER[i][1];
            }
        }
        return null;
    }

    private static Method findSetterMethod(final Class<?> clazz, final String setterName, final Object value) {
        if (value == null) {
            return null;
        }
        final Class<?> valueClass = value.getClass();
        try {
            return clazz.getMethod(setterName, valueClass);
        } catch (NoSuchMethodException e) {
            // try with supertypes so we accept e.g. CertificateVersion subclass
            for (Class<?> c = valueClass; c != null; c = c.getSuperclass()) {
                try {
                    return clazz.getMethod(setterName, c);
                } catch (NoSuchMethodException e2) {
                    // continue
                }
            }
        }
        return null;
    }

    /** Wraps an AlgorithmId in a CertificateAlgorithmId for setAlgorithmId (JDK 21). */
    private static Object wrapAlgorithmIdInCertificateAlgorithmId(final Object algorithmId) throws Exception {
        final Class<?> certAlgIdClass = Class.forName("sun.security.x509.CertificateAlgorithmId");
        final Constructor<?> ctor = certAlgIdClass.getConstructor(algorithmId.getClass());
        return ctor.newInstance(algorithmId);
    }

    /**
     * Invokes get(name) on the given object (e.g. X509CertImpl).
     *
     * @param target instance (e.g. X509CertImpl)
     * @param name   attribute name
     * @return attribute value
     */
    public static Object invokeGet(final Object target, final Object name) throws Exception {
        if (target == null) {
            throw new IllegalArgumentException("target must not be null");
        }
        final Class<?> targetClass = target.getClass();
        final String nameStr = name != null ? name.toString() : null;

        try {
            final Method get = targetClass.getMethod("get", String.class);
            return get.invoke(target, nameStr);
        } catch (NoSuchMethodException e) {
            final Method get = targetClass.getMethod("get", Object.class);
            return get.invoke(target, name);
        }
    }

    private CertificateFactoryBridge() {
    }
}
