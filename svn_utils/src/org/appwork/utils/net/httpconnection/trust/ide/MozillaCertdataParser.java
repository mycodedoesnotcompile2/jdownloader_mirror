/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         ====================================================================================================================================================
 *         ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust.ide;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.appwork.utils.Hash;

/**
 * Parser for Mozilla NSS certdata.txt. Extracts root certificates and their NSS trust bits (CKA_TRUST_SERVER_AUTH,
 * CKA_TRUST_EMAIL_PROTECTION, CKA_TRUST_CODE_SIGNING, etc.). Only certs with CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR are
 * returned; roots with only CKA_TRUST_EMAIL_PROTECTION or CKA_TRUST_CODE_SIGNING (CKT_NSS_MUST_VERIFY_TRUST for server auth) are excluded,
 * so the result set is exactly the roots intended for <b>website / TLS server authentication</b>, consistent with CCADB
 * "TrustBitsInclude=Websites".
 * <p>
 * See https://firefox-source-docs.mozilla.org/security/nss/runbooks/rootstore.html and
 * https://hg.mozilla.org/projects/nss/file/tip/lib/ckfw/builtins/certdata.txt
 */
public final class MozillaCertdataParser {
    private static final String CKO_CERTIFICATE           = "CKO_CERTIFICATE";
    private static final String CKO_NSS_TRUST             = "CKO_NSS_TRUST";
    private static final String CKA_CLASS                 = "CKA_CLASS";
    private static final String CKA_VALUE                 = "CKA_VALUE";
    private static final String CKA_CERT_SHA1_HASH        = "CKA_CERT_SHA1_HASH";
    private static final String CKA_TRUST_SERVER_AUTH     = "CKA_TRUST_SERVER_AUTH";
    private static final String MULTILINE_OCTAL           = "MULTILINE_OCTAL";
    private static final String CKT_NSS_TRUSTED_DELEGATOR = "CKT_NSS_TRUSTED_DELEGATOR";
    private static final String CKT_NSS_NOT_TRUSTED       = "CKT_NSS_NOT_TRUSTED";

    /**
     * Result: SHA-256 fingerprints of roots that have CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR.
     *
     * @param input
     *            certdata.txt stream (UTF-8)
     * @return set of lowercase hex SHA-256 fingerprints (no colons)
     * @throws IOException
     *             on read error
     */
    public static Set<String> getTrustedForServerAuthFingerprints(final InputStream input) throws IOException {
        final X509Certificate[] certs = getTrustedForServerAuthCertificates(input);
        final Set<String> out = new TreeSet<String>();
        for (int i = 0; i < certs.length; i++) {
            try {
                final String fp = Hash.getSHA256(certs[i].getEncoded());
                if (fp != null) {
                    out.add(fp.toLowerCase(Locale.ROOT));
                }
            } catch (final Exception ignored) {
            }
        }
        return out;
    }

    /**
     * Result: Root certificates that have CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR.
     *
     * @param input
     *            certdata.txt stream (UTF-8)
     * @return array of X509Certificate (may be empty)
     * @throws IOException
     *             on read error
     */
    public static X509Certificate[] getTrustedForServerAuthCertificates(final InputStream input) throws IOException {
        final List<Object> objects = parseObjects(input);
        final List<Map<String, Object>> certObjs = filterByClass(objects, CKO_CERTIFICATE);
        final List<Map<String, Object>> trustObjs = filterByClass(objects, CKO_NSS_TRUST);
        final List<X509Certificate> trusted = new ArrayList<X509Certificate>();
        final CertificateFactory cf;
        try {
            cf = CertificateFactory.getInstance("X.509");
        } catch (final CertificateException e) {
            return new X509Certificate[0];
        }
        for (int i = 0; i < certObjs.size(); i++) {
            final Map<String, Object> certObj = certObjs.get(i);
            final byte[] der = (byte[]) certObj.get(CKA_VALUE);
            if (der == null || der.length == 0) {
                continue;
            }
            final byte[] certSha1 = sha1(der);
            Map<String, Object> trustObj = null;
            for (int t = 0; t < trustObjs.size(); t++) {
                final byte[] trustSha1 = (byte[]) trustObjs.get(t).get(CKA_CERT_SHA1_HASH);
                if (trustSha1 != null && trustSha1.length == certSha1.length && java.util.Arrays.equals(trustSha1, certSha1)) {
                    trustObj = trustObjs.get(t);
                    break;
                }
            }
            if (trustObj == null) {
                continue;
            }
            final String serverAuth = (String) trustObj.get(CKA_TRUST_SERVER_AUTH);
            if (!CKT_NSS_TRUSTED_DELEGATOR.equals(serverAuth)) {
                continue;
            }
            try {
                final X509Certificate cert = (X509Certificate) cf.generateCertificate(new java.io.ByteArrayInputStream(der));
                if (cert != null) {
                    trusted.add(cert);
                }
            } catch (final CertificateException ignored) {
            }
        }
        return trusted.toArray(new X509Certificate[trusted.size()]);
    }

    /**
     * Result: SHA-256 fingerprints of roots that have CKA_TRUST_SERVER_AUTH = CKT_NSS_NOT_TRUSTED (explicitly distrusted for server auth).
     */
    public static Set<String> getExplicitlyDistrustedForServerAuthFingerprints(final InputStream input) throws IOException {
        final X509Certificate[] certs = getExplicitlyDistrustedForServerAuthCertificates(input);
        final Set<String> out = new TreeSet<String>();
        for (int i = 0; i < certs.length; i++) {
            try {
                final String fp = Hash.getSHA256(certs[i].getEncoded());
                if (fp != null) {
                    out.add(fp.toLowerCase(Locale.ROOT));
                }
            } catch (final Exception ignored) {
            }
        }
        return out;
    }

    /**
     * Result: Root certificates that have CKA_TRUST_SERVER_AUTH = CKT_NSS_NOT_TRUSTED (explicitly distrusted for TLS/websites).
     */
    public static X509Certificate[] getExplicitlyDistrustedForServerAuthCertificates(final InputStream input) throws IOException {
        final List<Object> objects = parseObjects(input);
        final List<Map<String, Object>> certObjs = filterByClass(objects, CKO_CERTIFICATE);
        final List<Map<String, Object>> trustObjs = filterByClass(objects, CKO_NSS_TRUST);
        final List<X509Certificate> distrusted = new ArrayList<X509Certificate>();
        final CertificateFactory cf;
        try {
            cf = CertificateFactory.getInstance("X.509");
        } catch (final CertificateException e) {
            return new X509Certificate[0];
        }
        for (int i = 0; i < certObjs.size(); i++) {
            final Map<String, Object> certObj = certObjs.get(i);
            final byte[] der = (byte[]) certObj.get(CKA_VALUE);
            if (der == null || der.length == 0) {
                continue;
            }
            final byte[] certSha1 = sha1(der);
            Map<String, Object> trustObj = null;
            for (int t = 0; t < trustObjs.size(); t++) {
                final byte[] trustSha1 = (byte[]) trustObjs.get(t).get(CKA_CERT_SHA1_HASH);
                if (trustSha1 != null && trustSha1.length == certSha1.length && java.util.Arrays.equals(trustSha1, certSha1)) {
                    trustObj = trustObjs.get(t);
                    break;
                }
            }
            if (trustObj == null) {
                continue;
            }
            final String serverAuth = (String) trustObj.get(CKA_TRUST_SERVER_AUTH);
            if (!CKT_NSS_NOT_TRUSTED.equals(serverAuth)) {
                continue;
            }
            try {
                final X509Certificate cert = (X509Certificate) cf.generateCertificate(new java.io.ByteArrayInputStream(der));
                if (cert != null) {
                    distrusted.add(cert);
                }
            } catch (final CertificateException ignored) {
            }
        }
        return distrusted.toArray(new X509Certificate[distrusted.size()]);
    }

    private static byte[] sha1(final byte[] data) {
        try {
            final MessageDigest md = MessageDigest.getInstance("SHA-1");
            md.update(data);
            return md.digest();
        } catch (final Exception e) {
            return new byte[0];
        }
    }

    private static List<Map<String, Object>> filterByClass(final List<Object> objects, final String className) {
        final List<Map<String, Object>> out = new ArrayList<Map<String, Object>>();
        for (int i = 0; i < objects.size(); i++) {
            @SuppressWarnings("unchecked")
            final Map<String, Object> obj = (Map<String, Object>) objects.get(i);
            if (className.equals(obj.get(CKA_CLASS))) {
                out.add(obj);
            }
        }
        return out;
    }

    /**
     * Parses certdata.txt into a list of attribute maps. Each object is a Map from attribute name (e.g. CKA_VALUE, CKA_CLASS) to value
     * (byte[] for MULTILINE_OCTAL, String for simple values).
     */
    private static List<Object> parseObjects(final InputStream input) throws IOException {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(input, "UTF-8"));
        boolean begun = false;
        final List<Object> objects = new ArrayList<Object>();
        Map<String, Object> current = null;
        String line;
        while ((line = reader.readLine()) != null) {
            if (line.trim().length() == 0 || line.trim().startsWith("#")) {
                continue;
            }
            if (line.trim().equals("BEGINDATA")) {
                begun = true;
                continue;
            }
            if (!begun) {
                continue;
            }
            final String[] parts = line.split("\\s+", 3);
            if (parts.length < 2) {
                continue;
            }
            final String attrName = parts[0];
            final String attrType = parts[1];
            Object value;
            if (MULTILINE_OCTAL.equals(attrType)) {
                value = readMultilineOctal(reader);
                if (value == null) {
                    continue;
                }
            } else {
                value = parts.length >= 3 ? parts[2].trim() : "";
            }
            if (CKA_CLASS.equals(attrName)) {
                if (current != null) {
                    objects.add(current);
                }
                current = new LinkedHashMap<String, Object>();
            }
            if (current == null) {
                continue;
            }
            current.put(attrName, value);
        }
        if (current != null) {
            objects.add(current);
        }
        return objects;
    }

    private static byte[] readMultilineOctal(final BufferedReader reader) throws IOException {
        final List<Byte> list = new ArrayList<Byte>();
        String line;
        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if ("END".equals(line)) {
                break;
            }
            int i = 0;
            while (i < line.length()) {
                if (line.charAt(i) == '\\' && i + 4 <= line.length()) {
                    final String oct = line.substring(i + 1, i + 4);
                    try {
                        list.add(Byte.valueOf((byte) Integer.parseInt(oct, 8)));
                    } catch (final NumberFormatException e) {
                        // skip invalid
                    }
                    i += 4;
                } else {
                    i++;
                }
            }
        }
        final byte[] out = new byte[list.size()];
        for (int i = 0; i < list.size(); i++) {
            out[i] = list.get(i).byteValue();
        }
        return out;
    }

    private MozillaCertdataParser() {
    }
}
