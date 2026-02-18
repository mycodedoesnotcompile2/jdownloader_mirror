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
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import org.appwork.utils.Hash;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.formatter.HexFormatter;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinCrypt;
import com.sun.jna.platform.win32.WinCrypt.CERT_CONTEXT;
import com.sun.jna.platform.win32.WinCrypt.DATA_BLOB;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;

/**
 * @author thomas
 * @date 29.01.2026
 *
 */
public class WindowsCertUtils {
    public enum TargetKeyStore {
        CURRENT_USER,
        /* requires admin */
        LOCAL_MACHINE
    }

    static byte[] getCertSha1Thumbprint(final Pointer certContext) {
        final IntByReference size = new IntByReference(0);
        // First call to get required size
        final boolean okSize = WindowsCertUtilsCrypt.INSTANCE.CertGetCertificateContextProperty(certContext, WindowsCertUtilsCrypt.CERT_SHA1_HASH_PROP_ID, null, size);
        if (!okSize || size.getValue() <= 0) {
            return null;
        }
        final byte[] buf = new byte[size.getValue()];
        final boolean okData = WindowsCertUtilsCrypt.INSTANCE.CertGetCertificateContextProperty(certContext, WindowsCertUtilsCrypt.CERT_SHA1_HASH_PROP_ID, buf, size);
        if (!okData) {
            return null;
        }
        // SHA-1 should be 20 bytes; if larger, trim to reported size
        if (size.getValue() == buf.length) {
            return buf;
        }
        final byte[] out = new byte[size.getValue()];
        System.arraycopy(buf, 0, out, 0, out.length);
        return out;
    }

    static byte[] parseThumbprintHex(final String s) {
        if (s == null) {
            return null;
        }
        final String hex = s.replaceAll("[^0-9A-Fa-f]", "").toUpperCase();
        if (hex.length() == 0 || (hex.length() % 2) != 0) {
            throw new IllegalArgumentException("Invalid thumbprint hex: " + s);
        }
        final byte[] out = new byte[hex.length() / 2];
        for (int i = 0; i < out.length; i++) {
            final int hi = Character.digit(hex.charAt(i * 2), 16);
            final int lo = Character.digit(hex.charAt(i * 2 + 1), 16);
            if (hi < 0 || lo < 0) {
                throw new IllegalArgumentException("Invalid thumbprint hex: " + s);
            }
            out[i] = (byte) ((hi << 4) | lo);
        }
        return out;
    }

    /**
     * Opens a Windows certificate store as a PHYSICAL registry-backed store (no merged / inherited certificates).
     *
     * IMPORTANT: - Uses CERT_STORE_PROV_SYSTEM_REGISTRY (not CERT_STORE_PROV_SYSTEM) - This guarantees that only certificates physically
     * stored in the specified location are visible.
     *
     * Store mapping: - CURRENT_USER -> HKCU\Software\Microsoft\SystemCertificates\<Store> - LOCAL_MACHINE ->
     * HKLM\Software\Microsoft\SystemCertificates\<Store>
     *
     * Why this matters: Using CERT_STORE_PROV_SYSTEM would open a logical/collection store and include inherited certificates (e.g.
     * LocalMachine Root appearing in CurrentUser Root).
     *
     * @param target
     *            Certificate store location (CURRENT_USER or LOCAL_MACHINE)
     * @param readOnly
     *            If true, the store is opened read-only. If false, the store is opened read-write (required for delete/install).
     *
     * @return HANDLE to the opened certificate store
     * @throws Win32Exception
     *             if the store cannot be opened
     */
    static HANDLE openStore(final WindowsCertUtils.TargetKeyStore target, final boolean readOnly) {
        // Select physical registry location (HKCU or HKLM)
        final int locationFlag = (target == WindowsCertUtils.TargetKeyStore.LOCAL_MACHINE) ? WindowsCertUtilsCrypt.CERT_SYSTEM_STORE_LOCAL_MACHINE : WindowsCertUtilsCrypt.CERT_SYSTEM_STORE_CURRENT_USER;
        int flags = locationFlag | WindowsCertUtilsCrypt.CERT_STORE_OPEN_EXISTING_FLAG;
        if (readOnly) {
            flags |= WindowsCertUtilsCrypt.CERT_STORE_READONLY_FLAG;
        }
        // CERT_STORE_PROV_SYSTEM_REGISTRY opens a physical store only
        final HANDLE store = WindowsCertUtilsCrypt.INSTANCE.CertOpenStore(WindowsCertUtilsCrypt.CERT_STORE_PROV_SYSTEM_REGISTRY, 0, null, flags, new WString("Root"));
        if (store == null || Pointer.nativeValue(store.getPointer()) == 0) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        return store;
    }

    public static boolean isCertificateInstalled(final String thumbprintHex, final WindowsCertUtils.TargetKeyStore target) {
        final byte[] wanted = parseThumbprintHex(thumbprintHex);
        if (wanted == null || wanted.length == 0) {
            return false;
        }
        final HANDLE store = WindowsCertUtils.openStore(target, true);
        Pointer ctx = null;
        try {
            while (true) {
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                final byte[] actual = getCertSha1Thumbprint(ctx);
                if (actual != null && Arrays.equals(wanted, actual)) {
                    return true;
                }
            }
            return false;
        } finally {
            if (ctx != null && Pointer.nativeValue(ctx) != 0) {
                WindowsCertUtilsCrypt.INSTANCE.CertFreeCertificateContext(ctx);
            }
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
    }

    /**
     * Installs the given X509 certificate into the Windows "ROOT" store for the given target. Silent (no prompts).
     *
     * @throws Exception
     *             if Windows store open/add fails.
     */
    public static void installCertificate(final X509Certificate certificate, final WindowsCertUtils.TargetKeyStore target, final String friendlyName) throws Exception {
        if (certificate == null) {
            return;
        }
        final byte[] der = certificate.getEncoded();
        final HANDLE store = WindowsCertUtils.openStore(target, false);
        final com.sun.jna.ptr.PointerByReference pAdded = new com.sun.jna.ptr.PointerByReference();
        Pointer addedCtx = null;
        try {
            final boolean ok = WindowsCertUtilsCrypt.INSTANCE.CertAddEncodedCertificateToStore(store, WindowsCertUtilsCrypt.X509_ASN_ENCODING | WindowsCertUtilsCrypt.PKCS_7_ASN_ENCODING, der, der.length, WindowsCertUtilsCrypt.CERT_STORE_ADD_ALWAYS, pAdded);
            if (!ok) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            addedCtx = pAdded.getValue();
            if (addedCtx == null || Pointer.nativeValue(addedCtx) == 0) {
                throw new IllegalStateException("Null PCCERT_CONTEXT from CertAddEncodedCertificateToStore");
            }
            if (friendlyName != null && !friendlyName.isEmpty()) {
                try {
                    // CERT_FRIENDLY_NAME_PROP_ID expects pvData to point to a CRYPT_DATA_BLOB (cbData + pbData), not raw string
                    final byte[] utf16le = (friendlyName + "\0").getBytes(StandardCharsets.UTF_16LE);
                    final DATA_BLOB blob = new WinCrypt.DATA_BLOB(utf16le);
                    blob.write();
                    final boolean okProp = WindowsCertUtilsCrypt.INSTANCE.CertSetCertificateContextProperty(addedCtx, WindowsCertUtilsCrypt.CERT_FRIENDLY_NAME_PROP_ID, 0, blob.getPointer());
                    if (!okProp) {
                        LogV3.warning("Failed to set friendly name: " + Kernel32.INSTANCE.GetLastError());
                    }
                } catch (Throwable e) {
                    LogV3.log(e);
                }
            }
        } finally {
            // CertAdd... returned a context we must free
            if (addedCtx != null && Pointer.nativeValue(addedCtx) != 0) {
                WindowsCertUtilsCrypt.INSTANCE.CertFreeCertificateContext(addedCtx);
            }
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
    }

    static String getName(final Pointer certContext, final boolean issuer) {
        final int flags = issuer ? WindowsCertUtilsCrypt.CERT_NAME_ISSUER_FLAG : 0;
        final int type = WindowsCertUtilsCrypt.CERT_NAME_SIMPLE_DISPLAY_TYPE; // "CN ..." style
        final int len = WindowsCertUtilsCrypt.INSTANCE.CertGetNameStringW(certContext, type, flags, null, null, 0);
        if (len <= 1) {
            return "";
        }
        final char[] buf = new char[len];
        WindowsCertUtilsCrypt.INSTANCE.CertGetNameStringW(certContext, type, flags, null, buf, buf.length);
        final String s = new String(buf);
        final int nul = s.indexOf('\0');
        return (nul >= 0) ? s.substring(0, nul) : s;
    }

    public static int removeCertificatesByIssuedToOrBy(final TargetKeyStore target, final String issuedToContains, final String issuedByContains) {
        final HANDLE store = openStore(target, false);
        Pointer ctx = null;
        int removed = 0;
        try {
            while (true) {
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                final Pointer dup = WindowsCertUtilsCrypt.INSTANCE.CertDuplicateCertificateContext(ctx);
                try {
                    final String subject = getName(dup, false); // issued to
                    final String issuer = getName(dup, true); // issued by
                    final boolean matchSubject = issuedToContains != null && !issuedToContains.isEmpty() && subject.toLowerCase().contains(issuedToContains.toLowerCase());
                    final boolean matchIssuer = issuedByContains != null && !issuedByContains.isEmpty() && issuer.toLowerCase().contains(issuedByContains.toLowerCase());
                    if (matchSubject || matchIssuer) {
                        // NOTE: CertDeleteCertificateFromStore frees the context on success.
                        final boolean ok = WindowsCertUtilsCrypt.INSTANCE.CertDeleteCertificateFromStore(ctx);
                        ctx = null; // prevent use-after-free
                        if (!ok) {
                            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                        }
                        removed++;
                    }
                } finally {
                    if (dup != null && Pointer.nativeValue(dup) != 0) {
                        WindowsCertUtilsCrypt.INSTANCE.CertFreeCertificateContext(dup);
                    }
                }
            }
        } finally {
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
        return removed;
    }

    // Reads FriendlyName (CERT_FRIENDLY_NAME_PROP_ID) from a cert context. Returns "" if not set.
    static String getFriendlyName(final Pointer certContext) {
        final IntByReference cb = new IntByReference(0);
        // Query size (includes terminating NUL, UTF-16LE)
        final boolean okSize = WindowsCertUtilsCrypt.INSTANCE.CertGetCertificateContextProperty(certContext, WindowsCertUtilsCrypt.CERT_FRIENDLY_NAME_PROP_ID, null, cb);
        if (!okSize || cb.getValue() <= 2) {
            return "";
        }
        final byte[] buf = new byte[cb.getValue()];
        final boolean okData = WindowsCertUtilsCrypt.INSTANCE.CertGetCertificateContextProperty(certContext, WindowsCertUtilsCrypt.CERT_FRIENDLY_NAME_PROP_ID, buf, cb);
        if (!okData) {
            return "";
        }
        try {
            // Decode UTF-16LE, trim at first NUL
            final String s = new String(buf, 0, cb.getValue(), "UTF-16LE");
            final int nul = s.indexOf('\0');
            return (nul >= 0) ? s.substring(0, nul) : s;
        } catch (final Exception e) {
            return "";
        }
    }

    // Case-insensitive "contains" helper; empty needle => false.
    static boolean containsIgnoreCase(final String haystack, final String needle) {
        if (needle == null || needle.isEmpty()) {
            return false;
        }
        if (haystack == null || haystack.isEmpty()) {
            return false;
        }
        return haystack.toLowerCase().contains(needle.toLowerCase());
    }

    // Returns true if ANY provided filter matches. If no filters provided => false (safe default).
    static boolean matchesAny(final String subject, final String issuer, final String friendly, final String issuedToContains, final String issuedByContains, final String friendlyNameContains) {
        final boolean hasAnyFilter = (issuedToContains != null && !issuedToContains.isEmpty()) || (issuedByContains != null && !issuedByContains.isEmpty()) || (friendlyNameContains != null && !friendlyNameContains.isEmpty());
        if (!hasAnyFilter) {
            return false;
        }
        return containsIgnoreCase(subject, issuedToContains) || containsIgnoreCase(issuer, issuedByContains) || containsIgnoreCase(friendly, friendlyNameContains);
    }

    public static class CertListEntry {
        public final String subject;      // Issued to
        public final String issuer;       // Issued by
        public final String friendlyName; // Windows "Friendly Name"
        public final String thumbprint;   // SHA-1 hex

        public CertListEntry(final String subject, final String issuer, final String friendlyName, final String thumbprint) {
            this.subject = subject;
            this.issuer = issuer;
            this.friendlyName = friendlyName;
            this.thumbprint = thumbprint;
        }
    }

    // LIST: returns matching certificates (safe: returns empty if no filters provided)
    public static List<CertListEntry> listCertificates(final TargetKeyStore target, final String issuedToContains, final String issuedByContains, final String friendlyNameContains) {
        final List<CertListEntry> ret = new ArrayList<CertListEntry>();
        final HANDLE store = openStore(target, true);
        Pointer ctx = null;
        try {
            while (true) {
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                final Pointer dup = WindowsCertUtilsCrypt.INSTANCE.CertDuplicateCertificateContext(ctx);
                try {
                    final String subject = getName(dup, false);
                    final String issuer = getName(dup, true);
                    final String friendly = getFriendlyName(dup);
                    if (matchesAny(subject, issuer, friendly, issuedToContains, issuedByContains, friendlyNameContains)) {
                        final byte[] sha1 = getCertSha1Thumbprint(dup);
                        final String sha1Hex = (sha1 != null) ? HexFormatter.byteArrayToHex(sha1) : null;
                        ret.add(new CertListEntry(subject, issuer, friendly, sha1Hex));
                    }
                } finally {
                    if (dup != null && Pointer.nativeValue(dup) != 0) {
                        WindowsCertUtilsCrypt.INSTANCE.CertFreeCertificateContext(dup);
                    }
                }
            }
        } finally {
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
        return ret;
    }

    // REMOVE: deletes matching certificates; returns count removed (safe: removes 0 if no filters provided)
    public static int removeCertificates(final TargetKeyStore target, final String issuedToContains, final String issuedByContains, final String friendlyNameContains) {
        final HANDLE store = openStore(target, false);
        Pointer ctx = null;
        int removed = 0;
        try {
            while (true) {
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                // Duplicate to read properties safely before deletion
                final Pointer dup = WindowsCertUtilsCrypt.INSTANCE.CertDuplicateCertificateContext(ctx);
                boolean doDelete = false;
                try {
                    final String subject = getName(dup, false);
                    final String issuer = getName(dup, true);
                    final String friendly = getFriendlyName(dup);
                    doDelete = matchesAny(subject, issuer, friendly, issuedToContains, issuedByContains, friendlyNameContains);
                    if (doDelete) {
                        LogV3.info("Deleting cert: subject=" + subject + " | issuer=" + issuer + " | friendly=" + friendly);
                    }
                } finally {
                    if (dup != null && Pointer.nativeValue(dup) != 0) {
                        WindowsCertUtilsCrypt.INSTANCE.CertFreeCertificateContext(dup);
                    }
                }
                if (doDelete) {
                    // CertDeleteCertificateFromStore frees ctx on success
                    final boolean ok = WindowsCertUtilsCrypt.INSTANCE.CertDeleteCertificateFromStore(ctx);
                    ctx = null; // prevent use-after-free
                    if (!ok) {
                        throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                    }
                    removed++;
                }
            }
        } finally {
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
        return removed;
    }

    static String getCertDisplayName(final Pointer certContext) {
        // First call: get required length (incl. null-terminator)
        int len = WindowsCertUtilsCrypt.INSTANCE.CertGetNameStringW(certContext, WindowsCertUtilsCrypt.CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, null, null, 0);
        if (len <= 1) {
            return "<unknown>";
        }
        char[] buf = new char[len];
        WindowsCertUtilsCrypt.INSTANCE.CertGetNameStringW(certContext, WindowsCertUtilsCrypt.CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, null, buf, buf.length);
        String s = new String(buf);
        int nul = s.indexOf('\0');
        return (nul >= 0) ? s.substring(0, nul) : s;
    }

    /**
     * Removes a certificate from the Windows "ROOT" store by SHA-1 thumbprint (hex). Silent (no prompts).
     *
     * @param thumbprintHex
     *            SHA-1 thumbprint as hex (spaces/colons allowed; case-insensitive)
     * @return true if a matching cert was found & removed; false if not found
     * @throws Exception
     *             if store operations fail.
     */
    public static boolean removeCertificate(final String thumbprintHex, final WindowsCertUtils.TargetKeyStore target) throws Exception {
        final byte[] wanted = parseThumbprintHex(thumbprintHex);
        if (wanted == null || wanted.length == 0) {
            return false;
        }
        final HANDLE store = WindowsCertUtils.openStore(target, false);
        Pointer ctx = null;
        try {
            while (true) {
                // frees ctx (previous)
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                final byte[] actual = getCertSha1Thumbprint(ctx);
                if (actual != null && Arrays.equals(wanted, actual)) {
                    // CertDeleteCertificateFromStore frees the context on success.
                    // Duplicate for safe logging / property access
                    {
                        Pointer dup = WindowsCertUtilsCrypt.INSTANCE.CertDuplicateCertificateContext(ctx);
                        try {
                            final String certName = getCertDisplayName(dup);
                            LogV3.info("Deleting certificate from " + target + ": " + certName);
                        } finally {
                            if (dup != null && Pointer.nativeValue(dup) != 0) {
                                WindowsCertUtilsCrypt.INSTANCE.CertFreeCertificateContext(dup);
                            }
                        }
                    }
                    // freed ctx in any case
                    final boolean deleted = WindowsCertUtilsCrypt.INSTANCE.CertDeleteCertificateFromStore(ctx);
                    ctx = null; // Do not touch original after delete call
                    if (!deleted) {
                        int rc = Kernel32.INSTANCE.GetLastError();
                        Win32Exception e = new Win32Exception(rc);
                        throw e;
                        // throw new RuntimeException("CertDeleteCertificateFromStore failed (target=" + target + ")");
                    }
                    ctx = null; // avoid double-free
                    return true;
                }
            }
            return false;
        } finally {
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
    }

    /**
     * Reads the Windows "Root" store (Trusted Root Certification Authorities) and returns all certificates as X509Certificate instances.
     * Opens the <b>logical</b> store (CERT_STORE_PROV_SYSTEM). Callers can filter by intended purpose (e.g. SSL) using
     * {@link org.appwork.utils.net.httpconnection.trust.TrustUtils#isAcceptableCaTrustAnchorForSsl}.
     *
     * @return List of X509Certificate, or null on error / non-Windows; empty list if no certs
     */
    public static List<X509Certificate> getRootStoreCertificates() {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return null;
        }
        final int flags = WindowsCertUtilsCrypt.CERT_SYSTEM_STORE_CURRENT_USER | WindowsCertUtilsCrypt.CERT_STORE_OPEN_EXISTING_FLAG | WindowsCertUtilsCrypt.CERT_STORE_READONLY_FLAG;
        final HANDLE store = WindowsCertUtilsCrypt.INSTANCE.CertOpenStore(WindowsCertUtilsCrypt.CERT_STORE_PROV_SYSTEM, 0, null, flags, new WString("Root"));
        if (store == null || Pointer.nativeValue(store.getPointer()) == 0) {
            return null;
        }
        final List<X509Certificate> certs = new ArrayList<X509Certificate>();
        Pointer ctx = null;
        try {
            CertificateFactory cf;
            try {
                cf = CertificateFactory.getInstance("X.509");
            } catch (final CertificateException e) {
                return certs;
            }
            while (true) {
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                try {
                    final CERT_CONTEXT certCtx = Structure.newInstance(CERT_CONTEXT.class, ctx);
                    certCtx.read();
                    if (certCtx.pbCertEncoded != null && certCtx.cbCertEncoded > 0) {
                        final byte[] der = certCtx.pbCertEncoded.getByteArray(0, certCtx.cbCertEncoded);
                        final Certificate c = cf.generateCertificate(new ByteArrayInputStream(der));
                        if (c instanceof X509Certificate) {
                            certs.add((X509Certificate) c);
                        }
                    }
                } catch (final Throwable t) {
                    LogV3.warning("Skip cert in Root store: " + t.getMessage());
                }
            }
        } finally {
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
        return certs;
    }

    /**
     * Reads the Windows "Root" store (Trusted Root Certification Authorities) and returns all certificates as SHA-256 fingerprints (hex,
     * lowercase). Opens the <b>logical</b> store (CERT_STORE_PROV_SYSTEM), i.e. the merged view of Current User and Local Machine – all roots
     * that Windows trusts for the current user (including Windows Update), without admin rights. Windows only; requires JNA/crypt32.
     *
     * @return Set of SHA-256 fingerprints (lowercase hex), or null on error / non-Windows
     */
    public static Set<String> getRootStoreFingerprintsSha256() {
        final List<X509Certificate> certs = getRootStoreCertificates();
        if (certs == null || certs.isEmpty()) {
            return null;
        }
        final Set<String> fingerprints = new TreeSet<String>();
        for (final X509Certificate c : certs) {
            try {
                final String fp = Hash.getSHA256(c.getEncoded());
                if (fp != null) {
                    fingerprints.add(fp.toLowerCase(Locale.ROOT));
                }
            } catch (final CertificateEncodingException ignored) {
            }
        }
        return fingerprints.isEmpty() ? null : fingerprints;
    }

    /**
     * Reads a serialized store file (.sst), e.g. from certutil -generateSSTFromWU, via the Windows Crypto API and returns all certificates
     * therein as X509Certificate instances. Allows evaluating the Windows Update root store without certutil -exportPFX.
     * Windows only; requires JNA/crypt32; the file must exist and be readable.
     * <p>
     * <b>Note:</b> The SST from {@code certutil -generateSSTFromWU} contains the <i>full</i> Windows Update root catalog (all roots
     * that WU can deploy), typically 500+. The local "Root" store ({@link #getRootStoreCertificates()}) contains only the roots
     * actually installed on this machine (often ~150–200). So SST count is usually much higher than the local store.
     *
     * @param sstFile
     *            .sst file (e.g. from certutil -generateSSTFromWU roots.sst)
     * @return List of X509Certificate, or null on error / non-Windows / invalid file; empty list if no certs
     */
    public static List<X509Certificate> getRootStoreCertificatesFromSstFile(final java.io.File sstFile) {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return null;
        }
        if (sstFile == null || !sstFile.isFile() || !sstFile.canRead()) {
            return null;
        }
        final String path = sstFile.getAbsolutePath();
        final int flags = WindowsCertUtilsCrypt.CERT_STORE_OPEN_EXISTING_FLAG | WindowsCertUtilsCrypt.CERT_STORE_READONLY_FLAG;
        final HANDLE store = WindowsCertUtilsCrypt.INSTANCE.CertOpenStore(WindowsCertUtilsCrypt.CERT_STORE_PROV_FILENAME, 0, null, flags, new WString(path));
        if (store == null || Pointer.nativeValue(store.getPointer()) == 0) {
            return null;
        }
        final List<X509Certificate> certs = new ArrayList<X509Certificate>();
        Pointer ctx = null;
        try {
            final CertificateFactory cf;
            try {
                cf = CertificateFactory.getInstance("X.509");
            } catch (final CertificateException e) {
                return certs;
            }
            while (true) {
                ctx = WindowsCertUtilsCrypt.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                try {
                    final CERT_CONTEXT certCtx = Structure.newInstance(CERT_CONTEXT.class, ctx);
                    certCtx.read();
                    if (certCtx.pbCertEncoded != null && certCtx.cbCertEncoded > 0) {
                        final byte[] der = certCtx.pbCertEncoded.getByteArray(0, certCtx.cbCertEncoded);
                        final Certificate c = cf.generateCertificate(new ByteArrayInputStream(der));
                        if (c instanceof X509Certificate) {
                            certs.add((X509Certificate) c);
                        }
                    }
                } catch (final Throwable t) {
                    LogV3.warning("Skip cert in SST store: " + t.getMessage());
                }
            }
        } finally {
            WindowsCertUtilsCrypt.INSTANCE.CertCloseStore(store, 0);
        }
        return certs;
    }

    public static String getCertificateFingerprint(Certificate cert) throws NoSuchAlgorithmException, CertificateEncodingException {
        final MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(cert.getEncoded());
        final byte[] thumbprintBytes = md.digest();
        return HexFormatter.byteArrayToHex(thumbprintBytes);
    }
}
