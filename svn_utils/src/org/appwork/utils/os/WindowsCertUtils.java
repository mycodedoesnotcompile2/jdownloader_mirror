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

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Arrays;

import org.appwork.jna.windows.interfaces.Crypt32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.formatter.HexFormatter;

import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinCrypt;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;

/**
 * @author thomas
 * @date 29.01.2026
 *
 */
public class WindowsCertUtils {
    public enum KeyStore {
        CURRENT_USER,
        /* requires admin */
        LOCAL_MACHINE
    }

    static byte[] getCertSha1Thumbprint(final Pointer certContext) {
        final IntByReference size = new IntByReference(0);
        // First call to get required size
        final boolean okSize = Crypt32Ext.INSTANCE.CertGetCertificateContextProperty(certContext, Crypt32Ext.CERT_SHA1_HASH_PROP_ID, null, size);
        if (!okSize || size.getValue() <= 0) {
            return null;
        }
        final byte[] buf = new byte[size.getValue()];
        final boolean okData = Crypt32Ext.INSTANCE.CertGetCertificateContextProperty(certContext, Crypt32Ext.CERT_SHA1_HASH_PROP_ID, buf, size);
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
    static HANDLE openStore(final WindowsCertUtils.KeyStore target, final boolean readOnly) {
        // Select physical registry location (HKCU or HKLM)
        final int locationFlag = (target == WindowsCertUtils.KeyStore.LOCAL_MACHINE) ? WinCrypt.CERT_SYSTEM_STORE_LOCAL_MACHINE : WinCrypt.CERT_SYSTEM_STORE_CURRENT_USER;
        int flags = locationFlag | WinCrypt.CERT_STORE_OPEN_EXISTING_FLAG;
        if (readOnly) {
            flags |= WinCrypt.CERT_STORE_READONLY_FLAG;
        }
        // CERT_STORE_PROV_SYSTEM_REGISTRY opens a physical store only
        final HANDLE store = Crypt32Ext.INSTANCE.CertOpenStore(WinCrypt.CERT_STORE_PROV_SYSTEM_REGISTRY, 0, null, flags, new WString("Root"));
        if (store == null || Pointer.nativeValue(store.getPointer()) == 0) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        return store;
    }

    public static boolean isCertificateInstalled(final String thumbprintHex, final WindowsCertUtils.KeyStore target) {
        final byte[] wanted = parseThumbprintHex(thumbprintHex);
        if (wanted == null || wanted.length == 0) {
            return false;
        }
        final HANDLE store = WindowsCertUtils.openStore(target, true);
        Pointer ctx = null;
        try {
            while (true) {
                ctx = Crypt32Ext.INSTANCE.CertEnumCertificatesInStore(store, ctx);
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
                Crypt32Ext.INSTANCE.CertFreeCertificateContext(ctx);
            }
            Crypt32Ext.INSTANCE.CertCloseStore(store, 0);
        }
    }

    /**
     * Installs the given X509 certificate into the Windows "ROOT" store for the given target. Silent (no prompts).
     *
     * @throws Exception
     *             if Windows store open/add fails.
     */
    public static void installCertificate(final X509Certificate certificate, final WindowsCertUtils.KeyStore target) throws Exception {
        if (certificate == null) {
            return;
        }
        final byte[] der = certificate.getEncoded();
        final HANDLE store = WindowsCertUtils.openStore(target, false);
        try {
            final boolean ok = Crypt32Ext.INSTANCE.CertAddEncodedCertificateToStore(store, WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, der, der.length, Crypt32Ext.CERT_STORE_ADD_REPLACE_EXISTING, null);
            if (!ok) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
        } finally {
            Crypt32Ext.INSTANCE.CertCloseStore(store, 0);
        }
    }

    static String getCertDisplayName(final Pointer certContext) {
        // First call: get required length (incl. null-terminator)
        int len = Crypt32Ext.INSTANCE.CertGetNameStringW(certContext, Crypt32Ext.CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, null, null, 0);
        if (len <= 1) {
            return "<unknown>";
        }
        char[] buf = new char[len];
        Crypt32Ext.INSTANCE.CertGetNameStringW(certContext, Crypt32Ext.CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, null, buf, buf.length);
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
    public static boolean removeCertificate(final String thumbprintHex, final WindowsCertUtils.KeyStore target) throws Exception {
        final byte[] wanted = parseThumbprintHex(thumbprintHex);
        if (wanted == null || wanted.length == 0) {
            return false;
        }
        final HANDLE store = WindowsCertUtils.openStore(target, false);
        Pointer ctx = null;
        try {
            while (true) {
                // frees ctx (previous)
                ctx = Crypt32Ext.INSTANCE.CertEnumCertificatesInStore(store, ctx);
                if (ctx == null || Pointer.nativeValue(ctx) == 0) {
                    break;
                }
                final byte[] actual = getCertSha1Thumbprint(ctx);
                if (actual != null && Arrays.equals(wanted, actual)) {
                    // CertDeleteCertificateFromStore frees the context on success.
                    // Duplicate for safe logging / property access
                    {
                        Pointer dup = Crypt32Ext.INSTANCE.CertDuplicateCertificateContext(ctx);
                        try {
                            final String certName = getCertDisplayName(dup);
                            LogV3.info("Deleting certificate from " + target + ": " + certName);
                        } finally {
                            if (dup != null && Pointer.nativeValue(dup) != 0) {
                                Crypt32Ext.INSTANCE.CertFreeCertificateContext(dup);
                            }
                        }
                    }
                    // freed ctx in any case
                    final boolean deleted = Crypt32Ext.INSTANCE.CertDeleteCertificateFromStore(ctx);
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
            Crypt32Ext.INSTANCE.CertCloseStore(store, 0);
        }
    }

    public static String getCertificateFingerprint(Certificate cert) throws NoSuchAlgorithmException, CertificateEncodingException {
        final MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(cert.getEncoded());
        final byte[] thumbprintBytes = md.digest();
        return HexFormatter.byteArrayToHex(thumbprintBytes);
    }
}
