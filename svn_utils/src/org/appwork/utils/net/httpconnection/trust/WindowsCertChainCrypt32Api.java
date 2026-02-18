/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinCrypt.CERT_CONTEXT;
import com.sun.jna.platform.win32.WinCrypt.HCERTSTORE;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * JNA interface for the subset of Windows Crypt32 API (crypt32.dll) used by {@link JNAWindowsTrustProvider} for certificate chain
 * validation. Contains only the two functions required to create a certificate context from encoded bytes and to add encoded certificates
 * to an in-memory store for chain building.
 * <p>
 * Reference: <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/">wincrypt.h (Microsoft Learn)</a>
 * </p>
 *
 * <h3>Supported Windows versions</h3>
 * <p>
 * Both functions are part of the standard Certificate Store Functions and are available on:
 * </p>
 * <ul>
 * <li><b>CertCreateCertificateContext</b>: Minimum supported client Windows XP; minimum supported server Windows Server 2003.</li>
 * <li><b>CertAddEncodedCertificateToStore</b>: Minimum supported client Windows 2000 Professional; minimum supported server Windows 2000
 * Server.</li>
 * </ul>
 * <p>
 * In practice, any Windows version that runs the JNA-based trust provider (typically Windows 7 or later in support) supports these APIs.
 * </p>
 */
public interface WindowsCertChainCrypt32Api extends Library {
    /** Singleton instance loading crypt32.dll. */
    WindowsCertChainCrypt32Api INSTANCE = Native.load("crypt32", WindowsCertChainCrypt32Api.class, W32APIOptions.UNICODE_OPTIONS);

    /**
     * Creates a certificate context from an encoded certificate. The context is not persisted to a store and must be freed by the caller
     * with {@link com.sun.jna.platform.win32.Crypt32#CertFreeCertificateContext}.
     *
     * @param dwCertEncodingType
     *            Encoding type, e.g. {@code X509_ASN_ENCODING | PKCS_7_ASN_ENCODING} (from {@link com.sun.jna.platform.win32.WinCrypt}).
     * @param pbCertEncoded
     *            Pointer to the encoded certificate bytes (e.g. JNA {@link com.sun.jna.Memory}).
     * @param cbCertEncoded
     *            Length of the encoded data in bytes.
     * @return A {@link CERT_CONTEXT} reference, or {@code null} on failure (use {@link com.sun.jna.Native#getLastError()} for error code).
     * @see <a href=
     *      "https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certcreatecertificatecontext">CertCreateCertificateContext
     *      (Microsoft Learn)</a>
     * @see <a href=
     *      "https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certfreecertificatecontext">CertFreeCertificateContext</a>
     */
    CERT_CONTEXT.ByReference CertCreateCertificateContext(int dwCertEncodingType, Pointer pbCertEncoded, int cbCertEncoded);

    /**
     * Adds an encoded certificate to a certificate store. Used by {@link JNAWindowsTrustProvider} to add intermediate certificates to an
     * in-memory store so that {@code CertGetCertificateChain} can build the full chain.
     *
     * @param hCertStore
     *            Handle to the certificate store (e.g. from {@link com.sun.jna.platform.win32.Crypt32#CertOpenStore} with
     *            {@code CERT_STORE_PROV_MEMORY}).
     * @param dwCertEncodingType
     *            Encoding type, e.g. {@code X509_ASN_ENCODING | PKCS_7_ASN_ENCODING}.
     * @param pbCertEncoded
     *            Pointer to the encoded certificate bytes.
     * @param cbCertEncoded
     *            Length of the encoded data in bytes.
     * @param dwAddDisposition
     *            Disposition, e.g. {@code CERT_STORE_ADD_ALWAYS} (4) to add even if duplicate.
     * @param ppCertContext
     *            Optional output for the created context; may be {@code null} if the context is not needed.
     * @return {@code true} on success, {@code false} on failure.
     * @see <a href=
     *      "https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certaddencodedcertificatetostore">CertAddEncodedCertificateToStore
     *      (Microsoft Learn)</a>
     */
    boolean CertAddEncodedCertificateToStore(HCERTSTORE hCertStore, int dwCertEncodingType, Pointer pbCertEncoded, int cbCertEncoded, int dwAddDisposition, PointerByReference ppCertContext);
}
