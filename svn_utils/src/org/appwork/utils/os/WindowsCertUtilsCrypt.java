/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         ====================================================================================================================================================
 *         ==================================================================================================================================================== */
package org.appwork.utils.os;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * Minimal JNA binding to Windows Crypt32 API (crypt32.dll) for use by {@link WindowsCertUtils} only. Exposes only the certificate
 * store and certificate context functions actually used: open/close store, enumerate certificates, add encoded certs, get/set
 * properties, display names, duplicate/free context. Does not extend {@link org.appwork.jna.windows.interfaces.Crypt32Ext}.
 * <p>
 * Reference: <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/">wincrypt.h (Microsoft Learn)</a>
 * </p>
 *
 * <h3>Supported Windows versions</h3>
 * <p>
 * The exposed functions are part of the standard Certificate Store API. Typical minimum support: client Windows 2000 Professional /
 * server Windows 2000 Server; some functions (e.g. CertOpenStore with system store) are documented as Windows XP / Windows Server
 * 2003 minimum. In practice, any Windows version targeted by {@link WindowsCertUtils} supports these APIs.
 * </p>
 */
public interface WindowsCertUtilsCrypt extends Library {
    WindowsCertUtilsCrypt INSTANCE = Native.load("crypt32", WindowsCertUtilsCrypt.class, W32APIOptions.UNICODE_OPTIONS);

    // -------------------------------------------------------------------------
    // CertOpenStore: store provider type (first argument)
    // @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopenstore">CertOpenStore</a>
    // -------------------------------------------------------------------------
    /** Store provider: open from file (pvPara = file path). */
    int CERT_STORE_PROV_FILENAME        = 8;
    /** Store provider: system store (pvPara = store name, e.g. "ROOT", "CA"). */
    int CERT_STORE_PROV_SYSTEM          = 10;
    /** Store provider: system store via registry. */
    int CERT_STORE_PROV_SYSTEM_REGISTRY = 13;

    // -------------------------------------------------------------------------
    // CertOpenStore: dwFlags (system store location in high word)
    // -------------------------------------------------------------------------
    /** Use current user's system store. */
    int CERT_SYSTEM_STORE_CURRENT_USER    = 0x00010000;
    /** Use local machine system store. */
    int CERT_SYSTEM_STORE_LOCAL_MACHINE   = 0x00020000;
    /** Open only if store already exists. */
    int CERT_STORE_OPEN_EXISTING_FLAG     = 0x00004000;
    /** Open store read-only. */
    int CERT_STORE_READONLY_FLAG          = 0x00008000;

    /** Add disposition: add cert even if duplicate (for CertAddEncodedCertificateToStore). */
    int CERT_STORE_ADD_ALWAYS = 4;

    /** Property ID: SHA-1 thumbprint (hash) of the certificate. */
    int CERT_SHA1_HASH_PROP_ID     = 3;
    /** Property ID: friendly name (for CertGetCertificateContextProperty / CertSetCertificateContextProperty). */
    int CERT_FRIENDLY_NAME_PROP_ID = 11;

    /** CertGetNameStringW: simple display format (e.g. "CN=Name, O=Org"). */
    int CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
    /** CertGetNameStringW: include issuer in name. */
    int CERT_NAME_ISSUER_FLAG         = 0x00000001;

    /** Encoding: X.509 ASN.1 (use with PKCS_7_ASN_ENCODING for cert operations). */
    int X509_ASN_ENCODING   = 0x00000001;
    /** Encoding: PKCS#7 ASN.1. */
    int PKCS_7_ASN_ENCODING = 0x00010000;

    /**
     * Opens a certificate store. Use {@link #CERT_STORE_PROV_SYSTEM} with {@link #CERT_SYSTEM_STORE_CURRENT_USER} or
     * {@link #CERT_SYSTEM_STORE_LOCAL_MACHINE} for system stores (e.g. ROOT, CA); pvPara is the store name.
     *
     * @param lpszStoreProvider
     *            Provider type (e.g. {@link #CERT_STORE_PROV_SYSTEM}, {@link #CERT_STORE_PROV_FILENAME}).
     * @param dwMsgAndCertEncodingType
     *            Encoding (e.g. {@link #X509_ASN_ENCODING} | {@link #PKCS_7_ASN_ENCODING}).
     * @param hCryptProv
     *            Unused; pass null.
     * @param dwFlags
     *            Store flags (e.g. {@link #CERT_SYSTEM_STORE_CURRENT_USER}, {@link #CERT_STORE_OPEN_EXISTING_FLAG}).
     * @param pvPara
     *            Store name (e.g. "ROOT") or file path, depending on provider.
     * @return Store handle, or null on failure.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopenstore">CertOpenStore (Microsoft Learn)</a>
     */
    HANDLE CertOpenStore(int lpszStoreProvider, int dwMsgAndCertEncodingType, Pointer hCryptProv, int dwFlags, WString pvPara);

    /**
     * Closes a certificate store and frees its handle.
     *
     * @param hCertStore
     *            Store handle from CertOpenStore.
     * @param dwFlags
     *            Usually 0; CERT_CLOSE_STORE_CHECK_FLAG etc. optional.
     * @return true on success.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certclosestore">CertCloseStore (Microsoft Learn)</a>
     */
    boolean CertCloseStore(HANDLE hCertStore, int dwFlags);

    /**
     * Enumerates certificates in the store. Call repeatedly with the previous context (or null for the first) until null.
     *
     * @param hCertStore
     *            Store handle.
     * @param pPrevCertContext
     *            Previous cert context pointer, or null to start.
     * @return Next CERT_CONTEXT pointer, or null when no more.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certenumcertificatesinstore">CertEnumCertificatesInStore (Microsoft Learn)</a>
     */
    Pointer CertEnumCertificatesInStore(HANDLE hCertStore, Pointer pPrevCertContext);

    /**
     * Duplicates a certificate context (increments reference count). Must be freed with CertFreeCertificateContext.
     *
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certduplicatecertificatecontext">CertDuplicateCertificateContext (Microsoft Learn)</a>
     */
    Pointer CertDuplicateCertificateContext(Pointer pCertContext);

    /**
     * Adds an encoded certificate to the store.
     *
     * @param hCertStore
     *            Store handle.
     * @param dwCertEncodingType
     *            {@link #X509_ASN_ENCODING} | {@link #PKCS_7_ASN_ENCODING}.
     * @param pbCertEncoded
     *            DER-encoded certificate bytes.
     * @param cbCertEncoded
     *            Length of encoded data.
     * @param dwAddDisposition
     *            {@link #CERT_STORE_ADD_ALWAYS} or CertStoreAddDisposition values.
     * @param pAdded
     *            Optional output for the created context; may be null.
     * @return true on success.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certaddencodedcertificatetostore">CertAddEncodedCertificateToStore (Microsoft Learn)</a>
     */
    boolean CertAddEncodedCertificateToStore(HANDLE hCertStore, int dwCertEncodingType, byte[] pbCertEncoded, int cbCertEncoded, int dwAddDisposition, PointerByReference pAdded);

    /**
     * Sets a property on a certificate context (e.g. friendly name).
     *
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certsetcertificatecontextproperty">CertSetCertificateContextProperty (Microsoft Learn)</a>
     */
    boolean CertSetCertificateContextProperty(Pointer pCertContext, int dwPropId, int dwFlags, Pointer pvData);

    /**
     * Retrieves a display string for the certificate (e.g. subject or issuer name). Pass a char[] for pszNameString and its length
     * in cchNameString.
     *
     * @param dwType
     *            Format type (e.g. {@link #CERT_NAME_SIMPLE_DISPLAY_TYPE}).
     * @param dwFlags
     *            Optional flags (e.g. {@link #CERT_NAME_ISSUER_FLAG}).
     * @return Number of characters written (excluding null), or 0 on failure.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetnamestringw">CertGetNameStringW (Microsoft Learn)</a>
     */
    int CertGetNameStringW(Pointer pCertContext, int dwType, int dwFlags, Pointer pvTypePara, char[] pszNameString, int cchNameString);

    /**
     * Retrieves a property from a certificate context (e.g. SHA-1 thumbprint, friendly name). Call with pvData null to get
     * required size in pcbData, then call again with allocated buffer.
     *
     * @param dwPropId
     *            Property ID (e.g. {@link #CERT_SHA1_HASH_PROP_ID}, {@link #CERT_FRIENDLY_NAME_PROP_ID}).
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetcertificatecontextproperty">CertGetCertificateContextProperty (Microsoft Learn)</a>
     */
    boolean CertGetCertificateContextProperty(Pointer pCertContext, int dwPropId, byte[] pvData, IntByReference pcbData);

    /**
     * Deletes a certificate from the store. The context is freed by the store; do not use it after this call.
     *
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certdeletecertificatefromstore">CertDeleteCertificateFromStore (Microsoft Learn)</a>
     */
    boolean CertDeleteCertificateFromStore(Pointer pCertContext);

    /**
     * Frees a certificate context obtained from CertDuplicateCertificateContext or from enumeration. Do not free contexts that were
     * deleted with CertDeleteCertificateFromStore.
     *
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certfreecertificatecontext">CertFreeCertificateContext (Microsoft Learn)</a>
     */
    boolean CertFreeCertificateContext(Pointer pCertContext);
}
