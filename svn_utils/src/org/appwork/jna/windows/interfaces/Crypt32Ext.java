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
package org.appwork.jna.windows.interfaces;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.WinCrypt.CERT_CONTEXT;
import com.sun.jna.platform.win32.WinCrypt.HCERTSTORE;
import com.sun.jna.platform.win32.WinCrypt.HCRYPTMSG;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * Extended Crypt32 (wincrypt) JNA interface. MSDN: <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/">wincrypt.h</a>
 *
 * @author thomas
 * @date 11.10.2025
 */
public interface Crypt32Ext extends com.sun.jna.platform.win32.Crypt32 {
    Crypt32Ext INSTANCE = Native.load("crypt32", Crypt32Ext.class, W32APIOptions.UNICODE_OPTIONS);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptmsggetparam">CryptMsgGetParam</a> */
    boolean CryptMsgGetParam(HCRYPTMSG hCryptMsg, int dwParamType, int dwIndex, Pointer pvData, IntByReference pcbData);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptmsgclose">CryptMsgClose</a> */
    void CryptMsgClose(HCRYPTMSG hCryptMsg);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptdecodeobject">CryptDecodeObject</a> */
    boolean CryptDecodeObject(int dwCertEncodingType, Pointer lpszStructType, Pointer pbEncoded, int cbEncoded, int dwFlags, Pointer pvStructInfo, IntByReference pcbStructInfo);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopenstore">CertOpenStore</a> – CERT_STORE_PROV_SYSTEM=10, CERT_SYSTEM_STORE_* etc. */
    int CERT_STORE_ADD_REPLACE_EXISTING = 3;
    /** Thumbprint (SHA-1 hash) property id; @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetcertificatecontextproperty">CertGetCertificateContextProperty</a> */
    int CERT_SHA1_HASH_PROP_ID          = 3;

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopenstore">CertOpenStore</a> */
    HANDLE CertOpenStore(int lpszStoreProvider, int dwMsgAndCertEncodingType, Pointer hCryptProv, int dwFlags, WString pvPara);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certclosestore">CertCloseStore</a> */
    boolean CertCloseStore(HANDLE hCertStore, int dwFlags);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certaddencodedcertificatetostore">CertAddEncodedCertificateToStore</a> */
    boolean CertAddEncodedCertificateToStore(HANDLE hCertStore, int dwCertEncodingType, byte[] pbCertEncoded, int cbCertEncoded, int dwAddDisposition, PointerByReference pAdded);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certsetcertificatecontextproperty">CertSetCertificateContextProperty</a> */
    boolean CertSetCertificateContextProperty(Pointer pCertContext, int dwPropId, int dwFlags, Pointer pvData);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certduplicatecertificatecontext">CertDuplicateCertificateContext</a> */
    Pointer CertDuplicateCertificateContext(Pointer pCertContext);

    /** CertGetNameStringW display type / flags; @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetnamestringw">CertGetNameStringW</a> */
    int                     CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
    int                     CERT_NAME_ISSUER_FLAG         = 0x00000001;
    /** CertOpenStore / CertAddEncodedCertificateToStore disposition; @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopenstore">CertOpenStore</a> */
    int                     CERT_STORE_ADD_ALWAYS         = 4;
    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetcertificatecontextproperty">CertGetCertificateContextProperty</a>, <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certsetcertificatecontextproperty">CertSetCertificateContextProperty</a> */
    public static final int CERT_FRIENDLY_NAME_PROP_ID    = 11;

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetnamestringw">CertGetNameStringW</a> */
    int CertGetNameStringW(Pointer pCertContext, int dwType, int dwFlags, Pointer pvTypePara, char[] pszNameString, int cchNameString);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certenumcertificatesinstore">CertEnumCertificatesInStore</a> */
    Pointer CertEnumCertificatesInStore(HANDLE hCertStore, Pointer pPrevCertContext);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certgetcertificatecontextproperty">CertGetCertificateContextProperty</a> */
    boolean CertGetCertificateContextProperty(Pointer pCertContext, int dwPropId, byte[] pvData, IntByReference pcbData);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certdeletecertificatefromstore">CertDeleteCertificateFromStore</a> */
    boolean CertDeleteCertificateFromStore(Pointer pCertContext);

    /** @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certfreecertificatecontext">CertFreeCertificateContext</a> */
    boolean CertFreeCertificateContext(Pointer pCertContext);

    /**
     * Creates a certificate context from an encoded certificate. The context is not persisted to a store and must be freed with
     * CertFreeCertificateContext.
     *
     * @param dwCertEncodingType
     *            X509_ASN_ENCODING | PKCS_7_ASN_ENCODING
     * @param pbCertEncoded
     *            encoded certificate bytes
     * @param cbCertEncoded
     *            length of encoded data
     * @return CERT_CONTEXT or null on failure
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certcreatecertificatecontext">CertCreateCertificateContext</a>
     */
    CERT_CONTEXT.ByReference CertCreateCertificateContext(int dwCertEncodingType, Pointer pbCertEncoded, int cbCertEncoded);

    /**
     * Adds an encoded certificate to a certificate store.
     *
     * @param hCertStore
     *            store handle
     * @param dwCertEncodingType
     *            encoding type
     * @param pbCertEncoded
     *            encoded certificate
     * @param cbCertEncoded
     *            length
     * @param dwAddDisposition
     *            CERT_STORE_ADD_* (e.g. CERT_STORE_ADD_NEW = 1)
     * @param ppCertContext
     *            optional output; can be null
     * @return true on success
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certaddencodedcertificatetostore">CertAddEncodedCertificateToStore</a>
     */
    boolean CertAddEncodedCertificateToStore(HCERTSTORE hCertStore, int dwCertEncodingType, Pointer pbCertEncoded, int cbCertEncoded, int dwAddDisposition, PointerByReference ppCertContext);
}
