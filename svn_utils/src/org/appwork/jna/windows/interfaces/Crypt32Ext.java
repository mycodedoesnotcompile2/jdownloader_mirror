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
import com.sun.jna.platform.win32.WinCrypt.HCRYPTMSG;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * @author thomas
 * @date 11.10.2025
 *
 */
public interface Crypt32Ext extends com.sun.jna.platform.win32.Crypt32 {
    Crypt32Ext INSTANCE = Native.load("crypt32", Crypt32Ext.class, W32APIOptions.UNICODE_OPTIONS);

    // BOOL CryptMsgGetParam(HCRYPTMSG, DWORD dwParamType, DWORD dwIndex, void* pvData, DWORD* pcbData);
    boolean CryptMsgGetParam(HCRYPTMSG hCryptMsg, int dwParamType, int dwIndex, Pointer pvData, IntByReference pcbData);

    // void CryptMsgClose(HCRYPTMSG);
    void CryptMsgClose(HCRYPTMSG hCryptMsg);

    // BOOL CryptDecodeObject(DWORD dwCertEncodingType, LPCSTR lpszStructType,
    // const BYTE *pbEncoded, DWORD cbEncoded, DWORD dwFlags,
    // void *pvStructInfo, DWORD *pcbStructInfo);
    boolean CryptDecodeObject(int dwCertEncodingType, Pointer lpszStructType, Pointer pbEncoded, int cbEncoded, int dwFlags, Pointer pvStructInfo, IntByReference pcbStructInfo);

    // int CERT_STORE_PROV_SYSTEM = 10;
    // int CERT_SYSTEM_STORE_CURRENT_USER = 0x00010000;
    // int CERT_SYSTEM_STORE_LOCAL_MACHINE = 0x00020000;
    // int CERT_STORE_OPEN_EXISTING_FLAG = 0x00004000;
    // int X509_ASN_ENCODING = 0x00000001;
    // int PKCS_7_ASN_ENCODING = 0x00010000;
    int CERT_STORE_ADD_REPLACE_EXISTING = 3;
    // Thumbprint (SHA-1 hash) property id
    int CERT_SHA1_HASH_PROP_ID          = 3;

    HANDLE CertOpenStore(int lpszStoreProvider, int dwMsgAndCertEncodingType, Pointer hCryptProv, int dwFlags, WString pvPara);

    boolean CertCloseStore(HANDLE hCertStore, int dwFlags);

    boolean CertAddEncodedCertificateToStore(HANDLE hCertStore, int dwCertEncodingType, byte[] pbCertEncoded, int cbCertEncoded, int dwAddDisposition, PointerByReference pAdded);

    boolean CertSetCertificateContextProperty(Pointer pCertContext, int dwPropId, int dwFlags, Pointer pvData);

    Pointer CertDuplicateCertificateContext(Pointer pCertContext);

    int                     CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
    int                     CERT_NAME_ISSUER_FLAG         = 0x00000001;
    int                     CERT_STORE_ADD_ALWAYS         = 4;
    public static final int CERT_FRIENDLY_NAME_PROP_ID    = 11;

    int CertGetNameStringW(Pointer pCertContext, int dwType, int dwFlags, Pointer pvTypePara, char[] pszNameString, int cchNameString);

    Pointer CertEnumCertificatesInStore(HANDLE hCertStore, Pointer pPrevCertContext);

    boolean CertGetCertificateContextProperty(Pointer pCertContext, int dwPropId, byte[] pvData, IntByReference pcbData);

    boolean CertDeleteCertificateFromStore(Pointer pCertContext);

    boolean CertFreeCertificateContext(Pointer pCertContext);
}
