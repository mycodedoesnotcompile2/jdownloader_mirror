/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.signature;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinCrypt.HCRYPTMSG;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.W32APIOptions;

/**
 * JNA interface for the subset of Windows Crypt32 API (crypt32.dll) used by {@link WindowsSignature} for reading Authenticode signatures:
 * retrieving signer info from PKCS#7 messages and decoding ASN.1 structures (e.g. countersignature blobs).
 * <p>
 * Reference: <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/">wincrypt.h (Microsoft Learn)</a>
 * </p>
 *
 * <h3>Supported Windows versions</h3>
 * <p>
 * Both functions are available on:
 * </p>
 * <ul>
 * <li><b>CryptMsgGetParam</b>: Minimum supported client Windows XP; minimum supported server Windows Server 2003.</li>
 * <li><b>CryptDecodeObject</b>: Minimum supported client Windows XP; minimum supported server Windows Server 2003.</li>
 * </ul>
 *
 * @see WindowsSignature
 */
public interface WindowsSignatureCrypt32Api extends Library {
    /** Singleton instance loading crypt32.dll. */
    WindowsSignatureCrypt32Api INSTANCE = Native.load("crypt32", WindowsSignatureCrypt32Api.class, W32APIOptions.UNICODE_OPTIONS);

    /**
     * Retrieves a specified type of data from a cryptographic message. Used by {@link WindowsSignature} to get signer info
     * (CMSG_SIGNER_INFO_PARAM) from the embedded PKCS#7 signature.
     *
     * @param hCryptMsg
     *            Handle to the cryptographic message (e.g. from {@link com.sun.jna.platform.win32.Crypt32#CryptQueryObject}).
     * @param dwParamType
     *            Type of parameter to retrieve (e.g. {@link WindowsSignature#CMSG_SIGNER_INFO_PARAM} = 6 for signer info).
     * @param dwIndex
     *            Index of the parameter (typically 0 for the first signer).
     * @param pvData
     *            Buffer to receive the data; may be {@code null} to query required size (then use {@code pcbData} for the size).
     * @param pcbData
     *            On input: size of {@code pvData}. On output: size of data copied, or required size if {@code pvData} was null.
     * @return {@code true} on success, {@code false} on failure (use {@link com.sun.jna.platform.win32.Kernel32#GetLastError}).
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptmsggetparam">CryptMsgGetParam (Microsoft
     *      Learn)</a>
     */
    boolean CryptMsgGetParam(HCRYPTMSG hCryptMsg, int dwParamType, int dwIndex, Pointer pvData, IntByReference pcbData);

    /**
     * Decodes an ASN.1-encoded structure. Used by {@link WindowsSignature} to decode PKCS#7 signer info in countersignature attributes (RFC
     * 3161 timestamps).
     *
     * @param dwCertEncodingType
     *            Encoding type, e.g. {@code X509_ASN_ENCODING | PKCS_7_ASN_ENCODING} (from {@link com.sun.jna.platform.win32.WinCrypt}).
     * @param lpszStructType
     *            OID or structure type (e.g. PKCS7_SIGNER_INFO = (LPCSTR)500). If high-order word is zero, low-order word is an integer
     *            type id.
     * @param pbEncoded
     *            Pointer to the encoded data.
     * @param cbEncoded
     *            Length of the encoded data in bytes.
     * @param dwFlags
     *            Decode flags (0 for default).
     * @param pvStructInfo
     *            Buffer for decoded structure; may be {@code null} to get required size in {@code pcbStructInfo}.
     * @param pcbStructInfo
     *            On input: size of {@code pvStructInfo}. On output: size of decoded data or required size.
     * @return {@code true} on success, {@code false} on failure.
     * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-cryptdecodeobject">CryptDecodeObject
     *      (Microsoft Learn)</a>
     */
    boolean CryptDecodeObject(int dwCertEncodingType, Pointer lpszStructType, Pointer pbEncoded, int cbEncoded, int dwFlags, Pointer pvStructInfo, IntByReference pcbStructInfo);
}
