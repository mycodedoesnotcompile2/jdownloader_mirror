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
package org.appwork.utils.os.windows.signature;

import java.io.File;
import java.security.MessageDigest;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import org.appwork.jna.windows.WinTrust;
import org.appwork.jna.windows.interfaces.CMSG_SIGNER_INFO;
import org.appwork.jna.windows.interfaces.CRYPT_ATTRIBUTE;
import org.appwork.jna.windows.interfaces.CRYPT_ATTRIBUTES;
import org.appwork.jna.windows.interfaces.CRYPT_ATTR_BLOB;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Crypt32;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinCrypt;
import com.sun.jna.platform.win32.WinCrypt.CERT_CONTEXT;
import com.sun.jna.platform.win32.WinCrypt.HCERTSTORE;
import com.sun.jna.platform.win32.WinCrypt.HCRYPTMSG;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Utility class for reading and verifying Authenticode signatures on Windows executables and DLLs.
 * <p>
 * This class provides methods to:
 * <ul>
 * <li>Extract code signing certificate information (subject, issuer, fingerprint, validity period)</li>
 * <li>Read signing timestamps from both legacy Authenticode and RFC 3161 countersignatures</li>
 * <li>Verify signature validity using Windows Trust API</li>
 * </ul>
 * </p>
 * <p>
 * The implementation uses JNA to access Windows Crypto API (crypt32.dll) and WinTrust API (wintrust.dll).
 * </p>
 *
 * @author thomas
 * @date 11.10.2025
 */
public class WindowsSignature {
    /**
     * Key name for signing date in result maps
     */
    public static final String SIGN_DATE   = "signDate";
    /**
     * Key name for certificate fingerprint in result maps
     */
    public static final String FINGERPRINT = "fingerprint";

    /**
     * Reads the primary Authenticode signer certificate from a signed PE file and extracts all relevant information:
     * <ul>
     * <li>Issued To (Subject)</li>
     * <li>Issued By (Issuer)</li>
     * <li>Fingerprint (SHA-1)</li>
     * <li>Certificate validity</li>
     * <li>Authenticode signing date (if present)</li>
     * </ul>
     *
     * @param file
     *            Signed executable (.exe / .dll)
     * @return CodeSignature object with parsed details or {@code null} if none found.
     */
    public static CodeSignature readCodeSignSignature(File file) throws InvalidNameException {
        if (file == null || !file.isFile()) {
            System.err.println("ERROR: Invalid file.");
            return null;
        }
        // Convert Java String to native wide-character (UTF-16) array for Windows API
        char[] fileChars = Native.toCharArray(file.getAbsolutePath());
        Pointer filePtr = new Memory((fileChars.length + 1L) * Native.WCHAR_SIZE);
        filePtr.write(0, fileChars, 0, fileChars.length);
        // Output parameters for CryptQueryObject
        PointerByReference phCertStore = new PointerByReference();
        PointerByReference phMsg = new PointerByReference();
        PointerByReference ppvContext = new PointerByReference();
        // Query the embedded PKCS#7 signature from the PE file
        boolean ok = Crypt32.INSTANCE.CryptQueryObject(WinCrypt.CERT_QUERY_OBJECT_FILE, filePtr, WinCrypt.CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED, WinCrypt.CERT_QUERY_FORMAT_FLAG_BINARY, 0, new IntByReference(), new IntByReference(), new IntByReference(), phCertStore, phMsg, ppvContext);
        if (!ok) {
            int lastError = Kernel32.INSTANCE.GetLastError();
            System.err.println("ERROR: CryptQueryObject failed, error: " + lastError);
            return null;
        }
        HCERTSTORE hStore = new HCERTSTORE(phCertStore.getValue());
        HCRYPTMSG hMsg = new HCRYPTMSG(phMsg.getValue());
        try {
            // ------------------------------------------------------------
            // Step 1: Extract signer info from the embedded PKCS#7 message
            // The signer info contains issuer+serial to identify the signing certificate
            // ------------------------------------------------------------
            IntByReference pcbData = new IntByReference();
            if (!WindowsSignatureCrypt32Api.INSTANCE.CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, Pointer.NULL, pcbData)) {
                return null;
            }
            int cbData = pcbData.getValue();
            if (cbData <= 0) {
                return null;
            }
            Memory signerInfoMem = new Memory(cbData);
            if (!WindowsSignatureCrypt32Api.INSTANCE.CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, signerInfoMem, pcbData)) {
                return null;
            }
            CMSG_SIGNER_INFO signerInfo = new CMSG_SIGNER_INFO(signerInfoMem);
            signerInfo.read();
            // ------------------------------------------------------------
            // Step 2: Find the matching signer certificate in the certificate store
            // We compare issuer+serial to avoid confusion with TSA certificates
            // ------------------------------------------------------------
            CERT_CONTEXT.ByReference current = null;
            CERT_CONTEXT.ByReference found = null;
            while ((current = Crypt32.INSTANCE.CertEnumCertificatesInStore(hStore, current == null ? Pointer.NULL : current.getPointer())) != null) {
                if (compareIssuerAndSerial(current.pCertInfo.Issuer, current.pCertInfo.SerialNumber, signerInfo.Issuer, signerInfo.SerialNumber)) {
                    found = current;
                    break;
                }
            }
            if (found == null) {
                System.err.println("WARNING: No matching signer certificate found.");
                return null;
            }
            CodeSignature sig = new CodeSignature();
            // ------------------------------------------------------------
            // Step 3: Extract certificate Subject (the "Issued To" entity)
            // Contains CN, O, OU, L, S, C and other RDNs
            // ------------------------------------------------------------
            extractNameFromBlob(found.pCertInfo.Subject, sig.getIssuedTo());
            // ------------------------------------------------------------
            // Step 4: Extract certificate Issuer (the Certificate Authority)
            // Contains the CA's distinguished name
            // ------------------------------------------------------------
            extractNameFromBlob(found.pCertInfo.Issuer, sig.getIssuedBy());
            // ------------------------------------------------------------
            // Step 5: Compute SHA-1 fingerprint (thumbprint) of the certificate
            // This is a unique identifier for the certificate
            // ------------------------------------------------------------
            Pointer pbEncoded = found.pbCertEncoded;
            int len = found.cbCertEncoded;
            if (pbEncoded != null && len > 0) {
                byte[] encoded = pbEncoded.getByteArray(0, len);
                String sha1 = computeFingerprint("SHA-1", encoded);
                sig.setFingerprint(sha1);
            }
            // ------------------------------------------------------------
            // Step 6: Extract certificate validity period (NotAfter timestamp)
            // Windows FILETIME is 100-nanosecond intervals since 1601-01-01
            // ------------------------------------------------------------
            if (found.pCertInfo.NotAfter != null) {
                int high = found.pCertInfo.NotAfter.dwHighDateTime;
                int low = found.pCertInfo.NotAfter.dwLowDateTime;
                long filetime = ((long) high << 32) | (low & 0xFFFFFFFFL);
                long msSince1601 = filetime / (10 * 1000l);
                long epochDiff = 11644473600000L; // milliseconds between 1601-01-01 and 1970-01-01
                long msSince1970 = msSince1601 - epochDiff;
                if (msSince1970 > 0) {
                    sig.setValidUntil(new Date(msSince1970));
                }
            }
            // ------------------------------------------------------------
            // Step 7: Extract signing timestamp from PKCS#9 attributes
            // Checks both legacy signingTime and RFC 3161 countersignatures
            // ------------------------------------------------------------
            String signDate = getSigningTimeFromMessage(hMsg);
            if (signDate != null) {
                try {
                    sig.setSigningDate(DateMapper.parse(signDate));
                } catch (Exception ignored) {
                }
            }
            return sig;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            Crypt32.INSTANCE.CertCloseStore(hStore, 0);
        }
    }

    /**
     * Parses an X.509 distinguished name (Subject or Issuer) and extracts all Relative Distinguished Names (RDNs) into a map.
     * <p>
     * Common RDN types include: CN (Common Name), O (Organization), OU (Organizational Unit), L (Locality), S (State/Province), C
     * (Country), STREET, SERIALNUMBER, etc.
     * </p>
     *
     * @param blob
     *            The encoded X.509 name blob from the certificate
     * @param target
     *            The map to populate with RDN key-value pairs
     * @throws InvalidNameException
     *             if the name cannot be parsed as a valid LDAP DN
     */
    private static void extractNameFromBlob(WinCrypt.DATA_BLOB blob, Map<String, String> target) throws InvalidNameException {
        if (target == null) {
            return;
        }
        int requiredChars = Crypt32.INSTANCE.CertNameToStr(WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, blob, WinCrypt.CERT_X500_NAME_STR, Pointer.NULL, 0);
        if (requiredChars <= 1) {
            return;
        }
        Memory buffer = new Memory((long) requiredChars * Native.WCHAR_SIZE);
        Crypt32.INSTANCE.CertNameToStr(WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, blob, WinCrypt.CERT_X500_NAME_STR, buffer, requiredChars);
        String name = buffer.getWideString(0);
        if (name == null || name.trim().isEmpty()) {
            return;
        }
        LdapName ldap = new LdapName(name);
        for (Rdn rdn : ldap.getRdns()) {
            target.put(rdn.getType(), String.valueOf(rdn.getValue()));
        }
    }

    /**
     * Compares two certificate identifiers (Issuer + SerialNumber) to match the correct signer certificate.
     * <p>
     * This is used to identify the actual signing certificate and avoid confusion with timestamp authority (TSA) certificates that may also
     * be present in the certificate store.
     * </p>
     *
     * @param issuer1
     *            Issuer blob from first certificate
     * @param serial1
     *            Serial number blob from first certificate
     * @param issuer2
     *            Issuer blob from second certificate
     * @param serial2
     *            Serial number blob from second certificate
     * @return true if both issuer and serial number match exactly
     */
    private static boolean compareIssuerAndSerial(WinCrypt.DATA_BLOB issuer1, WinCrypt.DATA_BLOB serial1, WinCrypt.DATA_BLOB issuer2, WinCrypt.DATA_BLOB serial2) {
        if (issuer1.cbData != issuer2.cbData || serial1.cbData != serial2.cbData) {
            return false;
        }
        byte[] i1 = issuer1.pbData.getByteArray(0, issuer1.cbData);
        byte[] i2 = issuer2.pbData.getByteArray(0, issuer2.cbData);
        byte[] s1 = serial1.pbData.getByteArray(0, serial1.cbData);
        byte[] s2 = serial2.pbData.getByteArray(0, serial2.cbData);
        return Arrays.equals(i1, i2) && Arrays.equals(s1, s2);
    }

    /**
     * Computes a cryptographic fingerprint (hash) of the given data using the specified algorithm.
     * <p>
     * The result is returned as an uppercase hexadecimal string (e.g., "AF2C88FF0B77...").
     * </p>
     *
     * @param algo
     *            Hash algorithm name (e.g., "SHA-1", "SHA-256")
     * @param data
     *            The byte array to hash
     * @return Uppercase hex-encoded fingerprint string
     * @throws Exception
     *             if the algorithm is not available
     */
    static String computeFingerprint(String algo, byte[] data) throws Exception {
        MessageDigest md = MessageDigest.getInstance(algo);
        byte[] digest = md.digest(data);
        StringBuilder sb = new StringBuilder(digest.length * 2);
        for (int i = 0; i < digest.length; i++) {
            int b = digest[i] & 0xFF;
            if (b < 0x10) {
                sb.append('0');
            }
            sb.append(Integer.toHexString(b).toUpperCase());
        }
        return sb.toString();
    }

    // ------------------------------------------------------------
    // Windows Crypto API Constants
    // ------------------------------------------------------------
    /**
     * Parameter ID for retrieving signer info from a cryptographic message. Used with CryptMsgGetParam.
     */
    public static final int CMSG_SIGNER_INFO_PARAM = 6;
    /**
     * PKCS#9 OID for the signingTime authenticated attribute (1.2.840.113549.1.9.5). This attribute contains the time when the signature
     * was created.
     */
    static final String     OID_SIGNING_TIME       = "1.2.840.113549.1.9.5";
    /**
     * PKCS#9 OID for the counterSignature unauthenticated attribute (1.2.840.113549.1.9.6). This attribute contains a timestamp from a
     * trusted timestamp authority (TSA).
     */
    static final String     OID_COUNTERSIGN        = "1.2.840.113549.1.9.6";

    /**
     * Extracts the signing timestamp from an Authenticode-signed file.
     * <p>
     * This method attempts to find the signing time in two locations:
     * <ol>
     * <li>Direct signingTime attribute in authenticated attributes (legacy Authenticode)</li>
     * <li>Countersignature attribute containing an RFC 3161 timestamp</li>
     * </ol>
     * </p>
     *
     * @param hMsg
     *            Handle to the cryptographic message
     * @return ISO-8601 formatted timestamp string (UTC) or null if not found
     */
    static String getSigningTimeFromMessage(HCRYPTMSG hMsg) {
        IntByReference pcbData = new IntByReference();
        // 1. Query size of signer info structure
        if (!WindowsSignatureCrypt32Api.INSTANCE.CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, Pointer.NULL, pcbData)) {
            return null;
        }
        int cbData = pcbData.getValue();
        if (cbData <= 0) {
            return null;
        }
        // 2. Retrieve the actual signer info structure
        Memory signerInfoMem = new Memory(cbData);
        if (!WindowsSignatureCrypt32Api.INSTANCE.CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, signerInfoMem, pcbData)) {
            return null;
        }
        CMSG_SIGNER_INFO signerInfo = new CMSG_SIGNER_INFO(signerInfoMem);
        signerInfo.read();
        // 3. Try to find signingTime in authenticated attributes (legacy Authenticode method)
        String time = findSigningTimeInAttrs(signerInfo.AuthAttrs);
        if (time != null) {
            return time;
        }
        // 4. Try to find signingTime in countersignature (RFC 3161 trusted timestamp)
        time = findSigningTimeInCountersign(signerInfo.UnauthAttrs);
        if (time != null) {
            return time;
        }
        // 5. No timestamp found in either location
        return null;
    }

    /**
     * Searches for a signingTime attribute (OID 1.2.840.113549.1.9.5) in the authenticated attributes.
     * <p>
     * The signingTime attribute is a PKCS#9 attribute that contains the time when the signature was created. It may be present in legacy
     * Authenticode signatures or within countersignature attributes.
     * </p>
     *
     * @param attrs
     *            The authenticated attributes structure to search
     * @return ISO-8601 formatted timestamp string (UTC) or null if not found
     */
    static String findSigningTimeInAttrs(CRYPT_ATTRIBUTES attrs) {
        if (attrs == null || attrs.cAttr <= 0 || attrs.rgAttr == null) {
            return null;
        }
        CRYPT_ATTRIBUTE proto = new CRYPT_ATTRIBUTE();
        int attrSize = proto.size();
        for (int i = 0; i < attrs.cAttr; i++) {
            Pointer pAttr = attrs.rgAttr.share((long) i * attrSize);
            CRYPT_ATTRIBUTE attr = new CRYPT_ATTRIBUTE(pAttr);
            attr.read();
            if (OID_SIGNING_TIME.equals(attr.pszObjId) && attr.cValue > 0 && attr.rgValue != null) {
                CRYPT_ATTR_BLOB blobProto = new CRYPT_ATTR_BLOB();
                int blobSize = blobProto.size();
                for (int j = 0; j < attr.cValue; j++) {
                    Pointer pBlob = attr.rgValue.share((long) j * blobSize);
                    CRYPT_ATTR_BLOB blob = new CRYPT_ATTR_BLOB(pBlob);
                    blob.read();
                    if (blob.cbData > 0 && blob.pbData != null) {
                        byte[] timeData = blob.pbData.getByteArray(0, blob.cbData);
                        String parsed = parseAsn1TimeToIso(timeData);
                        if (parsed != null) {
                            return parsed;
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * Windows Crypto API structure type identifier for PKCS7_SIGNER_INFO. Used with CryptDecodeObject to decode countersignature blobs.
     * Corresponds to: #define PKCS7_SIGNER_INFO ((LPCSTR) 500)
     */
    private static final Pointer PKCS7_SIGNER_INFO = new Pointer(500);

    /**
     * Searches for a counterSignature attribute (OID 1.2.840.113549.1.9.6) and extracts its embedded signing time.
     * <p>
     * Countersignatures are used by RFC 3161 timestamp authorities to provide trusted timestamps. This method decodes the ASN.1-encoded
     * countersignature blob and recursively searches for the signingTime attribute within it.
     * </p>
     *
     * @param attrs
     *            The unauthenticated attributes structure to search
     * @return ISO-8601 formatted timestamp string (UTC) or null if not found
     */
    static String findSigningTimeInCountersign(CRYPT_ATTRIBUTES attrs) {
        if (attrs == null || attrs.cAttr <= 0 || attrs.rgAttr == null) {
            return null;
        }
        CRYPT_ATTRIBUTE proto = new CRYPT_ATTRIBUTE();
        int attrSize = proto.size();
        for (int i = 0; i < attrs.cAttr; i++) {
            Pointer pAttr = attrs.rgAttr.share((long) i * attrSize);
            CRYPT_ATTRIBUTE attr = new CRYPT_ATTRIBUTE(pAttr);
            attr.read();
            if (WindowsSignature.OID_COUNTERSIGN.equals(attr.pszObjId) && attr.cValue > 0 && attr.rgValue != null) {
                CRYPT_ATTR_BLOB blobProto = new CRYPT_ATTR_BLOB();
                int blobSize = blobProto.size();
                for (int j = 0; j < attr.cValue; j++) {
                    Pointer pBlob = attr.rgValue.share((long) j * blobSize);
                    CRYPT_ATTR_BLOB blob = new CRYPT_ATTR_BLOB(pBlob);
                    blob.read();
                    if (blob.cbData > 0 && blob.pbData != null) {
                        try {
                            // Decode the ASN.1-encoded countersignature blob to CMSG_SIGNER_INFO structure
                            IntByReference pcbDecoded = new IntByReference();
                            boolean ok = WindowsSignatureCrypt32Api.INSTANCE.CryptDecodeObject(WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, PKCS7_SIGNER_INFO, blob.pbData, blob.cbData, 0, Pointer.NULL, pcbDecoded);
                            if (!ok || pcbDecoded.getValue() <= 0) {
                                continue;
                            }
                            Memory decodedMem = new Memory(pcbDecoded.getValue());
                            ok = WindowsSignatureCrypt32Api.INSTANCE.CryptDecodeObject(WinCrypt.X509_ASN_ENCODING | WinCrypt.PKCS_7_ASN_ENCODING, PKCS7_SIGNER_INFO, blob.pbData, blob.cbData, 0, decodedMem, pcbDecoded);
                            if (!ok) {
                                continue;
                            }
                            // Read the nested signer info and recursively search for signingTime
                            CMSG_SIGNER_INFO nested = new CMSG_SIGNER_INFO(decodedMem);
                            nested.read();
                            String parsed = findSigningTimeInAttrs(nested.AuthAttrs);
                            if (parsed != null) {
                                return parsed;
                            }
                        } catch (Throwable e) {
                            // Silently skip invalid or malformed countersignature blobs
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * Parses an ASN.1 DER-encoded time value and converts it to ISO-8601 format (UTC).
     * <p>
     * Supports both:
     * <ul>
     * <li>UTCTime (tag 0x17): YYMMDDHHMMSSZ format (e.g., 250122025712Z)</li>
     * <li>GeneralizedTime (tag 0x18): YYYYMMDDHHMMSSZ format (e.g., 20250122025712Z)</li>
     * </ul>
     * </p>
     *
     * @param der
     *            The DER-encoded ASN.1 time value
     * @return ISO-8601 formatted string (YYYY-MM-DDTHH:MM:SSZ) or null if parsing fails
     */
    private static String parseAsn1TimeToIso(byte[] der) {
        if (der == null || der.length < 2) {
            return null;
        }
        try {
            int idx = 0;
            // Read ASN.1 tag: 0x17 = UTCTime, 0x18 = GeneralizedTime
            int tag = der[idx++] & 0xFF;
            // Read length (supports both short and long form encoding)
            int len = der[idx++] & 0xFF;
            if ((len & 0x80) != 0) {
                // Long form: next (len & 0x7F) bytes contain the actual length
                int bytes = len & 0x7F;
                len = 0;
                for (int i = 0; i < bytes; i++) {
                    len = (len << 8) | (der[idx++] & 0xFF);
                }
            }
            // Bounds check
            if (idx + len > der.length) {
                len = der.length - idx;
            }
            // Extract the ASCII time string
            String s = new String(der, idx, len, "ASCII").trim();
            SimpleDateFormat in, out = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
            out.setTimeZone(TimeZone.getTimeZone("UTC"));
            if (s.endsWith("Z")) {
                if (tag == 0x17) {
                    // UTCTime: 2-digit year (YYMMDDHHMMSSZ)
                    in = new SimpleDateFormat("yyMMddHHmmss'Z'");
                } else {
                    // GeneralizedTime: 4-digit year (YYYYMMDDHHMMSSZ)
                    in = new SimpleDateFormat("yyyyMMddHHmmss'Z'");
                }
                in.setTimeZone(TimeZone.getTimeZone("UTC"));
                return out.format(in.parse(s));
            }
        } catch (Exception ignored) {
            // Return null on any parsing error
        }
        return null;
    }

    /**
     * Legacy ASN.1 time parser (simplified version).
     * <p>
     * <b>Note:</b> This method is retained for compatibility but is not currently used. Use {@link #parseAsn1TimeToIso(byte[])} instead for
     * proper DER decoding.
     * </p>
     *
     * @param bytes
     *            ASCII-encoded time string bytes
     * @return Formatted time string or null if parsing fails
     */
    @SuppressWarnings("unused")
    private static String parseAsn1Time(byte[] bytes) {
        try {
            String s = new String(bytes, "ASCII").trim();
            SimpleDateFormat fmt;
            if (s.endsWith("Z")) {
                if (s.length() == 13) {
                    fmt = new SimpleDateFormat("yyMMddHHmmss'Z'");
                } else {
                    fmt = new SimpleDateFormat("yyyyMMddHHmmss'Z'");
                }
                fmt.setTimeZone(TimeZone.getTimeZone("UTC"));
                return fmt.format(fmt.parse(s));
            }
        } catch (Exception e) {
        }
        return null;
    }

    /**
     * Verifies whether the given file has a valid and trusted Authenticode signature.
     *
     * @param filePath
     *            the full path to the file
     * @return true if the file has a valid digital signature; false otherwise
     */
    public static boolean verifySignature(File filePath) {
        if (filePath == null) {
            System.err.println("filePath is null");
            return false;
        }
        File file = new File(filePath.getAbsolutePath());
        if (!file.isFile()) {
            System.err.println("ERROR: File not found: " + filePath);
            return false;
        }
        WinTrust.WINTRUST_FILE_INFO fileInfo = new WinTrust.WINTRUST_FILE_INFO(file.getAbsolutePath());
        WinTrust.WINTRUST_DATA trustData = new WinTrust.WINTRUST_DATA(fileInfo);
        Guid.GUID action = WinTrust.WINTRUST_ACTION_GENERIC_VERIFY_V2;
        int result = WinTrust.INSTANCE.WinVerifyTrust(null, action, trustData);
        if (result == WinError.ERROR_SUCCESS) {
            return true;
        } else if (result == WinError.TRUST_E_NOSIGNATURE) {
            System.err.println("WARNING: No digital signature found.");
        } else if (result == WinError.TRUST_E_BAD_DIGEST) {
            System.err.println("WARNING: Signature present but invalid.");
        } else if (result == WinError.CERT_E_REVOKED) {
            System.err.println("WARNING: Certificate revoked.");
        } else {
            System.err.println("WARNING: WinVerifyTrust failed, code: 0x" + Integer.toHexString(result));
        }
        return false;
    }
}
