/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 * ====================================================================================================================================================
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.RSAPublicKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.utils.encoding.Base64;

/**
 * Symmetric (AES) encryption for the helper–client channel; key exchange is done via RSA. The payload is encrypted with a
 * random AES key; that key (and IV) are encrypted with the client's RSA public key. The helper encrypts responses with
 * the public key (from env {@link #ENV_PUBLIC_KEY_BASE64} or {@code -publicKeyBase64}); the client decrypts with the
 * private key. So only symmetric (AES) cipher is used for the actual data; RSA is used only to protect the AES key.
 * <p>
 * Compatible with Java 1.6 (JCE RSA and AES).
 */
public final class HelperChannelCrypto {
    /** Environment variable name for the client's public key (Base64-encoded X.509). Prefer this over command-line. */
    public static final String  ENV_PUBLIC_KEY_BASE64 = "AWTEST_HELPER_PUBLIC_KEY";
    /** RSA algorithm and key size. */
    private static final String RSA_ALG               = "RSA";
    private static final int    RSA_KEY_SIZE          = 2048;
    private static final String RSA_TRANSFORM         = "RSA/ECB/PKCS1Padding";
    private static final String AES_TRANSFORM         = "AES/CBC/PKCS5Padding";
    private static final int    AES_KEY_LEN           = 16;
    private static final int    IV_LEN                = 16;
    /** First byte = 1 for hybrid format (4-byte RSA block length + RSA block + AES cipher). */
    private static final byte   FORMAT_VERSION        = 1;

    /**
     * Generates a new RSA key pair for the client (main process). The public key is passed to the helper; the private key is used to
     * decrypt responses.
     */
    public static KeyPair generateKeyPair() throws Exception {
        KeyPairGenerator gen = KeyPairGenerator.getInstance(RSA_ALG);
        gen.initialize(RSA_KEY_SIZE, new SecureRandom());
        return gen.generateKeyPair();
    }

    /**
     * Encodes the public key to a Base64 string (X.509). Can be passed via env or {@code -publicKeyBase64}.
     */
    public static String encodePublicKeyToBase64(PublicKey key) {
        if (key == null) {
            return null;
        }
        return Base64.encodeToString(key.getEncoded(), false);
    }

    /**
     * Decodes a Base64 string (PKCS#8) to a private key. Used when tests receive connection params from PostBuildRunner via env.
     */
    public static PrivateKey decodePrivateKeyFromBase64(String base64) throws Exception {
        if (base64 == null || base64.length() == 0) {
            return null;
        }
        byte[] encoded = Base64.decode(base64.trim());
        if (encoded == null || encoded.length == 0) {
            return null;
        }
        PKCS8EncodedKeySpec spec = new PKCS8EncodedKeySpec(encoded);
        return java.security.KeyFactory.getInstance(RSA_ALG).generatePrivate(spec);
    }

    /**
     * Encodes the private key to a Base64 string (PKCS#8). Used to pass the key to child test processes via env.
     */
    public static String encodePrivateKeyToBase64(PrivateKey key) {
        if (key == null) {
            return null;
        }
        return Base64.encodeToString(key.getEncoded(), false);
    }

    /**
     * Builds a KeyPair from a private key (derives public key from RSA private key). Used when the process receives only
     * AWTEST_HELPER_PRIVATE_KEY_BASE64 from the parent.
     */
    public static KeyPair keyPairFromPrivateKey(PrivateKey privateKey) throws Exception {
        if (privateKey == null) {
            return null;
        }
        if (!(privateKey instanceof RSAPrivateCrtKey)) {
            throw new IllegalArgumentException("Only RSAPrivateCrtKey is supported for deriving public key");
        }
        RSAPrivateCrtKey crt = (RSAPrivateCrtKey) privateKey;
        RSAPublicKeySpec spec = new RSAPublicKeySpec(crt.getModulus(), crt.getPublicExponent());
        PublicKey publicKey = java.security.KeyFactory.getInstance(RSA_ALG).generatePublic(spec);
        return new KeyPair(publicKey, privateKey);
    }

    /**
     * Decodes a Base64 string (X.509) to a public key. Input from env or {@code -publicKeyBase64}.
     */
    public static PublicKey decodePublicKeyFromBase64(String base64) throws Exception {
        if (base64 == null || base64.length() == 0) {
            return null;
        }
        byte[] encoded = Base64.decode(base64.trim());
        if (encoded == null || encoded.length == 0) {
            return null;
        }
        X509EncodedKeySpec spec = new X509EncodedKeySpec(encoded);
        return java.security.KeyFactory.getInstance(RSA_ALG).generatePublic(spec);
    }

    /**
     * Encrypts the payload using symmetric (AES) encryption; the AES key and IV are encrypted with the client's RSA public key
     * (key exchange via RSA). Used by the helper to encrypt responses. Returns raw bytes; encode with Base64 for transport.
     */
    public static byte[] encryptWithPublicKey(PublicKey publicKey, byte[] payload) throws Exception {
        if (publicKey == null || payload == null) {
            throw new IllegalArgumentException("publicKey and payload must be non-null");
        }
        SecureRandom rnd = new SecureRandom();
        byte[] aesKey = new byte[AES_KEY_LEN];
        byte[] iv = new byte[IV_LEN];
        rnd.nextBytes(aesKey);
        rnd.nextBytes(iv);
        Cipher aesCipher = Cipher.getInstance(AES_TRANSFORM);
        aesCipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(aesKey, "AES"), new IvParameterSpec(iv));
        byte[] encryptedPayload = aesCipher.doFinal(payload);
        byte[] keyAndIv = new byte[AES_KEY_LEN + IV_LEN];
        System.arraycopy(aesKey, 0, keyAndIv, 0, AES_KEY_LEN);
        System.arraycopy(iv, 0, keyAndIv, AES_KEY_LEN, IV_LEN);
        Cipher rsaCipher = Cipher.getInstance(RSA_TRANSFORM);
        rsaCipher.init(Cipher.ENCRYPT_MODE, publicKey);
        byte[] encryptedKeyAndIv = rsaCipher.doFinal(keyAndIv);
        int rsaLen = encryptedKeyAndIv.length;
        byte[] out = new byte[1 + 4 + rsaLen + encryptedPayload.length];
        int pos = 0;
        out[pos++] = FORMAT_VERSION;
        out[pos++] = (byte) (rsaLen >> 24);
        out[pos++] = (byte) (rsaLen >> 16);
        out[pos++] = (byte) (rsaLen >> 8);
        out[pos++] = (byte) rsaLen;
        System.arraycopy(encryptedKeyAndIv, 0, out, pos, rsaLen);
        pos += rsaLen;
        System.arraycopy(encryptedPayload, 0, out, pos, encryptedPayload.length);
        return out;
    }

    /**
     * Decrypts data produced by {@link #encryptWithPublicKey}. Uses the private key only to recover the AES key; the payload
     * is decrypted with symmetric (AES). Used by the client.
     */
    public static byte[] decryptWithPrivateKey(PrivateKey privateKey, byte[] encrypted) throws Exception {
        if (privateKey == null || encrypted == null || encrypted.length < 1 + 4) {
            throw new IllegalArgumentException("invalid input");
        }
        int pos = 0;
        byte version = encrypted[pos++];
        if (version != FORMAT_VERSION) {
            throw new IllegalArgumentException("unsupported format version: " + version);
        }
        int rsaLen = ((encrypted[pos] & 0xff) << 24) | ((encrypted[pos + 1] & 0xff) << 16) | ((encrypted[pos + 2] & 0xff) << 8) | (encrypted[pos + 3] & 0xff);
        pos += 4;
        if (rsaLen <= 0 || pos + rsaLen > encrypted.length) {
            throw new IllegalArgumentException("invalid encrypted blob");
        }
        byte[] encryptedKeyAndIv = new byte[rsaLen];
        System.arraycopy(encrypted, pos, encryptedKeyAndIv, 0, rsaLen);
        pos += rsaLen;
        Cipher rsaCipher = Cipher.getInstance(RSA_TRANSFORM);
        rsaCipher.init(Cipher.DECRYPT_MODE, privateKey);
        byte[] keyAndIv = rsaCipher.doFinal(encryptedKeyAndIv);
        if (keyAndIv.length != AES_KEY_LEN + IV_LEN) {
            throw new IllegalArgumentException("invalid key+iv length");
        }
        byte[] aesKey = new byte[AES_KEY_LEN];
        byte[] iv = new byte[IV_LEN];
        System.arraycopy(keyAndIv, 0, aesKey, 0, AES_KEY_LEN);
        System.arraycopy(keyAndIv, AES_KEY_LEN, iv, 0, IV_LEN);
        byte[] encryptedPayload = new byte[encrypted.length - pos];
        System.arraycopy(encrypted, pos, encryptedPayload, 0, encryptedPayload.length);
        Cipher aesCipher = Cipher.getInstance(AES_TRANSFORM);
        aesCipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(aesKey, "AES"), new IvParameterSpec(iv));
        return aesCipher.doFinal(encryptedPayload);
    }

    private HelperChannelCrypto() {
    }
}
