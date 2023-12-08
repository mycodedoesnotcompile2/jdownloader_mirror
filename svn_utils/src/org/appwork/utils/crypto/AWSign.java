/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.crypto;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.appwork.utils.encoding.Base64;

public class AWSign {
    private static SecureRandom sr;
    static {
        try {
            AWSign.sr = new SecureRandom();
        } catch (final Throwable e) {
            e.printStackTrace();
        }
    }

    public static KeyPair createKeyPair() throws NoSuchAlgorithmException {
        final KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(2048);
        final KeyPair keyPair = keyPairGenerator.genKeyPair();
        System.out.println("PUBLIC  " + Base64.encodeToString(keyPair.getPublic().getEncoded(), false));
        System.out.println("PRIVATE " + Base64.encodeToString(keyPair.getPrivate().getEncoded(), false));
        return keyPair;
    }

    /**
     * @param bytes
     * @param pk
     * @param salt
     * @return
     * @throws SignatureViolationException
     */
    public static byte[] createSign(final byte[] bytes, final PrivateKey pk, final boolean salt) throws SignatureViolationException {
        return createSign(bytes, pk, salt, null);
    }

    public static byte[] createSign(final byte[] bytes, final PrivateKey pk, final boolean salt, final byte[] addInfo) throws SignatureViolationException {
        try {
            final Signature sig = Signature.getInstance("Sha256WithRSA");
            sig.initSign(pk);
            final byte[] saltBytes = AWSign.get16ByteSalt(salt);
            if (saltBytes != null) {
                sig.update(saltBytes);
            }
            if (addInfo != null) {
                sig.update(addInfo, 0, addInfo.length);
            }
            sig.update(bytes, 0, bytes.length);
            final byte[] ret = sig.sign();
            if (!salt) {
                return ret;
            }
            final byte[] merged = new byte[ret.length + saltBytes.length];
            System.arraycopy(saltBytes, 0, merged, 0, saltBytes.length);
            System.arraycopy(ret, 0, merged, saltBytes.length, ret.length);
            return merged;
        } catch (final Throwable e) {
            throw new SignatureViolationException(e);
        }
    }

    public static byte[] createSign(final File f, final PrivateKey publicKey, final boolean salt, final byte[] addInfo) throws SignatureViolationException {
        try {
            final Signature sig = Signature.getInstance("Sha256WithRSA");
            sig.initSign(publicKey);
            InputStream input = null;
            try {
                final byte[] saltBytes = AWSign.get16ByteSalt(salt);
                if (saltBytes != null) {
                    sig.update(saltBytes);
                }
                if (addInfo != null) {
                    sig.update(addInfo, 0, addInfo.length);
                }
                final byte[] buffer = new byte[1024];
                int len;
                input = new FileInputStream(f);
                while ((len = input.read(buffer)) != -1) {
                    if (len > 0) {
                        sig.update(buffer, 0, len);
                    }
                }
                final byte[] ret = sig.sign();
                if (!salt) {
                    return ret;
                }
                final byte[] merged = new byte[ret.length + saltBytes.length];
                System.arraycopy(saltBytes, 0, merged, 0, saltBytes.length);
                System.arraycopy(ret, 0, merged, saltBytes.length, ret.length);
                return merged;
            } finally {
                try {
                    input.close();
                } catch (final Exception e) {
                }
            }
        } catch (final Throwable e) {
            throw new SignatureViolationException(e);
        }
    }

    public static void decryptRSA_AES(final File srcFile, final File dstFile, final PublicKey pk) throws IOException, NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, IllegalBlockSizeException, BadPaddingException, InvalidAlgorithmParameterException {
        FileInputStream fis = null;
        FileOutputStream fos = null;
        boolean deleteDst = true;
        try {
            fis = new FileInputStream(srcFile);
            fos = new FileOutputStream(dstFile);
            final byte[] wrappedKey = new byte[256];
            final byte[] wrappedIV = new byte[256];
            final byte[] readDigest = new byte[32];
            int done = 0;
            int read = 0;
            while (done < 256 && (read = fis.read()) != -1) {
                wrappedKey[done++] = (byte) read;
            }
            done = 0;
            while (done < 256 && (read = fis.read()) != -1) {
                wrappedIV[done++] = (byte) read;
            }
            done = 0;
            while (done < 32 && (read = fis.read()) != -1) {
                readDigest[done++] = (byte) read;
            }
            Cipher cipher = Cipher.getInstance("RSA");
            cipher.init(Cipher.DECRYPT_MODE, pk);
            final SecretKey key = new SecretKeySpec(cipher.doFinal(wrappedKey), "AES");
            cipher.init(Cipher.DECRYPT_MODE, pk);
            final byte[] iv = cipher.doFinal(wrappedIV);
            cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv));
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            final CipherInputStream cis = new CipherInputStream(fis, cipher);
            final byte[] buffer = new byte[32767];
            final int maxReadSize = buffer.length;
            while ((read = cis.read(buffer, 0, maxReadSize)) != -1) {
                if (read > 0) {
                    fos.write(buffer, 0, read);
                    md.update(buffer, 0, read);
                }
            }
            final byte[] generatedDigest = md.digest();
            if (Arrays.equals(generatedDigest, readDigest) == false) {
                throw new IOException("Hash failed!");
            }
            fos.close();
            deleteDst = false;
        } finally {
            try {
                fis.close();
            } catch (final Throwable e) {
            }
            try {
                fos.close();
            } catch (final Throwable e) {
            }
            if (deleteDst) {
                dstFile.delete();
            }
        }
    }

    public static void encryptRSA_AES(final File srcFile, final File dstFile, final PrivateKey pk) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, IOException, IllegalBlockSizeException, BadPaddingException, InvalidAlgorithmParameterException {
        FileInputStream fis = null;
        FileOutputStream fos = null;
        boolean deleteDst = true;
        try {
            fis = new FileInputStream(srcFile);
            fos = new FileOutputStream(dstFile);
            final KeyGenerator keygen = KeyGenerator.getInstance("AES");
            if (AWSign.sr != null) {
                keygen.init(AWSign.sr);
            }
            byte[] iv = new byte[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            if (AWSign.sr != null) {
                iv = AWSign.sr.generateSeed(16);
            }
            keygen.init(128);
            final SecretKey key = keygen.generateKey();
            Cipher cipher = Cipher.getInstance("RSA");
            cipher.init(Cipher.ENCRYPT_MODE, pk);
            fos.write(cipher.doFinal(key.getEncoded()));
            cipher.init(Cipher.ENCRYPT_MODE, pk);
            fos.write(cipher.doFinal(iv));
            fos.write(new byte[32]);
            cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            cipher.init(Cipher.ENCRYPT_MODE, key, new IvParameterSpec(iv));
            final CipherOutputStream cos = new CipherOutputStream(new FilterOutputStream(fos) {
                @Override
                public void close() throws IOException {
                }

                @Override
                public void flush() throws IOException {
                    this.out.flush();
                }

                @Override
                public void write(final byte[] b) throws IOException {
                    this.out.write(b);
                }

                @Override
                public void write(final byte[] b, final int off, final int len) throws IOException {
                    this.out.write(b, off, len);
                }

                @Override
                public void write(final int b) throws IOException {
                    this.out.write(b);
                }
            }, cipher);
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            int read = 0;
            final byte[] buffer = new byte[32767];
            while ((read = fis.read(buffer)) != -1) {
                if (read > 0) {
                    cos.write(buffer, 0, read);
                    md.update(buffer, 0, read);
                }
            }
            cos.close();
            // only works because FileOutputStream is NOT in append mode
            fos.getChannel().position(2 * 256);
            fos.write(md.digest());
            deleteDst = false;
        } finally {
            try {
                fis.close();
            } catch (final Throwable e) {
            }
            try {
                fos.close();
            } catch (final Throwable e) {
            }
            if (deleteDst) {
                dstFile.delete();
            }
        }
    }

    /**
     * @param privateKey
     * @return
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeySpecException
     */
    public static PrivateKey getPrivateKey(final String privateKeyBase64) throws InvalidKeySpecException, NoSuchAlgorithmException {
        if (privateKeyBase64.contains("BEGIN")) {
            String privateKeyPem = privateKeyBase64;
            privateKeyPem = privateKeyPem.replaceAll("(?i)^\\s*[\\-]*BEGIN\\s*(RSA\\s*)?PRIVATE\\s*KEY[\\-]*\\s*", "");
            privateKeyPem = privateKeyPem.replaceAll("(?i)\\s*[\\-]*END\\s*(RSA\\s*)?PRIVATE\\s*KEY[\\-]*\\s*$", "");
            privateKeyPem = privateKeyPem.replaceAll("\\s+", "");
            return getPrivateKey(Base64.decode(privateKeyPem));
        } else {
            return getPrivateKey(Base64.decode(privateKeyBase64));
        }
    }

    public static PrivateKey getPrivateKey(final byte[] bytes) throws InvalidKeySpecException, NoSuchAlgorithmException {
        return KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(bytes));
    }

    public static PublicKey getPublicKey(final byte[] bytes) throws SignatureViolationException {
        try {
            return KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(bytes));
        } catch (final InvalidKeySpecException e) {
            throw new SignatureViolationException(e);
        } catch (final NoSuchAlgorithmException e) {
            throw new SignatureViolationException(e);
        }
    }

    public static PublicKey getPublicKey(String publicKeyBase64) throws SignatureViolationException {
        if (publicKeyBase64.contains("BEGIN")) {
            String publiceKeyPem = publicKeyBase64;
            publiceKeyPem = publiceKeyPem.replaceAll("(?i)^\\s*[\\-]*BEGIN\\s*(RSA\\s*)?PUBLIC\\s*KEY[\\-]*\\s*", "");
            publiceKeyPem = publiceKeyPem.replaceAll("(?i)\\s*[\\-]*END\\s*(RSA\\s*)?PUBLIC\\s*KEY[\\-]*\\s*$", "");
            publiceKeyPem = publiceKeyPem.replaceAll("\\s+", "");
            return getPublicKey(Base64.decode(publiceKeyPem));
        } else {
            return getPublicKey(Base64.decode(publicKeyBase64));
        }
    }

    @Deprecated
    public static byte[] getSalt(final boolean salt) throws NoSuchAlgorithmException {
        return get16ByteSalt(salt);
    }

    public static byte[] getByteSalt(final int length) throws NoSuchAlgorithmException {
        if (length <= 0) {
            return null;
        } else {
            final byte[] saltBytes = new byte[length];
            if (AWSign.sr != null) {
                AWSign.sr.nextBytes(saltBytes);
            }
            return saltBytes;
        }
    }

    /**
     * @param salt
     * @return
     * @throws NoSuchAlgorithmException
     */
    public static byte[] get16ByteSalt(final boolean salt) throws NoSuchAlgorithmException {
        if (!salt) {
            return null;
        } else {
            return getByteSalt(16);
        }
    }

    public static SecureRandom getSecureRandom() {
        return AWSign.sr;
    }

    public static byte[] HMACSHA256(final byte[] key, final byte[] content) throws NoSuchAlgorithmException, InvalidKeyException {
        final Mac sha256_HMAC = Mac.getInstance("HmacSHA256");
        final SecretKeySpec secret_key = new SecretKeySpec(key, "HmacSHA256");
        sha256_HMAC.init(secret_key);
        return sha256_HMAC.doFinal(content);
    }

    public static void main(final String[] args) throws NoSuchAlgorithmException, InvalidKeyException, NoSuchPaddingException, IOException, IllegalBlockSizeException, BadPaddingException, InvalidAlgorithmParameterException, InvalidKeySpecException {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            AWSign.createKeyPair();
        }
    }

    /**
     * @param salted
     *            TODO
     * @param decode
     * @param decode2
     * @throws SignatureViolationException
     */
    public static void verify(final byte[] dataToVerify, final PublicKey pub, byte[] signature, final boolean salted) throws SignatureViolationException {
        try {
            final Signature sig = Signature.getInstance("Sha256WithRSA");
            sig.initVerify(pub);
            if (salted && signature != null && signature.length > 16) {
                final byte[] salt = new byte[16];
                System.arraycopy(signature, 0, salt, 0, salt.length);
                final byte[] actualSignature = new byte[signature.length - salt.length];
                System.arraycopy(signature, 16, actualSignature, 0, actualSignature.length);
                signature = actualSignature;
                sig.update(salt);
            }
            sig.update(dataToVerify);
            if (!sig.verify(signature)) {
                throw new SignatureViolationException("Signatur Check Failed");
            }
        } catch (final SignatureViolationException e) {
            throw e;
        } catch (final Throwable e) {
            throw new SignatureViolationException(e);
        }
    }

    /**
     * @param f
     * @param pub
     * @param additionalBytes
     *            TODO
     * @param bs
     * @throws SignatureViolationException
     */
    public static void verify(final File f, final PublicKey pub, final byte[] signature, final boolean salted, final byte[] additionalBytes) throws SignatureViolationException {
        try {
            AWSign.verify(f.toURI().toURL().openStream(), pub, signature, salted, additionalBytes);
        } catch (final SignatureViolationException e) {
            throw e;
        } catch (final Throwable e) {
            throw new SignatureViolationException(e);
        }
    }

    /**
     * @param openStream
     * @param pub
     * @param signature
     * @param salted
     * @param additionalBytes
     */
    public static void verify(final InputStream input, final PublicKey pub, byte[] signature, final boolean salted, final byte[] additionalBytes) throws SignatureViolationException {
        try {
            final Signature sig = Signature.getInstance("Sha256WithRSA");
            sig.initVerify(pub);
            if (salted) {
                final byte[] salt = new byte[signature.length - 256];
                System.arraycopy(signature, 0, salt, 0, salt.length);
                final byte[] actualSignature = new byte[256];
                System.arraycopy(signature, signature.length - 256, actualSignature, 0, actualSignature.length);
                signature = actualSignature;
                sig.update(salt);
            }
            if (additionalBytes != null) {
                sig.update(additionalBytes);
            }
            final byte[] buffer = new byte[32767];
            int len;
            while ((len = input.read(buffer)) != -1) {
                if (len > 0) {
                    sig.update(buffer, 0, len);
                }
            }
            if (!sig.verify(signature)) {
                throw new SignatureViolationException("Signatur Check Failed");
            }
        } catch (final SignatureViolationException e) {
            throw e;
        } catch (final Throwable e) {
            throw new SignatureViolationException(e);
        } finally {
            try {
                input.close();
            } catch (final Exception e) {
            }
        }
    }

    /**
     * @param publicKey
     * @param privateKey
     * @throws SignatureViolationException
     */
    public static void checkIfPublicMatchesPrivateKey(PublicKey publicKey, PrivateKey privateKey) throws SignatureViolationException {
        byte[] test;
        try {
            test = (System.currentTimeMillis() + "").getBytes("UTF-8");
            AWSign.verify(test, publicKey, AWSign.createSign(test, privateKey, true), true);
        } catch (UnsupportedEncodingException e) {
            throw new WTFException(e);
        }
    }
}
