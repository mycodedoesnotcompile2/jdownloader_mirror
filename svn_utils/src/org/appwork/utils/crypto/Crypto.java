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

import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * Crypto class provides a few easy to use functions to encrypt or decrypt data. AES CBC Mode is used.
 *
 * @author thomas
 *
 */
public class Crypto {

    /**
     * Decrypts data which has been encrypted with {@link Crypto#encrypt(String, byte[])}
     *
     * @param b
     *            data to decrypt
     * @param key
     *            to use (128 Bit/16 Byte)
     * @return
     */
    public static String decrypt(final byte[] b, final byte[] key) {
        return Crypto.decrypt(b, key, key);
    }

    /**
     * Decrypt data which has been encrypted width {@link Crypto#encrypt(String, byte[], byte[])}
     *
     * @param b
     *            data to decrypt
     * @param key
     *            to use (128Bit (16 Byte))
     * @param iv
     *            to use (128Bit (16 Byte))
     * @return
     */
    public static String decrypt(final byte[] b, final byte[] key, final byte[] iv) {
        final byte[] ret = decryptAsByteArray(b, key, iv);
        if (ret != null) {
            try {
                return new String(ret, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return null;
    }

    protected static byte[] decryptAsByteArray(final byte[] b, final byte[] key, final byte[] iv) {
        try {
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
            return cipher.doFinal(b);
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            try {
                final Cipher cipher = Cipher.getInstance("AES/CBC/nopadding");
                cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
                cipher.doFinal(b);
            } catch (final Exception e1) {
                org.appwork.loggingv3.LogV3.log(e1);
            }
        }
        return null;
    }

    /**
     * Encrypts a string
     *
     * @param string
     *            String to encrypt
     * @param key
     *            to use (128Bit (16 Byte))
     * @param iv
     *            to use (128Bit (16 Byte))
     * @return
     */
    public static byte[] encrypt(final String string, final byte[] key, final byte[] iv) {
        try {
            return encryptByteArray(string.getBytes("UTF-8"), key, iv);
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        return null;
    }

    protected static byte[] encryptByteArray(final byte[] data, final byte[] key, final byte[] iv) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException {
        final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        final IvParameterSpec ivSpec = new IvParameterSpec(iv);
        final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
        cipher.init(Cipher.ENCRYPT_MODE, skeySpec, ivSpec);
        return cipher.doFinal(data);
    }

}
