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
package org.appwork.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Locale;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.exceptions.WTFException;

/**
 * @author thomas
 * @date 14.12.2021
 *
 */
public class ZipContentHasher {

    public static byte[] getContentSHA256(final File file, Customizer custom) throws IOException {
        if (!file.isFile()) {
            throw new WTFException("ContentSHA256 of directory!?:" + file);
        } else {
            final FileInputStream fis = new FileInputStream(file);
            try {
                return getContentSHA256("", fis, custom);
            } finally {
                fis.close();
            }
        }
    }

    public static interface Customizer {

        /**
         * @param path
         * @param zipStream
         * @param e
         * @param list
         * @return
         * @throws IOException
         */
        boolean handle(String path, ZipInputStream zipStream, ZipEntry entry, HashMap<String, byte[]> results) throws IOException;

    }

    public static byte[] getContentSHA256(final String path, final InputStream is, Customizer custom) throws IOException {
        final HashMap<String, byte[]> hashes = getHashes(path, is, custom, true);
        final ArrayList<String> list = new ArrayList<String>(hashes.keySet());
        Collections.sort(list, SORTER);
        try {
            final Charset utf = Charset.forName("UTF-8");
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            for (final String key : list) {
                md.update(key.getBytes(utf));
                final byte[] hash = hashes.get(key);
                if (hash == null) {
                    // folder
                } else {
                    md.update(hash);
                }
            }
            return md.digest();
        } catch (final NoSuchAlgorithmException e) {
            throw new IOException(e);
        }
    }

    public final static Comparator<? super String> SORTER = new Comparator<String>() {
        @Override
        public int compare(final String s1, final String s2) {
            final int n1 = s1.length();
            final int n2 = s2.length();
            final int min = Math.min(n1, n2);
            for (int i = 0; i < min; i++) {
                final char c1 = s1.charAt(i);
                final char c2 = s2.charAt(i);
                if (c1 != c2) {
                    return c1 - c2;
                }
            }
            return n1 - n2;
        }
    };

    /**
     * @param throwIOException
     *            TODO
     * @param f
     * @param customizer
     * @throws IOException
     */
    public static HashMap<String, byte[]> getHashes(String path, InputStream is, Customizer custom, boolean throwIOException) throws IOException {
        is = new FilterInputStream(is) {
            @Override
            public void close() throws IOException {
                // do not close the source stream
            }
        };
        final ZipInputStream zipStream = new ZipInputStream(is);
        is = null;
        try {
            final HashMap<String, byte[]> results = new HashMap<String, byte[]>();
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            final byte[] buffer = new byte[32767];
            if (path.length() > 0) {
                path += "!";
            }
            while (true) {
                try {
                    final ZipEntry e = zipStream.getNextEntry();
                    if (e == null) {
                        break;
                    } else if (custom != null && custom.handle(path, zipStream, e, results)) {
                        continue;
                    } else if (e.isDirectory()) {
                        results.put(path + e.getName(), null);
                    } else {
                        if (e.getName().toLowerCase(Locale.ROOT).endsWith(".jar") || e.getName().toLowerCase(Locale.ROOT).endsWith(".zip")) {
                            results.putAll(getHashes(path + e.getName(), zipStream, custom, throwIOException));
                        } else {
                            md.reset();
                            int len;
                            while ((len = zipStream.read(buffer)) != -1) {
                                if (len > 0) {
                                    md.update(buffer, 0, len);
                                }
                            }
                            results.put(path + e.getName(), md.digest());
                        }
                    }

                    zipStream.closeEntry();
                } catch (IOException e) {
                    if (throwIOException) {
                        throw e;
                    } else {
                        break;
                    }
                }
            }
            return results;
        } catch (final NoSuchAlgorithmException e) {
            throw new IOException(e);
        } finally {
            zipStream.close();
        }
    }
}
