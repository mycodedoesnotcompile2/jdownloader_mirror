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
package org.appwork.utils.awfc.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.appwork.utils.Hash;
import org.appwork.utils.awfc.AWFCEntry;
import org.appwork.utils.awfc.AWFCInputStream;
import org.appwork.utils.awfc.AWFCOutputStream;

/**
 * @author Daniel Wilhelm
 * 
 */
public class Tester {

    /**
     * @param args
     * @throws IOException
     * @throws NoSuchAlgorithmException
     * @throws InterruptedException
     */
    public static void main(final String[] args) throws IOException, NoSuchAlgorithmException, InterruptedException {

        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final MessageDigest md = MessageDigest.getInstance("SHA-256");
        byte[] buffer;
        final byte[] hash = md.digest(buffer = new byte[1024]);
        final long crc32 = Hash.getCRC32(buffer);
        long a = System.currentTimeMillis();
        final AWFCOutputStream cos = new AWFCOutputStream(bos, null);
        AWFCEntry entry = null;
        boolean file = false;
        boolean noPayLoad = false;
        for (int i = 0; i < 1024 * 500; i++) {
            if (file == false) {
                entry = new AWFCEntry("test" + i, buffer.length, hash);
                cos.putNextEntry(entry, noPayLoad);
                if (noPayLoad == false) {
                    cos.write(buffer);
                }
                noPayLoad = !noPayLoad;
                file = true;
            } else {
                entry = new AWFCEntry("test" + i + "/", 0, hash);
                cos.putNextEntry(entry);
                file = false;
            }
        }
        cos.close();
        System.out.println("Size: " + bos.size() + " " + (System.currentTimeMillis() - a));
        final byte[] b = bos.toByteArray();
        // b[31312] = 9;
        // b[75312] = 9;
        final ByteArrayInputStream bis = new ByteArrayInputStream(b);
        a = System.currentTimeMillis();
        System.out.println("next");
        final AWFCInputStream cis = new AWFCInputStream(bis);
        entry = null;
        long i = 0;
        final byte[] buffer2 = new byte[2048];
        while ((entry = cis.getNextEntry()) != null) {
            i++;
            while (cis.read(buffer2) != -1) {
                ;
            }
        }
        System.out.println(System.currentTimeMillis() - a + " = " + i);
        if (true) { return; }
        a = System.currentTimeMillis();
        final ZipOutputStream zos = new ZipOutputStream(bos);
        ZipEntry zentry = null;
        file = false;
        for (i = 0; i < 1024 * 1024; i++) {
            if (file == false) {
                zentry = new ZipEntry("test" + i);
                zentry.setMethod(ZipEntry.STORED);
                zentry.setCompressedSize(buffer.length);
                zentry.setSize(buffer.length);
                zentry.setCrc(crc32);
                zos.putNextEntry(zentry);
                zos.write(buffer);
                file = true;
            } else {
                zentry = new ZipEntry("test" + i + "/");

                zos.putNextEntry(zentry);
                file = false;
            }
        }
        zos.close();
        System.out.println("Size: " + bos.size() + " " + (System.currentTimeMillis() - a));

    }
}
