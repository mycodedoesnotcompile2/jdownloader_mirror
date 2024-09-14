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
package org.appwork.utils.awfc.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.Random;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Hash;
import org.appwork.utils.awfc.AWFCEntry;
import org.appwork.utils.awfc.AWFCInputStream;
import org.appwork.utils.awfc.AWFCOutputStream;
import org.appwork.utils.formatter.HexFormatter;

/**
 * @author daniel
 * @date Oct 13, 2022
 *
 */
public class AWFCTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final byte[] emptyHash = HexFormatter.hexToByteArray(Hash.getBytesHash(new byte[0], "SHA-256"));
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final MessageDigest md = MessageDigest.getInstance("SHA-256");
        for (int version = 1; version <= 2; version++) {
            for (int test = 1; test <= 3; test++) {
                bos.reset();
                boolean emptyHashFlag = true;
                final AWFCOutputStream os;
                switch (test) {
                case 1:
                    os = new AWFCOutputStream(bos, null);
                    break;
                case 2:
                    os = new AWFCOutputStream(bos, null);
                    emptyHashFlag = false;
                    break;
                case 3:
                    os = new AWFCOutputStream(bos, md);
                    break;
                default:
                    throw new Exception();
                }
                os.setVersion(version);
                System.out.println("Test:" + test);
                os.putNextEntry(new AWFCEntry("folder/", 0, null));
                os.putNextEntry(new AWFCEntry("empty.txt", 0, emptyHashFlag ? emptyHash : null));
                byte[] rnd = new byte[1024];
                new Random().nextBytes(rnd);
                byte[] sha256 = HexFormatter.hexToByteArray(Hash.getBytesHash(rnd, "SHA-256"));
                os.putNextEntry(new AWFCEntry("normal.txt", rnd.length, sha256));
                os.write(rnd);
                os.close();
                AWFCInputStream is = new AWFCInputStream(new ByteArrayInputStream(bos.toByteArray()));
                is.readAWFCHeader();
                System.out.println("Version:" + is.getVersion());
                while (true) {
                    AWFCEntry e = is.getNextEntry();
                    if (e == null) {
                        break;
                    } else if (!e.isFile()) {
                        System.out.println(e);
                    } else {
                        System.out.println(e + (e.getHash() != null ? HexFormatter.byteArrayToHex(e.getHash()) : ""));
                        System.out.println("Hash:" + Hash.getHashBytes(is, "SHA-256", -1, false, true));
                        if (e.getSize() > 0 && e.getHash() != null && !Arrays.equals(sha256, e.getHash())) {
                            throw new Exception();
                        }
                    }
                }
            }
        }
    }
}
