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
package org.appwork.utils.zip.tests;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import org.appwork.utils.StringUtils;
import org.appwork.utils.zip.CompressedEntriesIndex;

/**
 * @author daniel
 * @date Aug 24, 2023
 *
 */
public class CompressedEntriesIndexTest extends OnClassPathZipJarTests {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        for (File testZip : getZipJarFilesFromClassPath()) {
            testZip(testZip);
        }
    }

    protected void testZip(File zipFile) throws Exception {
        final ArrayList<String> paths = new ArrayList<String>();
        // read ZipEntry from CEN
        final ZipFile zip = new ZipFile(zipFile);
        try {
            final Enumeration<? extends ZipEntry> entries = zip.entries();
            while (entries.hasMoreElements()) {
                final ZipEntry entry = entries.nextElement();
                paths.add(entry.getName());
            }
        } finally {
            zip.close();
        }
        // read ZipEntry from LOC
        final ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFile));
        try {
            int index = 0;
            while (true) {
                final ZipEntry entry = zis.getNextEntry();
                if (entry != null) {
                    // verify same order
                    assertEquals(entry.getName(), paths.get(index++));
                } else {
                    break;
                }
            }
            // verify same count
            assertEquals(index, paths.size());
        } finally {
            zis.close();
        }
        final CompressedEntriesIndex cei = new CompressedEntriesIndex();
        final byte[] data = cei.compress(paths);
        String list = StringUtils.join(paths, "\r\n");
        ByteArrayOutputStream baos;
        DeflaterOutputStream out = new DeflaterOutputStream(baos = new ByteArrayOutputStream(), new Deflater(9));
        out.write(list.getBytes("UTF-8"));
        out.close();
        InputStream is = new ByteArrayInputStream(data);
        if (false) {
            System.out.println("ZipFile:" + zipFile + "\t Entries:" + paths.size() + "\t IndexSize:" + data.length + "\t ZippedRAWSize: " + baos.size());
        } else {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DeflaterOutputStream dos = new DeflaterOutputStream(bos, new Deflater(9));
            dos.write(data);
            dos.close();
            System.out.println("ZipFile:" + zipFile + "\t Entries:" + paths.size() + "\t IndexSize:" + data.length + "\t CompressedIndexSize:" + bos.size() + "\t Ratio:" + ((100.0d * bos.size()) / data.length) + "\t ZippedRAWSize: " + baos.size());
            is = new InflaterInputStream(new ByteArrayInputStream(bos.toByteArray()));
        }
        final List<String> check = cei.uncompress(is);
        for (int index = 0; index < paths.size(); index++) {
            final String path = paths.get(index);
            final String restored = check.get(index);
            assertEquals(path, restored);
        }
        assertEquals(check.size(), paths.size());
    }
}
