/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SignatureException;
import java.security.spec.InvalidKeySpecException;
import java.util.zip.CheckedInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.crypto.AWSign;
import org.appwork.utils.crypto.SignatureViolationException;
import org.appwork.utils.net.NullOutputStream;
import org.appwork.utils.zip.ZipIOException;
import org.appwork.utils.zip.ZipIOReader;
import org.appwork.utils.zip.ZipIOWriter;
import org.appwork.utils.zip.ZipIOWriter.EmptyPathZipIOException;

/**
 * @author daniel
 * @date Aug 24, 2023
 *
 */
@TestDependency({ "org.appwork.utils.zip.ZipIOWriter" })
public class SignedZipsTest extends OnClassPathZipJarTests {
    public static void main(String[] args) {
        run();
    }

    private ZipEntry   damagedEntry = null;
    private PrivateKey priv;
    private PublicKey  pub;

    @Override
    public void runTest() throws Exception {
        testUnsigned();
        testSigned();
    }

    protected void testUnsigned() throws IOException, FileNotFoundException, ZipIOException, Exception {
        final File testFile = Application.getTempUniqueResource(".test");
        File extract = Application.getTempUniqueResource(".extract");
        try {
            IO.secureWrite(testFile, "12345".getBytes());
            new AssertAnException<EmptyPathZipIOException>() {
                @Override
                protected void run() throws Exception {
                    ZipIOWriter zipper = new ZipIOWriter(new ByteArrayOutputStream());
                    zipper.add(testFile, false);
                    zipper.close();
                }
            };
            ByteArrayOutputStream baos;
            ZipIOWriter zipper = new ZipIOWriter(baos = new ByteArrayOutputStream()) {
                /**
                 * @see org.appwork.utils.zip.ZipIOWriter#ignore(java.util.zip.ZipEntry)
                 */
                @Override
                public boolean ignore(ZipEntry e) {
                    if (e.getSize() > 1000 * 1000) {
                        return true;
                    }
                    return false;
                }
            };
            zipper.setFolderAutoCreationEnabled(true);
            zipper.setLogger(LogV3.defaultLogger());
            zipper.add(testFile, true, testFile.getName());
            zipper.add(testFile, true, "subfolder", testFile.getName());
            // zipper.add(extract, false);
            zipper.close();
            ZipIOReader reader = new ZipIOReader(baos.toByteArray());
            assertFalse(reader.isSigned());
            assertThat(reader.getZipFiles().length).is(3);
            assertThat(reader.getZipFiles()[0].getName().equals(testFile.getName()));
            assertThat(reader.getZipFiles()[1].getName().equals("subfolder"));
            assertTrue(reader.getZipFiles()[1].isDirectory());
            assertThat(reader.getZipFiles()[2].getName().equals("subfolder/" + testFile.getName()));
            reader.extractTo(extract);
            reader.close();
            assertThat(Hash.getFileHash(testFile, Hash.HASH_TYPE_SHA256)).is(Hash.getFileHash(new File(extract, testFile.getName()), Hash.HASH_TYPE_SHA256));
            assertThat(Hash.getFileHash(testFile, Hash.HASH_TYPE_SHA256)).is(Hash.getFileHash(new File(extract, "subfolder/" + testFile.getName()), Hash.HASH_TYPE_SHA256));
        } finally {
            if (extract.exists()) {
                Files.deleteRecursive(extract, false);
            }
            if (testFile.exists()) {
                Files.deleteRecursive(testFile, false);
            }
        }
        new AssertAnException<ZipIOException>() {
            @Override
            protected void run() throws ZipIOException {
                ZipIOWriter zipper = new ZipIOWriter(new ByteArrayOutputStream());
                zipper.add(new File("hjksadasdasdfkjasd.txt"), false);
                zipper.close();
            }

            /**
             * @see org.appwork.testframework.AWTest.AssertAnException#validateException(java.lang.Exception)
             */
            @Override
            protected boolean validateException(Exception e) {
                if (e instanceof ZipIOException) {
                    if (Exceptions.getInstanceof(e, FileNotFoundException.class) != null) {
                        return true;
                    }
                }
                return false;
            }
        };
    }

    protected void testSigned() throws InvalidKeySpecException, NoSuchAlgorithmException, SignatureViolationException, IOException, FileNotFoundException, ZipIOException, Exception, InvalidKeyException, SignatureException, UnsupportedEncodingException, ZipException {
        priv = AWSign.getPrivateKey(
                "MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCrzJ8fz7xdcOqoQpBmltIJKM0XmUiFO2Z3gdAs1vxQ/+xfr6Uu/gFEaG8hW12dXsPbx0oBZ8TRLmM9N1OZNlStF1rn760XSU/VUyA6PvFAhjSoSb+uutnU+SYTfGy66Gb8LVuRdnBf4FB4m7r8ACVur62SOzrQTpUaNfofA7s2siwS8Ns6CuN1lG57mxOehtsDtYFX8F40S+j7ZQsaOsOdspLJdadm6+P+m8B/1DQQZDWI6pwDSAE72E6DCNNRuAk6d1YniZGBn7Hf/stfPNXMh8lB71JfqPWj8QHp8jBonlkS2AivvFAsXQ8/iRuu9jIiBSvwd9zudSxm62UweCKTAgMBAAECggEACijGCigLMA1i2wNgrahikEcY5bnbR4GtPAjxqg0mi5WuwTPOVVqQ8Z4pYDQac5lshJ0i27VqtqIDHVYdu/QyCrBUYF3UgwmDTo6lW7xoINQu3frKeoczJXx8S2+ysFTY7jWycgYqfAj0SUWQR6GslQCPh5O4FSRofc3OVjIuynujAOCdspvDkmzBTsvRDULUv8pgcMR8hyllG1yyq4S2A3jzJ98QgUQ6MBud9TvmyYQCys9XwW5xcGDOeY06Be2fPof2aDaUhTRh9txhFaO+SFCC7Lr5rTUbipoPyJluuun8S2OB2dL0qTGinTi8A+rF0MuPhNQiA2BQYfmQxvBVKQKBgQDQr26y2C+9IZ4X7ZXMssnNXlWa8aKj9PE/IGt+KDYJ9N2MUgAZwfNikav2hDV8smibviXUecoW7G49CEoRaogFd8Y2a5Ok5tyONj5FSoYN5Jb4FGUnHcJXoES6ZVjc0VHM00HsUYzazJxgquW6bd/whMJMM845cd0JUH15qfftOQKBgQDSwD4/SfJB5NWgAXH7cl5LmTZE+3Z/Yb3LuaEh655vd2y24YfTcZjLHzOfyG+VfGHE31YQtYFK7czl02l5ZXoePSVi97WJiOaIvuZ5/c3OeYqtwYMCDzeV4Ck7Wl/32XCZw+BocUnQ2gkodqAxjUN0zZ9U5QNJfBw7YIwwpxeaKwKBgQCsPD63KSqthLOAuDxsyBs6fuNUkJ5Vh/Ic0dsmMnX3XbwE9iDTIXqJHgW53oBuvKkN8bAzX5llthl+EftfyHcfXC2h+MPQ1CGw1iAq/EkL6mCXu+2IjU07GTHgTYk8Jx7GWI4OR3hjlYBzYFletxSe5FOgpRlN3CY8a8xDv/66MQKBgQDLeHFglxr+RVl+Pob2Vaf2YviDRyhTuWENlK2d2ya+xVU/h6SkHtN+oCQuSsx4TXrzIRCt1x754XbmGOOHIdLx6OT0lOU+4N55/Zz1AHY3tx6NvPun/FsVJzOHqyxmVRciTXuWOnIi4q7e0xC8Zn9PAMqoouY2h02WghM3Ucs2uwKBgGLMbYcKGT247L72ENpfVIMgmuWWNvaiY37+Ju2kw7qNtX1X3RgkuAYjpUASGlah+dIXpBe6W1QwfPnDAE9aUZZ2TW2Oxe2kBydcfg494i+ruygg8AvhVuW6zLtZgdmCsoWbMzqNYl4VjlcEcDwLTumxnaTst/TPwO0RF4/5tgN/");
        pub = AWSign.getPublicKey("MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAq8yfH8+8XXDqqEKQZpbSCSjNF5lIhTtmd4HQLNb8UP/sX6+lLv4BRGhvIVtdnV7D28dKAWfE0S5jPTdTmTZUrRda5++tF0lP1VMgOj7xQIY0qEm/rrrZ1PkmE3xsuuhm/C1bkXZwX+BQeJu6/AAlbq+tkjs60E6VGjX6HwO7NrIsEvDbOgrjdZRue5sTnobbA7WBV/BeNEvo+2ULGjrDnbKSyXWnZuvj/pvAf9Q0EGQ1iOqcA0gBO9hOgwjTUbgJOndWJ4mRgZ+x3/7LXzzVzIfJQe9SX6j1o/EB6fIwaJ5ZEtgIr7xQLF0PP4kbrvYyIgUr8Hfc7nUsZutlMHgikwIDAQAB");
        byte[] testSig = AWSign.createSign(new byte[] { 1 }, priv, false);
        AWSign.verify(new byte[] { 1 }, pub, testSig, false);
        ByteArrayOutputStream baos;
        File testFile = Application.getTempUniqueResource(".test");
        IO.secureWrite(testFile, "12345".getBytes());
        ZipIOWriter zipper = new ZipIOWriter(baos = new ByteArrayOutputStream()) {
            /**
             * @see org.appwork.utils.zip.ZipIOWriter#ignore(java.util.zip.ZipEntry)
             */
            @Override
            public boolean ignore(ZipEntry e) {
                if (e.getSize() > 1000 * 1000) {
                    return true;
                }
                return false;
            }
        };
        zipper.setLogger(LogV3.defaultLogger());
        zipper.setSignaturePrivateKey(priv);
        zipper.add(Application.getResource(""), true);
        zipper.add(testFile, false, testFile.getName());
        zipper.close();
        final ByteArrayOutputStream damaged;
        ZipIOWriter zipper2 = new ZipIOWriter(damaged = new ByteArrayOutputStream()) {
            /**
             * @see org.appwork.utils.zip.ZipIOWriter#ignore(java.util.zip.ZipEntry)
             */
            @Override
            public boolean ignore(ZipEntry e) {
                if (e.getSize() > 1000 * 1000) {
                    return true;
                }
                return false;
            }

            /**
             * @see org.appwork.utils.zip.ZipIOWriter#updateSigner(int, byte[], java.util.zip.ZipEntry)
             */
            @Override
            protected void updateSigner(int len, byte[] buf, ZipEntry zipAdd) throws SignatureException {
                if (damagedEntry == null && len > 0) {
                    damagedEntry = zipAdd;
                    super.updateSigner(0, new byte[0], zipAdd);
                    System.out.println("Damage file: " + zipAdd.getName());
                } else {
                    super.updateSigner(len, buf, zipAdd);
                }
            }
        };
        zipper2.setLogger(LogV3.defaultLogger());
        zipper2.setSignaturePrivateKey(priv);
        zipper2.add(Application.getResource(""), true);
        zipper2.add(testFile, false, testFile.getName());
        zipper2.close();
        final byte[] bytes = baos.toByteArray();
        assertEqualsNot(bytes, damaged.toByteArray());
        File file = Application.getTempUniqueResource(".zip");
        file.delete();
        IO.secureWrite(file, bytes);
        file.deleteOnExit();
        testREader(new ZipIOReader(file));
        testREader(new ZipIOReader(bytes));
        new AssertAnException<Exception>() {
            @Override
            protected void run() throws Exception {
                // damage a byte.warning... this might not be a data byte and this create no - or a different exception
                testREader(new ZipIOReader(damaged.toByteArray()) {
                    /**
                     * @see org.appwork.utils.zip.ZipIOReader#crcCheck(java.util.zip.ZipEntry, java.util.zip.CheckedInputStream)
                     */
                    @Override
                    protected void crcCheck(ZipEntry entry, CheckedInputStream in) throws ZipIOException {
                        // we would get a crc error instead a sig val
                    }
                });
            }
        };
        file.delete();
    }

    protected void testREader(ZipIOReader reader) throws Exception {
        reader.setLogger(LogV3.defaultLogger());
        assertTrue(reader.isSigned());
        reader.setSignaturePublicKey(pub);
        reader.verify();
        try {
            for (ZipEntry e : reader.getZipFiles()) {
                reader.verify(e);
                if (e.isDirectory()) {
                    continue;
                }
                reader.extract(e, new NullOutputStream());
                ;
            }
        } finally {
            reader.close();
        }
        System.out.println();
    }
}
