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
package org.appwork.utils.crypto.tests;

import java.io.File;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Random;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.crypto.AWSign;
import org.appwork.utils.crypto.SignatureViolationException;
import org.appwork.utils.encoding.Base64;

/**
 * @author thomas
 * @date 15.01.2022
 *
 */
public class AWSignTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final PublicKey pub = KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(Base64.decode("MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAg+EQ1wHD62QGjzJalAkl1WjExeS345ZkCMtuyvqP3NLVpUbfZjc/IeHVi9qKBUPtV8ca8QfOZo8ACNIBvUxEiVy4YFE7vqZfBNV0uEz/kHSXxDlFeiv0+BFMgcXow0NYBjGDT02/1ddmjEMtnAXnjqUwlVPorzOmJoeuNSLCyCcOe0pKuF1yDha9TkEsaUcJ8kho+09kQvhMl5mKnuTUc81nIHHVb4GClRmFp1kfB9BbqPc9sL5jg1BrmjHMCD84HZk4OehxJ8AeA+veVRH2Gn6gcslPcrgNw1zK6VcXzCqsuZCAejAyDHnX+jay1SaxmHDgk5jc+agee+M2+QPiwQIDAQAB")));
        final PrivateKey pk = KeyFactory
                .getInstance("RSA")
                .generatePrivate(
                        new PKCS8EncodedKeySpec(
                                Base64.decode("MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCD4RDXAcPrZAaPMlqUCSXVaMTF5LfjlmQIy27K+o/c0tWlRt9mNz8h4dWL2ooFQ+1XxxrxB85mjwAI0gG9TESJXLhgUTu+pl8E1XS4TP+QdJfEOUV6K/T4EUyBxejDQ1gGMYNPTb/V12aMQy2cBeeOpTCVU+ivM6Ymh641IsLIJw57Skq4XXIOFr1OQSxpRwnySGj7T2RC+EyXmYqe5NRzzWcgcdVvgYKVGYWnWR8H0Fuo9z2wvmODUGuaMcwIPzgdmTg56HEnwB4D695VEfYafqByyU9yuA3DXMrpVxfMKqy5kIB6MDIMedf6NrLVJrGYcOCTmNz5qB574zb5A+LBAgMBAAECggEAWLi/lYZQgjoG16tumI0W8N3NE71toST6I5iI7vFme48zwD9P5/pe9LJz8eSSWjx6nkUK8QDpcMHfqg9usCVxLmA8gj/kS7ytzBi2r47NmCd4OsC05x5PbdxldiDpGQRjYbdJub56wqhpCw/ezUqDn8muR6ftsIC01NMO9hxuoiv1tE1GXZwBo36YSPb5NsB0Og5p0w8gwogXo0/TLIOJVy5ysZGACrXMaSN7DX/XP5hp4rXEfbY9vQdegVShejIKOIc9r5+0btRPjGP7YkMRWvTQQt43jWgI8cBIFUcZ4fYmwegzGnl1OONVzXjum10B2E3R2vmDEZqrLVB22I5Y4QKBgQDq0W0/g9H7hyd0NroZP8yP0/bOZO6ZYzUNMRQgZ/BO76yMUId6+wi0RREzqya+r/ur+kDXCs+liTlbJ+KyjRv29ls40eDW5OpCG9ccFguzg1CUpyIRu2obKC5i59x3I4KGiUplumKcSE8QILD09DslvoSf2pHBIQKZNEdVMROObQKBgQCPxoGORYshbsptqZ5batMTWAeb5xeBn6rxBNDWAzeD+qXazOsTWYgU4310nq/Vqyc7UU18VPoRTTflUyhJFoFxJRjTEHxa/hKjIOGPYayCK2EHrMXHoSxZsUvSbSH1Y84zFAbDcRPylXg1pGnn5CyDB5jijS6mQxnT94TRgX1hJQKBgFwzcUcgNmIiFn7OQlJJt8O9wcoW3Y0C5EDSxYlX5obIGyNZN2k1ipxmBjQYfvUe2p4TfEQzrYbdE9VUGvJq79EPuI/d8P/QEJ92mQchLOUGqaxE197IjQguxc/2JJ3vJoA3Bixde/zLc6fsfi8getz+Ksstok+H66JGYb/0ri4dAoGAFnZeAVtOHGAR0kZAzmmHJquHLM1S99Z5P4SQGA+SmdUMGn4PcAt53kGYdSLht9EwpOzT3UvtccyNog926MxSVtoD4d3ef9zYDpJxixQofoHGfAt7LvA4XJ79iJeySYNZUNOdJuXAxxKhIEhan3cfmS0Trrl+A03SeDJgltbTPt0CgYEA1uPP5gpL029gtx3shiQFblpVl3AhUE1dmDITJYrGqD+06Z+nPHu73kOnVdPKgy9wYIIxcyx/DrQfcT5e1+IZy9bZ5OOIUVi9qNsQ1RhvFzEwo8tiE/1LX7XUIC2gIjyY0Q+VXLk03UgjV7qAgOg4X/foetGZn2NHmc4NUUaCoNE=")));
        byte[] sign = AWSign.createSign("Apfelbaum".getBytes(), pk, true);
        AWSign.verify("Apfelbaum".getBytes(), pub, sign, true);
        try {
            AWSign.verify("ApfelbaumFailed".getBytes(), pub, sign, true);
            throw new WTFException("did not fail with SignatureViolationException");
        } catch (SignatureViolationException okay) {
        }
        // System.out.println(AWSignTest.class.getResource(AWSignTest.class.getSimpleName() + ".class").toURI());
        // final URL loc = AWSignTest.class.getProtectionDomain().getCodeSource().getLocation();
        // System.out.println(loc);
        // only works if the test is not part of a jar
        final File file = new File(AWSignTest.class.getResource(AWSignTest.class.getSimpleName() + ".class").toURI());
        byte[] additionalData = new byte[] { 1, 2, 3 };
        sign = AWSign.createSign(file, pk, true, additionalData);
        AWSign.verify(file, pub, sign, true, additionalData);
        try {
            additionalData = new byte[] { 1, 1, 1 };
            AWSign.verify(file, pub, sign, true, additionalData);
            throw new WTFException("did not fail with SignatureViolationException");
        } catch (SignatureViolationException okay) {
        }
        try {
            new Random().nextBytes(sign);
            AWSign.verify(file, pub, sign, true, additionalData);
            throw new WTFException("did not fail with SignatureViolationException");
        } catch (SignatureViolationException okay) {
        }
    }
}
