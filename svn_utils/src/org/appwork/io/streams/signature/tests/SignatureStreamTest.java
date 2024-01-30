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
package org.appwork.io.streams.signature.tests;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.util.Arrays;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.exceptions.WTFException;
import org.appwork.io.streams.signature.DigestInterface;
import org.appwork.io.streams.signature.HandleStreamSignatureInputStream;
import org.appwork.io.streams.signature.MacDigester;
import org.appwork.io.streams.signature.MessageDigester;
import org.appwork.io.streams.signature.SignatureMismatchException;
import org.appwork.io.streams.signature.StreamEndedUnexpectedException;
import org.appwork.io.streams.signature.StreamSignatureCreatingOutputStream;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;

/**
 * @author thomas
 * @date 07.10.2021
 *
 */
public class SignatureStreamTest {
    /**
     *
     */
    private static final byte[]                        B     = new byte[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    private static StreamSignatureCreatingOutputStream out;
    private static ByteArrayOutputStream               baos;
    private static byte[]                              rawInput;
    private static byte[]                              bytes;
    private static byte[]                              nonce = null;
    private static int                                 chunkLength;
    private static DigestInterface                     digester;
    private static MacDigester                         macDigester;

    public static void main(String[] args) throws Exception {
        // Stream Version Byte
        // calculate how much bytes we need to store it
        digester = new MessageDigester(MessageDigest.getInstance(Hash.HASH_TYPE_SHA256));
        SecretKeySpec secretKeySpec = new SecretKeySpec("ABC".getBytes(), Hash.HASH_TYPE_SHA256);
        Mac mac = Mac.getInstance("HmacSHA256");
        mac.init(secretKeySpec);
        macDigester = new MacDigester(mac);
        testReadExactContentLength();
        testIncrementRead();
        testSslMacEOFCutStreamMD5();
        /*     */
        for (Provider provider : Security.getProviders()) {
            for (Provider.Service service : provider.getServices()) {
                try {
                    mac = Mac.getInstance(service.getAlgorithm());
                    mac.init(secretKeySpec);
                    // System.out.println("Test on " + service.getAlgorithm() + " Cloneable:" + (mac instanceof Cloneable));
                    macDigester = new MacDigester(mac);
                    digester = macDigester;
                    try {
                        mac.clone();
                        testIncompleteStreamClonableMAC();
                    } catch (CloneNotSupportedException e) {
                        testIncompleteStreamNotCloneable();
                    }
                } catch (NoSuchAlgorithmException e) {
                    // e.printStackTrace();
                } catch (InvalidKeyException e) {
                    // e.printStackTrace();
                }
                try {
                    digester = new MessageDigester(MessageDigest.getInstance(service.getAlgorithm()));
                    try {
                        mac.clone();
                        testIncompleteStreamClonableMAC();
                    } catch (CloneNotSupportedException e) {
                        testIncompleteStreamNotCloneable();
                    }
                } catch (NoSuchAlgorithmException e) {
                    // e.printStackTrace();
                } catch (InvalidKeyException e) {
                    // e.printStackTrace();
                }
            }
        }
        testReadUntilEOF();
        testReadToEndOfChunk();
        testCorruptStream();
        testReadPlus1Scenario();
        testEmptyStream();
    }

    private static void testIncrementRead() {
    }

    protected static void testIncompleteStreamClonableMAC() throws Exception {
        write(null, 100, 1024, digester);
        HandleStreamSignatureInputStream in;
        byte[] bytesTmp = new byte[102 + digester.getLength()];
        // cut stream after the first signature. this looks like a valid end of stream
        System.arraycopy(bytes, 0, bytesTmp, 0, bytesTmp.length);
        bytes = bytesTmp;
        in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
        try {
            IO.readStream(-1, in);
            throw new Exception("Expected StreamEndedUnexpectedException here");
        } catch (StreamEndedUnexpectedException e) {
            if (in.getLastValidPayloadPosition() != chunkLength) {
                throw new Exception("Validation not correct");
            }
            if (in.getPayloadBytesLoaded() != chunkLength) {
                throw new Exception("Payload bytes read incorrect");
            }
            if (!in.isClosed()) {
                throw new Exception("STream should be closed");
            }
            System.out.println("TEST testIncompleteStream " + digester + ": SUCCESS");
        } catch (SignatureMismatchException e) {
            e.printStackTrace();
            System.out.println("The Digester " + digester + " does not support Cloning");
        }
    }

    protected static void testSslMacEOFCutStreamMD5() throws Exception {
        SecretKeySpec secretKeySpec = new SecretKeySpec("ABC".getBytes(), Hash.HASH_TYPE_SHA256);
        Mac mac = Mac.getInstance("SslMacMD5");
        mac.init(secretKeySpec);
        write(null, 100, 1024, new MacDigester(mac));
        HandleStreamSignatureInputStream in;
        byte[] bytesTmp = new byte[134];
        // cut stream after the first signature. this looks like a valid end of stream
        System.arraycopy(bytes, 0, bytesTmp, 0, bytesTmp.length);
        bytes = bytesTmp;
        in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
        try {
            IO.readStream(-1, in);
            throw new Exception("Expected StreamEndedUnexpectedException here");
        } catch (EOFException e) {
            if (in.getLastValidPayloadPosition() != chunkLength) {
                throw new Exception("Validation not correct");
            }
            if (in.getPayloadBytesLoaded() != chunkLength) {
                throw new Exception("Payload bytes read incorrect");
            }
            if (!in.isClosed()) {
                throw new Exception("STream should be closed");
            }
            System.out.println("TEST testSslMacMD5: SUCCESS");
        }
    }

    protected static void testIncompleteStreamNotCloneable() throws Exception {
        write(null, 100, 1024, digester);
        HandleStreamSignatureInputStream in;
        byte[] bytesTmp = new byte[102 + digester.getLength()];
        // cut stream after the first signature. this looks like a valid end of stream
        System.arraycopy(bytes, 0, bytesTmp, 0, bytesTmp.length);
        bytes = bytesTmp;
        in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
        try {
            IO.readStream(-1, in);
            throw new Exception("Expected StreamEndedUnexpectedException here");
        } catch (StreamEndedUnexpectedException e) {
            System.out.println("The Digester " + digester + " does not support Cloning and should not throw this exception");
            e.printStackTrace();
        } catch (SignatureMismatchException e) {
            if (in.getLastValidPayloadPosition() != 0) {
                throw new Exception("Validation not correct");
            }
            if (in.getPayloadBytesLoaded() != chunkLength) {
                throw new Exception("Payload bytes read incorrect");
            }
            if (!in.isClosed()) {
                throw new Exception("STream should be closed");
            }
            System.out.println("The Digester " + digester + " does not support Cloning");
            System.out.println("TEST testIncompleteStream " + digester + ": SUCCESS");
        }
    }

    protected static void testEmptyStream() throws Exception {
        write(nonce, chunkLength, 0, digester);
        HandleStreamSignatureInputStream in;
        IO.readStream(-1, in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce));
        if (in.getLastValidPayloadPosition() != rawInput.length) {
            throw new Exception("Validation not correct");
        }
        if (in.getPayloadBytesLoaded() != rawInput.length) {
            throw new Exception("Payload bytes read incorrect");
        }
        if (!in.isClosed()) {
            throw new Exception("STream should be closed");
        }
        System.out.println("TEST Empty STream: SUCCESS");
    }

    protected static void testReadPlus1Scenario() throws Exception {
        // create a frame that ends before its chunk end
        write(null, 10, 3, digester);
        HandleStreamSignatureInputStream in;
        // read exa
        IO.readStream(3, in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce));
        if (in.getLastValidPayloadPosition() != rawInput.length) {
            throw new Exception("Validation not correct");
        }
        if (in.getPayloadBytesLoaded() != rawInput.length) {
            throw new Exception("Payload bytes read incorrect");
        }
        if (!in.isClosed()) {
            throw new Exception("STream should be closed");
        }
        System.out.println("TEST +1 scenario: SUCCESS");
    }

    protected static void write(byte[] nonce, final int chunkLength, int byteCount, DigestInterface digester) throws IOException {
        SignatureStreamTest.nonce = nonce;
        if (chunkLength > 0) {
            SignatureStreamTest.chunkLength = chunkLength;
        }
        if (digester != null) {
            SignatureStreamTest.digester = digester;
        }
        ByteArrayOutputStream rawBaos = new ByteArrayOutputStream();
        out = new StreamSignatureCreatingOutputStream(baos = new ByteArrayOutputStream(), digester, nonce, chunkLength);
        while (byteCount > 0) {
            out.write(B, 0, Math.min(byteCount, B.length));
            rawBaos.write(B, 0, Math.min(byteCount, B.length));
            byteCount -= Math.min(byteCount, B.length);
        }
        rawInput = rawBaos.toByteArray();
        out.close();
        bytes = baos.toByteArray();
        int expectedBytes = rawInput.length;
        int x = IO.writeLongOptimized(out.getStreamVersion(), null);
        expectedBytes += x;
        expectedBytes += Math.max(1, Math.ceil(rawInput.length / (double) chunkLength)) * (digester.getLength() + IO.writeLongOptimized(chunkLength, null));
        if (bytes.length != expectedBytes) {
            throw new WTFException("We expected a stream size of " + expectedBytes);
        }
        if (bytes.length != out.getRawBytesWritten()) {
            throw new WTFException("We expected a stream size of " + expectedBytes);
        }
    }

    protected static void testCorruptStream() throws NoSuchAlgorithmException, IOException {
        write(null, 9, 1024, new MessageDigester(MessageDigest.getInstance(Hash.HASH_TYPE_SHA256)));
        try {
            bytes[0]++;
            HandleStreamSignatureInputStream in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
            IO.readStream(rawInput.length, in);
            throw new Exception("This should have bannged");
        } catch (Exception e) {
            bytes[0]--;
            // e.printStackTrace();
            System.out.println("TEST detect bad first byte: SUCCESS");
        }
        try {
            bytes[bytes.length / 2]++;
            HandleStreamSignatureInputStream in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
            IO.readStream(rawInput.length, in);
            throw new Exception("This should have bannged");
        } catch (Exception e) {
            bytes[bytes.length / 2]--;
            // e.printStackTrace();
            System.out.println("TEST detect bad middle byte: SUCCESS");
        }
        try {
            bytes[bytes.length - 1]++;
            HandleStreamSignatureInputStream in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
            byte[] readBytes = IO.readStream(rawInput.length, in);
            throw new Exception("This should have bannged");
        } catch (Exception e) {
            bytes[bytes.length - 1]--;
            // e.printStackTrace();
            System.out.println("TEST detect last byte: SUCCESS");
        }
    }

    protected static void testReadToEndOfChunk() throws Exception {
        write(null, 9, 1024, new MessageDigester(MessageDigest.getInstance(Hash.HASH_TYPE_SHA256)));
        HandleStreamSignatureInputStream in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
        byte[] readBytes = IO.readStream(chunkLength, in);
        if (in.getLastValidPayloadPosition() != in.getPayloadBytesLoaded()) {
            throw new WTFException("Not fully validated");
        }
        if (!in.isClosed()) {
            throw new Exception("STream should be closed");
        }
        if (in.getLastValidPayloadPosition() != chunkLength) {
            throw new Exception("Validation not correct");
        }
        if (in.getPayloadBytesLoaded() != chunkLength) {
            throw new Exception("Payload bytes read incorrect");
        }
        System.out.println("TEST Reading exact to end of chunk: SUCCESS");
    }

    protected static void testReadExactContentLength() throws Exception {
        write(null, 9, 1024, new MessageDigester(MessageDigest.getInstance(Hash.HASH_TYPE_SHA256)));
        HandleStreamSignatureInputStream in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce);
        byte[] readBytes = IO.readStream(rawInput.length, in);
        if (!Arrays.equals(readBytes, rawInput)) {
            throw new WTFException("COntent Mismatch");
        }
        if (in.getLastValidPayloadPosition() != in.getPayloadBytesLoaded()) {
            throw new WTFException("Not fully validated");
        }
        if (in.getLastValidPayloadPosition() != rawInput.length) {
            throw new Exception("Validation not correct");
        }
        if (in.getPayloadBytesLoaded() != rawInput.length) {
            throw new Exception("Payload bytes read incorrect");
        }
        if (!in.isClosed()) {
            throw new Exception("STream should be closed");
        }
        System.out.println("TEST Reading to end of chunvk: SUCCESS");
    }

    protected static void testReadUntilEOF() throws Exception {
        write(null, 9, 1024, new MessageDigester(MessageDigest.getInstance(Hash.HASH_TYPE_SHA256)));
        HandleStreamSignatureInputStream in = new HandleStreamSignatureInputStream(new ByteArrayInputStream(bytes), digester, nonce) {
            /*
             * (non-Javadoc)
             *
             * @see org.appwork.io.streams.signature.HandleStreamSignatureInputStream#readNextPayloadBlockSize()
             */
            @Override
            protected long readNextPayloadBlockSize() throws IOException {
                long ret = super.readNextPayloadBlockSize();
                if (ret != chunkLength) {
                    throw new IOException("bad Chunksize read");
                }
                return ret;
            }
        };
        // read header
        in.read(new byte[] {});
        if (in.getStreamVersion() != StreamSignatureCreatingOutputStream.STREAM_VERSION_08_10_2021) {
            throw new WTFException("Bad StreamVersion");
        }
        ByteArrayOutputStream baosCheck;
        byte[] buffer = new byte[10];
        try {
            while (true) {
                new DataInputStream(in).readFully(buffer);
                if (!Arrays.equals(buffer, B)) {
                    throw new Exception("Content MissMatch");
                }
            }
        } catch (EOFException e) {
            // e.printStackTrace();
            if (in.getLastValidPayloadPosition() != rawInput.length) {
                throw new Exception("Validation not correct");
            }
            if (in.getPayloadBytesLoaded() != rawInput.length) {
                throw new Exception("Payload bytes read incorrect");
            }
        }
        if (in.getLastValidPayloadPosition() != in.getPayloadBytesLoaded()) {
            throw new WTFException("Not fully validated");
        }
        if (!in.isClosed()) {
            throw new Exception("STream should be closed");
        }
        System.out.println("TEST Reading until -1: SUCCESS");
    }
}
