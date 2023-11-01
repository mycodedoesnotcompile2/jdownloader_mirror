package org.appwork.io.streams.signature;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.util.Arrays;

import javax.crypto.Mac;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.IO;

public class HandleStreamSignatureInputStream extends InputStream {

    private DigestInterface     digester;
    private long                remainingPayloadSize = 0;
    private long                payloadBytesRead     = 0;

    private byte[]              nonceBytes;

    /**
     * Holds the calculated signature after reading a chunk
     */
    private byte[]              calculatedSignature;
    private long                lastValidPayloadPosition;
    private final byte[]        single;
    private HoldBackInputStream in;

    private final int           signatureLength;
    /**
     * is used to read the expected signature from the stream
     */
    private final byte[]        expectedSignature;
    private long                streamVersion        = -1;
    private DataInputStream     dataIn;
    private boolean             closed;
    private DataOutputStream    digesterStream;

    public HandleStreamSignatureInputStream(final InputStream is, MessageDigest digest, final byte[] nonceBytes) {
        this(is, new MessageDigester(digest), nonceBytes);
    }

    public HandleStreamSignatureInputStream(final InputStream is, Mac mac, final byte[] nonceBytes) {
        this(is, new MacDigester(mac), nonceBytes);
    }

    /**
     *
     * @param is
     * @param digester
     * @param nonceBytes
     * @param useExtendedEndSignatureheck
     *            - works only if the MAc or MessageDigest supports cloneing
     */
    public HandleStreamSignatureInputStream(final InputStream is, DigestInterface digester, final byte[] nonceBytes) {

        single = new byte[1];
        signatureLength = digester.getLength();

        // +1 to detect EOF if we read exactly the payload length
        in = new HoldBackInputStream(is, signatureLength, 1);
        dataIn = new DataInputStream(in);
        expectedSignature = new byte[signatureLength];
        this.digester = digester;
        digesterStream = new DataOutputStream(new DigesterOutputStream(digester));
        this.nonceBytes = nonceBytes;

    }

    protected void initChunkDigester(long chunkSize) throws IOException {
        StreamSignatureCreatingOutputStream.initChunkDigesterStatic(digesterStream, streamVersion, chunkSize, nonceBytes, calculatedSignature);

    }

    @Override
    public int read() throws IOException {
        while (true) {
            final int ret = internalRead(single, 0, 1);
            if (ret == 0) {
                continue;
            } else if (ret < 0) {
                return -1;
            } else {
                // this method mnust return 0-255
                return single[0] & 0xFF;
            }
        }
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        return internalRead(b, off, len);
    }

    public long readVersionNumber() throws IOException {
        if (streamVersion < 0) {
            read(new byte[] {});
        }
        return streamVersion;
    }

    public synchronized void mark(int readlimit) {

    }

    /*
     * (non-Javadoc)
     *
     * @see java.io.InputStream#markSupported()
     */
    @Override
    public boolean markSupported() {

        return false;
    }

    private final int internalRead(final byte[] b, final int off, final int len) throws IOException, UnsupportedEncodingException, SignatureMismatchException {
        if (closed) {
            return -1;
        }
        if (in.getTail() != null) {
            // we already reached EOF
            closed = true;
            return -1;
        }
        if (in.getPayloadBytesLoaded() == 0) {
            // start of stream
            this.streamVersion = readHeader();
            if (streamVersion == 1) {
                // ok 08.10.2021
            } else {

                throw new IOException("Unknown Streamversion");
            }
        }

        if (remainingPayloadSize == 0) {
            // init next chunk
            remainingPayloadSize = readNextPayloadBlockSize();

            initChunkDigester(remainingPayloadSize);
        }

        final int toRead = (int) Math.min(remainingPayloadSize, len);
        int actuallyRead = in.read(b, off, toRead);
        if (actuallyRead >= 0) {
            remainingPayloadSize -= actuallyRead;
            payloadBytesRead += actuallyRead;
            digesterStream.write(b, off, actuallyRead);
        }
        if (in.getTail() != null) {

            byte[] tail = in.getTail();
            if (len == 0) {

                // the first time tail is set, len must be >0, else we would not have read anything
                throw new WTFException("not possible here");
            }

            validateSignature(tail, true);
            // no return. it is possible to receive the tail, but return read bytes
        } else if (remainingPayloadSize == 0) {
            // chunk end
            int n = 0;
            boolean tail = false;
            while (n < expectedSignature.length) {
                int count = in.read(expectedSignature, n, expectedSignature.length - n);
                if (count < 0) {
                    if (n == 0) {
                        // end of stream. the current buffer is the tail

                        if (in.getTail() == null || in.getTail().length != expectedSignature.length) {
                            throw new WTFException("Invalid Tail");
                        }
                        tail = true;
                        System.arraycopy(in.getTail(), 0, expectedSignature, 0, expectedSignature.length);
                        n = expectedSignature.length;
                    } else {
                        throw new EOFException("Unexpected End Of Stream");
                    }
                } else {
                    n += count;
                }
            }
            validateSignature(expectedSignature, tail);

            if (!tail) {
                if (in.getTail() != null) {
                    // we reached the EOF while reading the signature.

                    throw new EOFException("Unexpected End Of Stream");
                }
            }
        }
        if (actuallyRead < 0) {
            closed = true;
        }
        return actuallyRead;
    }

    public boolean isClosed() {
        return closed;
    }

    protected void validateSignature(byte[] tail, boolean eof) throws SignatureMismatchException, StreamEndedUnexpectedException {

        if (eof) {
            DigestInterface cloneBeforeEndSalt = null;

            if (remainingPayloadSize == 0) {
                // we need the "incomplete stream" check only if remaining ==0. only if the stream got cut exactly after a block, we need to
                // check if the final signature failed, or is just not an end signature.
                // if a digester does NOT IMPLEMENT CLONEABLE, this will fail, and the inputstream cannot tell
                // StreamEndedUnexpectedException and SignatureMismatchException appart.
                // instead of a StreamEndedUnexpectedException exception, the stream would throw a SignatureMismatchException exception,
                // because the final signature failed.
                try {
                    cloneBeforeEndSalt = digester.clone();
                } catch (CloneNotSupportedException e) {
                    // not supported
                    // e.printStackTrace();
                }

            }
            digester.update(StreamSignatureCreatingOutputStream.END_SIGNATURE_SALT);
            calculatedSignature = digester.doFinal();

            if (!Arrays.equals(tail, calculatedSignature)) {
                if (cloneBeforeEndSalt != null && Arrays.equals(tail, cloneBeforeEndSalt.doFinal())) {
                    // stream is broken end ended before the final block
                    // all bytes so far are correct, but the stream ended before the final block was read
                    this.lastValidPayloadPosition = payloadBytesRead;

                    throw new StreamEndedUnexpectedException();
                }
                throw new SignatureMismatchException();
            } else {
                this.lastValidPayloadPosition = payloadBytesRead;

            }

        } else {
            calculatedSignature = digester.doFinal();
            if (!Arrays.equals(tail, calculatedSignature)) {
                throw new SignatureMismatchException();
            } else {
                this.lastValidPayloadPosition = payloadBytesRead;

            }
        }
    }

    protected long readHeader() throws IOException {

        return IO.readLongOptimized(in);
    }

    public long getStreamVersion() {
        return streamVersion;
    }

    protected long readNextPayloadBlockSize() throws IOException {
        return IO.readLongOptimized(in);
    }

    public long getLastValidPayloadPosition() {
        return lastValidPayloadPosition;
    }

    public long getPayloadBytesLoaded() {
        return payloadBytesRead;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.io.InputStream#close()
     */
    @Override
    public void close() throws IOException {
        this.closed = true;
        in.close();

    }

    /*
     * (non-Javadoc)
     *
     * @see java.io.InputStream#skip(long)
     */
    @Override
    public long skip(long n) throws IOException {
        if (n <= 0) {
            return 0;
        }
        int ret;
        long remaining = n;
        byte[] buff = new byte[2000];
        while (n > 0) {
            ret = internalRead(buff, 0, (int) Math.min(buff.length, remaining));
            if (ret < 0) {
                break;
            }
            remaining -= ret;
        }

        return n - remaining;
    }

    public long getBytesLoadedAndValidated() {
        return lastValidPayloadPosition;
    }
};
