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
package org.appwork.io.streams.signature;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

/**
 * The stream reads the Inputstream, but ensures that the last X bytes are not piped but stored in the getTail byte array instead
 *
 * @author thomas
 * @date 07.10.2021
 *
 */
public class HoldBackInputStream extends InputStream {
    private byte[] single;
    /**
     * is set if we reach end of stream
     */
    private byte[] tail;

    public byte[] getTail() {
        return tail;
    }

    private int         tailLength;
    private InputStream in;
    private byte[]      readBuffer;
    private long        rawBytesLoaded;
    private long        payloadBytesLoaded;
    private boolean     closed;
    private int         bufferedBytes;
    private int         extraBufferLength;
    private boolean     eof;

    public long getPayloadBytesLoaded() {
        return payloadBytesLoaded;
    }

    /**
     *
     * @param is
     * @param tailLength
     * @param extraBufferLength
     */
    public HoldBackInputStream(InputStream is, int tailLength, int extraBufferLength) {
        this.in = is;
        single = new byte[1];
        this.tailLength = tailLength;
        // extra bytes should be at least 1 to detect EOF. with extrabytes = 0 we would not get an eof, because we would never get -1
        this.extraBufferLength = extraBufferLength;
        // use a tiny buffer to avoid reading very small reads. requests of reading small lengths will use this buffer. if more bytes are
        // requested, we will use the requests byte array and cut the last bytes as trailing buffer
        readBuffer = new byte[128 + tailLength + extraBufferLength];
        rawBytesLoaded = 0l;
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

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        return internalRead(b, off, len);
    }

    protected int internalRead(final byte[] b, final int off, final int len) throws IOException, UnsupportedEncodingException, SignatureMismatchException {
        if (closed) {
            return -1;
        }
        if (tail != null) {
            // we already reached EOF
            return -1;
        }
        if (eof) {
            return onEOF(b, off, len);
        }
        int bytesToHoldBack = tailLength + extraBufferLength;
        // ensure that the buffer contains at least enough bytes to hold the bytesToHoldBack
        if (bufferedBytes < bytesToHoldBack) {
            // fill buffer with trailingbytes length
            int ret = in.read(readBuffer, bufferedBytes, bytesToHoldBack - bufferedBytes);
            if (ret >= 0) {
                rawBytesLoaded += ret;
                bufferedBytes += ret;
            } else {
                throw new EOFException("Expected at least " + (tailLength + 4) + " bytes");
            }
            return 0;
        }
        if (len > readBuffer.length) {
            // b is larger than the readbuffer - it has enough storage to hold bytesToHoldBack AND payload
            // copy buffer to stream
            // totalBuffer is the amount of "rest bytes" we want to hold back. they are in the buffer - the if condition above ensures this
            System.arraycopy(readBuffer, 0, b, off, bytesToHoldBack);
            // try to read the rest
            int read = in.read(b, off + bytesToHoldBack, len - bytesToHoldBack);
            if (read == 0) {
                // nothing read - return 0 because we need to "hold back" bytesToHoldBack . we already copied bytes to b, but this does not
                // matter as long as they stay in the readbuffer
                return 0;
            }
            if (read < 0) {
                return onEOF(b, off, len);
            }
            rawBytesLoaded += read;
            // copy back the tail bytes to the readbuffer to "hold them back"
            System.arraycopy(b, off + read, readBuffer, 0, bytesToHoldBack);
            payloadBytesLoaded += read;
            // we returned only the amount of bytes we actually read. = readFromInputstream + bytesToHoldBack -bytesToHoldBack =
            // readFromInputstream
            return read;
        } else {
            // b is too small to hold bytesToHoldBack AND payload.
            // readBuffer contains the bytesToHoldBack at offset 0. We try to fill the rest with payload
            int read = in.read(readBuffer, bytesToHoldBack, Math.min(len, readBuffer.length - bytesToHoldBack));
            if (read < 0) {
                return onEOF(b, off, len);
            } else if (read > 0) {
                // readbuffer contains [bytesToHoldBack]{payload] now.
                rawBytesLoaded += read;
                // copy payload bytes to b
                System.arraycopy(readBuffer, 0, b, off, read);
                // keep last bytes in buffer and compact it. bytesToHoldBack musst be at offset 0
                System.arraycopy(readBuffer, read, readBuffer, 0, bytesToHoldBack);
                payloadBytesLoaded += read;
                return read;
            } else {
                // read 0
                return read;
            }
        }
    }

    /**
     *
     *
     * @param b
     * @param off
     * @param len
     * @return
     */
    protected int onEOF(final byte[] b, final int off, final int len) {
        eof = true;
        int ret = -1;
        if (bufferedBytes > tailLength) {
            ret = Math.min(bufferedBytes - tailLength, len);
            System.arraycopy(readBuffer, 0, b, off, ret);
            // keep last bytes in buffer and compact it.
            System.arraycopy(readBuffer, ret, readBuffer, 0, tailLength);
            payloadBytesLoaded += ret;
            bufferedBytes -= ret;
            // use else if. The tail is set only if the stream returns -1
        }
        if (tail == null && bufferedBytes <= tailLength) {
            tail = new byte[Math.min(bufferedBytes, tailLength)];
            System.arraycopy(readBuffer, 0, tail, 0, Math.min(bufferedBytes, tailLength));
            bufferedBytes = 0;
        }
        if (tail != null) {
            closed = true;
        }
        return ret;
    }

    public long getRawBytesLoaded() {
        return rawBytesLoaded;
    }

    /**
     *
     *
     * @param expectedSignature
     * @throws IOException
     */
    public void readFromBuffer(byte[] buffer) throws IOException {
        if (bufferedBytes < buffer.length) {
            throw new IOException("Not enough buffered bytes available");
        }
        System.arraycopy(this.readBuffer, 0, buffer, 0, buffer.length);
        if (buffer.length == bufferedBytes) {
            // all bytes invalid. no reason to move anything
            bufferedBytes = 0;
        } else {
            // compact buffer;
            System.arraycopy(readBuffer, buffer.length, readBuffer, 0, buffer.length);
            bufferedBytes -= buffer.length;
        }
    }
}
