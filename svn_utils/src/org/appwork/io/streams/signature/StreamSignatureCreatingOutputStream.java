package org.appwork.io.streams.signature;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;

import javax.crypto.Mac;

import org.appwork.utils.IO;

/**
 * This outputstream splits the stream in frames: [payload-length][payload][32 bytes HmacSHA256 signature]. The last frame may have less
 * bytes than defined in its frame header, but it also has the signature.
 *
 * @author thomas
 *
 */
public class StreamSignatureCreatingOutputStream extends OutputStream {
    public static final long      STREAM_VERSION_08_10_2021 = 1;
    public static final byte[]    END_SIGNATURE_SALT        = new byte[] { 0x54, 0x68, 0x65, 0x45, 0x6e, 0x64 };
    private final DigestInterface digester;
    private long                  chunkLength;

    public long getChunkLength() {
        return chunkLength;
    }

    public void setChunkLength(long chunkLength) {
        this.chunkLength = chunkLength;
    }

    private long         remainingFrameSize;
    private boolean      signatureRequest;
    private final byte[] nonceBytes;
    private byte[]       lastFrameSignature;
    private long         payloadBytesWritten = 0;

    public long getPayloadBytesWritten() {
        return payloadBytesWritten;
    }

    public long getRawBytesWritten() {
        return rawBytesWritten;
    }

    private long             rawBytesWritten = 0;
    private OutputStream     out;
    private final long       streamVersion;
    private DataOutputStream digesterStream;
    private boolean          closed;

    public StreamSignatureCreatingOutputStream(final OutputStream out, final Mac mac, final byte[] nonceBytes, final int interval) {
        this(out, new MacDigester(mac), nonceBytes, interval);
    }

    public StreamSignatureCreatingOutputStream(final OutputStream out, final MessageDigest digest, final byte[] nonceBytes, final int interval) {
        this(out, new MessageDigester(digest), nonceBytes, interval);
    }

    public StreamSignatureCreatingOutputStream(final OutputStream out, final DigestInterface digester, final byte[] nonceBytes, final int interval) {
        this(out, digester, nonceBytes, interval, STREAM_VERSION_08_10_2021);
    }

    public StreamSignatureCreatingOutputStream(final OutputStream out, final DigestInterface digester, final byte[] nonceBytes, final int interval, long streamVersion) {
        super();
        this.streamVersion = streamVersion;
        this.out = out;
        signatureRequest = false;
        this.nonceBytes = nonceBytes;
        this.chunkLength = interval;
        this.digester = digester;
        digesterStream = new DataOutputStream(new DigesterOutputStream(digester));
    }

    public long getStreamVersion() {
        return streamVersion;
    }

    protected void initChunkDigester(long chunkSize) throws IOException {
        // ensure reset
        digester.doFinal();
        initChunkDigesterStatic(digesterStream, streamVersion, chunkSize, nonceBytes, lastFrameSignature);

    }

    /**
     * @param digesterStream
     * @param streamVersion2
     * @param chunkSize
     * @param nonceBytes2
     * @param lastFrameSignature2
     * @throws IOException
     */
    public static void initChunkDigesterStatic(DataOutputStream digesterStream, long streamVersion, long chunkSize, byte[] nonceBytes, byte[] lastFrameSignature) throws IOException {
        digesterStream.writeLong(streamVersion);
        digesterStream.writeLong(chunkSize);
        if (nonceBytes != null) {
            digesterStream.write(nonceBytes);
        }

        if (lastFrameSignature != null) {
            digesterStream.write(lastFrameSignature);
        }

    }

    @Override
    public void write(final byte[] b) throws IOException {
        internalWrite(b, 0, b.length);
    }

    @Override
    public void write(final byte[] b, int off, int len) throws IOException {
        internalWrite(b, off, len);
    }

    protected void internalWrite(final byte[] b, int off, int len) throws IOException, UnsupportedEncodingException {
        if (closed) {
            throw new IOException("Stream is closed");
        }
        while (true) {

            if (payloadBytesWritten == 0) {
                rawBytesWritten += IO.writeLongOptimized(getStreamVersion(), out);
            }
            if (signatureRequest) {
                writeSignature();
            }
            if (remainingFrameSize == 0) {
                remainingFrameSize = nextChunk();
                writeNextChunkSize(remainingFrameSize);
                initChunkDigester(remainingFrameSize);
            }
            final int toWrite = (int) Math.min(remainingFrameSize, len);
            writeToOut(b, off, toWrite);
            payloadBytesWritten += toWrite;

            digesterStream.write(b, off, toWrite);
            remainingFrameSize -= toWrite;
            if (remainingFrameSize == 0) {
                signatureRequest = true;
            }
            off += toWrite;
            len -= toWrite;
            if (len == 0) {
                break;
            }
        }
    }

    protected void writeToOut(final byte[] b, int off, final int toWrite) throws IOException {
        out.write(b, off, toWrite);
        rawBytesWritten += toWrite;
    }

    protected void writeSignature() throws IOException {
        lastFrameSignature = digester.doFinal();

        writeToOut(lastFrameSignature, 0, lastFrameSignature.length);

        signatureRequest = false;

    }

    protected long nextChunk() {

        return chunkLength;
    }

    private void writeNextChunkSize(final long v) throws IOException {

        rawBytesWritten += IO.writeLongOptimized(v, out);

    }

    public long getInterval() {
        return chunkLength;
    }

    @Override
    public void close() throws IOException {
        if (closed) {
            return;
        }
        writeEndSignature();
        out.close();
        closed = true;
    }

    protected void writeEndSignature() throws IOException {
        if (rawBytesWritten == 0) {
            write(new byte[0]);
        }
        digesterStream.write(END_SIGNATURE_SALT);
        writeSignature();

    }

    /*
     * (non-Javadoc)
     *
     * @see java.io.OutputStream#flush()
     */
    @Override
    public void flush() throws IOException {
        out.flush();
    }

    @Override
    public void write(final int b) throws IOException {
        internalWrite(new byte[] { (byte) b }, 0, 1);
    }
}
