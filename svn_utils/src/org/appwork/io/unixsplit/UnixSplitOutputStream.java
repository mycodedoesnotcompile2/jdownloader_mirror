package org.appwork.io.unixsplit;

import java.io.IOException;
import java.io.OutputStream;

import org.appwork.io.unixsplit.StreamProvider.PartInfo;
import org.appwork.utils.net.CountingOutputStream;

/**
 * An OutputStream, that splits a stream in several sequences based on the StreamProvider interface. A new stream will be created, if
 * StreamProvider.getRemaining returns 0. By default, this happens if the stream has reached its max bytesize, but the condition can be
 * overwritten to create a new stream based on e.g. time
 *
 * @author thomas
 * @date 18.07.2021
 *
 */
public class UnixSplitOutputStream extends OutputStream {

    protected final StreamProvider streamProvider;

    private int                    partCount = 0;

    public int getPartCount() {
        return partCount;
    }

    private CountingOutputStream out;
    private PartInfo             stream;

    /**
     * @param streamProvider
     */
    public UnixSplitOutputStream(StreamProvider streamProvider) {
        this.streamProvider = streamProvider;
    }

    @Override
    public void write(final byte b[]) throws IOException {
        write(b, 0, b.length);
    }

    @Override
    public void flush() throws IOException {
        if (out != null) {
            out.flush();
        }
    }

    @Override
    public void write(final byte b[], int off, int len) throws IOException {
        while (len > 0) {
            final long remaining = ensureStream();
            final int chunkSize = Math.min((int) Math.min(Integer.MAX_VALUE, remaining), len);
            out.write(b, off, chunkSize);
            len -= chunkSize;
            off += chunkSize;
        }
    }

    @Override
    public void close() throws IOException {
        if (out != null && stream != null) {
            stream.close();
        }
    }

    protected long ensureStream() throws IOException {
        long remaining;
        if (stream == null || (remaining = stream.getRemaining(out.transferedBytes())) == 0) {
            if (stream != null) {
                stream.close();
            }
            stream = streamProvider.getNextStream(partCount++);
            out = new CountingOutputStream(stream.stream);
            remaining = stream.getRemaining(out.transferedBytes());
            if (remaining <= 0) {
                throw new IOException("Invalid Part Configuration!");
            }
        }
        return remaining;
    }

    @Override
    public void write(final int b) throws IOException {
        ensureStream();
        out.write(b);
    }

}
