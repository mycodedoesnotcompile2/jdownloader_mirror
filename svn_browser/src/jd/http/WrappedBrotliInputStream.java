package jd.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;

import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.CountingInputStreamInterface;
import org.brotli.dec.BrotliInputStream;

public class WrappedBrotliInputStream extends InputStream implements CountingInputStreamInterface {
    protected volatile BrotliInputStream   bis = null;
    private final InputStream              isParent;
    protected final PushbackInputStream    is;
    protected volatile CountingInputStream cis = null;

    public WrappedBrotliInputStream(final InputStream is) {
        this.isParent = is;
        this.is = new PushbackInputStream(this.isParent, 32);
    }

    private synchronized void initializeBrotliInputStream() throws IOException {
        if (this.bis == null) {
            this.bis = new BrotliInputStream(this.cis = new CountingInputStream(this.is));
        }
    }

    @Override
    public InputStream getInputStream() {
        return this;
    }

    @Override
    public synchronized int read(final byte[] b, final int off, final int len) throws IOException {
        if (this.bis == null) {
            final int read = this.is.read();
            if (read == -1) {
                return -1;
            }
            this.is.unread(read);
            this.initializeBrotliInputStream();
        }
        return this.bis.read(b, off, len);
    }

    @Override
    public synchronized int read() throws IOException {
        if (this.bis == null) {
            final int read = this.is.read();
            if (read == -1) {
                return -1;
            }
            this.is.unread(read);
            this.initializeBrotliInputStream();
        }
        return this.bis.read();
    }

    @Override
    public void close() throws IOException {
        this.bis.close();
    }

    @Override
    public int available() throws IOException {
        if (this.bis != null) {
            return this.bis.available();
        } else {
            return this.is.available();
        }
    }

    @Override
    public long transferedBytes() {
        final CountingInputStream cis = this.cis;
        if (cis == null) {
            return 0;
        } else {
            return cis.transferedBytes();
        }
    }

    @Override
    public InputStream getParentInputStream() {
        return this.isParent;
    }
}
