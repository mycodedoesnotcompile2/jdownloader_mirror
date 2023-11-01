package org.appwork.utils.net;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class SkippingLimitedOutputStream extends FilterOutputStream {
    private final long skip;
    private final long limit;
    protected long     position = 0;

    public SkippingLimitedOutputStream(final OutputStream out, final long skip) {
        this(out, skip, -1);
    }

    public SkippingLimitedOutputStream(final OutputStream out, final long skip, final long limit) {
        super(out);
        this.skip = Math.max(0, skip);
        if (limit < 0) {
            this.limit = -1;
        } else {
            this.limit = Math.max(this.skip, limit);
        }
    }

    public long getSkipLeft() {
        return Math.max(0, this.skip - this.position);
    }

    public boolean isSkipping() {
        return this.position < this.skip;
    }

    public long getLimitLeft() {
        if (this.limit == -1) {
            return Long.MAX_VALUE;
        } else {
            return Math.max(0, this.limit - this.position);
        }
    }

    public boolean isLimitReached() {
        return this.limit != -1 && this.position >= this.limit;
    }

    @Override
    public void write(int b) throws IOException {
        final byte[] array = new byte[1];
        array[0] = (byte) b;
        this.write(array, 0, 1);
    }

    public void simulateWrite(long len) throws IOException {
        this.position += Math.max(0, len);
    }

    protected int writeSkip(byte b[], int off, int len) throws IOException {
        final long skipLeft = this.getSkipLeft();
        if (skipLeft > 0) {
            if (skipLeft >= len) {
                this.simulateWrite(len);
                return len;
            } else {
                final int skip = (int) skipLeft;
                this.simulateWrite(skip);
                return skip;
            }
        } else {
            return 0;
        }
    }

    protected int writeUnlimited(byte b[], int off, int len) throws IOException {
        this.out.write(b, off, len);
        this.position += len;
        return len;
    }

    protected int writeLimited(byte b[], int off, int len) throws IOException {
        final long limitLeft = this.getLimitLeft();
        if (limitLeft > 0) {
            if (limitLeft >= len) {
                return this.writeUnlimited(b, off, len);
            } else {
                return this.writeUnlimited(b, off, (int) limitLeft);
            }
        } else {
            return 0;
        }
    }

    @Override
    public void write(byte b[], int off, int len) throws IOException {
        if (len > 0) {
            final int skipped = this.writeSkip(b, off, len);
            if (skipped != len) {
                this.writeLimited(b, skipped, len - skipped);
            }
        }
    }
}
