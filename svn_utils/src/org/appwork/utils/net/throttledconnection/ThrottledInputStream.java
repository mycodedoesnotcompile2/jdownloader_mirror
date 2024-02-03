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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.throttledconnection;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;

import org.appwork.utils.Exceptions;

/**
 * @author daniel
 *
 */
public class ThrottledInputStream extends InputStream implements ThrottledConnection {
    private ThrottledConnectionHandler handler;
    private InputStream                in;
    protected volatile long            transferedCounter  = 0;
    protected volatile long            transferedCounter2 = 0;
    private volatile int               limitCurrent       = 0;
    private int                        limitCounter       = 0;
    private int                        lastRead2;
    private long                       slotTimeLeft       = 0;
    private long                       lastTimeReset      = 0;
    private final long                 onems              = 1000000l;
    private final long                 onesec             = 1000000000l;

    /**
     * constructor for not managed ThrottledInputStream
     *
     * @param in
     */
    public ThrottledInputStream(final InputStream in) {
        this.in = in;
    }

    @Override
    public int available() throws IOException {
        return this.in.available();
    }

    /**
     * DO NOT FORGET TO CLOSE
     */
    @Override
    public void close() throws IOException {
        /* remove this stream from handler */
        if (this.handler != null) {
            this.handler.removeThrottledConnection(this);
            this.handler = null;
        }
        synchronized (this) {
            this.notify();
        }
        this.in.close();
    }

    @Override
    public ThrottledConnectionHandler getHandler() {
        return this.handler;
    }

    public InputStream getInputStream() {
        return this.in;
    }

    @Override
    public int getLimit() {
        return this.limitCurrent;
    }

    @Override
    public synchronized void mark(final int readlimit) {
        this.in.mark(readlimit);
    }

    @Override
    public boolean markSupported() {
        return this.in.markSupported();
    }

    /**
     * WARNING: this function has a huge overhead
     */
    @Override
    public int read() throws IOException {
        this.lastRead2 = this.in.read();
        if (this.lastRead2 == -1) {
            /* end of line */
            return -1;
        }
        this.transferedCounter++;
        if (this.limitCurrent != 0) {
            /* a Limit is set */
            this.limitCounter--;
            /* a Limit is set */
            this.readWait(1);
        }
        return this.lastRead2;
    }

    @Override
    public int read(final byte b[], final int off, final int len) throws IOException {
        if (this.limitCurrent == 0) {
            this.lastRead2 = this.in.read(b, off, len);
            if (this.lastRead2 == -1) {
                /* end of line */
                return -1;
            }
            this.transferedCounter += this.lastRead2;
        } else {
            this.readWait(len);
            this.lastRead2 = this.in.read(b, off, Math.min(this.limitCounter, len));
            if (this.lastRead2 == -1) {
                /* end of line */
                return -1;
            }
            this.transferedCounter += this.lastRead2;
            this.limitCounter -= this.lastRead2;
        }
        return this.lastRead2;
    }

    private final void readWait(final int len) throws IOException {
        /* a Limit is set */
        final long current = System.nanoTime();
        this.slotTimeLeft = Math.max(0, current - this.lastTimeReset);
        if (this.limitCounter <= 0 && this.slotTimeLeft < this.onesec) {
            /* Limit reached and slotTime not over yet */
            synchronized (this) {
                try {
                    long wait = this.onesec - this.slotTimeLeft;
                    this.lastTimeReset = current + wait;
                    final long ns = wait % this.onems;
                    wait = wait / this.onems;
                    this.wait(wait, (int) ns);
                } catch (final InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw Exceptions.initCause(new InterruptedIOException("throttle interrupted"), e);
                }
            }
            /* refill Limit */
            this.limitCounter = this.limitCurrent;
            if (this.limitCounter <= 0) {
                this.limitCounter = len;
            }
        } else if (this.slotTimeLeft >= this.onesec) {
            /* slotTime is over, refill Limit too */
            this.limitCounter = this.limitCurrent;
            this.lastTimeReset = current;
            if (this.limitCounter <= 0) {
                this.limitCounter = len;
            }
        }
    }

    @Override
    public synchronized void reset() throws IOException {
        this.in.reset();
    }

    /**
     * set a new ThrottledConnectionHandler
     *
     * @param manager
     */
    public void setHandler(final ThrottledConnectionHandler manager) {
        if (this.handler != null && this.handler != manager) {
            this.handler.removeThrottledConnection(this);
        }
        this.handler = manager;
        if (this.handler != null) {
            this.handler.addThrottledConnection(this);
        }
    }

    public void setInputStream(final InputStream is) {
        if (is == null) {
            throw new IllegalArgumentException("InputStream is null");
        } else if (is == this) {
            throw new IllegalArgumentException("InputStream loop!");
        } else {
            this.in = is;
        }
    }

    /**
     * sets limit 0: no limit >0: use limit
     *
     * @param bytes
     *            /S
     */
    public void setLimit(final int kpsLimit) {
        if (kpsLimit == this.limitCurrent) {
            return;
        }
        /* TODO: maybe allow little jitter here */
        this.limitCurrent = Math.max(0, kpsLimit);
    }

    @Override
    public long skip(final long n) throws IOException {
        return this.in.skip(n);
    }

    public long transfered() {
        return this.transferedCounter;
    }
}