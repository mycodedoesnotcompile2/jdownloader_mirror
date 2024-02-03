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
import java.io.InterruptedIOException;
import java.io.OutputStream;

import org.appwork.utils.Exceptions;

/**
 * @author daniel
 *
 */
public class ThrottledOutputStream extends OutputStream implements ThrottledConnection {
    private ThrottledConnectionHandler handler;
    private OutputStream               out;
    protected volatile long            transferedCounter  = 0;
    protected volatile long            transferedCounter2 = 0;
    private volatile int               limitCurrent       = 0;
    private int                        limitCounter       = 0;
    private int                        offset;
    private int                        todo;
    private int                        rest;
    private long                       slotTimeLeft       = 0;
    private long                       lastTimeReset      = 0;
    private final long                 onems              = 1000000l;
    private final long                 onesec             = 1000000000l;

    /**
     * constructor for not managed ThrottledOutputStream
     *
     * @param in
     */
    public ThrottledOutputStream(final OutputStream out) {
        this.out = out;
    }

    /**
     * DO NOT FORGET TO CLOSE
     */
    @Override
    public void close() throws IOException {
        /* remove this stream from manager */
        if (this.handler != null) {
            this.handler.removeThrottledConnection(this);
            this.handler = null;
        }
        synchronized (this) {
            this.notify();
        }
        this.out.close();
    }

    @Override
    public void flush() throws IOException {
        this.out.flush();
    }

    @Override
    public ThrottledConnectionHandler getHandler() {
        return this.handler;
    }

    @Override
    public int getLimit() {
        return this.limitCurrent;
    }

    public OutputStream getOutputStream() {
        return this.out;
    }

    /**
     * set a new ThrottledConnectionManager
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

    /**
     * sets limit 0: no limit >0: use limit
     *
     * @param kpsLimit
     */
    public void setLimit(final int kpsLimit) {
        if (kpsLimit == this.limitCurrent) {
            return;
        }
        /* TODO: maybe allow little jitter here */
        this.limitCurrent = Math.max(0, kpsLimit);
    }

    public void setOutputStream(final OutputStream os) {
        if (os == null) {
            throw new IllegalArgumentException("Outputstream is null");
        } else if (os == this) {
            throw new IllegalArgumentException("Outputstream loop!");
        } else {
            this.out = os;
        }
    }

    @Override
    public long transfered() {
        return this.transferedCounter;
    }

    @Override
    public void write(final byte b[], final int off, final int len) throws IOException {
        if (this.limitCurrent == 0) {
            /* no limit is set */
            this.out.write(b, off, len);
            this.transferedCounter += len;
        } else {
            /* a limit is set */
            this.offset = off;
            this.rest = len;
            while (this.rest > 0) {
                /* loop until all data is written */
                this.writeWait(this.rest);
                this.todo = Math.min(this.limitCounter, this.rest);
                this.out.write(b, this.offset, this.todo);
                this.offset += this.todo;
                this.rest -= this.todo;
                this.transferedCounter += this.todo;
                this.limitCounter -= this.todo;
            }
        }
    }

    /**
     * WARNING: this function has a huge overhead
     */
    @Override
    public void write(final int b) throws IOException {
        this.out.write(b);
        this.transferedCounter++;
        if (this.limitCurrent != 0) {
            /* a Limit is set */
            this.limitCounter--;
            this.writeWait(1);
        }
    }

    private final void writeWait(final int len) throws IOException {
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
}
