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
package org.appwork.utils.net;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author daniel,ChunkedOutputStream, see rfc2616#section-3.6
 *
 */
public class ChunkedOutputStream extends OutputStream {

    private final static byte[] RN      = new byte[] { (byte) '\r', (byte) '\n' };
    private final OutputStream  os;
    private final byte[]        buffer;
    private int                 bufUsed = 0;
    boolean                     closed  = false;

    public ChunkedOutputStream(final OutputStream os) {
        this(os, 4096);
    }

    public ChunkedOutputStream(final OutputStream os, final byte[] buffer) {
        this.os = os;
        this.buffer = buffer;
    }

    public ChunkedOutputStream(final OutputStream os, final int bufSize) {
        this.os = os;
        this.buffer = new byte[bufSize];
    }

    private void _flush(final boolean emptyFlush) throws IOException {
        if (this.closed == false) {
            if (this.bufUsed > 0 || emptyFlush) {
                final byte[] bytes = Integer.toHexString(this.bufUsed).getBytes();
                /* send chunk size */
                this.os.write(bytes);
                this.os.write(RN);
                /* send chunk data */
                if (this.bufUsed > 0 || emptyFlush) {
                    /* flush buffered data if any available */
                    if (this.bufUsed > 0) {
                        this.os.write(this.buffer, 0, this.bufUsed);
                    }
                    this.os.write(RN);
                }
                this.bufUsed = 0;
            }
        }
    }

    @Override
    public synchronized void close() throws IOException {
        if (this.closed == false) {
            this.sendEOF();
            this.closed = true;
        }
        this.os.close();
    }

    @Override
    public synchronized void flush() throws IOException {
        this._flush(false);
        this.os.flush();
    }

    public synchronized void sendEOF() throws IOException {
        /* flush rest available chunk data */
        this._flush(false);
        /* send empty chunk = EOF */
        this._flush(true);
        this.os.flush();
        this.closed = true;
    }

    @Override
    public synchronized void write(final byte b[], final int off, final int len) throws IOException {
        if (len == 0) {
            return;
        }
        if (this.bufUsed + len < this.buffer.length) {
            /* buffer has enough space for len bytes */
            /* fill buffer */
            System.arraycopy(b, off, this.buffer, this.bufUsed, len);
            this.bufUsed += len;
        } else {
            /* buffer has not enough space for len bytes, send as chunk */
            final byte[] bytes = Integer.toHexString(this.bufUsed + len).getBytes();
            /* send chunk size */
            this.os.write(bytes);
            this.os.write(RN);
            /* send chunk data */
            if (this.bufUsed > 0) {
                /* flush buffered data if any available */
                this.os.write(this.buffer, 0, this.bufUsed);
                this.bufUsed = 0;
            }
            this.os.write(b, off, len);
            this.os.write(RN);
        }
    }

    @Override
    public synchronized void write(final int b) throws IOException {
        if (this.bufUsed == this.buffer.length) {
            /* buffer full,send as chunked data */
            final byte[] bytes = Integer.toHexString(this.buffer.length).getBytes();
            /* send chunk size */
            this.os.write(bytes);
            this.os.write(RN);
            /* send buffer as chunk data */
            this.os.write(this.buffer);
            this.os.write(RN);
            this.bufUsed = 0;
        }
        /* fill buffer */
        this.buffer[this.bufUsed++] = (byte) b;
    }
}
