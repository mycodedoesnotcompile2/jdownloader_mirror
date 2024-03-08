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
package org.appwork.storage.flexijson;

import java.io.IOException;
import java.io.Reader;

/**
 * @author daniel
 * @date Dec 3, 2021
 *
 */
public class CharSequenceReader extends Reader {
    private CharSequence str;
    private int          length;
    private int          next = 0;
    private int          mark = 0;

    /**
     * Creates a new string reader.
     *
     * @param s
     *            String providing the character stream.
     */
    public CharSequenceReader(CharSequence s) {
        this.str = s;
        this.length = s.length();
    }

    /** Check to make sure that the stream has not been closed */
    private CharSequence ensureOpen() throws IOException {
        final CharSequence ret = this.str;
        if (ret == null) {
            throw new IOException("Stream closed");
        } else {
            return ret;
        }
    }

    /**
     * Reads a single character.
     *
     * @return The character read, or -1 if the end of the stream has been reached
     *
     * @exception IOException
     *                If an I/O error occurs
     */
    public int read() throws IOException {
        synchronized (lock) {
            final CharSequence str = ensureOpen();
            if (next >= length) {
                return -1;
            } else {
                return str.charAt(next++);
            }
        }
    }

    /**
     * Reads characters into a portion of an array.
     *
     * @param cbuf
     *            Destination buffer
     * @param off
     *            Offset at which to start writing characters
     * @param len
     *            Maximum number of characters to read
     *
     * @return The number of characters read, or -1 if the end of the stream has been reached
     *
     * @exception IOException
     *                If an I/O error occurs
     */
    public int read(char cbuf[], int off, int len) throws IOException {
        synchronized (lock) {
            final CharSequence str = ensureOpen();
            if ((off < 0) || (off > cbuf.length) || (len < 0) || ((off + len) > cbuf.length) || ((off + len) < 0)) {
                throw new IndexOutOfBoundsException();
            } else if (len == 0) {
                return 0;
            } else if (next >= length) {
                return -1;
            } else {
                int n = Math.min(length - next, len);
                for (int copy = 0; copy < n; copy++) {
                    cbuf[off + copy] = str.charAt(next + copy);
                }
                next += n;
                return n;
            }
        }
    }

    /**
     * Skips the specified number of characters in the stream. Returns the number of characters that were skipped.
     *
     * <p>
     * The <code>ns</code> parameter may be negative, even though the <code>skip</code> method of the {@link Reader} superclass throws an
     * exception in this case. Negative values of <code>ns</code> cause the stream to skip backwards. Negative return values indicate a skip
     * backwards. It is not possible to skip backwards past the beginning of the string.
     *
     * <p>
     * If the entire string has been read or skipped, then this method has no effect and always returns 0.
     *
     * @exception IOException
     *                If an I/O error occurs
     */
    public long skip(long ns) throws IOException {
        synchronized (lock) {
            final CharSequence str = ensureOpen();
            if (next >= length) {
                return 0;
            }
            // Bound skip by beginning and end of the source
            long n = Math.min(length - next, ns);
            n = Math.max(-next, n);
            next += n;
            return n;
        }
    }

    /**
     * Tells whether this stream is ready to be read.
     *
     * @return True if the next read() is guaranteed not to block for input
     *
     * @exception IOException
     *                If the stream is closed
     */
    public boolean ready() throws IOException {
        synchronized (lock) {
            ensureOpen();
            return true;
        }
    }

    /**
     * Tells whether this stream supports the mark() operation, which it does.
     */
    public boolean markSupported() {
        return true;
    }

    /**
     * Marks the present position in the stream. Subsequent calls to reset() will reposition the stream to this point.
     *
     * @param readAheadLimit
     *            Limit on the number of characters that may be read while still preserving the mark. Because the stream's input comes from
     *            a string, there is no actual limit, so this argument must not be negative, but is otherwise ignored.
     *
     * @exception IllegalArgumentException
     *                If {@code readAheadLimit < 0}
     * @exception IOException
     *                If an I/O error occurs
     */
    public void mark(int readAheadLimit) throws IOException {
        if (readAheadLimit < 0) {
            throw new IllegalArgumentException("Read-ahead limit < 0");
        }
        synchronized (lock) {
            ensureOpen();
            mark = next;
        }
    }

    /**
     * Resets the stream to the most recent mark, or to the beginning of the string if it has never been marked.
     *
     * @exception IOException
     *                If an I/O error occurs
     */
    public void reset() throws IOException {
        synchronized (lock) {
            ensureOpen();
            next = mark;
        }
    }

    /**
     * Closes the stream and releases any system resources associated with it. Once the stream has been closed, further read(), ready(),
     * mark(), or reset() invocations will throw an IOException. Closing a previously closed stream has no effect.
     */
    public void close() {
        str = null;
    }
}