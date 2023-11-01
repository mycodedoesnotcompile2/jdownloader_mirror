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
package org.appwork.utils.net;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

import org.appwork.utils.JDK8BufferHelper;

/**
 * @author daniel
 * @date Apr 16, 2019
 *
 */
public class CharSequenceInputStream extends InputStream {
    protected final CharBuffer     cb;
    protected final CharsetEncoder enc;

    public CharBuffer getCharBuffer() {
        return cb;
    }

    public Charset get() {
        return enc.charset();
    }

    protected final ByteBuffer buf;
    private final byte[]       readBuf = new byte[1];

    public CharSequenceInputStream(final CharBuffer charBuffer, final Charset charSet) {
        cb = charBuffer;
        enc = newEncoder(charSet);
        buf = initByteBuffer(cb, enc);
    }

    protected CharsetEncoder newEncoder(final Charset charSet) {
        return charSet.newEncoder();
    }

    public CharSequenceInputStream(final CharSequence charSequence, final Charset charSet) {
        this(CharBuffer.wrap(charSequence), charSet);
    }

    protected ByteBuffer initByteBuffer(final CharBuffer charSequence, final CharsetEncoder encoder) {
        final ByteBuffer ret = ByteBuffer.allocate((int) encoder.averageBytesPerChar() * 16);
        JDK8BufferHelper.flip(ret);
        return ret;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        if (buf.remaining() > 0) {
            len = Math.min(len, buf.remaining());
            buf.get(b, off, len);
            return len;
        } else if (!cb.hasRemaining()) {
            return -1;
        } else {
            final ByteBuffer bb = ByteBuffer.wrap(b, off, len);
            CoderResult cr = enc.encode(cb, bb, !cb.hasRemaining());
            if (bb.position() > 0) {
                return bb.position();
            } else if (cr.isOverflow()) {
                JDK8BufferHelper.clear(buf);
                cr = enc.encode(cb, buf, !cb.hasRemaining());
                if (buf.position() > 0) {
                    JDK8BufferHelper.flip(buf);
                    return read(b, off, len);
                } else {
                    throw new IOException(cr.toString());
                }
            } else {
                throw new IOException(cr.toString());
            }
        }
    }

    @Override
    public int read() throws IOException {
        if (read(readBuf, 0, 1) == -1) {
            return -1;
        } else {
            return readBuf[0];
        }
    }
}
