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
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;

import org.appwork.utils.JDK8BufferHelper;
import org.appwork.utils.ReusableByteArrayOutputStream;

/**
 * @author daniel
 * @date 18.10.2018
 *
 */
public class LineParsingOutputStream extends OutputStream {
    public enum NEWLINE {
        CR,
        LF
    }

    protected final ReusableByteArrayOutputStream bos         = new ReusableByteArrayOutputStream(4096);
    protected boolean                             closed;
    protected final Charset                       charset;
    protected NEWLINE                             lastNewLine = null;
    protected final CharsetDecoder                decoder;
    protected final StringBuilder                 sb;
    protected int                                 lastIndex   = 0;
    protected final CharBuffer                    cb;
    protected long                                lines       = 0;

    public long getLines() {
        return lines;
    }

    public LineParsingOutputStream(Charset charset, int bufferSize) {
        if (charset == null) {
            throw new IllegalArgumentException("charset is null!");
        }
        this.charset = charset;
        this.decoder = this.charset.newDecoder();
        this.sb = new StringBuilder(bufferSize);
        this.cb = CharBuffer.wrap(new char[bufferSize]);
    }

    public LineParsingOutputStream(Charset charset) {
        this(charset, 4096);
    }

    @Override
    public synchronized void write(int b) throws IOException {
        if (!closed) {
            bos.write(b);
            this.forwardLinesToSink();
        } else {
            throw new IOException("stream is closed");
        }
    }

    @Override
    public synchronized void close() throws IOException {
        if (!closed) {
            closed = true;
            this.forwardLinesToSink();
        }
    }

    @Override
    public synchronized void write(byte[] b, int off, int len) throws IOException {
        if (!closed) {
            bos.write(b, off, len);
            this.forwardLinesToSink();
        } else {
            throw new IOException("stream is closed");
        }
    }

    protected final int forwardLinesToSink() throws IOException {
        final ByteBuffer bb = ByteBuffer.wrap(bos.getInternalBuffer(), 0, bos.size());
        while (true) {
            final CoderResult result = decoder.decode(bb, cb, closed);
            if (cb.position() > 0) {
                sb.append(cb.array(), 0, cb.position());
                JDK8BufferHelper.clear(cb);
            }
            if (result.isError()) {
                sb.append(decoder.replacement());
                JDK8BufferHelper.position(bb, bb.position() + result.length());
                continue;
            } else if (result.isOverflow()) {
                continue;
            } else if (result.isUnderflow()) {
                break;
            } else {
                System.out.println(result);
                break;
            }
        }
        if (bb.remaining() == 0) {
            bos.reset();
        } else if (bb.position() > 0) {
            final int position = bb.position();
            final int remaining = bb.remaining();
            System.arraycopy(bos.getInternalBuffer(), position, bos.getInternalBuffer(), 0, remaining);
            bos.setUsed(remaining);
        }
        int lines = 0;
        if (sb.length() > 0) {
            int removeIndex = -1;
            for (int index = lastIndex; index < sb.length(); index++) {
                final char c = sb.charAt(index);
                if (c == '\r') {
                    // \r
                    lastNewLine = NEWLINE.CR;
                    onNextLine(lastNewLine, lines, sb, lastIndex, index);
                    lines++;
                    removeIndex = index + 1;
                    lastIndex = index + 1;
                } else if (c == '\n') {
                    if (lastNewLine == NEWLINE.CR) {
                        // \r\n
                        lastNewLine = NEWLINE.LF;
                    } else {
                        // \n
                        lastNewLine = NEWLINE.LF;
                        onNextLine(lastNewLine, lines, sb, lastIndex, index);
                        lines++;
                    }
                    removeIndex = index + 1;
                    lastIndex = index + 1;
                } else {
                    lastNewLine = null;
                }
            }
            if (removeIndex > 0) {
                sb.delete(0, removeIndex);
                lastIndex = 0;
            }

            if (sb.length() > 0 && closed) {
                onNextLine(null, lines, sb, 0, sb.length());
                lines++;
                sb.delete(0, sb.length());
            }

            int index;
            while (sb.length() > 0 && (index = split(sb)) > 0) {
                onNextLine(null, lines, sb, 0, index);
                lines++;
                sb.delete(0, index);
            }

            this.lines += lines;
        }
        return lines;
    }

    /**
     * @param sb2
     * @return
     */
    protected int split(StringBuilder sb) {        
        return 0;
    }

    protected void onNextLine(NEWLINE newLine, long line, StringBuilder sb, int startIndex, int endIndex) {
    }
}
