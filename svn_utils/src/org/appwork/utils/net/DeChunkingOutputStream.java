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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author daniel
 *
 */
public class DeChunkingOutputStream extends OutputStream {

    public static void main(final String[] args) throws Throwable {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final ChunkedOutputStream cos = new ChunkedOutputStream(new DeChunkingOutputStream(bos), 0);
        cos.write("Hello ".getBytes("UTF-8"));
        cos.write("H".getBytes("UTF-8"));
        cos.write("ell".getBytes("UTF-8"));
        cos.write("o! ".getBytes("UTF-8"));
        cos.write("Hello This is a simple Test".getBytes("UTF-8"));
        cos.close();
        System.out.println("Output: " + new String(bos.toByteArray(), "UTF-8"));
    }

    protected final OutputStream os;
    protected int                nextExpectedChunkLeft = 0;
    protected byte[]             chunkSize             = new byte[8];
    protected int                chunkSizePosition     = 0;
    protected boolean            chunkedExtension      = false;
    protected boolean            chunkedTrailers       = false;
    private int                  lastWrite             = -1;

    public DeChunkingOutputStream(final OutputStream os) {
        this.os = os;
    }

    @Override
    public void close() throws IOException {
        if (this.nextExpectedChunkLeft > 0) {
            throw new IOException("malformed chunk, still " + this.nextExpectedChunkLeft + " bytes expected!");
        }
        this.os.close();
    }

    @Override
    public void flush() throws IOException {
        this.os.flush();
    }

    protected int nextChunkInfoProcessed(final int b) throws IOException {
        if (this.nextExpectedChunkLeft > 0) {
            return this.nextExpectedChunkLeft;
        }
        if (this.lastWrite == 13 || this.lastWrite == 10) {
            /* finish of LF/CRLF */
            final boolean returnChunkSize = this.lastWrite == 10;
            if (this.lastWrite == 13 && b != 10) {
                throw new IOException("malformed chunk, bad/missing LF/CRLF");
            }
            this.lastWrite = -1;
            this.chunkedExtension = false;
            if (this.chunkSizePosition > 0) {
                /* we can parse nextExpectedChunkLeft */
                final String size = new String(this.chunkSize, 0, this.chunkSizePosition, "UTF-8");
                this.nextExpectedChunkLeft = Integer.parseInt(size.toString().trim(), 16);
                this.chunkSizePosition = 0;
                // System.out.println("Next ChunkSize: " +
                // this.nextExpectedChunkLeft);
                if (this.nextExpectedChunkLeft == 0) {
                    this.chunkedTrailers = true;
                }
            }
            if (returnChunkSize) {
                return this.nextExpectedChunkLeft;
            } else {
                return 0;
            }
        }
        if (b != 10 && b != 13) {
            /* no LF/CRLF */
            if (b == 59) {
                /* chunkedExtension found */
                this.chunkedExtension = true;
            } else {
                if (this.chunkedExtension == false) {
                    /* no chunkedExtension byte, we can add it to our chunkSize */
                    this.chunkSize[this.chunkSizePosition++] = (byte) b;
                }
            }
        } else {
            this.lastWrite = b;
        }
        return this.nextExpectedChunkLeft;
    }

    @Override
    public void write(final byte b[], final int off, final int len) throws IOException {
        if (this.chunkedTrailers) {
            /* we no longer have data to write, only Trailers are following */
            return;
        }
        if (this.nextExpectedChunkLeft >= len) {
            /* we can still write len bytes because they belong to expectedChunk */
            this.os.write(b, off, len);
            /* reduce nextExpectedChunkLeft by len */
            this.nextExpectedChunkLeft -= len;
        } else {
            /* we have to write in multiple steps */
            int rest = len;
            int done = 0;
            int next = 0;
            while (rest > 0 && this.chunkedTrailers == false) {
                next = Math.min(this.nextExpectedChunkLeft, rest);
                if (next > 0) {
                    /* we can write up to next bytes */
                    this.os.write(b, off + done, next);
                    this.nextExpectedChunkLeft -= next;
                    rest -= next;
                    done += next;
                } else {
                    final int temp = b[off + done] & 0xff;
                    rest -= 1;
                    done += 1;
                    this.nextChunkInfoProcessed(temp);
                }
            }
        }
    }

    @Override
    public void write(final int b) throws IOException {
        if (this.chunkedTrailers) {
            /* we no longer have data to write, only Trailers are following */
            return;
        }
        if (this.nextChunkInfoProcessed(b) > 0) {
            /* we can still write 1 byte because it belongs to expectedChunk */
            this.os.write(b);
            /* reduce nextExpectedChunkLeft by 1 */
            this.nextExpectedChunkLeft--;
        }
    }

}
