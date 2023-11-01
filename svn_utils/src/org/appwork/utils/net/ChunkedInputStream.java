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

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author daniel, ChunkedInputStream, see rfc2616#section-3.6
 *
 */
public class ChunkedInputStream extends InputStream implements StreamValidEOF {
    private final InputStream is;
    private volatile int      nextChunkSize = 0;
    private volatile int      nextChunkLeft = 0;
    private volatile long     completeSize  = 0;
    private volatile boolean  EOF           = false;

    public ChunkedInputStream(final InputStream is) {
        this.is = is;
    }

    @Override
    public int available() throws IOException {
        if (this.nextChunkLeft > 0) {
            return this.nextChunkLeft;
        } else {
            return this.is.available();
        }
    }

    /**
     * returns available bytes in current Chunk or reads next Chunk and parses it
     *
     * @return
     * @throws IOException
     */
    private int availableChunkData() throws IOException {
        if (this.nextChunkLeft == -1) {
            return -1;
        }
        if (this.nextChunkLeft > 0) {
            return this.nextChunkLeft;
        }
        final StringBuilder sb = new StringBuilder();
        boolean chunkExt = false;
        final byte[] b = { (byte) 0x00 };
        int read = 0;
        if (this.nextChunkSize > 0) {
            /* finish LF/CRLF from previous chunk */
            read = this.is.read();
            if (read == 13) {
                read = this.is.read();
            }
        }
        read = this.is.read();
        while (read > -1 && read != 10 && read != 13) {
            if (read == 59) {
                /* ignore chunkExtensions */
                // System.out.println("chunkedExtension found");
                chunkExt = true;
            }
            if (chunkExt == false) {
                b[0] = (byte) (read & 0xFF);
                sb.append(new String(b, 0, 1));
            }
            read = this.is.read();
        }
        if (read == -1 && sb.length() == 0) {
            return -1;
        }
        if (read == 13) {
            /* finish CRLF here */
            read = this.is.read();
        }
        this.nextChunkSize = 0;
        if (sb.length() > 0) {
            try {
                this.nextChunkSize = Integer.parseInt(sb.toString().trim(), 16);
            } catch (NumberFormatException e) {
                throw new IOException(e);
            }
        }
        if (this.nextChunkSize == 0) {
            // System.out.println("lastChunk");
            this.nextChunkLeft = -1;
            this.readTrailers();
        } else {
            // System.out.println("nextchunkSize: " + this.nextChunkSize);
            this.completeSize += this.nextChunkSize;
            this.nextChunkLeft = this.nextChunkSize;
        }
        return this.nextChunkLeft;
    }

    @Override
    public void close() throws IOException {
        this.is.close();
    }

    /**
     * Exhaust an input stream, reading until EOF has been encountered.
     *
     * @throws IOException
     */
    private void exhaustInputStream() throws IOException {
        /* check for final \r\n CRLF of trailers ending */
        int read = -1;
        boolean gotNL = false;
        while ((read = this.is.read()) >= 0) {
            if (gotNL) {
                if (read == 10) {
                    this.EOF = true;
                    return;
                }
                gotNL = false;
            }
            if (read == 13) {
                gotNL = true;
            }
        }
    }

    /**
     * @return the completeSize
     */
    public long getCompleteSize() {
        return this.completeSize;
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    @Override
    public int read() throws IOException {
        final int left = this.availableChunkData();
        if (left > 0) {
            final int ret = this.is.read();
            if (ret != -1) {
                this.nextChunkLeft--;
                return ret;
            } else {
                throw new EOFException("premature EOF");
            }
        } else {
            return -1;
        }
    }

    @Override
    public int read(final byte b[], final int off, final int len) throws IOException {
        final int left = this.availableChunkData();
        if (left > 0) {
            final int ret = this.is.read(b, off, Math.min(left, len));
            if (ret != -1) {
                this.nextChunkLeft -= ret;
                return ret;
            } else {
                throw new EOFException("premature EOF");
            }
        } else {
            return -1;
        }
    }

    /**
     * TODO: read the Trailers we read until EOF at the moment;
     *
     * @throws IOException
     */
    private void readTrailers() throws IOException {
        this.exhaustInputStream();
    }

    @Override
    public boolean isValidEOF() {
        return this.EOF;
    }
}
