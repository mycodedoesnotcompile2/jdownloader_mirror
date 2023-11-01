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
package org.appwork.utils.net.usenet;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class UUInputStream extends InputStream {
    /**
     * https://en.wikipedia.org/wiki/Uuencoding
     */
    private final InputStream           inputStream;
    private final ByteArrayOutputStream buffer;
    private final String                name;
    private final SimpleUseNet          client;

    protected UUInputStream(SimpleUseNet client, ByteArrayOutputStream buffer) throws IOException {
        this.client = client;
        this.inputStream = client.getInputStream();
        this.buffer = buffer;
        String line = new String(buffer.toByteArray(), 0, buffer.size(), client.getCharSet());
        if (!line.matches("^begin \\d{3} .+")) {
            throw new IOException("missing uuEncode begin");
        }
        name = line.replaceFirst("begin \\d{3} ", "");
        readNextLine();
    }

    private int     lineSize     = 0;
    private byte    lineBuffer[] = null;
    private boolean eof          = false;
    private int     dataIndex    = 0;
    private int     dataLength   = -1;
    private String  trailer      = null;

    private void readNextLine() throws IOException {
        buffer.reset();
        lineSize = client.readLine(inputStream, buffer);
        lineBuffer = buffer.toByteArray();
        if (lineSize == 1) {
            if (lineBuffer[0] == (byte) 96) {
                eof = true;
                parseTrailer(inputStream);
                return;
            } else {
                throw new IOException("unexpected single byte line");
            }
        }
        dataIndex = 0;
        dataLength = (lineBuffer[0] & 0xff) - 32;
        int writeIndex = 0;
        int readIndex = 1;
        while (true) {
            final int encodedLeft = dataLength - writeIndex;
            if (encodedLeft <= 0) {
                break;
            }
            if (encodedLeft >= 3) {
                final int a = lineBuffer[readIndex] & 0xff;
                final int b = lineBuffer[readIndex + 1] & 0xff;
                final int c = lineBuffer[readIndex + 2] & 0xff;
                final int d = lineBuffer[readIndex + 3] & 0xff;
                final int x = (((a - 32) & 63) << 2) | (((b - 32) & 63) >> 4);
                final int y = (((b - 32) & 63) << 4) | (((c - 32) & 63) >> 2);
                final int z = (((c - 32) & 63) << 6) | (((d - 32) & 63));
                lineBuffer[writeIndex++] = (byte) x;
                lineBuffer[writeIndex++] = (byte) y;
                lineBuffer[writeIndex++] = (byte) z;
            } else {
                if (encodedLeft >= 1) {
                    final int a = lineBuffer[readIndex] & 0xff;
                    final int b = lineBuffer[readIndex + 1] & 0xff;
                    final int x = (((a - 32) & 63) << 2) | (((b - 32) & 63) >> 4);
                    lineBuffer[writeIndex++] = (byte) x;
                }
                if (encodedLeft >= 2) {
                    final int b = lineBuffer[readIndex + 1] & 0xff;
                    final int c = lineBuffer[readIndex + 2] & 0xff;
                    final int y = (((b - 32) & 63) << 4) | (((c - 32) & 63) >> 2);
                    lineBuffer[writeIndex++] = (byte) y;
                }
            }
            readIndex += 4;
        }
    }

    /**
     * TODO: optimize to use larger reads (readline works with read()) and support read(byte[] b, int off, int len)
     */
    @Override
    public synchronized int read() throws IOException {
        if (eof) {
            return -1;
        } else if (dataIndex == dataLength) {
            readNextLine();
            if (eof) {
                return -1;
            }
        }
        final int c = lineBuffer[dataIndex++] & 0xff;
        return c;
    }

    /**
     * TODO: optimize to use larger reads for underlying inputstream
     *
     * @param b
     * @param off
     * @param len
     * @return
     * @throws IOException
     */
    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        if (eof) {
            return -1;
        } else if (dataIndex == dataLength) {
            readNextLine();
            if (eof) {
                return -1;
            }
        }
        int written = 0;
        while (dataIndex < dataLength && written < len) {
            b[off + written++] = (byte) (lineBuffer[dataIndex++] & 0xff);
        }
        return written;
    }

    private void parseTrailer(final InputStream inputStream) throws IOException {
        buffer.reset();
        client.readLine(inputStream, buffer);
        trailer = new String(buffer.toByteArray(), 0, buffer.size(), client.getCharSet());
        // read body to end to drain inputstream
        readBodyEnd(inputStream);
        // error checks
        if (!"end".equals(trailer)) {
            throw new IOException("missing body termination(end): " + trailer);
        }
    }

    public String getTrailer() {
        return trailer;
    }

    private void readBodyEnd(final InputStream is) throws IOException {
        final BodyInputStream bodyInputStream = new BodyInputStream(is);
        final byte[] bodyEndBuf = new byte[32];
        while (true) {
            if (bodyInputStream.read(bodyEndBuf) == -1) {
                bodyInputStream.close();
                break;
            }
        }
    }

    @Override
    public void close() throws IOException {
    }

    /**
     * returns the name of the original file
     *
     * @return
     */
    public String getName() {
        return name;
    }
}
