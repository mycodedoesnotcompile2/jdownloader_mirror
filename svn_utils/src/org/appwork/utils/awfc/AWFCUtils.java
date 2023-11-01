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
package org.appwork.utils.awfc;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;

/**
 * @author daniel
 *
 */
public class AWFCUtils {
    private final byte[]       buffer = new byte[16];
    private final OutputStream os;
    private final InputStream  is;

    public AWFCUtils() {
        this.is = null;
        this.os = null;
    }

    public AWFCUtils(final InputStream is) {
        this.is = is;
        this.os = null;
    }

    public AWFCUtils(final OutputStream os) {
        this.os = os;
        this.is = null;
    }

    public int ensureRead() throws IOException {
        final int read = this.getCurrentInputStream().read();
        if (read == -1) {
            throw new EOFException();
        }
        return read;
    }

    public byte[] ensureRead(final int size, final byte[] buffer) throws IOException {
        byte[] stringBytes = buffer;
        if (stringBytes == null) {
            stringBytes = new byte[size];
        }
        if (size > stringBytes.length) {
            throw new IOException("buffer too small");
        }
        int done = 0;
        int read = 0;
        while (done < size && (read = this.getCurrentInputStream().read(stringBytes, done, size - done)) != -1) {
            done += read;
        }
        if (done != size) {
            throw new EOFException(done + "!=" + size);
        }
        return stringBytes;
    }

    public InputStream getCurrentInputStream() throws IOException {
        if (this.is != null) {
            return this.is;
        }
        throw new IOException("no InputStream available");
    }

    public OutputStream getCurrentOutputStream() throws IOException {
        if (this.os != null) {
            return this.os;
        }
        throw new IOException("no OutputStream available");
    }

    public boolean readBoolean() throws IOException {
        final int read = this.ensureRead();
        if (read == 1) {
            return true;
        }
        if (read == 0) {
            return false;
        }
        throw new IOException("Invalid boolean value!");
    }

    public long readLong() throws IOException {
        this.ensureRead(8, this.buffer);
        return ((long) this.buffer[0] << 56) + ((long) (this.buffer[1] & 255) << 48) + ((long) (this.buffer[2] & 255) << 40) + ((long) (this.buffer[3] & 255) << 32) + ((long) (this.buffer[4] & 255) << 24) + ((this.buffer[5] & 255) << 16) + ((this.buffer[6] & 255) << 8) + ((this.buffer[7] & 255) << 0);
    }

    public int readShort() throws IOException {
        this.ensureRead(2, this.buffer);
        return ((this.buffer[0] & 255) << 8) + ((this.buffer[1] & 255) << 0);
    }

    public String readString() throws IOException {
        return this.readString(this.readShort());
    }

    public String readString(final int size) throws IOException {
        return new String(this.ensureRead(size, null), 0, size, UTF8);
    }

    public String readString(byte[] tempBuffer) throws IOException {
        final int size = this.readShort();
        return new String(this.ensureRead(size, tempBuffer), 0, size, UTF8);
    }

    public void writeBoolean(final boolean b) throws IOException {
        this.getCurrentOutputStream().write(b ? 1 : 0);
    }

    public void writeLong(final long l) throws IOException {
        this.buffer[0] = (byte) (l >>> 56);
        this.buffer[1] = (byte) (l >>> 48);
        this.buffer[2] = (byte) (l >>> 40);
        this.buffer[3] = (byte) (l >>> 32);
        this.buffer[4] = (byte) (l >>> 24);
        this.buffer[5] = (byte) (l >>> 16);
        this.buffer[6] = (byte) (l >>> 8);
        this.buffer[7] = (byte) (l >>> 0);
        this.getCurrentOutputStream().write(this.buffer, 0, 8);
    }

    public long readLongOptimized() throws IOException {
        long ret = 0;
        long read = 0;
        int position = 0;
        while (true) {
            read = this.ensureRead();
            ret = ret + (read >>> 1 << position * 7);
            if ((read & 1) == 0) {
                return ret;
            }
            position++;
        }
    }

    public void writeLongOptimized(final long value) throws IOException {
        if (value < 0) {
            //
            throw new NumberFormatException("value must be >=0");
        }
        long rest = value;
        int bufferPosition = 0;
        while (true) {
            final int write = (int) ((rest & 127) << 1 & 0xFF);
            this.buffer[bufferPosition] = (byte) write;
            rest = rest >>> 7;
            if (rest == 0) {
                this.getCurrentOutputStream().write(this.buffer, 0, bufferPosition + 1);
                return;
            }
            this.buffer[bufferPosition] = (byte) (this.buffer[bufferPosition] | 1);
            bufferPosition++;
        }
    }

    public void writeShort(final int v) throws IOException {
        this.buffer[0] = (byte) (v >>> 8 & 0xFF);
        this.buffer[1] = (byte) (v >>> 0 & 0xFF);
        this.getCurrentOutputStream().write(this.buffer, 0, 2);
    }

    private final static Charset UTF8 = Charset.forName("UTF-8");

    public void writeString(final String string) throws IOException {
        if (string == null) {
            throw new IOException("string == null");
        }
        final byte[] stringBytes = string.getBytes(UTF8);
        if (stringBytes.length > 32767) {
            throw new IllegalArgumentException("StringSize must not be greater than 32767 bytes");
        }
        this.writeShort(stringBytes.length);
        this.getCurrentOutputStream().write(stringBytes);
    }
}
