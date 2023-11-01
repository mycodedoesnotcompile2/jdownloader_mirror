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
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author daniel
 * 
 */
public class HexInputStream extends FilterInputStream {

    private static final int[] HEXMAP = new int[256];
    static {
        for (int index = 0; index < HexInputStream.HEXMAP.length; index++) {
            HexInputStream.HEXMAP[index] = -1;
        }
        for (int index = 48; index <= 57; index++) {
            HexInputStream.HEXMAP[index] = -48 + index;
        }
        for (int index = 65; index <= 70; index++) {
            HexInputStream.HEXMAP[index] = -55 + index;
        }
        for (int index = 97; index <= 102; index++) {
            HexInputStream.HEXMAP[index] = -87 + index;
        }
    }

    public HexInputStream(final InputStream in) {
        super(in);
    }

    private int convertRaw(final int raw) throws IOException {
        final int mapped = HexInputStream.HEXMAP[raw];
        if (mapped < 0) { throw new IOException("unsupported hexchar" + raw); }
        return mapped;
    }

    @Override
    public synchronized void mark(final int readlimit) {
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    @Override
    public int read() throws IOException {
        final int hexRawPart1 = super.read();
        if (hexRawPart1 == -1) { return -1; }
        final int hexRawPart2 = super.read();
        if (hexRawPart2 == -1) { throw new EOFException("incomplete hex"); }
        final int hexPart1 = this.convertRaw(hexRawPart1);
        final int hexPart2 = this.convertRaw(hexRawPart2);
        final int ret = hexPart1 * 16 + hexPart2;
        return ret;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        /* taken from InputStream */
        if (b == null) {
            throw new NullPointerException();
        } else if (off < 0 || len < 0 || len > b.length - off) {
            throw new IndexOutOfBoundsException();
        } else if (len == 0) { return 0; }

        int c = this.read();
        if (c == -1) { return -1; }
        int index = 0;
        b[off + index++] = (byte) c;
        for (; index < len; index++) {
            c = this.read();
            if (c == -1) {
                break;
            }
            b[off + index] = (byte) c;
        }
        return index;
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new IOException("mark/reset not supported");
    }

    @Override
    public long skip(final long n) throws IOException {
        return super.skip(n);
    }

}
