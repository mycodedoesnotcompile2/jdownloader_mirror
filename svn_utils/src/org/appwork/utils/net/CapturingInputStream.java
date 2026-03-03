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
import java.io.InputStream;
import java.io.OutputStream;

/**
 * InputStream that delegates reads to an underlying stream and simultaneously writes every read byte to a referenced OutputStream
 * (capture). Use to capture a copy of the data read from the input (e.g. request body) without consuming it twice. The capture stream is
 * not closed when this stream is closed; the caller manages its lifecycle.
 */
public class CapturingInputStream extends CountingInputStream {
    private final OutputStream capture;

    /**
     * @param in
     *            the stream to read from
     * @param capture
     *            the stream to write a copy of the read bytes to; may be null (then no copy is written)
     */
    public CapturingInputStream(final InputStream in, final OutputStream capture) {
        super(in);
        this.capture = capture;
        if (capture == null) {
            throw new IllegalArgumentException("capture Stream must not be null");
        }
    }

    /**
     * @return the capture
     */
    public OutputStream getCaptureStream() {
        return capture;
    }

    @Override
    public int read() throws IOException {
        final int b = super.read();
        if (b != -1) {
            capture.write(b);
        }
        return b;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        final int n = super.read(b, off, len);
        if (n > 0) {
            capture.write(b, off, n);
        }
        return n;
    }
}
