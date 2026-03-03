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
 * OutputStream that delegates every write to multiple target streams (tee). All targets receive the same bytes. Use e.g. with a socket
 * stream and a ByteArrayOutputStream to capture a copy of the response while sending it to the client. When this stream is closed, all
 * target streams are closed.
 */
public class CapturingOutputStream extends OutputStream {
    private final OutputStream[] targets;

    /**
     * @param targets
     *            one or more streams to which all writes are forwarded. All are closed when this stream is closed.
     */
    public CapturingOutputStream(final OutputStream... targets) {
        if (targets == null || targets.length == 0) {
            throw new IllegalArgumentException("at least one target stream required");
        }
        this.targets = targets.clone();
    }

    @Override
    public void write(final int b) throws IOException {
        IOException first = null;
        for (int i = 0; i < targets.length; i++) {
            try {
                targets[i].write(b);
            } catch (IOException e) {
                if (first == null) {
                    first = e;
                }
            }
        }
        if (first != null) {
            throw first;
        }
    }

    @Override
    public void write(final byte[] b) throws IOException {
        write(b, 0, b.length);
    }

    @Override
    public void write(final byte[] b, final int off, final int len) throws IOException {
        IOException first = null;
        for (int i = 0; i < targets.length; i++) {
            try {
                targets[i].write(b, off, len);
            } catch (IOException e) {
                if (first == null) {
                    first = e;
                }
            }
        }
        if (first != null) {
            throw first;
        }
    }

    @Override
    public void flush() throws IOException {
        IOException first = null;
        for (int i = 0; i < targets.length; i++) {
            try {
                targets[i].flush();
            } catch (IOException e) {
                if (first == null) {
                    first = e;
                }
            }
        }
        if (first != null) {
            throw first;
        }
    }

    @Override
    public void close() throws IOException {
        IOException first = null;
        for (int i = 0; i < targets.length; i++) {
            try {
                targets[i].close();
            } catch (IOException e) {
                if (first == null) {
                    first = e;
                }
            }
        }
        if (first != null) {
            throw first;
        }
    }

    /**
     * @return
     */
    public OutputStream[] getDelegateStreams() {
        return targets;
    }
}
