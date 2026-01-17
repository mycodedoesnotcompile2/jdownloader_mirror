/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.utils.net.httpserver;

import java.io.IOException;
import java.io.InputStream;

import org.appwork.utils.net.CountingInputStream;

/**
 * Extended LimitedInputStream that throws an exception when the limit is exceeded instead of just returning -1. Based on
 * CountingInputStream to always count bytes.
 *
 * @author AppWork
 */
public class LimitedInputStreamWithException extends CountingInputStream {

    protected final byte[] skipBuffer = new byte[32767];

    private long           limit;

    /**
     * WARNING: Limit may be 0 --> do not read anything!
     *
     * @param inputStream
     * @param httpServer
     * @param l
     * @param m
     */
    public LimitedInputStreamWithException(InputStream inputStream, long limit) {
        super(inputStream);
        this.limit = limit;

    }

    @Override
    public int read() throws IOException {

        if (limit < 0) {
            return super.read();
        }

        final long left = limit - this.transferedBytes();
        if (left <= 0) {
            // we already have too much
            throw createException();
        }

        return super.read();

    }

    /**
     * @return
     */
    protected RequestSizeLimitExceededException createException() {
        return new RequestSizeLimitExceededException("Request size limit exceeded. Maximum allowed size: " + limit + " bytes");
    }

    @Override
    public int read(final byte[] b, final int off, int len) throws IOException {
        // is this ok?
        if (len == 0) {
            return 0;
        }

        if (limit < 0) {
            return super.read(b, off, len);
        }

        final long left = limit - this.transferedBytes();
        if (left <= 0) {
            // we already have too much
            throw createException();
        }
        if (left < len) {
            // to int cast should be fine this way!
            len = (int) left;
        }
        final int result = super.read(b, off, len);
        return result;

    }

    @Override
    public long skip(long n) throws IOException {
        if (n > Integer.MAX_VALUE) {
            n = Integer.MAX_VALUE;

        }
        if (n > skipBuffer.length) {
            n = skipBuffer.length;
        }
        return this.read(this.skipBuffer, 0, (int) n);

    }

    // @Override
    // public boolean isValidEOF() {
    // if (isLimitEnabled()) {
    // return this.transferedBytes() == this.limit;
    // }
    // return false;
    // }

}
