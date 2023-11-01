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

import java.io.DataInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;

/**
 * @author daniel
 * @date Apr 14, 2020
 *
 */
public class AutoWrapInflaterInputStream extends FilterInputStream {

    public AutoWrapInflaterInputStream(final InputStream inputStream) throws IOException {
        super(autoWrap(inputStream));
    }

    /**
     * auto wrap RFC1950(zlib stream) or RFC1951(deflate stream)
     *
     * https://tools.ietf.org/html/rfc1950
     *
     * https://tools.ietf.org/html/rfc1951
     *
     * https://stackoverflow.com/questions/9050260/what-does-a-zlib-header-look-like
     *
     * http://carsten.codimi.de/gzip.yaws/
     *
     * @param inputStream
     * @return
     * @throws IOException
     */
    public static InflaterInputStream autoWrap(final InputStream inputStream) throws IOException {
        final PushbackInputStream peekInputStream = new PushbackInputStream(inputStream, 2);
        /**
         * A zlib stream has the following structure:
         *
         * (0)CMF|(1)FLG
         */
        final byte[] zlibHeader = new byte[2];
        new DataInputStream(peekInputStream).readFully(zlibHeader);
        peekInputStream.unread(zlibHeader);
        /**
         * CMF (Compression Method and flags) This byte is divided into a 4-bit compression method and a 4- bit information field depending
         * on the compression method.
         *
         */
        final int CMF = zlibHeader[0] & 0xFF;

        /**
         * bits 0 to 3 CM Compression method
         */
        /**
         * CM (Compression method) This identifies the compression method used in the file. CM = 8 denotes the "deflate" compression method
         * with a window size up to 32K. This is the method used by gzip and PNG (see references [1] and [2] in Chapter 3, below, for the
         * reference documents). CM = 15 is reserved. It might be used in a future version of this specification to indicate the presence of
         * an extra field before the compressed data.
         */
        final int CM = CMF & 0xF;
        /**
         * bits 4 to 7 CINFO Compression info
         */
        /**
         * CINFO (Compression info) For CM = 8, CINFO is the base-2 logarithm of the LZ77 window size, minus eight (CINFO=7 indicates a 32K
         * window size). Values of CINFO above 7 are not allowed in this version of the specification. CINFO is not defined in this
         * specification for CM not equal to 8.
         */
        final int CINFO = CMF >> 4 & 0xF;
        /**
         * FLG (FLaGs) This flag byte is divided as follows:
         *
         * bits 0 to 4 FCHECK (check bits for CMF and FLG) bit 5 FDICT (preset dictionary) bits 6 to 7 FLEVEL (compression level)
         */
        final int FLG = zlibHeader[1] & 0xFF;
        /**
         * The FCHECK value must be such that CMF and FLG, when viewed as a 16-bit unsigned integer stored in MSB order (CMF*256 + FLG), is
         * a multiple of 31.
         */
        final boolean FCHECK = ((CMF << 8) | FLG) % 31 == 0;
        final Inflater inflater;
        if (CM == 8 && CINFO <= 7 && FCHECK) {
            // zip deflate format
            inflater = new Inflater(false);
        } else {
            // raw deflate format
            inflater = new Inflater(true);
        }
        return new InflaterInputStream(peekInputStream, inflater) {
            private boolean closedFlag = false;

            /**
             * InflaterInputStream.close doesn't autoclose custom Inflater
             */
            @Override
            public void close() throws IOException {
                if (!closedFlag) {
                    try {
                        inf.end();
                    } finally {
                        super.close();
                        closedFlag = true;
                    }
                }

            }
        };

    }
}
