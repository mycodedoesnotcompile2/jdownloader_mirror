/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLDecoder;
import java.nio.CharBuffer;
import java.nio.charset.StandardCharsets;

import org.appwork.utils.net.Base64InputStream;
import org.appwork.utils.net.CharSequenceInputStream;

/**
 * @author Daniel Wilhelm
 * @date 16.10.2026
 *
 */
public abstract class DataURLInputStream extends FilterInputStream {

    protected static final String BASE64_MARKER = ";base64";

    public DataURLInputStream(InputStream in) {
        super(in);
    }

    public abstract String getMimeType();

    protected static boolean endsWithBase64(String header) {
        if (header == null) {
            return false;
        }
        final int targetLength = BASE64_MARKER.length();
        final int headerLength = header.length();
        if (headerLength < targetLength) {
            return false;
        }
        return header.regionMatches(true, headerLength - targetLength, BASE64_MARKER, 0, targetLength);
    }

    public static DataURLInputStream open(String dataURL) throws IOException {
        if (dataURL == null) {
            return null;
        }
        if (!dataURL.regionMatches(true, 0, "data:", 0, 5)) {
            return null;
        }
        final int dataIndex = dataURL.indexOf(",");
        if (dataIndex == -1) {
            throw new IOException("invalid or unsupported dataURL:" + dataURL);
        }
        String header = dataURL.substring(5, dataIndex);
        final boolean isBase64 = endsWithBase64(header);
        if (isBase64) {
            header = header.substring(0, header.length() - BASE64_MARKER.length());
        }
        final String mimeType = header;
        String data = dataURL.substring(dataIndex + 1);
        data = URLDecoder.decode(data, StandardCharsets.UTF_8.name());
        final CharBuffer cb = CharBuffer.wrap(data);
        InputStream is = new CharSequenceInputStream(cb, StandardCharsets.UTF_8);
        if (isBase64) {
            is = new Base64InputStream(is);
        }
        return new DataURLInputStream(is) {

            @Override
            public String getMimeType() {
                return mimeType;
            }
        };

    }

}
