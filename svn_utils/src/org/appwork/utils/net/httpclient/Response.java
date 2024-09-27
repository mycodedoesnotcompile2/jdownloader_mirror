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
package org.appwork.utils.net.httpclient;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClient.ToStringByteArrayOutputStream;
import org.appwork.utils.net.httpconnection.HTTPConnection;

/**
 * @author thomas
 * @date 25.09.2024
 *
 */
public class Response {
    private HTTPConnection                connection;
    private ToStringByteArrayOutputStream bytes;
    private RequestContext                context;

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        try {
            return connection + "\r\n\r\n" + getString(Charset.forName("UTF-8"));
        } catch (IOException e) {
            return connection.toString();
        }
    }

    /**
     * @param httpConnection
     * @param os
     * @param context
     */
    public Response(HTTPConnection httpConnection, ToStringByteArrayOutputStream os, RequestContext context) {
        this.connection = httpConnection;
        this.bytes = os;
        this.context = context;
    }

    /**
     * @param utf8
     * @return
     * @throws IOException
     */
    public String getString(Charset charset) throws IOException {
        if (charset == null) {
            String ct = connection.getCharset();
            if (StringUtils.isEmpty(ct)) {
                ct = "UTF-8";
            }
            charset = Charset.forName(ct);
        }
        CountingInputStream fromContext = context.getInputStream();
        if (fromContext != null && bytes.size() == 0) {
            IO.readStream(-1, fromContext.getInputStream(), bytes, true);
        }
        return bytes.toString(charset);
    }

    /**
     * @return
     * @throws IOException
     */
    public byte[] getBytes() throws IOException {
        CountingInputStream fromContext = context.getInputStream();
        if (fromContext != null && bytes.size() == 0) {
            IO.readStream(-1, fromContext.getInputStream(), bytes, true);
        }
        return bytes.toByteArray();
    }

    /**
     * @return
     * @throws IOException
     */
    public InputStream getInputStream() throws IOException {
        CountingInputStream fromContext = context.getInputStream();
        if (fromContext != null) {
            return fromContext;
        }
        return new ByteArrayInputStream(getBytes());
    }

    /**
     * @return
     */
    public int getCode() {
        return connection.getResponseCode();
    }

    /**
     * @return
     */
    public Response log() {
        LogV3.info(this + "");
        return this;
    }
}
