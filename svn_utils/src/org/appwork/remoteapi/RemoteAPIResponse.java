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
package org.appwork.remoteapi;

import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.GZIPOutputStream;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.ResponseCodeInterface;
import org.appwork.remoteapi.RemoteAPIRequest.REQUESTTYPE;
import org.appwork.utils.Application;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.net.httpserver.responses.HttpResponseInterface;

/**
 * @author daniel
 *
 */
public class RemoteAPIResponse implements HttpResponseInterface {
    protected final int        MAXUNCOMPRESSED = 32767;
    private final HttpResponse response;
    private final RemoteAPI    remoteAPI;

    @Override
    public String toString() {
        return super.toString();
    }

    public RemoteAPIResponse(final HttpResponse response, final RemoteAPI remoteAPI) {
        this.response = response;
        this.remoteAPI = remoteAPI;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.net.httpserver.responses.HttpResponseInterface# closeConnection()
     */
    @Override
    public void closeConnection() {
        this.response.closeConnection();
    }

    public HttpResponse getHttpResponse() {
        return this.response;
    }

    public OutputStream getOutputStream(final boolean sendResponseHeaders) throws IOException {
        return this.response.getOutputStream(sendResponseHeaders);
    }

    /**
     * @return the remoteAPI
     */
    public RemoteAPI getRemoteAPI() {
        return this.remoteAPI;
    }

    public ResponseCodeInterface getResponseCode() {
        return this.response.getResponseCode();
    }

    /**
     * @return the responseHeaders
     */
    public HeaderCollection getResponseHeaders() {
        return this.response.getResponseHeaders();
    }

    /**
     * @param gzip
     * @param b
     * @param bytes
     */
    public void sendBytes(final RemoteAPIRequest request, final byte[] bytes) throws IOException {
        /* we dont want this api response to get cached */
        if (this.getResponseHeaders().get(HTTPConstants.HEADER_REQUEST_CACHE_CONTROL) == null) {
            this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_CACHE_CONTROL, "no-store, no-cache"));
        }
        if (this.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE) == null) {
            this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/json"));
        }
        final boolean gzip = RemoteAPI.gzip(request);
        final boolean deflate = RemoteAPI.gzip(request) && Application.getJavaVersion() >= Application.JAVA16;
        final boolean isHeadRequest = REQUESTTYPE.HEAD.equals(request.getRequestType());
        if (gzip == false && deflate == false || bytes.length <= this.MAXUNCOMPRESSED) {
            this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
            final OutputStream os = this.getOutputStream(true);
            if (!isHeadRequest) {
                os.write(bytes);
            }
        } else {
            if (deflate) {
                this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "deflate"));
                this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
                final OutputStream os = this.getOutputStream(true);
                if (!isHeadRequest) {
                    final ChunkedOutputStream cos = new ChunkedOutputStream(os);
                    final DeflaterOutputStream out = new DeflaterOutputStream(cos, new Deflater(9, true));
                    out.write(bytes);
                    out.finish();
                    out.flush();
                    cos.sendEOF();
                }
            } else {
                this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
                this.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
                final OutputStream os = this.getOutputStream(true);
                if (!isHeadRequest) {
                    final ChunkedOutputStream cos = new ChunkedOutputStream(os);
                    final GZIPOutputStream out = new GZIPOutputStream(cos);
                    out.write(bytes);
                    out.finish();
                    out.flush();
                    cos.sendEOF();
                }
            }
        }
    }

    /**
     * @param responseCode
     *            the responseCode to set
     */
    public void setResponseCode(final ResponseCodeInterface responseCode) {
        this.response.setResponseCode(responseCode);
    }
}
