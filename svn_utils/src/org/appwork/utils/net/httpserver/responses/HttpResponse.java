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
package org.appwork.utils.net.httpserver.responses;

import java.io.IOException;
import java.io.OutputStream;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.net.protocol.http.ResponseCodeInterface;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.HttpConnection;
import org.appwork.utils.net.httpserver.HttpConnection.ConnectionHook;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.RawHttpConnectionInterface;
import org.appwork.utils.net.httpserver.ResponseSecurityHeaders;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.HttpServerInterface;

/**
 * @author daniel
 *
 */
public class HttpResponse implements HttpResponseInterface {
    private final HeaderCollection             responseHeaders;
    public static final byte[]                 NEWLINE       = "\r\n".getBytes();
    public static final byte[]                 HTTP11        = "HTTP/1.1 ".getBytes();
    public static final byte[]                 _0            = "0".getBytes();
    private ResponseCodeInterface              responseCode  = ResponseCode.SUCCESS_NO_CONTENT;
    protected final RawHttpConnectionInterface connection;
    protected OutputStream                     outputStream  = null;
    protected boolean                          asyncResponse = false;

    public RawHttpConnectionInterface getConnection() {
        return connection;
    }

    public HttpResponse(final RawHttpConnectionInterface connection) {
        this.connection = connection;
        this.responseHeaders = new HeaderCollection();
        this.responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_CONNECTION, "close"));
        this.addDefaultServerHeader();
        // Add default security headers if not already present (allows overriding)
        this.addDefaultSecurityHeaders();
        // Add default CORS headers if not already present (allows overriding)
        this.addDefaultCorsHeaders();
    }

    private void addDefaultServerHeader() {
        String serverHeader = null;
        if (this.connection instanceof HttpConnection) {
            final HttpServerInterface server = ((HttpConnection) this.connection).getServer();
            if (server != null) {
                serverHeader = server.getResponseServerHeader();
            }
        }
        if (serverHeader == null) {
            serverHeader = HttpServer.APP_WORK_GMB_H_HTTP_SERVER;
        }
        if (!StringUtils.isEmpty(serverHeader)) {
            this.responseHeaders.add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_SERVER, serverHeader));
        }
    }

    /**
     * Adds default security headers to the response if they are not already present. This allows handlers to override these defaults if
     * needed. The security headers configuration is obtained from the HttpServer via the connection.
     */
    private void addDefaultSecurityHeaders() {
        ResponseSecurityHeaders securityHeaders = null;
        // Try to get security headers config from HttpServer via HttpConnection
        if (this.connection instanceof HttpConnection) {
            final HttpServerInterface server = ((HttpConnection) this.connection).getServer();
            if (server != null) {
                securityHeaders = server.getResponseSecurityHeaders();
            }
        } else {
            DebugMode.debugger();
        }
        // Use default values if no config is available
        if (securityHeaders == null) {
            securityHeaders = new ResponseSecurityHeaders();
        }
        // Add security headers to response (only if not already present)
        securityHeaders.addSecurityHeaders(this.responseHeaders);
    }

    /**
     * Adds default CORS headers to the response if CORS is enabled and the request is a cross-origin request. This allows handlers to
     * override these defaults if needed. The CORS configuration is obtained from the HttpServer via the connection.
     */
    private void addDefaultCorsHeaders() {
        CorsHandler corsHandler = null;
        // Try to get CORS config from HttpServer via HttpConnection
        if (this.connection instanceof HttpConnection) {
            final HttpServerInterface server = ((HttpConnection) this.connection).getServer();
            if (server != null) {
                corsHandler = server.getCorsHandler();
            }
        }
        // Only add CORS headers if CORS is enabled
        if (corsHandler != null) {
            final HttpRequest request = this.connection.getRequest();
            if (request != null) {
                corsHandler.addCorsHeaders(request, this.responseHeaders);
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.net.httpserver.responses.HttpResponseInterface# closeConnection()
     */
    @Override
    public void closeConnection() {
        try {
            this.connection.closeConnection();
        } finally {
            this.connection.close();
        }
    }

    /**
     * returns this HttpResonse's OutputStream. NOTE: set ResponseHeaders/Code before first call of this. once the OutputStream is available
     * you cannot change ResponseHeaders/Code anymore
     *
     * @return
     * @throws IOException
     */
    public OutputStream getOutputStream(final boolean sendResponseHeaders) throws IOException {
        if (sendResponseHeaders == false) {
            return this.connection.getOutputStream(false);
        }
        if (this.outputStream == null) {
            this.outputStream = this.connection.getOutputStream(true);
        }
        return this.outputStream;
    }

    /**
     * @return the responseCode
     */
    public ResponseCodeInterface getResponseCode() {
        return this.responseCode;
    }

    /**
     * @return the responseHeaders
     */
    public HeaderCollection getResponseHeaders() {
        return this.responseHeaders;
    }

    /**
     * @param responseCode
     *            the responseCode to set
     */
    public void setResponseCode(final ResponseCodeInterface responseCode) {
        this.responseCode = responseCode;
    }

    /**
     * @param remoteAPIImpl
     */
    public void setHook(ConnectionHook hook) {
        connection.setHook(hook);
    }
}
