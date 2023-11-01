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
package org.appwork.utils.net.httpserver;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketException;
import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpserver.handler.ExtendedHttpRequestHandler;
import org.appwork.utils.net.httpserver.handler.HttpProxyHandler;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.ConnectRequest;
import org.appwork.utils.net.httpserver.requests.DeleteRequest;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HeadRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.requests.PutRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author daniel
 *
 */
public class HttpConnection implements HttpConnectionRunnable, RawHttpConnectionInterface {
    public static enum HttpConnectionType {
        DELETE,
        CONNECT,
        PUT,
        HEAD,
        GET,
        POST,
        OPTIONS,
        UNKNOWN;

        private final byte[] requestTypeBytes;

        private HttpConnectionType() {
            byte[] bytes = null;
            try {
                bytes = this.name().getBytes("ISO-8859-1");
            } catch (final Throwable e) {
                bytes = this.name().getBytes();
            }
            this.requestTypeBytes = bytes;
        }

        public final boolean isRequestType(final byte[] input) {
            if (input.length < this.requestTypeBytes.length) {
                return false;
            }
            for (int i = 0; i < this.requestTypeBytes.length; i++) {
                if (this.requestTypeBytes[i] != input[i]) {
                    return false;
                }
            }
            return true;
        }

        public static HttpConnectionType get(final byte[] input) {
            for (HttpConnectionType type : values()) {
                if (type.isRequestType(input)) {
                    return type;
                }
            }
            return HttpConnectionType.UNKNOWN;
        }

        public final int length() {
            return this.requestTypeBytes.length;
        }
    }

    public static List<KeyValuePair> parseParameterList(final String requestedParameters) throws IOException {
        final List<KeyValuePair> requestedURLParameters = new LinkedList<KeyValuePair>();
        if (!StringUtils.isEmpty(requestedParameters)) {
            /* build requestedParamters */
            final String[] parameters = requestedParameters.split("\\&(?!#)", -1);
            for (final String parameter : parameters) {
                /* we only want the first = be parsed */
                final String params[] = parameter.split("=", 2);
                if (params.length == 1) {
                    /* no value */
                    requestedURLParameters.add(new KeyValuePair(null, URLDecoder.decode(params[0], "UTF-8")));
                } else {
                    /* key = value */
                    if ("_".equals(params[0])) {
                        /* we remove random timestamp from jquery here */
                        // System.out.println("remove timestamp param from jquery: "
                        // + params[1]);
                        continue;
                    }
                    requestedURLParameters.add(new KeyValuePair(URLDecoder.decode(params[0], "UTF-8"), URLDecoder.decode(params[1], "UTF-8")));
                }
            }
        }
        return requestedURLParameters;
    }

    protected final HttpServer   server;
    protected final Socket       clientSocket;
    protected boolean            outputStreamInUse = false;
    protected HttpResponse       response          = null;
    protected final InputStream  is;
    protected final OutputStream os;
    protected HttpRequest        request;
    private static final Pattern METHOD            = Pattern.compile("(GET|POST|HEAD|OPTIONS|CONNECT)");
    private static final Pattern REQUESTLINE       = Pattern.compile("\\s+(.+)\\s+HTTP/");
    public static final Pattern  REQUESTURL        = Pattern.compile("^(/.*?)($|\\?)");
    public static final Pattern  REQUESTPARAM      = Pattern.compile("^/.*?\\?(.+)");

    protected HttpConnection(final HttpServer server, final Socket clientSocket, final InputStream is, final OutputStream os) throws IOException {
        this.server = server;
        this.clientSocket = clientSocket;
        if (is == null) {
            this.is = clientSocket.getInputStream();
        } else {
            this.is = is;
        }
        if (os == null) {
            this.os = clientSocket.getOutputStream();
        } else {
            this.os = os;
        }
        if (clientSocket != null) {
            this.clientSocket.setSoTimeout(60 * 1000);
        }
    }

    public Socket getClientSocket() {
        return clientSocket;
    }

    protected HttpConnection() {
        this.server = null;
        this.clientSocket = null;
        this.is = null;
        this.os = null;
    }

    public HttpConnection(final HttpServer server, final Socket clientSocket) throws IOException {
        this(server, clientSocket, null, null);
    }

    protected HttpRequest buildGetRequest() throws IOException {
        return new GetRequest(this);
    }

    protected HttpRequest buildHeadRequest() throws IOException {
        return new HeadRequest(this);
    }

    protected HttpRequest buildOptionsRequest() throws IOException {
        return new OptionsRequest(this);
    }

    protected HttpRequest buildConnectRequest() throws IOException {
        return new ConnectRequest(this);
    }

    protected HttpRequest buildPostRequest() throws IOException {
        return new PostRequest(this);
    }

    protected HttpRequest buildPutRequest() throws IOException {
        return new PutRequest(this);
    }

    protected HttpRequest buildDeleteRequest() throws IOException {
        return new DeleteRequest(this);
    }

    /**
     * parses the request and creates a GET/POST-Request Object and fills it with all received data
     *
     * @return
     * @throws IOException
     */
    protected HttpRequest buildRequest() throws IOException {
        /* read request Method and Path */
        final String requestLine = this.parseRequestLine();
        if (StringUtils.isEmpty(requestLine)) {
            throw new EmptyRequestException();
        }
        // TOTO: requestLine may be "" in some cases (chrome pre connection...?)
        final HttpConnectionType connectionType = this.parseConnectionType(requestLine);
        if (connectionType == HttpConnectionType.UNKNOWN) {
            throw new IOException("Unsupported " + requestLine);
        }

        final String requestedURL = new Regex(requestLine, HttpConnection.REQUESTLINE).getMatch(0);
        final String requestedPath = new Regex(requestedURL, HttpConnection.REQUESTURL).getMatch(0);
        final List<KeyValuePair> requestedURLParameters = this.parseRequestURLParams(requestedURL);

        /* read request Headers */
        final HeaderCollection requestHeaders = this.parseRequestHeaders();
        final HttpRequest request;
        switch (connectionType) {
        case CONNECT:
            request = this.buildConnectRequest();
            break;
        case POST:
            request = this.buildPostRequest();
            break;
        case GET:
            request = this.buildGetRequest();
            break;
        case OPTIONS:
            request = this.buildOptionsRequest();
            break;
        case HEAD:
            request = this.buildHeadRequest();
            break;
        case PUT:
            request = this.buildPutRequest();
            break;
        case DELETE:
            request = this.buildDeleteRequest();
            break;
        default:
            throw new IOException("Unsupported " + requestLine);
        }
        if (request == null) {
            throw new IOException("Unsupported " + requestLine);
        }
        request.setBridge(server);
        /* parse remoteClientAddresses */
        request.setRemoteAddress(this.getRemoteAddress(requestHeaders));
        request.setRequestedURLParameters(requestedURLParameters);
        request.setRequestedPath(requestedPath);
        request.setRequestedURL(requestedURL);
        request.setRequestHeaders(requestHeaders);
        return request;
    }

    /**
     * @return
     */
    protected HttpResponse buildResponse() throws IOException {
        return new HttpResponse(this);
    }

    public boolean closableStreams() {
        return this.clientSocket == null;
    }

    public void close() {
    }

    /**
     * closes the client socket and removes this connection from server connection pool
     */
    public void closeConnection() {
        if (this.clientSocket != null) {
            try {
                this.clientSocket.shutdownOutput();
            } catch (final Throwable ignore) {
            }
            try {
                this.clientSocket.close();
            } catch (final Throwable ignore) {
            }
        }
    }

    protected boolean deferRequest(final HttpRequest request) throws Exception {
        return false;
    }

    public List<HttpRequestHandler> getHandler() {
        return this.server.getHandler();
    }

    /**
     * @return
     * @throws IOException
     */
    public InputStream getInputStream() throws IOException {
        return this.getRawInputStream();
    }

    /**
     * return the outputStream for this connection. send response headers if they have not been sent yet send yet
     *
     * @return
     * @throws IOException
     */
    public OutputStream getOutputStream(final boolean sendResponseHeaders) throws IOException {
        if (sendResponseHeaders) {
            this.openOutputStream();
        }
        return this.getRawOutputStream();
    }

    protected InputStream getRawInputStream() throws IOException {
        if (this.is == null) {
            throw new IllegalStateException("no RawInputStream available!");
        }
        return this.is;
    }

    protected OutputStream getRawOutputStream() throws IOException {
        if (this.os == null) {
            throw new IllegalStateException("no RawOutputStream available!");
        }
        return this.os;
    }

    protected List<String> getRemoteAddress(final HeaderCollection requestHeaders) {
        final java.util.List<String> remoteAddress = new ArrayList<String>();
        if (this.clientSocket != null) {
            remoteAddress.add(this.clientSocket.getInetAddress().getHostAddress());
        }
        final HTTPHeader forwardedFor = requestHeaders.get("X-Forwarded-For");
        if (forwardedFor != null && !StringUtils.isEmpty(forwardedFor.getValue())) {
            final String addresses[] = forwardedFor.getValue().split(", ");
            for (final String ip : addresses) {
                remoteAddress.add(ip.trim());
            }
        }
        return remoteAddress;
    }

    public HttpRequest getRequest() {
        return this.request;
    }

    public HttpResponse getResponse() {
        return this.response;
    }

    public boolean isOutputStreamInUse() {
        return this.outputStreamInUse;
    }

    public boolean onException(final Throwable e, final HttpRequest request, final HttpResponse response) throws IOException {
        if (Exceptions.containsInstanceOf(e, SocketException.class, ClosedChannelException.class)) {
            // socket already closed
            return true;
        } else if (e instanceof HttpConnectionExceptionHandler) {
            return ((HttpConnectionExceptionHandler) e).handle(response);
        } else if (request != null) {
            this.response = new HttpResponse(this);
            this.response.setResponseCode(ResponseCode.SERVERERROR_INTERNAL);
            final byte[] bytes = Exceptions.getStackTrace(e).getBytes("UTF-8");
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text; charset=UTF-8"));
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
            this.response.getOutputStream(true).write(bytes);
            this.response.getOutputStream(true).flush();
            return true;
        } else {
            return true;
        }
    }

    protected void onUnhandled(final HttpRequest request, final HttpResponse response) throws IOException {
        response.setResponseCode(ResponseCode.SERVERERROR_NOT_IMPLEMENTED);
    }

    protected HttpConnectionType parseConnectionType(final String requestLine) throws IOException {
        final String method = new Regex(requestLine, HttpConnection.METHOD).getMatch(0);
        // TOTO: requestLine may be "" in some cases (chrome pre connection...?)
        try {
            return HttpConnectionType.valueOf(method);
        } catch (final Exception e) {
            return HttpConnectionType.UNKNOWN;
        }
    }

    protected HeaderCollection parseRequestHeaders() throws IOException {
        final ByteBuffer headers = this.readRequestHeaders();
        final String[] headerStrings;
        if (headers.hasArray()) {
            headerStrings = new String(headers.array(), headers.arrayOffset(), headers.limit(), "ISO-8859-1").split("(\r\n)|(\n)");
        } else {
            final byte[] bytesHeaders = new byte[headers.limit()];
            headers.get(bytesHeaders);
            headerStrings = new String(bytesHeaders, "ISO-8859-1").split("(\r\n)|(\n)");
        }
        /* build requestHeaders HashMap */
        final HeaderCollection requestHeaders = new HeaderCollection();
        for (final String line : headerStrings) {
            String key = null;
            String value = null;
            int index = 0;
            if ((index = line.indexOf(": ")) > 0) {
                key = line.substring(0, index);
                value = line.substring(index + 2);
            } else if ((index = line.indexOf(":")) > 0) {
                /* buggy clients that don't have :space ARG */
                key = line.substring(0, index);
                value = line.substring(index + 1);
            } else {
                key = null;
                value = line;
            }
            requestHeaders.add(new HTTPHeader(key, value));
        }
        return requestHeaders;
    }

    protected String parseRequestLine() throws IOException {
        final ByteBuffer header = this.readRequestLine();
        if (header.hasArray()) {
            return this.preProcessRequestLine(new String(header.array(), header.arrayOffset(), header.limit(), "ISO-8859-1").trim());
        } else {
            final byte[] bytesRequestLine = new byte[header.limit()];
            header.get(bytesRequestLine);
            return this.preProcessRequestLine(new String(bytesRequestLine, "ISO-8859-1").trim());
        }
    }

    protected List<KeyValuePair> parseRequestURLParams(final String requestURL) throws IOException {
        return HttpConnection.parseParameterList(new Regex(requestURL, HttpConnection.REQUESTPARAM).getMatch(0));
    }

    protected String preProcessRequestLine(final String requestLine) throws IOException {
        return requestLine;
    }

    protected ByteBuffer readRequestHeaders() throws IOException {
        return HTTPConnectionUtils.readheader(this.getInputStream(), false);
    }

    protected ByteBuffer readRequestLine() throws IOException {
        return HTTPConnectionUtils.readheader(this.getInputStream(), true);
    }

    protected boolean isProxyRequest(HttpRequest request) {
        if (request != null) {
            return request instanceof ConnectRequest || StringUtils.startsWithCaseInsensitive(request.getRequestedURL(), "http://") || StringUtils.startsWithCaseInsensitive(request.getRequestedURL(), "https://");
        } else {
            return false;
        }
    }

    protected boolean isPostRequest(HttpRequest request) {
        return request instanceof PostRequest;
    }

    protected boolean isGetRequest(HttpRequest request) {
        return request instanceof GetRequest;
    }

    @Override
    public void run() {
        boolean closeConnection = true;
        try {
            if (this.request == null) {
                this.request = this.buildRequest();
            }
            if (this.response == null) {
                this.response = this.buildResponse();
            }
            final HttpRequest request = this.request;
            if (this.deferRequest(request)) {
                closeConnection = false;
            } else {
                boolean handled = false;
                final boolean isPostRequest = isPostRequest(request);
                final boolean isGetRequest = isGetRequest(request);
                final boolean isProxyRequest = isProxyRequest(request);
                final HttpResponse response = this.response;
                for (final HttpRequestHandler handler : this.getHandler()) {
                    final ExtendedHttpRequestHandler extendedHandler;
                    if (handler instanceof ExtendedHttpRequestHandler) {
                        extendedHandler = (ExtendedHttpRequestHandler) handler;
                    } else {
                        extendedHandler = null;
                    }
                    try {
                        if (extendedHandler != null) {
                            extendedHandler.onBeforeRequest(request, response);
                        }
                        if (isPostRequest) {
                            handled = handler.onPostRequest((PostRequest) request, response);
                        } else if (isGetRequest) {
                            handled = handler.onGetRequest((GetRequest) request, response);
                        } else if (isProxyRequest) {
                            handled = ((HttpProxyHandler) handler).onProxyConnectRequest(request, response);
                        }
                        if (extendedHandler != null) {
                            extendedHandler.onAfterRequest(request, response, handled);
                        }
                        if (handled) {
                            break;
                        }
                    } catch (Throwable e) {
                        if (extendedHandler != null) {
                            extendedHandler.onAfterRequestException(request, response, e);
                        }
                        throw e;
                    }
                }
                if (!handled) {
                    /* generate error handler */
                    this.onUnhandled(request, response);
                }
                /* send response headers if they have not been sent yet send yet */
                response.getOutputStream(true);
            }
        } catch (final Throwable e) {
            try {
                closeConnection = this.onException(e, this.request, this.response);
            } catch (final Throwable nothing) {
                e.printStackTrace();
                nothing.printStackTrace();
            }
        } finally {
            if (closeConnection) {
                this.closeConnection();
                this.close();
            }
            // if (getHook() == null) {
            // LogV3.finer("No Connection Hook!");
            // }
        }
    }

    public static interface ConnectionHook {
        /**
         * @param httpConnection
         */
        void onBeforeSendHeaders(HttpResponse response);
    }

    private ConnectionHook hook;

    public ConnectionHook getHook() {
        return hook;
    }

    public void setHook(ConnectionHook hook) {
        this.hook = hook;
    }

    /**
     * this function sends the response headers
     *
     * @throws IOException
     */
    protected void openOutputStream() throws IOException {
        if (this.isOutputStreamInUse()) {
            throw new OutputStreamIsAlreadyInUseException("OutputStream is already in use!");
        } else if (this.response != null) {
            try {
                final OutputStream out = this.getRawOutputStream();
                final ConnectionHook lHook = hook;
                if (lHook != null) {
                    lHook.onBeforeSendHeaders(response);
                }
                openOutputStream(out, response);
            } finally {
                this.setOutputStreamInUse(true);
            }
        }
    }

    protected void openOutputStream(OutputStream out, HttpResponse response) throws IOException {
        out.write(HttpResponse.HTTP11);
        out.write(response.getResponseCode().getBytes());
        out.write(HttpResponse.NEWLINE);
        for (final HTTPHeader h : this.response.getResponseHeaders()) {
            // null value is used to "remove" a header
            if (h.getValue() == null) {
                continue;
            }
            out.write(h.getKey().getBytes("ISO-8859-1"));
            out.write(HTTPHeader.DELIMINATOR);
            out.write(h.getValue().getBytes("ISO-8859-1"));
            out.write(HttpResponse.NEWLINE);
        }
        out.write(HttpResponse.NEWLINE);
        out.flush();
    }

    protected void setOutputStreamInUse(final boolean outputStreamInUse) {
        this.outputStreamInUse = outputStreamInUse;
    }

    @Override
    public String toString() {
        if (this.clientSocket != null) {
            return "HttpConnectionThread: " + this.clientSocket.toString();
        } else {
            return "HttpConnectionThread: IS and OS";
        }
    }
}
