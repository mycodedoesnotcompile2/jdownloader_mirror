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
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Joiner;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.CountingInputStreamInterface;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.handler.ExtendedHttpRequestHandler;
import org.appwork.utils.net.httpserver.handler.HttpProxyHandler;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.ConnectRequest;
import org.appwork.utils.net.httpserver.requests.CopyRequest;
import org.appwork.utils.net.httpserver.requests.DeleteRequest;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HeadRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.HttpServerInterface;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.requests.LockRequest;
import org.appwork.utils.net.httpserver.requests.MkcolRequest;
import org.appwork.utils.net.httpserver.requests.MoveRequest;
import org.appwork.utils.net.httpserver.requests.MsearchRequest;
import org.appwork.utils.net.httpserver.requests.NotifyRequest;
import org.appwork.utils.net.httpserver.requests.OptionsRequest;
import org.appwork.utils.net.httpserver.requests.PatchRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.requests.PropfindRequest;
import org.appwork.utils.net.httpserver.requests.ProppatchRequest;
import org.appwork.utils.net.httpserver.requests.PutRequest;
import org.appwork.utils.net.httpserver.requests.RealGetRequest;
import org.appwork.utils.net.httpserver.requests.RealPostRequest;
import org.appwork.utils.net.httpserver.requests.SubscribeRequest;
import org.appwork.utils.net.httpserver.requests.TraceRequest;
import org.appwork.utils.net.httpserver.requests.UnlockRequest;
import org.appwork.utils.net.httpserver.requests.UnsubscribeRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author daniel
 *
 */
public class HttpConnection implements HttpConnectionRunnable, RawHttpConnectionInterface {
    // public static enum RequestMethod {
    // DELETE,
    // CONNECT,
    // PUT,
    // HEAD,
    // GET,
    // POST,
    // OPTIONS,
    // UNKNOWN;
    //
    // private final byte[] requestTypeBytes;
    //
    // private RequestMethod() {
    // byte[] bytes = null;
    // try {
    // bytes = this.name().getBytes("ISO-8859-1");
    // } catch (final Throwable e) {
    // bytes = this.name().getBytes();
    // }
    // this.requestTypeBytes = bytes;
    // }
    //
    // public final boolean isRequestType(final byte[] input) {
    // if (input.length < this.requestTypeBytes.length) {
    // return false;
    // }
    // for (int i = 0; i < this.requestTypeBytes.length; i++) {
    // if (this.requestTypeBytes[i] != input[i]) {
    // return false;
    // }
    // }
    // return true;
    // }
    //
    // public static RequestMethod get(final byte[] input) {
    // for (RequestMethod type : values()) {
    // if (type.isRequestType(input)) {
    // return type;
    // }
    // }
    // return RequestMethod.UNKNOWN;
    // }
    //
    // public final int length() {
    // return this.requestTypeBytes.length;
    // }
    // }

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

    protected final HttpServerInterface server;
    protected final Socket              clientSocket;
    protected boolean                   outputStreamInUse = false;
    protected HttpResponse              response          = null;
    protected InputStream               is;
    protected final OutputStream        os;
    protected HttpRequest               request;
    private volatile boolean            isActive;

    // Thread currently processing this connection (set in beforeExecute, cleared in afterExecute)
    private static final Pattern        METHOD            = Pattern.compile("(" + new Joiner("|").join(RequestMethod.values()) + ")");
    private static final Pattern        REQUESTLINE       = Pattern.compile("\\s+(.+)\\s+HTTP/");
    public static final Pattern         REQUESTURL        = Pattern.compile("^(/.*?)($|\\?)");
    public static final Pattern         REQUESTPARAM      = Pattern.compile("^/.*?\\?(.+)");

    protected HttpConnection(final HttpServerInterface server, final Socket clientSocket, final InputStream is, final OutputStream os) throws IOException {
        this.server = server;
        this.clientSocket = clientSocket;
        // fcgi clentsocket may be null
        if (is == null && clientSocket != null) {
            this.is = clientSocket.getInputStream();
        } else {
            this.is = is;
        }
        if (os == null && clientSocket != null) {
            this.os = clientSocket.getOutputStream();
        } else {
            this.os = os;
        }

        this.isActive = true;
        // Set socket timeout from server configuration
        final ConnectionTimeouts timeouts = getServer().getConnectionTimeouts();
        if (timeouts != null && timeouts.getSocketTimeoutMs() > 0) {
            this.clientSocket.setSoTimeout(timeouts.getSocketTimeoutMs());
        } else {
            // Fallback to default if not configured
            this.clientSocket.setSoTimeout(ConnectionTimeouts.DEFAULT_SOCKET_TIMEOUT_MS);
        }

    }

    public Socket getClientSocket() {
        return clientSocket;
    }

    /**
     * Returns whether this connection is still active. Package-private for use by HttpServer.
     *
     * @return true if connection is active, false otherwise
     */
    boolean isActive() {
        return this.isActive;
    }

    protected HttpConnection() {
        this(null);
    }

    public HttpConnection(final HttpServerInterface server, final Socket clientSocket) throws IOException {
        this(server, clientSocket, null, null);
    }

    /**
     * @param server2
     */
    protected HttpConnection(HttpServerInterface server) {
        this.server = server;
        this.clientSocket = null;
        this.is = null;
        this.os = null;
        this.isActive = false;
    }

    protected HttpRequest buildGetRequest() throws IOException {
        return new RealGetRequest(this);
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
        return new RealPostRequest(this);
    }

    protected HttpRequest buildPutRequest() throws IOException {
        return new PutRequest(this);
    }

    protected HttpRequest buildDeleteRequest() throws IOException {
        return new DeleteRequest(this);
    }

    protected HttpRequest buildTraceRequest() throws IOException {
        return new TraceRequest(this);
    }

    protected HttpRequest buildPatchRequest() throws IOException {
        return new PatchRequest(this);
    }

    protected HttpRequest buildPropfindRequest() throws IOException {
        return new PropfindRequest(this);
    }

    protected HttpRequest buildProppatchRequest() throws IOException {
        return new ProppatchRequest(this);
    }

    protected HttpRequest buildMkcolRequest() throws IOException {
        return new MkcolRequest(this);
    }

    protected HttpRequest buildCopyRequest() throws IOException {
        return new CopyRequest(this);
    }

    protected HttpRequest buildMoveRequest() throws IOException {
        return new MoveRequest(this);
    }

    protected HttpRequest buildLockRequest() throws IOException {
        return new LockRequest(this);
    }

    protected HttpRequest buildUnlockRequest() throws IOException {
        return new UnlockRequest(this);
    }

    protected HttpRequest buildNotifyRequest() throws IOException {
        return new NotifyRequest(this);
    }

    protected HttpRequest buildMsearchRequest() throws IOException {
        return new MsearchRequest(this);
    }

    protected HttpRequest buildSubscribeRequest() throws IOException {
        return new SubscribeRequest(this);
    }

    protected HttpRequest buildUnsubscribeRequest() throws IOException {
        return new UnsubscribeRequest(this);
    }

    /**
     * parses the request and creates a GET/POST-Request Object and fills it with all received data
     *
     * @return
     * @throws IOException
     */
    protected HttpRequest buildRequest() throws IOException {
        /* read request Method and Path */

        RequestSizeLimits limits;
        InputStream headerStream = is;
        limits = getServer().getRequestSizeLimits();
        if (limits != null) {

            int headerLimit = limits.getMaxHeaderSize();
            if (headerLimit > 0) {
                headerStream = new LimitedInputStreamWithException(headerStream, headerLimit);
            }
        }

        final String requestLine = this.parseRequestLine(headerStream);
        if (StringUtils.isEmpty(requestLine)) {
            throw new EmptyRequestException();
        }
        // TOTO: requestLine may be "" in some cases (chrome pre connection...?)
        final RequestMethod connectionType = this.parseConnectionType(requestLine);

        String requestedURL = new Regex(requestLine, HttpConnection.REQUESTLINE).getMatch(0);
        if (!StringUtils.startsWithCaseInsensitive(requestedURL, "/")) {
            requestedURL = "/" + StringUtils.valueOrEmpty(requestedURL);
        }
        String requestedPath = new Regex(requestedURL, HttpConnection.REQUESTURL).getMatch(0);

        final List<KeyValuePair> requestedURLParameters = this.parseRequestURLParams(requestedURL);
        /* read request Headers */
        final HeaderCollection requestHeaders = this.parseRequestHeaders(headerStream);
        ;
        ;
        ;
        // wrap for further post Reads
        if (!connectionType.mayHavePostBody) {
            // ensure that nobody tries to read any further...
            is = new LimitedInputStreamWithException(is, 0) {
                protected RequestSizeLimitExceededException createException() {
                    return new RequestSizeLimitExceededException("This Method does not support POST data!");
                };
            };

        } else {
            if (limits != null) {
                long postLimits = limits.getMaxPostBodySize();
                if (postLimits > 0) {
                    is = new LimitedInputStreamWithException(is, postLimits);
                }
            }
        }

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
        case TRACE:
            request = this.buildTraceRequest();
            break;
        case PATCH:
            request = this.buildPatchRequest();
            break;
        case PROPFIND:
            request = this.buildPropfindRequest();
            break;
        case PROPPATCH:
            request = this.buildProppatchRequest();
            break;
        case MKCOL:
            request = this.buildMkcolRequest();
            break;
        case COPY:
            request = this.buildCopyRequest();
            break;
        case MOVE:
            request = this.buildMoveRequest();
            break;
        case LOCK:
            request = this.buildLockRequest();
            break;
        case UNLOCK:
            request = this.buildUnlockRequest();
            break;
        case NOTIFY:
            request = this.buildNotifyRequest();
            break;
        case MSEARCH:
            request = this.buildMsearchRequest();
            break;
        case SUBSCRIBE:
            request = this.buildSubscribeRequest();
            break;
        case UNSUBSCRIBE:
            request = this.buildUnsubscribeRequest();
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
     * @param is2
     * @param class1
     * @return
     */
    public static <T extends InputStream> T getInputStreamByType(InputStream is, Class<T> class1) {
        while (is instanceof CountingInputStreamInterface) {
            if (class1.isAssignableFrom(is.getClass())) {
                return (T) is;
            }
            InputStream newIs = ((CountingInputStreamInterface) is).getParentInputStream();
            if (newIs == is) {
                DebugMode.debugger();
                break;
            }
            is = newIs;
        }
        return null;
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
        this.isActive = false;

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
        return this.getServer().getHandler();
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
            final InetAddress clientAddress = this.clientSocket.getInetAddress();
            remoteAddress.add(clientAddress.getHostAddress());

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

    public boolean onException(final Throwable e, final HttpRequest request, HttpResponse response) throws IOException {
        // create a fresh new Response Instance - just to be sure that we return the exception only. we do not know at this stage if the
        // existing response Instance already contains parts of an answer
        HTTPHeader originAllowedOrigin = response == null ? null : response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        HTTPHeader originAllowedHeader = response == null ? null : response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        HTTPHeader originAlloweMethods = response == null ? null : response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        HTTPHeader accessAge = response == null ? null : response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);

        this.response = response = new HttpResponse(this);
        if (originAllowedOrigin != null) {
            response.getResponseHeaders().add(originAllowedOrigin);
        }
        if (originAllowedHeader != null) {
            response.getResponseHeaders().add(originAllowedHeader);
        }
        if (originAlloweMethods != null) {
            response.getResponseHeaders().add(originAlloweMethods);
        }
        if (accessAge != null) {
            response.getResponseHeaders().add(accessAge);
        }
        this.response.setResponseCode(HTTPConstants.ResponseCode.SERVERERROR_INTERNAL);
        if (Exceptions.containsInstanceOf(e, SocketException.class, ClosedChannelException.class)) {
            // socket already closed (likely due to timeout or security limit)
            // Don't log as error - this is expected when timeouts trigger or security limits are exceeded
            return true;

        } else if (e instanceof HttpMethodNotAllowedException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            final String method = request != null && request.getRequestMethod() != null ? request.getRequestMethod().name() : "unknown";
            LogV3.warning("Security: HTTP method not allowed | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Method: " + method + " | Message: " + e.getMessage());
            this.response.setResponseCode(ResponseCode.METHOD_NOT_ALLOWED);
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = e.getMessage() != null ? e.getMessage() : "HTTP method not allowed";
                final byte[] bytes = message.getBytes("UTF-8");
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                this.response.getOutputStream(true).write(bytes);
                this.response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;

        } else if (request instanceof OptionsRequest) {
            this.response.setResponseCode(HTTPConstants.ResponseCode.ERROR_FORBIDDEN);
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, "0"));
            this.response.getOutputStream(true).flush();
            return true;
        } else if (e instanceof RequestSizeLimitExceededException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            LogV3.warning("Security: Request size limit exceeded | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Message: " + e.getMessage());
            this.response.setResponseCode(ResponseCode.REQUEST_ENTITY_TOO_LARGE);
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = "Request size limit exceeded!";
                final byte[] bytes = message.getBytes("UTF-8");
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                this.response.getOutputStream(true).write(bytes);
                this.response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;

        } else if (e instanceof ForbiddenOriginException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            LogV3.warning("Security: Forbidden origin detected | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Message: " + e.getMessage());
            this.response.setResponseCode(ResponseCode.ERROR_FORBIDDEN);
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = "Forbidden Origin!";
                final byte[] bytes = message.getBytes("UTF-8");
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                this.response.getOutputStream(true).write(bytes);
                this.response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;
        } else if (e instanceof ForbiddenHeaderException) {
            final String remoteAddress = request != null ? StringUtils.join(request.getRemoteAddress(), ", ") : "unknown";
            final String requestUrl = request != null ? request.getRequestedURL() : "unknown";
            LogV3.warning("Security: Forbidden header detected | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Message: " + e.getMessage());
            this.response.setResponseCode(ResponseCode.ERROR_BAD_REQUEST);
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
            try {
                final String message = "Forbidden Headers!";
                final byte[] bytes = message.getBytes("UTF-8");
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
                this.response.getOutputStream(true).write(bytes);
                this.response.getOutputStream(true).flush();
            } catch (final Throwable ignore) {
                // Ignore
            }
            return true;
        } else if (e instanceof HttpConnectionExceptionHandler) {
            return ((HttpConnectionExceptionHandler) e).handle(request, response);
        } else if (request != null) {
            this.response.setResponseCode(ResponseCode.SERVERERROR_INTERNAL);
            LogV3.log(e);
            // do not return stacktraces!
            final byte[] bytes = "Unexpected Exception".getBytes("UTF-8");
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text; charset=UTF-8"));
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, bytes.length + ""));
            this.response.getOutputStream(true).write(bytes);
            this.response.getOutputStream(true).flush();
            return true;
        } else {
            // too early - no request read so far
            LogV3.log(e);
            return true;
        }
    }

    protected void onUnhandled(final HttpRequest request, final HttpResponse response) throws IOException {
        response.setResponseCode(ResponseCode.SERVERERROR_NOT_IMPLEMENTED);
    }

    protected RequestMethod parseConnectionType(final String requestLine) throws IOException {
        final String method = new Regex(requestLine, HttpConnection.METHOD).getMatch(0);
        // TOTO: requestLine may be "" in some cases (chrome pre connection...?)
        try {
            return RequestMethod.valueOf(method);
        } catch (final Exception e) {
            return RequestMethod.UNKNOWN;
        }
    }

    protected HeaderCollection parseRequestHeaders(InputStream headerStream) throws IOException {
        final ByteBuffer headers = this.readRequestHeaders(headerStream);
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

    protected String parseRequestLine(InputStream headerStream) throws IOException {
        final ByteBuffer header = this.readRequestLine(headerStream);
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

    protected ByteBuffer readRequestHeaders(InputStream headerStream) throws IOException {
        return HTTPConnectionUtils.readheader(headerStream, false);
    }

    protected ByteBuffer readRequestLine(InputStream headerStream) throws IOException {
        return HTTPConnectionUtils.readheader(headerStream, true);
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
        // make sure that there is no interrupt flag set - e.g. an api impl may have interrupted itself...
        Thread.interrupted();
        boolean closeConnection = true;
        try {
            if (this.request == null) {
                this.request = this.buildRequest();
            }
            if (this.response == null) {
                this.response = this.buildResponse();
            }
            HeaderValidationRules headerRules = getServer().getHeaderValidationRules();
            if (headerRules != null && headerRules.isEnabled()) {
                if (!headerRules.isRequestAllowed(request.getRequestHeaders())) {
                    final String errorMessage = headerRules.getValidationError(request.getRequestHeaders());
                    final String remoteAddress = StringUtils.join(request.getRemoteAddress(), ", ");
                    final String requestUrl = request.getRequestedURL();
                    LogV3.warning("Security: Header validation failed | Remote: " + remoteAddress + " | URL: " + requestUrl + " | Error: " + errorMessage);
                    throw new ForbiddenHeaderException(errorMessage);
                }
            }

            final Set<RequestMethod> allowedMethods = this.getServer().getAllowedMethods();
            if (allowedMethods != null) {
                RequestMethod method = request.getRequestMethod();
                if (method == RequestMethod.UNKNOWN || !allowedMethods.contains(method)) {

                    throw new HttpMethodNotAllowedException("Illegal Method");
                }
            } else {
                // if null everything is allowed
            }

            // Origin validation is handled by the server instance (CorsHandler).
            // If CORS is disabled (corsHandler == null), reject any request that sends an Origin header.
            final HTTPHeader originHeader = request.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_ORIGIN);
            if (originHeader != null) {
                final String origin = originHeader.getValue();
                if (StringUtils.isEmpty(origin)) {
                    throw new ForbiddenOriginException("Empty Origin header rejected");
                }
                final CorsHandler corsHandler = this.getServer().getCorsHandler();
                final boolean allowed = corsHandler != null && corsHandler.isOriginAllowed(request);
                if (!allowed) {
                    throw new ForbiddenOriginException("Cross-Origin request rejected");
                }
            }

            final HttpRequest request = this.request;
            final HttpResponse response = this.response;

            if (this.deferRequest(request)) {
                closeConnection = false;
            } else {
                boolean handled = false;
                final boolean isPostRequest = isPostRequest(request);
                final boolean isGetRequest = isGetRequest(request);
                final boolean isProxyRequest = isProxyRequest(request);
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
                            if (handler instanceof HttpProxyHandler) {
                                handled = ((HttpProxyHandler) handler).onProxyConnectRequest(request, response);
                            } else {
                                throw new HttpMethodNotAllowedException("This server is not a proxy!");
                            }

                        } else {
                            throw new HttpMethodNotAllowedException("Unsupported RequestType");
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

                LogV3.warning("Error: Cannot Send HTTP Response!");
                LogV3.log(Exceptions.addSuppressed(e, nothing));
            }
        } finally {
            if (closeConnection) {
                this.closeConnection();
                this.close();
            }
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
        // Validate response headers before writing them
        // Validate security headers if configured
        ResponseSecurityHeaders responseSecurityHeaders = getServer().getResponseSecurityHeaders();
        if (responseSecurityHeaders != null) {
            responseSecurityHeaders.validateResponseHeaders(response.getResponseHeaders());
        }
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

    /**
     * Returns the HttpServer instance associated with this connection.
     *
     * @return The HttpServer instance
     */
    public HttpServerInterface getServer() {
        return this.server;
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
