/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Joiner;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.ChunkedInputStream;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.CountingInputStreamInterface;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.LimitedInputStreamInterface;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.handler.ExtendedHttpRequestHandler;
import org.appwork.utils.net.httpserver.handler.HttpProxyHandler;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.AbstractGetRequest;
import org.appwork.utils.net.httpserver.requests.AbstractPostRequest;
import org.appwork.utils.net.httpserver.requests.ConnectRequest;
import org.appwork.utils.net.httpserver.requests.CopyRequest;
import org.appwork.utils.net.httpserver.requests.DeleteRequest;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HeadRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
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
import org.appwork.utils.net.httpserver.requests.SubscribeRequest;
import org.appwork.utils.net.httpserver.requests.TraceRequest;
import org.appwork.utils.net.httpserver.requests.UnlockRequest;
import org.appwork.utils.net.httpserver.requests.UnsubscribeRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author daniel
 *
 */
public class HttpServerConnection implements HttpConnectionRunnable, RawHttpConnectionInterface {
    @Deprecated
    // will be removed by daniel
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

        private final boolean isRequestType(final byte[] input) {
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

    protected final AbstractServerBasics server;
    protected final Socket               clientSocket;
    protected boolean                    outputStreamInUse = false;
    protected HttpResponse               response          = null;
    protected InputStream                is;
    protected final OutputStream         os;
    protected HttpRequest                request;
    private volatile boolean             isActive;

    // Thread currently processing this connection (set in beforeExecute, cleared in afterExecute)
    private static final Pattern         METHOD            = Pattern.compile("(" + new Joiner("|").join(RequestMethod.values()) + ")");
    private static final Pattern         REQUESTLINE       = Pattern.compile("\\s+(.+)\\s+HTTP/");
    public static final Pattern          REQUESTURL        = Pattern.compile("^(/.*?)($|\\?)");
    public static final Pattern          REQUESTPARAM      = Pattern.compile("^/.*?\\?(.+)");

    protected HttpServerConnection(final AbstractServerBasics server, final Socket clientSocket, final InputStream is, final OutputStream os) throws IOException {
        this.server = server;
        this.clientSocket = clientSocket;

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

    protected HttpServerConnection() {
        this(null);
    }

    public HttpServerConnection(final AbstractServerBasics server, final Socket clientSocket) throws IOException {
        this(server, clientSocket, null, null);
    }

    /**
     * @param server2
     */
    protected HttpServerConnection(AbstractServerBasics server) {
        this.server = server;
        this.clientSocket = null;
        this.is = null;
        this.os = null;
        this.isActive = false;
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
        final long buildRequestStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.buildRequest: Starting request parsing");
        }

        RequestSizeLimits limits;
        InputStream headerStream = is;
        limits = getServer().getRequestSizeLimits();
        if (limits != null) {

            long headerLimit = limits.getMaxHeaderSize();
            if (headerLimit > 0) {
                headerStream = new LimitedInputStreamWithException(headerStream, headerLimit);
            }
        }

        final long parseRequestLineStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        final String requestLine = this.parseRequestLine(headerStream);
        final long parseRequestLineElapsed = Time.systemIndependentCurrentJVMTimeMillis() - parseRequestLineStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.buildRequest: parseRequestLine() completed in " + parseRequestLineElapsed + "ms, requestLine: " + (requestLine != null ? requestLine.substring(0, Math.min(100, requestLine.length())) : "null"));
        }
        if (StringUtils.isEmpty(requestLine)) {
            throw new EmptyRequestException();
        }
        // TOTO: requestLine may be "" in some cases (chrome pre connection...?)
        final RequestMethod connectionType = this.parseConnectionType(requestLine);

        String requestedURL = new Regex(requestLine, HttpServerConnection.REQUESTLINE).getMatch(0);
        if (!StringUtils.startsWithCaseInsensitive(requestedURL, "/")) {
            requestedURL = "/" + StringUtils.valueOrEmpty(requestedURL);
        }
        String requestedPath = new Regex(requestedURL, HttpServerConnection.REQUESTURL).getMatch(0);

        final long parseRequestURLParamsStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        final List<KeyValuePair> requestedURLParameters = this.parseRequestURLParams(requestedURL);
        final long parseRequestURLParamsElapsed = Time.systemIndependentCurrentJVMTimeMillis() - parseRequestURLParamsStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.buildRequest: parseRequestURLParams() completed in " + parseRequestURLParamsElapsed + "ms");
        }
        /* read request Headers */
        final long parseRequestHeadersStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        final HeaderCollection requestHeaders = this.parseRequestHeaders(headerStream);
        final long parseRequestHeadersElapsed = Time.systemIndependentCurrentJVMTimeMillis() - parseRequestHeadersStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.buildRequest: parseRequestHeaders() completed in " + parseRequestHeadersElapsed + "ms, header count: " + (requestHeaders != null ? requestHeaders.size() : 0));
        }

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
        request.setServer(server);
        /* parse remoteClientAddresses */
        request.setRemoteAddress(this.getRemoteAddress(requestHeaders));
        request.setRequestedURLParameters(requestedURLParameters);
        request.setRequestedPath(requestedPath);
        request.setRequestedURL(requestedURL);
        request.setRequestHeaders(requestHeaders);
        final long buildRequestElapsed = Time.systemIndependentCurrentJVMTimeMillis() - buildRequestStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.buildRequest: buildRequest() completed in " + buildRequestElapsed + "ms");
        }
        return request;

    }

    /**
     * @param is2
     * @param class1
     * @return
     */
    public static <T extends InputStream> T getInputStreamByType(InputStream is, Class<T> class1) {
        if (class1.isAssignableFrom(is.getClass())) {
            return (T) is;
        }
        while (is instanceof CountingInputStreamInterface) {

            InputStream newIs = ((CountingInputStreamInterface) is).getParentInputStream();
            if (newIs == is) {
                DebugMode.debugger();
                break;
            }

            is = newIs;
            if (class1.isAssignableFrom(is.getClass())) {
                return (T) is;
            }
        }
        return null;
    }

    /**
     * @return
     */
    protected HttpResponse buildResponse() throws IOException {
        HttpResponse res = new HttpResponse(this);

        return res;
    }

    /**
     * @param request
     * @param res
     */
    void configure(HttpRequest request, HttpResponse response) {
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_CONNECTION, "close"));

        AbstractServerBasics server = getServer();
        {
            String serverHeader = server.getResponseServerHeader();

            if (!StringUtils.isEmpty(serverHeader)) {
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_SERVER, serverHeader));
            }
        }
        {
            ResponseSecurityHeaders securityHeaders = server.getResponseSecurityHeaders();

            if (securityHeaders != null) {
                securityHeaders.addSecurityHeaders(response.getResponseHeaders());
            }
        }
        if (request != null) {
            CorsHandler corsHandler = server.getCorsHandler();
            // Only add CORS headers if CORS is enabled
            if (corsHandler != null) {
                corsHandler.addCorsHeaders(request.getRequestHeaders(), response.getResponseHeaders());
            }
        }

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

    /**
     * Drains exactly the specified number of bytes from the input stream to prevent TCP RST. This is used when we know exactly how much
     * data the client will send (e.g. from Content-Length header).
     *
     * @param bytesToDrain
     *            Number of bytes to read and discard
     */
    public void drainContentLength(long bytesToDrain) {

        if (bytesToDrain <= 0) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Nothing to drain (bytesToDrain = " + bytesToDrain + "), skipping");
            }
            return;
        }
        if (this.is == null || this.clientSocket == null) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Cannot drain: input stream or socket is null");
            }
            return;
        }
        final RequestSizeLimits limits = getServer().getRequestSizeLimits();
        if (limits == null) {
            return;
        }
        if (!limits.isDrainInputStreamEnabled()) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Draining is disabled");
            }
            return;
        }
        long breakAt = 0;
        final long timeout = limits.getDrainInputStreamTimeout();
        if (timeout > 0) {
            breakAt = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        }
        final long maxDrain = limits.getMaxDrainInputStreamBytes();
        // Check if we would exceed drain limits
        if (maxDrain > 0 && bytesToDrain > maxDrain) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Content-Length " + bytesToDrain + " exceeds max drain limit " + maxDrain + ". Skipping drain to avoid long wait.");
            }
            return;
        }
        try {
            final long drainStartTime = Time.systemIndependentCurrentJVMTimeMillis();
            if (isVerboseLogEnabled()) {
                LogV3.fine("Draining exactly " + bytesToDrain + " bytes from Content-Length");
            }
            final byte[] buffer = new byte[8192];
            long totalRead = 0;
            int len;
            InputStream stream = is;
            // do not read from the limited InputStream.class..Cause.ACTIVATION..limits limited..
            while (stream instanceof CountingInputStream) {
                stream = ((CountingInputStream) stream).getParentInputStream();
            }
            while (totalRead < bytesToDrain && (len = stream.read(buffer, 0, (int) Math.min(buffer.length, bytesToDrain - totalRead))) != -1) {
                totalRead += len;
                if (breakAt > 0 && Time.systemIndependentCurrentJVMTimeMillis() > breakAt) {
                    final long drainElapsed = Time.systemIndependentCurrentJVMTimeMillis() - drainStartTime;
                    if (isVerboseLogEnabled()) {
                        LogV3.fine("Drained " + totalRead + " of " + bytesToDrain + " bytes. Break after timeout: " + timeout + " (draining took: " + drainElapsed + "ms)");
                    }
                    break;
                }
            }
            if (totalRead > 0) {
                final long drainElapsed = Time.systemIndependentCurrentJVMTimeMillis() - drainStartTime;
                if (isVerboseLogEnabled()) {
                    LogV3.fine("Drained " + totalRead + " of " + bytesToDrain + " bytes from input stream to prevent TCP RST (draining took: " + drainElapsed + "ms)");
                }
            }
        } catch (final java.net.SocketTimeoutException e) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Drain stopped due to socket timeout");
            }
        } catch (final Throwable e) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Drain stopped due to exception: " + e.getMessage());
            }
        }
    }

    /**
     * Drains a chunked input stream until EOF (end of chunks). Reads all remaining chunks until the stream signals EOF (read() returns -1).
     * Respects drain limits (maxDrain, timeout) from RequestSizeLimits.
     */
    public void drainChunked(InputStream chunkedStream) {
        if (chunkedStream == null || this.clientSocket == null) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Cannot drain chunked: input stream or socket is null");
            }
            return;
        }
        final RequestSizeLimits limits = getServer().getRequestSizeLimits();
        if (limits == null) {
            return;
        }
        if (!limits.isDrainInputStreamEnabled()) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Draining is disabled");
            }
            return;
        }
        long breakAt = 0;
        final long timeout = limits.getDrainInputStreamTimeout();
        if (timeout > 0) {
            breakAt = Time.systemIndependentCurrentJVMTimeMillis() + timeout;
        }
        final long maxDrain = limits.getMaxDrainInputStreamBytes();
        try {
            final long drainStartTime = Time.systemIndependentCurrentJVMTimeMillis();
            if (isVerboseLogEnabled()) {
                LogV3.fine("Draining chunked stream until EOF");
            }
            final byte[] buffer = new byte[8192];
            long totalRead = 0;
            int len;
            // Read until EOF (read() returns -1) or until limits are reached
            while ((len = chunkedStream.read(buffer)) != -1) {
                totalRead += len;
                // Check maxDrain limit
                if (maxDrain > 0 && totalRead > maxDrain) {
                    final long drainElapsed = Time.systemIndependentCurrentJVMTimeMillis() - drainStartTime;
                    if (isVerboseLogEnabled()) {
                        LogV3.fine("Drained " + totalRead + " bytes from chunked stream. Break after exceeding max drain limit: " + maxDrain + " (draining took: " + drainElapsed + "ms)");
                    }
                    break;
                }
                // Check timeout
                if (breakAt > 0 && Time.systemIndependentCurrentJVMTimeMillis() > breakAt) {
                    final long drainElapsed = Time.systemIndependentCurrentJVMTimeMillis() - drainStartTime;
                    if (isVerboseLogEnabled()) {
                        LogV3.fine("Drained " + totalRead + " bytes from chunked stream. Break after timeout: " + timeout + " (draining took: " + drainElapsed + "ms)");
                    }
                    break;
                }
            }
            if (totalRead > 0) {
                final long drainElapsed = Time.systemIndependentCurrentJVMTimeMillis() - drainStartTime;
                if (isVerboseLogEnabled()) {
                    LogV3.fine("Drained " + totalRead + " bytes from chunked stream to prevent TCP RST (draining took: " + drainElapsed + "ms)");
                }
            } else {
                final long drainElapsed = Time.systemIndependentCurrentJVMTimeMillis() - drainStartTime;
                if (isVerboseLogEnabled()) {
                    LogV3.fine("Drained chunked stream until EOF (no remaining data, draining took: " + drainElapsed + "ms)");
                }
            }
        } catch (final java.net.SocketTimeoutException e) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Drain chunked stopped due to socket timeout");
            }
        } catch (final Throwable e) {
            if (isVerboseLogEnabled()) {
                LogV3.fine("Drain chunked stopped due to exception: " + e.getMessage());
            }
        }
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

    protected void onUnhandled(final HttpRequest request, final HttpResponse response) throws IOException {
        response.setResponseCode(ResponseCode.SERVERERROR_NOT_IMPLEMENTED);
    }

    protected RequestMethod parseConnectionType(final String requestLine) throws IOException {
        final String method = new Regex(requestLine, HttpServerConnection.METHOD).getMatch(0);
        // TOTO: requestLine may be "" in some cases (chrome pre connection...?)
        try {
            return RequestMethod.valueOf(method);
        } catch (final Exception e) {
            return RequestMethod.UNKNOWN;
        }
    }

    protected HeaderCollection parseRequestHeaders(InputStream headerStream) throws IOException {
        final long readRequestHeadersStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.parseRequestHeaders: Starting readRequestHeaders()");
        }
        final ByteBuffer headers = this.readRequestHeaders(headerStream);
        final long readRequestHeadersElapsed = Time.systemIndependentCurrentJVMTimeMillis() - readRequestHeadersStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.parseRequestHeaders: readRequestHeaders() completed in " + readRequestHeadersElapsed + "ms, header size: " + (headers != null ? headers.limit() : 0) + " bytes");
        }
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
        final long readRequestLineStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.parseRequestLine: Starting readRequestLine()");
        }
        final ByteBuffer header = this.readRequestLine(headerStream);
        final long readRequestLineElapsed = Time.systemIndependentCurrentJVMTimeMillis() - readRequestLineStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.parseRequestLine: readRequestLine() completed in " + readRequestLineElapsed + "ms, line size: " + (header != null ? header.limit() : 0) + " bytes");
        }
        if (header.hasArray()) {
            return this.preProcessRequestLine(new String(header.array(), header.arrayOffset(), header.limit(), "ISO-8859-1").trim());
        } else {
            final byte[] bytesRequestLine = new byte[header.limit()];
            header.get(bytesRequestLine);
            return this.preProcessRequestLine(new String(bytesRequestLine, "ISO-8859-1").trim());
        }
    }

    protected List<KeyValuePair> parseRequestURLParams(final String requestURL) throws IOException {
        return HttpServerConnection.parseParameterList(new Regex(requestURL, HttpServerConnection.REQUESTPARAM).getMatch(0));
    }

    protected String preProcessRequestLine(final String requestLine) throws IOException {
        return requestLine;
    }

    protected ByteBuffer readRequestHeaders(InputStream headerStream) throws IOException {
        final long readheaderStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.readRequestHeaders: Starting HTTPConnectionUtils.readheader() for headers");
        }
        final ByteBuffer result = HTTPConnectionUtils.readheader(headerStream, false);
        final long readheaderElapsed = Time.systemIndependentCurrentJVMTimeMillis() - readheaderStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.readRequestHeaders: HTTPConnectionUtils.readheader() completed in " + readheaderElapsed + "ms");
        }
        return result;
    }

    protected ByteBuffer readRequestLine(InputStream headerStream) throws IOException {
        final long readheaderStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.readRequestLine: Starting HTTPConnectionUtils.readheader() for request line");
        }
        final ByteBuffer result = HTTPConnectionUtils.readheader(headerStream, true);
        final long readheaderElapsed = Time.systemIndependentCurrentJVMTimeMillis() - readheaderStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.readRequestLine: HTTPConnectionUtils.readheader() completed in " + readheaderElapsed + "ms");
        }
        return result;
    }

    protected boolean isProxyRequest(HttpRequest request) {
        if (request != null) {
            return request instanceof ConnectRequest || StringUtils.startsWithCaseInsensitive(request.getRequestedURL(), "http://") || StringUtils.startsWithCaseInsensitive(request.getRequestedURL(), "https://");
        } else {
            return false;
        }
    }

    protected boolean isPostRequest(HttpRequest request) {
        return request instanceof AbstractPostRequest;
    }

    protected boolean isGetRequest(HttpRequest request) {
        return request instanceof AbstractGetRequest;
    }

    @Override
    public void run() {
        // make sure that there is no interrupt flag set - e.g. an api impl may have interrupted itself...
        Thread.interrupted();
        final long requestStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("HttpConnection.run: Starting request processing");
        }
        boolean closeConnection = true;
        try {
            final HttpRequest request;
            final HttpResponse response;
            try {
                if (this.response == null) {
                    final long buildResponseStartTime = Time.systemIndependentCurrentJVMTimeMillis();
                    this.response = this.buildResponse();
                    final long buildResponseElapsed = Time.systemIndependentCurrentJVMTimeMillis() - buildResponseStartTime;
                    if (isVerboseLogEnabled()) {
                        LogV3.fine("HttpConnection.run: buildResponse() completed in " + buildResponseElapsed + "ms");
                    }
                }
                if (this.request == null) {
                    final long buildRequestStartTime = Time.systemIndependentCurrentJVMTimeMillis();
                    if (isVerboseLogEnabled()) {
                        LogV3.fine("HttpConnection.run: Starting buildRequest()");
                    }
                    this.request = this.buildRequest();
                    final long buildRequestElapsed = Time.systemIndependentCurrentJVMTimeMillis() - buildRequestStartTime;
                    if (isVerboseLogEnabled()) {
                        LogV3.fine("HttpConnection.run: buildRequest() completed in " + buildRequestElapsed + "ms");
                    }
                }
            } finally {
                request = this.request;
                response = this.response;

                final long configureStartTime = Time.systemIndependentCurrentJVMTimeMillis();
                configure(request, response);
                final long configureElapsed = Time.systemIndependentCurrentJVMTimeMillis() - configureStartTime;
                if (isVerboseLogEnabled()) {
                    LogV3.fine("HttpConnection.run: configure() completed in " + configureElapsed + "ms");
                }
            }
            final long beforeValidateTime = Time.systemIndependentCurrentJVMTimeMillis();
            if (isVerboseLogEnabled()) {
                LogV3.fine("HttpConnection.run: Request built in " + (beforeValidateTime - requestStartTime) + "ms, calling validateRequest");
            }

            server.validateRequest(request);
            final long afterValidateTime = Time.systemIndependentCurrentJVMTimeMillis();
            if (isVerboseLogEnabled()) {
                LogV3.fine("HttpConnection.run: validateRequest completed in " + (afterValidateTime - beforeValidateTime) + "ms");
            }

            if (this.deferRequest(request)) {
                closeConnection = false;
            } else {
                // Handle OPTIONS requests automatically if CORS is configured
                if (request instanceof OptionsRequest) {
                    final CorsHandler corsHandler = server.getCorsHandler();
                    if (corsHandler != null && corsHandler.answerOptionsRequest((OptionsRequest) request, response)) {
                        // CORS is configured and origin is allowed - OPTIONS preflight request handled
                        // Response is already prepared by answerOptionsRequest()
                        response.getOutputStream(true).flush();
                        closeConnection = true;
                        return;
                    }
                    // If no CORS handler is configured or origin is not allowed, fall through to normal handler processing
                }

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
                            handled = handler.onPostRequest((AbstractPostRequest) request, response);
                        } else if (isGetRequest) {
                            handled = handler.onGetRequest((AbstractGetRequest) request, response);
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
                final long beforeResponseTime = Time.systemIndependentCurrentJVMTimeMillis();
                response.getOutputStream(true);
                final long afterResponseTime = Time.systemIndependentCurrentJVMTimeMillis();
                if (isVerboseLogEnabled()) {
                    LogV3.fine("HttpConnection.run: Response sent in " + (afterResponseTime - beforeResponseTime) + "ms");
                }
            }
        } catch (final Throwable e) {
            final long exceptionTime = Time.systemIndependentCurrentJVMTimeMillis();
            if (isVerboseLogEnabled()) {
                LogV3.fine("HttpConnection.run: Exception caught after " + (exceptionTime - requestStartTime) + "ms: " + e.getClass().getSimpleName() + ": " + e.getMessage());
            }
            try {

                final AbstractServerBasics server = this.getServer();
                this.response = createErrorResponse();
                final long beforeOnExceptionTime = Time.systemIndependentCurrentJVMTimeMillis();
                closeConnection = server.onException(e, this.request, this.response);
                final long afterOnExceptionTime = Time.systemIndependentCurrentJVMTimeMillis();
                if (isVerboseLogEnabled()) {
                    LogV3.fine("HttpConnection.run: onException completed in " + (afterOnExceptionTime - beforeOnExceptionTime) + "ms");
                }

            } catch (final Throwable nothing) {

                LogV3.warning("Error: Cannot Send HTTP Response!");
                LogV3.log(Exceptions.addSuppressed(e, nothing));
            }
        } finally {
            final long beforeFinallyTime = Time.systemIndependentCurrentJVMTimeMillis();
            final long totalRequestTime = beforeFinallyTime - requestStartTime;
            if (isVerboseLogEnabled()) {
                LogV3.fine("HttpConnection.run: Request processing completed in " + totalRequestTime + "ms, closeConnection=" + closeConnection);
            }

            if (closeConnection) {
                final long beforeDrainingTime = Time.systemIndependentCurrentJVMTimeMillis();
                if (isVerboseLogEnabled()) {
                    LogV3.fine("HttpConnection.run: Calling handleDraining");
                }
                handleDraining();
                final long afterDrainingTime = Time.systemIndependentCurrentJVMTimeMillis();
                if (isVerboseLogEnabled()) {
                    LogV3.fine("HttpConnection.run: handleDraining completed in " + (afterDrainingTime - beforeDrainingTime) + "ms");
                }

                this.closeConnection();

                this.close();
            }
            final long totalTime = Time.systemIndependentCurrentJVMTimeMillis() - requestStartTime;
            if (isVerboseLogEnabled()) {
                LogV3.fine("HttpConnection.run: Total request handling time: " + totalTime + "ms");
            }
        }
    }

    /**
     *
     */
    protected void handleDraining() {
        final long handleDrainingStartTime = Time.systemIndependentCurrentJVMTimeMillis();
        if (isVerboseLogEnabled()) {
            LogV3.fine("handleDraining: Starting");
        }
        if (this.clientSocket != null) {
            try {
                this.clientSocket.shutdownOutput();
                if (isVerboseLogEnabled()) {
                    LogV3.fine("handleDraining: shutdownOutput() called");
                }
            } catch (final Throwable ignore) {
                if (isVerboseLogEnabled()) {
                    LogV3.fine("handleDraining: shutdownOutput() failed: " + ignore.getMessage());
                }
            }

        }

        try {

            InputStream inp = getInputStream();

            // reset limits
            if (inp instanceof LimitedInputStreamInterface) {
                ((LimitedInputStreamInterface) inp).setLimit(-1);
            }
            while (inp instanceof CountingInputStreamInterface) {

                InputStream newIs = ((CountingInputStreamInterface) inp).getParentInputStream();
                if (newIs == inp) {
                    DebugMode.debugger();
                    break;
                }

                inp = newIs;
                if (inp instanceof LimitedInputStreamInterface) {
                    ((LimitedInputStreamInterface) inp).setLimit(-1);
                }
            }
            if (request instanceof AbstractPostRequest) {
                HTTPHeader header = request.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_CONTENT_LENGTH);
                if (header != null) {
                    long cl = Long.parseLong(header.getValue());
                    CountingInputStream inputStream;

                    inputStream = getInputStreamByType(request.getConnection().getInputStream(), CountingInputStream.class);

                    if (inputStream != null) {
                        long remaining = cl - inputStream.transferedBytes();
                        if (isVerboseLogEnabled()) {
                            LogV3.fine("handleDraining: Content-Length=" + cl + ", already read=" + inputStream.transferedBytes() + ", remaining=" + remaining);
                        }
                        if (remaining > 0) {
                            if (isVerboseLogEnabled()) {
                                LogV3.fine("handleDraining: Calling drainContentLength(" + remaining + ")");
                            }
                            drainContentLength(remaining);
                            final long handleDrainingElapsed = Time.systemIndependentCurrentJVMTimeMillis() - handleDrainingStartTime;
                            if (isVerboseLogEnabled()) {
                                LogV3.fine("handleDraining: drainContentLength completed, total handleDraining time: " + handleDrainingElapsed + "ms");
                            }
                        } else {
                            if (isVerboseLogEnabled()) {
                                LogV3.fine("handleDraining: No draining needed (remaining=" + remaining + ")");
                            }
                        }
                    } else {
                        if (isVerboseLogEnabled()) {
                            LogV3.fine("handleDraining: CountingInputStream not found, cannot drain");
                        }
                    }
                } else {
                    // Check for chunked transfer encoding
                    HTTPHeader transferEncoding = request.getRequestHeaders().get(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING);
                    if (transferEncoding != null && "chunked".equalsIgnoreCase(transferEncoding.getValue())) {
                        if (isVerboseLogEnabled()) {
                            LogV3.fine("handleDraining: Chunked transfer encoding detected, draining chunked stream");
                        }
                        try {
                            // Get the InputStream from the request (should be a ChunkedInputStream)
                            InputStream requestInputStream = ((AbstractPostRequest) request).getInputStream();
                            if (requestInputStream != null) {
                                // Find ChunkedInputStream in the stream chain
                                ChunkedInputStream chunkedStream = getInputStreamByType(requestInputStream, ChunkedInputStream.class);

                                if (isVerboseLogEnabled()) {
                                    LogV3.fine("handleDraining: Found ChunkedInputStream, calling drainChunked()");
                                }
                                drainChunked(chunkedStream);
                                final long handleDrainingElapsed = Time.systemIndependentCurrentJVMTimeMillis() - handleDrainingStartTime;
                                if (isVerboseLogEnabled()) {
                                    LogV3.fine("handleDraining: drainChunked completed, total handleDraining time: " + handleDrainingElapsed + "ms");
                                }

                            } else {
                                if (isVerboseLogEnabled()) {
                                    LogV3.fine("handleDraining: Request InputStream is null, cannot drain chunked");
                                }
                            }
                        } catch (Exception e) {
                            if (isVerboseLogEnabled()) {
                                LogV3.fine("handleDraining: Exception while draining chunked stream: " + e.getMessage());
                            }
                            LogV3.log(e);
                        }
                    } else {
                        if (isVerboseLogEnabled()) {
                            LogV3.fine("handleDraining: No Content-Length header and no chunked transfer encoding");
                        }
                    }
                }
            } else {
                if (isVerboseLogEnabled()) {
                    LogV3.fine("handleDraining: Not a POST request, no draining needed");
                }
            }
        } catch (Exception e) {
            final long handleDrainingElapsed = Time.systemIndependentCurrentJVMTimeMillis() - handleDrainingStartTime;
            if (isVerboseLogEnabled()) {
                LogV3.fine("handleDraining: Exception after " + handleDrainingElapsed + "ms: " + e.getMessage());
            }
            LogV3.log(e);
        }
        final long handleDrainingElapsed = Time.systemIndependentCurrentJVMTimeMillis() - handleDrainingStartTime;
        if (isVerboseLogEnabled()) {
            LogV3.fine("handleDraining: Completed in " + handleDrainingElapsed + "ms");
        }
    }

    /**
     * @return
     */
    private boolean isVerboseLogEnabled() {
        return getServer().isVerboseLogEnabled();
    }

    /**
     * @return
     * @throws IOException
     *
     */
    public HttpResponse createErrorResponse() throws IOException {
        // create a fresh new Response Instance - just to be sure that we return the exception only. we do not know at this stage if the
        // existing response Instance already contains parts of an answer
        // DISCUSS: keep existing headers, or re-configure a new response
        // HTTPHeader originAllowedOrigin = response == null ? null :
        // response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_ORIGIN);
        // HTTPHeader originAllowedHeader = response == null ? null :
        // response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_HEADERS);
        // HTTPHeader originAlloweMethods = response == null ? null :
        // response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_ALLOW_METHODS);
        // HTTPHeader accessAge = response == null ? null :
        // response.getResponseHeaders().get(HTTPConstants.HEADER_RESPONSE_ACCESS_CONTROL_MAX_AGE);
        //
        HttpResponse ret = buildResponse();
        // if (originAllowedOrigin != null) {
        // ret.getResponseHeaders().add(originAllowedOrigin);
        // }
        // if (originAllowedHeader != null) {
        // ret.getResponseHeaders().add(originAllowedHeader);
        // }
        // if (originAlloweMethods != null) {
        // ret.getResponseHeaders().add(originAlloweMethods);
        // }
        // if (accessAge != null) {
        // ret.getResponseHeaders().add(accessAge);
        // }

        configure(request, ret);

        ret.setResponseCode(HTTPConstants.ResponseCode.SERVERERROR_INTERNAL);
        return ret;
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
    public AbstractServerBasics getServer() {
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
