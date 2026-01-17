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
import java.net.BindException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Exceptions;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.HttpServerInterface;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author daniel
 *
 */
public class HttpServer implements Runnable, HttpServerInterface {
    public static final String                             APP_WORK_GMB_H_HTTP_SERVER      = "AppWork GmbH HttpServer";

    private final int                                      wishPort;
    private final AtomicReference<List<ServerSocket>>      controlSockets                  = new AtomicReference<List<ServerSocket>>(null);
    private volatile Thread                                serverThread                    = null;
    private boolean                                        localhostOnly                   = true;
    private boolean                                        debug                           = false;
    private final CopyOnWriteArrayList<HttpRequestHandler> requestHandlers                 = new CopyOnWriteArrayList<HttpRequestHandler>();

    private RequestSizeLimits                              requestSizeLimits               = null;
    private HeaderValidationRules                          headerValidationRules           = null;
    private Set<RequestMethod>                             allowedMethods                  = null;
    private ResponseSecurityHeaders                        responseSecurityHeaders         = null;
    private CorsHandler                                    corsHandler                     = null;
    private ConnectionTimeouts                             connectionTimeouts              = null;
    private String                                         responseServerHeader            = APP_WORK_GMB_H_HTTP_SERVER;

    private ThreadPoolExecutor                             threadPool;

    /** Default maximum header size: 16 KB */
    private static final int                               DEFAULT_MAX_HEADER_SIZE         = 16 * 1024;
    /** Default maximum POST body size: 10 MB */
    private static final long                              DEFAULT_MAX_POST_BODY_SIZE      = 10 * 1024 * 1024;
    /** Default maximum processed POST data size: 50 MB (after decompression, decryption, etc.) */
    private static final long                              DEFAULT_MAX_POST_PROCESSED_SIZE = 50 * 1024 * 1024;

    public HttpServer(final int port) {
        this.wishPort = port;
        // Set default size limits
        this.requestSizeLimits = new RequestSizeLimits(DEFAULT_MAX_HEADER_SIZE, DEFAULT_MAX_POST_BODY_SIZE, DEFAULT_MAX_POST_PROCESSED_SIZE);

        // Set default header validation rules
        this.headerValidationRules = new HeaderValidationRules();
        // Set default allowed methods (only GET allowed by default)
        this.allowedMethods = EnumSet.of(RequestMethod.GET);
        // Set default security headers config
        this.responseSecurityHeaders = new ResponseSecurityHeaders();
        // Set default connection timeouts config
        this.connectionTimeouts = new ConnectionTimeouts();

    }

    protected HttpConnectionRunnable createHttpConnection(Socket clientSocket) throws IOException {
        InputStream inputStream = clientSocket.getInputStream();
        HttpConnection con = new HttpConnection(this, clientSocket, inputStream, null);

        return con;
    }

    @Override
    public List<HttpRequestHandler> getHandler() {
        return this.requestHandlers;
    }

    protected InetAddress[] getLocalHost() throws UnknownHostException {
        return HTTPConnectionUtils.getLoopback(IPVERSION.IPV4_IPV6);
    }

    /**
     * This method returns the actual port @see getActualPort() or the @see getWishedPort
     *
     * @return the port
     * @deprecated use {@link #getActualPort()} or {@link #getWishedPort()} instead
     */
    @Deprecated
    public int getPort() {
        try {
            return getActualPort();
        } catch (final Throwable e) {
        }
        return this.getWishedPort();
    }

    public int getActualPort() throws IllegalStateException {
        final List<ServerSocket> controlSockets = this.controlSockets.get();
        if (controlSockets != null) {
            return controlSockets.get(0).getLocalPort();
        } else {
            throw new IllegalStateException("Server not started yet");
        }
    }

    public int getWishedPort() {
        return wishPort;
    }

    /**
     * @return the debug
     */
    public boolean isDebug() {
        return this.debug;
    }

    /**
     * @return the localhostOnly
     */
    public boolean isLocalhostOnly() {
        return this.localhostOnly;
    }

    /**
     * Checks if the given IP address is a loopback (localhost) address.
     *
     * @param addr
     *            The InetAddress to check
     * @return true if the address is a loopback address, false otherwise
     */
    private boolean isFromLocalhost(final InetAddress addr) {
        if (addr == null) {
            return false;
        }
        return addr.isLoopbackAddress();
    }

    public boolean isRunning() {
        final Thread serverThread = this.serverThread;
        return controlSockets.get() != null && serverThread != null && serverThread.isAlive();
    }

    /*
     * to register a new handler we create a copy of current handlerList and then add new handler to it and set it as new handlerList. by
     * doing so, all current connections dont have to sync on their handlerlist
     */
    public HttpHandlerInfo registerRequestHandler(final HttpRequestHandler handler) {
        if (handler != null) {
            requestHandlers.addIfAbsent(handler);
        }
        return new HttpHandlerInfo(this, handler);
    }

    public List<SocketAddress> getLocalAddresses() {
        final List<ServerSocket> controlSockets = this.controlSockets.get();
        if (controlSockets != null) {
            final List<SocketAddress> ret = new ArrayList<SocketAddress>();
            for (final ServerSocket controlSocket : controlSockets) {
                final SocketAddress localAddress = controlSocket.getLocalSocketAddress();
                if (localAddress instanceof InetSocketAddress && ((InetSocketAddress) localAddress).getAddress() != null && "127.0.0.1".equals(((InetSocketAddress) localAddress).getAddress().getHostAddress())) {
                    ret.add(0, localAddress);
                } else {
                    ret.add(localAddress);
                }
            }
            // return 127.0.0.1 first as some tools/extensions might only listen to this local IP
            return ret;
        } else {
            return null;
        }
    }

    public String getServerAddress() {
        final List<SocketAddress> localAddresses = getLocalAddresses();
        if (localAddresses != null) {
            InetSocketAddress loInetSocketAddress = null;
            for (final SocketAddress localAddress : localAddresses) {
                if (localAddress instanceof InetSocketAddress) {
                    final InetSocketAddress inetSocketAddress = (InetSocketAddress) localAddress;
                    final InetAddress address = inetSocketAddress.getAddress();
                    if (address.isLoopbackAddress()) {
                        loInetSocketAddress = inetSocketAddress;
                        if ("127.0.0.1".equals(address.getHostAddress())) {
                            // we prefer 127.0.0.1 as some tools/extensions might only listen to this local IP
                            break;
                        }
                    }
                }
            }
            if (loInetSocketAddress != null && loInetSocketAddress.getAddress() != null) {
                final InetAddress addr = loInetSocketAddress.getAddress();
                final String ret;
                if (addr instanceof Inet6Address) {
                    ret = "[" + addr.getHostAddress() + "]:" + loInetSocketAddress.getPort();
                } else {
                    ret = addr.getHostAddress() + ":" + loInetSocketAddress.getPort();
                }
                return ret;
            }
        }
        return "127.0.0.1:" + getPort();
    }

    public void run() {
        final List<ServerSocket> controlSockets = this.controlSockets.get();

        try {
            if (controlSockets == null || controlSockets.size() == 0) {
                return;
            }
            final AtomicInteger threadsStarted = new AtomicInteger(0);
            threadPool = new ThreadPoolExecutor(0, 20, 10000l, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>(100), new ThreadFactory() {
                public Thread newThread(final Runnable r) {
                    final HttpConnectionThread ret = new HttpConnectionThread(HttpServer.this, r);
                    ret.setServerThreadID(threadsStarted.incrementAndGet());
                    ret.setName(HttpServer.this, null);
                    return ret;
                }
            }, new ThreadPoolExecutor.AbortPolicy()) {
                final ThreadPoolExecutor threadPool;
                {
                    threadPool = this;
                }

                @Override
                protected void afterExecute(Runnable r, Throwable t) {
                    if (Thread.currentThread() instanceof HttpConnectionThread) {
                        ((HttpConnectionThread) Thread.currentThread()).setName(HttpServer.this, null);
                        ((HttpConnectionThread) Thread.currentThread()).setCurrentConnection(null, null);
                    }
                }

                @Override
                protected void beforeExecute(final Thread t, final Runnable r) {
                    /*
                     * WORKAROUND for stupid SUN /ORACLE way of "how a threadpool should work" !
                     */

                    final int active = threadPool.getPoolSize();
                    final int max = threadPool.getMaximumPoolSize();
                    if (active < max) {
                        final int working = threadPool.getActiveCount();
                        if (working == active) {
                            /*
                             * we can increase max pool size so new threads get started
                             */
                            threadPool.setCorePoolSize(Math.min(max, active + 1));
                        }
                    }
                    if (t instanceof HttpConnectionThread) {
                        final HttpConnectionThread httpT = ((HttpConnectionThread) t);
                        final HttpConnection connection = (r instanceof HttpConnection) ? (HttpConnection) r : null;
                        final Socket socket = (r instanceof HttpConnectionRunnable) ? ((HttpConnectionRunnable) r).getClientSocket() : null;
                        ((HttpConnectionThread) t).setCurrentConnection(connection, socket);
                        httpT.setName(HttpServer.this, socket);

                    }

                    super.beforeExecute(t, r);
                }
            };
            threadPool.allowCoreThreadTimeOut(true);

            final List<Thread> controlSocketThreads = new ArrayList<Thread>();
            for (final ServerSocket controlSocket : controlSockets) {
                final Thread controlSocketThread = new Thread(Thread.currentThread().getName() + ":Listener:" + controlSocket.getLocalSocketAddress()) {
                    @Override
                    public void run() {
                        while (HttpServer.this.controlSockets.get() == controlSockets) {
                            try {
                                final Socket clientSocket = controlSocket.accept();
                                boolean closeSocket = true;
                                try {
                                    // Check if connection is from localhost when server is bound to localhost only
                                    if (HttpServer.this.localhostOnly) {
                                        final InetAddress clientAddress = clientSocket.getInetAddress();
                                        if (!HttpServer.this.isFromLocalhost(clientAddress)) {
                                            // Non-loopback address on localhost-only server - reject connection
                                            try {
                                                LogV3.warning("Security: Non-loopback connection rejected | Remote: " + clientSocket.getRemoteSocketAddress() + " | LocalhostOnly: " + HttpServer.this.localhostOnly + " | Closing connection");
                                                clientSocket.close();
                                            } catch (final Throwable ignore) {
                                            }
                                            closeSocket = false;
                                            continue;
                                        }
                                    }

                                    final HttpConnectionRunnable connection = HttpServer.this.createHttpConnection(clientSocket);

                                    if (connection != null) {
                                        threadPool.execute(connection);
                                        closeSocket = false;
                                    }
                                } catch (final RejectedExecutionException e) {
                                    LogV3.log(e);
                                } catch (final Throwable e) {
                                    LogV3.log(e);

                                } finally {
                                    if (closeSocket && clientSocket != null) {
                                        try {
                                            clientSocket.close();
                                        } catch (final Throwable e2) {
                                        }
                                    }
                                }
                            } catch (final SocketTimeoutException e) {
                                /*
                                 * nothing, our 5 mins connect timeout for the http server socket
                                 */
                            } catch (final IOException e) {
                                break;
                            }
                        }
                    }
                };
                controlSocketThreads.add(controlSocketThread);
                controlSocketThread.start();
            }
            for (final Thread controlSocketThread : controlSocketThreads) {
                try {
                    controlSocketThread.join();
                } catch (InterruptedException e) {
                }
            }
        } finally {
            shutDownControlSockets(controlSockets);
            if (threadPool != null) {
                final List<Runnable> waiting = threadPool.shutdownNow();
                if (waiting != null) {
                    /* close all waiting HttpConnections */
                    for (final Runnable runnable : waiting) {
                        try {
                            if (runnable instanceof HttpConnectionRunnable) {
                                ((HttpConnectionRunnable) runnable).getClientSocket().close();
                            }
                        } catch (final Throwable e) {
                        }
                    }
                }
            }
        }
    }

    /**
     * @param debug
     *            the debug to set
     */
    public void setDebug(final boolean debug) {
        this.debug = debug;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Runnable#run()
     */
    /**
     * @param localhostOnly
     *            the localhostOnly to set
     */
    public void setLocalhostOnly(final boolean localhostOnly) {
        this.localhostOnly = localhostOnly;
    }

    public synchronized void shutdown() {
        shutDownControlSockets(null);
    }

    private int lastPort = -1;

    /**
     *
     */

    public synchronized void start() throws IOException {
        List<ServerSocket> serverSockets = new ArrayList<ServerSocket>();
        try {
            final int port;
            if (lastPort != -1) {
                port = lastPort;
            } else {
                port = getWishedPort();
            }
            if (this.isLocalhostOnly()) {
                /* we only want localhost bound here */
                final List<BindException> bindExceptions = new ArrayList<BindException>();
                final InetAddress[] localhost = this.getLocalHost();
                int localPort = port;
                for (final InetAddress inetAddress : localhost) {
                    final SocketAddress socketAddress = new InetSocketAddress(inetAddress, localPort);
                    final ServerSocket controlSocket = new ServerSocket();
                    try {
                        controlSocket.setReuseAddress(true);
                        LogV3.info("Try to bind Server to " + socketAddress);
                        controlSocket.bind(socketAddress);
                        serverSockets.add(controlSocket);
                        localPort = controlSocket.getLocalPort();
                    } catch (BindException e) {
                        bindExceptions.add(Exceptions.addSuppressed(new BindException("cannot bind to:" + socketAddress), e));
                    } catch (SocketException e) {
                        // eg IPv6 on IPv4 stack only
                        bindExceptions.add(Exceptions.addSuppressed(new BindException("cannot bind to:" + socketAddress), e));
                    }
                }
                if (serverSockets.size() == 0) {
                    throw bindExceptions.get(0);
                } else {
                    lastPort = serverSockets.get(0).getLocalPort();
                }
            } else {
                final ServerSocket controlSocket = new ServerSocket(port);
                lastPort = controlSocket.getLocalPort();
                serverSockets.add(controlSocket);
                controlSocket.setReuseAddress(true);
            }
            shutDownControlSockets(null);
            if (controlSockets.compareAndSet(null, serverSockets)) {
                final Thread serverThread = new Thread(this);
                serverThread.setName("HttpServerThread|Port:" + getWishedPort() + "->" + getActualPort() + "|LocalHost:" + this.localhostOnly);
                this.serverThread = serverThread;
                LogV3.fine("Start HTTP Server. " + getWishedPort() + "->" + getActualPort() + "|LocalHost:" + this.localhostOnly);
                serverThread.start();
                serverSockets = null;
            } else {
                throw new IOException("Failed to start HTTP Server. " + getWishedPort() + "|LocalHost:" + this.localhostOnly);
            }
        } finally {
            if (serverSockets != null) {
                for (final ServerSocket controlSocket : serverSockets) {
                    try {
                        controlSocket.close();
                    } catch (IOException ignore) {
                    }
                }
            }
        }
    }

    protected List<ServerSocket> shutDownControlSockets(List<ServerSocket> compare) {
        final List<ServerSocket> shutDownControlSockets;
        if (compare != null) {
            if (this.controlSockets.compareAndSet(compare, null)) {
                shutDownControlSockets = compare;
            } else {
                shutDownControlSockets = null;
            }
        } else {
            shutDownControlSockets = this.controlSockets.getAndSet(null);
        }
        if (shutDownControlSockets != null) {
            for (final ServerSocket controlSocket : shutDownControlSockets) {
                try {
                    controlSocket.close();
                } catch (IOException ignore) {
                }
            }
        }
        return shutDownControlSockets;
    }

    public synchronized void stop() {
        shutDownControlSockets(null);
        lastPort = -1;
    }

    /*
     * to unregister a new handler we create a copy of current handlerList and then remove handler to it and set it as new handlerList. by
     * doing so, all current connections dont have to sync on their handlerlist
     */
    public void unregisterRequestHandler(final HttpRequestHandler handler) {
        if (handler != null) {
            requestHandlers.remove(handler);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.appwork.utils.net.httpserver.requests.HTTPBridge#canHandleChunkedEncoding(org.appwork.utils.net.httpserver.requests.HttpRequest,
     * org.appwork.utils.net.httpserver.responses.HttpResponse)
     */
    @Override
    public boolean isChunkedEncodedResponseAllowed(HttpRequest request, HttpResponse response) {
        return true;
    }

    /**
     * Sets the request size limits. Set to null to disable size limits.
     *
     * @param requestSizeLimits
     *            The request size limits instance, or null to disable
     */
    public void setRequestSizeLimits(final RequestSizeLimits requestSizeLimits) {
        this.requestSizeLimits = requestSizeLimits;
    }

    /**
     * Returns the request size limits configuration.
     *
     * @return the request size limits or null if size limits are disabled
     */
    @Override
    public RequestSizeLimits getRequestSizeLimits() {
        return this.requestSizeLimits;
    }

    /**
     * Sets the header validation rules. Set to null to disable header validation.
     *
     * @param headerValidationRules
     *            The header validation rules instance, or null to disable
     */
    public void setHeaderValidationRules(final HeaderValidationRules headerValidationRules) {
        this.headerValidationRules = headerValidationRules;
    }

    /**
     * Returns the header validation rules configuration.
     *
     * @return the header validation rules or null if header validation is disabled
     */
    @Override
    public HeaderValidationRules getHeaderValidationRules() {
        return this.headerValidationRules;
    }

    /**
     * Sets the allowed HTTP methods. Set to null to disable method validation.
     *
     * @param allowedMethods
     *            Set of allowed HTTP methods. If null or empty, method validation is disabled.
     */
    public void setAllowedMethods(final Set<RequestMethod> allowedMethods) {
        if (allowedMethods != null && !allowedMethods.isEmpty()) {
            this.allowedMethods = EnumSet.copyOf(allowedMethods);
        } else {
            this.allowedMethods = null;
        }
    }

    /**
     * Returns the set of allowed HTTP methods.
     *
     * @return the set of allowed methods or null if method validation is disabled
     */
    @Override
    public Set<RequestMethod> getAllowedMethods() {
        return this.allowedMethods != null ? EnumSet.copyOf(this.allowedMethods) : null;
    }

    /**
     * Sets the response security headers configuration. Set to null to disable default security headers.
     *
     * @param responseSecurityHeaders
     *            The response security headers instance, or null to disable
     */
    public void setResponseSecurityHeaders(final ResponseSecurityHeaders responseSecurityHeaders) {
        this.responseSecurityHeaders = responseSecurityHeaders;
    }

    /**
     * Returns the response security headers configuration.
     *
     * @return the response security headers configuration or null if security headers are disabled
     */
    @Override
    public ResponseSecurityHeaders getResponseSecurityHeaders() {
        return this.responseSecurityHeaders;
    }

    /**
     * Sets the connection timeouts configuration. Set to null to disable timeouts (not recommended).
     *
     * @param connectionTimeouts
     *            The connection timeouts instance, or null to disable
     */
    public void setConnectionTimeouts(final ConnectionTimeouts connectionTimeouts) {
        this.connectionTimeouts = connectionTimeouts;
    }

    /**
     * Returns the connection timeouts configuration.
     *
     * @return the connection timeouts configuration or null if timeouts are disabled
     */
    @Override
    public ConnectionTimeouts getConnectionTimeouts() {
        return this.connectionTimeouts;
    }

    /**
     * Sets the CORS handler. Set to null to disable CORS headers (default).
     *
     * @param corsHandler
     *            The CORS handler instance, or null to disable CORS
     */
    public void setCorsHandler(final CorsHandler corsHandler) {
        this.corsHandler = corsHandler;
    }

    /**
     * Returns the CORS handler.
     *
     * @return the CORS handler or null if CORS is disabled
     */
    @Override
    public CorsHandler getCorsHandler() {
        return this.corsHandler;
    }

    /**
     * Sets the Server response header value. Set to null to disable the Server header.
     *
     * @param responseServerHeader
     *            The Server header value, or null to disable
     */
    public void setResponseServerHeader(final String responseServerHeader) {
        this.responseServerHeader = responseServerHeader;
    }

    /**
     * Returns the Server response header value.
     *
     * @return the Server header value, or null if disabled
     */
    @Override
    public String getResponseServerHeader() {
        return this.responseServerHeader;
    }

}
