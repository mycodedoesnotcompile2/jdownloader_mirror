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
import java.util.List;
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
import org.appwork.utils.Time;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;

/**
 * @author daniel
 *
 */
public class HttpServer extends AbstractServerBasics implements Runnable {
    public static final String                             APP_WORK_GMB_H_HTTP_SERVER = "AppWork GmbH HttpServer";
    private final int                                      wishPort;
    private final AtomicReference<List<ServerSocket>>      controlSockets             = new AtomicReference<List<ServerSocket>>(null);
    private volatile Thread                                serverThread               = null;
    private boolean                                        localhostOnly              = true;
    private boolean                                        debug                      = false;
    private boolean                                        verboseLog                 = false;
    private final CopyOnWriteArrayList<HttpRequestHandler> requestHandlers            = new CopyOnWriteArrayList<HttpRequestHandler>();
    private ThreadPoolExecutor                             threadPool;
    /**
     * Maximum thread pool size for concurrent HTTP connections.
     *
     * <p>
     * <b>Default: 20</b>
     * </p>
     *
     * <p>
     * <b>Rationale:</b> 20 threads provide a good compromise between parallelism and resource consumption. HTTP requests are typically
     * I/O-bound (network, file system), so a few threads can efficiently handle many concurrent connections. A higher value would consume
     * more memory and stack space per thread (typically ~1MB stack per thread) without providing significant benefit for I/O-bound
     * operations. For most use cases (local APIs, internal services) 20 is sufficient. For very high load or CPU-intensive handlers, this
     * value can be increased.
     * </p>
     *
     * <p>
     * <b>Note:</b> These default values are optimized for a local REST API HTTP server. If the server is used differently or as a real web
     * server, these values should be adjusted accordingly.
     * </p>
     */
    private int                                            maxThreadPoolSize          = 20;
    /**
     * Queue size for waiting requests in the thread pool.
     *
     * <p>
     * <b>Default: 100</b>
     * </p>
     *
     * <p>
     * <b>Rationale:</b> A queue size of 100 allows buffering of up to 100 waiting requests before new threads need to be created. This
     * prevents {@link RejectedExecutionException} during short load spikes and enables smooth load distribution. The memory overhead is
     * minimal (only references to Runnable objects). For very high load, this value can be increased to buffer more requests, though
     * {@link #maxThreadPoolSize} should also be adjusted accordingly to limit wait times in the queue.
     * </p>
     *
     * <p>
     * <b>Note:</b> These default values are optimized for a local REST API HTTP server. If the server is used differently or as a real web
     * server, these values should be adjusted accordingly.
     * </p>
     */
    private int                                            threadPoolQueueSize        = 100;
    /**
     * Keep-alive time for inactive threads in the pool (in milliseconds).
     *
     * <p>
     * <b>Default: 10000 (10 seconds)</b>
     * </p>
     *
     * <p>
     * <b>Rationale:</b> 10 seconds is a balanced value that automatically terminates threads under low load to save resources, while
     * keeping threads quickly available during active use. The time is long enough to bridge short pauses between request bursts (e.g., in
     * interactive applications), but short enough to avoid unnecessarily binding memory and system resources. Since
     * {@link ThreadPoolExecutor#allowCoreThreadTimeOut(boolean)} is set to {@code true}, core threads are also terminated after this time,
     * which is useful for local/internal servers that are not permanently under load.
     * </p>
     *
     * <p>
     * <b>Note:</b> These default values are optimized for a local REST API HTTP server. If the server is used differently or as a real web
     * server, these values should be adjusted accordingly.
     * </p>
     */
    private long                                           threadPoolKeepAliveTime    = 10000L;

    public HttpServer(final int port) {
        super("AppWork GmbH HttpServer");
        this.wishPort = port;
    }

    protected HttpConnectionRunnable createHttpConnection(Socket clientSocket) throws IOException {
        InputStream inputStream = clientSocket.getInputStream();
        HttpServerConnection con = new HttpServerConnection(this, clientSocket, inputStream, null);
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
            threadPool = new ThreadPoolExecutor(0, this.maxThreadPoolSize, this.threadPoolKeepAliveTime, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>(this.threadPoolQueueSize), new ThreadFactory() {
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
                        final HttpServerConnection connection = (r instanceof HttpServerConnection) ? (HttpServerConnection) r : null;
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
                                final long beforeAcceptTime = Time.systemIndependentCurrentJVMTimeMillis();
                                if (HttpServer.this.verboseLog) {
                                    LogV3.fine("HttpServer: Waiting for connection on " + controlSocket.getLocalSocketAddress());
                                }
                                final Socket clientSocket = controlSocket.accept();
                                final long acceptElapsed = Time.systemIndependentCurrentJVMTimeMillis() - beforeAcceptTime;
                                if (HttpServer.this.verboseLog) {
                                    LogV3.fine("HttpServer: Connection accepted from " + clientSocket.getRemoteSocketAddress() + " after SocketIDLE " + acceptElapsed + "ms");
                                }
                                boolean closeSocket = true;
                                try {
                                    // Check if connection is from localhost when server is bound to localhost only
                                    // TODO: Is this check meaningful/useful?
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
                                    final long createConnectionStartTime = Time.systemIndependentCurrentJVMTimeMillis();
                                    final HttpConnectionRunnable connection = HttpServer.this.createHttpConnection(clientSocket);
                                    final long createConnectionElapsed = Time.systemIndependentCurrentJVMTimeMillis() - createConnectionStartTime;
                                    if (HttpServer.this.verboseLog) {
                                        LogV3.fine("HttpServer: createHttpConnection completed in " + createConnectionElapsed + "ms");
                                    }
                                    if (connection != null) {
                                        final long executeStartTime = Time.systemIndependentCurrentJVMTimeMillis();
                                        threadPool.execute(connection);
                                        final long executeElapsed = Time.systemIndependentCurrentJVMTimeMillis() - executeStartTime;
                                        if (HttpServer.this.verboseLog) {
                                            LogV3.fine("HttpServer: threadPool.execute() completed in " + executeElapsed + "ms");
                                        }
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

    /**
     * @return the verboseLog
     */
    public boolean isVerboseLog() {
        return verboseLog;
    }

    /**
     * @param verboseLog
     *            the verboseLog to set
     */
    public void setVerboseLog(final boolean verboseLog) {
        this.verboseLog = verboseLog;
    }

    /**
     * Sets the maximum thread pool size for concurrent HTTP connections.
     *
     * <p>
     * This setting only takes effect on the next server start. Changes while the server is running have no effect.
     * </p>
     *
     * @param maxThreadPoolSize
     *            maximum number of threads (must be >= 1)
     * @throws IllegalArgumentException
     *             if maxThreadPoolSize < 1
     * @see #maxThreadPoolSize for details on default value and rationale
     */
    public void setMaxThreadPoolSize(final int maxThreadPoolSize) {
        if (maxThreadPoolSize < 1) {
            throw new IllegalArgumentException("Max thread pool size must be at least 1, was: " + maxThreadPoolSize);
        }
        this.maxThreadPoolSize = maxThreadPoolSize;
    }

    /**
     * Returns the currently configured maximum thread pool size.
     *
     * @return maximum thread pool size
     */
    public int getMaxThreadPoolSize() {
        return this.maxThreadPoolSize;
    }

    /**
     * Sets the queue size for waiting requests in the thread pool.
     *
     * <p>
     * This setting only takes effect on the next server start. Changes while the server is running have no effect.
     * </p>
     *
     * @param threadPoolQueueSize
     *            queue size (must be >= 1)
     * @throws IllegalArgumentException
     *             if threadPoolQueueSize < 1
     * @see #threadPoolQueueSize for details on default value and rationale
     */
    public void setThreadPoolQueueSize(final int threadPoolQueueSize) {
        if (threadPoolQueueSize < 1) {
            throw new IllegalArgumentException("Thread pool queue size must be at least 1, was: " + threadPoolQueueSize);
        }
        this.threadPoolQueueSize = threadPoolQueueSize;
    }

    /**
     * Returns the currently configured thread pool queue size.
     *
     * @return thread pool queue size
     */
    public int getThreadPoolQueueSize() {
        return this.threadPoolQueueSize;
    }

    /**
     * Sets the keep-alive time for inactive threads in the pool.
     *
     * <p>
     * This setting only takes effect on the next server start. Changes while the server is running have no effect.
     * </p>
     *
     * @param threadPoolKeepAliveTime
     *            keep-alive time in milliseconds (must be >= 0, 0 = threads are terminated immediately when inactive)
     * @throws IllegalArgumentException
     *             if threadPoolKeepAliveTime < 0
     * @see #threadPoolKeepAliveTime for details on default value and rationale
     */
    public void setThreadPoolKeepAliveTime(final long threadPoolKeepAliveTime) {
        if (threadPoolKeepAliveTime < 0) {
            throw new IllegalArgumentException("Thread pool keep-alive time must be non-negative, was: " + threadPoolKeepAliveTime);
        }
        this.threadPoolKeepAliveTime = threadPoolKeepAliveTime;
    }

    /**
     * Returns the currently configured keep-alive time for threads in the pool.
     *
     * @return keep-alive time in milliseconds
     */
    public long getThreadPoolKeepAliveTime() {
        return this.threadPoolKeepAliveTime;
    }

    public synchronized void shutdown() {
        shutDownControlSockets(null);
    }

    private int lastPort = -1;

    /**
     * Creates a new ServerSocket for the given socket address.
     * 
     * <p>
     * This method can be overridden by subclasses to create specialized server sockets (e.g., SSLServerSocket).
     * The default implementation creates a standard ServerSocket.
     * </p>
     * 
     * <p>
     * Note: The socket address parameter is provided for subclasses that may need it, but the default implementation
     * creates an unbound ServerSocket. The socket will be bound later using {@link ServerSocket#bind(SocketAddress)}.
     * </p>
     * 
     * @return A new ServerSocket instance
     * @throws IOException
     *             if the server socket cannot be created
     */
    protected ServerSocket createServerSocket() throws IOException {
        final ServerSocket serverSocket = new ServerSocket();
        serverSocket.setReuseAddress(true);
        return serverSocket;
    }

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
                    final ServerSocket controlSocket = this.createServerSocket();
                    try {
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
                final ServerSocket controlSocket = this.createServerSocket();
                controlSocket.bind(new InetSocketAddress(port));
                lastPort = controlSocket.getLocalPort();
                serverSockets.add(controlSocket);
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
}
