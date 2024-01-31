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
import java.net.BindException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
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
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.HTTPBridge;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * @author daniel
 *
 */
public class HttpServer implements Runnable, HTTPBridge {
    private final int                                      wishPort;
    private final AtomicReference<List<ServerSocket>>      controlSockets  = new AtomicReference<List<ServerSocket>>(null);
    private volatile Thread                                serverThread    = null;
    private boolean                                        localhostOnly   = false;
    private boolean                                        debug           = false;
    private final CopyOnWriteArrayList<HttpRequestHandler> requestHandlers = new CopyOnWriteArrayList<HttpRequestHandler>();

    public HttpServer(final int port) {
        this.wishPort = port;
    }

    @Deprecated
    protected Runnable createConnectionHandler(final Socket clientSocket) throws IOException {
        return createPlainHttpConnection(clientSocket);
    }

    protected Runnable createConnectionHandler(ThreadPoolExecutor threadPool, final Socket clientSocket) throws IOException {
        return createConnectionHandler(clientSocket);
    }

    protected HttpConnection createPlainHttpConnection(Socket clientSocket) throws IOException {
        return new HttpConnection(this, clientSocket);
    }

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
        ThreadPoolExecutor threadPool = null;
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
            final ThreadPoolExecutor finalThreadPool = threadPool;
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
                                    final Runnable runnable;
                                    try {
                                        runnable = HttpServer.this.createConnectionHandler(finalThreadPool, clientSocket);
                                    } catch (Throwable e) {
                                        throw new IOException(e);
                                    }
                                    if (runnable != null) {
                                        finalThreadPool.execute(runnable);
                                        closeSocket = false;
                                    }
                                } catch (final IOException e) {
                                    e.printStackTrace();
                                } catch (final RejectedExecutionException e) {
                                    e.printStackTrace();
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
    public boolean canHandleChunkedEncoding(HttpRequest request, HttpResponse response) {
        return true;
    }
}
