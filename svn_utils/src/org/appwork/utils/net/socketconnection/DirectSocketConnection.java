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
package org.appwork.utils.net.socketconnection;

import java.io.IOException;
import java.net.ConnectException;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;

import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.ProxyAuthException;
import org.appwork.utils.net.httpconnection.ProxyConnectException;
import org.appwork.utils.net.httpconnection.ProxyEndpointConnectException;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;

/**
 * @author daniel
 *
 */
public class DirectSocketConnection extends SocketConnection {
    public DirectSocketConnection(HTTPProxy proxy) {
        super(proxy);
    }

    @Override
    protected HTTPProxy isProxySupported(HTTPProxy proxy) {
        if (proxy != null) {
            switch (proxy.getType()) {
            case DIRECT:
            case NONE:
                return proxy;
            default:
                throw new IllegalArgumentException("proxy must be of type direct/none:" + proxy);
            }
        } else {
            throw new IllegalArgumentException("proxy must be of type direct/none:" + proxy);
        }
    }

    public DirectSocketConnection() {
        this(HTTPProxy.NONE);
    }

    @Override
    public void connect(final SocketAddress endPoint, final int connectTimeout, final StringBuffer logger) throws IOException {
        try {
            try {
                if (connectTimeout == 0) {
                    /** no workaround for infinite connect timeouts **/
                    final SocketStreamInterface connectSocket = this.createConnectSocket(endPoint, connectTimeout);
                    connect(connectSocket, endPoint, connectTimeout);
                } else {
                    /**
                     * workaround for too early connect timeouts
                     */
                    int remainingConnectTimeout = connectTimeout;
                    while (true) {
                        final long beforeConnectMS = Time.systemIndependentCurrentJVMTimeMillis();
                        try {
                            final SocketStreamInterface connectSocket = this.createConnectSocket(endPoint, connectTimeout);
                            connect(connectSocket, endPoint, connectTimeout);
                            break;
                        } catch (final IOException e) {
                            closeConnectSocket();
                            if (Exceptions.containsInstanceOf(e, new Class[] { ConnectException.class, SocketTimeoutException.class }) && StringUtils.containsIgnoreCase(e.getMessage(), "timed out")) {
                                long timeout = Time.systemIndependentCurrentJVMTimeMillis() - beforeConnectMS;
                                if (timeout < 1000) {
                                    final int sleep = Math.max(100, (int) (2000 - timeout));
                                    System.out.println("Too Fast ConnectTimeout(Normal): " + timeout + "->Wait " + sleep);
                                    try {
                                        Thread.sleep(sleep);
                                        timeout = Time.systemIndependentCurrentJVMTimeMillis() - beforeConnectMS;
                                    } catch (final InterruptedException ie) {
                                        throw Exceptions.addSuppressed(e, ie);
                                    }
                                }
                                final int lastConnectTimeout = remainingConnectTimeout;
                                remainingConnectTimeout = (int) Math.max(0, remainingConnectTimeout - timeout);
                                if (remainingConnectTimeout == 0) {
                                    throw e;
                                } else if (Thread.currentThread().isInterrupted()) {
                                    throw e;
                                }
                                System.out.println("Workaround for ConnectTimeout(Normal): " + lastConnectTimeout + ">" + timeout);
                            } else {
                                throw e;
                            }
                        }
                    }
                }
            } catch (final IOException e) {
                throw new ProxyEndpointConnectException(e, this.getProxy(), endPoint);
            }
            final SocketStreamInterface connectedSocket = this.connectProxySocket(this.getConnectSocket(), endPoint, logger);
            if (connectedSocket != null) {
                this.proxySocket = connectedSocket;
                return;
            }
            throw new ProxyEndpointConnectException(this.getProxy(), endPoint);
        } catch (final ProxyAuthException e) {
            throw e;
        } catch (final ProxyConnectException e) {
            throw e;
        } catch (final IOException e) {
            throw new ProxyConnectException(e, this.getProxy());
        } finally {
            if (this.proxySocket == null) {
                this.closeConnectSocket();
            }
        }
    }

    protected InetAddress getBindInetAddress(InetAddress dest, HTTPProxy proxy) throws IOException {
        if (proxy != null && proxy.isDirect()) {
            final InetAddress[] ret = HTTPConnectionImpl.getNetworkInterfaceInetAdress(proxy);
            if (ret != null && ret.length > 0) {
                for (final InetAddress ia : ret) {
                    if (dest instanceof Inet4Address) {
                        if (ia instanceof Inet4Address) {
                            return ia;
                        }
                    } else if (dest instanceof Inet6Address) {
                        if (ia instanceof Inet6Address) {
                            return ia;
                        }
                    }
                }
            }
        }
        return null;
    }

    @Override
    protected SocketStreamInterface createConnectSocket(final SocketAddress endPoint, int connectTimeout) throws IOException {
        final SocketStreamInterface socket = super.createConnectSocket(endPoint, connectTimeout);
        if (this.getProxy().isDirect()) {
            try {
                if (socket.getSocket() != null && endPoint instanceof InetSocketAddress) {
                    final InetAddress bind = getBindInetAddress(((InetSocketAddress) endPoint).getAddress(), getProxy());
                    if (bind != null) {
                        socket.getSocket().bind(new InetSocketAddress(bind, 0));
                    }
                }
            } catch (IOException e) {
                socket.close();
                throw e;
            }
        }
        return socket;
    }

    @Override
    protected SocketStreamInterface connectProxySocket(SocketStreamInterface proxySocket, SocketAddress endpoint, StringBuffer logger) throws IOException {
        return proxySocket;
    }
}
