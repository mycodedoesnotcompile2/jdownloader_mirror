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

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.SocketFactory;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.ProxyAuthException;
import org.appwork.utils.net.httpconnection.ProxyConnectException;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;

/**
 * @author daniel
 *
 */
public abstract class SocketConnection extends Socket {
    protected SocketAddress endPointSocketAddress;

    public static SocketAddress getRootEndPointSocketAddress(final SocketStreamInterface socketStream) {
        if (socketStream != null) {
            final Socket socket = socketStream.getSocket();
            SocketAddress ret = getRootEndPointSocketAddress(socket);
            if (ret == null && socket instanceof SocketConnection) {
                ret = ((SocketConnection) socket).getEndPointSocketAddress();
            }
            return ret;
        } else {
            return null;
        }
    }

    public static SocketAddress getRootEndPointSocketAddress(final Socket socket) {
        SocketAddress ret = null;
        Socket nextSocket = socket;
        while (nextSocket != null) {
            ret = nextSocket.getRemoteSocketAddress();
            if (nextSocket instanceof SocketConnection) {
                nextSocket = ((SocketConnection) nextSocket).getProxySocket();
            } else {
                break;
            }
        }
        return ret;
    }

    public SocketAddress getEndPointSocketAddress() {
        return endPointSocketAddress;
    }

    protected SocketAddress setEndPointSocketAddress(SocketAddress endPointSocketAddress) throws IOException {
        this.endPointSocketAddress = endPointSocketAddress;
        return endPointSocketAddress;
    }

    protected static int ensureRead(final InputStream is) throws IOException {
        final int read = is.read();
        if (read == -1) {
            throw new EOFException();
        }
        return read;
    }

    protected static byte[] ensureRead(final InputStream is, final int size, final byte[] buffer) throws IOException {
        if (size <= 0) {
            throw new IllegalArgumentException("size <=0");
        }
        final byte[] buf;
        if (buffer == null) {
            buf = new byte[size];
        } else {
            buf = buffer;
        }
        if (size > buf.length) {
            throw new IOException("buffer too small");
        }
        int done = 0;
        int read = 0;
        while (done < size && (read = is.read(buf, done, size - done)) != -1) {
            done += read;
        }
        if (done != size) {
            throw new EOFException();
        }
        return buf;
    }

    protected static final int byteToInt(byte b) {
        return b & 0xFF;
    }

    protected static final int[] byteArrayToIntArray(byte[] b) {
        final int ret[] = new int[b.length];
        for (int index = 0; index < b.length; index++) {
            ret[index] = byteToInt(b[index]);
        }
        return ret;
    }

    private SocketAddress                                bindPoint            = null;
    private Boolean                                      keepAlive            = null;
    private Boolean                                      oobInline            = null;
    private final HTTPProxy                              proxy;
    protected SocketStreamInterface                      proxySocket          = null;
    private Integer                                      receiveBufferSize    = null;
    private Boolean                                      reuseAddress         = null;
    private Integer                                      sendBufferSize       = null;
    private Integer                                      soLinger             = null;
    private Integer                                      soTimeout            = null;
    private Boolean                                      tcpNoDelay           = null;
    private Integer                                      trafficClass         = null;
    private final AtomicReference<SocketStreamInterface> pendingConnectSocket = new AtomicReference<SocketStreamInterface>(null);
    protected static final Charset                       ISO_8859_1           = Charset.forName("ISO-8859-1");
    protected IPVERSION                                  ipVersion            = null;

    public IPVERSION getIPVersion() {
        return ipVersion;
    }

    public void setIPVersion(IPVERSION ipVersion) {
        this.ipVersion = ipVersion;
    }

    public SocketConnection(HTTPProxy proxy) {
        this.proxy = isProxySupported(proxy);
    }

    protected abstract HTTPProxy isProxySupported(HTTPProxy proxy);

    @Override
    public void bind(SocketAddress bindpoint) throws IOException {
        if (this.proxySocket != null && proxySocket.getSocket() != null) {
            proxySocket.getSocket().bind(bindpoint);
        } else {
            this.bindPoint = bindpoint;
        }
    }

    public static String getHostName(SocketAddress endpoint) {
        if (endpoint != null && endpoint instanceof InetSocketAddress) {
            final InetSocketAddress endPointAddress = (InetSocketAddress) endpoint;
            if (Application.getJavaVersion() >= Application.JAVA17) {
                return endPointAddress.getHostString();
            } else {
                final InetAddress address = endPointAddress.getAddress();
                if (address != null) {
                    if (address.getHostName() != null) {
                        return address.getHostName();
                    } else {
                        return address.getHostAddress();
                    }
                } else {
                    return endPointAddress.getHostName();
                }
            }
        }
        return null;
    }

    @Override
    public synchronized void close() throws IOException {
        final SocketStreamInterface socketStream = getProxySocketStream();
        if (socketStream != null) {
            socketStream.close();
        } else {
            this.closeConnectSocket();
        }
    }

    @Override
    public void connect(SocketAddress endpoint) throws IOException {
        this.connect(endpoint, 0);
    }

    protected SocketStreamInterface createConnectSocket(final SocketAddress endPoint, int connectTimeout) throws IOException {
        this.closeConnectSocket();
        final Socket connectSocket = SocketFactory.get().create(this);
        final SocketStreamInterface ret = new SocketStreamInterface() {
            @Override
            public Socket getSocket() {
                return connectSocket;
            }

            @Override
            public OutputStream getOutputStream() throws IOException {
                return connectSocket.getOutputStream();
            }

            @Override
            public InputStream getInputStream() throws IOException {
                return connectSocket.getInputStream();
            }

            @Override
            public void close() throws IOException {
                connectSocket.close();
            }
        };
        try {
            this.pendingConnectSocket.set(ret);
            this.setSocketOptions(connectSocket);
        } catch (final IOException e) {
            connectSocket.close();
            throw e;
        }
        return ret;
    }

    protected boolean closeConnectSocket() throws IOException {
        final SocketStreamInterface socket = this.pendingConnectSocket.getAndSet(null);
        if (socket != null) {
            socket.close();
            return true;
        } else {
            return false;
        }
    }

    protected SocketStreamInterface getConnectSocket() throws IOException {
        final SocketStreamInterface socket = this.pendingConnectSocket.get();
        if (socket == null) {
            throw new SocketException("Socket is not connecting");
        } else {
            return socket;
        }
    }

    @Override
    public void connect(SocketAddress endpoint, final int connectTimeout) throws IOException {
        this.connect(endpoint, connectTimeout, null);
    }

    protected InetAddress[] resolvHostIP(HTTPProxy host) throws IOException {
        return HTTPConnectionUtils.resolvHostIP(host.getHost(), getIPVersion());
    }

    protected volatile InetAddress remoteIPs[] = null;

    protected InetAddress[] getRemoteIPs(final HTTPProxy proxy, final boolean resolve) throws IOException {
        if (this.remoteIPs == null && resolve) {
            this.remoteIPs = this.resolvHostIP(proxy);
        }
        if (resolve && (remoteIPs == null || remoteIPs.length == 0)) {
            throw new UnknownHostException("Could not resolve(" + getIPVersion() + "):" + proxy.getHost());
        } else {
            return remoteIPs;
        }
    }

    protected void setRemoteIPs(InetAddress[] remoteIPs) {
        this.remoteIPs = remoteIPs;
    }

    protected void connect(SocketStreamInterface socketStreamInterface, SocketAddress connectSocketAddress, int connectTimeout) throws IOException {
        if (socketStreamInterface.getSocket() != null) {
            setEndPointSocketAddress(connectSocketAddress);
            socketStreamInterface.getSocket().connect(connectSocketAddress, connectTimeout);
        } else {
            throw new IOException("SocketStreamInterface does not provide a connectable socket");
        }
    }

    public void connect(SocketAddress endpoint, final int connectTimeout, final StringBuffer logger) throws IOException {
        try {
            final int port = getProxy().getPort();
            final InetAddress[] socksIPs = getRemoteIPs(this.getProxy(), true);
            boolean retryFlag = true;
            while (retryFlag) {
                IOException ioE = null;
                retryFlag = false;
                for (final InetAddress connectAddress : socksIPs) {
                    final InetSocketAddress connectSocketAddress = new InetSocketAddress(connectAddress, port);
                    try {
                        if (connectTimeout == 0) {
                            /** no workaround for infinite connect timeouts **/
                            final SocketStreamInterface connectSocket = this.createConnectSocket(endpoint, connectTimeout);
                            connect(connectSocket, connectSocketAddress, connectTimeout);
                        } else {
                            /**
                             * workaround for too early connect timeouts
                             */
                            int remainingConnectTimeout = connectTimeout;
                            while (true) {
                                final long beforeConnectMS = Time.systemIndependentCurrentJVMTimeMillis();
                                try {
                                    final SocketStreamInterface connectSocket = this.createConnectSocket(endpoint, connectTimeout);
                                    connect(connectSocket, connectSocketAddress, connectTimeout);
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
                        ioE = null;
                        break;
                    } catch (final IOException e) {
                        if (logger != null) {
                            logger.append("->" + e.getMessage() + ":" + connectSocketAddress + "\r\n");
                        }
                        ioE = e;
                        this.closeConnectSocket();
                    }
                }
                if (ioE != null) {
                    throw ioE;
                }
                final SocketStreamInterface connectSocket = this.getConnectSocket();
                final SocketStreamInterface connectedSocket;
                try {
                    connectedSocket = this.connectProxySocket(connectSocket, endpoint, logger);
                } catch (IOException e) {
                    if (retryConnectProxySocket(e, connectSocket, endpoint)) {
                        closeConnectSocket();
                        if (logger != null) {
                            logger.append("->" + e.getMessage() + "\r\n");
                        }
                        retryFlag = true;
                        continue;
                    } else {
                        throw e;
                    }
                }
                if (connectedSocket != null) {
                    this.proxySocket = connectedSocket;
                    return;
                } else {
                    throw new ProxyConnectException(this.getProxy());
                }
            }
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

    protected boolean retryConnectProxySocket(IOException e, SocketStreamInterface connectSocket, SocketAddress endpoint) {
        return false;
    }

    protected abstract SocketStreamInterface connectProxySocket(SocketStreamInterface proxySocket, SocketAddress endpoint, final StringBuffer logger) throws IOException;

    public SocketStreamInterface getProxySocketStream() {
        return proxySocket;
    }

    protected Socket getProxySocket() {
        final SocketStreamInterface lProxySocket = getProxySocketStream();
        if (lProxySocket != null) {
            return lProxySocket.getSocket();
        } else {
            return null;
        }
    }

    @Override
    public SocketChannel getChannel() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getChannel();
        } else {
            return null;
        }
    }

    @Override
    public InetAddress getInetAddress() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getInetAddress();
        } else {
            return null;
        }
    }

    @Override
    public InputStream getInputStream() throws IOException {
        final SocketStreamInterface socketStream = getProxySocketStream();
        if (socketStream != null) {
            return socketStream.getInputStream();
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public boolean getKeepAlive() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getKeepAlive();
        } else if (this.keepAlive != null) {
            return this.keepAlive;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public InetAddress getLocalAddress() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getLocalAddress();
        } else {
            return new InetSocketAddress(0).getAddress();
        }
    }

    @Override
    public int getLocalPort() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getLocalPort();
        } else {
            return -1;
        }
    }

    @Override
    public SocketAddress getLocalSocketAddress() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getLocalSocketAddress();
        } else {
            return null;
        }
    }

    @Override
    public boolean getOOBInline() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getOOBInline();
        } else if (this.oobInline != null) {
            return this.oobInline;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public OutputStream getOutputStream() throws IOException {
        final SocketStreamInterface socketStream = getProxySocketStream();
        if (socketStream != null) {
            return socketStream.getOutputStream();
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public int getPort() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getPort();
        } else {
            return -1;
        }
    }

    public HTTPProxy getProxy() {
        return this.proxy;
    }

    @Override
    public synchronized int getReceiveBufferSize() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getReceiveBufferSize();
        } else if (this.receiveBufferSize != null) {
            return this.receiveBufferSize;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public SocketAddress getRemoteSocketAddress() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getRemoteSocketAddress();
        } else {
            return null;
        }
    }

    @Override
    public boolean getReuseAddress() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getReuseAddress();
        } else if (this.reuseAddress != null) {
            return this.reuseAddress;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public synchronized int getSendBufferSize() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getSendBufferSize();
        } else if (this.sendBufferSize != null) {
            return this.sendBufferSize;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public int getSoLinger() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getSoLinger();
        } else {
            return this.soLinger == null ? -1 : this.soLinger;
        }
    }

    @Override
    public synchronized int getSoTimeout() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getSoTimeout();
        } else if (this.soTimeout != null) {
            return this.soTimeout;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public boolean getTcpNoDelay() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getTcpNoDelay();
        } else if (this.tcpNoDelay != null) {
            return this.tcpNoDelay;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public int getTrafficClass() throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.getTrafficClass();
        } else if (this.trafficClass != null) {
            return this.trafficClass;
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public boolean isBound() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.isBound();
        } else {
            return this.bindPoint != null;
        }
    }

    @Override
    public boolean isClosed() {
        final Socket socket = getProxySocket();
        return socket != null && socket.isClosed();
    }

    @Override
    public boolean isConnected() {
        final Socket socket = getProxySocket();
        return socket != null && socket.isConnected();
    }

    @Override
    public boolean isInputShutdown() {
        final Socket socket = getProxySocket();
        return socket != null && socket.isInputShutdown();
    }

    @Override
    public boolean isOutputShutdown() {
        final Socket socket = getProxySocket();
        return socket != null && socket.isOutputShutdown();
    }

    @Override
    public void sendUrgentData(int data) throws IOException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.sendUrgentData(data);
        }
    }

    @Override
    public void setKeepAlive(boolean on) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setKeepAlive(on);
        } else {
            this.keepAlive = on;
        }
    }

    @Override
    public void setOOBInline(boolean on) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setOOBInline(on);
        } else {
            this.oobInline = on;
        }
    }

    @Override
    public void setPerformancePreferences(int connectionTime, int latency, int bandwidth) {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setPerformancePreferences(connectionTime, latency, bandwidth);
        }
    }

    @Override
    public synchronized void setReceiveBufferSize(int size) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setReceiveBufferSize(size);
        } else {
            this.receiveBufferSize = size;
        }
    }

    @Override
    public void setReuseAddress(boolean on) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setReuseAddress(on);
        } else {
            this.reuseAddress = on;
        }
    }

    @Override
    public synchronized void setSendBufferSize(int size) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setSendBufferSize(size);
        } else {
            this.sendBufferSize = size;
        }
    }

    @Override
    public String toString() {
        final Socket socket = getProxySocket();
        if (socket != null) {
            return socket.toString();
        } else {
            return super.toString();
        }
    }

    private void setSocketOptions(final Socket connectSocket) throws IOException {
        if (connectSocket != null) {
            if (this.bindPoint != null) {
                connectSocket.bind(this.bindPoint);
            }
            if (this.keepAlive != null) {
                connectSocket.setKeepAlive(this.keepAlive);
            }
            if (this.receiveBufferSize != null) {
                connectSocket.setReceiveBufferSize(this.receiveBufferSize);
            }
            if (this.reuseAddress != null) {
                connectSocket.setReuseAddress(this.reuseAddress);
            }
            if (this.sendBufferSize != null) {
                connectSocket.setSendBufferSize(this.sendBufferSize);
            }
            if (this.soLinger != null) {
                connectSocket.setSoLinger(true, this.soLinger);
            }
            if (this.tcpNoDelay != null) {
                connectSocket.setTcpNoDelay(this.tcpNoDelay);
            }
            if (this.trafficClass != null) {
                connectSocket.setTrafficClass(this.trafficClass);
            }
            if (this.oobInline != null) {
                connectSocket.setOOBInline(this.oobInline);
            }
            if (this.soTimeout != null) {
                connectSocket.setSoTimeout(this.soTimeout);
            }
        }
    }

    @Override
    public void setSoLinger(boolean on, int linger) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setSoLinger(on, linger);
        } else {
            if (on) {
                this.soLinger = linger;
            } else {
                this.soLinger = null;
            }
        }
    }

    @Override
    public synchronized void setSoTimeout(int timeout) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setSoTimeout(timeout);
        } else {
            this.soTimeout = timeout;
        }
    }

    @Override
    public void setTcpNoDelay(boolean on) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setTcpNoDelay(on);
        } else {
            this.tcpNoDelay = on;
        }
    }

    @Override
    public void setTrafficClass(int tc) throws SocketException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.setTrafficClass(tc);
        } else {
            this.trafficClass = tc;
        }
    }

    @Override
    public void shutdownInput() throws IOException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.shutdownInput();
        } else {
            throw new SocketException("Socket is not connected");
        }
    }

    @Override
    public void shutdownOutput() throws IOException {
        final Socket socket = getProxySocket();
        if (socket != null) {
            socket.shutdownOutput();
        } else {
            throw new SocketException("Socket is not connected");
        }
    }
}
