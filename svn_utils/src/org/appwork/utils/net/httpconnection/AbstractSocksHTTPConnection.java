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
package org.appwork.utils.net.httpconnection;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.URL;

import org.appwork.utils.Time;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.socketconnection.SocketConnection;
import org.appwork.utils.net.socketconnection.SocksSocketConnection;
import org.appwork.utils.net.socketconnection.SocksSocketConnection.DESTTYPE;

/**
 * @author daniel
 *
 */
public abstract class AbstractSocksHTTPConnection extends HTTPConnectionImpl {

    protected SocketStreamInterface sockssocket  = null;
    protected StringBuffer          proxyRequest = null;
    protected final DESTTYPE        destType;

    public AbstractSocksHTTPConnection(final URL url, final HTTPProxy proxy) {
        this(url, proxy, DESTTYPE.AUTO);
    }

    public AbstractSocksHTTPConnection(final URL url, final HTTPProxy proxy, DESTTYPE destType) {
        super(url, proxy);
        this.destType = destType != null ? destType : DESTTYPE.AUTO;
        buildSocksSocketConnection();// does check proxy and destType
    }

    public DESTTYPE getDestType() {
        return destType;
    }

    @Override
    protected HTTPProxy isProxySupported(HTTPProxy proxy) {
        if (proxy != null && proxy.getType() != null) {
            switch (proxy.getType()) {
            case SOCKS4:
            case SOCKS4A:
            case SOCKS5:
                return proxy;
            default:
                break;
            }
        }
        throw new IllegalArgumentException("proxy type incompatible:" + proxy);
    }

    protected abstract SocksSocketConnection buildSocksSocketConnection();

    @Override
    public void connect() throws IOException {
        final HTTPConnectionProfilerInterface profiler = getProfiler();
        if (profiler != null) {
            profiler.onConnect(this);
        }
        /* establish to destination through socks */
        if (!isHostnameResolved()) {
            setHostname(resolveHostname(httpURL.getHost()));
        }
        SSLSocketStreamOptions sslSocketStreamOptions = null;
        SSLSocketStreamFactory factory = null;
        final int port = getConnectEndpointPort();
        connect: while (true) {
            if (this.isConnectionSocketValid()) {
                return;/* oder fehler */
            }
            this.resetConnection();
            this.proxyRequest = new StringBuffer();
            try {
                final long startTime = Time.systemIndependentCurrentJVMTimeMillis();
                this.sockssocket = this.createConnectionSocket(null);
                this.sockssocket = connect(sockssocket);
                if (this.httpURL.getProtocol().startsWith("https")) {
                    /* we need to lay ssl over normal socks5 connection */
                    try {
                        final String hostName = getHostname();
                        if (sslSocketStreamOptions == null) {
                            sslSocketStreamOptions = getSSLSocketStreamOptions(hostName, port, isSSLTrustALL());
                        }
                        factory = getSSLSocketStreamFactory(sslSocketStreamOptions);
                        this.connectionSocket = factory.create(sockssocket, hostName, port, true, sslSocketStreamOptions);
                    } catch (final IOException e) {
                        final String retrySSL;
                        try {
                            retrySSL = sslSocketStreamOptions != null ? (sslSocketStreamOptions = sslSocketStreamOptions.clone()).retry(factory, e) : null;
                            setSSLSocketStreamOptions(sslSocketStreamOptions);
                            this.connectExceptions.add(this.sockssocket + "|" + getExceptionMessage(e) + "|retrySSL:" + retrySSL);
                        } finally {
                            this.disconnect();
                        }
                        if (retrySSL != null) {
                            continue connect;
                        } else {
                            throw new ProxyConnectException(e, this.proxy);
                        }
                    }
                } else {
                    /* we can continue to use the socks connection */
                    this.connectionSocket = this.sockssocket;
                }
                this.httpResponseCode = -1;
                this.connectTime = Time.systemIndependentCurrentJVMTimeMillis() - startTime;
                if (profiler != null) {
                    profiler.onConnected(this);
                }
                /* now send Request */
                this.sendRequest();
                return;
            } catch (final IOException e) {
                final String retrySSL;
                try {
                    retrySSL = sslSocketStreamOptions != null ? (sslSocketStreamOptions = sslSocketStreamOptions.clone()).retry(factory, e) : null;
                    setSSLSocketStreamOptions(sslSocketStreamOptions);
                    this.connectExceptions.add("Socks(" + getProxy().getType() + ")" + SocketConnection.getRootEndPointSocketAddress(sockssocket) + "|EndPoint:" + this.getEndPointInetSocketAddress() + "|Exception:" + getExceptionMessage(e) + "|retrySSL:" + retrySSL);
                } finally {
                    this.disconnect();
                }
                if (retrySSL != null) {
                    continue connect;
                } else if (e instanceof HTTPProxyException) {
                    throw e;
                } else {
                    throw new ProxyConnectException(e, this.proxy);
                }
            }
        }
    }

    @Override
    public void setReadTimeout(int readTimeout) {
        try {
            this.readTimeout = Math.max(0, readTimeout);
            final SocketStreamInterface sockssocket = getSocksSocket();
            final Socket socket;
            if (sockssocket != null && (socket = sockssocket.getSocket()) != null) {
                socket.setSoTimeout(this.readTimeout);
            }
        } catch (final Throwable ignore) {
        }
    }

    @Override
    protected boolean isKeepAlivedEnabled() {
        return false;
    }

    @Override
    public void disconnect() {
        try {
            super.disconnect();
        } finally {
            final SocketStreamInterface sockssocket = getSocksSocket();
            try {
                if (sockssocket != null) {
                    sockssocket.close();
                }
            } catch (final Throwable e) {
                this.sockssocket = null;
            }
        }
    }

    protected SocketStreamInterface getSocksSocket() {
        return this.sockssocket;
    }

    protected int getConnectEndpointPort() {
        final int ret = this.httpURL.getPort();
        if (ret == -1) {
            return this.httpURL.getDefaultPort();
        } else {
            return ret;
        }
    }

    protected boolean resolveConnectEndPoint(SocksSocketConnection socksSocketConnection) {
        return (proxy != null && proxy.isResolveHostname()) || !socksSocketConnection.isSupported(DESTTYPE.DOMAIN);
    }

    abstract protected IPVERSION getEndPointIPVersion();

    protected volatile InetSocketAddress endPointInetSocketAddress = null;

    protected InetSocketAddress getEndPointInetSocketAddress() {
        return endPointInetSocketAddress;
    }

    protected volatile InetAddress customEndPointInetAddress = null;

    protected InetAddress getCustomEndPointInetAddress() {
        return customEndPointInetAddress;
    }

    protected void setCustomEndPointInetAddress(InetAddress customEndPointInetAddress) {
        this.customEndPointInetAddress = customEndPointInetAddress;
    }

    protected InetSocketAddress buildConnectEndPointSocketAddress(SocksSocketConnection socksSocketConnection) throws IOException {
        final InetAddress customEndPointInetAddress = getCustomEndPointInetAddress();
        if (customEndPointInetAddress != null) {
            return new InetSocketAddress(customEndPointInetAddress, getConnectEndpointPort());
        }
        try {
            final InetAddress[] ret = resolveLiteralIP(getHostname());
            if (ret != null && ret.length > 0) {
                return new InetSocketAddress(ret[0], getConnectEndpointPort());
            }
        } catch (IOException e) {
            this.connectExceptions.add(getExceptionMessage(e));
        }
        if (resolveConnectEndPoint(socksSocketConnection) && (socksSocketConnection.isSupported(DESTTYPE.IPV4) || socksSocketConnection.isSupported(DESTTYPE.IPV6))) {
            try {
                IPVERSION version = getEndPointIPVersion();
                if (!socksSocketConnection.isSupported(DESTTYPE.IPV6)) {
                    version = IPVERSION.IPV4_ONLY;
                } else if (!socksSocketConnection.isSupported(DESTTYPE.IPV4) || SocksSocketConnection.DESTTYPE.IPV6.equals(socksSocketConnection.getDestType(null))) {
                    version = IPVERSION.IPV6_IPV4;
                }
                final InetAddress[] inetAddress = HTTPConnectionUtils.resolvHostIP(getHostname(), version);
                if (inetAddress != null && inetAddress.length > 0) {
                    return new InetSocketAddress(inetAddress[0], getConnectEndpointPort());
                }
            } catch (IOException e) {
                this.connectExceptions.add(getExceptionMessage(e));
            }
        }
        return InetSocketAddress.createUnresolved(getHostname(), getConnectEndpointPort());
    }

    abstract protected SocketStreamInterface connect(SocketStreamInterface socketStream) throws IOException;

    @Override
    protected String getRequestInfo() {
        if (this.proxyRequest != null) {
            final StringBuilder sb = new StringBuilder();
            final String type = this.proxy.getType().name();
            final SocketAddress socketAddress = SocketConnection.getRootEndPointSocketAddress(getSocksSocket());
            if (socketAddress != null) {
                sb.append("-->" + type + ":");
                sb.append(socketAddress);
                sb.append("\r\n");
            }
            final InetAddress customEndPointInetAddress = getCustomEndPointInetAddress();
            if (customEndPointInetAddress != null) {
                sb.append("-->CustomEndPointInetAddress:").append(customEndPointInetAddress).append("\r\n");
            }
            sb.append("----------------CONNECTRequest(" + type + ")----------\r\n");
            sb.append(this.proxyRequest.toString());
            sb.append("------------------------------------------------\r\n");
            sb.append(super.getRequestInfo());
            return sb.toString();
        }
        return super.getRequestInfo();
    }
}
