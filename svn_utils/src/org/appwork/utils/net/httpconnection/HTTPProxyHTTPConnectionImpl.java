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
import java.io.InputStream;
import java.net.ConnectException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;

public class HTTPProxyHTTPConnectionImpl extends HTTPConnectionImpl {
    private StringBuilder                    proxyRequest;
    private final boolean                    preferConnectMethod;
    protected InetSocketAddress              proxyInetSocketAddress = null;
    protected volatile SocketStreamInterface proxySocket            = null;

    protected SocketStreamInterface getProxySocket() {
        return this.proxySocket;
    }

    public HTTPProxyHTTPConnectionImpl(final URL url, final HTTPProxy p) {
        super(url, p);
        this.preferConnectMethod = p.isConnectMethodPrefered();
        this.setRequestProperty(HTTPConstants.HEADER_REQUEST_PROXY_CONNECTION, "close");
        if (!url.getProtocol().startsWith("https") && !preferConnectMethod) {
            this.httpPath = getRequestPath(url, true);
        }
    }

    @Override
    protected HTTPProxy isProxySupported(HTTPProxy proxy) {
        if (proxy != null) {
            switch (proxy.getType()) {
            case HTTP:
            case HTTPS:
                return proxy;
            default:
                throw new IllegalArgumentException("proxy must be of type http(s):" + proxy);
            }
        } else {
            throw new IllegalArgumentException("proxy must be of type http(s):" + proxy);
        }
    }

    protected boolean appendPortToHostHeader(HTTPProxy p) {
        // some proxy implementation might expect 'HOST: host:port' header
        return false;
    }

    protected String getConnectHostname() {
        final InetAddress customEndPointInetAddress = getCustomEndPointInetAddress();
        if (customEndPointInetAddress != null) {
            if (customEndPointInetAddress instanceof Inet6Address) {
                return "[" + customEndPointInetAddress.getHostAddress() + "]";
            } else {
                return customEndPointInetAddress.getHostAddress();
            }
        } else {
            return getHostname();
        }
    }

    protected InetAddress[] getRemoteIPs(HTTPProxy proxy, final boolean resolve) throws IOException {
        if (StringUtils.isEmpty(proxy.getHost())) {
            throw new ProxyConnectException(new UnknownHostException("Could not resolve: -empty host-"), proxy);
        } else {
            return getRemoteIPs(proxy.getHost(), resolve);
        }
    }

    protected volatile InetAddress customEndPointInetAddress = null;

    protected InetAddress getCustomEndPointInetAddress() {
        return customEndPointInetAddress;
    }

    protected void setCustomEndPointInetAddress(InetAddress customEndPointInetAddress) {
        this.customEndPointInetAddress = customEndPointInetAddress;
    }

    /*
     * SSL over HTTP Proxy, see http://muffin.doit.org/docs/rfc/tunneling_ssl.html
     */
    @Override
    public void connect() throws IOException {
        final HTTPConnectionProfilerInterface profiler = getProfiler();
        if (profiler != null) {
            profiler.onConnect(this);
        }
        SSLSocketStreamOptions sslSocketStreamEndPointOptions = null;
        SSLSocketStreamOptions sslSocketStreamProxyOptions = null;
        SSLSocketStreamFactory factory = null;
        connect: while (true) {
            if (this.isConnectionSocketValid()) {
                return;/* oder fehler */
            }
            this.resetConnection();
            if (!isHostnameResolved()) {
                setHostname(resolveHostname(httpURL.getHost()));
            }
            SSL_STATE state = SSL_STATE.NA;
            try {
                if (StringUtils.isNotEmpty(proxy.getUser()) || StringUtils.isNotEmpty(proxy.getPass())) {
                    /* add proxy auth in case username/pw are set */
                    final String user = StringUtils.valueOrEmpty(proxy.getUser());
                    final String pass = StringUtils.valueOrEmpty(proxy.getPass());
                    final String basicAuth = "Basic " + new String(Base64.encodeToByte((user + ":" + pass).getBytes(), false));
                    this.requestProperties.put(HTTPConstants.HEADER_REQUEST_PROXY_AUTHORIZATION, basicAuth);
                }
                IOException ee = null;
                List<InetAddress> proxyIPs = new ArrayList<InetAddress>(Arrays.asList(getRemoteIPs(getProxy(), true)));
                long startTime = Time.systemIndependentCurrentJVMTimeMillis();
                while (proxyIPs.size() > 0) {
                    final InetAddress host = proxyIPs.remove(0);
                    this.resetConnection();
                    startTime = Time.systemIndependentCurrentJVMTimeMillis();
                    this.connectionSocket = createConnectionSocket(null);
                    final InetSocketAddress connectedInetSocketAddress = new InetSocketAddress(host, this.proxy.getPort());
                    try {
                        /* create and connect to socks5 proxy */
                        proxyInetSocketAddress = connectedInetSocketAddress;
                        this.connectionSocket.getSocket().connect(proxyInetSocketAddress, getConnectTimeout());
                        setReadTimeout(getReadTimeout());
                        /* connection is okay */
                        ee = null;
                        break;
                    } catch (final IOException e) {
                        this.disconnect();
                        if (connectedInetSocketAddress.getAddress() instanceof Inet6Address && Exceptions.containsInstanceOf(e, new Class[] { SocketException.class, ConnectException.class, SocketTimeoutException.class })) {
                            if (StringUtils.containsIgnoreCase(e.getMessage(), "Network is unreachable")) {
                                proxyIPs = new ArrayList<InetAddress>(Arrays.asList(HTTPConnectionUtils.sortAndFilter(proxyIPs.toArray(new InetAddress[0]), IPVERSION.IPV4_ONLY)));
                            } else if (StringUtils.containsIgnoreCase(e.getMessage(), "timed out")) {
                                proxyIPs = new ArrayList<InetAddress>(Arrays.asList(HTTPConnectionUtils.sortAndFilter(proxyIPs.toArray(new InetAddress[0]), IPVERSION.IPV4_IPV6)));
                            }
                        }
                        this.connectExceptions.add(this.proxyInetSocketAddress + "|" + getExceptionMessage(e));
                        /* connection failed, try next available ip */
                        ee = e;
                    }
                }
                if (ee != null) {
                    throw new ProxyConnectException(ee, this.proxy);
                }
                proxySocket = connectionSocket;
                if (HTTPProxy.TYPE.HTTPS.equals(proxy.getType())) {
                    if (sslSocketStreamProxyOptions == null) {
                        sslSocketStreamProxyOptions = getSSLSocketStreamOptions(proxy.getHost(), proxy.getPort(), isSSLTrustALL());
                    }
                    factory = getSSLSocketStreamFactory(sslSocketStreamProxyOptions);
                    state = SSL_STATE.PROXY;
                    this.connectionSocket = factory.create(connectionSocket, proxy.getHost(), proxy.getPort(), true, sslSocketStreamProxyOptions);
                    proxySocket = connectionSocket;
                }
                this.connectTime = Time.systemIndependentCurrentJVMTimeMillis() - startTime;
                if (this.httpURL.getProtocol().startsWith("https") || this.isConnectMethodPrefered()) {
                    /* ssl via CONNECT method or because we prefer CONNECT */
                    /* build CONNECT request */
                    final int hostPort = this.httpURL.getPort() != -1 ? this.httpURL.getPort() : this.httpURL.getDefaultPort();
                    final boolean addHostPort = appendPortToHostHeader(getProxy());
                    this.proxyRequest = new StringBuilder();
                    this.proxyRequest.append("CONNECT ");
                    this.proxyRequest.append(getConnectHostname() + ":" + hostPort);
                    this.proxyRequest.append(" HTTP/1.1\r\n");
                    if (this.requestProperties.get(HTTPConstants.HEADER_REQUEST_USER_AGENT) != null) {
                        this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_USER_AGENT + ": " + this.requestProperties.get(HTTPConstants.HEADER_REQUEST_USER_AGENT) + "\r\n");
                    }
                    if (this.requestProperties.get(HTTPConstants.HEADER_REQUEST_HOST) != null) {
                        /* use existing host header */
                        final String host = this.requestProperties.get(HTTPConstants.HEADER_REQUEST_HOST);
                        if (!host.contains(":") && addHostPort) {
                            this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_HOST + ": " + host + ":" + hostPort + "\r\n");
                        } else {
                            this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_HOST + ": " + host + "\r\n");
                        }
                    } else {
                        /* add host from url as fallback */
                        if (addHostPort) {
                            this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_HOST + ": " + this.httpURL.getHost() + ":" + hostPort + "\r\n");
                        } else {
                            this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_HOST + ": " + this.httpURL.getHost() + "\r\n");
                        }
                    }
                    if (this.requestProperties.get(HTTPConstants.HEADER_REQUEST_PROXY_CONNECTION) != null) {
                        this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_PROXY_CONNECTION + ": " + this.requestProperties.remove(HTTPConstants.HEADER_REQUEST_PROXY_CONNECTION) + "\r\n");
                    }
                    if (this.requestProperties.get(HTTPConstants.HEADER_REQUEST_PROXY_AUTHORIZATION) != null) {
                        this.proxyRequest.append(HTTPConstants.HEADER_REQUEST_PROXY_AUTHORIZATION + ": " + this.requestProperties.remove(HTTPConstants.HEADER_REQUEST_PROXY_AUTHORIZATION) + "\r\n");
                    }
                    this.proxyRequest.append("\r\n");
                    /* send CONNECT to proxy */
                    this.connectionSocket.getOutputStream().write(this.proxyRequest.toString().getBytes("UTF-8"));
                    this.connectionSocket.getOutputStream().flush();
                    /* parse CONNECT response */
                    ByteBuffer header = HTTPConnectionUtils.readheader(this.connectionSocket.getInputStream(), true);
                    byte[] bytes = new byte[header.limit()];
                    header.get(bytes);
                    final String proxyResponseStatus = new String(bytes, "ISO-8859-1").trim();
                    this.proxyRequest.append(proxyResponseStatus + "\r\n");
                    String proxyCode = null;
                    if (proxyResponseStatus.startsWith("HTTP")) {
                        /* parse response code */
                        proxyCode = new Regex(proxyResponseStatus, "HTTP.*? (\\d+)").getMatch(0);
                    }
                    if (!"200".equals(proxyCode)) {
                        /* something went wrong */
                        try {
                            this.connectionSocket.close();
                        } catch (final Throwable nothing) {
                        }
                        if ("407".equals(proxyCode)) {
                            /* auth invalid/missing */
                            throw new ProxyAuthException(this.proxy);
                        } else {
                            throw new ProxyConnectException(this.proxy);
                        }
                    }
                    /* read rest of CONNECT headers */
                    /*
                     * Again, the response follows the HTTP/1.0 protocol, so the response line starts with the protocol version specifier,
                     * and the response line is followed by zero or more response headers, followed by an empty line. The line separator is
                     * CR LF pair, or a single LF.
                     */
                    while (true) {
                        /*
                         * read line by line until we reach the single empty line as separator
                         */
                        header = HTTPConnectionUtils.readheader(this.connectionSocket.getInputStream(), true);
                        if (header.limit() <= 2) {
                            /* empty line, <=2, as it may contains \r and/or \n */
                            break;
                        }
                        bytes = new byte[header.limit()];
                        header.get(bytes);
                        final String temp = fromBytes(bytes, -1, -1);
                        this.proxyRequest.append(temp + "\r\n");
                    }
                    if (this.httpURL.getProtocol().startsWith("https")) {
                        final String hostName = getHostname();
                        if (sslSocketStreamEndPointOptions == null) {
                            sslSocketStreamEndPointOptions = getSSLSocketStreamOptions(hostName, getPort(), isSSLTrustALL());
                        }
                        factory = getSSLSocketStreamFactory(sslSocketStreamEndPointOptions);
                        state = SSL_STATE.ENDPOINT;
                        this.connectionSocket = factory.create(connectionSocket, hostName, getPort(), true, sslSocketStreamEndPointOptions);
                    }
                    /*
                     * httpPath needs to be like normal http request, eg /index.html
                     */
                } else {
                    /* direct connect via proxy */
                    /*
                     * httpPath needs to include complete path here, eg http://google.de/
                     */
                    this.proxyRequest = new StringBuilder("DIRECT\r\n");
                }
                if (profiler != null) {
                    profiler.onConnected(this);
                }
                /* now send Request */
                this.sendRequest();
                return;
            } catch (final IOException e) {
                String retrySSL = null;
                try {
                    if (SSL_STATE.ENDPOINT.equals(state) && sslSocketStreamEndPointOptions != null) {
                        retrySSL = (sslSocketStreamEndPointOptions = sslSocketStreamEndPointOptions.clone()).retry(factory, e);
                        setSSLSocketStreamOptions(sslSocketStreamEndPointOptions);
                    }
                    if (SSL_STATE.PROXY.equals(state) && sslSocketStreamProxyOptions != null) {
                        retrySSL = (sslSocketStreamProxyOptions = sslSocketStreamProxyOptions.clone()).retry(factory, e);
                        setSSLSocketStreamOptions(sslSocketStreamProxyOptions);
                    }
                    this.connectExceptions.add(state + ":" + getExceptionMessage(e) + "|retrySSL:" + retrySSL);
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

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.net.httpconnection.HTTPConnectionImpl#disconnect()
     */
    @Override
    public void disconnect() {
        try {
            super.disconnect();
        } finally {
            proxySocket = null;
        }
    }

    @Override
    protected boolean isKeepAlivedEnabled() {
        return false;
    }

    @Override
    public InputStream getInputStream() throws IOException {
        this.connect();
        this.connectInputStream();
        if (this.getResponseCode() == 405) {
            // 405 - Method not allowed
            throw new ProxyConnectException(this.getResponseCode() + " " + this.getResponseMessage(), getProxy());
        } else if (this.getResponseCode() == 407) {
            /* auth invalid/missing */
            throw new ProxyAuthException(this.proxy);
        } else if (this.getResponseCode() == 502 && StringUtils.containsIgnoreCase(getResponseMessage(), "ISA Server denied the specified")) {
            throw new ProxyConnectException(this.getResponseCode() + " " + this.getResponseMessage(), getProxy());
        } else if (this.getResponseCode() == 504) {
            throw new ProxyConnectException(this.getResponseCode() + " " + this.getResponseMessage(), getProxy());
        } else {
            return super.getInputStream();
        }
    }

    @Override
    protected String getRequestInfo() {
        if (this.proxyRequest != null) {
            final StringBuilder sb = new StringBuilder();
            if (HTTPProxy.TYPE.HTTPS.equals(proxy.getType())) {
                sb.append("-->HTTPSProxy:").append(this.proxy.getHost()).append(":").append(this.proxy.getPort()).append("\r\n");
            } else {
                sb.append("-->HTTPProxy:").append(this.proxy.getHost()).append(":").append(this.proxy.getPort()).append("\r\n");
            }
            if (this.proxyInetSocketAddress != null && this.proxyInetSocketAddress.getAddress() != null) {
                if (HTTPProxy.TYPE.HTTPS.equals(proxy.getType())) {
                    sb.append("-->HTTPSProxyIP:").append(this.proxyInetSocketAddress.getAddress().getHostAddress()).append("\r\n");
                } else {
                    sb.append("-->HTTPProxyIP:").append(this.proxyInetSocketAddress.getAddress().getHostAddress()).append("\r\n");
                }
            }
            final InetAddress customEndPointInetAddress = getCustomEndPointInetAddress();
            if (customEndPointInetAddress != null) {
                sb.append("-->CustomEndPointInetAddress:").append(customEndPointInetAddress).append("\r\n");
            }
            if (HTTPProxy.TYPE.HTTPS.equals(proxy.getType())) {
                sb.append("----------------CONNECTRequest(HTTPS)------------\r\n");
                final SocketStreamInterface proxySocketStream = getProxySocket();
                if (proxySocketStream != null) {
                    final Socket proxySocket = proxySocketStream.getSocket();
                    if (proxySocket != null && proxySocket.isConnected()) {
                        if (proxySocketStream instanceof SSLSocketStreamInterface) {
                            final SSLSocketStreamInterface sslSocketStream = (SSLSocketStreamInterface) proxySocketStream;
                            final SSLSocketStreamOptions options = sslSocketStream.getOptions();
                            if (options != null && options.getRetryReasons().size() > 0) {
                                sb.append("SSLCipher: ").append(sslSocketStream.getCipherSuite() + "|" + options.getRetryReasons().toString()).append("\r\n");
                            } else {
                                sb.append("SSLCipher: ").append(sslSocketStream.getCipherSuite()).append("\r\n");
                            }
                        }
                    }
                }
            } else {
                sb.append("----------------CONNECTRequest(HTTP)------------\r\n");
            }
            sb.append(this.proxyRequest.toString());
            sb.append("------------------------------------------------\r\n");
            sb.append(super.getRequestInfo());
            return sb.toString();
        }
        return super.getRequestInfo();
    }

    public boolean isConnectMethodPrefered() {
        return this.preferConnectMethod;
    }
}
