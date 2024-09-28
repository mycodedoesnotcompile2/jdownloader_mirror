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

import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.io.UnsupportedEncodingException;
import java.net.ConnectException;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousCloseException;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.WeakHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.net.ChunkedInputStream;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.EmptyInputStream;
import org.appwork.utils.net.LimitedInputStream;
import org.appwork.utils.net.PublicSuffixList;
import org.appwork.utils.net.SocketFactory;
import org.appwork.utils.net.StreamValidEOF;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpconnection.NetworkInterfaceException.ERROR;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.UrlQuery;

public class HTTPConnectionImpl implements HTTPConnection {
    protected class TryNextConnectException extends IOException {
        protected TryNextConnectException(Throwable cause) {
            super(cause);
        }
    }

    protected static enum SSL_STATE {
        NA,
        PROXY,
        ENDPOINT
    }

    public static enum KEEPALIVE {
        /**
         * KEEP-ALIVE is disabled
         */
        DISABLED,
        /**
         * KEEP-ALIVE is enabled for GET/HEAD/OPTIONS/DELETE
         */
        /* ENABLED_INTERNAL, */
        /**
         * KEEP-ALIVE is enabled, caller must handle HTTPKeepAliveSocketException
         */
        EXTERNAL_EXCEPTION
    }

    /**
     *
     */
    public static final String            UNKNOWN_HTTP_RESPONSE         = "unknown HTTP response";
    private static SSLSocketStreamFactory defaultSSLSocketStreamFactory = null;

    public static void setDefaultSSLSocketStreamFactory(SSLSocketStreamFactory defaultSSLSocketStreamFactory) {
        HTTPConnectionImpl.defaultSSLSocketStreamFactory = defaultSSLSocketStreamFactory;
    }

    public static SSLSocketStreamFactory getDefaultSSLSocketStreamFactory() {
        final SSLSocketStreamFactory ret = defaultSSLSocketStreamFactory;
        if (ret != null) {
            return ret;
        } else {
            return JavaSSLSocketStreamFactory.getInstance();
        }
    }

    protected HTTPHeaderMap<String>          requestProperties = null;
    protected volatile long[]                ranges;
    protected String                         customcharset     = null;
    protected volatile SocketStreamInterface connectionSocket  = null;

    protected SocketStreamInterface getConnectionSocket() {
        return this.connectionSocket;
    }

    protected final URL       httpURL;
    protected final HTTPProxy proxy;

    public HTTPProxy getProxy() {
        return this.proxy;
    }

    protected String                      httpPath;
    protected RequestMethod               httpMethod              = RequestMethod.GET;
    protected HTTPHeaderMap<List<String>> headers                 = null;
    protected int                         httpResponseCode        = -1;
    protected String                      httpResponseMessage     = "";
    protected volatile int                readTimeout             = 30000;
    protected volatile int                requestedConnectTimeout = 30000;
    protected IPVERSION                   ipVersion               = null;

    public IPVERSION getIPVersion() {
        return ipVersion;
    }

    public void setIPVersion(IPVERSION ipVersion) {
        this.ipVersion = ipVersion;
    }

    public int getReadTimeout() {
        return this.readTimeout;
    }

    public int getConnectTimeout() {
        return this.requestedConnectTimeout;
    }

    protected volatile long                      connectTime          = -1;
    protected volatile long                      requestTime          = -1;
    protected HTTPOutputStream                   outputStream         = null;
    protected InputStream                        inputStream          = null;
    protected InputStream                        convertedInputStream = null;
    protected volatile boolean                   inputStreamConnected = false;
    protected String                             httpHeader           = null;
    protected String                             invalidHttpHeader    = null;
    protected boolean                            contentDecoded       = true;
    protected long                               postTodoLength       = -1;
    private int[]                                allowedResponseCodes = new int[0];
    protected final CopyOnWriteArrayList<String> connectExceptions    = new CopyOnWriteArrayList<String>();
    protected volatile KEEPALIVE                 keepAlive            = KEEPALIVE.DISABLED;
    protected volatile InetAddress               remoteIPs[]          = null;
    protected boolean                            sslTrustALL          = true;
    protected InetAddress                        lastConnection       = null;
    protected int                                lastConnectionPort   = -1;
    protected String                             hostName;
    private boolean                              legacyConnectFlag    = true;
    private final static PublicSuffixList        PSL                  = PublicSuffixList.getInstance();

    public KEEPALIVE getKeepAlive() {
        return this.keepAlive;
    }

    public void setKeepAlive(KEEPALIVE keepAlive) {
        if (keepAlive == null) {
            this.keepAlive = KEEPALIVE.DISABLED;
        } else {
            this.keepAlive = keepAlive;
        }
    }

    /**
     * Keep-Alive stuff
     */
    protected static final HashMap<String, LinkedList<KeepAliveSocketStream>> KEEPALIVEPOOL         = new HashMap<String, LinkedList<KeepAliveSocketStream>>();
    protected static final Object                                             LOCK                  = new Object();
    protected static final DelayedRunnable                                    KEEPALIVECLEANUPTIMER = new DelayedRunnable(10000, 30000) {
                                                                                                        @Override
                                                                                                        public void delayedrun() {
                                                                                                            cleanupKeepAlivePools();
                                                                                                        }
                                                                                                    };

    private static final void cleanupKeepAlivePools() {
        synchronized (HTTPConnectionImpl.LOCK) {
            try {
                final Iterator<Entry<String, LinkedList<KeepAliveSocketStream>>> hostIterator = HTTPConnectionImpl.KEEPALIVEPOOL.entrySet().iterator();
                while (hostIterator.hasNext()) {
                    final Entry<String, LinkedList<KeepAliveSocketStream>> next = hostIterator.next();
                    final LinkedList<KeepAliveSocketStream> keepAliveSockets = next.getValue();
                    if (keepAliveSockets != null) {
                        final Iterator<KeepAliveSocketStream> keepAliveIterator = keepAliveSockets.iterator();
                        while (keepAliveIterator.hasNext()) {
                            final KeepAliveSocketStream socketStream = keepAliveIterator.next();
                            final Socket socket = socketStream.getSocket();
                            if (socket.isClosed() || socketStream.isTimedOut()) {
                                try {
                                    socket.close();
                                } catch (final Throwable ignore) {
                                }
                                keepAliveIterator.remove();
                                continue;
                            }
                            try {
                                if (socket.getChannel() != null) {
                                    final SocketChannel channel = socket.getChannel();
                                    channel.configureBlocking(false);
                                    final ByteBuffer check = ByteBuffer.wrap(new byte[1]);
                                    final int read = channel.read(check);
                                    if (read == -1) {
                                        throw new AsynchronousCloseException();
                                    } else if (read != 0) {
                                        throw new IOException("Unexpected data received");
                                    }
                                    channel.configureBlocking(true);
                                }
                            } catch (IOException e) {
                                try {
                                    socket.close();
                                } catch (final Throwable ignore) {
                                }
                                keepAliveIterator.remove();
                                continue;
                            }
                        }
                    }
                    if (keepAliveSockets == null || keepAliveSockets.size() == 0) {
                        hostIterator.remove();
                    }
                }
            } finally {
                if (HTTPConnectionImpl.KEEPALIVEPOOL.size() > 0) {
                    HTTPConnectionImpl.KEEPALIVECLEANUPTIMER.resetAndStart();
                }
            }
        }
    }

    public HTTPConnectionImpl(final URL url) {
        this(url, null);
    }

    public HTTPConnectionImpl(final URL url, final HTTPProxy p) {
        this.httpURL = url;
        this.proxy = isProxySupported(p);
        this.requestProperties = new HTTPHeaderMap<String>();
        this.headers = new HTTPHeaderMap<List<String>>();
        this.httpPath = getRequestPath(url, false);
    }

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
            return null;
        }
    }

    protected String getRequestPath(final URL url, final boolean includeAll) {
        final String httpPath = new org.appwork.utils.Regex(url.toString(), "https?://.*?(/.+)").getMatch(0);
        final String ret;
        if (httpPath == null) {
            ret = "/";
        } else {
            ret = httpPath;
        }
        if (includeAll) {
            final StringBuilder sb = new StringBuilder();
            sb.append(url.getProtocol());
            sb.append("://");
            sb.append(resolveHostname(url.getHost()));
            if (url.getPort() != -1) {
                sb.append(":").append(url.getPort());
            }
            sb.append(ret);
            return sb.toString();
        } else {
            return ret;
        }
    }

    protected long getDefaultKeepAliveMaxRequests(final String server) {
        if (StringUtils.containsIgnoreCase(server, "nginx")) {
            return 100;
        } else if (StringUtils.containsIgnoreCase(server, "apache")) {
            return 100;
        } else {
            return 100;
        }
    }

    protected long getMaxKeepAliveSockets() {
        if (CrossSystem.OperatingSystem.WINDOWS_XP.equals(CrossSystem.getOS())) {
            return 1;
        } else {
            return 5;
        }
    }

    protected long getDefaultKeepAliveTimeout(final String server) {
        if (StringUtils.containsIgnoreCase(server, "nginx")) {
            return 75 * 1000l;
        } else if (StringUtils.containsIgnoreCase(server, "apache")) {
            return 5 * 1000l;
        } else {
            return 60 * 1000l;
        }
    }

    protected boolean isKeepAliveOK() {
        final int code = this.getResponseCode();
        return this.isOK() || code == 404 || code == 403 || code == 416;
    }

    protected boolean putKeepAliveSocket(final SocketStreamInterface socketStream) throws IOException {
        /**
         * only keep-Alive sockets if
         *
         * 1.) keepAliveEnabled, HTTP Request/Response signals Keep-Alive and keep-Alive feature is enabled
         *
         * 2.) responseCode is ok
         *
         * 3.) socket is open/not closed/input and output open
         *
         * 4.) used inputstream has reached valid EOF
         *
         * 5.) available outputstream has written all data
         *
         *
         */
        if (socketStream != null) {
            final Socket socket = socketStream.getSocket();
            if (socket != null && this.isKeepAlivedEnabled() && this.isKeepAliveOK() && socket.isConnected() && !socket.isClosed() && socket.isInputShutdown() == false && socket.isOutputShutdown() == false) {
                final InputStream rawInputStream = getRawInputStream();
                if (rawInputStream != null && rawInputStream instanceof StreamValidEOF && ((StreamValidEOF) rawInputStream).isValidEOF()) {
                    if (!this.isRequiresOutputStream() || this.outputStream.transferedBytes() == this.postTodoLength) {
                        socket.setKeepAlive(true);
                        synchronized (HTTPConnectionImpl.LOCK) {
                            final KeepAliveSocketStream keepAliveSocketStream;
                            if (socketStream instanceof KeepAliveSocketStream) {
                                keepAliveSocketStream = (KeepAliveSocketStream) socketStream;
                            } else {
                                final String connectionResponse = this.getHeaderField(HTTPConstants.HTTP_KEEP_ALIVE);
                                final String server = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_SERVER);
                                final String maxKeepAliveTimeoutString = new Regex(connectionResponse, "timeout\\s*=\\s*(\\d+)").getMatch(0);
                                final String maxKeepAliveRequestsString = new Regex(connectionResponse, "max\\s*=\\s*(\\d+)").getMatch(0);
                                final long maxKeepAliveTimeout;
                                if (maxKeepAliveTimeoutString != null) {
                                    maxKeepAliveTimeout = Long.parseLong(maxKeepAliveTimeoutString) * 1000l;
                                } else {
                                    maxKeepAliveTimeout = this.getDefaultKeepAliveTimeout(server);
                                }
                                final long maxKeepAliveRequests;
                                if (maxKeepAliveRequestsString != null) {
                                    maxKeepAliveRequests = Long.parseLong(maxKeepAliveRequestsString);
                                } else {
                                    maxKeepAliveRequests = this.getDefaultKeepAliveMaxRequests(server);
                                }
                                final InetAddress localIP;
                                if (this.proxy != null && this.proxy.isDirect()) {
                                    localIP = socket.getLocalAddress();
                                } else {
                                    localIP = null;
                                }
                                final InetAddress[] remoteIPs;
                                if (this.remoteIPs != null) {
                                    remoteIPs = this.remoteIPs;
                                } else {
                                    remoteIPs = new InetAddress[] { ((InetSocketAddress) socket.getRemoteSocketAddress()).getAddress() };
                                }
                                if (StringUtils.equalsIgnoreCase("https", this.httpURL.getProtocol())) {
                                    keepAliveSocketStream = new KeepAliveSSLSocketStream(getProxy(), getHostname(), (SSLSocketStreamInterface) socketStream, maxKeepAliveTimeout, maxKeepAliveRequests, localIP, remoteIPs);
                                } else {
                                    keepAliveSocketStream = new KeepAliveSocketStream(getProxy(), getHostname(), socketStream, maxKeepAliveTimeout, maxKeepAliveRequests, localIP, remoteIPs);
                                }
                            }
                            keepAliveSocketStream.increaseRequests();
                            if (keepAliveSocketStream.getRequestsLeft() > 0) {
                                String domain = null;
                                if (HTTPConnectionImpl.PSL != null) {
                                    domain = HTTPConnectionImpl.PSL.getDomain(keepAliveSocketStream.getHost());
                                }
                                if (StringUtils.isEmpty(domain)) {
                                    domain = "FALLBACK";
                                }
                                LinkedList<KeepAliveSocketStream> keepAlivePool = HTTPConnectionImpl.KEEPALIVEPOOL.get(domain);
                                if (keepAlivePool == null) {
                                    keepAlivePool = new LinkedList<KeepAliveSocketStream>();
                                    HTTPConnectionImpl.KEEPALIVEPOOL.put(domain, keepAlivePool);
                                }
                                keepAlivePool.add(keepAliveSocketStream);
                                keepAliveSocketStream.keepAlive();
                                final long maxKeepAlive = this.getMaxKeepAliveSockets();
                                if (keepAlivePool.size() > maxKeepAlive) {
                                    final Iterator<KeepAliveSocketStream> it = keepAlivePool.iterator();
                                    while (it.hasNext() && keepAlivePool.size() > maxKeepAlive) {
                                        final KeepAliveSocketStream next = it.next();
                                        try {
                                            next.close();
                                        } catch (final Throwable ignore) {
                                        }
                                        it.remove();
                                    }
                                }
                                HTTPConnectionImpl.KEEPALIVECLEANUPTIMER.resetAndStart();
                                return true;
                            }
                        }
                    }
                }
            }
        }
        return false;
    }

    protected boolean checkSocketChannel(Socket socket) {
        if (socket != null) {
            try {
                if (socket.getChannel() != null) {
                    final SocketChannel channel = socket.getChannel();
                    channel.configureBlocking(false);
                    final ByteBuffer check = ByteBuffer.wrap(new byte[1]);
                    final int read = channel.read(check);
                    if (read == -1) {
                        throw new AsynchronousCloseException();
                    } else if (read != 0) {
                        throw new IOException("Unexpected data received");
                    }
                    channel.configureBlocking(true);
                }
                return true;
            } catch (IOException e) {
                try {
                    if (socket != null) {
                        socket.close();
                    }
                } catch (final Throwable ignore) {
                }
            }
        }
        return false;
    }

    protected KeepAliveSocketStream getKeepAliveSocket(final boolean dnsLookup) throws IOException {
        final InetAddress[] localIP = getNetworkInterfaceInetAdress(getProxy());
        final int port = getPort(httpURL);
        final String host = getHostname();
        final boolean ssl = StringUtils.equalsIgnoreCase("https", this.httpURL.getProtocol());
        String domain = null;
        if (HTTPConnectionImpl.PSL != null) {
            domain = HTTPConnectionImpl.PSL.getDomain(host);
        }
        if (StringUtils.isEmpty(domain)) {
            domain = "FALLBACK";
        }
        synchronized (HTTPConnectionImpl.LOCK) {
            final LinkedList<KeepAliveSocketStream> socketPool = HTTPConnectionImpl.KEEPALIVEPOOL.get(domain);
            if (socketPool != null) {
                final Iterator<KeepAliveSocketStream> socketPoolIterator = socketPool.descendingIterator();
                while (socketPoolIterator.hasNext()) {
                    final KeepAliveSocketStream socketStream = socketPoolIterator.next();
                    final Socket socket = socketStream.getSocket();
                    if (socket.isClosed() || socketStream.isTimedOut()) {
                        try {
                            socket.close();
                        } catch (final Throwable ignore) {
                        }
                        socketPoolIterator.remove();
                        continue;
                    } else if (socket.getPort() != port || !socketStream.sameBoundIP(localIP)) {
                        continue;
                    } else if (socketStream.isSsl() && ssl && socketStream.sameHost(host)) {
                        /**
                         * ssl needs to have same hostname to avoid (SNI)
                         *
                         * <p>
                         * Your browser sent a request that this server could not understand.<br />
                         * Host name provided via SNI and via HTTP are different
                         * </p>
                         */
                        socketPoolIterator.remove();
                        if (checkSocketChannel(socket)) {
                            return socketStream;
                        } else {
                            continue;
                        }
                    } else if (socketStream.isSsl() == false && ssl == false && (socketStream.sameHost(host) || (dnsLookup && socketStream.sameRemoteIPs(getRemoteIPs(host, true))))) {
                        // same hostname or same ip
                        socketPoolIterator.remove();
                        if (checkSocketChannel(socket)) {
                            return socketStream;
                        } else {
                            continue;
                        }
                    }
                }
                if (socketPool.isEmpty()) {
                    HTTPConnectionImpl.KEEPALIVEPOOL.remove(domain);
                }
            }
        }
        return null;
    }

    protected boolean addHostHeader() {
        /* check if host entry does exist */
        for (final String key : this.requestProperties.keySet()) {
            if ("Host".equalsIgnoreCase(key)) {
                return false;
            }
        }
        final URL url = getURL();
        final int defaultPort = url.getDefaultPort();
        final int usedPort = url.getPort();
        String appendPort = "";
        if (usedPort != -1 && defaultPort != -1 && usedPort != defaultPort) {
            appendPort = ":" + usedPort;
        }
        this.requestProperties.put("Host", getHostname() + appendPort);
        return true;
    }

    protected void resetConnection() {
        this.inputStreamConnected = false;
        this.httpResponseCode = -1;
        this.httpResponseMessage = "";
        this.postTodoLength = -1;
        this.outputStream = null;
        this.inputStream = null;
        this.convertedInputStream = null;
        this.connectTime = -1;
        this.requestTime = -1;
        this.headers.clear();
        this.ranges = null;
        this.lastConnection = null;
        this.lastConnectionPort = -1;
    }

    protected InetAddress getBindInetAddress(InetAddress dest, HTTPProxy proxy) throws IOException {
        if (proxy != null && proxy.isDirect()) {
            final InetAddress[] ret = getNetworkInterfaceInetAdress(proxy);
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

    public static InetAddress[] resolveLiteralIP(final String ip) throws IOException {
        if (ip != null) {
            final InetAddress[] ret = new InetAddress[1];
            if (ip.matches("^\\d+\\.\\d+\\.\\d+\\.\\d+$")) {
                ret[0] = InetAddress.getByName(ip);
                return ret;
            } else if (ip.matches("^\\[[a-f0-9:]+\\]$")) {
                ret[0] = InetAddress.getByName(ip);
                return ret;
            }
        }
        return null;
    }

    public static InetAddress[] getNetworkInterfaceInetAdress(HTTPProxy proxy) throws IOException {
        if (proxy != null && proxy.isDirect()) {
            final String local = proxy.getLocal();
            if (local != null) {
                final InetAddress[] ret = resolveLiteralIP(local);
                if (ret != null && ret.length > 0) {
                    return ret;
                }
            }
            final String interfaceName = local;
            if (interfaceName != null) {
                final String ifName;
                final boolean subInterface;
                final int index = interfaceName.indexOf(":");
                if (index != -1) {
                    ifName = interfaceName.substring(0, index);
                    subInterface = true;
                } else {
                    ifName = interfaceName;
                    subInterface = false;
                }
                final NetworkInterface netif = NetworkInterface.getByName(ifName);
                if (netif == null) {
                    throw new NetworkInterfaceException(ERROR.INTERFACE_NOT_FOUND, ifName, proxy);
                } else if (!netif.isUp()) {
                    throw new NetworkInterfaceException(ERROR.INTERFACE_NOT_CONNECTED, ifName, proxy);
                } else {
                    if (subInterface) {
                        final HashSet<InetAddress> ret = new HashSet<InetAddress>();
                        final Enumeration<NetworkInterface> subNetworkInterfaces = netif.getSubInterfaces();
                        while (subNetworkInterfaces.hasMoreElements()) {
                            final NetworkInterface subNetworkInterface = subNetworkInterfaces.nextElement();
                            if (subNetworkInterface != null && interfaceName.equals(subNetworkInterface.getName())) {
                                if (!subNetworkInterface.isUp()) {
                                    throw new NetworkInterfaceException(ERROR.INTERFACE_NOT_CONNECTED, interfaceName, proxy);
                                }
                                final Enumeration<InetAddress> inetAddresses = subNetworkInterface.getInetAddresses();
                                while (inetAddresses.hasMoreElements()) {
                                    final InetAddress inetAddress = inetAddresses.nextElement();
                                    if (inetAddress != null) {
                                        ret.add(inetAddress);
                                    }
                                }
                                if (ret.size() > 0) {
                                    return ret.toArray(new InetAddress[0]);
                                } else {
                                    throw new NetworkInterfaceException(ERROR.INTERFACE_NOT_SUPPORTED, interfaceName, proxy);
                                }
                            }
                        }
                        throw new NetworkInterfaceException(ERROR.INTERFACE_NOT_SUPPORTED, interfaceName, proxy);
                    } else {
                        /**
                         * root.getInetAddresses contains all InetAddress (rootInterface+subInterfaces), so we have to filter out
                         * subInterfaces
                         */
                        final HashSet<InetAddress> ret = new HashSet<InetAddress>();
                        Enumeration<InetAddress> inetAddresses = netif.getInetAddresses();
                        while (inetAddresses.hasMoreElements()) {
                            final InetAddress inetAddress = inetAddresses.nextElement();
                            if (inetAddress != null) {
                                ret.add(inetAddress);
                            }
                        }
                        if (ret.size() > 0) {
                            final Enumeration<NetworkInterface> subNetworkInterfaces = netif.getSubInterfaces();
                            while (subNetworkInterfaces.hasMoreElements()) {
                                final NetworkInterface subNetworkInterface = subNetworkInterfaces.nextElement();
                                if (subNetworkInterface != null) {
                                    inetAddresses = subNetworkInterface.getInetAddresses();
                                    while (inetAddresses.hasMoreElements()) {
                                        final InetAddress inetAddress = inetAddresses.nextElement();
                                        if (inetAddress != null) {
                                            ret.remove(inetAddress);
                                        }
                                    }
                                }
                            }
                        }
                        if (ret.size() > 0) {
                            return ret.toArray(new InetAddress[0]);
                        } else {
                            throw new NetworkInterfaceException(ERROR.INTERFACE_NOT_SUPPORTED, interfaceName, proxy);
                        }
                    }
                }
            }
            throw new ProxyConnectException("Invalid Direct Proxy", proxy);
        }
        return null;
    }

    protected Socket createRawConnectionSocket(final InetAddress bindInetAddress) throws IOException {
        final Socket socket = SocketFactory.get().create(this, bindInetAddress);
        if (bindInetAddress != null) {
            try {
                socket.bind(new InetSocketAddress(bindInetAddress, 0));
            } catch (final IOException e) {
                try {
                    socket.close();
                } catch (final Throwable ignore) {
                }
                connectExceptions.add("Bind: " + bindInetAddress + "|" + getExceptionMessage(e));
                throw new NetworkInterfaceException(ERROR.INTERFACE_BIND_ERROR, bindInetAddress.toString(), getProxy(), e);
            }
        }
        // socket.setSoTimeout(readTimeout);
        return socket;
    }

    protected SocketStreamInterface createConnectionSocket(final InetAddress bindInetAddress) throws IOException {
        final SocketStreamInterface connectionSocket = getConnectionSocket();
        closeConnectionSocket(connectionSocket);
        final Socket socket = createRawConnectionSocket(bindInetAddress);
        return new SocketStreamInterface() {
            @Override
            public Socket getSocket() {
                return socket;
            }

            @Override
            public OutputStream getOutputStream() throws IOException {
                return socket.getOutputStream();
            }

            @Override
            public InputStream getInputStream() throws IOException {
                return socket.getInputStream();
            }

            @Override
            public void close() throws IOException {
                socket.close();
            }
        };
    }

    private void closeConnectionSocket(final SocketStreamInterface connectionSocket) throws IOException {
        if (connectionSocket != null && this.connectionSocket == connectionSocket) {
            this.connectionSocket = null;
            connectionSocket.close();
        }
    }

    protected String resolveHostname(final String hostName) {
        final String resolvHost;
        if (!hostName.matches("^[a-zA-Z0-9\\-\\.]+$") && Application.getJavaVersion() >= Application.JAVA16) {
            resolvHost = java.net.IDN.toASCII(hostName.trim());
        } else {
            /* remove spaces....so literal IP's work without resolving */
            resolvHost = hostName.trim();
        }
        return resolvHost.toLowerCase(Locale.ENGLISH);
    }

    protected void setHostname(String hostName) {
        this.hostName = hostName;
    }

    protected boolean isHostnameResolved() {
        return this.hostName != null;
    }

    protected InetAddress[] resolvHostIP(final String host) throws IOException {
        return HTTPConnectionUtils.resolvHostIP(host, getIPVersion());
    }

    protected InetAddress[] getRemoteIPs(final String hostName, final boolean resolve) throws IOException {
        if (this.remoteIPs == null && resolve) {
            this.remoteIPs = this.resolvHostIP(hostName);
        }
        if (resolve && (remoteIPs == null || remoteIPs.length == 0)) {
            throw new UnknownHostException("Could not resolve(" + getIPVersion() + "):" + hostName);
        } else {
            return remoteIPs;
        }
    }

    protected void setRemoteIPs(InetAddress[] remoteIPs) {
        this.remoteIPs = remoteIPs;
    }

    protected static HashMap<String, SSLSocketStreamOptions>             SSL_SOCKETSTREAM_OPTIONS          = new HashMap<String, SSLSocketStreamOptions>();
    protected static WeakHashMap<SSLSocketStreamOptionsModifier, Object> SSL_SOCKETSTREAM_OPTIONS_MODIFIER = new WeakHashMap<SSLSocketStreamOptionsModifier, Object>();

    public static void addSSLSocketStreamOptionsModifier(SSLSocketStreamOptionsModifier sslSocketStreamModifier) {
        if (sslSocketStreamModifier != null) {
            synchronized (SSL_SOCKETSTREAM_OPTIONS_MODIFIER) {
                SSL_SOCKETSTREAM_OPTIONS_MODIFIER.put(sslSocketStreamModifier, Boolean.TRUE);
            }
        }
    }

    public static boolean removeSSLSocketStreamOptionsModifier(SSLSocketStreamOptionsModifier sslSocketStreamModifier) {
        if (sslSocketStreamModifier != null) {
            synchronized (SSL_SOCKETSTREAM_OPTIONS_MODIFIER) {
                return Boolean.TRUE.equals(SSL_SOCKETSTREAM_OPTIONS_MODIFIER.remove(sslSocketStreamModifier));
            }
        }
        return false;
    }

    protected SSLSocketStreamOptions getSSLSocketStreamOptions(String host, int port, boolean trustAllFlag) {
        final String id = getSSLSocketStreamOptionsID(host, port, trustAllFlag);
        synchronized (SSL_SOCKETSTREAM_OPTIONS) {
            SSLSocketStreamOptions options = SSL_SOCKETSTREAM_OPTIONS.get(id);
            if (options == null || !options.isValid()) {
                options = getNewSSLSocketStreamOptionsInstance(host, port, trustAllFlag);
                SSL_SOCKETSTREAM_OPTIONS.put(id, options);
            }
            return getSSLSocketStreamOptions(options);
        }
    }

    protected SSLSocketStreamOptions getSSLSocketStreamOptions(SSLSocketStreamOptions options) {
        return options;
    }

    protected void setSSLSocketStreamOptions(SSLSocketStreamOptions options) {
        if (options != null) {
            synchronized (SSL_SOCKETSTREAM_OPTIONS) {
                SSL_SOCKETSTREAM_OPTIONS.put(options.getId(), options);
            }
        }
    }

    protected SSLSocketStreamOptions processSSLSocketStreamOptionsModifier(SSLSocketStreamOptions sslSocketStreamOptions) {
        final List<SSLSocketStreamOptionsModifier> modifiers;
        synchronized (SSL_SOCKETSTREAM_OPTIONS_MODIFIER) {
            modifiers = new ArrayList<SSLSocketStreamOptionsModifier>(SSL_SOCKETSTREAM_OPTIONS_MODIFIER.keySet());
        }
        for (final SSLSocketStreamOptionsModifier modifier : modifiers) {
            final SSLSocketStreamOptions ret = modifier.modify(sslSocketStreamOptions, this);
            if (ret != null) {
                sslSocketStreamOptions = ret;
            }
        }
        return sslSocketStreamOptions;
    }

    protected String getSSLSocketStreamOptionsID(final String host, final int port, final boolean trustAllFlag) {
        return host + ":" + port + ":" + trustAllFlag;
    }

    protected SSLSocketStreamOptions getNewSSLSocketStreamOptionsInstance(String host, int port, boolean trustAllFlag) {
        final String id = getSSLSocketStreamOptionsID(host, port, trustAllFlag);
        final SSLSocketStreamOptions ret = new SSLSocketStreamOptions(id, trustAllFlag);
        return processSSLSocketStreamOptionsModifier(ret);
    }

    protected int getPort(URL url) {
        final int port = url.getPort();
        if (port == -1) {
            return url.getDefaultPort();
        } else {
            return port;
        }
    }

    protected HTTPConnectionProfilerInterface profiler;

    /**
     * @return the profiler
     */
    @Override
    public HTTPConnectionProfilerInterface getProfiler() {
        return profiler;
    }

    /**
     * @param profiler
     *            the profiler to set
     */
    @Override
    public void setProfiler(HTTPConnectionProfilerInterface profiler) {
        this.profiler = profiler;
    }

    public void connect() throws IOException {
        final HTTPConnectionProfilerInterface profiler = getProfiler();
        if (profiler != null) {
            profiler.onConnect(this);
        }
        SSLSocketStreamOptions sslSocketStreamOptions = null;
        SSLSocketStreamFactory factory = null;
        connect: while (true) {
            if (this.isConnectionSocketValid()) {
                return;/* oder fehler */
            }
            this.resetConnection();
            if (!isHostnameResolved()) {
                setHostname(resolveHostname(this.httpURL.getHost()));
            }
            if (!KEEPALIVE.DISABLED.equals(getKeepAlive())) {
                this.connectionSocket = this.getKeepAliveSocket(false);
                if (this.connectionSocket == null) {
                    this.connectionSocket = this.getKeepAliveSocket(true);
                }
            }
            if (this.connectionSocket == null) {
                /* try all different ip's until one is valid and connectable */
                IOException ee = null;
                List<InetAddress> remoteIPs = new ArrayList<InetAddress>(Arrays.asList(getRemoteIPs(getHostname(), true)));
                while (remoteIPs.size() > 0) {
                    final InetAddress host = remoteIPs.remove(0);
                    this.resetConnection();
                    final int port = getPort(httpURL);
                    long startMS = Time.systemIndependentCurrentJVMTimeMillis();
                    final HTTPProxy lProxy = getProxy();
                    InetAddress bindInetAddress = null;
                    if (lProxy != null) {
                        if (lProxy.isDirect()) {
                            bindInetAddress = getBindInetAddress(host, lProxy);
                        } else if (!lProxy.isNone()) {
                            throw new ProxyConnectException("Invalid Direct Proxy", lProxy);
                        }
                    }
                    final InetSocketAddress connectedInetSocketAddress = new InetSocketAddress(host, port);
                    try {
                        /* try to connect to given host now */
                        final int requestedConnectTimeout = getConnectTimeout();
                        if (requestedConnectTimeout == 0) {
                            startMS = Time.systemIndependentCurrentJVMTimeMillis();
                            this.connectionSocket = createConnectionSocket(bindInetAddress);
                            /** no workaround for infinite connect timeouts **/
                            this.connectionSocket.getSocket().connect(connectedInetSocketAddress, requestedConnectTimeout);
                            this.setReadTimeout(getReadTimeout());
                        } else {
                            /**
                             * workaround for too early connect timeouts
                             */
                            int remainingConnectTimeout = requestedConnectTimeout;
                            while (true) {
                                startMS = Time.systemIndependentCurrentJVMTimeMillis();
                                this.connectionSocket = createConnectionSocket(bindInetAddress);
                                final long beforeConnectMS = Time.systemIndependentCurrentJVMTimeMillis();
                                try {
                                    this.connectionSocket.getSocket().connect(connectedInetSocketAddress, remainingConnectTimeout);
                                    this.setReadTimeout(getReadTimeout());
                                    break;
                                } catch (final IOException e) {
                                    closeConnectionSocket(this.connectionSocket);
                                    if (connectedInetSocketAddress.getAddress() instanceof Inet6Address && Exceptions.containsInstanceOf(e, new Class[] { SocketException.class, ConnectException.class, SocketTimeoutException.class })) {
                                        if (StringUtils.containsIgnoreCase(e.getMessage(), "Network is unreachable")) {
                                            remoteIPs = new ArrayList<InetAddress>(Arrays.asList(HTTPConnectionUtils.sortAndFilter(remoteIPs.toArray(new InetAddress[0]), IPVERSION.IPV4_ONLY)));
                                            throw new TryNextConnectException(e);
                                        } else if (StringUtils.containsIgnoreCase(e.getMessage(), "timed out")) {
                                            remoteIPs = new ArrayList<InetAddress>(Arrays.asList(HTTPConnectionUtils.sortAndFilter(remoteIPs.toArray(new InetAddress[0]), IPVERSION.IPV4_IPV6)));
                                            throw new TryNextConnectException(e);
                                        }
                                    }
                                    if (Exceptions.containsInstanceOf(e, new Class[] { ConnectException.class, SocketTimeoutException.class }) && StringUtils.containsIgnoreCase(e.getMessage(), "timed out")) {
                                        long timeout = Time.systemIndependentCurrentJVMTimeMillis() - beforeConnectMS;
                                        if (timeout < 1000) {
                                            final int sleep = Math.max(100, (int) (2000 - timeout));
                                            this.connectExceptions.add("Too Fast ConnectTimeout(Normal): " + timeout + "->Wait " + sleep);
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
                                        this.connectExceptions.add("Workaround for ConnectTimeout(Normal): " + lastConnectTimeout + ">" + timeout);
                                    } else {
                                        throw e;
                                    }
                                }
                            }
                        }
                        if (profiler != null) {
                            profiler.onConnected(this);
                        }
                        if (this.httpURL.getProtocol().startsWith("https")) {
                            final String hostName = getHostname();
                            if (sslSocketStreamOptions == null) {
                                sslSocketStreamOptions = getSSLSocketStreamOptions(hostName, port, isSSLTrustALL());
                            }
                            factory = getSSLSocketStreamFactory(sslSocketStreamOptions);
                            this.connectionSocket = factory.create(connectionSocket, hostName, port, true, sslSocketStreamOptions);
                        }
                        this.connectTime = Time.systemIndependentCurrentJVMTimeMillis() - startMS;
                        ee = null;
                        break;
                    } catch (final TryNextConnectException e) {
                        final IOException cause;
                        if (e.getCause() instanceof IOException) {
                            cause = (IOException) e.getCause();
                        } else if (e.getCause() != null) {
                            cause = new IOException(e.getCause());
                        } else {
                            cause = new IOException(e);
                        }
                        try {
                            this.connectExceptions.add(connectedInetSocketAddress + "|" + getExceptionMessage(cause));
                        } finally {
                            this.disconnect();
                        }
                        if (bindInetAddress != null) {
                            ee = new ProxyConnectException(cause, lProxy);
                        } else {
                            ee = cause;
                        }
                    } catch (final IOException e) {
                        final String retrySSL;
                        try {
                            retrySSL = sslSocketStreamOptions != null ? (sslSocketStreamOptions = sslSocketStreamOptions.clone()).retry(factory, e) : null;
                            setSSLSocketStreamOptions(sslSocketStreamOptions);
                            this.connectExceptions.add(connectedInetSocketAddress + "|" + getExceptionMessage(e) + "|retrySSL:" + retrySSL);
                        } finally {
                            this.disconnect();
                        }
                        if (retrySSL != null) {
                            continue connect;
                        } else {
                            if (bindInetAddress != null) {
                                ee = new ProxyConnectException(e, lProxy);
                            } else {
                                ee = e;
                            }
                        }
                    }
                }
                if (ee != null) {
                    throw ee;
                }
            }
            /* now send Request */
            final Socket lastConnectionSocket = getConnectionSocket().getSocket();
            try {
                this.lastConnection = lastConnectionSocket.getInetAddress();
                this.lastConnectionPort = lastConnectionSocket.getPort();
                this.sendRequest();
                return;
            } catch (final IOException e) {
                final String retrySSL;
                try {
                    retrySSL = sslSocketStreamOptions != null ? (sslSocketStreamOptions = sslSocketStreamOptions.clone()).retry(factory, e) : null;
                    setSSLSocketStreamOptions(sslSocketStreamOptions);
                    this.connectExceptions.add(lastConnection + "|" + getExceptionMessage(e) + "|retrySSL:" + retrySSL);
                } finally {
                    this.disconnect();
                }
                if (retrySSL != null) {
                    continue connect;
                } else {
                    throw e;
                }
            }
        }
    }

    protected SSLSocketStreamFactory getSSLSocketStreamFactory(final SSLSocketStreamOptions sslSocketStreamOptions) {
        final SSLSocketStreamFactory ret = sslSocketStreamOptions != null ? sslSocketStreamOptions.getSSLSocketStreamFactory() : null;
        if (ret != null) {
            return ret;
        } else {
            return getDefaultSSLSocketStreamFactory();
        }
    }

    protected boolean isKeepAlivedEnabled() {
        final KEEPALIVE keepAlive = this.getKeepAlive();
        if (!KEEPALIVE.DISABLED.equals(keepAlive)) {
            final String connectionRequest = this.getRequestProperty(HTTPConstants.HEADER_REQUEST_CONNECTION);
            final String connectionResponse = this.getHeaderField(HTTPConstants.HEADER_REQUEST_CONNECTION);
            final boolean tryKeepAlive = (!this.isRequiresOutputStream() || KEEPALIVE.EXTERNAL_EXCEPTION.equals(keepAlive)) && (connectionResponse == null || StringUtils.containsIgnoreCase(connectionResponse, "Keep-Alive")) && (connectionRequest == null || !StringUtils.containsIgnoreCase(connectionRequest, "close"));
            return tryKeepAlive;
        } else {
            return false;
        }
    }

    protected String fromBytes(byte[] bytes, int start, int end) throws IOException {
        final StringBuilder sb = new StringBuilder();
        if (start < 0) {
            start = 0;
        }
        if (end < 0 || end >= bytes.length) {
            end = bytes.length;
        }
        for (int index = start; index < end; index++) {
            final int c = bytes[index] & 0xff;
            if (c <= 127) {
                sb.append((char) c);
            } else {
                // (buggy?) apache server expects upper case encoding
                final String hexEncoded = Integer.toString(c, 16).toUpperCase(Locale.ENGLISH);
                if (hexEncoded.length() == 1) {
                    sb.append("%0");
                } else {
                    sb.append("%");
                }
                sb.append(hexEncoded);
            }
        }
        return sb.toString();
    }

    protected synchronized void connectInputStream() throws IOException {
        final SocketStreamInterface connectionSocket = this.getConnectionSocket();
        try {
            final HTTPConnectionProfilerInterface profiler = getProfiler();
            if (this.isRequiresOutputStream() && this.postTodoLength >= 0) {
                final long done = this.outputStream.transferedBytes();
                if (done != this.postTodoLength) {
                    throw new IllegalStateException("Content-Length " + this.postTodoLength + " does not match send " + done + " bytes");
                }
            }
            if (this.inputStreamConnected) {
                return;
            }
            if (this.isRequiresOutputStream()) {
                /* flush outputstream in case some buffers are not flushed yet */
                this.outputStream.flush();
            }
            if (profiler != null) {
                profiler.onBeforeSocketGetInputStream(this);
            }
            final long startTime = Time.systemIndependentCurrentJVMTimeMillis();
            final InputStream inputStream = connectionSocket.getInputStream();
            if (profiler != null) {
                profiler.onAfterSocketGetInputStream(this);
            }
            this.inputStreamConnected = true;
            /* first read http header */
            ByteBuffer header = null;
            try {
                header = HTTPConnectionUtils.readheader(new CountingInputStream(inputStream) {
                    private boolean first = true;

                    @Override
                    protected void inc(int ret) throws IOException {
                        super.inc(ret);
                        if (first) {
                            first = false;
                            if (profiler != null) {
                                profiler.onFirstHeaderByteRead(HTTPConnectionImpl.this);
                            }
                        }
                    }
                }, true);
                if (header.limit() == 0) {
                    throw new EOFException("empty HTTP-Response");
                }
            } catch (final IOException e) {
                if (connectionSocket instanceof KeepAliveSocketStream) {
                    throw new KeepAliveSocketStreamException(e, connectionSocket);
                } else {
                    throw e;
                }
            }
            if (header.hasArray()) {
                this.httpHeader = new String(header.array(), 0, header.limit(), "ISO-8859-1").trim();
            } else {
                final byte[] bytes = new byte[header.limit()];
                header.get(bytes);
                this.httpHeader = new String(bytes, "ISO-8859-1").trim();
            }
            /* parse response code/message */
            if (this.httpHeader.matches("^[a-zA-Z0-9/\\.]+\\s*\\d+.*?")) {
                /**
                 * HTTP/1.0 or HTTP/1.1 or HTTP/1.0 compatible header
                 */
                final String code = new Regex(this.httpHeader, "[a-zA-Z0-9/\\.]+\\s*(\\d+)").getMatch(0);
                if (code != null) {
                    this.httpResponseCode = Integer.parseInt(code);
                }
                this.httpResponseMessage = new Regex(this.httpHeader, "[a-zA-Z0-9/\\.]+\\s*\\d+\\s*(.+)").getMatch(0);
                if (this.httpResponseMessage == null) {
                    this.httpResponseMessage = "";
                }
            } else {
                if (connectionSocket instanceof KeepAliveSocketStream) {
                    throw new KeepAliveSocketStreamException(new IOException("unknown HTTP response"), connectionSocket);
                }
                this.invalidHttpHeader = this.httpHeader;
                this.httpHeader = HTTPConnectionImpl.UNKNOWN_HTTP_RESPONSE;
                // Unknown HTTP Response: 999!
                this.httpResponseCode = 999;
                this.httpResponseMessage = HTTPConnectionImpl.UNKNOWN_HTTP_RESPONSE;
                if (header.limit() > 0) {
                    /*
                     * push back the data that got read because no http header exists
                     */
                    final PushbackInputStream pushBackInputStream;
                    if (header.hasArray()) {
                        pushBackInputStream = new PushbackInputStream(inputStream, header.limit());
                        pushBackInputStream.unread(header.array(), 0, header.limit());
                    } else {
                        final byte[] bytes = new byte[header.limit()];
                        header.get(bytes);
                        pushBackInputStream = new PushbackInputStream(inputStream, bytes.length);
                        pushBackInputStream.unread(bytes);
                    }
                    this.inputStream = pushBackInputStream;
                } else {
                    /* nothing to push back */
                    this.inputStream = inputStream;
                }
                return;
            }
            /* read rest of http headers */
            try {
                header = HTTPConnectionUtils.readheader(inputStream, false);
            } catch (final IOException e) {
                if (connectionSocket instanceof KeepAliveSocketStream) {
                    throw new KeepAliveSocketStreamException(e, connectionSocket);
                } else {
                    throw e;
                }
            }
            if (profiler != null) {
                profiler.onAllResponseHeadersRead(this);
            }
            final String temp;
            if (header.hasArray()) {
                temp = fromBytes(header.array(), 0, header.limit());
            } else {
                final byte[] bytes = new byte[header.limit()];
                header.get(bytes);
                temp = fromBytes(bytes, -1, -1);
            }
            this.requestTime = Time.systemIndependentCurrentJVMTimeMillis() - startTime;
            /*
             * split header into single strings, use RN or N(buggy f****ng non rfc)
             */
            String[] headerStrings = temp.split("(\r\n)|(\n)");
            for (final String line : headerStrings) {
                String key = null;
                String value = null;
                int index = 0;
                if ((index = line.indexOf(": ")) > 0) {
                    key = line.substring(0, index);
                    value = line.substring(index + 2);
                } else if ((index = line.indexOf(":")) > 0) {
                    /* buggy servers that don't have :space ARG */
                    key = line.substring(0, index);
                    value = line.substring(index + 1);
                } else {
                    key = null;
                    value = line;
                }
                if (key != null) {
                    key = key.trim();
                }
                if (value != null) {
                    value = value.trim();
                }
                List<String> list = this.headers.get(key);
                if (list == null) {
                    list = new ArrayList<String>();
                    this.headers.put(key, list);
                }
                list.add(value);
            }
            headerStrings = null;
            InputStream wrappedInputStream;
            if (this.isKeepAlivedEnabled()) {
                /* keep-alive-> do not close the inputstream! */
                wrappedInputStream = new FilterInputStream(inputStream) {
                    @Override
                    public void close() throws IOException {
                        /* do not close, keep-Alive */
                    }
                };
            } else {
                wrappedInputStream = inputStream;
            }
            if (RequestMethod.HEAD.equals(this.getRequestMethod())) {
                wrappedInputStream = new LimitedInputStream(wrappedInputStream, 0);
            } else {
                final boolean isChunked = StringUtils.containsIgnoreCase(this.getHeaderField(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING), HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED);
                if (isChunked) {
                    /* wrap chunkedInputStream */
                    // TODO: Handle auto-decoding marker. e.g. remove the header if the stream is decoded
                    wrappedInputStream = new ChunkedInputStream(wrappedInputStream);
                } else {
                    final long contentLength = this.getContentLength();
                    if (contentLength >= 0) {
                        /* wrap limitedInputStream */
                        wrappedInputStream = new LimitedInputStream(wrappedInputStream, contentLength);
                    } else if (HTTPConstants.ResponseCode.SUCCESS_NO_CONTENT.matches(getResponseCode())) {
                        /*
                         * https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/204
                         *
                         * Although this status code is intended to describe a response with no body, servers may erroneously include data
                         * following the headers.
                         */
                        // that's why we check for contentLength first
                        wrappedInputStream = new LimitedInputStream(wrappedInputStream, 0);
                    }
                }
            }
            this.inputStream = wrappedInputStream;
        } catch (final IOException e) {
            this.disconnect();
            throw e;
        }
    }

    public void disconnect() {
        final HTTPConnectionProfilerInterface profiler = getProfiler();
        if (profiler != null) {
            profiler.onDisconnect(this);
        }
        SocketStreamInterface connectionSocket = null;
        try {
            connectionSocket = this.getConnectionSocket();
            if (connectionSocket != null && !this.putKeepAliveSocket(connectionSocket)) {
                connectionSocket.close();
            }
        } catch (final Throwable e) {
            e.printStackTrace();
            try {
                if (connectionSocket != null) {
                    connectionSocket.close();
                }
            } catch (final Throwable ignore) {
            }
        } finally {
            this.connectionSocket = null;
        }
        if (connectionSocket != null && profiler != null) {
            profiler.onDisconnected(this);
        }
    }

    @Override
    public void finalizeConnect() throws IOException {
        final HTTPConnectionProfilerInterface profiler = getProfiler();
        if (profiler != null) {
            profiler.onFinalizeConnection(this);
        }
        this.connect();
        this.connectInputStream();
    }

    protected String getExceptionMessage(Exception e) {
        if (e == null) {
            return null;
        } else if (e.getMessage() != null) {
            return e.getClass().getName() + "(" + e.getMessage() + ")";
        } else {
            return e.getClass().getName();
        }
    }

    @Override
    public int[] getAllowedResponseCodes() {
        return this.allowedResponseCodes;
    }

    public String getCharset() {
        if (this.customcharset != null) {
            return this.customcharset;
        }
        String charSet = this.getContentType();
        if (charSet != null) {
            final int charSetIndex = this.getContentType().toLowerCase().indexOf("charset=");
            if (charSetIndex > 0) {
                charSet = this.getContentType().substring(charSetIndex + 8).trim();
                if (charSet.length() > 2) {
                    if (charSet.startsWith("\"")) {
                        charSet = charSet.substring(1);
                        final int indexLast = charSet.lastIndexOf("\"");
                        if (indexLast > 0) {
                            charSet = charSet.substring(0, indexLast);
                        }
                    }
                    return charSet;
                }
            }
        }
        return null;
    }

    @Override
    public long getCompleteContentLength() {
        final long[] ranges = this.getRange();
        if (ranges != null) {
            return ranges[2];
        } else {
            return this.getContentLength();
        }
    }

    public long getContentLength() {
        final String length = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH);
        if (length != null && length.trim().matches("^\\d+$")) {
            return Long.parseLong(length.trim());
        } else {
            return -1;
        }
    }

    public String getContentType() {
        final String type = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE);
        if (type == null) {
            return "unknown";
        } else {
            return type;
        }
    }

    public String getHeaderField(final String string) {
        final List<String> ret = this.headers.get(string);
        if (ret == null || ret.size() == 0) {
            return null;
        } else {
            return ret.get(0);
        }
    }

    public Map<String, List<String>> getHeaderFields() {
        return this.headers;
    }

    public List<String> getHeaderFields(final String string) {
        final List<String> ret = this.headers.get(string);
        if (ret == null || ret.size() == 0) {
            return null;
        } else {
            return ret;
        }
    }

    protected InputStream getRawInputStream() {
        return inputStream;
    }

    public InputStream getInputStream() throws IOException {
        if (!isLegacyConnectEnabled() && !isConnectionSocketValid()) {
            throw new IllegalStateException("not connected!");
        }
        this.connect();
        this.connectInputStream();
        final int code = this.getResponseCode();
        if (this.isOK() || code == 404 || code == 403 || code == 416 || code == 401) {
            if (this.convertedInputStream == null) {
                InputStream rawInputStream = getRawInputStream();
                if (this.contentDecoded && !RequestMethod.HEAD.equals(this.getRequestMethod())) {
                    if (getContentLength() == 0) {
                        // Content-Length is 0, return EmptyInputStream
                        this.convertedInputStream = new EmptyInputStream();
                        this.contentDecoded = false;
                    } else {
                        final String encodingTransfer = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_TRANSFER_ENCODING);
                        if ("base64".equalsIgnoreCase(encodingTransfer)) {
                            /* base64 encoded content */
                            // TRansfer encoding is not allowed to use Content.Length, thus does not need the counter. We implement it
                            // anyway to have access to it if we need it some day
                            // https://tools.ietf.org/html/rfc7230#section-3.3.1
                            rawInputStream = new CountingBase64InputStream(rawInputStream);
                        } else if ("binary".equalsIgnoreCase(encodingTransfer)) {
                            /* binary encoded content */
                            rawInputStream = new CountingInputStream(rawInputStream);
                        }
                        /* we convert different content-encodings to normal inputstream */
                        final String encoding = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING);
                        // TODO: multiple encodings in one header are allowed. The code below does not support this
                        if (encoding == null || encoding.length() == 0 || "none".equalsIgnoreCase(encoding) || "identity".equalsIgnoreCase(encoding)) {
                            /* no encoding */
                            // Any data acceptable. CRLF may or may not indicate a line break.
                            this.convertedInputStream = new CountingInputStream(rawInputStream);
                            this.contentDecoded = false;
                        } else if ("gzip".equalsIgnoreCase(encoding) || "x-gzip".equalsIgnoreCase(encoding)) {
                            /* gzip encoding */
                            // TODO: Handle auto-decoding marker. e.g. remove the header if the stream is decoded
                            this.convertedInputStream = new CountingGZIPInputStream(rawInputStream);
                            this.contentDecoded = true;
                        } else if ("deflate".equalsIgnoreCase(encoding) || "x-deflate".equalsIgnoreCase(encoding)) {
                            /* deflate encoding */
                            // TODO: Handle auto-decoding marker. e.g. remove the header if the stream is decoded
                            this.convertedInputStream = new CountingInflaterInputStream(new CountingInputStream(rawInputStream));
                            this.contentDecoded = true;
                        } else {
                            /* unsupported */
                            this.convertedInputStream = new CountingInputStream(rawInputStream);
                            this.contentDecoded = false;
                        }
                    }
                } else {
                    /*
                     * use original inputstream OR LimitedInputStream from HeadRequest
                     */
                    this.convertedInputStream = new CountingInputStream(rawInputStream);
                    this.contentDecoded = false;
                }
            }
            return this.convertedInputStream;
        } else {
            throw new IOException(this.getResponseCode() + " " + this.getResponseMessage());
        }
    }

    public HTTPOutputStream getOutputStream() throws IOException {
        if (this.outputStream != null && this.isRequiresOutputStream()) {
            return outputStream;
        } else {
            throw new IOException("OutputStream is not available");
        }
    }

    public long[] getRange() {
        if (this.ranges == null) {
            this.ranges = HTTPConnectionUtils.parseContentRange(this);
        }
        return this.ranges;
    }

    protected String getHostname() {
        if (isHostnameResolved()) {
            return this.hostName;
        } else {
            return resolveHostname(httpURL.getHost());
        }
    }

    protected int getPort() {
        return getPort(getURL());
    }

    protected String getRequestInfo() {
        final StringBuilder sb = new StringBuilder();
        sb.append("----------------Request Information-------------\r\n");
        sb.append("URL: ").append(this.getURL()).append("\r\n");
        try {
            final UrlQuery query = UrlQuery.parse(getURL().getQuery());
            if (query.list().size() > 0) {
                sb.append("URL-Parameter" + (query.list().size() > 1 ? "s" : "") + ":").append("\r\n");
                int max = 0;
                for (KeyValueStringEntry p : query.list()) {
                    if (p.getKey() != null) {
                        max = Math.max(p.getKey().length(), max);
                    }
                }
                for (KeyValueStringEntry p : query.list()) {
                    sb.append("   " + org.appwork.utils.StringUtils.fillPost(p.getKey() == null ? "" : p.getKey(), " ", max)).append(" : ").append(URLEncode.decodeURIComponent(p.getValue())).append("\r\n");
                }
            }
        } catch (Throwable e) {
            LogV3.logger(HTTPConnectionImpl.class).exception("Error during toString", e);
        }
        final SocketStreamInterface connectionSocketStream = getConnectionSocket();
        final Socket connectionSocket;
        if (connectionSocketStream != null) {
            connectionSocket = connectionSocketStream.getSocket();
        } else {
            connectionSocket = null;
        }
        final InetAddress lLastConnection = this.lastConnection;
        if (connectionSocket != null && connectionSocket.isConnected()) {
            if (connectionSocketStream instanceof SSLSocketStreamInterface) {
                final SSLSocketStreamInterface sslSocketStream = (SSLSocketStreamInterface) connectionSocketStream;
                final SSLSocketStreamOptions options = sslSocketStream.getOptions();
                if (options != null && options.getRetryReasons().size() > 0) {
                    sb.append("SSLCipher: ").append(sslSocketStream.getCipherSuite() + "|" + options.getRetryReasons().toString()).append("\r\n");
                } else {
                    sb.append("SSLCipher: ").append(sslSocketStream.getCipherSuite()).append("\r\n");
                }
            }
            sb.append("ConnectIP: ").append(connectionSocket.getInetAddress()).append(":").append(connectionSocket.getPort()).append("\r\n");
        } else if (lLastConnection != null) {
            sb.append("ConnectIP: ").append(lLastConnection).append(":").append(this.lastConnectionPort).append("\r\n");
        } else {
            sb.append("Host: ").append(getHostname()).append("\r\n");
        }
        if (this.proxy != null && this.proxy.isDirect()) {
            if (connectionSocket != null) {
                sb.append("Local: ").append(this.proxy.getLocal()).append(connectionSocket.getLocalAddress().toString()).append("\r\n");
            } else {
                sb.append("Local: ").append(this.proxy.getLocal()).append("\r\n");
            }
        }
        sb.append("Connection-Timeout: ").append(getConnectTimeout() + "ms").append("\r\n");
        sb.append("Read-Timeout: ").append(getReadTimeout() + "ms").append("\r\n");
        if (this.connectExceptions.size() > 0) {
            sb.append("----------------ConnectionExceptions-------------------------\r\n");
            int index = 0;
            for (String connectException : this.connectExceptions) {
                sb.append(index++).append(":").append(connectException).append("\r\n");
            }
        }
        sb.append("----------------Request-------------------------\r\n");
        if (getRawInputStream() != null) {
            sb.append(this.httpMethod.toString()).append(' ').append(this.httpPath).append(" HTTP/1.1\r\n");
            final Iterator<Entry<String, String>> it = this.getRequestProperties().entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, String> next = it.next();
                if (next.getValue() == null) {
                    continue;
                }
                sb.append(next.getKey());
                sb.append(": ");
                sb.append(next.getValue());
                sb.append("\r\n");
            }
        } else {
            sb.append("-------------Not Connected Yet!-----------------\r\n");
        }
        return sb.toString();
    }

    public RequestMethod getRequestMethod() {
        return this.httpMethod;
    }

    public Map<String, String> getRequestProperties() {
        return this.requestProperties;
    }

    public String getRequestProperty(final String string) {
        return this.requestProperties.get(string);
    }

    public long getRequestTime() {
        return this.requestTime;
    }

    public long getConnectTime() {
        return this.connectTime;
    }

    public int getResponseCode() {
        return this.httpResponseCode;
    }

    protected String getResponseInfo() {
        final StringBuilder sb = new StringBuilder();
        sb.append("----------------Response Information------------\r\n");
        try {
            if (getRawInputStream() != null) {
                final long lconnectTime = getConnectTime();
                if (lconnectTime >= 0) {
                    sb.append("Connection-Time: ").append(lconnectTime + "ms").append("\r\n");
                } else {
                    sb.append("Connection-Time: keep-Alive\r\n");
                }
                final long lrequestTime = getRequestTime();
                sb.append("Request-Time: ").append(Math.max(0, lrequestTime) + "ms").append("\r\n");
                sb.append("----------------Response------------------------\r\n");
                this.connectInputStream();
                sb.append(this.httpHeader).append("\r\n");
                if (this.invalidHttpHeader != null) {
                    sb.append("InvalidHTTPHeader: ").append(this.invalidHttpHeader).append("\r\n");
                }
                for (final Entry<String, List<String>> next : this.getHeaderFields().entrySet()) {
                    for (int i = 0; i < next.getValue().size(); i++) {
                        if (next.getKey() == null) {
                            sb.append(next.getValue().get(i));
                            sb.append("\r\n");
                        } else {
                            sb.append(next.getKey());
                            sb.append(": ");
                            sb.append(next.getValue().get(i));
                            sb.append("\r\n");
                        }
                    }
                }
                sb.append("------------------------------------------------\r\n");
            } else {
                sb.append("-------------Not Connected Yet!------------------\r\n");
            }
        } catch (final IOException nothing) {
            sb.append("----------No InputStream Available!--------------\r\n");
        }
        sb.append("\r\n");
        return sb.toString();
    }

    public String getResponseMessage() {
        return this.httpResponseMessage;
    }

    public URL getURL() {
        return this.httpURL;
    }

    public boolean isConnected() {
        final SocketStreamInterface connectionSocket = getConnectionSocket();
        final Socket socket;
        if (connectionSocket != null && (socket = connectionSocket.getSocket()) != null && socket.isConnected()) {
            return true;
        } else {
            return false;
        }
    }

    protected boolean isConnectionSocketValid() {
        final SocketStreamInterface connectionSocket = this.getConnectionSocket();
        final Socket socket;
        if (connectionSocket != null && (socket = connectionSocket.getSocket()) != null && socket.isConnected() && !socket.isClosed()) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean isContentDecoded() {
        if (convertedInputStream == null && contentDecoded && isConnected()) {
            try {
                getInputStream();
            } catch (IOException e) {
                LogV3.log(e);
            }
        }
        return this.contentDecoded;
    }

    public boolean isContentDisposition() {
        return this.getHeaderField("Content-Disposition") != null;
    }

    public boolean isOK() {
        final int code = this.getResponseCode();
        if (code >= 200 && code < 400) {
            return true;
        } else if (this.isResponseCodeAllowed(code)) {
            return true;
        } else {
            return false;
        }
    }

    protected boolean isResponseCodeAllowed(final int code) {
        for (final int c : this.allowedResponseCodes) {
            if (c == code || c == -1) {
                return true;
            }
        }
        return false;
    }

    protected void putHostToTop(final Map<String, String> oldRequestProperties) {
        final HTTPHeaderMap<String> newRet = new HTTPHeaderMap<String>();
        final String host = oldRequestProperties.remove("Host");
        if (host != null) {
            newRet.put("Host", host);
        }
        newRet.putAll(oldRequestProperties);
        oldRequestProperties.clear();
        oldRequestProperties.putAll(newRet);
    }

    protected boolean isRequiresOutputStream() {
        return httpMethod.requiresOutputStream;
    }

    protected void sendRequest() throws UnsupportedEncodingException, IOException {
        /* now send Request */
        final HTTPConnectionProfilerInterface profiler = getProfiler();
        if (profiler != null) {
            profiler.onSendRequest(this);
        }
        final SocketStreamInterface connectionSocket = getConnectionSocket();
        final StringBuilder sb = new StringBuilder();
        sb.append(this.httpMethod.name()).append(' ').append(this.httpPath).append(" HTTP/1.1\r\n");
        this.addHostHeader();
        this.putHostToTop(this.requestProperties);
        final Iterator<Entry<String, String>> it = this.requestProperties.entrySet().iterator();
        while (it.hasNext()) {
            final Entry<String, String> next = it.next();
            final String key = next.getKey();
            final String value = next.getValue();
            if (value == null) {
                continue;
            } else if (HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH.equalsIgnoreCase(key)) {
                /* content length to check if we send out all data */
                this.postTodoLength = Long.parseLong(value.trim());
            }
            sb.append(key).append(": ").append(value).append("\r\n");
        }
        sb.append("\r\n");
        try {
            if (profiler != null) {
                profiler.onBeforeRequestHeadersSent(this, sb);
            }
            final OutputStream outputStream = connectionSocket.getOutputStream();
            outputStream.write(sb.toString().getBytes("ISO-8859-1"));
            outputStream.flush();
            if (profiler != null) {
                profiler.onAfterRequestHeadersSent(this);
            }
            if (this.isRequiresOutputStream()) {
                this.outputStream = new HTTPOutputStream(outputStream);
            } else {
                this.connectInputStream();
            }
        } catch (final IOException e) {
            this.disconnect();
            throw e;
        }
    }

    @Override
    public void setAllowedResponseCodes(final int[] codes) {
        if (codes == null) {
            throw new IllegalArgumentException("codes==null");
        } else {
            this.allowedResponseCodes = codes;
        }
    }

    public void setCharset(final String Charset) {
        this.customcharset = Charset;
    }

    public void setConnectTimeout(final int connectTimeout) {
        this.requestedConnectTimeout = Math.max(0, connectTimeout);
    }

    @Override
    public void setContentDecoded(final boolean b) {
        if (this.convertedInputStream != null) {
            throw new IllegalStateException("InputStream already in use!");
        }
        this.contentDecoded = b;
    }

    public void setReadTimeout(final int readTimeout) {
        try {
            this.readTimeout = Math.max(0, readTimeout);
            final SocketStreamInterface connectionSocket = this.getConnectionSocket();
            final Socket socket;
            if (connectionSocket != null && (socket = connectionSocket.getSocket()) != null) {
                socket.setSoTimeout(this.readTimeout);
            }
        } catch (final Throwable ignore) {
        }
    }

    public void setRequestMethod(final RequestMethod method) {
        this.httpMethod = method;
    }

    public void setRequestProperty(final String key, final String value) {
        this.requestProperties.put(key, value);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(this.getRequestInfo());
        sb.append(this.getResponseInfo());
        return sb.toString();
    }

    @Override
    public void setSSLTrustALL(boolean trustALL) {
        this.sslTrustALL = trustALL;
    }

    @Override
    public boolean isSSLTrustALL() {
        return this.sslTrustALL;
    }

    @Override
    public void setLegacyConnectEnabled(boolean enabled) {
        this.legacyConnectFlag = enabled;
    }

    @Override
    public boolean isLegacyConnectEnabled() {
        return legacyConnectFlag;
    }
}
