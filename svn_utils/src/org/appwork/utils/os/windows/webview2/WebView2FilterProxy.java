/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.webview2;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.socketconnection.HTTPProxySocketConnection;
import org.appwork.utils.net.socketconnection.Socks4SocketConnection;
import org.appwork.utils.net.socketconnection.Socks5SocketConnection;

import com.sun.jna.Memory;
import com.sun.jna.platform.win32.IPHlpAPI;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Tlhelp32;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;

/**
 * Filter between a {@link WindowsWebView2Browser} and the network. The browser is started with this filter as its only proxy, also for
 * loopback addresses, so every connection it opens passes here: page loads, resources, XHR/fetch, WebSockets, workers. That is the
 * only place where connections can be judged that WebView2 does not report as a request.
 * <p>
 * For each connection the filter
 * <ul>
 * <li>accepts it only from the browser processes it was started for; every other program on this PC is refused,</li>
 * <li>refuses a target on this PC or in the local network (unless the application opened that host itself), judged by the address the
 * name really resolves to, and connects to exactly that address,</li>
 * <li>applies the {@link WebView2UrlFilter} of the browser to the target,</li>
 * <li>connects directly or through the proxy chosen by the {@link WebView2ProxySelector}.</li>
 * </ul>
 * It listens on a random port of the loopback interface only and speaks SOCKS5 with {@code CONNECT} only (no authentication, no
 * {@code BIND}, no {@code UDP ASSOCIATE}). Every TCP connection of the browser is one SOCKS connection to one target, so a kept
 * connection can never carry requests for another server past the checks.
 * <p>
 * Limits: only host and port of a connection are known here, never the path, also for plain http; path rules of the
 * {@link WebView2UrlFilter} are applied by the browser to the requests WebView2 reports. UDP does not pass this filter; the profile
 * of the browser is set so that WebRTC uses no UDP that bypasses the proxy. When a connection goes through a proxy that resolves
 * names itself ({@link HTTPProxy#isResolveHostname()} is {@code false}), the address is resolved by that proxy and cannot be pinned
 * here; the name is still looked up locally and refused if it points to a local address.
 */
public final class WebView2FilterProxy {
    /** What one browser allows. All browsers that share a profile share one filter; a connection must pass the policy of each. */
    interface Policy {
        boolean isBlockLocalNetwork();

        /** The application itself opened this local host. */
        boolean isLocalTargetAllowed(String host);

        WebView2UrlFilter getUrlFilter();

        WebView2ProxySelector getProxySelector();

        void onBlocked(String url);
    }

    /** Name lookup; replaceable in tests. */
    public interface Resolver {
        InetAddress[] resolve(String host) throws IOException;
    }

    private static final Resolver                         SYSTEM_RESOLVER     = new Resolver() {
                                                                                  public InetAddress[] resolve(String host) throws IOException {
                                                                                      return InetAddress.getAllByName(host);
                                                                                  }
                                                                              };
    /** A name that never exists in DNS; requests for it end at the filter, see {@link #expectSelfTest(String)}. */
    static final String                                   SELF_TEST_HOST      = "webview2-filter-selftest.invalid";
    /**
     * System property for tests: {@code true} leaves out the browser arguments. It can only make the creation of a browser fail,
     * never make a browser less safe, because a browser without the filter does not pass the self test.
     */
    public static final String                            TEST_OMIT_ARGUMENTS = "webview2.test.omitFilterProxyArguments";
    private static final int                              MAX_HEAD_BYTES      = 64 * 1024;
    private static final int                              HEAD_TIMEOUT_MILLIS = (int) TimeUnit.SECONDS.toMillis(30);
    private static final int                              CONNECT_TIMEOUT     = (int) TimeUnit.SECONDS.toMillis(30);
    private static final byte                             SOCKS_VERSION       = 0x05;
    private static final int                              SOCKS_SUCCEEDED     = 0x00;
    private static final int                              SOCKS_GENERAL_FAILURE = 0x01;
    private static final int                              SOCKS_NOT_ALLOWED   = 0x02;
    private static final int                              SOCKS_HOST_UNREACHABLE = 0x04;
    private static final int                              SOCKS_COMMAND_NOT_SUPPORTED = 0x07;
    private static final int                              SOCKS_ADDRESS_TYPE_NOT_SUPPORTED = 0x08;
    private static final int                              AF_INET             = 2;
    /** {@code TCP_TABLE_OWNER_PID_CONNECTIONS} */
    private static final int                              TCP_TABLE_OWNER_PID = 4;
    private static final int                              LOOPBACK_ADDRESS    = 0x0100007F;
    private static final Map<String, WebView2FilterProxy> SHARED              = new HashMap<String, WebView2FilterProxy>();
    private static final AtomicInteger                    THREAD_ID           = new AtomicInteger();
    private static volatile Resolver                      resolver            = SYSTEM_RESOLVER;
    private final ServerSocket                            server;
    private final ExecutorService                         workers;
    private final CopyOnWriteArrayList<Policy>            policies            = new CopyOnWriteArrayList<Policy>();
    /** Process ids of the browsers this filter serves. */
    private final ConcurrentHashMap<Integer, Boolean>     browserProcesses    = new ConcurrentHashMap<Integer, Boolean>();
    private final ConcurrentHashMap<String, AtomicBoolean> selfTests          = new ConcurrentHashMap<String, AtomicBoolean>();
    private volatile boolean                              closed;

    private WebView2FilterProxy() throws IOException {
        this.server = new ServerSocket(0, 100, InetAddress.getByAddress(new byte[] { 127, 0, 0, 1 }));
        this.workers = Executors.newCachedThreadPool(new ThreadFactory() {
            public Thread newThread(Runnable r) {
                final Thread thread = new Thread(r, "WebView2-FilterProxy-" + THREAD_ID.incrementAndGet());
                thread.setDaemon(true);
                return thread;
            }
        });
        final Thread accept = new Thread(new Runnable() {
            public void run() {
                acceptLoop();
            }
        }, "WebView2-FilterProxy-Accept");
        accept.setDaemon(true);
        accept.start();
    }

    /**
     * The filter for the browsers of one profile folder. Browsers that share a persistent profile run in one browser process and
     * must use the same proxy, so they get the same filter, which stays open for the life of the application.
     *
     * @param shared
     *            {@code false} for a throw-away profile: a filter of its own, closed with its browser
     */
    static WebView2FilterProxy forProfile(String userDataFolder, boolean shared) throws IOException {
        if (!shared) {
            return new WebView2FilterProxy();
        }
        final String key = userDataFolder.toLowerCase(Locale.ENGLISH);
        synchronized (SHARED) {
            WebView2FilterProxy proxy = SHARED.get(key);
            if (proxy == null || proxy.closed) {
                proxy = new WebView2FilterProxy();
                SHARED.put(key, proxy);
            }
            return proxy;
        }
    }

    /** Test hook: replaces the name lookup of all filters; {@code null} restores the lookup of the system. */
    public static void setResolver(Resolver custom) {
        resolver = custom != null ? custom : SYSTEM_RESOLVER;
    }

    public int getPort() {
        return server.getLocalPort();
    }

    /**
     * The browser arguments that route everything through this filter, loopback addresses included. {@code socks5://} makes the
     * browser send the host name; it does not resolve names itself. SOCKS5 of the browser carries TCP only; that WebRTC sends no UDP
     * past this filter is set in the profile, see {@code WindowsWebView2Browser.applyWebRtcPolicy} (WebView2 has no command line
     * switch for it).
     */
    String getBrowserArguments() {
        if (Boolean.getBoolean(TEST_OMIT_ARGUMENTS)) {
            /* Test only: simulates a browser that ignores the proxy. The self test must then refuse to start it. */
            return "";
        }
        return "--proxy-server=socks5://127.0.0.1:" + getPort() + " --proxy-bypass-list=<-loopback>";
    }

    void addPolicy(Policy policy) {
        policies.addIfAbsent(policy);
    }

    void removePolicy(Policy policy) {
        policies.remove(policy);
    }

    /**
     * Starts a self test: the returned flag turns {@code true} when a request of the browser for
     * {@code http://}{@link #SELF_TEST_HOST}{@code /nonce} arrived at this filter, which shows that the browser really sends its
     * traffic here and that it is recognized as the browser.
     */
    AtomicBoolean expectSelfTest(String nonce) {
        final AtomicBoolean arrived = new AtomicBoolean(false);
        selfTests.put(nonce, arrived);
        return arrived;
    }

    void cancelSelfTest(String nonce) {
        selfTests.remove(nonce);
    }

    /** Allows the browser process {@code pid} and its child processes to connect. */
    void allowProcess(int pid) {
        if (pid > 0) {
            browserProcesses.put(Integer.valueOf(pid), Boolean.TRUE);
        }
    }

    void close() {
        closed = true;
        try {
            server.close();
        } catch (IOException ignored) {
            /* Already closed. */
        }
        workers.shutdownNow();
    }

    private void acceptLoop() {
        while (!closed) {
            final Socket client;
            try {
                client = server.accept();
            } catch (IOException e) {
                if (!closed) {
                    LogV3.log(e);
                }
                return;
            }
            try {
                workers.execute(new Runnable() {
                    public void run() {
                        try {
                            handle(client);
                        } catch (Throwable t) {
                            closeQuietly(client);
                        }
                    }
                });
            } catch (RuntimeException rejected) {
                closeQuietly(client);
            }
        }
    }

    private void handle(Socket client) throws IOException {
        client.setTcpNoDelay(true);
        if (!isBrowserConnection(client)) {
            /* Another program on this PC, or the browser could not be identified: never serve it, not even with a protocol answer. */
            closeQuietly(client);
            return;
        }
        client.setSoTimeout(HEAD_TIMEOUT_MILLIS);
        final DataInputStream in = new DataInputStream(new BufferedInputStream(client.getInputStream()));
        final OutputStream out = client.getOutputStream();
        final String authority;
        try {
            authority = readSocksRequest(in, out);
        } catch (IOException e) {
            /* Not SOCKS5, too slow, or the client closed. */
            closeQuietly(client);
            return;
        }
        if (authority == null) {
            closeQuietly(client);
            return;
        }
        final String host = WebView2UrlFilter.hostOf("tcp://" + authority);
        final int port = parsePort(authority.substring(authority.lastIndexOf(':') + 1));
        if (host == null || port <= 0) {
            socksReply(out, SOCKS_GENERAL_FAILURE);
            closeQuietly(client);
            return;
        }
        /* Only host and port of the target are known. Port 80 is taken as plain http, every other port as an encrypted connection. */
        final String url = (port == 80 ? "http://" : "https://") + authority + "/";
        if (SELF_TEST_HOST.equals(host)) {
            answerSelfTest(in, out);
            closeQuietly(client);
            return;
        }
        final Socket upstream;
        try {
            upstream = connect(host, port, url);
        } catch (BlockedException blocked) {
            for (Policy policy : policies) {
                policy.onBlocked(url);
            }
            socksReply(out, SOCKS_NOT_ALLOWED);
            closeQuietly(client);
            return;
        } catch (IOException e) {
            socksReply(out, SOCKS_HOST_UNREACHABLE);
            closeQuietly(client);
            return;
        }
        try {
            socksReply(out, SOCKS_SUCCEEDED);
            client.setSoTimeout(0);
            relay(client, in, upstream);
        } finally {
            closeQuietly(upstream);
            closeQuietly(client);
        }
    }

    /**
     * Reads the SOCKS5 greeting and the request (RFC 1928). Only "no authentication" and {@code CONNECT} are accepted; {@code BIND},
     * {@code UDP ASSOCIATE} and every other command are refused, so no UDP passes this filter.
     *
     * @return the target as {@code host:port} ({@code [address]:port} for IPv6), or {@code null} when the request was refused (the
     *         refusal was already sent)
     */
    private static String readSocksRequest(DataInputStream in, OutputStream out) throws IOException {
        if (in.readUnsignedByte() != SOCKS_VERSION) {
            return null;
        }
        final int methodCount = in.readUnsignedByte();
        boolean noAuthentication = false;
        for (int i = 0; i < methodCount; i++) {
            noAuthentication |= in.readUnsignedByte() == 0x00;
        }
        if (!noAuthentication) {
            out.write(new byte[] { SOCKS_VERSION, (byte) 0xFF });
            out.flush();
            return null;
        }
        out.write(new byte[] { SOCKS_VERSION, 0x00 });
        out.flush();
        if (in.readUnsignedByte() != SOCKS_VERSION) {
            return null;
        }
        final int command = in.readUnsignedByte();
        in.readUnsignedByte();
        final int addressType = in.readUnsignedByte();
        String host;
        switch (addressType) {
        case 0x01: {
            final byte[] address = new byte[4];
            in.readFully(address);
            host = InetAddress.getByAddress(address).getHostAddress();
            break;
        }
        case 0x03: {
            final byte[] name = new byte[in.readUnsignedByte()];
            in.readFully(name);
            host = new String(name, "ISO-8859-1");
            if (!isValidHostName(host)) {
                socksReply(out, SOCKS_GENERAL_FAILURE);
                return null;
            }
            break;
        }
        case 0x04: {
            final byte[] address = new byte[16];
            in.readFully(address);
            host = InetAddress.getByAddress(address).getHostAddress();
            break;
        }
        default:
            socksReply(out, SOCKS_ADDRESS_TYPE_NOT_SUPPORTED);
            return null;
        }
        final int port = in.readUnsignedShort();
        if (command != 0x01) {
            /* Only CONNECT. */
            socksReply(out, SOCKS_COMMAND_NOT_SUPPORTED);
            return null;
        }
        if (host.indexOf(':') >= 0) {
            host = "[" + host + "]";
        }
        return host + ":" + port;
    }

    /** Letters, digits, dot, hyphen, underscore, and colons of an IPv6 literal; nothing that could change how the address is read. */
    private static boolean isValidHostName(String host) {
        if (host.length() == 0) {
            return false;
        }
        for (int i = 0; i < host.length(); i++) {
            final char c = host.charAt(i);
            if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '.' || c == '-' || c == '_' || c == ':')) {
                return false;
            }
        }
        return true;
    }

    /** Reply to the SOCKS5 request; the bound address is not used by the browser and sent as {@code 0.0.0.0:0}. */
    private static void socksReply(OutputStream out, int code) {
        try {
            out.write(new byte[] { SOCKS_VERSION, (byte) code, 0x00, 0x01, 0, 0, 0, 0, 0, 0 });
            out.flush();
        } catch (IOException ignored) {
            /* The client is gone. */
        }
    }

    /**
     * The browser proves that its traffic arrives here: it opened a connection to {@link #SELF_TEST_HOST} and sends
     * {@code GET /nonce} through it. Nothing is forwarded.
     */
    private void answerSelfTest(InputStream in, OutputStream out) throws IOException {
        socksReply(out, SOCKS_SUCCEEDED);
        final String head = readHead(in);
        if (head == null) {
            return;
        }
        final String[] requestLine = head.substring(0, head.indexOf("\r\n")).split(" ");
        if (requestLine.length == 3) {
            final AtomicBoolean waiting = selfTests.remove(requestLine[1].substring(requestLine[1].lastIndexOf('/') + 1));
            if (waiting != null) {
                waiting.set(true);
            }
        }
        respond(out, 204, "No Content");
    }

    private static final class BlockedException extends IOException {
        private static final long serialVersionUID = 1L;

        private BlockedException(String message) {
            super(message);
        }
    }

    /** Applies the policies and opens the connection to the target. */
    private Socket connect(String host, int port, String url) throws IOException {
        if (policies.isEmpty()) {
            throw new BlockedException("No browser is using this filter");
        }
        boolean blockLocal = false;
        boolean localAllowed = false;
        WebView2ProxySelector selector = null;
        for (Policy policy : policies) {
            blockLocal |= policy.isBlockLocalNetwork();
            localAllowed |= policy.isLocalTargetAllowed(host);
            if (selector == null) {
                selector = policy.getProxySelector();
            }
            final WebView2UrlFilter filter = policy.getUrlFilter();
            if (filter != null) {
                /* Only host and port are known; the connection may carry http, https, ws or wss. Paths are judged by the browser. */
                if (!filter.isConnectionAllowed(true, host, port) && !filter.isConnectionAllowed(false, host, port)) {
                    throw new BlockedException("URL filter: " + url);
                }
            }
        }
        final boolean guardLocal = blockLocal && !localAllowed;
        if (guardLocal && WebView2UrlFilter.isLocalNetworkHost(host)) {
            throw new BlockedException("Local network: " + host);
        }
        final List<HTTPProxy> proxies = selector != null ? selector.getProxiesByURL(url) : null;
        final ArrayList<HTTPProxy> candidates = new ArrayList<HTTPProxy>();
        if (proxies != null) {
            for (HTTPProxy proxy : proxies) {
                if (proxy != null) {
                    candidates.add(proxy);
                }
            }
        }
        if (candidates.isEmpty()) {
            candidates.add(HTTPProxy.NONE);
        }
        IOException last = null;
        for (HTTPProxy proxy : candidates) {
            try {
                return connect(proxy, host, port, guardLocal);
            } catch (BlockedException blocked) {
                throw blocked;
            } catch (IOException e) {
                last = e;
            }
        }
        throw last != null ? last : new IOException("No connection to " + host);
    }

    private Socket connect(HTTPProxy proxy, String host, int port, boolean guardLocal) throws IOException {
        final boolean direct;
        switch (proxy.getType()) {
        case HTTP:
        case HTTPS:
        case SOCKS4:
        case SOCKS4A:
        case SOCKS5:
            direct = false;
            break;
        case NONE:
        case DIRECT:
        default:
            direct = true;
            break;
        }
        InetAddress pinned = null;
        if (direct || proxy.isResolveHostname()) {
            final InetAddress[] addresses = resolver.resolve(host);
            if (addresses == null || addresses.length == 0) {
                throw new IOException("Unknown host " + host);
            }
            if (guardLocal) {
                /* Every address the name has must be public: the one we connect to is among those that were checked. */
                for (InetAddress address : addresses) {
                    if (WebView2UrlFilter.isLocalAddress(address)) {
                        throw new BlockedException("Local network: " + host + " resolves to " + address.getHostAddress());
                    }
                }
            }
            pinned = addresses[0];
        } else if (guardLocal) {
            /* The proxy resolves the name itself. Look it up here as well and refuse a name that points into the local network. */
            try {
                for (InetAddress address : resolver.resolve(host)) {
                    if (WebView2UrlFilter.isLocalAddress(address)) {
                        throw new BlockedException("Local network: " + host + " resolves to " + address.getHostAddress());
                    }
                }
            } catch (BlockedException blocked) {
                throw blocked;
            } catch (IOException unknownHere) {
                /* Only the proxy can resolve it. */
            }
        }
        final InetSocketAddress endpoint = pinned != null ? new InetSocketAddress(pinned, port) : InetSocketAddress.createUnresolved(host, port);
        final Socket socket;
        switch (proxy.getType()) {
        case HTTP:
        case HTTPS:
            socket = new HTTPProxySocketConnection(proxy);
            break;
        case SOCKS4:
        case SOCKS4A:
            socket = new Socks4SocketConnection(proxy);
            break;
        case SOCKS5:
            socket = new Socks5SocketConnection(proxy);
            break;
        case NONE:
        case DIRECT:
        default:
            socket = new Socket();
            break;
        }
        try {
            socket.connect(endpoint, CONNECT_TIMEOUT);
            socket.setTcpNoDelay(true);
            return socket;
        } catch (IOException e) {
            closeQuietly(socket);
            throw e;
        } catch (RuntimeException e) {
            closeQuietly(socket);
            throw new IOException(e);
        }
    }

    /** Copies both directions until one side closes. */
    private void relay(final Socket client, final InputStream clientIn, final Socket upstream) throws IOException {
        final OutputStream upstreamOut = upstream.getOutputStream();
        workers.execute(new Runnable() {
            public void run() {
                try {
                    copy(clientIn, upstreamOut);
                } catch (IOException ignored) {
                    /* One side closed. */
                } finally {
                    closeQuietly(upstream);
                    closeQuietly(client);
                }
            }
        });
        copy(upstream.getInputStream(), client.getOutputStream());
    }

    private static void copy(InputStream in, OutputStream out) throws IOException {
        final byte[] buffer = new byte[16 * 1024];
        int read;
        while ((read = in.read(buffer)) >= 0) {
            out.write(buffer, 0, read);
            out.flush();
        }
    }

    /** @return the request head including the empty line, or {@code null} when it is too large or the client closed */
    private static String readHead(InputStream in) throws IOException {
        final ByteArrayOutputStream head = new ByteArrayOutputStream();
        int matched = 0;
        while (head.size() < MAX_HEAD_BYTES) {
            final int b = in.read();
            if (b < 0) {
                return null;
            }
            head.write(b);
            if ((matched % 2 == 0 && b == '\r') || (matched % 2 == 1 && b == '\n')) {
                matched++;
                if (matched == 4) {
                    return head.toString("ISO-8859-1");
                }
            } else {
                matched = b == '\r' ? 1 : 0;
            }
        }
        return null;
    }

    private static int parsePort(String text) {
        try {
            final int port = Integer.parseInt(text.trim());
            return port > 0 && port < 65536 ? port : -1;
        } catch (NumberFormatException e) {
            return -1;
        }
    }

    private static void respond(OutputStream out, int status, String reason) {
        try {
            out.write(("HTTP/1.1 " + status + " " + reason + "\r\nContent-Length: 0\r\nConnection: close\r\n\r\n").getBytes("ISO-8859-1"));
            out.flush();
        } catch (IOException ignored) {
            /* The client is gone. */
        }
    }

    private static void closeQuietly(Socket socket) {
        if (socket != null) {
            try {
                socket.close();
            } catch (IOException ignored) {
                /* Already closed. */
            }
        }
    }

    /**
     * {@code true} when the program at the other end of {@code client} is one of the browser processes of this filter or one of
     * their child processes (the network service of the browser is a child process). Fails closed: when the owner of the connection
     * cannot be found out, it is not served.
     */
    private boolean isBrowserConnection(Socket client) {
        try {
            if (browserProcesses.isEmpty() || !client.getInetAddress().isLoopbackAddress()) {
                return false;
            }
            final int pid = owningProcess(client.getPort(), client.getLocalPort());
            if (pid <= 0) {
                return false;
            }
            if (browserProcesses.containsKey(Integer.valueOf(pid))) {
                return true;
            }
            final Map<Integer, Integer> parents = parentProcesses();
            Integer current = Integer.valueOf(pid);
            for (int depth = 0; depth < 4 && current != null; depth++) {
                current = parents.get(current);
                if (current != null && browserProcesses.containsKey(current)) {
                    return true;
                }
            }
            return false;
        } catch (Throwable t) {
            LogV3.log(t);
            return false;
        }
    }

    /** Process id that owns the loopback TCP connection from {@code clientPort} to {@code serverPort}, or -1. */
    private static int owningProcess(int clientPort, int serverPort) {
        final IntByReference size = new IntByReference(0);
        IPHlpAPI.INSTANCE.GetExtendedTcpTable(null, size, false, AF_INET, TCP_TABLE_OWNER_PID, 0);
        for (int attempt = 0; attempt < 3; attempt++) {
            final Memory table = new Memory(Math.max(4, size.getValue() + 1024));
            size.setValue((int) table.size());
            final int result = IPHlpAPI.INSTANCE.GetExtendedTcpTable(table, size, false, AF_INET, TCP_TABLE_OWNER_PID, 0);
            if (result != 0) {
                /* ERROR_INSUFFICIENT_BUFFER: the table grew, try again with the new size. */
                continue;
            }
            final int entries = table.getInt(0);
            /* MIB_TCPROW_OWNER_PID: state, localAddr, localPort, remoteAddr, remotePort, owningPid (6 DWORDs). */
            for (int i = 0; i < entries; i++) {
                final long row = 4L + i * 24L;
                final int localAddress = table.getInt(row + 4);
                final int localPort = networkPort(table.getInt(row + 8));
                final int remoteAddress = table.getInt(row + 12);
                final int remotePort = networkPort(table.getInt(row + 16));
                if (localPort == clientPort && remotePort == serverPort && localAddress == LOOPBACK_ADDRESS && remoteAddress == LOOPBACK_ADDRESS) {
                    return table.getInt(row + 20);
                }
            }
            return -1;
        }
        return -1;
    }

    private static int networkPort(int value) {
        return ((value & 0xFF) << 8) | ((value >> 8) & 0xFF);
    }

    /** Process id to parent process id of all running processes. */
    private static Map<Integer, Integer> parentProcesses() {
        final HashMap<Integer, Integer> parents = new HashMap<Integer, Integer>();
        final HANDLE snapshot = Kernel32.INSTANCE.CreateToolhelp32Snapshot(Tlhelp32.TH32CS_SNAPPROCESS, new DWORD(0));
        if (snapshot == null || Kernel32.INVALID_HANDLE_VALUE.equals(snapshot)) {
            return parents;
        }
        try {
            final Tlhelp32.PROCESSENTRY32.ByReference entry = new Tlhelp32.PROCESSENTRY32.ByReference();
            if (Kernel32.INSTANCE.Process32First(snapshot, entry)) {
                do {
                    parents.put(Integer.valueOf(entry.th32ProcessID.intValue()), Integer.valueOf(entry.th32ParentProcessID.intValue()));
                } while (Kernel32.INSTANCE.Process32Next(snapshot, entry));
            }
        } finally {
            Kernel32.INSTANCE.CloseHandle(snapshot);
        }
        return parents;
    }
}
