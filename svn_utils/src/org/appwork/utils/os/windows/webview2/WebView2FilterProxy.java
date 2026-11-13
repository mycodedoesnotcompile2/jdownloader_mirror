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
 * It listens on a random port of the loopback interface only and understands nothing but proxy requests ({@code CONNECT} and requests
 * with an absolute address), so a web page in another browser cannot use it either.
 * <p>
 * Limits: for an encrypted connection only host and port are known, not the path. When a connection goes through a proxy that resolves
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

    /** The browser arguments that route everything through this filter, loopback addresses included. */
    String getBrowserArguments() {
        if (Boolean.getBoolean(TEST_OMIT_ARGUMENTS)) {
            /* Test only: simulates a browser that ignores the proxy. The self test must then refuse to start it. */
            return "";
        }
        return "--proxy-server=http://127.0.0.1:" + getPort() + " --proxy-bypass-list=<-loopback>";
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
        final OutputStream out = client.getOutputStream();
        if (!isBrowserConnection(client)) {
            /* Another program on this PC, or the browser could not be identified: never serve it. */
            respond(out, 403, "Forbidden");
            closeQuietly(client);
            return;
        }
        client.setSoTimeout(HEAD_TIMEOUT_MILLIS);
        final BufferedInputStream in = new BufferedInputStream(client.getInputStream());
        final String head = readHead(in);
        if (head == null) {
            respond(out, 400, "Bad Request");
            closeQuietly(client);
            return;
        }
        client.setSoTimeout(0);
        final int lineEnd = head.indexOf("\r\n");
        final String[] requestLine = head.substring(0, lineEnd).split(" ");
        if (requestLine.length != 3) {
            respond(out, 400, "Bad Request");
            closeQuietly(client);
            return;
        }
        final String method = requestLine[0].toUpperCase(Locale.ENGLISH);
        final boolean tunnel = "CONNECT".equals(method);
        final String host;
        final int port;
        final String url;
        String forwardHead = null;
        if (tunnel) {
            final int colon = requestLine[1].lastIndexOf(':');
            if (colon <= 0) {
                respond(out, 400, "Bad Request");
                closeQuietly(client);
                return;
            }
            host = WebView2UrlFilter.hostOf("tcp://" + requestLine[1]);
            port = parsePort(requestLine[1].substring(colon + 1));
            url = "https://" + requestLine[1] + "/";
        } else {
            /* Only a proxy request names an absolute address. A request for this port itself (a web page trying to talk to us) does not. */
            if (!requestLine[1].toLowerCase(Locale.ENGLISH).startsWith("http://")) {
                respond(out, 400, "Bad Request");
                closeQuietly(client);
                return;
            }
            url = requestLine[1];
            host = WebView2UrlFilter.hostOf(url);
            final String afterScheme = url.substring("http://".length());
            int authorityEnd = afterScheme.length();
            for (int i = 0; i < afterScheme.length(); i++) {
                final char c = afterScheme.charAt(i);
                if (c == '/' || c == '?' || c == '#') {
                    authorityEnd = i;
                    break;
                }
            }
            final String authority = afterScheme.substring(0, authorityEnd);
            final int colon = authority.lastIndexOf(':');
            port = colon > authority.lastIndexOf(']') && colon >= 0 ? parsePort(authority.substring(colon + 1)) : 80;
            String path = afterScheme.substring(authorityEnd);
            if (!path.startsWith("/")) {
                path = "/" + path;
            }
            forwardHead = rewriteHead(requestLine[0] + " " + path + " " + requestLine[2], head.substring(lineEnd + 2));
        }
        if (host == null || port <= 0) {
            respond(out, 400, "Bad Request");
            closeQuietly(client);
            return;
        }
        if (!tunnel && SELF_TEST_HOST.equals(host)) {
            /* The browser proves that its traffic arrives here. Nothing is forwarded. */
            final int nonceStart = url.lastIndexOf('/') + 1;
            final AtomicBoolean waiting = selfTests.remove(url.substring(nonceStart));
            if (waiting != null) {
                waiting.set(true);
            }
            respond(out, 204, "No Content");
            closeQuietly(client);
            return;
        }
        final Socket upstream;
        try {
            upstream = connect(tunnel, host, port, url);
        } catch (BlockedException blocked) {
            for (Policy policy : policies) {
                policy.onBlocked(url);
            }
            respond(out, 403, "Blocked");
            closeQuietly(client);
            return;
        } catch (IOException e) {
            respond(out, 502, "Bad Gateway");
            closeQuietly(client);
            return;
        }
        try {
            if (tunnel) {
                out.write("HTTP/1.1 200 Connection established\r\n\r\n".getBytes("ISO-8859-1"));
                out.flush();
            } else {
                final OutputStream upstreamOut = upstream.getOutputStream();
                upstreamOut.write(forwardHead.getBytes("ISO-8859-1"));
                upstreamOut.flush();
            }
            relay(client, in, upstream);
        } finally {
            closeQuietly(upstream);
            closeQuietly(client);
        }
    }

    private static final class BlockedException extends IOException {
        private static final long serialVersionUID = 1L;

        private BlockedException(String message) {
            super(message);
        }
    }

    /** Applies the policies and opens the connection to the target. */
    private Socket connect(boolean tunnel, String host, int port, String url) throws IOException {
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
                /* Of a tunnel only host and port are known; it may carry https, wss or ws. */
                final boolean allowed = tunnel ? (filter.isConnectionAllowed(true, host, port) || filter.isConnectionAllowed(false, host, port)) : filter.isAllowed(url, false);
                if (!allowed) {
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

    /**
     * Turns a proxy request into a request for the server itself: the request line names the path only, proxy headers are dropped and
     * the connection is used for this one request. The browser may put the next request for another server on a kept connection,
     * which would skip the checks.
     */
    private static String rewriteHead(String requestLine, String headers) {
        final StringBuilder result = new StringBuilder(requestLine).append("\r\n");
        for (String line : headers.split("\r\n")) {
            if (line.length() == 0) {
                continue;
            }
            final String lower = line.toLowerCase(Locale.ENGLISH);
            if (lower.startsWith("proxy-") || lower.startsWith("connection:") || lower.startsWith("keep-alive:")) {
                continue;
            }
            result.append(line).append("\r\n");
        }
        return result.append("Connection: close\r\n\r\n").toString();
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
