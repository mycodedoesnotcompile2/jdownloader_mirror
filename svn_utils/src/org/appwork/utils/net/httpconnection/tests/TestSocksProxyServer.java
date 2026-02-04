package org.appwork.utils.net.httpconnection.tests;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.loggingv3.LogV3;

/**
 * Java 1.6 compatible SOCKS5 test proxy server.
 *
 * Key fixes for your issue: - DOMAIN targets are resolved via InetAddress.getByName(host) (no unresolved connect) - Outbound connection
 * uses Proxy.NO_PROXY to avoid recursion via system SOCKS settings - No flush-per-chunk; flush once - Explicit charset (no
 * StandardCharsets) - Read timeout to avoid stuck handlers - Null-safe closes - Useful debug logs (auth, request parsing, DNS, connect
 * impl, forwarding stats)
 */
public class TestSocksProxyServer {
    private static final Charset UTF8                             = Charset.forName("UTF-8");
    private static final Charset ISO_8859_1                       = Charset.forName("ISO-8859-1");
    private ServerSocket         serverSocket;
    private Thread               serverThread;
    private final AtomicBoolean  running                          = new AtomicBoolean(false);
    private int                  port;
    private String               username;
    private String               password;
    // SOCKS5 constants
    private static final byte    SOCKS_VERSION                    = 0x05;
    private static final byte    AUTH_NONE                        = 0x00;
    private static final byte    AUTH_USERNAME_PASSWORD           = 0x02;
    private static final byte    AUTH_NOT_ACCEPTABLE              = (byte) 0xFF;
    private static final byte    CMD_CONNECT                      = 0x01;
    private static final byte    ADDR_TYPE_IPV4                   = 0x01;
    private static final byte    ADDR_TYPE_DOMAIN                 = 0x03;
    private static final byte    ADDR_TYPE_IPV6                   = 0x04;
    private static final byte    REPLY_SUCCESS                    = 0x00;
    private static final byte    REPLY_GENERAL_FAILURE            = 0x01;
    private static final byte    REPLY_HOST_UNREACHABLE           = 0x04;
    private static final byte    REPLY_COMMAND_NOT_SUPPORTED      = 0x07;
    private static final byte    REPLY_ADDRESS_TYPE_NOT_SUPPORTED = 0x08;

    public TestSocksProxyServer(int port, String username, String password) {
        this.port = port;
        this.username = username;
        this.password = password;
    }

    public TestSocksProxyServer(int port) {
        this(port, null, null);
    }

    public void start() throws IOException {
        if (running.get()) {
            throw new IllegalStateException("Server is already running");
        }
        serverSocket = new ServerSocket(port);
        serverSocket.setReuseAddress(true);
        if (port == 0) {
            port = serverSocket.getLocalPort();
        }
        running.set(true);
        serverThread = new Thread(new Runnable() {
            public void run() {
                runServer();
            }
        }, "TestSocksProxyServer-" + port);
        serverThread.setDaemon(true);
        serverThread.start();
        LogV3.info("TestSocksProxyServer started on port " + port);
    }

    public void stop() {
        if (!running.get()) {
            return;
        }
        running.set(false);
        try {
            if (serverSocket != null && !serverSocket.isClosed()) {
                serverSocket.close();
            }
        } catch (IOException e) {
            LogV3.log(e);
        }
        if (serverThread != null) {
            try {
                serverThread.join(5000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        LogV3.info("TestSocksProxyServer stopped");
    }

    public int getPort() {
        return port;
    }

    public boolean isRunning() {
        return running.get();
    }

    private void runServer() {
        while (running.get()) {
            try {
                final Socket clientSocket = serverSocket.accept();
                Thread t = new Thread(new Runnable() {
                    public void run() {
                        handleClient(clientSocket);
                    }
                }, "SocksHandler-" + String.valueOf(clientSocket.getRemoteSocketAddress()));
                t.setDaemon(true);
                t.start();
            } catch (IOException e) {
                if (running.get()) {
                    LogV3.log(e);
                }
            }
        }
    }

    private void handleClient(Socket clientSocket) {
        Socket targetSocket = null;
        InetSocketAddress target = null;
        try {
            clientSocket.setSoTimeout(15000);
            InputStream input = clientSocket.getInputStream();
            OutputStream output = clientSocket.getOutputStream();
            LogV3.info("SOCKS CLIENT connected: " + clientSocket.getRemoteSocketAddress());
            // 1) Auth negotiation
            if (!handleAuthentication(input, output)) {
                LogV3.info("SOCKS AUTH failed/closed for client: " + clientSocket.getRemoteSocketAddress());
                return;
            }
            LogV3.info("SOCKS AUTH ok for client: " + clientSocket.getRemoteSocketAddress());
            // 2) CONNECT request
            target = handleConnectionRequest(input, output);
            if (target == null) {
                LogV3.info("SOCKS CONNECT request invalid from client: " + clientSocket.getRemoteSocketAddress());
                return;
            }
            LogV3.info("SOCKS CONNECT -> " + formatTarget(target) + " unresolved=" + target.isUnresolved());
            // 3) Connect to target WITHOUT any system proxies to avoid SOCKS recursion
            targetSocket = new Socket(Proxy.NO_PROXY);
            LogV3.info("SOCKS OUTBOUND socketImpl=" + targetSocket.getClass().getName());
            targetSocket.connect(target, 10000);
            LogV3.info("SOCKS CONNECT OK -> " + formatTarget(target));
            sendConnectionReply(output, REPLY_SUCCESS);
            final Socket finalTargetSocket = targetSocket;
            final InetSocketAddress fTarget = target;
            Thread forwardToTarget = new Thread(new Runnable() {
                public void run() {
                    try {
                        forwardData(clientSocket, finalTargetSocket, true, fTarget);
                    } catch (IOException e) {
                        // ignore
                    } finally {
                        safeClose(finalTargetSocket, "targetSocket (C->T)", fTarget);
                    }
                }
            }, "ForwardToTarget");
            forwardToTarget.setDaemon(true);
            forwardToTarget.start();
            // T -> C in current thread
            forwardData(finalTargetSocket, clientSocket, false, target);
        } catch (IOException e) {
            LogV3.info("SOCKS ERROR (target=" + (target != null ? formatTarget(target) : "n/a") + "): " + e.getClass().getSimpleName() + " / " + e.getMessage());
            LogV3.log(e);
            try {
                sendConnectionReply(clientSocket.getOutputStream(), REPLY_HOST_UNREACHABLE);
            } catch (Throwable ignore) {
            }
        } finally {
            safeClose(clientSocket, "clientSocket", target);
            if (targetSocket != null) {
                safeClose(targetSocket, "targetSocket", target);
            }
            LogV3.info("SOCKS CLIENT closed: " + String.valueOf(clientSocket.getRemoteSocketAddress()) + (target != null ? (" target=" + formatTarget(target)) : ""));
        }
    }

    private boolean handleAuthentication(InputStream input, OutputStream output) throws IOException {
        byte[] header = new byte[2];
        if (readFully(input, header) != 2) {
            return false;
        }
        if (header[0] != SOCKS_VERSION) {
            LogV3.info("SOCKS invalid version in auth: " + (header[0] & 0xFF));
            return false;
        }
        int numMethods = header[1] & 0xFF;
        byte[] methods = new byte[numMethods];
        if (readFully(input, methods) != numMethods) {
            return false;
        }
        boolean requiresAuth = (username != null && password != null);
        boolean supportsNoAuth = false;
        boolean supportsUserPass = false;
        for (int i = 0; i < methods.length; i++) {
            byte m = methods[i];
            if (m == AUTH_NONE) {
                supportsNoAuth = true;
            } else if (m == AUTH_USERNAME_PASSWORD) {
                supportsUserPass = true;
            }
        }
        LogV3.info("SOCKS AUTH methods offered: noAuth=" + supportsNoAuth + " userPass=" + supportsUserPass + " requiresAuth=" + requiresAuth);
        byte selected;
        if (requiresAuth) {
            if (!supportsUserPass) {
                output.write(new byte[] { SOCKS_VERSION, AUTH_NOT_ACCEPTABLE });
                output.flush();
                return false;
            }
            selected = AUTH_USERNAME_PASSWORD;
        } else {
            // be flexible: if no-auth not offered but user/pass is, accept user/pass anyway
            if (supportsNoAuth) {
                selected = AUTH_NONE;
            } else if (supportsUserPass) {
                selected = AUTH_USERNAME_PASSWORD;
            } else {
                output.write(new byte[] { SOCKS_VERSION, AUTH_NOT_ACCEPTABLE });
                output.flush();
                return false;
            }
        }
        output.write(new byte[] { SOCKS_VERSION, selected });
        output.flush();
        if (selected == AUTH_USERNAME_PASSWORD) {
            boolean ok = handleUsernamePasswordAuth(input, output, requiresAuth);
            LogV3.info("SOCKS AUTH user/pass result=" + ok);
            return ok;
        }
        return true;
    }

    private boolean handleUsernamePasswordAuth(InputStream input, OutputStream output, boolean requiresAuth) throws IOException {
        int version = input.read();
        if (version != 0x01) {
            LogV3.info("SOCKS USERPASS invalid version: " + version);
            return false;
        }
        int ulen = input.read();
        if (ulen < 0) {
            return false;
        }
        byte[] ub = new byte[ulen & 0xFF];
        if (readFully(input, ub) != ub.length) {
            return false;
        }
        String providedUsername = new String(ub, UTF8);
        int plen = input.read();
        if (plen < 0) {
            return false;
        }
        byte[] pb = new byte[plen & 0xFF];
        if (readFully(input, pb) != pb.length) {
            return false;
        }
        String providedPassword = new String(pb, UTF8);
        boolean authenticated;
        if (requiresAuth) {
            authenticated = providedUsername.equals(username) && providedPassword.equals(password);
        } else {
            authenticated = true; // server doesn't require creds
        }
        output.write(new byte[] { 0x01, (byte) (authenticated ? 0x00 : 0x01) });
        output.flush();
        return authenticated;
    }

    /**
     * Java 1.6 safe: - IPv4/IPv6 => resolved InetAddress - DOMAIN => resolve now via InetAddress.getByName(host) (avoid unresolved connect
     * failures)
     */
    private InetSocketAddress handleConnectionRequest(InputStream input, OutputStream output) throws IOException {
        byte[] req = new byte[4];
        if (readFully(input, req) != 4) {
            sendConnectionReply(output, REPLY_GENERAL_FAILURE);
            return null;
        }
        if (req[0] != SOCKS_VERSION) {
            LogV3.info("SOCKS invalid version in request: " + (req[0] & 0xFF));
            sendConnectionReply(output, REPLY_GENERAL_FAILURE);
            return null;
        }
        if (req[1] != CMD_CONNECT) {
            LogV3.info("SOCKS unsupported command: " + (req[1] & 0xFF));
            sendConnectionReply(output, REPLY_COMMAND_NOT_SUPPORTED);
            return null;
        }
        byte atyp = req[3];
        InetAddress ip = null;
        String host = null;
        if (atyp == ADDR_TYPE_IPV4) {
            byte[] addr = new byte[4];
            if (readFully(input, addr) != 4) {
                sendConnectionReply(output, REPLY_GENERAL_FAILURE);
                return null;
            }
            ip = InetAddress.getByAddress(addr);
        } else if (atyp == ADDR_TYPE_IPV6) {
            byte[] addr = new byte[16];
            if (readFully(input, addr) != 16) {
                sendConnectionReply(output, REPLY_GENERAL_FAILURE);
                return null;
            }
            ip = InetAddress.getByAddress(addr);
        } else if (atyp == ADDR_TYPE_DOMAIN) {
            int len = input.read();
            if (len < 0) {
                sendConnectionReply(output, REPLY_GENERAL_FAILURE);
                return null;
            }
            byte[] db = new byte[len & 0xFF];
            if (readFully(input, db) != db.length) {
                sendConnectionReply(output, REPLY_GENERAL_FAILURE);
                return null;
            }
            host = new String(db, ISO_8859_1);
        } else {
            LogV3.info("SOCKS unsupported addrType: " + (atyp & 0xFF));
            sendConnectionReply(output, REPLY_ADDRESS_TYPE_NOT_SUPPORTED);
            return null;
        }
        byte[] portBytes = new byte[2];
        if (readFully(input, portBytes) != 2) {
            sendConnectionReply(output, REPLY_GENERAL_FAILURE);
            return null;
        }
        int targetPort = ((portBytes[0] & 0xFF) << 8) | (portBytes[1] & 0xFF);
        InetSocketAddress result;
        if (ip != null) {
            result = new InetSocketAddress(ip, targetPort);
        } else {
            // IMPORTANT: resolve now to avoid unresolved connect issues (and to get proper failure reason)
            InetAddress resolved = InetAddress.getByName(host);
            LogV3.info("SOCKS DNS " + host + " -> " + resolved.getHostAddress());
            result = new InetSocketAddress(resolved, targetPort);
        }
        LogV3.info("SOCKS REQUEST parsed: atyp=" + (atyp & 0xFF) + " -> " + formatTarget(result) + " unresolved=" + result.isUnresolved());
        return result;
    }

    private void sendConnectionReply(OutputStream output, byte replyCode) throws IOException {
        // Simplified test reply: always BND.ADDR=0.0.0.0 and BND.PORT=0
        byte[] reply = new byte[10];
        reply[0] = SOCKS_VERSION;
        reply[1] = replyCode;
        reply[2] = 0x00;
        reply[3] = ADDR_TYPE_IPV4;
        reply[4] = 0x00;
        reply[5] = 0x00;
        reply[6] = 0x00;
        reply[7] = 0x00;
        reply[8] = 0x00;
        reply[9] = 0x00;
        output.write(reply);
        output.flush();
    }

    private int readFully(InputStream input, byte[] buffer) throws IOException {
        int total = 0;
        while (total < buffer.length) {
            int r = input.read(buffer, total, buffer.length - total);
            if (r == -1) {
                return total;
            }
            total += r;
        }
        return total;
    }

    /**
     * Copies data from src->dst. Flush once at the end.
     *
     * @param srcFromClient
     *            true: client->target, false: target->client
     */
    private void forwardData(Socket src, Socket dst, boolean srcFromClient, InetSocketAddress target) throws IOException {
        InputStream in = src.getInputStream();
        OutputStream out = dst.getOutputStream();
        byte[] buf = new byte[8192];
        int total = 0;
        try {
            int n;
            while ((n = in.read(buf)) != -1) {
                out.write(buf, 0, n);
                total += n;
            }
        } finally {
            try {
                out.flush();
            } catch (IOException ignore) {
            }
            LogV3.info("SOCKS FORWARD done " + (srcFromClient ? "C->T" : "T->C") + " target=" + (target != null ? formatTarget(target) : "n/a") + " bytes=" + total);
        }
    }

    private void safeClose(Socket s, String name, InetSocketAddress target) {
        if (s == null) {
            return;
        }
        try {
            s.close();
        } catch (IOException ignore) {
        }
        LogV3.info("SOCKS CLOSED " + name + " target=" + (target != null ? formatTarget(target) : "n/a"));
    }

    private String formatTarget(InetSocketAddress target) {
        if (target == null) {
            return "null";
        }
        InetAddress addr = target.getAddress();
        String ip = (addr != null ? addr.getHostAddress() : target.getHostName());
        return ip + ":" + target.getPort();
    }
}
