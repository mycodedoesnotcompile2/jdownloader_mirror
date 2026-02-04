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
package org.appwork.utils.net.httpconnection.tests;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Regex;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;

/**
 * Simple HTTP proxy server for testing purposes.
 * Handles CONNECT requests and forwards the connection to the target server.
 * 
 * @author thomas
 * @date 01.02.2026
 */
public class TestHttpProxyServer {
    private ServerSocket serverSocket;
    private Thread serverThread;
    private final AtomicBoolean running = new AtomicBoolean(false);
    private int port;
    private String username;
    private String password;

    /**
     * Creates a new HTTP proxy server.
     * 
     * @param port Port to listen on (0 for automatic port selection)
     * @param username Optional username for proxy authentication (null if no auth)
     * @param password Optional password for proxy authentication (null if no auth)
     */
    public TestHttpProxyServer(int port, String username, String password) {
        this.port = port;
        this.username = username;
        this.password = password;
    }

    /**
     * Creates a new HTTP proxy server without authentication.
     * 
     * @param port Port to listen on (0 for automatic port selection)
     */
    public TestHttpProxyServer(int port) {
        this(port, null, null);
    }

    /**
     * Starts the proxy server.
     * 
     * @throws IOException If the server cannot be started
     */
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
        serverThread = new Thread(this::runServer, "TestHttpProxyServer-" + port);
        serverThread.setDaemon(true);
        serverThread.start();
        LogV3.info("TestHttpProxyServer started on port " + port);
    }

    /**
     * Stops the proxy server.
     */
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
        LogV3.info("TestHttpProxyServer stopped");
    }

    /**
     * Gets the port the server is listening on.
     * 
     * @return The port number
     */
    public int getPort() {
        return port;
    }

    /**
     * Main server loop.
     */
    private void runServer() {
        while (running.get()) {
            try {
                Socket clientSocket = serverSocket.accept();
                new Thread(() -> handleClient(clientSocket), "ProxyHandler-" + clientSocket.getRemoteSocketAddress()).start();
            } catch (IOException e) {
                if (running.get()) {
                    LogV3.log(e);
                }
            }
        }
    }

    /**
     * Handles a client connection.
     * 
     * @param clientSocket The client socket
     */
    private void handleClient(Socket clientSocket) {
        OutputStream output = null;
        Socket targetSocket = null;
        try {
            final InputStream input = clientSocket.getInputStream();
            output = clientSocket.getOutputStream();

            // Read CONNECT request line using readheader to avoid buffering issues
            ByteBuffer requestLineBuffer = HTTPConnectionUtils.readheader(input, true);
            if (requestLineBuffer == null || requestLineBuffer.limit() == 0) {
                sendErrorResponse(output, 400, "Bad Request");
                return;
            }
            byte[] requestLineBytes = new byte[requestLineBuffer.limit()];
            requestLineBuffer.get(requestLineBytes);
            String requestLine = new String(requestLineBytes, "ISO-8859-1").trim();
            
            if (!requestLine.startsWith("CONNECT")) {
                sendErrorResponse(output, 400, "Bad Request");
                return;
            }

            // Parse target host:port
            Regex regex = new Regex(requestLine, "CONNECT\\s+([^\\s]+)\\s+HTTP/1\\.[01]");
            String target = regex.getMatch(0);
            if (target == null) {
                sendErrorResponse(output, 400, "Bad Request");
                return;
            }

            String[] parts = target.split(":");
            if (parts.length != 2) {
                sendErrorResponse(output, 400, "Bad Request");
                return;
            }
            String targetHost = parts[0];
            int targetPort = Integer.parseInt(parts[1]);

            // Read headers until empty line
            boolean authenticated = (username == null && password == null);
            while (true) {
                ByteBuffer headerLineBuffer = HTTPConnectionUtils.readheader(input, true);
                if (headerLineBuffer == null || headerLineBuffer.limit() <= 2) {
                    // Empty line (just CRLF or LF)
                    break;
                }
                byte[] headerLineBytes = new byte[headerLineBuffer.limit()];
                headerLineBuffer.get(headerLineBytes);
                String headerLine = new String(headerLineBytes, "ISO-8859-1").trim();
                
                if (headerLine.startsWith("Proxy-Authorization:")) {
                    if (username != null && password != null) {
                        String auth = headerLine.substring("Proxy-Authorization:".length()).trim();
                        if (auth.startsWith("Basic ")) {
                            try {
                                String credentials = new String(java.util.Base64.getDecoder().decode(auth.substring(6)));
                                String[] creds = credentials.split(":", 2);
                                if (creds.length == 2 && creds[0].equals(username) && creds[1].equals(password)) {
                                    authenticated = true;
                                }
                            } catch (Exception e) {
                                // Invalid auth encoding
                            }
                        }
                    }
                }
            }

            if (!authenticated) {
                sendErrorResponse(output, 407, "Proxy Authentication Required");
                return;
            }

            // Connect to target server
            targetSocket = new Socket();
            targetSocket.connect(new InetSocketAddress(targetHost, targetPort), 10000);
            sendSuccessResponse(output);

            // Forward data bidirectionally
            final Socket finalTargetSocket = targetSocket;
            Thread forwardToTarget = new Thread(() -> {
                try {
                    forwardData(input, finalTargetSocket.getOutputStream());
                } catch (IOException e) {
                    // Connection closed
                } finally {
                    try {
                        finalTargetSocket.close();
                    } catch (IOException e) {
                    }
                }
            }, "ForwardToTarget");
            forwardToTarget.setDaemon(true);
            forwardToTarget.start();

            forwardData(targetSocket.getInputStream(), output);
        } catch (IOException e) {
            LogV3.log(e);
            try {
                if (output != null) {
                    sendErrorResponse(output, 502, "Bad Gateway");
                }
            } catch (IOException e2) {
                // Ignore
            }
        } finally {
            try {
                if (clientSocket != null && !clientSocket.isClosed()) {
                    clientSocket.close();
                }
            } catch (IOException e) {
            }
            try {
                if (targetSocket != null && !targetSocket.isClosed()) {
                    targetSocket.close();
                }
            } catch (IOException e) {
            }
        }
    }

    /**
     * Sends a success response (200 Connection established).
     * 
     * @param writer The output stream
     * @throws IOException If writing fails
     */
    private void sendSuccessResponse(OutputStream writer) throws IOException {
        writer.write("HTTP/1.1 200 Connection established\r\n".getBytes());
        writer.write("\r\n".getBytes());
        writer.flush();
    }

    /**
     * Sends an error response.
     * 
     * @param writer The output stream
     * @param code The HTTP status code
     * @param message The status message
     * @throws IOException If writing fails
     */
    private void sendErrorResponse(OutputStream writer, int code, String message) throws IOException {
        writer.write(("HTTP/1.1 " + code + " " + message + "\r\n").getBytes());
        if (code == 407) {
            writer.write("Proxy-Authenticate: Basic realm=\"Proxy\"\r\n".getBytes());
        }
        writer.write("\r\n".getBytes());
        writer.flush();
    }

    /**
     * Forwards data from input to output stream.
     * 
     * @param input The input stream
     * @param output The output stream
     */
    private void forwardData(java.io.InputStream input, OutputStream output) {
        byte[] buffer = new byte[8192];
        try {
            int bytesRead;
            while ((bytesRead = input.read(buffer)) != -1) {
                output.write(buffer, 0, bytesRead);
                output.flush();
            }
        } catch (IOException e) {
            // Connection closed
        } finally {
            try {
                input.close();
            } catch (IOException e) {
            }
        }
    }

    /**
     * Checks if the server is running.
     * 
     * @return true if running, false otherwise
     */
    public boolean isRunning() {
        return running.get();
    }
}
