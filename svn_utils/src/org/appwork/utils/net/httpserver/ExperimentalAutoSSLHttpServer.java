package org.appwork.utils.net.httpserver;

import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.concurrent.atomic.AtomicReference;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLException;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.requests.HttpRequest;

public class ExperimentalAutoSSLHttpServer extends HttpServer {
    private final SSLContext      sslContext;
    private final X509Certificate clientAuthority;
    private boolean               autoUpgrade = true; // Default: enable auto-upgrade (HTTP/HTTPS on same port)

    /**
     * Creates a new JSSE-based SSL HTTP server.
     *
     * @param port
     *            The port to listen on
     * @param sslContext
     *            The SSL context (must be initialized with KeyManagers)
     */
    public ExperimentalAutoSSLHttpServer(final int port, final SSLContext sslContext) {
        this(port, sslContext, null);
    }

    /**
     * Creates a new JSSE-based SSL HTTP server with client certificate authority.
     *
     * @param port
     *            The port to listen on
     * @param sslContext
     *            The SSL context (must be initialized with KeyManagers)
     * @param clientAuthority
     *            Optional client certificate authority for client certificate authentication
     */
    public ExperimentalAutoSSLHttpServer(final int port, final SSLContext sslContext, final X509Certificate clientAuthority) {
        super(port);
        this.sslContext = sslContext;
        this.clientAuthority = clientAuthority;

    }

    /**
     * Sets whether to enable auto-upgrade (HTTP/HTTPS detection on the same port).
     *
     * <p>
     * If auto-upgrade is enabled (default), the server will detect whether incoming connections are HTTP or HTTPS and handle them
     * accordingly on the same port.
     * </p>
     *
     * <p>
     * If auto-upgrade is disabled, the server will only accept HTTPS connections. HTTP connections will be rejected.
     * </p>
     *
     * @param autoUpgrade
     *            true to enable auto-upgrade (HTTP/HTTPS on same port), false for HTTPS-only
     */
    public void setAutoUpgrade(final boolean autoUpgrade) {
        this.autoUpgrade = autoUpgrade;
    }

    /**
     * Returns whether auto-upgrade is enabled.
     *
     * @return true if auto-upgrade is enabled, false if HTTPS-only
     */
    public boolean isAutoUpgrade() {
        return this.autoUpgrade;
    }

    @Override
    protected HttpConnectionRunnable createHttpConnection(final Socket clientSocket) throws IOException {
        if (clientSocket == null) {
            throw new IOException("ClientSocket is null");
        }

        return new HttpConnectionRunnable() {

            @Override
            public Socket getClientSocket() {
                return clientSocket;
            }

            @Override
            public void run() {
                try {
                    final HttpConnectionRunnable run;
                    if (ExperimentalAutoSSLHttpServer.this.sslContext != null) {
                        if (ExperimentalAutoSSLHttpServer.this.autoUpgrade) {
                            // Auto-upgrade enabled: detect HTTP/HTTPS on same port
                            run = ExperimentalAutoSSLHttpServer.this.autoWrapSSLConnection(ExperimentalAutoSSLHttpServer.this, clientSocket);
                        } else {
                            // Auto-upgrade disabled: HTTPS-only, direct SSL connection
                            run = ExperimentalAutoSSLHttpServer.this.directSSLConnection(ExperimentalAutoSSLHttpServer.this, clientSocket);
                        }
                    } else {
                        // Fall back to plain HTTP handling when no SSL context is configured
                        run = ExperimentalAutoSSLHttpServer.super.createHttpConnection(clientSocket);
                    }

                    run.run();

                } catch (final Throwable e) {
                    // Log the error appropriately
                    if (e instanceof InterruptedException) {
                        Thread.currentThread().interrupt();
                        // InterruptedException is usually not an error, just log at fine level
                        LogV3.fine("Connection handling interrupted: " + e.getMessage());
                    } else if (e instanceof IOException) {
                        // IOExceptions during connection setup are common (e.g., client disconnect, SSL errors)
                        // Log at fine level unless it's a significant error
                        final String message = e.getMessage();
                        if (message != null && (message.contains("plaintext") || message.contains("Connection reset") || message.contains("Broken pipe"))) {
                            // Common connection issues - log at fine level
                            LogV3.fine("Connection error: " + message);
                        } else {
                            // Other IOExceptions - log at warning level
                            LogV3.warning("Connection error: " + message);
                            LogV3.log(e);
                        }
                    } else {
                        // Unexpected errors - log at error level
                        LogV3.log(e);
                    }

                    // Ensure socket is closed
                    try {
                        if (clientSocket != null && !clientSocket.isClosed()) {
                            clientSocket.close();
                        }
                    } catch (final IOException closeException) {
                        // Ignore errors during close
                        LogV3.fine("Error closing socket after connection error: " + closeException.getMessage());
                    }
                }
            }
        };
    }

    public static class AutoWrappedSSLHttpConnection extends HttpServerConnection {
        private final X509Certificate clientCertificate;
        private final boolean         ssl;

        public AutoWrappedSSLHttpConnection(final AbstractServerBasics server, final boolean ssl, final Socket clientSocket, final InputStream is, final OutputStream os, final X509Certificate clientCertificate) throws IOException {
            super(server, clientSocket, is, os);
            this.clientCertificate = clientCertificate;
            this.ssl = ssl;
        }

        @Override
        protected HttpRequest buildRequest() throws IOException {
            final HttpRequest ret = super.buildRequest();
            ret.setHttps(this.ssl);
            return ret;
        }

        public X509Certificate getClientCertificate() {
            return this.clientCertificate;
        }
    }

    /**
     * Creates a direct SSL connection without auto-detection (HTTPS-only mode).
     *
     * <p>
     * This method is used when auto-upgrade is disabled. It assumes all incoming connections are HTTPS and performs SSL handshake directly
     * using SSLEngine.
     * </p>
     *
     * @param server
     *            The server instance
     * @param clientSocket
     *            The client socket (plain TCP)
     * @return HttpServerConnection for the SSL-wrapped connection
     * @throws IOException
     *             if SSL handshake fails
     */
    private HttpConnectionRunnable directSSLConnection(final AbstractServerBasics server, final Socket clientSocket) throws IOException {
        boolean finallyCloseSocket = true;
        try {
            clientSocket.setSoTimeout(60 * 1000);

            if (this.sslContext == null) {
                throw new IOException("Missing SSL context! https not available!");
            }

            // Create SSLEngine and perform handshake directly (no protocol detection)
            final SSLEngine engine = this.sslContext.createSSLEngine();
            engine.setUseClientMode(false);
            engine.setNeedClientAuth(this.clientAuthority != null);

            // Perform TLS handshake using SSLEngine
            final SSLEngineStreams streams = performHandshake(engine, clientSocket.getInputStream(), clientSocket.getOutputStream());
            final InputStream httpIS = streams.getInputStream();
            final OutputStream httpOS = streams.getOutputStream();

            // Get client certificate if available
            final AtomicReference<X509Certificate> clientCertificate = new AtomicReference<X509Certificate>();
            try {
                if (engine.getSession().getPeerCertificates() != null && engine.getSession().getPeerCertificates().length > 0) {
                    clientCertificate.set((X509Certificate) engine.getSession().getPeerCertificates()[0]);
                }
            } catch (final javax.net.ssl.SSLPeerUnverifiedException e) {
                // Client did not provide a certificate - this is normal if needClientAuth is false
                // Just continue without client certificate
            }

            finallyCloseSocket = false;
            return this.createSSLHttpConnection(server, true, clientSocket, httpIS, httpOS, clientCertificate.get());
        } finally {
            try {
                if (finallyCloseSocket) {
                    clientSocket.close();
                }
            } catch (final IOException ignore) {
            }
        }
    }

    /**
     * Creates an SSL-wrapped HTTP connection.
     *
     * <p>
     * This method can be overridden by subclasses to provide custom SSL connection wrappers. The default implementation creates a basic
     * SSLConnectionWrapper.
     * </p>
     *
     * @param server
     *            The HTTP server instance
     * @param ssl
     *            true if this is an SSL/TLS connection
     * @param clientSocket
     *            The client socket
     * @param httpIS
     *            The wrapped input stream (TLS-decrypted)
     * @param httpOS
     *            The wrapped output stream (TLS-encrypted)
     * @param clientCertificate
     *            Optional client certificate (can be null)
     * @return HttpConnectionRunnable for the SSL-wrapped connection
     * @throws IOException
     *             if the connection cannot be created
     */
    protected HttpConnectionRunnable createSSLHttpConnection(final AbstractServerBasics server, final boolean ssl, final Socket clientSocket, final InputStream httpIS, final OutputStream httpOS, final Object clientCertificate) throws IOException {
        // Default implementation: create a basic SSLConnectionWrapper
        // Subclasses (e.g., in CoreAPIServer) can override this to provide their own implementation
        return new SSLConnectionWrapper(server, ssl, clientSocket, httpIS, httpOS, clientCertificate);
    }

    private HttpServerConnection autoWrapSSLConnection(final AbstractServerBasics server, final Socket clientSocket) throws IOException {
        boolean finallyCloseSocket = true;
        try {
            clientSocket.setSoTimeout(60 * 1000);
            final InputStream is = clientSocket.getInputStream();
            final byte[] guessProtocolBuffer = new byte[8];
            int index = 0;
            for (index = 0; index < 8; index++) {
                final int read = is.read();
                if (read == -1) {
                    if (index == 0) {
                        return null;
                    } else {
                        throw new EOFException("guess protocol failed: " + index);
                    }
                }
                guessProtocolBuffer[index] = (byte) read;
            }
            final RequestMethod requestMethod = RequestMethod.get(guessProtocolBuffer);
            final PushbackInputStream clientSocketIS = new PushbackInputStream(is, 8);
            clientSocketIS.unread(guessProtocolBuffer, 0, index);
            final InputStream httpIS;
            final OutputStream httpOS;
            final AtomicReference<X509Certificate> clientCertificate = new AtomicReference<X509Certificate>();
            boolean ssl = false;
            if (!RequestMethod.UNKNOWN.equals(requestMethod)) {
                // http
                ssl = false;
                httpIS = clientSocketIS;
                httpOS = clientSocket.getOutputStream();
            } else {
                // https
                ssl = true;
                if (this.sslContext == null) {
                    throw new IOException("Missing SSL context! https not available!");
                }

                // Create SSLEngine and perform handshake
                final SSLEngine engine = this.sslContext.createSSLEngine();
                engine.setUseClientMode(false);
                engine.setNeedClientAuth(this.clientAuthority != null);

                // Perform TLS handshake using SSLEngine
                final SSLEngineStreams streams = performHandshake(engine, clientSocketIS, clientSocket.getOutputStream());
                httpIS = streams.getInputStream();
                httpOS = streams.getOutputStream();

                // Get client certificate if available
                try {
                    if (engine.getSession().getPeerCertificates() != null && engine.getSession().getPeerCertificates().length > 0) {
                        clientCertificate.set((X509Certificate) engine.getSession().getPeerCertificates()[0]);
                    }
                } catch (final javax.net.ssl.SSLPeerUnverifiedException e) {
                    // Client did not provide a certificate - this is normal if needClientAuth is false
                    // Just continue without client certificate
                }
            }
            finallyCloseSocket = false;
            return this.createSSLHttpConnection(server, ssl, clientSocket, httpIS, httpOS, clientCertificate);
        } finally {
            try {
                if (finallyCloseSocket) {
                    clientSocket.close();
                }
            } catch (final IOException ignore) {
            }
        }
    }

    /**
     * Performs TLS handshake using SSLEngine.
     *
     * @param engine
     *            The SSLEngine instance
     * @param inputStream
     *            The input stream (with pushed-back bytes)
     * @param outputStream
     *            The output stream
     * @return Wrapped input/output streams
     * @throws IOException
     *             if handshake fails
     */
    private SSLEngineStreams performHandshake(final SSLEngine engine, final InputStream inputStream, final OutputStream outputStream) throws IOException {
        final int bufferSize = engine.getSession().getPacketBufferSize();
        final ByteBuffer inNetBuffer = ByteBuffer.allocate(bufferSize * 2);
        ByteBuffer outNetBuffer = ByteBuffer.allocate(bufferSize * 2);
        ByteBuffer inAppBuffer = ByteBuffer.allocate(bufferSize);

        HandshakeStatus status = engine.getHandshakeStatus();
        int loopCount = 0;
        final int maxLoops = 100; // Prevent infinite loops

        // If status is NOT_HANDSHAKING, we need to start the handshake by reading from the client
        // In server mode, the handshake starts when we receive ClientHello
        if (status == HandshakeStatus.NOT_HANDSHAKING) {
            // Read initial data from client to trigger handshake
            inNetBuffer.clear();
            final byte[] initialData = new byte[bufferSize];
            final int bytesRead = inputStream.read(initialData);
            if (bytesRead == -1) {
                throw new EOFException("Unexpected EOF before handshake started");
            }
            if (bytesRead > 0) {
                // Check if this looks like plaintext HTTP (starts with HTTP method like GET, POST, etc.)
                // This helps detect HTTP clients connecting to HTTPS-only port
                if (bytesRead >= 3) {
                    final String firstBytes = new String(initialData, 0, Math.min(bytesRead, 20), java.nio.charset.StandardCharsets.US_ASCII);
                    if (firstBytes.startsWith("GET ") || firstBytes.startsWith("POST ") || firstBytes.startsWith("PUT ") || firstBytes.startsWith("DELETE ") || firstBytes.startsWith("HEAD ") || firstBytes.startsWith("OPTIONS ") || firstBytes.startsWith("PATCH ")) {
                        throw new IOException("Received plaintext HTTP connection on HTTPS-only port. Auto-upgrade is disabled - only HTTPS connections are accepted.");
                    }
                }

                inNetBuffer.put(initialData, 0, bytesRead);
                inNetBuffer.flip();
                // Now unwrap to start the handshake
                try {
                    final SSLEngineResult unwrapResult = engine.unwrap(inNetBuffer, inAppBuffer);
                    status = unwrapResult.getHandshakeStatus();
                } catch (final javax.net.ssl.SSLException e) {
                    // If unwrap fails, it might be because the client sent plaintext HTTP
                    if (e.getMessage() != null && e.getMessage().contains("plaintext")) {
                        throw new IOException("Received plaintext HTTP connection on HTTPS-only port. Auto-upgrade is disabled - only HTTPS connections are accepted.", e);
                    }
                    throw e;
                }
            }
        }

        while (status != HandshakeStatus.FINISHED && status != HandshakeStatus.NOT_HANDSHAKING) {
            if (++loopCount > maxLoops) {
                throw new IOException("Handshake loop exceeded maximum iterations");
            }

            switch (status) {
            case NEED_UNWRAP:
                // Read data from network if buffer needs more data
                if (inNetBuffer.remaining() == 0) {
                    // Compact buffer if it has been partially consumed
                    if (inNetBuffer.position() > 0) {
                        inNetBuffer.compact();
                    } else {
                        inNetBuffer.clear();
                    }

                    // Read available data (blocking read if needed)
                    final byte[] data = new byte[bufferSize];
                    final int bytesRead = inputStream.read(data);

                    if (bytesRead == -1) {
                        throw new EOFException("Unexpected EOF during handshake");
                    }
                    if (bytesRead > 0) {
                        inNetBuffer.put(data, 0, bytesRead);
                        inNetBuffer.flip();
                    } else {
                        // No data available yet, wait a bit
                        try {
                            Thread.sleep(10);
                        } catch (final InterruptedException e) {
                            Thread.currentThread().interrupt();
                            throw new IOException("Handshake interrupted", e);
                        }
                        continue;
                    }
                }

                // Unwrap incoming data
                SSLEngineResult unwrapResult = engine.unwrap(inNetBuffer, inAppBuffer);
                status = unwrapResult.getHandshakeStatus();

                // Handle buffer overflow by resizing and retrying
                while (unwrapResult.getStatus() == SSLEngineResult.Status.BUFFER_OVERFLOW) {
                    inAppBuffer = ByteBuffer.allocate(inAppBuffer.capacity() * 2);
                    unwrapResult = engine.unwrap(inNetBuffer, inAppBuffer);
                    status = unwrapResult.getHandshakeStatus();
                }

                if (unwrapResult.getStatus() == SSLEngineResult.Status.BUFFER_UNDERFLOW) {
                    // Need more data - compact buffer and continue
                    inNetBuffer.compact();
                }

                // Process any tasks
                if (status == HandshakeStatus.NEED_TASK) {
                    Runnable task;
                    while ((task = engine.getDelegatedTask()) != null) {
                        task.run();
                    }
                    status = engine.getHandshakeStatus();
                }
                break;

            case NEED_WRAP:
                // Wrap outgoing data (empty buffer for handshake)
                outNetBuffer.clear();
                final ByteBuffer emptyAppBuffer = ByteBuffer.allocate(0);
                SSLEngineResult wrapResult = engine.wrap(emptyAppBuffer, outNetBuffer);
                status = wrapResult.getHandshakeStatus();

                // Handle buffer overflow by resizing and retrying
                while (wrapResult.getStatus() == SSLEngineResult.Status.BUFFER_OVERFLOW) {
                    final ByteBuffer newOutNetBuffer = ByteBuffer.allocate(outNetBuffer.capacity() * 2);
                    outNetBuffer.flip();
                    newOutNetBuffer.put(outNetBuffer);
                    outNetBuffer = newOutNetBuffer;
                    wrapResult = engine.wrap(emptyAppBuffer, outNetBuffer);
                    status = wrapResult.getHandshakeStatus();
                }

                // Write wrapped data if any was produced (regardless of wrap result status)
                if (outNetBuffer.position() > 0) {
                    outNetBuffer.flip();
                    final byte[] data = new byte[outNetBuffer.remaining()];
                    outNetBuffer.get(data);
                    outputStream.write(data);
                    outputStream.flush();
                }

                // Process any tasks
                if (status == HandshakeStatus.NEED_TASK) {
                    Runnable task;
                    while ((task = engine.getDelegatedTask()) != null) {
                        task.run();
                    }
                    status = engine.getHandshakeStatus();
                }
                break;

            case NEED_TASK:
                Runnable task;
                while ((task = engine.getDelegatedTask()) != null) {
                    task.run();
                }
                status = engine.getHandshakeStatus();
                break;

            default:
                break;
            }
        }
        // Normalize buffers for application I/O
        if (inNetBuffer.limit() == inNetBuffer.capacity()) {
            if (inNetBuffer.position() > 0) {
                // Buffer is in write mode with data in [0, position)
                inNetBuffer.flip();
            } else {
                // No pending data
                inNetBuffer.clear();
            }
        }
        if (inAppBuffer.position() > 0) {
            // Handshake should not leave application data to consume
            inAppBuffer.clear();
        }
        // Create wrapped streams for application data using existing buffers
        return new SSLEngineStreams(engine, inputStream, outputStream, bufferSize, inNetBuffer, inAppBuffer);
    }

    /**
     * Wrapper class for SSLEngine input/output streams.
     */
    private static class SSLEngineStreams {
        private final SSLEngine    engine;
        private final InputStream  inputStream;
        private final OutputStream outputStream;
        private final int          bufferSize;
        private final ByteBuffer   inNetBuffer;
        private ByteBuffer         inAppBuffer;
        private final ByteBuffer   outAppBuffer;

        public SSLEngineStreams(final SSLEngine engine, final InputStream inputStream, final OutputStream outputStream, final int bufferSize) {
            this(engine, inputStream, outputStream, bufferSize, null, null);
        }

        public SSLEngineStreams(final SSLEngine engine, final InputStream inputStream, final OutputStream outputStream, final int bufferSize, final ByteBuffer inNetBuffer, final ByteBuffer inAppBuffer) {
            this.engine = engine;
            this.inputStream = inputStream;
            this.outputStream = outputStream;
            this.bufferSize = bufferSize;
            this.inNetBuffer = inNetBuffer != null ? inNetBuffer : ByteBuffer.allocate(bufferSize);
            this.inAppBuffer = inAppBuffer != null ? inAppBuffer : ByteBuffer.allocate(bufferSize);
            if (this.inAppBuffer.position() > 0) {
                this.inAppBuffer.clear();
            }
            this.outAppBuffer = ByteBuffer.allocate(bufferSize);
        }

        public InputStream getInputStream() {
            return new SSLEngineInputStream();
        }

        public OutputStream getOutputStream() {
            return new SSLEngineOutputStream();
        }

        private class SSLEngineInputStream extends FilterInputStream {
            public SSLEngineInputStream() {
                super(inputStream);
            }

            @Override
            public int read() throws IOException {
                final byte[] b = new byte[1];
                final int n = read(b, 0, 1);
                return n == -1 ? -1 : b[0] & 0xFF;
            }

            @Override
            public int read(final byte[] b, final int off, final int len) throws IOException {
                // If we have decrypted data available, return it
                if (inAppBuffer.position() > 0) {
                    inAppBuffer.flip();
                    final int toRead = Math.min(len, inAppBuffer.remaining());
                    inAppBuffer.get(b, off, toRead);
                    inAppBuffer.compact();
                    return toRead;
                }

                // Unwrap loop: keep trying until we get data or need more input
                while (true) {
                    // Read encrypted data from network if buffer needs more data
                    if (inNetBuffer.remaining() == 0) {
                        // Compact if partially consumed
                        if (inNetBuffer.position() > 0) {
                            inNetBuffer.compact();
                        } else {
                            inNetBuffer.clear();
                        }

                        // Read available data (blocking read)
                        final byte[] data = new byte[bufferSize];
                        final int bytesRead = inputStream.read(data);
                        if (bytesRead == -1) {
                            return -1;
                        }
                        if (bytesRead > 0) {
                            // After handshake, check if incoming data looks like plaintext HTTP
                            // This can happen if client sends plain HTTP after SSL handshake
                            if (bytesRead >= 3) {
                                final String firstBytes = new String(data, 0, Math.min(bytesRead, 20), java.nio.charset.StandardCharsets.US_ASCII);
                                if (firstBytes.startsWith("GET ") || firstBytes.startsWith("POST ") || firstBytes.startsWith("PUT ") || firstBytes.startsWith("DELETE ") || firstBytes.startsWith("HEAD ") || firstBytes.startsWith("OPTIONS ") || firstBytes.startsWith("PATCH ")) {
                                    throw new IOException("Received plaintext HTTP data after SSL handshake. Client may have fallen back to HTTP after SSL error. Auto-upgrade is disabled - only HTTPS connections are accepted.");
                                }
                            }

                            inNetBuffer.put(data, 0, bytesRead);
                            inNetBuffer.flip();
                        } else {
                            return 0; // No data available yet
                        }
                    }

                    // Unwrap encrypted data
                    SSLEngineResult result;
                    try {
                        result = engine.unwrap(inNetBuffer, inAppBuffer);
                    } catch (final javax.net.ssl.SSLException e) {
                        // If we get an SSLException during unwrap, it might be because the client
                        // sent plaintext HTTP data instead of encrypted HTTPS data
                        // This can happen if auto-upgrade is disabled and an HTTP client connects
                        if (e.getMessage() != null && e.getMessage().contains("plaintext")) {
                            throw new IOException("Received plaintext HTTP connection on HTTPS-only port. Auto-upgrade is disabled - only HTTPS connections are accepted.", e);
                        }
                        throw e;
                    }

                    // Handle buffer overflow by resizing
                    while (result.getStatus() == SSLEngineResult.Status.BUFFER_OVERFLOW) {
                        final ByteBuffer newInAppBuffer = ByteBuffer.allocate(inAppBuffer.capacity() * 2);
                        inAppBuffer.flip();
                        newInAppBuffer.put(inAppBuffer);
                        inAppBuffer = newInAppBuffer;
                        try {
                            result = engine.unwrap(inNetBuffer, inAppBuffer);
                        } catch (final javax.net.ssl.SSLException e) {
                            if (e.getMessage() != null && e.getMessage().contains("plaintext")) {
                                throw new IOException("Received plaintext HTTP connection on HTTPS-only port. Auto-upgrade is disabled - only HTTPS connections are accepted.", e);
                            }
                            throw e;
                        }
                    }

                    if (result.getStatus() == SSLEngineResult.Status.BUFFER_UNDERFLOW) {
                        // Need more data - compact buffer and read more
                        inNetBuffer.compact();
                        // Continue loop to read more data
                        continue;
                    } else if (result.getStatus() == SSLEngineResult.Status.CLOSED) {
                        return -1;
                    }

                    // We got some data - return it
                    if (inAppBuffer.position() > 0) {
                        inAppBuffer.flip();
                        final int toRead = Math.min(len, inAppBuffer.remaining());
                        inAppBuffer.get(b, off, toRead);
                        inAppBuffer.compact();
                        return toRead > 0 ? toRead : 0;
                    }

                    // No data produced but status is OK - might need more input
                    // This shouldn't happen, but if it does, try reading more
                    if (inNetBuffer.remaining() == 0) {
                        inNetBuffer.clear();
                    }
                }
            }

            @Override
            public int available() throws IOException {
                return inAppBuffer.position() + (inNetBuffer.remaining() > 0 ? 1 : 0);
            }
        }

        private class SSLEngineOutputStream extends OutputStream {
            @Override
            public void write(final int b) throws IOException {
                write(new byte[] { (byte) b }, 0, 1);
            }

            @Override
            public void write(final byte[] b, final int off, final int len) throws IOException {
                int remaining = len;
                int offset = off;

                while (remaining > 0) {
                    final int toWrite = Math.min(remaining, outAppBuffer.remaining());
                    outAppBuffer.put(b, offset, toWrite);
                    outAppBuffer.flip();

                    // Wrap and send
                    final ByteBuffer outNetBuffer = ByteBuffer.allocate(bufferSize);
                    final SSLEngineResult result = engine.wrap(outAppBuffer, outNetBuffer);

                    if (result.getStatus() == SSLEngineResult.Status.OK) {
                        outNetBuffer.flip();
                        final byte[] data = new byte[outNetBuffer.remaining()];
                        outNetBuffer.get(data);
                        outputStream.write(data);
                        outputStream.flush();
                    }

                    outAppBuffer.clear();
                    remaining -= toWrite;
                    offset += toWrite;
                }
            }

            @Override
            public void flush() throws IOException {
                outputStream.flush();
            }

            @Override
            public void close() throws IOException {
                // Send close_notify
                try {
                    engine.closeOutbound();
                    final ByteBuffer outNetBuffer = ByteBuffer.allocate(bufferSize);
                    final SSLEngineResult result = engine.wrap(ByteBuffer.allocate(0), outNetBuffer);
                    if (result.getStatus() == SSLEngineResult.Status.OK) {
                        outNetBuffer.flip();
                        final byte[] data = new byte[outNetBuffer.remaining()];
                        outNetBuffer.get(data);
                        outputStream.write(data);
                        outputStream.flush();
                    }
                } catch (final SSLException e) {
                    // Ignore
                }
                outputStream.close();
            }
        }
    }

    protected AutoWrappedSSLHttpConnection createSSLHttpConnection(final AbstractServerBasics server, final boolean ssl, final Socket clientSocket, final InputStream httpIS, final OutputStream httpOS, final AtomicReference<X509Certificate> clientCertificate) throws IOException {
        return new AutoWrappedSSLHttpConnection(server, ssl, clientSocket, httpIS, httpOS, clientCertificate.get());
    }

    /**
     * Helper method to create SSLContext from PKCS12 keystore.
     *
     * @param keystorePath
     *            Path to PKCS12 keystore file
     * @param keystorePassword
     *            Password for the keystore
     * @return Initialized SSLContext
     * @throws KeyStoreException
     * @throws NoSuchAlgorithmException
     * @throws CertificateException
     * @throws IOException
     * @throws UnrecoverableKeyException
     * @throws KeyManagementException
     */
    public static SSLContext createSSLContextFromPKCS12(final String keystorePath, final String keystorePassword) throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException, UnrecoverableKeyException, KeyManagementException {
        final KeyStore ks = KeyStore.getInstance("PKCS12");
        ks.load(new java.io.FileInputStream(keystorePath), keystorePassword.toCharArray());
        final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        kmf.init(ks, keystorePassword.toCharArray());

        final SSLContext sc = SSLContext.getInstance("TLS");
        sc.init(kmf.getKeyManagers(), null, null);
        return sc;
    }
}
