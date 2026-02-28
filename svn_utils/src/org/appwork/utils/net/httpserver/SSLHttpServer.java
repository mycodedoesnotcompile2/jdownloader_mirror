package org.appwork.utils.net.httpserver;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.TrustCallback;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;

/**
 * Simple HTTPS server based on {@link HttpServer}.
 *
 * <p>
 * No protocol auto-detection, no fallback: every connection is treated as TLS.
 * </p>
 * <p>
 * Client certificate authentication is controlled by {@link ClientAuthMode}:
 * </p>
 * <ul>
 * <li>{@link ClientAuthMode#NONE}: No client cert requested.</li>
 * <li>{@link ClientAuthMode#WANT}: Server requests a client cert but connection succeeds without one; if present, cert is verified.</li>
 * <li>{@link ClientAuthMode#NEED}: Client cert is required; connection fails if none or untrusted.</li>
 * </ul>
 * <p>
 * Client certificate validation is done during the TLS handshake by the {@link SSLContext}'s TrustManagers (
 * {@link javax.net.ssl.X509TrustManager#checkClientTrusted}). To fully control validation, use a custom TrustManager or the
 * TrustProvider-based constructor below.
 * </p>
 * <p>
 * <b>Using the TrustProvider system (full override):</b> Use the constructor that takes {@link TrustProviderInterface} to delegate client
 * certificate validation entirely to the same abstraction used by the HttpClient stack. The provider's
 * {@link TrustProviderInterface#getAcceptedIssuers()} and {@link TrustProviderInterface#checkClientTrusted} are used during the TLS
 * handshake (via an internal {@link javax.net.ssl.X509TrustManager} bridge). Example: {@code new CustomTrustProvider(caCert)} or your own
 * implementation (e.g. validate against a keystore, CRL, or custom rules).
 * </p>
 */
public class SSLHttpServer extends HttpServer {
    /**
     * Client certificate authentication mode.
     */
    public enum ClientAuthMode {
        /** No client certificate requested. */
        NONE,
        /** Request client cert but optional (connection succeeds without one). */
        WANT,
        /** Client certificate required; connection fails if none or untrusted. */
        NEED
    }

    private final SSLContext               sslContext;
    private final ClientAuthMode           clientAuthMode;
    /** Set when using TrustProvider constructor; used to capture TrustResult from handshake callback. */
    private final ThreadLocal<TrustResult> clientCertTrustResultHolder;

    public SSLHttpServer(final int port, final SSLContext sslContext) {
        this(port, sslContext, ClientAuthMode.NONE);
    }

    /**
     * @param clientAuthMode
     *            NONE, WANT, or NEED
     */
    public SSLHttpServer(final int port, final SSLContext sslContext, final ClientAuthMode clientAuthMode) {
        super(port);
        if (sslContext == null) {
            throw new IllegalArgumentException("sslContext is null");
        }
        if (clientAuthMode == null) {
            throw new IllegalArgumentException("clientAuthMode is null");
        }
        this.sslContext = sslContext;
        this.clientAuthMode = clientAuthMode;
        this.clientCertTrustResultHolder = null;
    }

    /**
     * Creates an SSL HTTP server with client certificate validation fully delegated to a {@link TrustProviderInterface}. Same abstraction
     * as the HttpClient stack: the provider's {@link TrustProviderInterface#getAcceptedIssuers()} and
     * {@link TrustProviderInterface#checkClientTrusted} are used during the TLS handshake (no separate TrustManager needed). Use e.g.
     * {@link org.appwork.utils.net.httpconnection.trust.CustomTrustProvider} with your CA cert(s), or your own implementation (keystore,
     * CRL, custom rules).
     *
     * @param port
     *            server port (0 = choose automatically)
     * @param serverKeystore
     *            server identity (certificate + private key). Must not be null.
     * @param serverKeystorePassword
     *            password for serverKeystore. Must not be null.
     * @param clientCertTrustProvider
     *            provider for validating client certificates during the handshake. Must not be null.
     * @param clientAuthMode
     *            NONE, WANT, or NEED
     * @throws KeyStoreException
     * @throws NoSuchAlgorithmException
     * @throws UnrecoverableKeyException
     * @throws KeyManagementException
     */
    public SSLHttpServer(final int port, final KeyStore serverKeystore, final char[] serverKeystorePassword, final TrustProviderInterface clientCertTrustProvider, final ClientAuthMode clientAuthMode) throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
        super(port);
        if (serverKeystore == null) {
            throw new IllegalArgumentException("serverKeystore is null");
        } else if (clientCertTrustProvider == null) {
            throw new IllegalArgumentException("clientCertTrustProvider is null");
        } else if (clientAuthMode == null) {
            throw new IllegalArgumentException("clientAuthMode is null");
        }
        this.clientCertTrustResultHolder = new ThreadLocal<TrustResult>();
        this.sslContext = createSSLContext(serverKeystore, serverKeystorePassword, clientCertTrustProvider, this.clientCertTrustResultHolder);
        this.clientAuthMode = clientAuthMode;
    }

    /**
     * Builds an SSLContext with server identity from the keystore and client-cert validation from the TrustProvider. Used by the
     * constructor that takes TrustProviderInterface. When a client cert is validated, the callback stores the TrustResult in holder so
     * createHttpConnection can pass it to the request.
     */
    private static SSLContext createSSLContext(final KeyStore serverKeystore, final char[] serverKeystorePassword, final TrustProviderInterface clientCertTrustProvider, final ThreadLocal<TrustResult> holder) throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
        final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        kmf.init(serverKeystore, serverKeystorePassword);
        final TrustCallback callback = new TrustCallback() {
            public void onTrustResult(final TrustProviderInterface provider, final String authType, final TrustResult result) {
                if (holder != null && result != null) {
                    holder.set(result);
                }
            }

            public TrustProviderInterface getTrustProvider() {
                return clientCertTrustProvider;
            }

            @Override
            public KeyManager[] getKeyManager() {
                return null;
            }
        };
        final TrustManager trustManager = JavaSSLSocketStreamFactory.generateTrustManagerDelegate(callback);
        final SSLContext sc = SSLContext.getInstance("TLS");
        sc.init(kmf.getKeyManagers(), new TrustManager[] { trustManager }, null);
        return sc;
    }

    /**
     * ALPN protocol "http/1.1" (used only on Java 9+ via reflection to avoid compile dependency on {@code javax.net.ssl.SSLParameters}).
     */
    private static final String[] ALPN_HTTP_1_1 = new String[] { "http/1.1" };

    /**
     * Sets ALPN to "http/1.1" on the given SSLServerSocket when running on Java 9+. No-op on JRE 1.6–8. Uses reflection only so that
     * {@code javax.net.ssl.SSLParameters} (Java 7+) and {@code setApplicationProtocols} (Java 9+) are never loaded on JRE 1.6, keeping the
     * module runnable with target runtime 1.6.
     */
    private static void setAlpnHttp11IfSupported(final SSLServerSocket sslServerSocket) {
        if (!JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            return;
        }
        try {
            final Object params = sslServerSocket.getClass().getMethod("getSSLParameters").invoke(sslServerSocket);
            params.getClass().getMethod("setApplicationProtocols", String[].class).invoke(params, (Object) ALPN_HTTP_1_1);
            sslServerSocket.getClass().getMethod("setSSLParameters", params.getClass()).invoke(sslServerSocket, params);
        } catch (final Throwable e) {
            LogV3.finest("ALPN not set: " + e.getMessage());
        }
    }

    @Override
    protected ServerSocket createServerSocket() throws IOException {
        final SSLServerSocket sslServerSocket = (SSLServerSocket) this.sslContext.getServerSocketFactory().createServerSocket();
        sslServerSocket.setReuseAddress(true);
        setAlpnHttp11IfSupported(sslServerSocket);
        switch (clientAuthMode) {
        case NEED:
            sslServerSocket.setNeedClientAuth(true);
            break;
        case WANT:
            sslServerSocket.setWantClientAuth(true);
            break;
        default:
            sslServerSocket.setWantClientAuth(false);
            break;
        }
        return sslServerSocket;
    }

    @Override
    protected HttpConnectionRunnable createHttpConnection(final Socket clientSocket) throws IOException {
        if (clientSocket == null) {
            throw new IOException("ClientSocket is null");
        }
        final SSLSocket sslSocket = (SSLSocket) clientSocket;
        try {
            // Triggers SSL handshake; client may abort (e.g. Chrome cancelling speculative connections)
            final javax.net.ssl.SSLSession serverSession = sslSocket.getSession();
            X509Certificate[] clientCertChain = null;
            try {
                final Certificate[] peerCerts = serverSession.getPeerCertificates();
                if (peerCerts != null && peerCerts.length > 0) {
                    clientCertChain = new X509Certificate[peerCerts.length];
                    for (int i = 0; i < peerCerts.length; i++) {
                        clientCertChain[i] = (X509Certificate) peerCerts[i];
                    }
                }
            } catch (final SSLPeerUnverifiedException e) {
                // No client certificate presented (NONE/WANT without cert)
            }
            final ThreadLocal<TrustResult> clientCertTrustResultHolder = this.clientCertTrustResultHolder;
            TrustResult trustResult = null;
            if (clientCertTrustResultHolder != null) {
                trustResult = clientCertTrustResultHolder.get();
                clientCertTrustResultHolder.remove();
            }
            if (trustResult == null && clientCertChain != null && clientCertChain.length > 0) {
                trustResult = new TrustResult(null, clientCertChain, null, TrustResult.TrustType.CLIENT);
            }
            return new HttpServerConnection(this, sslSocket, sslSocket.getInputStream(), sslSocket.getOutputStream(), true, trustResult);
        } catch (final SocketException e) {
            // Client closed/aborted (e.g. browser cancelled connection, TLS abort, recv failed)
            closeQuietly(sslSocket);
            return null;
        } catch (final IOException e) {
            // Best-effort: treat common JSSE/client-abort messages as benign (avoid logging as error).
            final String msg = e.getMessage();
            if (msg != null && (msg.contains("closed") || msg.contains("abort") || msg.contains("recv failed") || msg.contains("write error"))) {
                closeQuietly(sslSocket);
                return null;
            }
            throw e;
        }
    }

    private static void closeQuietly(final SSLSocket sslSocket) {
        try {
            if (sslSocket != null && !sslSocket.isClosed()) {
                LogV3.fine("SSL connection closed by client or handshake abort");
                sslSocket.close();
            }
        } catch (final Throwable ignored) {
        }
    }

    /**
     * Creates an SSLContext from a PKCS12 keystore file.
     *
     * @param keystorePath
     *            path to the .p12/.pfx file
     * @param keystorePassword
     *            keystore password (prefer {@link #createSSLContextFromPKCS12(String, char[])} to avoid keeping password in String memory)
     * @return initialized SSLContext
     */
    public static SSLContext createSSLContextFromPKCS12(final String keystorePath, final String keystorePassword) throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException, UnrecoverableKeyException, KeyManagementException {
        return createSSLContextFromPKCS12(keystorePath, keystorePassword == null ? null : keystorePassword.toCharArray());
    }

    /**
     * Creates an SSLContext from a PKCS12 keystore file. Prefer this overload when the password is sensitive (char[] can be cleared after
     * use).
     *
     * @param keystorePath
     *            path to the .p12/.pfx file
     * @param keystorePassword
     *            keystore password; may be null for password-less keystores
     * @return initialized SSLContext
     */
    public static SSLContext createSSLContextFromPKCS12(final String keystorePath, final char[] keystorePassword) throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException, UnrecoverableKeyException, KeyManagementException {
        final FileInputStream fis = new FileInputStream(keystorePath);
        final KeyStore ks = KeyStore.getInstance("PKCS12");
        try {
            ks.load(fis, keystorePassword);
        } finally {
            fis.close();
        }
        return createSSLContextFromKeyStore(ks, keystorePassword);
    }

    /**
     * Creates an SSLContext from an in-memory KeyStore (e.g. from
     * {@link org.appwork.utils.net.httpconnection.tests.CertificateFactory#createPKCS12KeyStore}). Avoids writing keystores to temp files
     * when certificates are created on-the-fly.
     *
     * @param keyStore
     *            PKCS12 or other KeyStore containing the server certificate and private key
     * @param keyStorePassword
     *            password for the keystore
     * @return initialized SSLContext
     */
    public static SSLContext createSSLContextFromKeyStore(final KeyStore keyStore, final char[] keyStorePassword) throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
        final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        kmf.init(keyStore, keyStorePassword);
        final SSLContext sc = SSLContext.getInstance("TLS");
        sc.init(kmf.getKeyManagers(), null, null);
        return sc;
    }
}
