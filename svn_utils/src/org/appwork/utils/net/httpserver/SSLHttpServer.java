package org.appwork.utils.net.httpserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;

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
 * Client certificate validation is done during the TLS handshake by the {@link SSLContext}'s TrustManagers
 * ({@link javax.net.ssl.X509TrustManager#checkClientTrusted}). To fully control validation, use a custom
 * TrustManager or the TrustProvider-based constructor below.
 * </p>
 * <p>
 * <b>Using the TrustProvider system (full override):</b> Use the constructor that takes
 * {@link TrustProviderInterface} to delegate client certificate validation entirely to the same abstraction used by
 * the HttpClient stack. The provider's {@link TrustProviderInterface#getAcceptedIssuers()} and
 * {@link TrustProviderInterface#checkClientTrusted} are used during the TLS handshake (via an internal
 * {@link javax.net.ssl.X509TrustManager} bridge). Example: {@code new CustomTrustProvider(caCert)} or your own
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

    private final SSLContext              sslContext;
    private final ClientAuthMode          clientAuthMode;
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
     * Creates an SSL HTTP server with client certificate validation fully delegated to a {@link TrustProviderInterface}.
     * Same abstraction as the HttpClient stack: the provider's {@link TrustProviderInterface#getAcceptedIssuers()} and
     * {@link TrustProviderInterface#checkClientTrusted} are used during the TLS handshake (no separate
     * TrustManager needed). Use e.g. {@link org.appwork.utils.net.httpconnection.trust.CustomTrustProvider} with your
     * CA cert(s), or your own implementation (keystore, CRL, custom rules).
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
        }
        if (clientCertTrustProvider == null) {
            throw new IllegalArgumentException("clientCertTrustProvider is null");
        }
        if (clientAuthMode == null) {
            throw new IllegalArgumentException("clientAuthMode is null");
        }
        this.clientCertTrustResultHolder = new ThreadLocal<TrustResult>();
        this.sslContext = createSSLContext(serverKeystore, serverKeystorePassword, clientCertTrustProvider, this.clientCertTrustResultHolder);
        this.clientAuthMode = clientAuthMode;
    }

    /**
     * Builds an SSLContext with server identity from the keystore and client-cert validation from the TrustProvider.
     * Used by the constructor that takes TrustProviderInterface. When a client cert is validated, the callback
     * stores the TrustResult in holder so createHttpConnection can pass it to the request.
     */
    private static SSLContext createSSLContext(final KeyStore serverKeystore, final char[] serverKeystorePassword, final TrustProviderInterface clientCertTrustProvider, final ThreadLocal<TrustResult> holder) throws KeyStoreException, NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
        final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        kmf.init(serverKeystore, serverKeystorePassword);
        final TrustCallback callback = new TrustCallback() {
            public void onTrustResult(final TrustProviderInterface provider, final X509Certificate[] chain, final String authType, final TrustResult result) {
                if (holder != null && result != null) {
                    holder.set(result);
                }
            }

            public TrustProviderInterface getTrustProvider() {
                return clientCertTrustProvider;
            }
        };
        final TrustManager trustManager = JavaSSLSocketStreamFactory.generateTrustManagerDelegate(callback);
        final SSLContext sc = SSLContext.getInstance("TLS");
        sc.init(kmf.getKeyManagers(), new TrustManager[] { trustManager }, null);
        return sc;
    }

    @Override
    protected ServerSocket createServerSocket() throws IOException {
        final SSLServerSocket sslServerSocket = (SSLServerSocket) this.sslContext.getServerSocketFactory().createServerSocket();
        sslServerSocket.setReuseAddress(true);
        // Only call one of the two: the second call would override the first (per Javadoc).
        if (this.clientAuthMode == ClientAuthMode.NEED) {
            sslServerSocket.setNeedClientAuth(true);
        } else if (this.clientAuthMode == ClientAuthMode.WANT) {
            sslServerSocket.setWantClientAuth(true);
        } else {
            sslServerSocket.setWantClientAuth(false);
        }
        return sslServerSocket;
    }

    @Override
    protected HttpConnectionRunnable createHttpConnection(final Socket clientSocket) throws IOException {
        if (clientSocket == null) {
            throw new IOException("ClientSocket is null");
        }
        // When using SSLServerSocket, accept() always returns an SSLSocket with handshake completed
        final SSLSocket sslSocket = (SSLSocket) clientSocket;
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
        TrustResult trustResult = null;
        if (this.clientCertTrustResultHolder != null) {
            trustResult = this.clientCertTrustResultHolder.get();
            this.clientCertTrustResultHolder.remove();
        }
        if (trustResult == null && clientCertChain != null && clientCertChain.length > 0) {
            trustResult = new TrustResult(null, clientCertChain, null, TrustResult.TrustType.CLIENT);
        }
        return new HttpServerConnection(this, sslSocket, sslSocket.getInputStream(), sslSocket.getOutputStream(), true, trustResult);
    }

    public static SSLContext createSSLContextFromPKCS12(final String keystorePath, final String keystorePassword) throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException, UnrecoverableKeyException, KeyManagementException {
        final KeyStore ks = KeyStore.getInstance("PKCS12");
        java.io.FileInputStream fis = null;
        try {
            fis = new java.io.FileInputStream(keystorePath);
            ks.load(fis, keystorePassword.toCharArray());
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (final java.io.IOException ignored) {
                }
            }
        }
        return createSSLContextFromKeyStore(ks, keystorePassword.toCharArray());
    }

    /**
     * Creates an SSLContext from an in-memory KeyStore (e.g. from {@link org.appwork.utils.net.httpconnection.tests.CertificateFactory#createPKCS12KeyStore}).
     * Avoids writing keystores to temp files when certificates are created on-the-fly.
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
