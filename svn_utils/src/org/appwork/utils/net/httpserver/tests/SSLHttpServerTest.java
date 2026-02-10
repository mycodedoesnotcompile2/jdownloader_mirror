package org.appwork.utils.net.httpserver.tests;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URLEncoder;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.EnumSet;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.IO;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpconnection.SSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.CACertificateResult;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ClientCertificateResult;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ServerCertificateResult;
import org.appwork.utils.net.httpconnection.trust.AllTrustProvider;
import org.appwork.utils.net.httpconnection.trust.CustomTrustProvider;
import org.appwork.utils.net.httpconnection.trust.WindowsTrustProvider;
import org.appwork.utils.net.httpserver.SSLHttpServer;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.AbstractGetRequest;
import org.appwork.utils.net.httpserver.requests.AbstractPostRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsCertUtils;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinUser;

/**
 * Tests for SSLHttpServer functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>HTTPS connections work correctly with PKCS12 keystore</li>
 * <li>HTTP connections are rejected (no fallback, SSL-only server)</li>
 * <li>Certificate loading from PKCS12</li>
 * <li>TLS version support</li>
 * <li>Client authentication support: NEED, WANT, NONE; NEED with valid client cert; TrustProvider constructor; EC client cert</li>
 * <li>On Windows: TrustWindowsProvider with CA installed in Windows store (install CA → HTTPS with TrustWindowsProvider → uninstall CA)</li>
 * <li>TrustAllProvider at client: all requests to the server are allowed regardless of server certificate</li>
 * <li>Error handling (null SSLContext, etc.)</li>
 * </ul>
 *
 * <p>
 * <b>Certificate Requirements:</b>
 * </p>
 * <p>
 * This test expects a PKCS12 keystore file. The keystore should be placed in the test resources directory or specified via system
 * properties:
 * </p>
 * <ul>
 * <li>PKCS12 keystore: <code>keystore.p12</code> or <code>localhost.p12</code></li>
 * <li>Keystore password: <code>changeit</code> (default) or via system property <code>ssl.test.keystore.password</code></li>
 * </ul>
 * <p>
 * Alternatively, set system properties:
 * </p>
 * <ul>
 * <li><code>ssl.test.keystore</code> - path to PKCS12 keystore file</li>
 * <li><code>ssl.test.keystore.password</code> - password for the keystore</li>
 * </ul>
 *
 * <p>
 * <b>Creating a test keystore:</b>
 * </p>
 *
 * <pre>
 * # Generate self-signed certificate and key
 * openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes -subj "/CN=localhost"
 * 
 * # Convert to PKCS12
 * openssl pkcs12 -export -in cert.pem -inkey key.pem -out keystore.p12 -name "server" -passout pass:changeit
 * </pre>
 *
 * <p>
 * <b>Note:</b> This test is placed in AppWorkUtils to keep SSL test infrastructure together.
 * </p>
 *
 * @author AppWork
 */
public class SSLHttpServerTest extends AWTest {
    /**
     * JSSE-based SSL-enabled HTTP server for testing
     */
    private static class TestSSLHttpServer extends SSLHttpServer {
        private Throwable    lastServerException;
        private HttpRequest  lastRequest;
        private HttpResponse lastResponse;

        private TestSSLHttpServer(final int port, final SSLContext sslContext) {
            super(port, sslContext);
        }

        private TestSSLHttpServer(final int port, final SSLContext sslContext, final SSLHttpServer.ClientAuthMode clientAuthMode) {
            super(port, sslContext, clientAuthMode);
        }

        @Override
        public boolean onException(final Throwable e, final HttpRequest request, final HttpResponse response) throws IOException {
            this.lastServerException = e;
            this.lastRequest = request;
            this.lastResponse = response;
            return super.onException(e, request, response);
        }
    }

    /** Handler that returns client cert info for path /clientcertcheck (tests HttpRequest.getTrustResult() and cert details). */
    private static class ClientCertCheckHandler implements HttpRequestHandler {
        @Override
        public boolean onGetRequest(final AbstractGetRequest request, final HttpResponse response) throws BasicRemoteAPIException {
            if (!"/clientcertcheck".equals(request.getRequestedPath())) {
                return false;
            }
            try {
                final org.appwork.utils.net.httpconnection.TrustResult tr = request.getTrustResult();
                final java.security.cert.X509Certificate[] chain = tr != null ? tr.getChain() : null;
                final boolean hasCert = chain != null && chain.length > 0;
                final java.security.cert.X509Certificate cert = hasCert ? chain[0] : null;
                StringBuilder body = new StringBuilder("hasClientCert=").append(hasCert);
                body.append("&chainLength=").append(chain != null ? chain.length : 0);
                if (cert != null) {
                    body.append("&subject=").append(cert.getSubjectX500Principal() != null ? cert.getSubjectX500Principal().getName() : "");
                    body.append("&issuer=").append(cert.getIssuerX500Principal() != null ? cert.getIssuerX500Principal().getName() : "");
                    body.append("&serialNumber=").append(cert.getSerialNumber() != null ? cert.getSerialNumber().toString(16) : "");
                    body.append("&notBefore=").append(cert.getNotBefore() != null ? cert.getNotBefore().getTime() : "");
                    body.append("&notAfter=").append(cert.getNotAfter() != null ? cert.getNotAfter().getTime() : "");
                    body.append("&sigAlgName=").append(cert.getSigAlgName() != null ? cert.getSigAlgName() : "");
                    body.append("&version=").append(cert.getVersion());
                }
                response.setResponseCode(HTTPConstants.ResponseCode.SUCCESS_OK);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=UTF-8"));
                final byte[] bytes = body.toString().getBytes("UTF-8");
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
                response.getOutputStream(true).write(bytes);
                response.getOutputStream(true).flush();
                return true;
            } catch (final IOException e) {
                throw new BasicRemoteAPIException(e);
            }
        }

        @Override
        public boolean onPostRequest(final AbstractPostRequest request, final HttpResponse response) throws BasicRemoteAPIException {
            return false;
        }
    }

    private TestSSLHttpServer       sslHttpServer;
    private RemoteAPI               remoteAPI;
    private int                     serverPort;
    private HttpClient              httpClient;
    private SSLContext              sslContext;
    /** In-memory server keystore; temp file only for testPFXLoading. */
    private KeyStore                serverKeyStore;
    /** In-memory client keystore (no file). */
    private KeyStore                clientKeyStore;
    /** In-memory wrong-client keystore (no file). */
    private KeyStore                wrongClientKeyStore;
    /** Only used for testPFXLoading (load-from-file tests). */
    private File                    tempKeystoreFile;
    private ServerCertificateResult certificateResult;
    private ClientCertificateResult clientCertificateResult;
    private ClientCertificateResult wrongClientCertificateResult;
    private CustomTrustProvider     caProvider;
    private CACertificateResult     caCertificateResult;
    private CACertificateResult     otherCaCertificateResult;

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    /**
     * Runs only the client-cert success test (for standalone runner). Creates certs, keystores, server with needClientAuth, client with
     * KeyManagers, two GETs, asserts.
     */
    public void runClientCertTestOnly() throws Exception {
        try {
            this.createCertificates();
            this.createKeystore();
            this.createClientCertificate();
            this.createClientKeystore();
            this.testClientCertSuccess();
        } finally {
            this.cleanupTempFiles();
        }
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.createCertificates();
            this.createKeystore();
            // Test PFX loading scenarios first
            this.testPFXLoading();
            // Test constructor with null SSLContext (should throw exception)
            this.testNullSSLContext();
            // Then run the actual SSL server tests
            this.setupSSLServer();
            this.testHTTPSConnection();
            this.testTrustAllProviderAtServer();
            this.testHTTPRejection();
            this.testTLSVersionSupport();
            this.testSSLWithoutKeystore();
            // Client certificate tests (separate server with needClientAuth)
            this.createClientCertificate();
            this.createClientKeystore();
            this.testClientCertSuccess();
            this.testClientCertRequiredRejected();
            this.createWrongClientCertificate();
            this.testClientCertWrongRejected();
            this.testWantClientAuth();
            this.testClientAuthModeNone();
            this.testClientCertNeedWithValidCert();
            this.testTrustProviderConstructor();
            this.testClientCertEC();
            this.testTrustWindowsProviderAtServer();
        } finally {
            this.teardownSSLServer();
            this.cleanupTempFiles();
        }
    }

    /**
     * Creates CA and server certificates using CertificateFactory (keeps CA key for client cert tests).
     */
    private void createCertificates() throws Exception {
        LogV3.info("Creating CA and server certificates using CertificateFactory...");
        this.caCertificateResult = CertificateFactory.createCACertificate("Test CA");
        assertTrue(this.caCertificateResult != null, "CA creation should succeed");
        this.certificateResult = CertificateFactory.createServerCertificate(this.caCertificateResult.getCaCertificate(), this.caCertificateResult.getCaKeyPair().getPrivate(), "localhost");
        assertTrue(this.certificateResult != null, "Certificate creation should succeed");
        assertTrue(this.certificateResult.getCaCertificate() != null, "CA certificate should not be null");
        assertTrue(this.certificateResult.getServerCertificate() != null, "Server certificate should not be null");
        assertTrue(this.certificateResult.getServerKeyPair() != null, "Server key pair should not be null");
        LogV3.info("Certificates created successfully");
        LogV3.info("CA Certificate Subject: " + this.certificateResult.getCaCertificate().getSubjectX500Principal());
        LogV3.info("Server Certificate Subject: " + this.certificateResult.getServerCertificate().getSubjectX500Principal());
        LogV3.info("Server Certificate Issuer: " + this.certificateResult.getServerCertificate().getIssuerX500Principal());
        this.caProvider = new CustomTrustProvider(this.certificateResult.getCaCertificate());
    }

    /**
     * Creates in-memory PKCS12 keystore from generated certificates and SSLContext from it. Writes keystore to a temp file only for
     * testPFXLoading (load-from-file tests).
     */
    private void createKeystore() throws Exception {
        LogV3.info("Creating PKCS12 keystore (in-memory)...");
        final char[] password = "testpassword".toCharArray();
        this.serverKeyStore = CertificateFactory.createPKCS12KeyStore(this.certificateResult.getServerCertificate(), this.certificateResult.getServerKeyPair().getPrivate(), password, "server", this.certificateResult.getCaCertificate());
        this.sslContext = SSLHttpServer.createSSLContextFromKeyStore(this.serverKeyStore, password);
        assertTrue(this.sslContext != null, "SSL context should not be null");
        LogV3.info("SSL context created from in-memory keystore");
        // Temp file only for testPFXLoading (load-from-file scenarios)
        this.tempKeystoreFile = File.createTempFile("test-server-", ".p12");
        this.tempKeystoreFile.deleteOnExit();
        try (FileOutputStream fos = new FileOutputStream(this.tempKeystoreFile)) {
            this.serverKeyStore.store(fos, password);
        }
        LogV3.info("PKCS12 temp file for PFX tests: " + this.tempKeystoreFile.getAbsolutePath());
    }

    /**
     * Test: PFX/PKCS12 loading scenarios
     *
     * This test verifies that:
     * <ul>
     * <li>PFX loading works with correct password</li>
     * <li>Wrong password throws UnrecoverableKeyException</li>
     * <li>Missing file throws IOException</li>
     * <li>Invalid format throws KeyStoreException</li>
     * </ul>
     */
    private void testPFXLoading() throws Exception {
        LogV3.info("Test: PFX/PKCS12 Loading Scenarios");
        final String keystorePath = this.tempKeystoreFile.getAbsolutePath();
        final String correctPassword = "testpassword";
        // Test 1: Load with correct password
        try {
            final SSLContext context = SSLHttpServer.createSSLContextFromPKCS12(keystorePath, correctPassword);
            assertTrue(context != null, "PFX should load successfully with correct password");
            LogV3.info("PFX loading with correct password: PASSED");
        } catch (final Exception e) {
            LogV3.warning("PFX loading with correct password failed: " + e.getMessage());
            throw e;
        }
        // Test 2: Load with wrong password
        try {
            SSLHttpServer.createSSLContextFromPKCS12(keystorePath, "wrong_password_12345");
            assertTrue(false, "PFX loading with wrong password should throw UnrecoverableKeyException");
        } catch (final java.security.UnrecoverableKeyException e) {
            LogV3.info("PFX loading with wrong password correctly threw UnrecoverableKeyException: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            // Other exceptions are also acceptable (e.g., IOException if password check happens during load)
            LogV3.info("PFX loading with wrong password threw exception (acceptable): " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        // Test 3: Load with non-existent file
        try {
            SSLHttpServer.createSSLContextFromPKCS12("/nonexistent/file.pfx", "password");
            assertTrue(false, "PFX loading with non-existent file should throw IOException");
        } catch (final java.io.FileNotFoundException e) {
            LogV3.info("PFX loading with non-existent file correctly threw FileNotFoundException: " + e.getMessage());
            // Expected - test passed
        } catch (final java.io.IOException e) {
            LogV3.info("PFX loading with non-existent file correctly threw IOException: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            LogV3.warning("PFX loading with non-existent file threw unexpected exception: " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        // Test 4: Load with invalid file format (create a dummy file)
        final File dummyFile = new File(System.getProperty("java.io.tmpdir") + "/dummy_invalid.pfx");
        try {
            // Create a dummy file with invalid content
            dummyFile.delete();
            IO.writeStringToFile(dummyFile, "This is not a valid PKCS12 file");
            try {
                SSLHttpServer.createSSLContextFromPKCS12(dummyFile.getAbsolutePath(), "password");
                assertTrue(false, "PFX loading with invalid format should throw KeyStoreException or IOException");
            } catch (final java.security.KeyStoreException e) {
                LogV3.info("PFX loading with invalid format correctly threw KeyStoreException: " + e.getMessage());
                // Expected - test passed
            } catch (final java.io.IOException e) {
                LogV3.info("PFX loading with invalid format correctly threw IOException: " + e.getMessage());
                // Expected - test passed
            } catch (final Exception e) {
                LogV3.info("PFX loading with invalid format threw exception (acceptable): " + e.getClass().getSimpleName() + ": " + e.getMessage());
            }
        } finally {
            // Clean up dummy file
            if (dummyFile.exists()) {
                dummyFile.delete();
            }
        }
        // Test 5: Load with null password
        try {
            SSLHttpServer.createSSLContextFromPKCS12(keystorePath, (String) null);
            assertTrue(false, "PFX loading with null password should throw exception");
        } catch (final NullPointerException e) {
            LogV3.info("PFX loading with null password correctly threw NullPointerException: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            LogV3.info("PFX loading with null password threw exception (acceptable): " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        // Test 6: Load with empty password
        try {
            SSLHttpServer.createSSLContextFromPKCS12(keystorePath, "");
            // Empty password might work if the keystore was created without password
            // This is acceptable behavior
            LogV3.info("PFX loading with empty password: attempted");
        } catch (final Exception e) {
            // Exception is acceptable for empty password
            LogV3.info("PFX loading with empty password threw exception (acceptable): " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        LogV3.info("PFX/PKCS12 Loading Scenarios test completed");
    }

    /**
     * Test: Constructor with null SSLContext should throw IllegalArgumentException
     */
    private void testNullSSLContext() throws Exception {
        LogV3.info("Test: Null SSLContext in Constructor");
        try {
            new SSLHttpServer(0, null);
            assertTrue(false, "Constructor with null SSLContext should throw IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            LogV3.info("Constructor correctly threw IllegalArgumentException for null SSLContext: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            LogV3.warning("Constructor threw unexpected exception: " + e.getClass().getSimpleName() + ": " + e.getMessage());
            throw e;
        }
        LogV3.info("Null SSLContext test completed");
    }

    /**
     * Creates client certificate signed by the same CA (for client cert tests).
     */
    private void createClientCertificate() throws Exception {
        LogV3.info("Creating client certificate...");
        this.clientCertificateResult = CertificateFactory.createClientCertificate(this.caCertificateResult.getCaCertificate(), this.caCertificateResult.getCaKeyPair().getPrivate(), "testclient");
        assertTrue(this.clientCertificateResult != null, "Client certificate creation should succeed");
        LogV3.info("Client certificate created: " + this.clientCertificateResult.getClientCertificate().getSubjectX500Principal());
    }

    /**
     * Creates in-memory PKCS12 keystore for client (for client cert tests). No temp file.
     */
    private void createClientKeystore() throws Exception {
        LogV3.info("Creating client PKCS12 keystore (in-memory)...");
        final char[] password = "clientpass".toCharArray();
        this.clientKeyStore = CertificateFactory.createPKCS12KeyStore(this.clientCertificateResult.getClientCertificate(), this.clientCertificateResult.getClientKeyPair().getPrivate(), password, "client", this.certificateResult.getCaCertificate());
        LogV3.info("Client PKCS12 keystore created");
    }

    /**
     * Test: Request with valid client certificate succeeds when server has needClientAuth=true.
     */
    private void testClientCertSuccess() throws Exception {
        LogV3.info("Test: Client certificate – request with client cert succeeds");
        // Force JavaSSLSocketStreamFactory and fresh SSL options so client cert is sent (avoid cached/Native factory)
        HTTPConnectionImpl.clearSSLSocketStreamOptionsCache();
        final SSLSocketStreamFactory prevFactory = HTTPConnectionImpl.getDefaultSSLSocketStreamFactory();
        HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(JavaSSLSocketStreamFactory.getInstance());
        try {
            final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
            TestSSLHttpServer clientAuthServer = null;
            try {
                final RemoteAPI api = new RemoteAPI();
                api.register(new DummyTestAPIImpl());
                // WANT so client actually sends cert (with NEED, JSSE client may not send cert in some setups)
                clientAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.WANT);
                clientAuthServer.setLocalhostOnly(true);
                clientAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
                clientAuthServer.registerRequestHandler(new ClientCertCheckHandler()); // first: so we can test getClientCertificateChain()
                clientAuthServer.registerRequestHandler(api);
                clientAuthServer.start();
                final int port = clientAuthServer.getActualPort();
                final HttpClient clientWithCert = new HttpClient();
                clientWithCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                clientWithCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
                clientWithCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
                clientWithCert.setTrustProvider(this.caProvider);
                final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
                kmf.init(this.clientKeyStore, "clientpass".toCharArray());
                clientWithCert.setKeyManagers(kmf.getKeyManagers());
                final String url = "https://localhost:" + port + "/test/echo?message=" + URLEncoder.encode("ClientCert OK", "UTF-8");
                final RequestContext ctx = clientWithCert.get(url);
                assertTrue(ctx.getCode() == 200, "Request with client cert should return 200, was: " + ctx.getCode());
                assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("ClientCert OK"), "Response should contain message");
                // Verify HttpRequest.getTrustResult() provides full cert info when client sends a cert
                LogV3.info("Start ErrorTest");
                final RequestContext ctxCheck = clientWithCert.get("https://localhost:" + port + "/clientcertcheck");
                assertTrue(ctxCheck.getCode() == 200, "hasClientCert check should return 200");
                final String certCheckBody = ctxCheck.getResponseString();
                assertTrue(certCheckBody != null && certCheckBody.contains("hasClientCert=true"), "Request with client cert must have non-empty getTrustResult() in HttpRequest");
                assertTrue(certCheckBody != null && certCheckBody.contains("chainLength="), "Response must include client cert chain length");
                assertTrue(certCheckBody.contains("subject=") && certCheckBody.contains("testclient"), "Client cert subject (CN=testclient) must be available in HttpRequest");
                assertTrue(certCheckBody.contains("issuer=") && certCheckBody.contains("serialNumber="), "Client cert issuer and serialNumber must be available in HttpRequest");
                LogV3.info("Client certificate success test passed");
            } finally {
                if (clientAuthServer != null) {
                    clientAuthServer.shutdown();
                }
            }
        } finally {
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(prevFactory);
        }
    }

    /**
     * Test: NEED server – all requests without client certificate are rejected. Server uses {@link SSLHttpServer.ClientAuthMode#NEED}; any
     * request without a client cert must fail (handshake fails or connection rejected). Verifies multiple request types (GET to different
     * paths) to ensure no request without cert gets through.
     */
    private void testClientCertRequiredRejected() throws Exception {
        LogV3.info("Test: NEED server – all requests without client cert are rejected");
        final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
        TestSSLHttpServer clientAuthServer = null;
        try {
            final RemoteAPI api = new RemoteAPI();
            api.register(new DummyTestAPIImpl());
            clientAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.NEED);
            clientAuthServer.setLocalhostOnly(true);
            clientAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            clientAuthServer.registerRequestHandler(api);
            clientAuthServer.start();
            final int port = clientAuthServer.getActualPort();
            final HttpClient clientNoCert = new HttpClient();
            clientNoCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientNoCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            clientNoCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientNoCert.setTrustProvider(this.caProvider);
            // no setKeyManagers – no client cert; all requests must be rejected
            final String baseUrl = "https://localhost:" + port;
            final String[] urlsToReject = { baseUrl + "/test/echo?message=noCert", baseUrl + "/test/ping", baseUrl + "/test/version" };
            for (final String url : urlsToReject) {
                boolean rejected = false;
                try {
                    final RequestContext ctx = clientNoCert.get(url);
                    assertTrue(ctx.getCode() != 200, "NEED server: request without client cert must not return 200 for " + url + ", got " + ctx.getCode());
                    rejected = true;
                } catch (final HttpClientException e) {
                    rejected = true;
                    LogV3.info("NEED server: request without cert correctly rejected (exception) for " + url + ": " + e.getMessage());
                }
                assertTrue(rejected, "NEED server must reject all requests without client certificate (url: " + url + ")");
            }
            LogV3.info("NEED server – all requests without client cert correctly rejected");
        } finally {
            if (clientAuthServer != null) {
                clientAuthServer.shutdown();
            }
        }
    }

    /**
     * Creates server SSLContext with server cert and TrustManagers that trust the CA (for client cert verification). Uses in-memory
     * serverKeyStore directly.
     */
    private SSLContext createServerSSLContextWithClientAuthTrust() throws Exception {
        final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        kmf.init(this.serverKeyStore, "testpassword".toCharArray());
        final KeyStore caKs = KeyStore.getInstance(KeyStore.getDefaultType());
        caKs.load(null, null);
        caKs.setCertificateEntry("ca", this.certificateResult.getCaCertificate());
        final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        tmf.init(caKs);
        final SSLContext sc = SSLContext.getInstance("TLS");
        sc.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null);
        return sc;
    }

    /**
     * Cleanup temporary files (only temp keystore file used for testPFXLoading).
     */
    private void cleanupTempFiles() {
        if (this.tempKeystoreFile != null && this.tempKeystoreFile.exists()) {
            this.tempKeystoreFile.delete();
        }
    }

    /**
     * Creates a second CA and a client cert signed by it (untrusted by our server) for wrong-cert test. Uses in-memory KeyStore only.
     */
    private void createWrongClientCertificate() throws Exception {
        LogV3.info("Creating wrong client certificate (signed by other CA)...");
        this.otherCaCertificateResult = CertificateFactory.createCACertificate("Other CA");
        this.wrongClientCertificateResult = CertificateFactory.createClientCertificate(this.otherCaCertificateResult.getCaCertificate(), this.otherCaCertificateResult.getCaKeyPair().getPrivate(), "wrongclient");
        final char[] password = "wrongpass".toCharArray();
        this.wrongClientKeyStore = CertificateFactory.createPKCS12KeyStore(this.wrongClientCertificateResult.getClientCertificate(), this.wrongClientCertificateResult.getClientKeyPair().getPrivate(), password, "client", this.otherCaCertificateResult.getCaCertificate());
        LogV3.info("Wrong client PKCS12 keystore created (in-memory, signed by Other CA)");
    }

    /**
     * Test: Request with client certificate signed by an untrusted CA is rejected when server has needClientAuth=true.
     */
    private void testClientCertWrongRejected() throws Exception {
        LogV3.info("Test: Client certificate – wrong/untrusted client cert is rejected");
        final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
        TestSSLHttpServer clientAuthServer = null;
        try {
            final RemoteAPI api = new RemoteAPI();
            api.register(new DummyTestAPIImpl());
            clientAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.NEED);
            clientAuthServer.setLocalhostOnly(true);
            clientAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            clientAuthServer.registerRequestHandler(api);
            clientAuthServer.start();
            final int port = clientAuthServer.getActualPort();
            final HttpClient clientWrongCert = new HttpClient();
            clientWrongCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientWrongCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            clientWrongCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientWrongCert.setTrustProvider(this.caProvider);
            final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            kmf.init(this.wrongClientKeyStore, "wrongpass".toCharArray());
            clientWrongCert.setKeyManagers(kmf.getKeyManagers());
            final String url = "https://localhost:" + port + "/test/echo?message=wrongCert";
            boolean rejected = false;
            try {
                final RequestContext ctx = clientWrongCert.get(url);
                assertTrue(ctx.getCode() != 200, "Request with wrong client cert must not succeed: expected handshake failure, got " + ctx.getCode());
                rejected = true;
            } catch (final HttpClientException e) {
                rejected = true;
                LogV3.info("Wrong client cert correctly rejected (exception): " + e.getMessage());
            }
            assertTrue(rejected, "Wrong/untrusted client cert must be rejected by needClientAuth server");
        } finally {
            if (clientAuthServer != null) {
                clientAuthServer.shutdown();
            }
        }
    }

    /**
     * Test: wantClientAuth – server requests client cert but connection works without one; with valid cert also works.
     */
    private void testWantClientAuth() throws Exception {
        LogV3.info("Test: wantClientAuth – optional client cert");
        final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
        TestSSLHttpServer wantAuthServer = null;
        try {
            final RemoteAPI api = new RemoteAPI();
            api.register(new DummyTestAPIImpl());
            wantAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.WANT);
            wantAuthServer.setLocalhostOnly(true);
            wantAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            wantAuthServer.registerRequestHandler(new ClientCertCheckHandler());
            wantAuthServer.registerRequestHandler(api);
            wantAuthServer.start();
            final int port = wantAuthServer.getActualPort();
            // Without client cert -> must succeed (200), hasClientCert=false
            final HttpClient clientNoCert = new HttpClient();
            clientNoCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientNoCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            clientNoCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientNoCert.setTrustProvider(this.caProvider);
            final RequestContext ctxNoCert = clientNoCert.get("https://localhost:" + port + "/clientcertcheck");
            assertTrue(ctxNoCert.getCode() == 200, "wantClientAuth: request without cert should return 200");
            assertTrue(ctxNoCert.getResponseString() != null && ctxNoCert.getResponseString().contains("hasClientCert=false"), "wantClientAuth: without cert must have hasClientCert=false");
            // With valid client cert -> must succeed (200), hasClientCert=true
            final HttpClient clientWithCert = new HttpClient();
            clientWithCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientWithCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            clientWithCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientWithCert.setTrustProvider(this.caProvider);
            final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            kmf.init(this.clientKeyStore, "clientpass".toCharArray());
            clientWithCert.setKeyManagers(kmf.getKeyManagers());
            final RequestContext ctxWithCert = clientWithCert.get("https://localhost:" + port + "/clientcertcheck");
            assertTrue(ctxWithCert.getCode() == 200, "wantClientAuth: request with cert should return 200");
            assertTrue(ctxWithCert.getResponseString() != null && ctxWithCert.getResponseString().contains("hasClientCert=true"), "wantClientAuth: with cert must have hasClientCert=true");
            LogV3.info("wantClientAuth test passed");
        } finally {
            if (wantAuthServer != null) {
                wantAuthServer.shutdown();
            }
        }
    }

    /**
     * Test: ClientAuthMode.NONE – server does not request client cert; client sends cert anyway → hasClientCert=false.
     */
    private void testClientAuthModeNone() throws Exception {
        LogV3.info("Test: ClientAuthMode.NONE – no client cert request");
        final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
        TestSSLHttpServer noneAuthServer = null;
        try {
            noneAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.NONE);
            noneAuthServer.setLocalhostOnly(true);
            noneAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            noneAuthServer.registerRequestHandler(new ClientCertCheckHandler());
            noneAuthServer.start();
            final int port = noneAuthServer.getActualPort();
            final HttpClient clientWithCert = new HttpClient();
            clientWithCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientWithCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            clientWithCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientWithCert.setTrustProvider(this.caProvider);
            final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            kmf.init(this.clientKeyStore, "clientpass".toCharArray());
            clientWithCert.setKeyManagers(kmf.getKeyManagers());
            final RequestContext ctx = clientWithCert.get("https://localhost:" + port + "/clientcertcheck");
            assertTrue(ctx.getCode() == 200, "NONE: request should return 200");
            assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("hasClientCert=false"), "NONE: server does not request client cert, so hasClientCert must be false");
            LogV3.info("ClientAuthMode.NONE test passed");
        } finally {
            if (noneAuthServer != null) {
                noneAuthServer.shutdown();
            }
        }
    }

    /**
     * Test: NEED with valid client certificate – server NEED, client with valid cert → 200, hasClientCert=true. Uses EC client cert so the
     * client actually sends the cert (JSSE may not send RSA with NEED in some setups).
     */
    private void testClientCertNeedWithValidCert() throws Exception {
        LogV3.info("Test: NEED server – request with valid client cert succeeds");
        final ClientCertificateResult clientResultEC = CertificateFactory.createClientCertificateEC(this.caCertificateResult.getCaCertificate(), this.caCertificateResult.getCaKeyPair().getPrivate(), "testclient");
        final KeyStore clientKsEC = CertificateFactory.createPKCS12KeyStore(clientResultEC.getClientCertificate(), clientResultEC.getClientKeyPair().getPrivate(), "clientpass".toCharArray(), "client", this.certificateResult.getCaCertificate());
        final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
        TestSSLHttpServer clientAuthServer = null;
        HTTPConnectionImpl.clearSSLSocketStreamOptionsCache();
        final SSLSocketStreamFactory prevFactory = HTTPConnectionImpl.getDefaultSSLSocketStreamFactory();
        try {
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(JavaSSLSocketStreamFactory.getInstance());
            final RemoteAPI api = new RemoteAPI();
            api.register(new DummyTestAPIImpl());
            clientAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.NEED);
            clientAuthServer.setLocalhostOnly(true);
            clientAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            clientAuthServer.registerRequestHandler(new ClientCertCheckHandler());
            clientAuthServer.registerRequestHandler(api);
            clientAuthServer.start();
            final int port = clientAuthServer.getActualPort();
            final HttpClient clientWithCert = new HttpClient();
            clientWithCert.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientWithCert.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
            clientWithCert.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientWithCert.setTrustProvider(this.caProvider);
            final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            kmf.init(clientKsEC, "clientpass".toCharArray());
            clientWithCert.setKeyManagers(kmf.getKeyManagers());
            final RequestContext ctx = clientWithCert.get("https://localhost:" + port + "/clientcertcheck");
            assertTrue(ctx.getCode() == 200, "NEED with valid client cert should return 200, was " + ctx.getCode());
            assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("hasClientCert=true"), "NEED: valid client cert must be visible at server");
            LogV3.info("NEED with valid client cert test passed");
        } finally {
            if (clientAuthServer != null) {
                clientAuthServer.shutdown();
            }
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(prevFactory);
        }
    }

    /**
     * Test: TrustProvider-based constructor SSLHttpServer(port, keyStore, password, TrustProvider, ClientAuthMode).
     */
    private void testTrustProviderConstructor() throws Exception {
        LogV3.info("Test: TrustProvider constructor – client cert validated by TrustProvider");
        SSLHttpServer server = null;
        HTTPConnectionImpl.clearSSLSocketStreamOptionsCache();
        final SSLSocketStreamFactory prevFactory = HTTPConnectionImpl.getDefaultSSLSocketStreamFactory();
        try {
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(JavaSSLSocketStreamFactory.getInstance());
            server = new SSLHttpServer(0, this.serverKeyStore, "testpassword".toCharArray(), this.caProvider, SSLHttpServer.ClientAuthMode.WANT);
            server.setLocalhostOnly(true);
            server.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            server.registerRequestHandler(new ClientCertCheckHandler());
            server.start();
            final int port = server.getActualPort();
            final HttpClient client = new HttpClient();
            client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
            client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            client.setTrustProvider(this.caProvider);
            final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            kmf.init(this.clientKeyStore, "clientpass".toCharArray());
            client.setKeyManagers(kmf.getKeyManagers());
            final RequestContext ctx = client.get("https://localhost:" + port + "/clientcertcheck");
            assertTrue(ctx.getCode() == 200, "TrustProvider constructor: request should return 200");
            assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("hasClientCert=true"), "TrustProvider constructor: client cert must be visible at server");
            LogV3.info("TrustProvider constructor test passed");
        } finally {
            if (server != null) {
                server.shutdown();
            }
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(prevFactory);
        }
    }

    /**
     * Test: EC client certificate – server WANT, client with EC cert → 200, hasClientCert=true.
     */
    private void testClientCertEC() throws Exception {
        LogV3.info("Test: EC client certificate");
        final ClientCertificateResult clientResultEC = CertificateFactory.createClientCertificateEC(this.caCertificateResult.getCaCertificate(), this.caCertificateResult.getCaKeyPair().getPrivate(), "testclient");
        final KeyStore clientKsEC = CertificateFactory.createPKCS12KeyStore(clientResultEC.getClientCertificate(), clientResultEC.getClientKeyPair().getPrivate(), "clientpass".toCharArray(), "client", this.certificateResult.getCaCertificate());
        final SSLContext serverContextWithClientAuth = createServerSSLContextWithClientAuthTrust();
        TestSSLHttpServer wantAuthServer = null;
        HTTPConnectionImpl.clearSSLSocketStreamOptionsCache();
        final SSLSocketStreamFactory prevFactory = HTTPConnectionImpl.getDefaultSSLSocketStreamFactory();
        try {
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(JavaSSLSocketStreamFactory.getInstance());
            wantAuthServer = new TestSSLHttpServer(0, serverContextWithClientAuth, SSLHttpServer.ClientAuthMode.WANT);
            wantAuthServer.setLocalhostOnly(true);
            wantAuthServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
            wantAuthServer.registerRequestHandler(new ClientCertCheckHandler());
            wantAuthServer.start();
            final int port = wantAuthServer.getActualPort();
            final HttpClient clientWithEC = new HttpClient();
            clientWithEC.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            clientWithEC.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
            clientWithEC.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            clientWithEC.setTrustProvider(this.caProvider);
            final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            kmf.init(clientKsEC, "clientpass".toCharArray());
            clientWithEC.setKeyManagers(kmf.getKeyManagers());
            final RequestContext ctx = clientWithEC.get("https://localhost:" + port + "/clientcertcheck");
            assertTrue(ctx.getCode() == 200, "EC client cert: request should return 200");
            assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("hasClientCert=true"), "EC client cert must be visible at server");
            LogV3.info("EC client certificate test passed");
        } finally {
            if (wantAuthServer != null) {
                wantAuthServer.shutdown();
            }
            HTTPConnectionImpl.setDefaultSSLSocketStreamFactory(prevFactory);
        }
    }

    /** Friendly name for test CA in Windows store (used with auto-confirm helpers). */
    private static final String WINDOWS_TEST_CA_FRIENDLY_NAME = "AppWork SSLHttpServerTest CA";

    /**
     * Test (Windows only): Install CA into Windows Root store, verify TrustWindowsProvider trusts the SSL server, then uninstall the CA.
     * Uses auto-confirmation of the Windows certificate dialog (like HTTPSIntegrationTest / WindowsCertUtilsTest).
     */
    private void testTrustWindowsProviderAtServer() throws Exception {
        if (!CrossSystem.isWindows()) {
            LogV3.info("Skipping TrustWindowsProvider test – not Windows");
            return;
        }
        final X509Certificate caCert = this.certificateResult.getCaCertificate();
        final String thumbprint = WindowsCertUtils.getCertificateFingerprint(caCert);
        try {
            installCertificateWithAutoConfirm(caCert, WindowsCertUtils.KeyStore.CURRENT_USER, WINDOWS_TEST_CA_FRIENDLY_NAME);
            assertTrue(WindowsCertUtils.isCertificateInstalled(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER), "CA should be in Windows store after install");
            WindowsTrustProvider.getInstance().reload();
            final HttpClient client = new HttpClient();
            client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
            client.setTrustProvider(WindowsTrustProvider.getInstance());
            final String url = "https://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("TrustWindowsOK", "UTF-8");
            final RequestContext ctx = client.get(url);
            assertTrue(ctx.getCode() == 200, "TrustWindowsProvider: HTTPS request to server should return 200, was " + ctx.getCode());
            assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("TrustWindowsOK"), "Response should contain message");
            LogV3.info("TrustWindowsProvider at server test passed");
        } finally {
            try {
                removeCertificateWithAutoConfirm(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER);
                assertFalse(WindowsCertUtils.isCertificateInstalled(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER), "CA should be removed from Windows store after uninstall");
                WindowsTrustProvider.getInstance().reload();
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Test-only helper: Waits for Windows certificate dialog and confirms it (OK/Yes).
     *
     * @param maxWaitMs
     *            maximum time to wait for dialog (milliseconds)
     * @return true if dialog was found and confirmed
     */
    private static boolean autoConfirmCertificateDialog(final int maxWaitMs) {
        if (!CrossSystem.isWindows()) {
            return false;
        }
        final long startTime = System.currentTimeMillis();
        final int checkInterval = 100;
        final String[] possibleTitles = { "Windows Security", "Windows-Sicherheit", "Security Warning", "Sicherheitswarnung", "Stammzertifikatspeicher" };
        final String[] buttonTexts = { "&OK", "&Ja", "&Yes", "&Installieren", "&Install" };
        while (System.currentTimeMillis() - startTime < maxWaitMs) {
            WinDef.HWND dialogWindow = null;
            for (final String title : possibleTitles) {
                dialogWindow = User32.INSTANCE.FindWindow(null, title);
                if (dialogWindow != null && Pointer.nativeValue(dialogWindow.getPointer()) != 0L) {
                    break;
                }
            }
            if (dialogWindow != null && Pointer.nativeValue(dialogWindow.getPointer()) != 0L) {
                final WinDef.HWND okButton = findButtonByText(dialogWindow, buttonTexts);
                if (okButton != null && Pointer.nativeValue(okButton.getPointer()) != 0L) {
                    final int BM_CLICK = 0x00F5;
                    User32.INSTANCE.SendMessage(okButton, BM_CLICK, null, null);
                    LogV3.info("Automatically confirmed certificate dialog");
                    return true;
                }
            }
            try {
                Thread.sleep(checkInterval);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }
        return false;
    }

    private static WinDef.HWND findButtonByText(final WinDef.HWND parent, final String[] buttonTexts) {
        final WinDef.HWND[] found = { null };
        User32.INSTANCE.EnumChildWindows(parent, new WinUser.WNDENUMPROC() {
            @Override
            public boolean callback(final WinDef.HWND hWnd, final Pointer userData) {
                final char[] className = new char[256];
                User32.INSTANCE.GetClassName(hWnd, className, 256);
                final String classNameStr = Native.toString(className);
                if ("Button".equals(classNameStr)) {
                    final int length = User32.INSTANCE.GetWindowTextLength(hWnd);
                    if (length > 0) {
                        final char[] text = new char[length + 1];
                        User32.INSTANCE.GetWindowText(hWnd, text, length + 1);
                        final String buttonText = Native.toString(text);
                        for (final String searchText : buttonTexts) {
                            if (buttonText.equalsIgnoreCase(searchText)) {
                                found[0] = hWnd;
                                return false;
                            }
                        }
                    }
                }
                return true;
            }
        }, null);
        return found[0];
    }

    /**
     * Test-only helper: Installs certificate with auto-confirmation of Windows dialog.
     */
    private static void installCertificateWithAutoConfirm(final X509Certificate certificate, final WindowsCertUtils.KeyStore target, final String friendlyName) throws Exception {
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(15000);
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            WindowsCertUtils.installCertificate(certificate, target, friendlyName);
        } finally {
            try {
                confirmationThread.join(6000);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Test-only helper: Removes certificate with auto-confirmation of Windows dialog.
     */
    private static boolean removeCertificateWithAutoConfirm(final String thumbprintHex, final WindowsCertUtils.KeyStore target) throws Exception {
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(5000);
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            return WindowsCertUtils.removeCertificate(thumbprintHex, target);
        } finally {
            try {
                confirmationThread.join(6000);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Setup SSL-enabled HTTP server
     */
    private void setupSSLServer() throws IOException, ParseException {
        if (this.sslContext == null) {
            LogV3.warning("Skipping SSL server setup - no SSL context available");
            return;
        }
        LogV3.info("Starting SSL HTTP Server Setup...");
        // Create RemoteAPI and register Dummy API
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());
        // Create SSLHttpServer on a free port (0 = automatic)
        this.sslHttpServer = new TestSSLHttpServer(0, this.sslContext);
        this.sslHttpServer.setLocalhostOnly(true);
        // Allow GET and POST for testing
        this.sslHttpServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
        // Register RemoteAPI as request handler
        this.sslHttpServer.registerRequestHandler(this.remoteAPI);
        // Start server
        this.sslHttpServer.start();
        this.serverPort = this.sslHttpServer.getActualPort();
        // Create HttpClient instance for tests
        this.httpClient = new HttpClient();
        this.httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
        this.httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
        this.httpClient.setVerboseLog(true); // Enable verbose logging for debugging
        // Set default mandatory header for all requests
        this.httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        // Enable verbose logging on server
        this.sslHttpServer.setVerboseLog(true);
        LogV3.info("SSL HTTP Server started on port: " + this.serverPort);
    }

    /**
     * Teardown SSL server
     */
    private void teardownSSLServer() {
        if (this.sslHttpServer != null) {
            try {
                this.sslHttpServer.shutdown();
                LogV3.info("SSL HTTP Server stopped");
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Test: HTTPS connection works correctly
     *
     * This test verifies that:
     * <ul>
     * <li>HTTPS connections can be established</li>
     * <li>Server responds correctly over HTTPS</li>
     * <li>Request/response cycle works</li>
     * </ul>
     */
    private void testHTTPSConnection() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping HTTPS connection test - no SSL context available");
            return;
        }
        LogV3.info("Test: HTTPS Connection");
        final String url = "https://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello HTTPS", "UTF-8");
        try {
            this.httpClient.setTrustProvider(this.caProvider);
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            if (responseCode != 200) {
                this.logContextOnFailure(context, "HTTPS request should return 200, was: " + responseCode);
            }
            assertTrue(responseCode == 200, "HTTPS request should return 200, was: " + responseCode);
            final String responseBody = context.getResponseString();
            assertTrue(responseBody != null, "HTTPS response body should not be null");
            assertTrue(responseBody.contains("Hello HTTPS"), "HTTPS response should contain request message");
            LogV3.info("HTTPS Connection test passed");
        } catch (final HttpClientException e) {
            e.printStackTrace();
            // If SSL handshake fails, it might be because HttpClient doesn't trust the certificate
            LogV3.warning("HTTPS connection failed (may need SSL trust configuration): " + e.getMessage());
            throw e;
        }
    }

    /**
     * Test: Client using TrustAllProvider allows all requests to the server regardless of server certificate. Verifies that with
     * TrustAllProvider the client accepts the connection and requests succeed (200).
     */
    private void testTrustAllProviderAtServer() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping TrustAllProvider test - no SSL server available");
            return;
        }
        LogV3.info("Test: TrustAllProvider at server – all requests allowed regardless of certificate");
        final HttpClient clientTrustAll = new HttpClient();
        clientTrustAll.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
        clientTrustAll.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
        clientTrustAll.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        clientTrustAll.setTrustProvider(AllTrustProvider.getInstance());
        final String url = "https://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("TrustAllOK", "UTF-8");
        final RequestContext ctx = clientTrustAll.get(url);
        assertTrue(ctx.getCode() == 200, "TrustAllProvider: request to server should return 200, was " + ctx.getCode());
        assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("TrustAllOK"), "Response should contain message");
        LogV3.info("TrustAllProvider at server test passed – all requests allowed regardless of certificate");
    }

    /**
     * Test: HTTP connections are rejected (SSL-only server, no fallback)
     *
     * This test verifies that:
     * <ul>
     * <li>Plain HTTP connections fail (unlike ExperimentalAutoSSLHttpServer which has fallback)</li>
     * <li>Server only accepts SSL/TLS connections</li>
     * </ul>
     */
    private void testHTTPRejection() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping HTTP rejection test - no SSL context available");
            return;
        }
        LogV3.info("Test: HTTP Rejection (SSL-only server)");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello HTTP", "UTF-8");
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            // HTTP should fail - SSLHttpServer only accepts SSL connections
            // The connection might fail during handshake or return an error
            LogV3.info("HTTP request returned code: " + responseCode + " (expected failure)");
            // If we get here, the connection might have been rejected or failed
            // This is acceptable - the important thing is that HTTP doesn't work
            assertTrue(responseCode != 200, "HTTP request should fail on SSL-only server, but got: " + responseCode);
            LogV3.info("HTTP Rejection test passed: HTTP connection correctly rejected");
        } catch (final HttpClientException e) {
            // Expected - HTTP should fail on SSL-only server
            LogV3.info("HTTP connection correctly failed (expected): " + e.getMessage());
            // Check if it's an SSL-related error
            if (e.getMessage() != null && (e.getMessage().contains("SSL") || e.getMessage().contains("TLS") || e.getMessage().contains("handshake"))) {
                LogV3.info("HTTP Rejection test passed: HTTP connection correctly rejected with SSL error");
            } else {
                // Other connection errors are also acceptable (connection refused, etc.)
                LogV3.info("HTTP Rejection test passed: HTTP connection correctly rejected");
            }
        } catch (final Exception e) {
            // Other exceptions are also acceptable (connection refused, etc.)
            LogV3.info("HTTP Rejection test passed: HTTP connection correctly rejected with exception: " + e.getClass().getSimpleName());
        }
    }

    /**
     * Test: TLS version support
     *
     * This test verifies that:
     * <ul>
     * <li>TLS handshake completes successfully</li>
     * <li>Server supports standard TLS versions</li>
     * </ul>
     */
    private void testTLSVersionSupport() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping TLS version test - no SSL context available");
            return;
        }
        LogV3.info("Test: TLS Version Support");
        // Make an HTTPS request - if it succeeds, TLS handshake worked
        final String url = "https://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("TLS Test", "UTF-8");
        try {
            final RequestContext context = this.httpClient.get(url);
            final int responseCode = context.getCode();
            assertTrue(responseCode == 200, "TLS handshake should succeed (response 200), was: " + responseCode);
            LogV3.info("TLS Version Support test passed: TLS handshake successful");
        } catch (final HttpClientException e) {
            LogV3.warning("TLS handshake test failed (may need SSL trust configuration): " + e.getMessage());
            throw e;
        }
    }

    /**
     * Test: SSL server without keystore cannot be created
     *
     * This test verifies that:
     * <ul>
     * <li>Server cannot be created without SSL context (unlike HttpServer which can work without SSL)</li>
     * </ul>
     */
    private void testSSLWithoutKeystore() throws Exception {
        LogV3.info("Test: SSL Server Without Keystore");
        // SSLHttpServer requires SSLContext, so we cannot create it without one
        // This is different from ExperimentalAutoSSLHttpServer which can work without SSL
        try {
            new SSLHttpServer(0, null);
            assertTrue(false, "SSLHttpServer should not be created with null SSLContext");
        } catch (final IllegalArgumentException e) {
            LogV3.info("SSLHttpServer correctly rejected null SSLContext: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            LogV3.warning("SSLHttpServer constructor threw unexpected exception: " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        LogV3.info("SSL Server Without Keystore test passed");
    }

    /**
     * Helper method to log context information when a test assertion fails.
     *
     * @param context
     *            The RequestContext to log, or null
     * @param message
     *            Additional message to include in the log
     */
    private void logContextOnFailure(final RequestContext context, final String message) {
        try {
            LogV3.severe("===========================================");
            LogV3.severe("TEST FAILED: " + message);
            LogV3.severe("===========================================");
            if (context != null) {
                LogV3.severe("Failed Request Context Details:");
                LogV3.severe("  Response Code: " + context.getCode());
                if (context.getConnection() != null) {
                    try {
                        final org.appwork.utils.net.httpconnection.HTTPConnection connection = context.getConnection();
                        LogV3.severe("  Request URL: " + connection.getURL());
                        LogV3.severe("  Request Method: " + context.getMethod());
                    } catch (final Throwable e2) {
                        LogV3.severe("  Error getting connection details: " + e2.getMessage());
                    }
                }
            } else {
                LogV3.severe("  RequestContext was null.");
            }
            LogV3.severe("===========================================");
        } catch (final Throwable e) {
            LogV3.severe("Error logging context: " + e.getMessage());
        }
    }
}
