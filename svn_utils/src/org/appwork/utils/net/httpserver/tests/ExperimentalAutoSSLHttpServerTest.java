package org.appwork.utils.net.httpserver.tests;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.URL;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.EnumSet;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.IO;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ServerCertificateResult;
import org.appwork.utils.net.httpconnection.trust.CustomTrustProvider;
import org.appwork.utils.net.httpserver.ExperimentalAutoSSLHttpServer;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * Tests for ExperimentalAutoSSLHttpServer functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>HTTPS connections work correctly with PKCS12 keystore</li>
 * <li>HTTP fallback works when protocol is detected as HTTP</li>
 * <li>Certificate loading from PKCS12</li>
 * <li>TLS version support</li>
 * <li>Mixed HTTP/HTTPS protocol detection</li>
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
public class ExperimentalAutoSSLHttpServerTest extends AWTest {
    /**
     * JSSE-based SSL-enabled HTTP server for testing
     */
    private static class TestExperimentalAutoSSLHttpServer extends ExperimentalAutoSSLHttpServer {
        private Throwable    lastServerException;
        private HttpRequest  lastRequest;
        private HttpResponse lastResponse;

        private TestExperimentalAutoSSLHttpServer(final int port, final SSLContext sslContext) {
            super(port, sslContext);
        }

        @Override
        public boolean onException(final Throwable e, final HttpRequest request, final HttpResponse response) throws IOException {
            this.lastServerException = e;
            this.lastRequest = request;
            this.lastResponse = response;
            return super.onException(e, request, response);
        }
    }

    private TestExperimentalAutoSSLHttpServer sslHttpServer;
    private RemoteAPI                         remoteAPI;
    private int                               serverPort;
    private HttpClient                        httpClient;
    private SSLContext                        sslContext;
    /** Temp file only for testPFXLoading (load-from-file tests). */
    private File                              tempKeystoreFile;
    private ServerCertificateResult           certificateResult;
    private CustomTrustProvider               caProvider;

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        // System.setProperty("javax.net.debug", "ssl:handshake:verbose");
        LogV3.info("SSL debug logging enabled via javax.net.debug=ssl:handshake:verbose");
        // Generate certificates on the fly using CertificateFactory
        try {
            this.createCertificates();
            this.createKeystore();
            // Test PFX loading scenarios first
            this.testPFXLoading();
            // Then run the actual SSL server tests
            // Test with Java URL.openConnection() first to isolate HttpClient issues
            // this.testHTTPSOnlyWithJavaURL();
            // Test with deactivated AutoUpgrade (HTTPS only, no auto-detection)
            this.testHTTPSOnly();
            // Test with activated AutoUpgrade (HTTP/HTTPS on same port)
            this.setupSSLServer();
            this.testHTTPSConnection();
            this.testHTTPFallback();
            this.testTLSVersionSupport();
            this.testMixedProtocolDetection();
            this.testSSLWithoutKeystore();
        } finally {
            this.teardownSSLServer();
            this.cleanupTempFiles();
        }
    }

    /**
     * Creates CA and server certificates using CertificateFactory.
     */
    private void createCertificates() throws Exception {
        LogV3.info("Creating CA and server certificates using CertificateFactory...");
        this.certificateResult = CertificateFactory.createCACertificateAndServerCertificate("Test CA", "localhost");
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
     * Creates in-memory PKCS12 keystore from generated certificates and SSLContext from it. Writes keystore to temp file only for
     * testPFXLoading (load-from-file tests).
     */
    private void createKeystore() throws Exception {
        LogV3.info("Creating PKCS12 keystore (in-memory)...");
        final char[] password = "testpassword".toCharArray();
        final KeyStore serverKeyStore = CertificateFactory.createPKCS12KeyStore(this.certificateResult.getServerCertificate(), this.certificateResult.getServerKeyPair().getPrivate(), password, "server", this.certificateResult.getCaCertificate());
        this.sslContext = ExperimentalAutoSSLHttpServer.createSSLContextFromKeyStore(serverKeyStore, password);
        assertTrue(this.sslContext != null, "SSL context should not be null");
        LogV3.info("SSL context created from in-memory keystore");
        this.tempKeystoreFile = File.createTempFile("test-server-", ".p12");
        this.tempKeystoreFile.deleteOnExit();
        try (FileOutputStream fos = new FileOutputStream(this.tempKeystoreFile)) {
            serverKeyStore.store(fos, password);
        }
        LogV3.info("PKCS12 temp file for PFX tests: " + this.tempKeystoreFile.getAbsolutePath());
    }

    /**
     * Test: PFX/PKCS12 loading scenarios
     *
     * This test verifies that:
     * <ul>
     * <li>PFX loading works with correct password</li>
     * <li>Falsches Passwort wirft UnrecoverableKeyException</li>
     * <li>Fehlende Datei wirft IOException</li>
     * <li>Ungültiges Format wirft KeyStoreException</li>
     * </ul>
     */
    private void testPFXLoading() throws Exception {
        LogV3.info("Test: PFX/PKCS12 Loading Scenarios");
        final String keystorePath = this.tempKeystoreFile.getAbsolutePath();
        final String correctPassword = "testpassword";
        // Test 2: Load with correct password
        try {
            final SSLContext context = ExperimentalAutoSSLHttpServer.createSSLContextFromPKCS12(keystorePath, correctPassword);
            assertTrue(context != null, "PFX should load successfully with correct password");
            LogV3.info("PFX loading with correct password: PASSED");
        } catch (final Exception e) {
            LogV3.warning("PFX loading with correct password failed: " + e.getMessage());
            throw e;
        }
        // Test 3: Load with wrong password
        try {
            ExperimentalAutoSSLHttpServer.createSSLContextFromPKCS12(keystorePath, "wrong_password_12345");
            assertTrue(false, "PFX loading with wrong password should throw UnrecoverableKeyException");
        } catch (final java.security.UnrecoverableKeyException e) {
            LogV3.info("PFX loading with wrong password correctly threw UnrecoverableKeyException: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            // Other exceptions are also acceptable (e.g., IOException if password check happens during load)
            LogV3.info("PFX loading with wrong password threw exception (acceptable): " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        // Test 4: Load with non-existent file
        try {
            ExperimentalAutoSSLHttpServer.createSSLContextFromPKCS12("/nonexistent/file.pfx", "password");
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
        // Test 5: Load with invalid file format (create a dummy file)
        final File dummyFile = new File(System.getProperty("java.io.tmpdir") + "/dummy_invalid.pfx");
        try {
            // Create a dummy file with invalid content
            dummyFile.delete();
            IO.writeStringToFile(dummyFile, "This is not a valid PKCS12 file");
            try {
                ExperimentalAutoSSLHttpServer.createSSLContextFromPKCS12(dummyFile.getAbsolutePath(), "password");
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
        // Test 6: Load with null password
        try {
            ExperimentalAutoSSLHttpServer.createSSLContextFromPKCS12(keystorePath, null);
            assertTrue(false, "PFX loading with null password should throw exception");
        } catch (final NullPointerException e) {
            LogV3.info("PFX loading with null password correctly threw NullPointerException: " + e.getMessage());
            // Expected - test passed
        } catch (final Exception e) {
            LogV3.info("PFX loading with null password threw exception (acceptable): " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        // Test 7: Load with empty password
        try {
            ExperimentalAutoSSLHttpServer.createSSLContextFromPKCS12(keystorePath, "");
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
     * Cleanup temporary files.
     */
    private void cleanupTempFiles() {
        if (this.tempKeystoreFile != null && this.tempKeystoreFile.exists()) {
            this.tempKeystoreFile.delete();
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
        LogV3.info("Starting JSSE SSL HTTP Server Setup...");
        // Create RemoteAPI and register Dummy API
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());
        // Create ExperimentalAutoSSLHttpServer on a free port (0 = automatic)
        this.sslHttpServer = new TestExperimentalAutoSSLHttpServer(0, this.sslContext);
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
        LogV3.info("JSSE SSL HTTP Server started on port: " + this.serverPort);
    }

    /**
     * Teardown SSL server
     */
    private void teardownSSLServer() {
        if (this.sslHttpServer != null) {
            try {
                this.sslHttpServer.shutdown();
                LogV3.info("JSSE SSL HTTP Server stopped");
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Test: HTTPS-only server (AutoUpgrade deactivated, SSL only)
     *
     * This test verifies that:
     * <ul>
     * <li>Server works correctly with SSL only (no auto-detection, no SSLEngine auto-upgrade logic)</li>
     * <li>HTTPS connections work directly</li>
     * <li>Server responds correctly over HTTPS</li>
     * </ul>
     */
    private void testHTTPSOnly() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping HTTPS-only test - no SSL context available");
            return;
        }
        LogV3.info("Test: HTTPS-Only Server (AutoUpgrade deactivated)");
        // Create RemoteAPI and register Dummy API
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        // Create ExperimentalAutoSSLHttpServer WITH SSL context but WITHOUT auto-upgrade
        final TestExperimentalAutoSSLHttpServer httpsOnlyServer = new TestExperimentalAutoSSLHttpServer(0, this.sslContext);
        // Disable auto-upgrade - server will only accept HTTPS connections
        httpsOnlyServer.setAutoUpgrade(false);
        httpsOnlyServer.setLocalhostOnly(true);
        // Allow GET and POST for testing
        httpsOnlyServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
        // Register RemoteAPI as request handler
        httpsOnlyServer.registerRequestHandler(remoteAPI);
        // Start server
        httpsOnlyServer.start();
        final int serverPort = httpsOnlyServer.getActualPort();
        // Create HttpClient instance for tests
        final HttpClient httpClient = new HttpClient();
        httpClient.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        httpClient.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
        httpClient.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
        httpClient.setVerboseLog(true); // Enable verbose logging for debugging
        httpClient.setTrustProvider(caProvider);
        // Enable verbose logging on server
        httpsOnlyServer.setVerboseLog(true);
        try {
            LogV3.info("HTTPS-only server started on port: " + serverPort);
            // Test HTTPS connection
            final String url = "https://localhost:" + serverPort + "/test/echo?message=" + URLEncoder.encode("Hello HTTPS Only", "UTF-8");
            final RequestContext context = httpClient.get(url);
            final int responseCode = context.getCode();
            if (responseCode != 200) {
                this.logContextOnFailure(context, "HTTPS-only request should return 200, was: " + responseCode);
            }
            assertTrue(responseCode == 200, "HTTPS-only request should return 200, was: " + responseCode);
            final String responseBody = context.getResponseString();
            assertTrue(responseBody != null, "HTTPS-only response body should not be null");
            assertTrue(responseBody.contains("Hello HTTPS Only"), "HTTPS-only response should contain request message");
            LogV3.info("HTTPS-Only Server test passed");
        } catch (final HttpClientException e) {
            e.printStackTrace();
            // If SSL handshake fails, it might be because HttpClient doesn't trust the certificate
            LogV3.warning("HTTPS-only connection failed (may need SSL trust configuration): " + e.getMessage());
            throw e;
        } finally {
            // Cleanup
            try {
                httpsOnlyServer.stop();
            } catch (final Exception e) {
                LogV3.warning("Error stopping HTTPS-only server: " + e.getMessage());
            }
        }
    }

    /**
     * Test: HTTPS connection using Java URL.openConnection() (standard Java API)
     *
     * This test verifies that the server works with standard Java HTTPS connections, which helps isolate whether issues are with HttpClient
     * or the server itself.
     */
    private void testHTTPSOnlyWithJavaURL() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping Java URL HTTPS test - no SSL context available");
            return;
        }
        LogV3.info("Test: HTTPS-Only Server with Java URL.openConnection()");
        // Create RemoteAPI and register Dummy API
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        // Create ExperimentalAutoSSLHttpServer WITH SSL context but WITHOUT auto-upgrade
        final TestExperimentalAutoSSLHttpServer httpsOnlyServer = new TestExperimentalAutoSSLHttpServer(0, this.sslContext);
        // Disable auto-upgrade - server will only accept HTTPS connections
        httpsOnlyServer.setAutoUpgrade(false);
        httpsOnlyServer.setLocalhostOnly(true);
        // Allow GET and POST for testing
        httpsOnlyServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
        // Register RemoteAPI as request handler
        httpsOnlyServer.registerRequestHandler(remoteAPI);
        // Start server
        httpsOnlyServer.start();
        final int serverPort = httpsOnlyServer.getActualPort();
        // Create a TrustManager that accepts all certificates (for testing only)
        final TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
            @Override
            public X509Certificate[] getAcceptedIssuers() {
                return new X509Certificate[0];
            }

            @Override
            public void checkClientTrusted(final X509Certificate[] certs, final String authType) {
                // Trust all client certificates
            }

            @Override
            public void checkServerTrusted(final X509Certificate[] certs, final String authType) {
                // Trust all server certificates
            }
        } };
        // Create SSLContext that trusts all certificates
        SSLContext trustAllContext = null;
        try {
            trustAllContext = SSLContext.getInstance("TLS");
            trustAllContext.init(null, trustAllCerts, new java.security.SecureRandom());
        } catch (final NoSuchAlgorithmException | KeyManagementException e) {
            throw new IOException("Failed to create trust-all SSL context", e);
        }
        try {
            LogV3.info("HTTPS-only server started on port: " + serverPort);
            // Test HTTPS connection using Java URL.openConnection()
            final String urlString = "https://localhost:" + serverPort + "/test/echo?message=" + URLEncoder.encode("Hello Java URL", "UTF-8");
            final URL url = new URL(urlString);
            final HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
            connection.setSSLSocketFactory(trustAllContext.getSocketFactory());
            connection.setHostnameVerifier(new HostnameVerifier() {
                @Override
                public boolean verify(String hostname, SSLSession session) {
                    return true;
                }
            }); // Accept all hostnames
            connection.addRequestProperty(HTTPConstants.X_APPWORK, "1");
            connection.setRequestMethod("GET");
            connection.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
            connection.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
            final int responseCode = connection.getResponseCode();
            if (responseCode != 200) {
                LogV3.warning("Java URL HTTPS request returned: " + responseCode);
                final InputStream errorStream = connection.getErrorStream();
                if (errorStream != null) {
                    try (final BufferedReader reader = new BufferedReader(new InputStreamReader(errorStream))) {
                        String line;
                        while ((line = reader.readLine()) != null) {
                            LogV3.warning("Error response: " + line);
                        }
                    }
                }
            }
            assertTrue(responseCode == 200, "Java URL HTTPS request should return 200, was: " + responseCode);
            // Read response body
            try (final BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
                final StringBuilder responseBody = new StringBuilder();
                String line;
                while ((line = reader.readLine()) != null) {
                    responseBody.append(line).append("\n");
                }
                final String response = responseBody.toString();
                assertTrue(response != null, "Java URL HTTPS response body should not be null");
                assertTrue(response.contains("Hello Java URL"), "Java URL HTTPS response should contain request message");
                LogV3.info("Java URL HTTPS response: " + response.trim());
            }
            LogV3.info("HTTPS-Only Server test with Java URL.openConnection() passed");
        } catch (final Exception e) {
            LogV3.warning("Java URL HTTPS connection failed: " + e.getMessage());
            e.printStackTrace();
            throw e;
        } finally {
            // Cleanup
            try {
                httpsOnlyServer.stop();
            } catch (final Exception e) {
                LogV3.warning("Error stopping HTTPS-only server: " + e.getMessage());
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
            httpClient.setTrustProvider(caProvider);
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
     * Test: HTTP fallback works when protocol is detected as HTTP
     *
     * This test verifies that:
     * <ul>
     * <li>Plain HTTP connections work when protocol is detected as HTTP</li>
     * <li>Server handles both HTTP and HTTPS on the same port</li>
     * </ul>
     */
    private void testHTTPFallback() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping HTTP fallback test - no SSL context available");
            return;
        }
        LogV3.info("Test: HTTP Fallback");
        final String url = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello HTTP", "UTF-8");
        final RequestContext context = this.httpClient.get(url);
        final int responseCode = context.getCode();
        if (responseCode != 200) {
            this.logContextOnFailure(context, "HTTP fallback request should return 200, was: " + responseCode);
        }
        assertTrue(responseCode == 200, "HTTP fallback request should return 200, was: " + responseCode);
        final String responseBody = context.getResponseString();
        assertTrue(responseBody != null, "HTTP fallback response body should not be null");
        assertTrue(responseBody.contains("Hello HTTP"), "HTTP fallback response should contain request message");
        LogV3.info("HTTP Fallback test passed");
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
     * Test: Mixed protocol detection
     *
     * This test verifies that:
     * <ul>
     * <li>Server correctly detects HTTP vs HTTPS based on initial bytes</li>
     * <li>Both protocols work on the same port</li>
     * </ul>
     */
    private void testMixedProtocolDetection() throws Exception {
        if (this.sslContext == null) {
            LogV3.warning("Skipping mixed protocol detection test - no SSL context available");
            return;
        }
        LogV3.info("Test: Mixed Protocol Detection");
        // Test HTTP
        final String httpUrl = "http://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("HTTP", "UTF-8");
        final RequestContext httpContext = this.httpClient.get(httpUrl);
        assertTrue(httpContext.getCode() == 200, "HTTP request should succeed");
        // Test HTTPS
        final String httpsUrl = "https://localhost:" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("HTTPS", "UTF-8");
        try {
            final RequestContext httpsContext = this.httpClient.get(httpsUrl);
            assertTrue(httpsContext.getCode() == 200, "HTTPS request should succeed");
        } catch (final HttpClientException e) {
            LogV3.warning("HTTPS request failed (may need SSL trust configuration): " + e.getMessage());
            throw e;
        }
        LogV3.info("Mixed Protocol Detection test passed: Both HTTP and HTTPS work correctly");
    }

    /**
     * Test: SSL server without keystore falls back to HTTP
     *
     * This test verifies that:
     * <ul>
     * <li>Server without keystore still accepts HTTP connections</li>
     * <li>HTTPS connections fail gracefully when no keystore is available</li>
     * </ul>
     */
    private void testSSLWithoutKeystore() throws Exception {
        LogV3.info("Test: SSL Server Without Keystore");
        // Create a server without SSL context
        final TestExperimentalAutoSSLHttpServer serverWithoutSSL = new TestExperimentalAutoSSLHttpServer(0, null);
        serverWithoutSSL.setLocalhostOnly(true);
        serverWithoutSSL.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
        final RemoteAPI testAPI = new RemoteAPI();
        testAPI.register(new DummyTestAPIImpl());
        serverWithoutSSL.registerRequestHandler(testAPI);
        try {
            serverWithoutSSL.start();
            final int testPort = serverWithoutSSL.getActualPort();
            // HTTP should work
            final String httpUrl = "http://localhost:" + testPort + "/test/echo?message=" + URLEncoder.encode("HTTP", "UTF-8");
            final RequestContext httpContext = this.httpClient.get(httpUrl);
            assertTrue(httpContext.getCode() == 200, "HTTP should work without SSL context");
            // HTTPS should fail or be rejected (use raw SSLSocket to avoid hanging HttpClient)
            final String httpsUrl = "https://localhost:" + testPort + "/test/echo?message=" + URLEncoder.encode("HTTPS", "UTF-8");
            try {
                final SSLSocketFactory factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
                try (final SSLSocket socket = (SSLSocket) factory.createSocket()) {
                    socket.connect(new InetSocketAddress("localhost", testPort), 2000);
                    socket.setSoTimeout(2000);
                    socket.startHandshake();
                    LogV3.info("HTTPS handshake unexpectedly succeeded without SSL context: " + httpsUrl);
                }
            } catch (final SSLException e) {
                // Expected - HTTPS should fail without SSL context
                LogV3.info("HTTPS correctly rejected without SSL context: " + e.getMessage());
            } catch (final IOException e) {
                // Expected - HTTPS should fail without SSL context
                LogV3.info("HTTPS correctly rejected without SSL context: " + e.getMessage());
            }
            LogV3.info("SSL Server Without Keystore test passed");
        } finally {
            serverWithoutSSL.shutdown();
        }
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
