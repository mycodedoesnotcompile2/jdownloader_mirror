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
import java.net.URLEncoder;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLContext;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ServerCertificateResult;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpconnection.trust.CustomTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustAllProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpserver.ExperimentalAutoSSLHttpServer;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

/**
 * Tests for CertificateFactory functionality.
 *
 * Important changes vs your version: - Avoid reusing a HttpClient that was switched to TrustAll (prevents false-green tests). - Actually
 * verify SAN values via X509Certificate.getSubjectAlternativeNames(). - Keep a dedicated client for TrustAll and another dedicated client
 * for CA-trust.
 */
public class CertificateFactoryTest extends AWTest {
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
    private SSLContext                        sslContext;
    private KeyStore                          serverKeyStore;
    private ServerCertificateResult           certificateResult;
    private CustomTrustProvider               caProvider;

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            createCertificates();
            createKeystore();
            createSSLContext();
            setupSSLServer();
            // 1) Sanity: TLS works at all (TrustAll)
            testHTTPSConnectionTrustAll("localhost");
            testHTTPSConnectionTrustAll("127.0.0.1");
            testHTTPSConnectionTrustAll("[::1]"); // may be skipped if no IPv6
            // 2) Real test: CA trust (no TrustAll)
            testHTTPSConnectionWithCustomTrustProvider("localhost");
            testHTTPSConnectionWithCustomTrustProvider("127.0.0.1");
            testHTTPSConnectionWithCustomTrustProvider("[::1]"); // may be skipped if no IPv6
            // 3) Real test: SAN content
            testCertificateSubjectAlternativeNames();
        } finally {
            teardownSSLServer();
            cleanupTempFiles();
        }
    }

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

    private void createKeystore() throws Exception {
        LogV3.info("Creating PKCS12 keystore (in-memory)...");
        final char[] password = "testpassword".toCharArray();
        this.serverKeyStore = CertificateFactory.createPKCS12KeyStore(this.certificateResult.getServerCertificate(), this.certificateResult.getServerKeyPair().getPrivate(), password, "server", this.certificateResult.getCaCertificate());
        LogV3.info("PKCS12 keystore created");
    }

    private void createSSLContext() throws Exception {
        LogV3.info("Creating SSL context from in-memory keystore...");
        final char[] password = "testpassword".toCharArray();
        final java.util.Enumeration<String> aliases = this.serverKeyStore.aliases();
        while (aliases.hasMoreElements()) {
            final String alias = aliases.nextElement();
            if (this.serverKeyStore.isKeyEntry(alias)) {
                final java.security.cert.Certificate[] chain = this.serverKeyStore.getCertificateChain(alias);
                if (chain != null) {
                    LogV3.info("Keystore contains certificate chain with " + chain.length + " certificates for alias: " + alias);
                    for (int i = 0; i < chain.length; i++) {
                        if (chain[i] instanceof X509Certificate) {
                            final X509Certificate cert = (X509Certificate) chain[i];
                            LogV3.info("  Chain[" + i + "]: " + cert.getSubjectX500Principal().getName());
                        }
                    }
                }
            }
        }
        this.sslContext = ExperimentalAutoSSLHttpServer.createSSLContextFromKeyStore(this.serverKeyStore, password);
        assertTrue(this.sslContext != null, "SSL context should not be null");
        LogV3.info("SSL context created successfully");
    }

    private void setupSSLServer() throws ParseException, IOException {
        if (this.sslContext == null) {
            LogV3.warning("Skipping SSL server setup - no SSL context available");
            return;
        }
        LogV3.info("Starting SSL HTTP Server Setup...");
        this.remoteAPI = new RemoteAPI();
        this.remoteAPI.register(new DummyTestAPIImpl());
        this.sslHttpServer = new TestExperimentalAutoSSLHttpServer(0, this.sslContext);
        this.sslHttpServer.setAutoUpgrade(false); // HTTPS only
        this.sslHttpServer.setLocalhostOnly(true);
        this.sslHttpServer.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
        this.sslHttpServer.registerRequestHandler(this.remoteAPI);
        this.sslHttpServer.start();
        this.serverPort = this.sslHttpServer.getActualPort();
        LogV3.info("SSL HTTP Server started on port: " + this.serverPort);
    }

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

    private void cleanupTempFiles() {
        // No temp files: keystore is in-memory only
    }
    // =========================================================================================
    // Tests
    // =========================================================================================

    private HttpClient newClient(final TrustProviderInterface trustProvider) {
        final HttpClient c = new HttpClient();
        c.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
        c.setReadTimeout((int) TimeUnit.SECONDS.toMillis(30));
        c.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        // Prefer direct API if present
        try {
            c.getClass().getMethod("setTrustProvider", TrustProviderInterface.class).invoke(c, trustProvider);
        } catch (final Exception e) {
            // Fallback
            c.trust(trustProvider);
        }
        return c;
    }

    private void testHTTPSConnectionTrustAll(final String host) throws Exception {
        if (this.sslContext == null) {
            return;
        }
        final HttpClient client = newClient(TrustAllProvider.getInstance());
        final String url = "https://" + host + ":" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello " + host, "UTF-8");
        LogV3.info("Test: HTTPS Connection (TrustAll) -> " + host);
        try {
            final RequestContext context = client.get(url);
            final int code = context.getCode();
            if (code != 200) {
                logContextOnFailure(context, "HTTPS TrustAll request should return 200, was: " + code);
            }
            assertTrue(code == 200, "HTTPS TrustAll request should return 200, was: " + code);
            final String responseBody = context.getResponseString();
            assertTrue(responseBody != null, "HTTPS response body should not be null");
            assertTrue(responseBody.contains("Hello " + host), "HTTPS response should contain request message");
        } catch (final HttpClientException e) {
            // IPv6 may not be available on all systems
            if ("[::1]".equals(host)) {
                LogV3.info("IPv6 TrustAll failed (IPv6 might not be available): " + e.getMessage());
                return;
            }
            throw e;
        }
    }

    private void testHTTPSConnectionWithCustomTrustProvider(final String host) throws Exception {
        if (this.sslContext == null) {
            return;
        }
        final HttpClient client = newClient(this.caProvider);
        final String url = "https://" + host + ":" + this.serverPort + "/test/echo?message=" + URLEncoder.encode("Hello CA " + host, "UTF-8");
        LogV3.info("Test: HTTPS Connection (CA trust) -> " + host);
        // Ensure server cert is signed by CA (sanity)
        this.certificateResult.getServerCertificate().verify(this.certificateResult.getCaCertificate().getPublicKey());
        try {
            final RequestContext context = client.get(url);
            final int code = context.getCode();
            if (code != 200) {
                logContextOnFailure(context, "HTTPS CA request should return 200, was: " + code);
            }
            assertTrue(code == 200, "HTTPS CA request should return 200, was: " + code);
            final String responseBody = context.getResponseString();
            assertTrue(responseBody != null, "HTTPS response body should not be null");
            assertTrue(responseBody.contains("Hello CA " + host), "HTTPS response should contain request message");
            // If your HttpClient exposes TrustResult, validate it's trusted
            if (context.getTrustResult() != null) {
                assertTrue(context.getTrustResult().isTrusted(), "Server certificate should be trusted via CA");
            }
        } catch (final HttpClientException e) {
            if ("[::1]".equals(host)) {
                LogV3.info("IPv6 CA-trust failed (IPv6 might not be available): " + e.getMessage());
                return;
            }
            throw e;
        }
    }

    private void testCertificateSubjectAlternativeNames() throws Exception {
        LogV3.info("Test: Certificate Subject Alternative Names (parse via JDK API)");
        final X509Certificate serverCert = this.certificateResult.getServerCertificate();
        assertTrue(serverCert != null, "Server certificate should not be null");
        final Collection<List<?>> sans = serverCert.getSubjectAlternativeNames();
        assertTrue(sans != null, "SAN extension must exist");
        // Extract SANs into sets for easy assertions
        final Set<String> dnsNames = new HashSet<String>();
        final Set<String> ipNames = new HashSet<String>();
        for (final List<?> san : sans) {
            if (san == null || san.size() < 2) {
                continue;
            }
            final Integer type = (Integer) san.get(0);
            final Object value = san.get(1);
            // 2 = dNSName, 7 = iPAddress
            if (type != null && type.intValue() == 2 && value instanceof String) {
                dnsNames.add(((String) value).toLowerCase());
            } else if (type != null && type.intValue() == 7) {
                // value can be String in many JDKs; sometimes byte[] depending on provider
                if (value instanceof String) {
                    ipNames.add((String) value);
                } else if (value instanceof byte[]) {
                    ipNames.add(bytesToIpString((byte[]) value));
                }
            }
        }
        assertTrue(dnsNames.contains("localhost"), "SAN must contain DNS localhost");
        assertTrue(ipNames.contains("127.0.0.1"), "SAN must contain IP 127.0.0.1");
        assertTrue(ipNames.contains("::1") || ipNames.contains("0:0:0:0:0:0:0:1"), "SAN must contain IP ::1 or 0:0:0:0:0:0:0:1");
        LogV3.info("SANs OK: DNS=" + dnsNames + " IP=" + ipNames);
    }

    private static String bytesToIpString(final byte[] ipBytes) throws Exception {
        // Convert raw IP bytes to textual representation
        return java.net.InetAddress.getByAddress(ipBytes).getHostAddress();
    }
    // =========================================================================================
    // Logging helper
    // =========================================================================================

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
                if (context.getTrustResult() != null) {
                    LogV3.severe("  Trust Result: " + context.getTrustResult().isTrusted());
                    if (context.getTrustResult().getException() != null) {
                        LogV3.severe("  Trust Exception: " + context.getTrustResult().getException().getMessage());
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
