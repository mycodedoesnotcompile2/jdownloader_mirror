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

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.List;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.NativeHTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.SSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptions;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ServerCertificateResult;
import org.appwork.utils.net.httpserver.ExperimentalAutoSSLHttpServer;

/**
 * Base class for SSL Trust Provider tests providing common setup and helper methods.
 */
public abstract class SSLTrustProviderTestBase extends AWTest {
    /**
     *
     */
    public static final String  APP_WORK_AW_TEST_CA = "AppWork AWTest CA";
    /** OID id-kp-serverAuth (Erweiterte Schlüsselverwendung / Zweck) */
    private static final String EKU_SERVER_AUTH     = "1.3.6.1.5.5.7.3.1";
    /** OID id-kp-clientAuth (Erweiterte Schlüsselverwendung / Zweck) */
    private static final String EKU_CLIENT_AUTH     = "1.3.6.1.5.5.7.3.2";
    protected File              tempKeystoreFile;
    protected SSLContext        sslContext;
    protected X509Certificate   serverCertificate;
    protected X509Certificate   caCertificate;
    protected String            caCertificateFingerPrint;

    /**
     * Creates test certificates using CertificateFactory.
     */
    protected void createTestCertificates() throws Exception {
        LogV3.info("Creating test certificates using CertificateFactory...");
        final ServerCertificateResult certResult = CertificateFactory.createCACertificateAndServerCertificate(APP_WORK_AW_TEST_CA, "localhost");
        assertNotNull(certResult, "Certificate creation should succeed");
        assertNotNull(certResult.getCaCertificate(), "CA certificate should not be null");
        assertNotNull(certResult.getServerCertificate(), "Server certificate should not be null");
        assertNotNull(certResult.getServerKeyPair(), "Server key pair should not be null");
        this.caCertificate = certResult.getCaCertificate();
        this.serverCertificate = certResult.getServerCertificate();
        // Calculate CA certificate fingerprint
        final MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(caCertificate.getEncoded());
        final byte[] thumbprintBytes = md.digest();
        final StringBuilder thumbprint = new StringBuilder();
        for (final byte b : thumbprintBytes) {
            thumbprint.append(String.format("%02X", b));
        }
        this.caCertificateFingerPrint = thumbprint.toString();
        // Verify CA certificate has Extended Key Usage (Zweck): serverAuth, clientAuth
        final List<String> caEku = this.caCertificate.getExtendedKeyUsage();
        assertNotNull(caEku, "CA certificate should have Extended Key Usage extension");
        assertTrue(caEku.contains(EKU_SERVER_AUTH), "CA certificate should contain id-kp-serverAuth, got: " + caEku);
        assertTrue(caEku.contains(EKU_CLIENT_AUTH), "CA certificate should contain id-kp-clientAuth, got: " + caEku);
        // Create in-memory PKCS12 keystore and SSL context; write to temp file only for tests that need file path (e.g. CustomTrustProvider
        // from keystore)
        final char[] password = "testpassword".toCharArray();
        final KeyStore serverKs = CertificateFactory.createPKCS12KeyStore(this.serverCertificate, certResult.getServerKeyPair().getPrivate(), password, "server", this.caCertificate);
        this.sslContext = ExperimentalAutoSSLHttpServer.createSSLContextFromKeyStore(serverKs, password);
        assertNotNull(this.sslContext, "SSL context should not be null");
        this.tempKeystoreFile = File.createTempFile("test-ssl-trust-", ".p12");
        this.tempKeystoreFile.deleteOnExit();
        try (FileOutputStream fos = new FileOutputStream(this.tempKeystoreFile)) {
            serverKs.store(fos, password);
        }
        LogV3.info("Test certificates created successfully (in-memory keystore; temp file for provider tests)");
    }

    /**
     * Cleanup temporary files.
     */
    protected void cleanupTempFiles() {
        if (this.tempKeystoreFile != null && this.tempKeystoreFile.exists()) {
            this.tempKeystoreFile.delete();
        }
    }

    /**
     * @param protocol
     * @return
     */
    protected HttpClient getClientWithSSLProtocol(final String protocol) {
        HttpClient clientTls10 = new HttpClient() {
            protected HTTPConnection createHTTPConnection(URL url, org.appwork.utils.net.httpconnection.HTTPProxy proxy) {
                return new HTTPConnectionImpl(url, proxy) {
                    /**
                     * @see org.appwork.utils.net.httpconnection.HTTPConnectionImpl#getSSLSocketStreamFactory(org.appwork.utils.net.httpconnection.SSLSocketStreamOptions)
                     */
                    @Override
                    protected SSLSocketStreamFactory getSSLSocketStreamFactory(SSLSocketStreamOptions sslSocketStreamOptions) {
                        return new JavaSSLSocketStreamFactory() {
                            /**
                             * @see org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory#filterEnabledSupportedProtocols(org.appwork.utils.net.httpconnection.SSLSocketStreamOptions,
                             *      javax.net.ssl.SSLContext, java.lang.String[])
                             */
                            @Override
                            protected String[] filterEnabledSupportedProtocols(SSLSocketStreamOptions options, SSLContext sslContext, String... onlyEnabledProtocols) {
                                LogV3.info("Filtered: " + protocol);
                                return new String[] { protocol };
                            }

                            @Override
                            protected SSLContext createContext(List<String> protocols) throws NoSuchAlgorithmException {
                                LogV3.info("Context: " + protocol);
                                return SSLContext.getInstance(protocol);
                            }
                        };
                    }
                };
            }
        };
        return clientTls10;
    }

    /**
     * Opens input stream via native Java URL path (NativeHTTPConnectionImpl → URL.openConnection().getInputStream()) with the given
     * TrustProvider.
     */
    protected InputStream openStreamWithTrustProvider(final String urlString, final org.appwork.utils.net.httpconnection.trust.TrustProviderInterface provider) throws Exception {
        final URL url = new URL(urlString);
        final HTTPConnection conn = new NativeHTTPConnectionImpl(url, null);
        conn.setTrustProvider(provider);
        conn.setConnectTimeout(5000);
        conn.setReadTimeout(5000);
        conn.connect();
        return conn.getInputStream();
    }

    public static class TestHTTPServer extends ExperimentalAutoSSLHttpServer {
        public TestHTTPServer(final int port, final SSLContext sslContext) {
            super(port, sslContext);
        }
    }

    protected static class TestSSLSocketStreamFactory extends JavaSSLSocketStreamFactory {
        public javax.net.ssl.TrustManager[] getTrustManagersForProvider(final org.appwork.utils.net.httpconnection.trust.TrustProviderInterface provider) throws Exception {
            return new javax.net.ssl.TrustManager[] { generateTrustManagerDelegate(new org.appwork.utils.net.httpconnection.trust.TrustCallback() {
                @Override
                public void onTrustResult(org.appwork.utils.net.httpconnection.trust.TrustProviderInterface provider, X509Certificate[] chain, String authType, org.appwork.utils.net.httpconnection.TrustResult result) {
                }

                @Override
                public org.appwork.utils.net.httpconnection.trust.TrustProviderInterface getTrustProvider() {
                    return provider;
                }

                @Override
                public KeyManager[] getKeyManager() {
                    return null;
                }
            }) };
        }
    }
}
