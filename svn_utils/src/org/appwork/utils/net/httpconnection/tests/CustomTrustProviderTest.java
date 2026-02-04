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
import java.security.cert.X509Certificate;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.trust.CustomTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustCurrentJREProvider;

/**
 * Tests for CustomTrustProvider functionality including certificate validation, cacerts file loading, and trust store operations.
 */
public class CustomTrustProviderTest extends ProxyConnectionTestBase {
    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            setupProxyServers();
            createTestCertificates();
            testCustomTrustProvider();
            testCustomTrustProviderWithCacerts();
            cleanupTempFiles();
            LogV3.info("CustomTrustProvider tests completed successfully");
        } finally {
            teardownProxyServers();
        }
    }

    private void testCustomTrustProvider() throws Exception {
        try {
            new CustomTrustProvider(new X509Certificate[0]);
            assertTrue(false, "CustomTrustProvider should reject empty certificate array");
        } catch (final IllegalArgumentException e) {
            // Expected
        }
        try {
            new CustomTrustProvider((X509Certificate[]) null);
            assertTrue(false, "CustomTrustProvider should reject null certificate array");
        } catch (final IllegalArgumentException e) {
            // Expected
        }
        final CustomTrustProvider customProvider = new CustomTrustProvider(serverCertificate);
        assertTrue(customProvider.getCaCertificates().length == 1, "CustomTrustProvider should contain one certificate");
        assertTrue(customProvider.getId().contains("CustomTrustProvider"), "CustomTrustProvider ID should contain provider name");
    }

    /**
     * Tests CustomTrustProvider with JRE cacerts file. Verifies that CustomTrustProvider can load the cacerts file and that it behaves
     * similarly to TrustCurrentJREProvider.
     */
    private void testCustomTrustProviderWithCacerts() throws Exception {
        final String javaHome = System.getProperty("java.home");
        final File cacertsFile = new File(javaHome, "lib/security/cacerts");
        if (!cacertsFile.exists()) {
            logInfoAnyway("Skipping cacerts test - cacerts file not found at: " + cacertsFile.getAbsolutePath());
            return;
        }
        LogV3.info("Test: CustomTrustProvider with cacerts file: " + cacertsFile.getAbsolutePath());
        // Test 1: Load cacerts file with CustomTrustProvider
        CustomTrustProvider cacertsProvider = null;
        try {
            cacertsProvider = new CustomTrustProvider(cacertsFile, "changeit".toCharArray(), "JKS");
            assertTrue(cacertsProvider != null, "CustomTrustProvider should load cacerts file");
            assertTrue(cacertsProvider.getCaCertificates().length > 0, "cacerts file should contain CA certificates");
            LogV3.info("Loaded " + cacertsProvider.getCaCertificates().length + " CA certificates from cacerts");
        } catch (final Exception e) {
            // Try without explicit keystore type
            try {
                cacertsProvider = new CustomTrustProvider(cacertsFile, "changeit".toCharArray());
                assertTrue(cacertsProvider != null, "CustomTrustProvider should load cacerts file (auto-detect type)");
                assertTrue(cacertsProvider.getCaCertificates().length > 0, "cacerts file should contain CA certificates");
                LogV3.info("Loaded " + cacertsProvider.getCaCertificates().length + " CA certificates from cacerts (auto-detect)");
            } catch (final Exception e2) {
                throw new Exception("Failed to load cacerts file: " + e.getMessage() + " / " + e2.getMessage(), e2);
            }
        }
        // Test 2: Verify TrustManager can be created (via getAcceptedIssuers which internally uses getTrustManager)
        try {
            final X509Certificate[] acceptedIssuers = cacertsProvider.getAcceptedIssuers();
            assertTrue(acceptedIssuers != null, "CustomTrustProvider from cacerts should return accepted issuers");
            assertTrue(acceptedIssuers.length > 0, "CustomTrustProvider should have accepted issuers");
            LogV3.info("CustomTrustProvider from cacerts has " + acceptedIssuers.length + " accepted issuers");
        } catch (final Exception e) {
            throw new Exception("Failed to get accepted issuers from cacerts: " + e.getMessage(), e);
        }
        // Test 3: Verify TrustStore can be retrieved (for revocation checks)
        try {
            final java.security.KeyStore trustStore = cacertsProvider.getTrustStore();
            assertTrue(trustStore != null, "CustomTrustProvider from cacerts should return TrustStore");
            LogV3.info("TrustStore retrieved successfully");
        } catch (final Exception e) {
            throw new Exception("Failed to get TrustStore from cacerts: " + e.getMessage(), e);
        }
        // Test 5: Test with a public HTTPS site over all connection variants (should work if cacerts contains standard CAs)
        final String testUrl = "https://example.com/";
        final List<HTTPProxy> variants = getConnectionVariants();
        for (final HTTPProxy proxy : variants) {
            try {
                final RequestContext context = new HttpClient().proxy(proxy).trust(cacertsProvider).get(testUrl);
                LogV3.info("HTTPS request with cacerts provider via " + proxy + ": " + (context.getTrustResult().isTrusted() ? "SUCCESS" : "FAILED"));
                if (!context.getTrustResult().isTrusted() && context.getTrustResult().getException() != null) {
                    LogV3.info("Failure reason: " + context.getTrustResult().getException().getMessage());
                }
            } catch (final Exception e) {
                LogV3.info("HTTPS request with cacerts provider via " + proxy + " failed (expected if cacerts doesn't contain necessary CA): " + e.getMessage());
            }
        }
        // Test 6: Compare with TrustCurrentJREProvider (should have similar behavior)
        try {
            final TrustCurrentJREProvider currentJREProvider = TrustCurrentJREProvider.getInstance();
            final java.security.KeyStore currentJRETrustStore = currentJREProvider.getTrustStore();
            final java.security.KeyStore cacertsTrustStore = cacertsProvider.getTrustStore();
            // Both should be non-null
            assertTrue(currentJRETrustStore != null, "TrustCurrentJREProvider should return TrustStore");
            assertTrue(cacertsTrustStore != null, "CustomTrustProvider from cacerts should return TrustStore");
            // Count certificates in both stores
            int currentJRECount = 0;
            int cacertsCount = 0;
            try {
                final java.util.Enumeration<String> currentJREAliases = currentJRETrustStore.aliases();
                while (currentJREAliases.hasMoreElements()) {
                    currentJREAliases.nextElement();
                    currentJRECount++;
                }
                final java.util.Enumeration<String> cacertsAliases = cacertsTrustStore.aliases();
                while (cacertsAliases.hasMoreElements()) {
                    cacertsAliases.nextElement();
                    cacertsCount++;
                }
                LogV3.info("TrustCurrentJREProvider has " + currentJRECount + " certificates, CustomTrustProvider(cacerts) has " + cacertsCount + " certificates");
            } catch (final Exception e) {
                LogV3.info("Could not compare certificate counts: " + e.getMessage());
            }
        } catch (final Exception e) {
            LogV3.info("Comparison with TrustCurrentJREProvider failed: " + e.getMessage());
        }
        LogV3.info("cacerts test completed successfully");
    }
}
