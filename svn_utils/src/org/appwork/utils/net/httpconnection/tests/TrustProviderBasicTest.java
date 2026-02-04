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

import javax.net.ssl.TrustManager;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptions;
import org.appwork.utils.net.httpconnection.trust.TrustAllProvider;
import org.appwork.utils.net.httpconnection.trust.TrustCurrentJREProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.TrustWindowsProvider;
import org.appwork.utils.net.httpconnection.trust.bridge.Java1_6TrustBridge;

/**
 * Basic tests for TrustProvider functionality including option mappings, null provider handling, and platform-specific providers.
 */
public class TrustProviderBasicTest extends SSLTrustProviderTestBase {
    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        testOptionMappings();
        testNullProviderRejects();
        testPlatformSpecificProviders();
        LogV3.info("TrustProvider basic tests completed successfully");
    }

    private void testOptionMappings() throws Exception {
        // Options are identified by id only; trustProvider/keyManagers are passed to factory.create()/getSSLSocketFactory()
        final SSLSocketStreamOptions optsTrustAll = new SSLSocketStreamOptions("id1");
        assertTrue("id1".equals(optsTrustAll.getId()), "options should store id");
        final SSLSocketStreamOptions optsJre = new SSLSocketStreamOptions("id2");
        assertTrue("id2".equals(optsJre.getId()), "options should store id");
        final SSLSocketStreamOptions optsCombined = new SSLSocketStreamOptions("id3");
        assertTrue("id3".equals(optsCombined.getId()), "options should store id");
    }

    private void testPlatformSpecificProviders() throws Exception {
        // Bridge always returns a TrustManager; platform providers throw only when actually used.
        final TestSSLSocketStreamFactory factory = new TestSSLSocketStreamFactory();
        factory.getTrustManagersForProvider(TrustWindowsProvider.getInstance());
        // factory.getTrustManagersForProvider(TrustLinuxProvider.getInstance());
    }

    private void testNullProviderRejects() throws Exception {
        final TestSSLSocketStreamFactory factory = new TestSSLSocketStreamFactory();
        final TrustManager[] managers = factory.getTrustManagersForProvider(null);
        assertTrue(managers != null && managers.length > 0, "Null provider yields rejecting bridge");
        final javax.net.ssl.X509TrustManager rejecting = (javax.net.ssl.X509TrustManager) managers[0];
        try {
            rejecting.checkServerTrusted(new java.security.cert.X509Certificate[0], "RSA");
            assertTrue(false, "Rejecting trust manager must throw CertificateException");
        } catch (final java.security.cert.CertificateException e) {
            assertTrue(e.getMessage() != null && e.getMessage().equals(Java1_6TrustBridge.NO_TRUSTPROVIDER_AVAILABLE), "Exception should mention " + Java1_6TrustBridge.NO_TRUSTPROVIDER_AVAILABLE);
        }
    }
}
