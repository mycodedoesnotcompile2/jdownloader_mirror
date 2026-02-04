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

import java.security.cert.X509Certificate;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpconnection.CompositeTrustResult;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.CompositeTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustAllProvider;
import org.appwork.utils.net.httpconnection.trust.TrustCurrentJREProvider;
import org.appwork.utils.net.httpconnection.trust.TrustWindowsProvider;

/**
 * Tests for CompositeTrustProvider functionality including delegate management, failed provider tracking, and composite trust results.
 */
public class CompositeTrustProviderTest extends SSLTrustProviderTestBase {

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        createTestCertificates();
        testCompositeTrustProvider();
        testCompositeSSLTrustInfoWithFailedProviders();
        cleanupTempFiles();
        LogV3.info("CompositeTrustProvider tests completed successfully");
    }

    private void testCompositeTrustProvider() throws Exception {
        final CompositeTrustProvider emptyComposite = new CompositeTrustProvider();
        assertTrue(emptyComposite.getDelegates().length == 0, "Empty CompositeTrustProvider has no delegates");
        final CompositeTrustProvider singleComposite = new CompositeTrustProvider(TrustAllProvider.getInstance());
        assertTrue(singleComposite.getDelegates().length == 1, "Single-provider composite has one delegate");
        final CompositeTrustProvider multiComposite = new CompositeTrustProvider(TrustCurrentJREProvider.getInstance(), TrustWindowsProvider.getInstance());
        assertTrue(multiComposite.getDelegates().length == 2, "Multi-provider composite has three delegates");
    }

    /**
     * Tests CompositeSSLTrustInfo with the scenario: JRE -> Windows -> TrustAll. JRE and Windows should fail (self-signed cert not in their
     * stores), TrustAll accepts. CompositeSSLTrustInfo should contain JRE and Windows in failedProviders list.
     */
    private void testCompositeSSLTrustInfoWithFailedProviders() throws Exception {
        // Create composite provider: JRE -> Windows -> TrustAll
        // JRE and Windows will reject the self-signed certificate, TrustAll will accept it
        final TrustCurrentJREProvider jreProvider = TrustCurrentJREProvider.getInstance();
        final TrustWindowsProvider windowsProvider = TrustWindowsProvider.getInstance();
        final TrustAllProvider trustAllProvider = TrustAllProvider.getInstance();
        final CompositeTrustProvider composite = new CompositeTrustProvider(jreProvider, windowsProvider, trustAllProvider);
        // Create a certificate chain with just the server certificate (self-signed, not in JRE/Windows trust stores)
        final X509Certificate[] chain = new X509Certificate[] { serverCertificate };
        // Test checkServerTrusted - should succeed via TrustAllProvider, with JRE and Windows in failed list
        TrustResult trustInfo = null;
        trustInfo = composite.checkServerTrusted(chain, "RSA", null);
        assertTrue(trustInfo.isTrusted(), "CompositeTrustProvider should accept certificate via TrustAllProvider");
        assertTrue(trustInfo != null, "CompositeTrustProvider should return trust info");
        assertTrue(trustInfo instanceof CompositeTrustResult, "CompositeTrustProvider should return CompositeSSLTrustInfo");
        final CompositeTrustResult compositeInfo = (CompositeTrustResult) trustInfo;
        // Verify the successful provider is TrustAllProvider
        assertTrue(trustAllProvider.equals(compositeInfo.getSuccess().getTrustProvider()), "Successful provider should be TrustAllProvider");
        // Verify failed providers list contains JRE and Windows (in order)
        assertTrue(compositeInfo.getFails().size() > 0, "CompositeSSLTrustInfo should have failed providers");
        final List<TrustResult> failed = compositeInfo.getFails();
        assertTrue(failed.size() == 2, "Should have 2 failed providers (JRE and Windows), but got: " + failed.size());
        assertTrue(jreProvider.equals(failed.get(0).getTrustProvider()), "First failed provider should be TrustCurrentJREProvider");
        assertTrue(windowsProvider.equals(failed.get(1).getTrustProvider()), "Second failed provider should be TrustWindowsProvider");
        // Verify certificate info is present
        assertTrue(compositeInfo.getChain() != null, "Trust info should contain the chain");
        // Test checkClientTrusted as well
        TrustResult clientTrustInfo = null;
        clientTrustInfo = composite.checkClientTrusted(chain, "RSA", null);
        assertTrue(clientTrustInfo.isTrusted(), "CompositeTrustProvider should accept client certificate via TrustAllProvider");
        assertTrue(clientTrustInfo instanceof CompositeTrustResult, "checkClientTrusted should also return CompositeSSLTrustInfo");
        final CompositeTrustResult clientCompositeInfo = (CompositeTrustResult) clientTrustInfo;
        assertTrue(trustAllProvider.equals(clientCompositeInfo.getSuccess().getTrustProvider()), "Client: Successful provider should be TrustAllProvider");
        assertTrue(clientCompositeInfo.getFails().size() == 2, "Client: Should have 2 failed providers");
        assertTrue(jreProvider.equals(clientCompositeInfo.getFails().get(0).getTrustProvider()), "Client: First failed provider should be TrustCurrentJREProvider");
        assertTrue(windowsProvider.equals(clientCompositeInfo.getFails().get(1).getTrustProvider()), "Client: Second failed provider should be TrustWindowsProvider");
        // Test composite where first provider succeeds (no failed providers)
        final CompositeTrustProvider firstSucceeds = new CompositeTrustProvider(trustAllProvider, jreProvider);
        final TrustResult firstSucceedsInfo = firstSucceeds.checkServerTrusted(chain, "RSA", null);
        assertTrue(firstSucceedsInfo instanceof CompositeTrustResult, "Should return CompositeSSLTrustInfo even when first provider succeeds");
        final CompositeTrustResult firstSucceedsComposite = (CompositeTrustResult) firstSucceedsInfo;
        assertTrue(trustAllProvider.equals(firstSucceedsComposite.getSuccess().getTrustProvider()), "First provider should succeed");
        assertFalse(firstSucceedsComposite.getFails().size() > 0, "No providers should have failed when first succeeds");
    }
}
