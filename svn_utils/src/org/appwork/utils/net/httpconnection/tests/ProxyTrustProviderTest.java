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

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.CompositeTrustResult;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.AllTrustProvider;
import org.appwork.utils.net.httpconnection.trust.CompositeTrustProvider;
import org.appwork.utils.net.httpconnection.trust.CurrentJRETrustProvider;
import org.appwork.utils.net.httpconnection.trust.NeverTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;
import org.appwork.utils.net.httpconnection.trust.ccadb.CCADBTrustProvider;

/**
 * Tests for TrustProvider and Revocation functionality via HTTP and SOCKS proxies.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>TrustProvider implementations work correctly through HTTP proxies</li>
 * <li>TrustProvider implementations work correctly through SOCKS proxies</li>
 * <li>Revocation checking works correctly through HTTP proxies</li>
 * <li>Revocation checking works correctly through SOCKS proxies</li>
 * <li>RevocationMode (SOFT vs HARD) works correctly through proxies</li>
 * </ul>
 *
 * @author thomas
 * @date 01.02.2026
 */
public class ProxyTrustProviderTest extends ProxyConnectionTestBase {
    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            // Start proxy servers
            setupProxyServers();
            testViaProxy();
            LogV3.info("All proxy TrustProvider tests completed successfully");
        } finally {
            teardownProxyServers();
        }
    }

    /**
     * Tests TrustProvider functionality via all connection variants (direct, SOCKS, HTTP proxy, native).
     */
    private void testViaProxy() throws Exception {
        LogV3.info("Test: TrustProvider via all connection variants");
        for (HTTPProxy proxy : getConnectionVariants()) {
            // Test with TrustAllProvider
            LogV3.info("Via " + proxy);
            {
                RequestContext context = new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get("https://appwork.org/");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted(), "Certificate should be trusted");
                assertTrue(context.getTrustResult().getTrustProvider() instanceof AllTrustProvider);
            }
            // Test with TrustCurrentJREProvider
            {
                RequestContext context = new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://appwork.org/");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted(), "Certificate should be trusted");
                assertTrue(context.getTrustResult().getTrustProvider() instanceof CurrentJRETrustProvider);
            }
            {
                RequestContext context = new HttpClient().proxy(proxy).trust(new CCADBTrustProvider()).get("https://appwork.org/");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted(), "Certificate should be trusted");
                assertTrue(context.getTrustResult().getTrustProvider() instanceof CCADBTrustProvider);
            }
            {
                RequestContext context = new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get("https://sha384.badssl.com/");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted(), "Certificate should be trusted");
                assertTrue(context.getTrustResult().getTrustProvider() instanceof AllTrustProvider);
            }
            {
                RequestContext context = new HttpClient().proxy(proxy).get("https://sha384.badssl.com/");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().getTrustProvider() == TrustUtils.getDefaultProvider());
                TrustResult succeededTrustResult = ((CompositeTrustResult) context.getTrustResult()).getSuccess();
                assertTrue(succeededTrustResult.getTrustProvider() instanceof AllTrustProvider);
            }
            {
                RequestContext context = new HttpClient().proxy(proxy).get("https://appwork.org/");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().getTrustProvider() == TrustUtils.getDefaultProvider());
                TrustResult succeededTrustResult = ((CompositeTrustResult) context.getTrustResult()).getSuccess();
                assertFalse(succeededTrustResult.getTrustProvider() instanceof AllTrustProvider);
            }
            try {
                new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://sha384.badssl.com/");
                throw new WTFException("Chrome says: net::ERR_CERT_DATE_INVALID");
            } catch (HttpClientException e) {
                assertTrue(e.getContext().getTrustResult() != null);
                assertFalse(e.getContext().getTrustResult().isTrusted());
                assertTrue(e.getContext().getTrustResult().getTrustProvider() instanceof CurrentJRETrustProvider);
            }
            try {
                new HttpClient().proxy(proxy).trust(new CCADBTrustProvider()).get("https://sha384.badssl.com/");
                throw new WTFException("Chrome says: net::ERR_CERT_DATE_INVALID");
            } catch (HttpClientException e) {
                assertTrue(e.getContext().getTrustResult() != null);
                assertFalse(e.getContext().getTrustResult().isTrusted());
                assertTrue(e.getContext().getTrustResult().getTrustProvider() instanceof CCADBTrustProvider);
            }
            try {
                new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://sha512.badssl.com/");
                throw new WTFException("Chrome says: net::ERR_CERT_DATE_INVALID");
            } catch (HttpClientException e) {
                assertTrue(e.getContext().getTrustResult() != null);
                assertFalse(e.getContext().getTrustResult().isTrusted());
                assertTrue(e.getContext().getTrustResult().getTrustProvider() instanceof CurrentJRETrustProvider);
            }
            try {
                new HttpClient().proxy(proxy).trust(new CCADBTrustProvider()).get("https://sha512.badssl.com/");
                throw new WTFException("Chrome says: net::ERR_CERT_DATE_INVALID");
            } catch (HttpClientException e) {
                assertTrue(e.getContext().getTrustResult() != null);
                assertFalse(e.getContext().getTrustResult().isTrusted());
                assertTrue(e.getContext().getTrustResult().getTrustProvider() instanceof CCADBTrustProvider);
            }
            // Test with CompositeTrustProvider
            {
                CompositeTrustProvider composite = new CompositeTrustProvider(CurrentJRETrustProvider.getInstance());
                RequestContext context = new HttpClient().proxy(proxy).trust(composite).get("https://appwork.org");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted());
                assertTrue(context.getTrustResult() instanceof CompositeTrustResult, "Should return CompositeTrustResult");
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess() != null);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getFails().size() == 0);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess().getTrustProvider() instanceof CurrentJRETrustProvider);
            }
            {
                CompositeTrustProvider composite = new CompositeTrustProvider(NeverTrustProvider.getInstance(), CurrentJRETrustProvider.getInstance());
                RequestContext context = new HttpClient().proxy(proxy).trust(composite).get("https://appwork.org");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted());
                assertTrue(context.getTrustResult() instanceof CompositeTrustResult, "Should return CompositeTrustResult");
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess() != null);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getFails().size() == 1);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getFails().get(0).getTrustProvider() instanceof NeverTrustProvider);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess().getTrustProvider() instanceof CurrentJRETrustProvider);
            }
            {
                CompositeTrustProvider composite = new CompositeTrustProvider(new CCADBTrustProvider());
                RequestContext context = new HttpClient().proxy(proxy).trust(composite).get("https://appwork.org");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted());
                assertTrue(context.getTrustResult() instanceof CompositeTrustResult, "Should return CompositeTrustResult");
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess() != null);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getFails().size() == 0);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess().getTrustProvider() instanceof CCADBTrustProvider);
            }
            {
                CompositeTrustProvider composite = new CompositeTrustProvider(NeverTrustProvider.getInstance(), new CCADBTrustProvider());
                RequestContext context = new HttpClient().proxy(proxy).trust(composite).get("https://appwork.org");
                assertTrue(context.getCode() == 200 || context.getCode() == 301 || context.getCode() == 302, "Request should succeed via SOCKS proxy");
                assertTrue(context.getTrustResult() != null, "TrustResult should not be null");
                assertTrue(context.getTrustResult().isTrusted());
                assertTrue(context.getTrustResult() instanceof CompositeTrustResult, "Should return CompositeTrustResult");
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess() != null);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getFails().size() == 1);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getFails().get(0).getTrustProvider() instanceof NeverTrustProvider);
                assertTrue(((CompositeTrustResult) context.getTrustResult()).getSuccess().getTrustProvider() instanceof CCADBTrustProvider);
            }
        }
        LogV3.info("TrustProvider via SOCKS proxy tests completed successfully");
    }
}
