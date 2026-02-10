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

import java.util.List;

import javax.net.ssl.SSLHandshakeException;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Exceptions;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory.TLS;
import org.appwork.utils.net.httpconnection.trust.AllTrustProvider;

/**
 * Tests for SSL/TLS protocol version support and restrictions.
 */
public class SSLProtocolVersionTest extends ProxyConnectionTestBase {
    /**
     *
     */
    private static final String HTTPS_TLS_V1_0_BADSSL_COM_1010 = "https://tls-v1-0.badssl.com:1010/";
    /**
     *
     */
    private static final String HTTPS_TLS_V1_1_BADSSL_COM_1011 = "https://tls-v1-1.badssl.com:1011/";

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            setupProxyServers();
            // getClientWithSSLProtocol(...) supports only direct and none proxy
            final List<HTTPProxy> directOnlyVariants = ConnectionVariants.createDirectOnly();
            final List<HTTPProxy> variants = getConnectionVariants();
            // Test TLS 1.1 - should fail (disabled by default in modern Java)
            for (final HTTPProxy proxy : directOnlyVariants) {
                try {
                    getClientWithSSLProtocol(TLS.TLS_1_1.id).proxy(proxy).trust(AllTrustProvider.getInstance()).get(HTTPS_TLS_V1_1_BADSSL_COM_1011);
                    if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_11_0)) {
                        throw new Exception("Java should have disable TLS1.1 via " + proxy);
                    }
                } catch (HttpClientException e) {
                    assertTrue(Exceptions.containsInstanceOf(e, SSLHandshakeException.class));
                }
            }
            // Test TLS 1.0 - should fail (disabled by default in modern Java)
            for (final HTTPProxy proxy : directOnlyVariants) {
                try {
                    getClientWithSSLProtocol(TLS.TLS_1_0.id).proxy(proxy).trust(AllTrustProvider.getInstance()).get(HTTPS_TLS_V1_0_BADSSL_COM_1010);
                    if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_11_0)) {
                        throw new Exception("Java should have disable TLS1.0 via " + proxy);
                    }
                } catch (HttpClientException e) {
                    assertTrue(Exceptions.containsInstanceOf(e, SSLHandshakeException.class));
                }
            }
            // Test TLS 1.2 / 1.3 and default client (getClientWithSSLProtocol only via direct/none)
            for (final HTTPProxy proxy : directOnlyVariants) {
                LogV3.info("SSL protocol tests via " + proxy);
                getClientWithSSLProtocol(TLS.TLS_1_2.id).proxy(proxy).trust(AllTrustProvider.getInstance()).get("https://tls-v1-2.badssl.com:1012/");
                if (JavaSSLSocketStreamFactory.getInstance().isTLSSupported(TLS.TLS_1_3, null, null)) {
                    getClientWithSSLProtocol(TLS.TLS_1_3.id).proxy(proxy).trust(AllTrustProvider.getInstance()).get("https://appwork.org");
                }
                new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get("https://tls-v1-2.badssl.com:1012/");
                new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get("https://appwork.org");
            }
            // Test that TLS 1.1/1.0 are rejected even with TrustAllProvider (all variants)
            for (final HTTPProxy proxy : variants) {
                try {
                    new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get(HTTPS_TLS_V1_1_BADSSL_COM_1011);
                    if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_11_0)) {
                        throw new WTFException("Should fail. 1.1 not allowed via " + proxy);
                    }
                } catch (HttpClientException e) {
                    assertTrue(Exceptions.containsInstanceOf(e, SSLHandshakeException.class));
                }
                try {
                    new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get(HTTPS_TLS_V1_0_BADSSL_COM_1010);
                    if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_11_0)) {
                        throw new WTFException("Should fail. 1.0 not allowed via " + proxy);
                    }
                } catch (HttpClientException e) {
                    assertTrue(Exceptions.containsInstanceOf(e, SSLHandshakeException.class));
                }
            }
            LogV3.info("SSL Protocol Version tests completed successfully");
        } finally {
            teardownProxyServers();
        }
    }
}
