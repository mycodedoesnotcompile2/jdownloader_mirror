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

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Exceptions;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;
import org.appwork.utils.net.httpconnection.trust.CompositeTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustAllProvider;
import org.appwork.utils.net.httpconnection.trust.TrustCurrentJREProvider;
import org.appwork.utils.net.httpconnection.trust.TrustWindowsProvider;

/**
 * Tests for SSL hostname verification functionality.
 */
public class HostnameVerificationTest extends ProxyConnectionTestBase {

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            setupProxyServers();
            final List<HTTPProxy> variants = getConnectionVariants();
            for (final HTTPProxy proxy : variants) {
                LogV3.info("Hostname verification via " + proxy);
                testHostnameVerificationForProxy(proxy);
            }
            LogV3.info("Hostname verification tests completed successfully");
        } finally {
            teardownProxyServers();
        }
    }

    private void testHostnameVerificationForProxy(final HTTPProxy proxy) throws Exception {
        final String url = "https://ipcheck1.jdownloader.org/";
        final RequestContext trustAllOk = new HttpClient().proxy(proxy).trust(TrustAllProvider.getInstance()).get(url);
        RequestContext javaDefaultOk;
        try {
            new HttpClient().proxy(proxy).trust(TrustCurrentJREProvider.getInstance()).get(url);
            throw new WTFException("Expect HTTPS hostname wrong:  should be <ipcheck1.jdownloader.org> != [cdn15.appwork.org]");
        } catch (HttpClientException e) {
            javaDefaultOk = e.getContext();
            assertTrue(Exceptions.containsInstanceOf(e, IllegalSSLHostnameException.class));
        }
        RequestContext combinedOk;
        try {
            new HttpClient().proxy(proxy).trust(TrustCurrentJREProvider.getInstance(), TrustWindowsProvider.getInstance()).get(url);
            throw new WTFException("Expect HTTPS hostname wrong:  should be <ipcheck1.jdownloader.org> != [cdn15.appwork.org]");
        } catch (HttpClientException e) {
            combinedOk = e.getContext();
            assertTrue(Exceptions.containsInstanceOf(e, IllegalSSLHostnameException.class));
        }
        RequestContext windowsOnlyOk;
        try {
            new HttpClient().proxy(proxy).trust(TrustWindowsProvider.getInstance()).get(url);
            throw new WTFException("Expect HTTPS hostname wrong:  should be <ipcheck1.jdownloader.org> != [cdn15.appwork.org]");
        } catch (HttpClientException e) {
            windowsOnlyOk = e.getContext();
            assertTrue(Exceptions.containsInstanceOf(e, IllegalSSLHostnameException.class));
            assertTrue(Exceptions.containsInstanceOf(e.getContext().getTrustResult().getException(), IllegalSSLHostnameException.class));
        }
        assertTrue(trustAllOk.getTrustResult().isTrusted(), "Invalid cert should only be reachable with TrustAllProvider");
        assertFalse(javaDefaultOk.getTrustResult().isTrusted(), "Host Name check failed");
        assertFalse(combinedOk.getTrustResult().isTrusted(), "Host Name check failed");
        assertFalse(windowsOnlyOk.getTrustResult().isTrusted(), "Host Name check failed");
        assertTrue(Exceptions.containsInstanceOf(javaDefaultOk.getTrustResult().getException(), IllegalSSLHostnameException.class));
        assertTrue(Exceptions.containsInstanceOf(combinedOk.getTrustResult().getException(), IllegalSSLHostnameException.class));
        assertTrue(Exceptions.containsInstanceOf(windowsOnlyOk.getTrustResult().getException(), IllegalSSLHostnameException.class));
    }
}
