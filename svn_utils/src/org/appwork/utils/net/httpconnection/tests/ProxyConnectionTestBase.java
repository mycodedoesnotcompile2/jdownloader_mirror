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

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.net.httpconnection.HTTPProxy;

/**
 * Base class for HTTP client tests that run over all connection variants (direct, SOCKS proxy, HTTP proxy, and native variants).
 * Provides proxy server lifecycle and {@link #getConnectionVariants()} so tests can iterate without duplicating setup.
 */
public abstract class ProxyConnectionTestBase extends SSLTrustProviderTestBase {

    private TestHttpProxyServer  httpProxyServer;
    private TestSocksProxyServer socksProxyServer;
    private List<HTTPProxy>      connectionVariants;

    /**
     * Starts HTTP and SOCKS proxy servers and initializes {@link #getConnectionVariants()}. Call from {@link #runTest()} before running
     * tests that use connection variants.
     */
    protected void setupProxyServers() throws Exception {
        LogV3.info("Setting up proxy servers...");
        httpProxyServer = new TestHttpProxyServer(0);
        httpProxyServer.start();
        socksProxyServer = new TestSocksProxyServer(0);
        socksProxyServer.start();
        LogV3.info("HTTP proxy server started on port " + httpProxyServer.getPort() + ", SOCKS on " + socksProxyServer.getPort());
        connectionVariants = ConnectionVariants.createWithProxies(socksProxyServer.getPort(), httpProxyServer.getPort());
    }

    /**
     * Stops proxy servers. Call from {@link #runTest()} in a finally block after tests.
     */
    protected void teardownProxyServers() {
        if (httpProxyServer != null) {
            httpProxyServer.stop();
            httpProxyServer = null;
        }
        if (socksProxyServer != null) {
            socksProxyServer.stop();
            socksProxyServer = null;
        }
        connectionVariants = null;
    }

    /**
     * Returns the list of connection variants (direct, SOCKS, HTTP proxy, and native variants). Valid only after
     * {@link #setupProxyServers()} has been called.
     *
     * @return list of HTTPProxy to test (never null after setup)
     */
    protected List<HTTPProxy> getConnectionVariants() {
        if (connectionVariants == null) {
            return ConnectionVariants.createDirectOnly();
        }
        return connectionVariants;
    }

    /**
     * Returns true if proxy servers are running and full connection variants (including SOCKS/HTTP proxy) are available.
     */
    protected boolean isProxyServersRunning() {
        return connectionVariants != null && connectionVariants.size() > 2;
    }
}
