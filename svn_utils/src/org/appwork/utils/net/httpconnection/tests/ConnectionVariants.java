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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.HTTPProxy.TYPE;

/**
 * Provides standard connection variants for HTTP client tests: direct connection, SOCKS proxy, HTTP proxy, and native (URL.openConnection)
 * variants. Use with {@link ProxyConnectionTestBase} to run tests over all variants without code duplication.
 */
public final class ConnectionVariants {

    private ConnectionVariants() {
    }

    /**
     * Builds the full list of connection variants: direct (NONE), SOCKS5, HTTP proxy, SOCKS5 native, HTTP native, and NONE with native implementation.
     * Requires running HTTP and SOCKS proxy servers on the given ports.
     *
     * @param socksProxyPort port of the SOCKS proxy server (e.g. from {@link TestSocksProxyServer#getPort()})
     * @param httpProxyPort  port of the HTTP proxy server (e.g. from {@link TestHttpProxyServer#getPort()})
     * @return list of proxies to test (unmodifiable)
     */
    public static List<HTTPProxy> createWithProxies(final int socksProxyPort, final int httpProxyPort) {
        final List<HTTPProxy> proxies = new ArrayList<HTTPProxy>();
        proxies.add(HTTPProxy.NONE);
        proxies.add(new HTTPProxy(TYPE.SOCKS5, "localhost", socksProxyPort));
        proxies.add(new HTTPProxy(TYPE.HTTP, "localhost", httpProxyPort));
        proxies.add(new HTTPProxy(TYPE.SOCKS5, "localhost", socksProxyPort).setPreferNativeImplementation(true));
        proxies.add(new HTTPProxy(TYPE.HTTP, "localhost", httpProxyPort).setPreferNativeImplementation(true));
        proxies.add(new HTTPProxy(TYPE.NONE).setPreferNativeImplementation(true));
        return Collections.unmodifiableList(proxies);
    }

    /**
     * Builds the list of connection variants when no proxy servers are used: NONE (default) and NONE with native implementation.
     *
     * @return list of proxies to test (unmodifiable)
     */
    public static List<HTTPProxy> createDirectOnly() {
        final List<HTTPProxy> proxies = new ArrayList<HTTPProxy>();
        proxies.add(HTTPProxy.NONE);
        proxies.add(new HTTPProxy(TYPE.NONE).setPreferNativeImplementation(true));
        return Collections.unmodifiableList(proxies);
    }
}
