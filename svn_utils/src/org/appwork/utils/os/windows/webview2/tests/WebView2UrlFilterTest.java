/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.webview2.tests;

import org.appwork.testframework.AWTest;
import org.appwork.utils.os.windows.webview2.WebView2UrlFilter;

/** Headless coverage of the whitelist/blacklist patterns and of the local-network host detection. Runs on every platform. */
public class WebView2UrlFilterTest extends AWTest {
    public static void main(String[] args) {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        testEmptyFilter();
        testHostPatterns();
        testUrlPatterns();
        testBlacklistWins();
        testNoBypass();
        testConnections();
        testResourcesSwitch();
        testHostParsing();
        testLocalNetworkHosts();
        testInvalidPatterns();
    }

    private void testEmptyFilter() throws Exception {
        final WebView2UrlFilter filter = new WebView2UrlFilter();
        assertTrue(filter.isAllowed("https://example.com/", true));
        assertTrue(filter.isAllowed("http://anything.test/x.js", false));
        assertFalse(filter.isAllowed(null, true));
    }

    private void testHostPatterns() throws Exception {
        final WebView2UrlFilter exact = new WebView2UrlFilter().allow("example.com");
        assertTrue(exact.isAllowed("https://example.com/path?x=1", true));
        assertTrue(exact.isAllowed("HTTPS://EXAMPLE.COM:8443/", true));
        assertFalse(exact.isAllowed("https://www.example.com/", true));
        assertFalse(exact.isAllowed("https://example.com.evil.test/", true));
        assertFalse(exact.isAllowed("https://evil.test/example.com", true));
        assertFalse(exact.isAllowed("https://example.com@evil.test/", true));
        final WebView2UrlFilter domain = new WebView2UrlFilter().allow(".example.com");
        assertTrue(domain.isAllowed("https://example.com/", true));
        assertTrue(domain.isAllowed("https://a.b.example.com/", true));
        assertFalse(domain.isAllowed("https://notexample.com/", true));
        assertFalse(domain.isAllowed("https://example.com.evil.test/", true));
        final WebView2UrlFilter wildcard = new WebView2UrlFilter().allow("*.example.com", "cdn*.example.org");
        assertTrue(wildcard.isAllowed("https://www.example.com/", true));
        assertFalse(wildcard.isAllowed("https://example.com/", true));
        assertTrue(wildcard.isAllowed("https://cdn12.example.org/a.js", false));
        assertFalse(wildcard.isAllowed("https://www.example.org/a.js", false));
    }

    private void testUrlPatterns() throws Exception {
        final WebView2UrlFilter filter = new WebView2UrlFilter().allow("https://example.com/app/*", "example.org/public/*");
        assertTrue(filter.isAllowed("https://example.com/app/index.html", true));
        assertFalse(filter.isAllowed("http://example.com/app/index.html", true));
        assertFalse(filter.isAllowed("https://example.com/other", true));
        assertTrue(filter.isAllowed("http://example.org/public/a", true));
        assertTrue(filter.isAllowed("https://example.org/public/a", true));
        assertFalse(filter.isAllowed("https://example.org/private/a", true));
    }

    private void testBlacklistWins() throws Exception {
        final WebView2UrlFilter filter = new WebView2UrlFilter().allow(".example.com").block("ads.example.com", "https://example.com/track/*");
        assertTrue(filter.isAllowed("https://www.example.com/", true));
        assertFalse(filter.isAllowed("https://ads.example.com/banner.js", false));
        assertFalse(filter.isAllowed("https://example.com/track/pixel.gif", false));
        final WebView2UrlFilter blacklistOnly = new WebView2UrlFilter().block(".tracker.test");
        assertTrue(blacklistOnly.isAllowed("https://example.com/", true));
        assertFalse(blacklistOnly.isAllowed("https://x.tracker.test/", true));
    }

    private void testResourcesSwitch() throws Exception {
        final WebView2UrlFilter filter = new WebView2UrlFilter().allow("example.com").setFilterResources(false);
        assertFalse(filter.isAllowed("https://other.test/", true));
        assertTrue(filter.isAllowed("https://other.test/lib.js", false));
        /* Not http(s): judged by the scheme rules of the browser, not by the lists. */
        assertTrue(new WebView2UrlFilter().allow("example.com").isAllowed("about:blank", true));
    }

    private void testHostParsing() throws Exception {
        assertEquals("example.com", WebView2UrlFilter.hostOf("https://user:pw@Example.COM.:8080/a?b#c"));
        assertEquals("::1", WebView2UrlFilter.hostOf("http://[::1]:9666/flash"));
        assertEquals("example.com", WebView2UrlFilter.hostOf("https://example.com"));
        assertEquals(null, WebView2UrlFilter.hostOf("about:blank"));
        assertEquals(null, WebView2UrlFilter.hostOf(null));
    }

    private void testLocalNetworkHosts() throws Exception {
        final String[] local = new String[] { "localhost", "app.localhost", "127.0.0.1", "127.8.9.1", "10.0.0.5", "192.168.1.1", "172.16.0.1", "172.31.255.255", "169.254.10.10", "0.0.0.0", "::1", "fe80::1", "fd12:3456::1", "::ffff:127.0.0.1", "::ffff:7f00:1", "::ffff:c0a8:101", "100.64.1.1", "100.127.255.254", "127.1", "64:ff9b::7f00:1", "224.0.0.1", "router", "printer.local" };
        for (String host : local) {
            assertTrue(WebView2UrlFilter.isLocalNetworkHost(host));
        }
        final String[] remote = new String[] { "example.com", "8.8.8.8", "172.32.0.1", "192.169.1.1", "100.128.0.1", "100.63.0.1", "::ffff:808:808", "2001:db8::1", "localhost.example.com", null };
        for (String host : remote) {
            assertFalse(WebView2UrlFilter.isLocalNetworkHost(host));
        }
    }

    /** The bypasses found in the review of the first version. */
    private void testNoBypass() throws Exception {
        final WebView2UrlFilter whitelist = new WebView2UrlFilter().allow("https://*.example.com/*", "*.example.net/api/*");
        assertTrue(whitelist.isAllowed("https://www.example.com/page", true));
        assertTrue(whitelist.isAllowed("https://a.example.net/api/x", true));
        /* A wildcard in the host must not reach into the path, the query or a user name. */
        assertFalse(whitelist.isAllowed("https://evil.attacker.net/?.example.com/", true));
        assertFalse(whitelist.isAllowed("https://evil.attacker.net/x.example.com/", true));
        assertFalse(whitelist.isAllowed("https://a.example.com@evil.attacker.net/", true));
        assertFalse(whitelist.isAllowed("https://evil.attacker.net/.example.net/api/x", true));
        assertFalse(whitelist.isAllowed("https://evil.attacker.net\\.example.com/", true));
        final WebView2UrlFilter blacklist = new WebView2UrlFilter().block("https://example.com/track/*", "ads.example.com");
        assertFalse(blacklist.isAllowed("https://example.com/track/x", false));
        /* Other spellings of the same address. */
        assertFalse(blacklist.isAllowed("https://example.com./track/x", false));
        assertFalse(blacklist.isAllowed("https://example.com:443/track/x", false));
        assertFalse(blacklist.isAllowed("https://EXAMPLE.com/track/x#frag", false));
        assertFalse(blacklist.isAllowed("https://user@example.com/track/x", false));
        assertFalse(blacklist.isAllowed("https://ads.example.com.:8443/x", false));
        assertTrue(blacklist.isAllowed("https://example.com/other", false));
        /* An http(s) address that cannot be understood is not let through. */
        assertFalse(new WebView2UrlFilter().allow("example.com").isAllowed("https:///nohost", true));
    }

    private void testConnections() throws Exception {
        final WebView2UrlFilter filter = new WebView2UrlFilter().allow(".example.com", "https://api.example.org/v1/*").block("ads.example.com", "https://example.com/track/*");
        assertTrue(filter.isConnectionAllowed(true, "www.example.com", 443));
        assertTrue(filter.isConnectionAllowed(true, "API.example.org.", 443));
        assertFalse(filter.isConnectionAllowed(false, "api.example.org", 80));
        assertFalse(filter.isConnectionAllowed(true, "ads.example.com", 443));
        assertFalse(filter.isConnectionAllowed(true, "other.test", 443));
        /* A blacklist pattern with a path cannot refuse the whole host. */
        assertTrue(filter.isConnectionAllowed(true, "example.com", 443));
        assertTrue(new WebView2UrlFilter().allow("example.com").setFilterResources(false).isConnectionAllowed(true, "other.test", 443));
        assertTrue(new WebView2UrlFilter().allow("example.com:8443").isConnectionAllowed(true, "example.com", 8443));
        assertFalse(new WebView2UrlFilter().allow("example.com:8443").isConnectionAllowed(true, "example.com", 443));
    }

    private void testInvalidPatterns() throws Exception {
        final String[] invalid = new String[] { "", "  ", ".", ".*.example.com", "ftp://example.com/", "https://", "exa mple.com", "example.com:port", null };
        for (String pattern : invalid) {
            try {
                new WebView2UrlFilter().allow(pattern);
                throw new Exception("Pattern must be rejected: " + pattern);
            } catch (IllegalArgumentException expected) {
                /* expected */
            }
        }
    }
}
