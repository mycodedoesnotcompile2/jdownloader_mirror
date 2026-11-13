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
package org.appwork.utils.os.windows.webview2;

import java.net.InetAddress;
import java.util.Locale;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Pattern;

/**
 * Whitelist and blacklist for the addresses a {@link WindowsWebView2Browser} may load.
 *
 * <pre>
 * WebView2UrlFilter filter = new WebView2UrlFilter();
 * filter.allow("example.com", ".example.org", "https://cdn.example.net/lib/*");
 * filter.block("ads.example.org");
 * options.urlFilter = filter;
 * </pre>
 *
 * Patterns:
 * <ul>
 * <li>{@code example.com}: exactly this host.</li>
 * <li>{@code .example.com}: this domain and all its subdomains.</li>
 * <li>{@code *.example.com}, {@code cdn*.example.com}: host with {@code *} as wildcard. A {@code *} in the host never reaches beyond
 * the host.</li>
 * <li>{@code example.com:8443}: host and port. Without a port every port matches.</li>
 * <li>{@code https://example.com/app/*}: scheme, host and path. In the path, {@code *} stands for any characters and the path must
 * match completely, so a pattern for a folder ends with {@code /*}. Without a scheme ({@code example.com/app/*}) both http and https
 * match; without a path every path matches. {@code ws}/{@code wss} are treated as {@code http}/{@code https}.</li>
 * </ul>
 * Matching ignores case and works on the parts of the address as the browser understands them, not on its text: user names, a dot at
 * the end of the host or an explicit default port do not change the result. An address is loaded when no blacklist pattern matches it
 * and, if the whitelist is not empty, at least one whitelist pattern matches it. The blacklist always wins.
 * <p>
 * The filter judges pages, iframes and, unless {@link #setFilterResources(boolean)} is switched off, everything a page loads
 * (scripts, images, XHR/fetch). Connections the browser does not report as a request, in particular WebSockets, are judged by the
 * filter proxy of the browser, which only knows scheme, host and port: see {@link #isConnectionAllowed(boolean, String, int)}.
 * <p>
 * Thread safe; patterns can be added while the browser is running.
 */
public final class WebView2UrlFilter {
    /** The parts of an http(s)/ws(s) address that the patterns are compared with. */
    private static final class Address {
        private final boolean tls;
        private final String  host;
        private final int     port;
        private final String  path;

        private Address(boolean tls, String host, int port, String path) {
            this.tls = tls;
            this.host = host;
            this.port = port;
            this.path = path;
        }

        /** @return {@code null} when {@code url} is not an http(s)/ws(s) address with a host */
        private static Address parse(String url) {
            final String lower = url.trim().toLowerCase(Locale.ENGLISH).replace('\\', '/');
            final int schemeEnd = lower.indexOf("://");
            if (schemeEnd <= 0) {
                return null;
            }
            final String scheme = lower.substring(0, schemeEnd);
            final boolean tls;
            if ("https".equals(scheme) || "wss".equals(scheme)) {
                tls = true;
            } else if ("http".equals(scheme) || "ws".equals(scheme)) {
                tls = false;
            } else {
                return null;
            }
            int end = lower.length();
            for (int i = schemeEnd + 3; i < lower.length(); i++) {
                final char c = lower.charAt(i);
                if (c == '/' || c == '?' || c == '#') {
                    end = i;
                    break;
                }
            }
            final String[] hostPort = splitAuthority(lower.substring(schemeEnd + 3, end));
            if (hostPort == null) {
                return null;
            }
            int port = tls ? 443 : 80;
            if (hostPort[1] != null) {
                try {
                    port = Integer.parseInt(hostPort[1]);
                } catch (NumberFormatException e) {
                    return null;
                }
            }
            String path = lower.substring(end);
            final int fragment = path.indexOf('#');
            if (fragment >= 0) {
                path = path.substring(0, fragment);
            }
            if (!path.startsWith("/")) {
                path = "/" + path;
            }
            return new Address(tls, hostPort[0], port, path);
        }
    }

    private static final class Rule {
        private static final Pattern HOST_CHARS = Pattern.compile("[a-z0-9.*_-]+|[0-9a-f:.]+");
        /** Domain and subdomains for a {@code .example.com} pattern, otherwise {@code null}. */
        private final String         domain;
        private final Pattern        host;
        /** {@code null} = http and https. */
        private final Boolean        tls;
        /** -1 = every port. */
        private final int            port;
        /** {@code null} = every path. */
        private final Pattern        path;

        private Rule(String pattern) {
            String p = pattern.trim().toLowerCase(Locale.ENGLISH);
            if (p.length() == 0) {
                throw new IllegalArgumentException("Empty URL filter pattern");
            }
            if (p.startsWith(".")) {
                if (p.length() == 1 || !p.matches("\\.[a-z0-9._-]+") || p.endsWith(".")) {
                    throw new IllegalArgumentException("Invalid domain pattern: " + pattern);
                }
                this.domain = p.substring(1);
                this.host = null;
                this.tls = null;
                this.port = -1;
                this.path = null;
                return;
            }
            this.domain = null;
            final int schemeEnd = p.indexOf("://");
            if (schemeEnd >= 0) {
                final String scheme = p.substring(0, schemeEnd);
                if ("https".equals(scheme) || "wss".equals(scheme)) {
                    this.tls = Boolean.TRUE;
                } else if ("http".equals(scheme) || "ws".equals(scheme)) {
                    this.tls = Boolean.FALSE;
                } else {
                    throw new IllegalArgumentException("Only http, https, ws and wss can be filtered: " + pattern);
                }
                p = p.substring(schemeEnd + 3);
            } else {
                this.tls = null;
            }
            final int slash = p.indexOf('/');
            final String authority = slash >= 0 ? p.substring(0, slash) : p;
            final String pathPattern = slash >= 0 ? p.substring(slash) : null;
            if (authority.indexOf('?') >= 0 || authority.indexOf('#') >= 0) {
                throw new IllegalArgumentException("Invalid URL filter pattern: " + pattern);
            }
            final String[] hostPort = splitAuthority(authority);
            if (hostPort == null || !HOST_CHARS.matcher(hostPort[0]).matches()) {
                throw new IllegalArgumentException("Invalid host in URL filter pattern: " + pattern);
            }
            /* In a host, * stands for host characters only, so it can never swallow a slash, a user name or another host. */
            this.host = wildcard(hostPort[0], "[a-z0-9._-]*");
            if (hostPort[1] == null) {
                this.port = -1;
            } else {
                try {
                    this.port = Integer.parseInt(hostPort[1]);
                } catch (NumberFormatException e) {
                    throw new IllegalArgumentException("Invalid port in URL filter pattern: " + pattern);
                }
            }
            this.path = pathPattern == null || "/*".equals(pathPattern) ? null : wildcard(pathPattern, ".*");
        }

        private boolean matchesConnection(boolean tls, String host, int port) {
            if (domain != null) {
                return host.equals(domain) || host.endsWith("." + domain);
            }
            if (this.tls != null && this.tls.booleanValue() != tls) {
                return false;
            }
            if (this.port >= 0 && this.port != port) {
                return false;
            }
            return this.host.matcher(host).matches();
        }

        private boolean matches(Address address) {
            return matchesConnection(address.tls, address.host, address.port) && (path == null || path.matcher(address.path).matches());
        }

        private static Pattern wildcard(String pattern, String star) {
            final StringBuilder regex = new StringBuilder();
            int start = 0;
            while (true) {
                final int index = pattern.indexOf('*', start);
                if (index < 0) {
                    regex.append(Pattern.quote(pattern.substring(start)));
                    break;
                }
                regex.append(Pattern.quote(pattern.substring(start, index))).append(star);
                start = index + 1;
            }
            return Pattern.compile(regex.toString(), Pattern.DOTALL);
        }
    }

    private final CopyOnWriteArrayList<Rule> allowed         = new CopyOnWriteArrayList<Rule>();
    private final CopyOnWriteArrayList<Rule> blocked         = new CopyOnWriteArrayList<Rule>();
    private volatile boolean                 filterResources = true;

    /**
     * Adds whitelist patterns. As soon as the whitelist is not empty, only matching addresses are loaded.
     *
     * @throws IllegalArgumentException
     *             for an empty or malformed pattern
     */
    public WebView2UrlFilter allow(String... patterns) {
        add(allowed, patterns);
        return this;
    }

    /**
     * Adds blacklist patterns. A matching address is never loaded, even if the whitelist matches it too.
     *
     * @throws IllegalArgumentException
     *             for an empty or malformed pattern
     */
    public WebView2UrlFilter block(String... patterns) {
        add(blocked, patterns);
        return this;
    }

    private static void add(CopyOnWriteArrayList<Rule> rules, String... patterns) {
        if (patterns == null) {
            return;
        }
        for (String pattern : patterns) {
            if (pattern == null) {
                throw new IllegalArgumentException("URL filter pattern is null");
            }
            rules.add(new Rule(pattern));
        }
    }

    public boolean isFilterResources() {
        return filterResources;
    }

    /**
     * {@code true} (default): the lists apply to everything, also to the scripts, images, data and connections of a page.
     * {@code false}: the lists only apply to pages and iframes; an allowed page may then load its resources from anywhere and open
     * connections to anywhere.
     */
    public WebView2UrlFilter setFilterResources(boolean filterResources) {
        this.filterResources = filterResources;
        return this;
    }

    /**
     * @param document
     *            {@code true} for a page or iframe, {@code false} for a resource of a page
     * @return {@code true} when {@code url} may be loaded. Addresses that are not http(s)/ws(s) are not judged here and return
     *         {@code true}; an http(s) address that cannot be understood returns {@code false}.
     */
    public boolean isAllowed(String url, boolean document) {
        if (url == null) {
            return false;
        }
        final String lower = url.trim().toLowerCase(Locale.ENGLISH);
        if (!lower.startsWith("http:") && !lower.startsWith("https:") && !lower.startsWith("ws:") && !lower.startsWith("wss:")) {
            return true;
        }
        if (!document && !filterResources) {
            return true;
        }
        final Address address = Address.parse(lower);
        if (address == null) {
            return false;
        }
        for (Rule rule : blocked) {
            if (rule.matches(address)) {
                return false;
            }
        }
        if (allowed.isEmpty()) {
            return true;
        }
        for (Rule rule : allowed) {
            if (rule.matches(address)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Judges a connection of which only scheme, host and port are known, e.g. a WebSocket or the tunnel of an https request. A
     * blacklist pattern refuses the connection when it covers the whole host (no path). A non-empty whitelist allows it when one of
     * its patterns names the host, whatever path that pattern has. Always {@code true} when the lists only apply to pages, see
     * {@link #setFilterResources(boolean)}.
     *
     * @param tls
     *            {@code true} for https/wss
     */
    public boolean isConnectionAllowed(boolean tls, String host, int port) {
        if (!filterResources) {
            return true;
        }
        final String normalized = normalizeHost(host);
        if (normalized == null) {
            return false;
        }
        for (Rule rule : blocked) {
            if (rule.path == null && rule.matchesConnection(tls, normalized, port)) {
                return false;
            }
        }
        if (allowed.isEmpty()) {
            return true;
        }
        for (Rule rule : allowed) {
            if (rule.matchesConnection(tls, normalized, port)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Splits {@code [user@]host[:port]}. Brackets of an IPv6 address and dots at the end of the host are removed.
     *
     * @return host and port (port {@code null} if absent), or {@code null} when there is no host
     */
    private static String[] splitAuthority(String authority) {
        String rest = authority;
        final int user = rest.lastIndexOf('@');
        if (user >= 0) {
            rest = rest.substring(user + 1);
        }
        String host;
        String port = null;
        if (rest.startsWith("[")) {
            final int close = rest.indexOf(']');
            if (close < 0) {
                return null;
            }
            host = rest.substring(1, close);
            if (close + 1 < rest.length()) {
                if (rest.charAt(close + 1) != ':') {
                    return null;
                }
                port = rest.substring(close + 2);
            }
        } else {
            final int colon = rest.lastIndexOf(':');
            if (colon >= 0) {
                host = rest.substring(0, colon);
                port = rest.substring(colon + 1);
            } else {
                host = rest;
            }
        }
        host = normalizeHost(host);
        if (host == null) {
            return null;
        }
        return new String[] { host, port != null && port.length() > 0 ? port : null };
    }

    private static String normalizeHost(String host) {
        if (host == null) {
            return null;
        }
        String h = host.trim().toLowerCase(Locale.ENGLISH);
        if (h.startsWith("[") && h.endsWith("]")) {
            h = h.substring(1, h.length() - 1);
        }
        while (h.endsWith(".")) {
            h = h.substring(0, h.length() - 1);
        }
        return h.length() == 0 ? null : h;
    }

    /** Lower-case host of an address with a scheme ({@code scheme://host...}), without user, port and brackets; {@code null} if none. */
    public static String hostOf(String url) {
        if (url == null) {
            return null;
        }
        final String text = url.replace('\\', '/');
        final int scheme = text.indexOf("://");
        if (scheme < 0) {
            return null;
        }
        int end = text.length();
        for (int i = scheme + 3; i < text.length(); i++) {
            final char c = text.charAt(i);
            if (c == '/' || c == '?' || c == '#') {
                end = i;
                break;
            }
        }
        final String[] hostPort = splitAuthority(text.substring(scheme + 3, end));
        return hostPort != null ? hostPort[0] : null;
    }

    /**
     * {@code true} for a host that names this PC or the local network by itself: {@code localhost}, {@code *.localhost},
     * {@code *.local}, names without a dot, and every IP address for which {@link #isLocalAddress(InetAddress)} is {@code true}. No
     * name is looked up here: a public name that resolves to a local address is only caught where the address is resolved, in the
     * filter proxy of the browser.
     */
    public static boolean isLocalNetworkHost(String host) {
        final String h = normalizeHost(host);
        if (h == null) {
            return false;
        }
        if (h.equals("localhost") || h.endsWith(".localhost") || h.endsWith(".local")) {
            return true;
        }
        final boolean ipv6 = h.indexOf(':') >= 0;
        if (ipv6 || h.matches("[0-9.]+")) {
            /* An IP literal; getByName does not ask DNS for it. It also understands short forms such as 127.1. */
            try {
                return isLocalAddress(InetAddress.getByName(ipv6 ? "[" + h + "]" : h));
            } catch (Exception notAnAddress) {
                /* Looks like an address but is none: do not let it through as "public". */
                return true;
            }
        }
        /* Single label: an intranet name such as "router" or "nas". */
        return h.indexOf('.') < 0;
    }

    /**
     * {@code true} for an address of this PC or of a local network: loopback, unspecified, private (10/8, 172.16/12, 192.168/16),
     * link-local (169.254/16, fe80::/10), carrier-grade NAT (100.64/10), unique local (fc00::/7), site-local, multicast and broadcast
     * addresses, and IPv6 addresses that carry such an IPv4 address (IPv4-mapped, IPv4-compatible, NAT64).
     */
    public static boolean isLocalAddress(InetAddress address) {
        if (address == null) {
            return false;
        }
        if (address.isLoopbackAddress() || address.isAnyLocalAddress() || address.isLinkLocalAddress() || address.isSiteLocalAddress() || address.isMulticastAddress()) {
            return true;
        }
        final byte[] b = address.getAddress();
        if (b.length == 4) {
            return isLocalIPv4(b[0] & 0xFF, b[1] & 0xFF, b[2] & 0xFF, b[3] & 0xFF);
        }
        if (b.length == 16) {
            if ((b[0] & 0xFE) == 0xFC) {
                /* fc00::/7 unique local */
                return true;
            }
            boolean zeroPrefix = true;
            for (int i = 0; i < 10; i++) {
                zeroPrefix &= b[i] == 0;
            }
            final boolean mapped = zeroPrefix && (b[10] & 0xFF) == 0xFF && (b[11] & 0xFF) == 0xFF;
            final boolean compatible = zeroPrefix && b[10] == 0 && b[11] == 0;
            final boolean nat64 = (b[0] & 0xFF) == 0x00 && (b[1] & 0xFF) == 0x64 && (b[2] & 0xFF) == 0xFF && (b[3] & 0xFF) == 0x9B && b[4] == 0 && b[5] == 0 && b[6] == 0 && b[7] == 0 && b[8] == 0 && b[9] == 0 && b[10] == 0 && b[11] == 0;
            if (mapped || compatible || nat64) {
                return isLocalIPv4(b[12] & 0xFF, b[13] & 0xFF, b[14] & 0xFF, b[15] & 0xFF);
            }
        }
        return false;
    }

    private static boolean isLocalIPv4(int a, int b, int c, int d) {
        if (a == 127 || a == 10 || a == 0 || (a == 192 && b == 168) || (a == 172 && b >= 16 && b <= 31) || (a == 169 && b == 254)) {
            return true;
        }
        if (a == 100 && b >= 64 && b <= 127) {
            /* 100.64.0.0/10 carrier-grade NAT, also used by VPN meshes */
            return true;
        }
        return a >= 224;
    }
}
