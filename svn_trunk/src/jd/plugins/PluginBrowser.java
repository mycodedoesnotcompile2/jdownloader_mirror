package jd.plugins;

import java.io.IOException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.URL;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpconnection.DNSResolver;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.CompiledFiletypeExtension;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

import jd.http.BlockLevelType;
import jd.http.BlockSourceType;
import jd.http.BlockedTypeInterface;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.RequestHeader;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;

public class PluginBrowser<T extends Plugin> extends Browser {
    private final T plugin;

    public T getPlugin() {
        return plugin;
    }

    public PluginBrowser(final T plugin) {
        super();
        this.plugin = plugin;
    }

    @Override
    public Browser createNewBrowserInstance() {
        return getPlugin().createNewBrowserInstance();
    }

    public Map<String, Object> getVideoObject() {
        final String[] ldJSONs = getRegex("<script type\\s*=\\s*\"application/ld\\+json\"[^>]*>\\s*(\\{.*?\\})\\s*</script>").getColumn(0);
        for (final String ldJSON : ldJSONs) {
            try {
                final Map<String, Object> map = getPlugin().restoreFromString(ldJSON, TypeRef.MAP);
                if (map == null) {
                    continue;
                }
                // https://schema.org/VideoObject
                if ("VideoObject".equals(map.get("@type"))) {
                    return map;
                }
            } catch (Exception e) {
                getPlugin().getLogger().log(e);
            }
        }
        return null;
    }

    @Override
    public void setCookies(String url, Cookies iCookies) {
        final Plugin plugin = getPlugin();
        setCookiesOnSiteSupportedNames: if (plugin.getHost().equals(url) && !plugin.hasFeature(FEATURE.GENERIC)) {
            final String[] siteSupportedNames = plugin.siteSupportedNames();
            if (siteSupportedNames == null || siteSupportedNames.length < 2) {
                break setCookiesOnSiteSupportedNames;
            }
            for (final String siteSupportedName : siteSupportedNames) {
                if (siteSupportedName == null || siteSupportedName.equals(url)) {
                    continue;
                }
                super.setCookies(siteSupportedName, iCookies);
            }
        }
        super.setCookies(url, iCookies);
    }

    @Override
    public void setCookies(Cookies iCookies) {
        setCookies(getPlugin().getHost(), iCookies);
    }

    protected final class SecChUaGenerator {
        private final char[]   GREASE_CHARS    = { ' ', '(', ')', '-', '.', '/', ':', ';', '=', '?', '_' };
        private final String[] GREASE_VERSIONS = { "8", "99", "24" };
        // Alle 6 Permutationen für 3 Elemente (Indizes 0, 1, 2)
        private final int[][]  PERMUTATIONS    = { { 0, 1, 2 }, { 0, 2, 1 }, { 1, 0, 2 }, { 1, 2, 0 }, { 2, 0, 1 }, { 2, 1, 0 } };

        protected SecChUaGenerator() {
        }

        /**
         * Erzeugt den Sec-CH-UA Header für Google Chrome anhand der Major-Version.
         *
         * @param majorVersion
         *            z. B. 130 oder 156
         * @return fertiger Header-Wert, z. B. "\"Chromium\";v=\"130\", \"Google Chrome\";v=\"130\", \"Not?A_Brand\";v=\"99\""
         */
        public String buildChromeSecChUa(int majorVersion) {
            return buildSecChUa("Google Chrome", majorVersion, null);
        }

        /**
         * Erzeugt den Sec-CH-UA-Full-Version-List Header für Google Chrome.
         *
         * @param majorVersion
         *            z. B. 130
         * @param fullVersion
         *            z. B. "130.0.6723.91"
         */
        public String buildChromeFullVersionList(int majorVersion, String fullVersion) {
            return buildSecChUa("Google Chrome", majorVersion, fullVersion);
        }

        /**
         * Generische Variante für beliebige Chromium-Browser ("Google Chrome", "Microsoft Edge", "Brave", "Opera").
         *
         * @param productBrand
         *            z. B. "Google Chrome"
         * @param majorVersion
         *            Major-Version als int
         * @param fullVersion
         *            null für Sec-CH-UA (Low-Entropy), oder vollständige Version (z. B. "130.0.6723.91") für Full-Version-List
         */
        public String buildSecChUa(String productBrand, int majorVersion, String fullVersion) {
            // 1. GREASE Brand-String & Version berechnen (Chromium-Logik seit ~Chrome 103+)
            char c1 = GREASE_CHARS[majorVersion % GREASE_CHARS.length];
            char c2 = GREASE_CHARS[(majorVersion + 1) % GREASE_CHARS.length];
            String greaseBrand = "Not" + c1 + "A" + c2 + "Brand";
            String greaseBaseVersion = GREASE_VERSIONS[majorVersion % GREASE_VERSIONS.length];
            String greaseVersion = (fullVersion != null) ? (greaseBaseVersion + ".0.0.0") : greaseBaseVersion;
            // 2. Engine- und Produkt-Version bestimmen
            String brandVersion = (fullVersion != null) ? fullVersion : Integer.toString(majorVersion);
            // 3. Die 3 Einträge vor Permutation: [0 = GREASE, 1 = Chromium, 2 = Produkt]
            String[] brands = { greaseBrand, "Chromium", productBrand };
            String[] versions = { greaseVersion, brandVersion, brandVersion };
            // 4. Deterministische Permutation anhand majorVersion % 6
            int[] order = PERMUTATIONS[majorVersion % PERMUTATIONS.length];
            StringBuilder sb = new StringBuilder(96);
            for (int i = 0; i < 3; i++) {
                if (i > 0) {
                    sb.append(", ");
                }
                int idx = order[i];
                sb.append('"').append(brands[idx]).append("\";v=\"").append(versions[idx]).append('"');
            }
            return sb.toString();
        }
    }

    protected boolean isRequestHeaderForAutoCompleteSet(final Request request, final String headerKey) {
        return getHeaders().getHeader(headerKey) != null || request.getHeaders().getHeader(headerKey) != null;
    }

    @Override
    protected void autoCompleteHeaders(Request request) throws IOException {
        if (request == null || !"https".equalsIgnoreCase(request.getURL().getProtocol())) {
            return;
        }
        final RequestHeader requestHeaders = request.getHeaders();
        final String userAgent = requestHeaders.getValue(HTTPConstants.HEADER_REQUEST_USER_AGENT);
        boolean addSecHeaders = false;
        final String firefoxVersionString = new Regex(userAgent, "FireFox/(\\d+)").getMatch(0);
        final int firefoxVersion = firefoxVersionString != null ? Integer.parseInt(firefoxVersionString) : -1;
        if (firefoxVersion >= 90) {
            addSecHeaders = true;
        }
        final String chromeVersionString = new Regex(userAgent, "Chrome/(\\d+)").getMatch(0);
        final int chromeVersion = chromeVersionString != null ? Integer.parseInt(chromeVersionString) : -1;
        if (chromeVersion >= 76) {
            addSecHeaders = true;
        }
        final String operaVersionString = new Regex(userAgent, "OPR/(\\d+)").getMatch(0);
        final int operaVersion = operaVersionString != null ? Integer.parseInt(operaVersionString) : -1;
        if (operaVersion >= 63) {
            addSecHeaders = true;
        }
        final String edgeVersionString = new Regex(userAgent, "Edg/(\\d+)").getMatch(0);
        final int edgeVersion = edgeVersionString != null ? Integer.parseInt(edgeVersionString) : -1;
        if (edgeVersion >= 79) {
            addSecHeaders = true;
        }
        if (!addSecHeaders) {
            return;
        }
        if (chromeVersion >= 103) {
            if (!isRequestHeaderForAutoCompleteSet(request, "sec-ch-ua")) {
                requestHeaders.put("sec-ch-ua", new SecChUaGenerator().buildChromeSecChUa(chromeVersion));
            }
            if (!isRequestHeaderForAutoCompleteSet(request, "sec-ch-ua-mobile")) {
                requestHeaders.put("sec-ch-ua-mobile", "?0");
            }
            if (!isRequestHeaderForAutoCompleteSet(request, "sec-ch-ua-platform")) {
                final String platform;
                if (StringUtils.containsIgnoreCase(userAgent, "Windows")) {
                    platform = "Windows";
                } else if (StringUtils.containsIgnoreCase(userAgent, "Macintosh") || StringUtils.containsIgnoreCase(userAgent, "Mac OS")) {
                    platform = "macOS";
                } else if (StringUtils.containsIgnoreCase(userAgent, "Linux")) {
                    platform = "Linux";
                } else {
                    platform = "Unknown";
                }
                requestHeaders.put("sec-ch-ua-platform", "\"" + platform + "\"");
            }
        }
        if (!isRequestHeaderForAutoCompleteSet(request, HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST)) {
            final String fetchDest;
            if (request.isXHR()) {
                fetchDest = "empty";
            } else {
                final String path = request.getURL().getFile();
                final CompiledFiletypeExtension extension = CompiledFiletypeFilter.getExtensionsFilterInterface(Files.getExtension(path, true));
                if (CompiledFiletypeFilter.AudioExtensions.MP3.isSameExtensionGroup(extension)) {
                    fetchDest = "audio";
                } else if (CompiledFiletypeFilter.VideoExtensions.MP4.isSameExtensionGroup(extension)) {
                    fetchDest = "video";
                } else if (CompiledFiletypeFilter.ImageExtensions.BMP.isSameExtensionGroup(extension)) {
                    fetchDest = "image";
                } else {
                    fetchDest = "document";
                }
            }
            requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, fetchDest));
        }
        if (!isRequestHeaderForAutoCompleteSet(request, HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE)) {
            if (request.isXHR()) {
                requestHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, "cors");
            } else {
                requestHeaders.put(HTTPConstants.HEADER_REQUEST_SEC_FETCH_MODE, "cors");
            }
        }
        if (!isRequestHeaderForAutoCompleteSet(request, HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE)) {
            final String refererURL = getRefererURL(request);
            final String fetchSite;
            if (StringUtils.isEmpty(refererURL)) {
                fetchSite = "none";
            } else if (!StringUtils.equalsIgnoreCase(new URL(refererURL).getProtocol(), request.getURL().getProtocol())) {
                fetchSite = "cross-site";
            } else if (StringUtils.equalsIgnoreCase(new URL(refererURL).getHost(), request.getURL().getHost())) {
                fetchSite = "same-origin";
            } else if (StringUtils.equalsIgnoreCase(getHost(refererURL), getHost(request.getURL()))) {
                fetchSite = "same-site";
            } else {
                fetchSite = "cross-site";
            }
            requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, fetchSite));
        }
    }

    public final static class SunriseChecker implements BlockedTypeInterface {
        private final static BlockedTypeInterface TYPE = new SunriseChecker();

        @Override
        public BlockedTypeInterface isBlocked(Browser browser, Request request) {
            return null;
        }

        @Override
        public String getLabel() {
            return "sunrise.ch DNS block";
        }

        @Override
        public BlockLevelType getBlockLevelType() {
            return BlockLevelType.DNS;
        }

        @Override
        public BlockSourceType getBlockSourceType() {
            return BlockSourceType.ISP;
        }

        @Override
        public Boolean prepareBlockDetection(Browser browser, Request request) {
            return null;
        }

        private static boolean isIpInRange(String ip, String startIp, String endIp) {
            final long ipToTest = ipToLong(ip);
            final long start = ipToLong(startIp);
            final long end = ipToLong(endIp);
            return ipToTest >= start && ipToTest <= end;
        }

        private static long ipToLong(String ipAddress) {
            final String[] octets = ipAddress.split("\\.");
            long result = 0;
            for (final String octet : octets) {
                result <<= 8;
                result |= Integer.parseInt(octet);
            }
            return result;
        }

        public static void check(final Browser br, final Request request, final HTTPProxy proxy, final InetAddress remoteIP) throws IOException {
            if (remoteIP instanceof Inet6Address) {
                return;
            }
            final InetAddress[] rawIPInetAddress = HTTPConnectionImpl.resolveLiteralIP(request.getURL().getHost());
            if (rawIPInetAddress != null) {
                return;
            }
            if (isIpInRange(remoteIP.getHostAddress(), "194.230.0.0", "194.230.3.255")) {
                throw br.new BlockedByException(request, TYPE);
            }
        }
    };

    public final static class LocalHostChecker {
        public static void check(final Browser br, final Request request, final HTTPProxy proxy, final InetAddress remoteIP) throws IOException {
            if (!remoteIP.isLoopbackAddress()) {
                if (remoteIP.isAnyLocalAddress()) {
                    throw br.new BlockedByException(request, GenericSupportedBlockTypes.GENERIC_LOCALHOST_DNS);
                }
                return;
            }
            {
                if ("localhost".equalsIgnoreCase(request.getURL().getHost())) {
                    return;
                }
                final InetAddress[] rawIPInetAddress = HTTPConnectionImpl.resolveLiteralIP(request.getURL().getHost());
                if (rawIPInetAddress != null && rawIPInetAddress[0].isLoopbackAddress()) {
                    return;
                }
            }
            if (remoteIP.getHostAddress().startsWith("127.42.")) {
                throw br.new BlockedByException(request, GenericSupportedBlockTypes.MALWAREBYTES_LOCALHOST_DNS);
            } else {
                throw br.new BlockedByException(request, GenericSupportedBlockTypes.GENERIC_LOCALHOST_DNS);
            }
        }
    }

    @Override
    public Regex getRegex(Pattern compile) {
        final Request request = getRequest();
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && request != null && RequestMethod.HEAD.equals(request.getRequestMethod())) {
            getPlugin().getLogger().log(new Exception("FIXME: getRegex on HEAD request"));
        }
        return super.getRegex(compile);
    }

    @Override
    public Regex getRegex(String string) {
        final Request request = getRequest();
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && request != null && RequestMethod.HEAD.equals(request.getRequestMethod())) {
            getPlugin().getLogger().log(new Exception("FIXME: getRegex on HEAD request"));
        }
        return super.getRegex(string);
    }

    @Override
    public boolean containsHTML(String regex) {
        final Request request = getRequest();
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && request != null && RequestMethod.HEAD.equals(request.getRequestMethod())) {
            getPlugin().getLogger().log(new Exception("FIXME: containsHTML on HEAD request"));
        }
        return super.containsHTML(regex);
    }

    protected DNSResolver getDNSResolver(final Request request, final HTTPProxy proxy) throws IOException {
        final DNSResolver resolver = new DNSResolver() {
            @Override
            public InetAddress[] resolveDomain(REQUESTOR requestor, IPVERSION ipVersion, String domain) throws IOException {
                final InetAddress[] ret = DEFAULT.resolveDomain(requestor, ipVersion, domain);
                if (ret == null) {
                    return null;
                }
                for (final InetAddress inetAddress : ret) {
                    LocalHostChecker.check(PluginBrowser.this, request, proxy, inetAddress);
                    SunriseChecker.check(PluginBrowser.this, request, proxy, inetAddress);
                }
                return ret;
            }
        };
        return resolver;
    }

    @Override
    public URLConnectionAdapter createHTTPConnection(final Request request, HTTPProxy proxy) throws IOException {
        final URLConnectionAdapter ret = super.createHTTPConnection(request, proxy);
        ret.setDNSResolver(getDNSResolver(request, proxy));
        return ret;
    }
}
