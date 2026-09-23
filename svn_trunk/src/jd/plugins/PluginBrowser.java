package jd.plugins;

import java.io.IOException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.util.Map;
import java.util.regex.Pattern;

import jd.http.BlockLevelType;
import jd.http.BlockSourceType;
import jd.http.BlockedTypeInterface;
import jd.http.Browser;
import jd.http.Request;
import jd.http.RequestHeader;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpconnection.DNSResolver;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.CompiledFiletypeExtension;

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
    protected void autoCompleteHeaders(Request request) {
        if (request == null) {
            return;
        }
        final RequestHeader requestHeaders = request.getHeaders();
        boolean addSecHeaders = false;
        final String firefoxVersionString = new Regex(requestHeaders.getValue("User-Agent"), "FireFox/(\\d+)").getMatch(0);
        final int firefoxVersion = firefoxVersionString != null ? Integer.parseInt(firefoxVersionString) : -1;
        if (firefoxVersion >= 90) {
            addSecHeaders = true;
        }
        final String chromeVersionString = new Regex(requestHeaders.getValue("User-Agent"), "Chrome/(\\d+)").getMatch(0);
        final int chromeVersion = chromeVersionString != null ? Integer.parseInt(chromeVersionString) : -1;
        if (chromeVersion >= 76) {
            addSecHeaders = true;
        }
        final String operaVersionString = new Regex(requestHeaders.getValue("User-Agent"), "OPR/(\\d+)").getMatch(0);
        final int operaVersion = operaVersionString != null ? Integer.parseInt(operaVersionString) : -1;
        if (operaVersion >= 63) {
            addSecHeaders = true;
        }
        final String edgeVersionString = new Regex(requestHeaders.getValue("User-Agent"), "Edg/(\\d+)").getMatch(0);
        final int edgeVersion = edgeVersionString != null ? Integer.parseInt(edgeVersionString) : -1;
        if (edgeVersion >= 79) {
            addSecHeaders = true;
        }
        if (!addSecHeaders) {
            return;
        }
        if (getHeaders().getHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE) == null) {
            requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_SITE, "same-origin"));
        }
        if (getHeaders().getHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST) == null) {
            final String path = request.getURL().getFile();
            final CompiledFiletypeExtension extension = CompiledFiletypeFilter.getExtensionsFilterInterface(Files.getExtension(path, true));
            if (CompiledFiletypeFilter.AudioExtensions.MP3.isSameExtensionGroup(extension)) {
                requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, "audio"));
            } else if (CompiledFiletypeFilter.VideoExtensions.MP4.isSameExtensionGroup(extension)) {
                requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, "video"));
            } else if (CompiledFiletypeFilter.ImageExtensions.BMP.isSameExtensionGroup(extension)) {
                requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, "image"));
            } else {
                requestHeaders.put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_SEC_FETCH_DEST, "document"));
            }
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
