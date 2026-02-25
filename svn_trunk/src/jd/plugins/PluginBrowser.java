package jd.plugins;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.URL;
import java.util.Map;
import java.util.regex.Pattern;

import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.URLConnectionAdapterDirectImpl;
import jd.http.URLConnectionAdapterHTTPProxyImpl;
import jd.http.URLConnectionAdapterNative;
import jd.http.URLConnectionAdapterSocks4Impl;
import jd.http.URLConnectionAdapterSocks5Impl;
import jd.parser.Regex;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;

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
                // https://schema.org/VideoObject
                if (map != null && "VideoObject".equals(map.get("@type"))) {
                    return map;
                }
            } catch (Exception e) {
                getPlugin().getLogger().log(e);
            }
        }
        return null;
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

    @Override
    public URLConnectionAdapter createHTTPConnection(final Request request, HTTPProxy proxy) throws IOException {
        final class LocalHostChecker {
            public InetSocketAddress check(final InetSocketAddress remoteIP) throws IOException {
                if ("localhost".equalsIgnoreCase(request.getURL().getHost())) {
                    return remoteIP;
                }
                final InetAddress[] rawIPInetAddress = HTTPConnectionImpl.resolveLiteralIP(request.getURL().getHost());
                if (rawIPInetAddress != null && rawIPInetAddress[0].isLoopbackAddress()) {
                    return remoteIP;
                }
                if (remoteIP.isUnresolved() || !remoteIP.getAddress().isLoopbackAddress()) {
                    return remoteIP;
                }
                if (remoteIP.getAddress().toString().startsWith("127.42.")) {
                    throw new BlockedByException(request, GenericSupportedBlockTypes.MALWAREBYTES_LOCALHOST_DNS);
                }
                throw new BlockedByException(request, GenericSupportedBlockTypes.GENERIC_LOCALHOST_DNS);
            }
        }
        final LocalHostChecker checker = new LocalHostChecker();
        final URL url = URLHelper.getURL(request.getURL(), true, false, false);
        if (proxy == null) {
            return new URLConnectionAdapterDirectImpl(url) {
                @Override
                protected void connectEndPoint(SocketStreamInterface socket, InetSocketAddress connectedInetSocketAddress, int requestedConnectTimeout) throws IOException {
                    super.connectEndPoint(socket, checker.check(connectedInetSocketAddress), requestedConnectTimeout);
                }
            };
        } else {
            if (proxy.isPreferNativeImplementation()) {
                return new URLConnectionAdapterNative(url, proxy);
            } else {
                switch (proxy.getType()) {
                case NONE:
                case DIRECT:
                    return new URLConnectionAdapterDirectImpl(url, proxy) {
                        @Override
                        protected void connectEndPoint(SocketStreamInterface socket, InetSocketAddress connectedInetSocketAddress, int requestedConnectTimeout) throws IOException {
                            super.connectEndPoint(socket, checker.check(connectedInetSocketAddress), requestedConnectTimeout);
                        }
                    };
                case HTTP:
                case HTTPS:
                    return new URLConnectionAdapterHTTPProxyImpl(url, proxy);
                case SOCKS4:
                case SOCKS4A:
                    return new URLConnectionAdapterSocks4Impl(url, proxy) {
                        @Override
                        protected void connectEndPoint(SocketStreamInterface socket, InetSocketAddress connectedInetSocketAddress, int requestedConnectTimeout) throws IOException {
                            super.connectEndPoint(socket, checker.check(connectedInetSocketAddress), requestedConnectTimeout);
                        }
                    };
                case SOCKS5:
                    return new URLConnectionAdapterSocks5Impl(url, proxy) {
                        @Override
                        protected void connectEndPoint(SocketStreamInterface socket, InetSocketAddress connectedInetSocketAddress, int requestedConnectTimeout) throws IOException {
                            super.connectEndPoint(socket, checker.check(connectedInetSocketAddress), requestedConnectTimeout);
                        }
                    };
                default:
                    throw new RuntimeException("unsupported proxy type: " + proxy.getType().name());
                }
            }
        }
    }
}
