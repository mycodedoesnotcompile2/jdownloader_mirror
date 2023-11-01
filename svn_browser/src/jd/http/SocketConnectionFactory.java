package jd.http;

import java.net.Proxy;
import java.net.Socket;

import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.SocksHTTPconnection;
import org.appwork.utils.net.socketconnection.DirectSocketConnection;
import org.appwork.utils.net.socketconnection.HTTPProxySocketConnection;
import org.appwork.utils.net.socketconnection.Socks4SocketConnection;
import org.appwork.utils.net.socketconnection.Socks5SocketConnection;
import org.appwork.utils.net.socketconnection.SocksSocketConnection;

public class SocketConnectionFactory {
    private static boolean requiresBackwardsCompatibility = SocketConnectionFactory.requiresBackwardsCompatibility();

    public static Socket createSocket(final HTTPProxy proxy) {
        if (proxy == null) {
            return new DirectSocketConnection();
        } else {
            if (proxy.isPreferNativeImplementation()) {
                final Proxy nativeProxy = proxy.toNativeProxy();
                if (nativeProxy != null) {
                    return new Socket(nativeProxy);
                } else {
                    return new Socket();
                }
            } else {
                switch (proxy.getType()) {
                case NONE:
                case DIRECT:
                    return new DirectSocketConnection(proxy);
                case HTTP:
                case HTTPS:
                    return new HTTPProxySocketConnection(proxy);
                case SOCKS4:
                    // only supports IPv4 and resolved IPs
                    if (SocketConnectionFactory.requiresBackwardsCompatibility) {
                        return new Socks4SocketConnection(proxy, SocksHTTPconnection.DESTTYPE.IPV4);
                    } else {
                        return new Socks4SocketConnection(proxy, SocksSocketConnection.DESTTYPE.IPV4);
                    }
                case SOCKS4A:
                    if (SocketConnectionFactory.requiresBackwardsCompatibility) {
                        return new Socks4SocketConnection(proxy, SocksHTTPconnection.DESTTYPE.AUTO);
                    } else {
                        return new Socks4SocketConnection(proxy, SocksSocketConnection.DESTTYPE.AUTO);
                    }
                case SOCKS5:
                    if (SocketConnectionFactory.requiresBackwardsCompatibility) {
                        return new Socks5SocketConnection(proxy, SocksHTTPconnection.DESTTYPE.AUTO);
                    } else {
                        return new Socks5SocketConnection(proxy, SocksSocketConnection.DESTTYPE.AUTO);
                    }
                default:
                    throw new RuntimeException("unsupported proxy type: " + proxy.getType().name());
                }
            }
        }
    }

    private static boolean requiresBackwardsCompatibility() {
        try {
            Class.forName("org.appwork.utils.net.socketconnection.SocksSocketConnection.DESTTYPE");
            return false;
        } catch (Throwable e) {
            return true;
        }
    }
}
