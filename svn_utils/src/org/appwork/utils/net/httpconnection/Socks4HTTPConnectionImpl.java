/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
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
package org.appwork.utils.net.httpconnection;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URL;
import java.net.UnknownHostException;

import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.net.socketconnection.Socks4SocketConnection;
import org.appwork.utils.net.socketconnection.SocksSocketConnection;
import org.appwork.utils.net.socketconnection.SocksSocketConnection.DESTTYPE;

/**
 * @author daniel
 *
 */
public class Socks4HTTPConnectionImpl extends AbstractSocksHTTPConnection {

    public Socks4HTTPConnectionImpl(URL url, HTTPProxy proxy, DESTTYPE destType) {
        super(url, proxy, destType);
    }

    public Socks4HTTPConnectionImpl(URL url, HTTPProxy proxy) {
        super(url, proxy);
    }

    @Override
    protected Socket createRawConnectionSocket(final InetAddress bindInetAddress) throws IOException {
        final Socks4SocketConnection socket = buildSocksSocketConnection();
        socket.setSoTimeout(getReadTimeout());
        return socket;
    }

    @Override
    protected IPVERSION getEndPointIPVersion() {
        // Socks4 only supports IPv4
        return IPVERSION.IPV4_ONLY;
    }

    @Override
    protected InetSocketAddress buildConnectEndPointSocketAddress(SocksSocketConnection socksSocketConnection) throws IOException {
        final InetSocketAddress ret = super.buildConnectEndPointSocketAddress(socksSocketConnection);
        if (HTTPProxy.TYPE.SOCKS4.equals(getProxy().getType())) {
            if (ret.isUnresolved()) {
                throw new UnknownHostException("Socks4 only supports IPv4 but is not resolved(" + getEndPointIPVersion() + "):" + getHostname());
            } else if (!(ret.getAddress() instanceof Inet4Address)) {
                throw new UnknownHostException("Socks4 only supports IPv4 but is resolved IPv6(" + getEndPointIPVersion() + "):" + getHostname());
            } else {
                return ret;
            }
        } else {
            return ret;
        }
    }

    @Override
    protected SocketStreamInterface connect(SocketStreamInterface socketStream) throws IOException {
        final Socket socket = socketStream.getSocket();
        final Socks4SocketConnection socks4Socket = ((Socks4SocketConnection) socket);
        this.endPointInetSocketAddress = buildConnectEndPointSocketAddress(socks4Socket);
        socks4Socket.connect(endPointInetSocketAddress, this.getConnectTimeout(), this.proxyRequest);
        return socketStream;
    }

    @Override
    protected Socks4SocketConnection buildSocksSocketConnection() {
        final Socks4SocketConnection socket = new Socks4SocketConnection(this.getProxy(), getDestType());
        return socket;
    }
}
