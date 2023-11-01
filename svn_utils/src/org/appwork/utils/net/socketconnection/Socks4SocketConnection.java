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
package org.appwork.utils.net.socketconnection;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;

import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.ProxyConnectException;
import org.appwork.utils.net.httpconnection.ProxyEndpointConnectException;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;
import org.appwork.utils.net.httpconnection.SocksHTTPconnection;

/**
 * @author daniel
 *
 */
public class Socks4SocketConnection extends SocksSocketConnection {

    public static enum CONNECT_ERROR {
        REJECTED("Socks4 request rejected or failed"),
        CLIENT_IDENTD_UNREACHABLE("Socks4 request failed because client is not running identd (or not reachable from the server)"),
        CLIENT_IDENTD_AUTH("Socks4 request failed because client's identd could not confirm the user ID string in the request"),
        UNKNOWN("Unknown");
        private final String msg;

        private CONNECT_ERROR(String msg) {
            this.msg = msg;
        }

        public String getMsg() {
            return msg;
        }
    }

    public class Socks4EndpointConnectException extends ConnectException {
        private static final long   serialVersionUID = -1993301003920927143L;
        private final CONNECT_ERROR error;

        private Socks4EndpointConnectException(CONNECT_ERROR error) {
            super(error.msg);
            this.error = error;
        }

        private Socks4EndpointConnectException(CONNECT_ERROR error, String msg) {
            super(error.msg + ":" + msg);
            this.error = error;
        }

        public CONNECT_ERROR getError() {
            return error;
        }

        public HTTPProxy getProxy() {
            return Socks4SocketConnection.this.getProxy();
        }
    }

    public DESTTYPE getDestType(final SocketAddress endpoint) {
        final DESTTYPE destType = getDestType();
        if (endpoint != null && endpoint instanceof InetSocketAddress) {
            final InetSocketAddress inetSocketAddress = (InetSocketAddress) endpoint;
            if (inetSocketAddress.getAddress() != null) {
                switch (inetSocketAddress.getAddress().getAddress().length) {
                case 4:
                    if (isSupported(DESTTYPE.IPV4) && (DESTTYPE.IPV4.equals(destType) || DESTTYPE.AUTO.equals(destType))) {
                        return DESTTYPE.IPV4;
                    }
                    break;
                case 16:
                    // socks4/socks4a does not support IPv6
                    return DESTTYPE.DOMAIN;
                }
            }
        }
        return destType;
    }

    public Socks4SocketConnection(HTTPProxy proxy, DESTTYPE destType) {
        super(proxy, destType);
    }

    @Deprecated
    public Socks4SocketConnection(HTTPProxy proxy, SocksHTTPconnection.DESTTYPE destType) {
        super(proxy, SocksHTTPconnection.DESTTYPE.convert(destType));
    }

    public Socks4SocketConnection(HTTPProxy proxy) {
        this(proxy, DESTTYPE.AUTO);
    }

    @Override
    protected DESTTYPE initSocksSocketConnection(HTTPProxy proxy, DESTTYPE destType) {
        if (destType == null) {
            destType = DESTTYPE.AUTO;
        }
        supportedDestType.add(DESTTYPE.AUTO);
        supportedDestType.add(DESTTYPE.IPV4);
        if (HTTPProxy.TYPE.SOCKS4A.equals(getProxy().getType())) {
            supportedDestType.add(DESTTYPE.DOMAIN);
        }
        if (!supportedDestType.contains(destType)) {
            throw new IllegalArgumentException("Unsupported dest type:" + getDestType());
        } else {
            return destType;
        }
    }

    @Override
    protected HTTPProxy isProxySupported(HTTPProxy proxy) {
        if (proxy != null && (HTTPProxy.TYPE.SOCKS4.equals(proxy.getType()) || HTTPProxy.TYPE.SOCKS4A.equals(proxy.getType()))) {
            return proxy;
        } else {
            throw new IllegalArgumentException("proxy must be of type socks4/socks4a:" + proxy);
        }
    }

    @Override
    protected SocketStreamInterface connectProxySocket(final SocketStreamInterface proxySocket, final SocketAddress endPoint, final StringBuffer logger) throws IOException {
        final HTTPProxy proxy = getProxy();
        try {
            return establishConnection(this, proxySocket, proxy.getUser(), setEndPointSocketAddress(endPoint), this.getDestType(endPoint), logger);
        } catch (ProxyConnectException e) {
            throw e;
        } catch (final Socks4EndpointConnectException e) {
            throw new ProxyEndpointConnectException(e, proxy, endPoint);
        } catch (final IOException e) {
            throw new ProxyConnectException(e, proxy);
        }
    }

    protected SocketStreamInterface establishConnection(Socks4SocketConnection sock4SocketConnection, final SocketStreamInterface proxySocket, final String userID, final SocketAddress endPoint, final DESTTYPE destType, final StringBuffer logger) throws IOException {
        final InetSocketAddress endPointAddress = (InetSocketAddress) endPoint;
        final OutputStream os = proxySocket.getOutputStream();
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        if (logger != null) {
            logger.append("->" + getProxy().getType() + " Hello to:" + SocketConnection.getRootEndPointSocketAddress(proxySocket) + "\r\n");
        }
        /* socks4 */
        bos.write((byte) 4);
        bos.write((byte) 1);
        /* send port */
        /* network byte order */
        final int port = endPointAddress.getPort();
        bos.write(port >> 8 & 0xff);
        bos.write(port & 0xff);
        /* send ipv4/domain */
        final DESTTYPE usedDestType;
        switch (destType) {
        case AUTO:
        case IPV4:
            if (((DESTTYPE.AUTO.equals(destType) && isSupported(DESTTYPE.IPV4)) || DESTTYPE.IPV4.equals(destType))) {
                final InetAddress ipv4 = endPointAddress.getAddress();
                if (ipv4 != null && ipv4.getAddress().length == 4) {
                    if (logger != null) {
                        logger.append("->SEND tcp connect request to:" + endPoint + " by ipv4:" + ipv4.getHostAddress() + "\r\n");
                    }
                    bos.write(ipv4.getAddress());
                    usedDestType = DESTTYPE.IPV4;
                    break;
                } else if (DESTTYPE.IPV4.equals(destType)) {
                    if (logger != null) {
                        if (ipv4 == null) {
                            logger.append("->Cannot connect request by ipv4 (unresolved)\r\n");
                        } else {
                            logger.append("->Cannot connect request by ipv4 (no ipv4)\r\n");
                        }
                    }
                }
            }
        case IPV6:
            if (DESTTYPE.IPV6.equals(destType)) {
                if (logger != null) {
                    logger.append("->Skip Unsupported destType:" + destType + "\r\n");
                }
            }
        case DOMAIN:
            if (HTTPProxy.TYPE.SOCKS4A.equals(getProxy().getType()) && ((DESTTYPE.AUTO.equals(destType) && isSupported(DESTTYPE.DOMAIN)) || DESTTYPE.DOMAIN.equals(destType))) {
                /* we use domain, signaled via invalid 0.0.0.X IP */
                bos.write((byte) 0);
                bos.write((byte) 0);
                bos.write((byte) 0);
                bos.write((byte) 1);
                if (logger != null) {
                    logger.append("->SEND tcp connect request to:" + endPoint + " by domain:" + SocketConnection.getHostName(endPointAddress) + "\r\n");
                }
                usedDestType = DESTTYPE.DOMAIN;
                break;
            }
        default:
            throw new IllegalArgumentException("Unsupported destType:" + destType + "|endPoint:" + endPointAddress);
        }
        /* send user ID string */
        if (userID != null && userID.length() > 0) {
            bos.write(userID.getBytes(ISO_8859_1));
        }
        /* NULL/end */
        bos.write((byte) 0);
        if (DESTTYPE.DOMAIN.equals(usedDestType)) {
            final byte[] domainBytes = SocketConnection.getHostName(endPointAddress).getBytes("ISO-8859-1");
            /* send domain as string,socks4a */
            bos.write(domainBytes);
            /* NULL/end */
            bos.write((byte) 0);
        }
        bos.writeTo(os);
        os.flush();
        /* read response, 8 bytes */
        final InputStream is = proxySocket.getInputStream();
        final byte[] read;
        try {
            read = SocketConnection.ensureRead(is, 2, null);
        } catch (final IOException e) {
            throw new ProxyEndpointConnectException(e, getProxy(), endPoint);
        }
        final int[] resp = SocketConnection.byteArrayToIntArray(read);
        if (resp[0] != 0) {
            throw new IOException("Invalid response:" + resp[0]);
        }
        switch (resp[1]) {
        case 0x5a:
            break;
        case 0x5b:
            throw new Socks4EndpointConnectException(CONNECT_ERROR.REJECTED);
        case 0x5c:
            throw new Socks4EndpointConnectException(CONNECT_ERROR.CLIENT_IDENTD_UNREACHABLE);
        case 0x5d:
            throw new Socks4EndpointConnectException(CONNECT_ERROR.CLIENT_IDENTD_AUTH);
        default:
            throw new Socks4EndpointConnectException(CONNECT_ERROR.UNKNOWN, String.valueOf(resp[1]));
        }
        /* port */
        final byte[] connectedPort = SocketConnection.ensureRead(is, 2, null);
        /* ip4v response */
        final byte[] connectedIP = SocketConnection.ensureRead(is, 4, null);
        if (logger != null) {
            logger.append("<-BOUND IP:" + InetAddress.getByAddress(connectedIP) + ":" + (ByteBuffer.wrap(connectedPort).getShort() & 0xffff) + "\r\n");
        }
        return proxySocket;
    }
}
