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

import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.ProxyAuthException;
import org.appwork.utils.net.httpconnection.ProxyConnectException;
import org.appwork.utils.net.httpconnection.ProxyEndpointConnectException;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;
import org.appwork.utils.net.httpconnection.SocksHTTPconnection;

/**
 * @author daniel
 *
 */
public class Socks5SocketConnection extends SocksSocketConnection {
    public static enum CONNECT_ERROR {
        GENERAL_SERVER_FAILURE("Socks5 general server failure"),
        NOT_ALLOWED_BY_RULESET("Socks5 connection not allowed by ruleset"),
        NETWORK_UNREACHABLE("Network is unreachable"),
        HOST_UNREACHABLE("Host is unreachable"),
        CONNECTION_REFUSED("Connection refused"),
        TTL_EXPIRED("TTL expired"),
        COMMMAND_NOT_SUPPORTED("Command not supported"),
        ADDRESS_TYPE_NOT_SUPPORTED("Address type not supported"),
        ADDRESS_TYPE_NOT_SUPPORTED_IPV4("Address type not supported: IPv4"),
        ADDRESS_TYPE_NOT_SUPPORTED_IPV6("Address type not supported: IPv6"),
        ADDRESS_TYPE_NOT_SUPPORTED_DOMAIN("Address type not supported: Domain"),
        UNKNOWN("Unknown");
        private final String msg;

        private CONNECT_ERROR(String msg) {
            this.msg = msg;
        }

        public String getMsg() {
            return msg;
        }
    }

    public static enum AUTH_ERROR {
        ERROR,
        MISSING_PLAIN,
        DENIED,
        UNSUPPORTED
    }

    public class Socks5AuthException extends IOException {
        private static final long serialVersionUID = -6926931806394311910L;
        private final AUTH_ERROR  error;

        private Socks5AuthException(AUTH_ERROR error) {
            super(error.name());
            this.error = error;
        }

        private Socks5AuthException(AUTH_ERROR error, final String msg) {
            super(error.name() + ":" + msg);
            this.error = error;
        }

        public AUTH_ERROR getError() {
            return error;
        }

        public HTTPProxy getProxy() {
            return Socks5SocketConnection.this.getProxy();
        }
    }

    public class Socks5EndpointConnectException extends ConnectException {
        private static final long   serialVersionUID = -1993301003920927143L;
        private final CONNECT_ERROR error;
        private final DESTTYPE      destType;

        private Socks5EndpointConnectException(CONNECT_ERROR error) {
            this(error, null);
        }

        private Socks5EndpointConnectException(CONNECT_ERROR error, DESTTYPE destType) {
            super(error.msg);
            this.error = error;
            this.destType = destType;
        }

        private Socks5EndpointConnectException(CONNECT_ERROR error, DESTTYPE destType, String msg) {
            super(error.msg + ":" + msg);
            this.error = error;
            this.destType = destType;
        }

        public CONNECT_ERROR getError() {
            return error;
        }

        public HTTPProxy getProxy() {
            return Socks5SocketConnection.this.getProxy();
        }

        public DESTTYPE getDestType() {
            return destType;
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
                    if (isSupported(DESTTYPE.IPV6) && (DESTTYPE.IPV6.equals(destType) || DESTTYPE.AUTO.equals(destType))) {
                        return DESTTYPE.IPV6;
                    }
                    break;
                }
            }
        }
        return destType;
    }

    @Override
    protected boolean retryConnectProxySocket(IOException e, SocketStreamInterface connectSocket, SocketAddress endpoint) {
        final Socks5EndpointConnectException connectException = Exceptions.getInstanceof(e, Socks5EndpointConnectException.class);
        if (connectException != null) {
            switch (connectException.getError()) {
            case ADDRESS_TYPE_NOT_SUPPORTED_IPV4:
                return supportedDestType.remove(DESTTYPE.IPV4);
            case ADDRESS_TYPE_NOT_SUPPORTED_IPV6:
                return supportedDestType.remove(DESTTYPE.IPV6);
            case ADDRESS_TYPE_NOT_SUPPORTED_DOMAIN:
                return supportedDestType.remove(DESTTYPE.DOMAIN);
            case NETWORK_UNREACHABLE:
            case ADDRESS_TYPE_NOT_SUPPORTED:
                if (connectException.getDestType() != null) {
                    switch (connectException.getDestType()) {
                    case IPV4:
                    case IPV6:
                        return supportedDestType.remove(connectException.getDestType());
                    default:
                        return false;
                    }
                }
                return false;
            default:
                return false;
            }
        }
        return false;
    }

    public Socks5SocketConnection(HTTPProxy proxy, DESTTYPE destType) {
        super(proxy, destType);
    }

    @Deprecated
    public Socks5SocketConnection(HTTPProxy proxy, SocksHTTPconnection.DESTTYPE destType) {
        super(proxy, SocksHTTPconnection.DESTTYPE.convert(destType));
    }

    public Socks5SocketConnection(HTTPProxy proxy) {
        this(proxy, DESTTYPE.AUTO);
    }

    @Override
    protected DESTTYPE initSocksSocketConnection(HTTPProxy proxy, DESTTYPE destType) {
        if (destType == null) {
            destType = DESTTYPE.AUTO;
        }
        supportedDestType.add(DESTTYPE.AUTO);
        supportedDestType.add(DESTTYPE.IPV4);
        supportedDestType.add(DESTTYPE.IPV6);
        supportedDestType.add(DESTTYPE.DOMAIN);
        if (!supportedDestType.contains(destType)) {
            throw new IllegalArgumentException("Unsupported dest type:" + destType);
        } else {
            return destType;
        }
    }

    @Override
    protected HTTPProxy isProxySupported(HTTPProxy proxy) {
        if (proxy != null && HTTPProxy.TYPE.SOCKS5.equals(proxy.getType())) {
            return proxy;
        } else {
            throw new IllegalArgumentException("proxy must be of type socks5:" + proxy);
        }
    }

    @Override
    protected SocketStreamInterface connectProxySocket(final SocketStreamInterface proxySocket, final SocketAddress endPoint, final StringBuffer logger) throws IOException {
        final AUTH authOffer;
        final HTTPProxy proxy = getProxy();
        final String userName = proxy.getUser();
        final String passWord = proxy.getPass();
        if (!StringUtils.isEmpty(userName) || !StringUtils.isEmpty(passWord)) {
            authOffer = AUTH.PLAIN;
        } else {
            authOffer = AUTH.NONE;
        }
        try {
            final AUTH authRequest = sayHello(proxySocket, authOffer, logger);
            switch (authRequest) {
            case NONE:
                break;
            case PLAIN:
                switch (authOffer) {
                case NONE:
                    throw new Socks5AuthException(AUTH_ERROR.MISSING_PLAIN);
                case PLAIN:
                    authPlain(proxySocket, userName, passWord, logger);
                    break;
                }
                break;
            }
            return establishConnection(this, proxySocket, setEndPointSocketAddress(endPoint), this.getDestType(endPoint), logger);
        } catch (ProxyConnectException e) {
            throw e;
        } catch (final Socks5AuthException e) {
            throw new ProxyAuthException(e, proxy);
        } catch (final Socks5EndpointConnectException e) {
            throw new ProxyEndpointConnectException(e, getProxy(), endPoint);
        } catch (final IOException e) {
            throw new ProxyConnectException(e, this.getProxy());
        }
    }

    protected SocketStreamInterface establishConnection(Socks5SocketConnection socks5SocketConnection, final SocketStreamInterface proxySocket, final SocketAddress endpoint, final DESTTYPE destType, final StringBuffer logger) throws IOException {
        final InetSocketAddress endPointAddress = (InetSocketAddress) endpoint;
        final OutputStream os = proxySocket.getOutputStream();
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        /* socks5 */
        bos.write((byte) 5);
        /* tcp/ip connection */
        bos.write((byte) 1);
        /* reserved */
        bos.write((byte) 0);
        /* send ipv4/ipv6/domain */
        final DESTTYPE usedDestType;
        switch (destType) {
        case AUTO:
        case IPV4:
            if (((DESTTYPE.AUTO.equals(destType) && isSupported(DESTTYPE.IPV4)) || DESTTYPE.IPV4.equals(destType))) {
                final InetAddress ipv4 = endPointAddress.getAddress();
                if (ipv4 != null && ipv4.getAddress().length == 4) {
                    /* we use ipv4 */
                    bos.write((byte) 1);
                    if (logger != null) {
                        logger.append("->SEND tcp connect request to:" + endpoint + " by ipv4:" + ipv4.getHostAddress() + "\r\n");
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
            if (((DESTTYPE.AUTO.equals(destType) && isSupported(DESTTYPE.IPV6)) || DESTTYPE.IPV6.equals(destType))) {
                final InetAddress ipv6 = endPointAddress.getAddress();
                if (ipv6 != null && ipv6.getAddress().length == 16) {
                    /* we use ipv6 */
                    bos.write((byte) 4);
                    if (logger != null) {
                        logger.append("->SEND tcp connect request to:" + endpoint + " by ipv6:" + ipv6.getHostAddress() + "\r\n");
                    }
                    bos.write(ipv6.getAddress());
                    usedDestType = DESTTYPE.IPV6;
                    break;
                } else if (DESTTYPE.IPV6.equals(destType)) {
                    if (logger != null) {
                        if (ipv6 == null) {
                            logger.append("->Cannot connect request by ipv6 (unresolved)\r\n");
                        } else {
                            logger.append("->Cannot connect request by ipv6 (no ipv6)\r\n");
                        }
                    }
                }
            }
        case DOMAIN:
            if (((DESTTYPE.AUTO.equals(destType) && isSupported(DESTTYPE.DOMAIN)) || DESTTYPE.DOMAIN.equals(destType))) {
                /* we use domain */
                bos.write((byte) 3);
                final String domainString = SocketConnection.getHostName(endPointAddress);
                if (logger != null) {
                    logger.append("->SEND tcp connect request to:" + endpoint + " by domain:" + domainString + "\r\n");
                }
                final byte[] domainBytes = domainString.getBytes(ISO_8859_1);
                bos.write((byte) domainBytes.length & 0xff);
                bos.write(domainBytes);
                usedDestType = DESTTYPE.DOMAIN;
                break;
            }
        default:
            throw new IllegalArgumentException("Unsupported destType:" + destType + "|endPoint:" + endPointAddress);
        }
        /* send port */
        /* network byte order */
        final int port = endPointAddress.getPort();
        bos.write(port >> 8 & 0xff);
        bos.write(port & 0xff);
        bos.writeTo(os);
        os.flush();
        /* read response, 4 bytes and then read rest of response */
        final InputStream is = proxySocket.getInputStream();
        final byte[] read;
        try {
            read = SocketConnection.ensureRead(is, 4, null);
        } catch (final IOException e) {
            throw new ProxyEndpointConnectException(e, getProxy(), endpoint);
        }
        final int[] resp = SocketConnection.byteArrayToIntArray(read);
        if (resp[0] != 5) {
            throw new IOException("Invalid response:" + resp[0]);
        }
        switch (resp[1]) {
        case 0:
            break;
        case 1:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.GENERAL_SERVER_FAILURE);
        case 2:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.NOT_ALLOWED_BY_RULESET);
        case 3:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.NETWORK_UNREACHABLE, usedDestType);
        case 4:// TODO: check for DESTTYPE.DOMAIN and maybe retry with IP
            throw new Socks5EndpointConnectException(CONNECT_ERROR.HOST_UNREACHABLE, usedDestType);
        case 5:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.CONNECTION_REFUSED);
        case 6:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.TTL_EXPIRED);
        case 7:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.COMMMAND_NOT_SUPPORTED);
        case 8:
            switch (usedDestType) {
            case DOMAIN:
                throw new Socks5EndpointConnectException(CONNECT_ERROR.ADDRESS_TYPE_NOT_SUPPORTED_DOMAIN, usedDestType);
            case IPV4:
                throw new Socks5EndpointConnectException(CONNECT_ERROR.ADDRESS_TYPE_NOT_SUPPORTED_IPV4, usedDestType);
            case IPV6:
                throw new Socks5EndpointConnectException(CONNECT_ERROR.ADDRESS_TYPE_NOT_SUPPORTED_IPV6, usedDestType);
            default:
                throw new Socks5EndpointConnectException(CONNECT_ERROR.ADDRESS_TYPE_NOT_SUPPORTED, usedDestType);
            }
        default:
            throw new Socks5EndpointConnectException(CONNECT_ERROR.UNKNOWN, usedDestType, "status=" + resp[1]);
        }
        if (resp[3] == 1) {
            /* ipv4 response */
            final byte[] connectedIP = SocketConnection.ensureRead(is, 4, null);
            /* port */
            final byte[] connectedPort = SocketConnection.ensureRead(is, 2, null);
            if (logger != null) {
                logger.append("<-BOUND IPv4:" + InetAddress.getByAddress(connectedIP) + ":" + (ByteBuffer.wrap(connectedPort).getShort() & 0xffff) + "\r\n");
            }
        } else if (resp[3] == 3) {
            /* domain name response */
            final byte[] length = SocketConnection.ensureRead(is, 1, null);
            final byte[] connectedDomain = SocketConnection.ensureRead(is, SocketConnection.byteToInt(length[0]), null);
            /* port */
            final byte[] connectedPort = SocketConnection.ensureRead(is, 2, null);
            if (logger != null) {
                logger.append("<-BOUND Domain:" + new String(connectedDomain, ISO_8859_1) + ":" + (ByteBuffer.wrap(connectedPort).getShort() & 0xffff) + "\r\n");
            }
        } else if (resp[3] == 4) {
            /* ipv6 response */
            final byte[] connectedIP = SocketConnection.ensureRead(is, 16, null);
            /* port */
            final byte[] connectedPort = SocketConnection.ensureRead(is, 2, null);
            if (logger != null) {
                logger.append("<-BOUND IPv6:" + InetAddress.getByAddress(connectedIP) + ":" + (ByteBuffer.wrap(connectedPort).getShort() & 0xffff) + "\r\n");
            }
        } else {
            throw new Socks5EndpointConnectException(CONNECT_ERROR.ADDRESS_TYPE_NOT_SUPPORTED, usedDestType, String.valueOf(resp[3]));
        }
        return proxySocket;
    }

    protected void authPlain(final SocketStreamInterface proxySocket, String userName, String passWord, final StringBuffer logger) throws IOException {
        final String user = userName == null ? "" : userName;
        final String pass = passWord == null ? "" : passWord;
        if (logger != null) {
            logger.append("->AUTH user:pass\r\n");
        }
        final byte[] userNameBytes = user.getBytes(ISO_8859_1);
        final byte[] passWordBytes = pass.getBytes(ISO_8859_1);
        final OutputStream os = proxySocket.getOutputStream();
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        /* must be 1 */
        bos.write((byte) 1);
        /* send username */
        bos.write((byte) userNameBytes.length & 0xff);
        if (userNameBytes.length > 0) {
            bos.write(userNameBytes);
        }
        /* send password */
        bos.write((byte) passWordBytes.length & 0xff);
        if (passWordBytes.length > 0) {
            bos.write(passWordBytes);
        }
        bos.writeTo(os);
        os.flush();
        /* read response, 2 bytes */
        final InputStream is = proxySocket.getInputStream();
        final byte[] read = SocketConnection.ensureRead(is, 2, null);
        final int[] resp = SocketConnection.byteArrayToIntArray(read);
        if (resp[0] != 1) {
            throw new IOException("Invalid response:" + resp[0]);
        }
        if (resp[1] != 0) {
            if (logger != null) {
                logger.append("<-AUTH Invalid!:" + resp[1] + "\r\n");
            }
            throw new Socks5AuthException(AUTH_ERROR.ERROR, String.valueOf(resp[1]));
        } else {
            if (logger != null) {
                logger.append("<-AUTH Valid!\r\n");
            }
        }
    }

    private final static boolean SENDONLYSINGLEAUTHMETHOD = true;

    protected AUTH sayHello(final SocketStreamInterface proxySocket, AUTH auth, final StringBuffer logger) throws IOException {
        final OutputStream os = proxySocket.getOutputStream();
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        if (logger != null) {
            logger.append("->SOCKS5 Hello to:" + SocketConnection.getRootEndPointSocketAddress(proxySocket) + "\r\n");
        }
        /* socks5 */
        bos.write((byte) 5);
        /* only none and password/username auth method */
        final boolean plainAuthPossible = AUTH.PLAIN.equals(auth);
        if (plainAuthPossible) {
            if (SENDONLYSINGLEAUTHMETHOD) {
                bos.write((byte) 1);
                if (logger != null) {
                    logger.append("->SOCKS5 Offer Plain Authentication\r\n");
                }
                /* username/password */
                bos.write((byte) 2);
            } else {
                bos.write((byte) 2);
                if (logger != null) {
                    logger.append("->SOCKS5 Offer None&Plain Authentication\r\n");
                }
                /* none */
                bos.write((byte) 0);
                /* username/password */
                bos.write((byte) 2);
            }
        } else {
            bos.write((byte) 1);
            if (logger != null) {
                logger.append("->SOCKS5 Offer None Authentication\r\n");
            }
            /* none */
            bos.write((byte) 0);
        }
        bos.writeTo(os);
        os.flush();
        /* read response, 2 bytes */
        final InputStream is = proxySocket.getInputStream();
        final byte[] read = SocketConnection.ensureRead(is, 2, null);
        final int[] resp = SocketConnection.byteArrayToIntArray(read);
        if (resp[0] != 5) {
            throw new IOException("Invalid response:" + resp[0]);
        } else if (resp[1] == 255) {
            if (logger != null) {
                logger.append("<-SOCKS5 Authentication Denied\r\n");
            }
            throw new Socks5AuthException(AUTH_ERROR.DENIED);
        } else if (resp[1] == 2) {
            if (!plainAuthPossible && logger != null) {
                logger.append("->SOCKS5 Plain auth required but not offered!\r\n");
            }
            return AUTH.PLAIN;
        } else if (resp[1] == 0) {
            return AUTH.NONE;
        } else {
            throw new Socks5AuthException(AUTH_ERROR.UNSUPPORTED, String.valueOf(resp[1]));
        }
    }
}
