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
package org.appwork.utils.net.httpconnection;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;

import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;

/**
 * @author daniel
 * @date 27.07.2017
 *
 */
public class KeepAliveSocketStream implements SocketStreamInterface {
    protected final SocketStreamInterface socket;
    protected final InetAddress           boundIP;
    protected final String                host;

    public InetAddress getBoundIP() {
        return this.boundIP;
    }

    public InetAddress[] getRemoteIPs() {
        return this.remoteIPs;
    }

    protected final InetAddress[] remoteIPs;

    public boolean sameBoundIP(final InetAddress bindIP[]) {
        final InetAddress boundIP = getBoundIP();
        if (bindIP == null && boundIP == null) {
            return true;
        } else if (bindIP != null && boundIP != null) {
            for (InetAddress bind : bindIP) {
                if (bind.equals(boundIP)) {
                    return true;
                }
            }
            return false;
        } else {
            return false;
        }
    }

    public boolean sameHost(final String otherHost) {
        return StringUtils.equalsIgnoreCase(this.getHost(), otherHost);
    }

    public boolean sameRemoteIPs(final InetAddress remoteIPs[]) {
        if (remoteIPs != null && remoteIPs.length > 0) {
            final InetAddress socketRemoteIP = socket.getSocket().getInetAddress();
            for (final InetAddress remoteIP : remoteIPs) {
                if (socketRemoteIP.equals(remoteIP)) {
                    //
                    return true;
                }
            }
            if (this.getRemoteIPs() != null) {
                for (final InetAddress knownRemoteIP : this.getRemoteIPs()) {
                    for (final InetAddress remoteIP : remoteIPs) {
                        if (knownRemoteIP.equals(remoteIP)) {
                            //
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    public long getKeepAliveTimeout() {
        return this.keepAliveTimeout;
    }

    public long getRequestsLeft() {
        return Math.max(0, this.getRequestsMax() - this.requests);
    }

    public long getRequestsMax() {
        return this.maxRequests;
    }

    public void increaseRequests() {
        this.requests += 1;
    }

    protected final long         keepAliveTimeout;
    protected final long         maxRequests;
    protected final HTTPProxy    proxy;
    protected volatile long      keepAliveTimestamp = -1;
    protected volatile long      requests           = 0;
    protected final InputStream  is;
    protected final OutputStream os;

    public boolean isSsl() {
        return false;
    }

    public boolean isTimedOut() {
        return Time.systemIndependentCurrentJVMTimeMillis() >= keepAliveTimestamp;
    }

    public void keepAlive() {
        this.keepAliveTimestamp = Time.systemIndependentCurrentJVMTimeMillis() + getKeepAliveTimeout();
    }

    public KeepAliveSocketStream(HTTPProxy proxy, final String host, final SocketStreamInterface socket, final long keepAliveTimeout, final long maxRequests, final InetAddress boundIP, final InetAddress[] remoteIPs) {
        this.proxy = proxy;
        this.host = host;
        this.socket = socket;
        this.boundIP = boundIP;
        this.remoteIPs = remoteIPs;
        this.keepAliveTimeout = Math.max(0, keepAliveTimeout);
        this.maxRequests = Math.max(0, maxRequests);
        os = new OutputStream() {
            @Override
            public void write(int b) throws IOException {
                try {
                    socket.getOutputStream().write(b);
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public void write(byte[] b) throws IOException {
                try {
                    socket.getOutputStream().write(b);
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public void write(byte[] b, int off, int len) throws IOException {
                try {
                    socket.getOutputStream().write(b, off, len);
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public void flush() throws IOException {
                try {
                    socket.getOutputStream().flush();
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public void close() throws IOException {
            }
        };
        is = new InputStream() {
            @Override
            public int read() throws IOException {
                try {
                    return socket.getInputStream().read();
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public int read(byte[] b) throws IOException {
                try {
                    return socket.getInputStream().read(b);
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public int read(byte[] b, int off, int len) throws IOException {
                try {
                    return socket.getInputStream().read(b, off, len);
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public long skip(long n) throws IOException {
                try {
                    return socket.getInputStream().skip(n);
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public int available() throws IOException {
                try {
                    return socket.getInputStream().available();
                } catch (IOException e) {
                    throw new KeepAliveSocketStreamException(e, KeepAliveSocketStream.this);
                }
            }

            @Override
            public void close() throws IOException {
            }
        };
    }

    public String getHost() {
        return this.host;
    }

    @Override
    public Socket getSocket() {
        return socket.getSocket();
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return is;
    }

    @Override
    public OutputStream getOutputStream() throws IOException {
        return os;
    }

    @Override
    public void close() throws IOException {
        socket.close();
    }
}
