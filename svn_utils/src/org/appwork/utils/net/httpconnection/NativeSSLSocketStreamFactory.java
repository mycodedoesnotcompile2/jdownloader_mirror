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
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.appwork.utils.Exceptions;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.StringUtils;

/**
 * @author daniel
 *
 */
public class NativeSSLSocketStreamFactory implements SSLSocketStreamFactory {
    private static final NativeSSLSocketStreamFactory INSTANCE = new NativeSSLSocketStreamFactory();

    public static final NativeSSLSocketStreamFactory getInstance() {
        return INSTANCE;
    }

    public SSLSocketFactory getSSLSocketFactory(final SSLSocketStreamOptions options, final String sniHostName) throws IOException {
        final boolean sniEnabled = !StringUtils.isEmpty(sniHostName) && (options == null || options.isSNIEnabled());
        return getSSLSocketFactory(getSSLContext(options), options, sniEnabled ? sniHostName : null);
    }

    protected X509TrustManager bridge(final X509TrustManagerBridge trustManagerBridge) throws SSLException {
        if (JVMVersion.isAtLeast(JavaVersion.JVM_1_8)) {
            return SSLSocketStreamFactory18.bridge(trustManagerBridge);
        } else {
            return new X509TrustManager() {
                @Override
                public void checkClientTrusted(final java.security.cert.X509Certificate[] chain, final String authType) throws CertificateException {
                    trustManagerBridge.checkClientTrusted(chain, authType, null, null);
                }

                @Override
                public void checkServerTrusted(final java.security.cert.X509Certificate[] chain, final String authType) throws CertificateException {
                    trustManagerBridge.checkServerTrusted(chain, authType, null, null);
                }

                @Override
                public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                    return trustManagerBridge.getAcceptedIssuers();
                }
            };
        }
    }

    protected TrustManager[] getTrustCerts(final SSLSocketStreamOptions options) throws SSLException {
        if (options == null || options.isTrustAll()) {
            return new TrustManager[] { bridge(new X509TrustManagerBridge() {
                @Override
                public X509Certificate[] getAcceptedIssuers() {
                    /*
                     * returning null here can cause a NPE in some java versions!
                     */
                    return new java.security.cert.X509Certificate[0];
                }

                @Override
                public void checkServerTrusted(X509Certificate[] chain, String authType, Socket socket, SSLEngine engine) throws CertificateException {
                }

                @Override
                public void checkClientTrusted(X509Certificate[] chain, String authType, Socket socket, SSLEngine engine) throws CertificateException {
                }

                @Override
                public X509TrustManager getTrustManager() {
                    return null;
                }
            }) };
        } else {
            return null;
        }
    }

    protected SSLContext getSSLContext(final SSLSocketStreamOptions options) throws IOException {
        final List<String> protocols = new ArrayList<String>();
        protocols.add("TLS");
        protocols.add("SSL");
        SSLContext sslContext = null;
        NoSuchAlgorithmException nsae = null;
        for (final String protocol : protocols) {
            try {
                sslContext = SSLContext.getInstance(protocol);
                break;
            } catch (NoSuchAlgorithmException e) {
                nsae = Exceptions.addSuppressed(e, nsae);
            }
        }
        if (sslContext != null) {
            try {
                sslContext.init(null, getTrustCerts(options), null);
                return sslContext;
            } catch (KeyManagementException e) {
                throw new SSLException(e);
            }
        } else {
            throw new SSLException(nsae);
        }
    }

    protected SSLSocket modify(SSLSocket sslSocket, final SSLSocketFactory factory, final SSLContext sslContext, final SSLSocketStreamOptions options, final String sniHostName) {
        if (JVMVersion.isAtLeast(JavaVersion.JVM_1_8) && options != null) {
            SSLSocketStreamFactory18.setSNIServerName(options, sslSocket, sniHostName);
        }
        return sslSocket;
    }

    protected SSLSocketFactory getSSLSocketFactory(final SSLContext sslContext, final SSLSocketStreamOptions options, final String sniHostName) throws IOException {
        final SSLSocketFactory factory = sslContext.getSocketFactory();
        return new SSLSocketFactory() {
            @Override
            public Socket createSocket(Socket arg0, String arg1, int arg2, boolean arg3) throws IOException {
                final SSLSocket ret = (SSLSocket) factory.createSocket(arg0, arg1, arg2, arg3);
                return modify(ret, factory, sslContext, options, sniHostName);
            }

            @Override
            public String[] getDefaultCipherSuites() {
                return factory.getDefaultCipherSuites();
            }

            @Override
            public String[] getSupportedCipherSuites() {
                return factory.getSupportedCipherSuites();
            }

            @Override
            public Socket createSocket(String arg0, int arg1) throws IOException, UnknownHostException {
                final SSLSocket ret = (SSLSocket) factory.createSocket(arg0, arg1);
                return modify(ret, factory, sslContext, options, sniHostName);
            }

            @Override
            public Socket createSocket(InetAddress arg0, int arg1) throws IOException {
                final SSLSocket ret = (SSLSocket) factory.createSocket(arg0, arg1);
                return modify(ret, factory, sslContext, options, sniHostName);
            }

            @Override
            public Socket createSocket(String arg0, int arg1, InetAddress arg2, int arg3) throws IOException, UnknownHostException {
                final SSLSocket ret = (SSLSocket) factory.createSocket(arg0, arg1, arg2, arg3);
                return modify(ret, factory, sslContext, options, sniHostName);
            }

            @Override
            public Socket createSocket(InetAddress arg0, int arg1, InetAddress arg2, int arg3) throws IOException {
                final SSLSocket ret = (SSLSocket) factory.createSocket(arg0, arg1, arg2, arg3);
                return modify(ret, factory, sslContext, options, sniHostName);
            }
        };
    }

    public interface NativeSSLSocketStreamInterface extends SSLSocketStreamInterface {
        public SSLContext getSSLContext();

        public SSLSession getSSLSession();

        public SSLSocketFactory getSSLSocketFactory();
    }

    @Override
    public SSLSocketStreamInterface create(final SocketStreamInterface socketStream, final String host, final int port, final boolean autoClose, final SSLSocketStreamOptions options) throws IOException {
        final boolean sniEnabled = !StringUtils.isEmpty(host) && (options == null || options.isSNIEnabled());
        final SSLContext sslContext = getSSLContext(options);
        final SSLSocketFactory sslFactory = getSSLSocketFactory(sslContext, options, sniEnabled ? host : null);
        final SSLSocket sslSocket = (SSLSocket) sslFactory.createSocket(socketStream.getSocket(), sniEnabled ? host : "", port, autoClose);
        if (options != null && !options.isTrustAll()) {
            verifySSLHostname(null, sslSocket.getSession(), host);
        }
        return new NativeSSLSocketStreamInterface() {
            @Override
            public SSLSocket getSocket() {
                return sslSocket;
            }

            @Override
            public OutputStream getOutputStream() throws IOException {
                return sslSocket.getOutputStream();
            }

            @Override
            public InputStream getInputStream() throws IOException {
                return sslSocket.getInputStream();
            }

            @Override
            public void close() throws IOException {
                sslSocket.close();
            }

            @Override
            public SocketStreamInterface getParentSocketStream() {
                return socketStream;
            }

            @Override
            public String getCipherSuite() {
                final SSLSession session = sslSocket.getSession();
                return "JVM|Protocol:" + session.getProtocol() + "|CipherSuite:" + session.getCipherSuite();
            }

            @Override
            public SSLSocketStreamOptions getOptions() {
                return options;
            }

            @Override
            public SSLContext getSSLContext() {
                return sslContext;
            }

            @Override
            public SSLSocketFactory getSSLSocketFactory() {
                return sslFactory;
            }

            @Override
            public SSLSocketStreamFactory getSSLSocketStreamFactory() {
                return NativeSSLSocketStreamFactory.this;
            }

            @Override
            public SSLSession getSSLSession() {
                return sslSocket.getSession();
            }
        };
    }

    protected Boolean verifySSLHostname(HostnameVerifier hostNameVerifier, SSLSession session, String host) throws IOException {
        return HTTPConnectionUtils.verifySSLHostname(hostNameVerifier, session, host);
    }

    @Override
    public String retry(SSLSocketStreamOptions options, Exception e) {
        return null;
    }
}
