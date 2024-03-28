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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.StringUtils;

/**
 * @author daniel
 *
 */
public class JavaSSLSocketStreamFactory implements SSLSocketStreamFactory {
    private static final JavaSSLSocketStreamFactory INSTANCE          = new JavaSSLSocketStreamFactory();
    private final static String                     TLS13_ENABLED     = "JSSE_TLS1.3_ENABLED";
    private final static String                     TLS10_11_DISABLED = "JSSE_TLS10_11_DISABLED";

    public static final JavaSSLSocketStreamFactory getInstance() {
        return INSTANCE;
    }

    public SSLSocketFactory getSSLSocketFactory(final SSLSocketStreamOptions options, final String sniHostName) throws IOException {
        final boolean sniEnabled = !StringUtils.isEmpty(sniHostName) && (options == null || options.isSNIEnabled());
        return getSSLSocketFactory(getSSLContext(options), options, sniEnabled ? sniHostName : null);
    }

    /*
     * https://risdenk.github.io/2018/03/26/oracle-jdk-missing-ciphers-libsunec.so.html
     *
     *
     * TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256,TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256, TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA,
     * TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA
     */
    public void isCipherSuiteSupported(final String... cipherSuites) throws SSLException {
        try {
            final SSLContext context = getSSLContext(null);
            final SSLParameters parameters = context.getSupportedSSLParameters();
            final List<String> supportedCipherSuites = Arrays.asList(parameters.getCipherSuites());
            for (final String cipherSuite : cipherSuites) {
                if (!supportedCipherSuites.contains(cipherSuite)) {
                    throw new SSLException(cipherSuite + " is unsupported!");
                }
            }
        } catch (IOException e) {
            throw new SSLException(e);
        } catch (RuntimeException e) {
            throw new SSLException(e);
        }
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
            if (false && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                final TrustManager[] custom;
                final TrustManager[] def;
                try {
                    // KeyStore dd = KeyStore.getInstance("Windows-ROOT");
                    FileInputStream is = new FileInputStream("/home/daniel/jdk/jdk1.8.0_131/jre/lib/security/cacerts.all");
                    KeyStore caKeyStore = KeyStore.getInstance("jks");
                    caKeyStore.load(is, "changeit".toCharArray());
                    TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance("PKIX");
                    trustManagerFactory.init(caKeyStore);
                    custom = trustManagerFactory.getTrustManagers();
                    trustManagerFactory = TrustManagerFactory.getInstance("PKIX");
                    trustManagerFactory.init((KeyStore) null);
                    def = trustManagerFactory.getTrustManagers();
                } catch (Exception e) {
                    return null;
                }
                return new TrustManager[] { new X509TrustManager() {
                    @Override
                    public void checkClientTrusted(final java.security.cert.X509Certificate[] chain, final String authType) throws CertificateException {
                    }

                    @Override
                    public void checkServerTrusted(final java.security.cert.X509Certificate[] chain, final String authType) throws CertificateException {
                        try {
                            ((X509TrustManager) def[0]).checkServerTrusted(chain, authType);
                            return;
                        } catch (CertificateException e) {
                            e.printStackTrace();
                        }
                        try {
                            ((X509TrustManager) custom[0]).checkServerTrusted(chain, authType);
                            return;
                        } catch (CertificateException e) {
                            e.printStackTrace();
                        }
                        throw new CertificateException();
                    }

                    @Override
                    public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                        /*
                         * returning null here can cause a NPE in some java versions!
                         */
                        return new java.security.cert.X509Certificate[0];
                    }
                } };
            }
            return null;
        }
    }

    protected SSLContext getSSLContext(final SSLSocketStreamOptions options) throws IOException {
        // https://bugs.openjdk.java.net/browse/JDK-8248721
        // https://stackoverflow.com/questions/29437596/tlsv1-3-is-it-available-now-in-java-8
        // https://java.com/en/jre-jdk-cryptoroadmap.html
        // OpenJDK 8u262-b10
        // OracleJDK 8u261-b12
        final List<String> protocols = new ArrayList<String>();
        if (options != null && options.getCustomFactorySettings().contains(TLS13_ENABLED)) {
            protocols.add(TLS.TLS_1_3.id);
        }
        if (options != null && options.getCustomFactorySettings().contains(TLS10_11_DISABLED)) {
            protocols.add(TLS.TLS_1_2.id);
        }
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

    protected String[] filterEnabledSupportedProtocols(final SSLSocketStreamOptions options, SSLContext sslContext, final String... onlyEnabledProtocols) {
        final List<String> supportedProtocols = Arrays.asList(sslContext.getSupportedSSLParameters().getProtocols());
        final List<String> enabledProtocols;
        if (onlyEnabledProtocols.length == 0) {
            enabledProtocols = new ArrayList<String>(supportedProtocols);
        } else {
            enabledProtocols = new ArrayList<String>(Arrays.asList(onlyEnabledProtocols));
        }
        enabledProtocols.retainAll(supportedProtocols);
        if (options != null) {
            if (!options.getCustomFactorySettings().contains(TLS13_ENABLED)) {
                enabledProtocols.remove(TLS.TLS_1_3.id);
            }
            if (options.getCustomFactorySettings().contains(TLS10_11_DISABLED)) {
                enabledProtocols.remove(TLS.TLS_1_1.id);
                enabledProtocols.remove(TLS.TLS_1_0.id);
            }
        }
        return enabledProtocols.toArray(new String[0]);
    }

    public static enum TLS {
        TLS_1_3("TLSv1.3"),
        TLS_1_2("TLSv1.2"),
        TLS_1_1("TLSv1.1"),
        TLS_1_0("TLSv1");
        protected final String id;

        private TLS(final String id) {
            this.id = id;
        }
    }

    protected SSLSocket modifyProtocols(final SSLSocket sslSocket, final SSLSocketFactory factory, final SSLContext sslContext, final SSLSocketStreamOptions options) {
        if (sslContext != null) {
            if (isTLSSupported(TLS.TLS_1_3, options, sslContext)) {
                sslSocket.setEnabledProtocols(filterEnabledSupportedProtocols(options, sslContext, new String[] { TLS.TLS_1_0.id, TLS.TLS_1_1.id, TLS.TLS_1_2.id, TLS.TLS_1_3.id }));
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_1_8)) {
                sslSocket.setEnabledProtocols(filterEnabledSupportedProtocols(options, sslContext, new String[] { TLS.TLS_1_0.id, TLS.TLS_1_1.id, TLS.TLS_1_2.id }));
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
                sslSocket.setEnabledProtocols(filterEnabledSupportedProtocols(options, sslContext, new String[] { TLS.TLS_1_0.id, TLS.TLS_1_1.id, TLS.TLS_1_2.id }));
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_16 + 121000)) {
                // https://www.oracle.com/java/technologies/javase/6-relnotes.html#R160_121
                sslSocket.setEnabledProtocols(filterEnabledSupportedProtocols(options, sslContext, new String[] { TLS.TLS_1_0.id, TLS.TLS_1_1.id, TLS.TLS_1_2.id }));
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_16 + 111000)) {
                // https://blogs.oracle.com/java-platform-group/diagnosing-tls,-ssl,-and-https
                sslSocket.setEnabledProtocols(filterEnabledSupportedProtocols(options, sslContext, new String[] { TLS.TLS_1_0.id, TLS.TLS_1_1.id }));
            } else {
                sslSocket.setEnabledProtocols(filterEnabledSupportedProtocols(options, sslContext, new String[] { TLS.TLS_1_0.id }));
            }
        }
        return sslSocket;
    }

    public boolean isTLSSupported(final TLS tls, final SSLSocketStreamOptions options, SSLContext sslContext) {
        try {
            // https://github.com/wildfly-security/wildfly-openssl
            // https://github.com/openjsse/openjsse
            if (sslContext == null) {
                sslContext = getSSLContext(options);
            }
            return tls != null && Arrays.asList(sslContext.getSupportedSSLParameters().getProtocols()).contains(tls.id);
        } catch (Exception e) {
            return false;
        }
    }

    protected SSLSocket modifyCipherSuites(final SSLSocket sslSocket, final SSLSocketStreamOptions options) {
        if (sslSocket != null) {
            sslSocket.setEnabledCipherSuites(modifyCipherSuites(sslSocket.getEnabledCipherSuites(), options));
            if (JVMVersion.isMinimum(JVMVersion.JAVA_1_8) && (options != null && options.hasCipherSuitesPreferences())) {
                final SSLParameters sslParams = sslSocket.getSSLParameters();
                sslParams.setUseCipherSuitesOrder(true);
                sslSocket.setSSLParameters(sslParams);
            }
        }
        return sslSocket;
    }

    protected String[] modifyCipherSuites(String[] cipherSuites, final SSLSocketStreamOptions options) {
        if (cipherSuites != null && options != null) {
            final List<String> avoided = new ArrayList<String>();
            final List<String> preferred = new ArrayList<String>();
            final List<String> disabled = new ArrayList<String>();
            final ArrayList<String> list = new ArrayList<String>(Arrays.asList(cipherSuites));
            if (options.getDisabledCipherSuites().size() > 0) {
                for (final String disabledEntry : options.getDisabledCipherSuites()) {
                    final Iterator<String> it = list.iterator();
                    disableLoop: while (it.hasNext()) {
                        final String next = it.next();
                        if (StringUtils.containsIgnoreCase(next, disabledEntry)) {
                            if (!disabledEntry.endsWith("_")) {
                                for (String enabledEntry : options.getEnabledCipherSuites()) {
                                    if (StringUtils.containsIgnoreCase(next, enabledEntry)) {
                                        continue disableLoop;
                                    }
                                }
                            }
                            it.remove();
                            disabled.add(next);
                        }
                    }
                }
            }
            if (options.getAvoidedCipherSuites().size() > 0) {
                for (final String avoidedEntry : options.getAvoidedCipherSuites()) {
                    final Iterator<String> it = list.iterator();
                    while (it.hasNext()) {
                        final String next = it.next();
                        if (StringUtils.containsIgnoreCase(next, avoidedEntry)) {
                            it.remove();
                            avoided.add(next);
                        }
                    }
                }
            }
            if (options.getPreferredCipherSuites().size() > 0) {
                for (final String preferredEntry : options.getPreferredCipherSuites()) {
                    Iterator<String> it = list.iterator();
                    while (it.hasNext()) {
                        final String next = it.next();
                        if (StringUtils.containsIgnoreCase(next, preferredEntry)) {
                            it.remove();
                            preferred.add(next);
                        }
                    }
                    it = avoided.iterator();
                    while (it.hasNext()) {
                        final String next = it.next();
                        if (StringUtils.containsIgnoreCase(next, preferredEntry)) {
                            it.remove();
                            preferred.add(next);
                        }
                    }
                }
            }
            if (disabled.size() > 0 || preferred.size() > 0 || avoided.size() > 0) {
                if (preferred.size() > 0) {
                    list.addAll(0, preferred);
                }
                if (avoided.size() > 0) {
                    list.addAll(avoided);
                }
                cipherSuites = list.toArray(new String[0]);
            }
            return options.sortCipherSuites(cipherSuites);
        }
        return cipherSuites;
    }

    protected SSLSocket modify(SSLSocket sslSocket, final SSLSocketFactory factory, final SSLContext sslContext, final SSLSocketStreamOptions options, final String sniHostName) {
        sslSocket = modifyCipherSuites(modifyProtocols(sslSocket, factory, sslContext, options), options);
        if (JVMVersion.isAtLeast(JavaVersion.JVM_1_8) && options != null) {
            SSLSocketStreamFactory18.setSNIServerName(options, sslSocket, sniHostName);
        }
        return sslSocket;
    }

    protected SSLSocketFactory getSSLSocketFactory(final SSLContext sslContext, final SSLSocketStreamOptions options, final String sniHostName) throws IOException {
        final SSLSocketFactory factory = sslContext.getSocketFactory();
        return new SSLSocketFactory() {
            /**
             * remove SSL because of POODLE Vulnerability
             *
             * https://www.us-cert.gov/ncas/alerts/TA14-290A
             *
             * @param socket
             */
            @Override
            public Socket createSocket(Socket arg0, String arg1, int arg2, boolean arg3) throws IOException {
                final SSLSocket ret = (SSLSocket) factory.createSocket(arg0, arg1, arg2, arg3);
                return modify(ret, factory, sslContext, options, sniHostName);
            }

            @Override
            public String[] getDefaultCipherSuites() {
                return modifyCipherSuites(factory.getDefaultCipherSuites(), options);
            }

            @Override
            public String[] getSupportedCipherSuites() {
                return modifyCipherSuites(factory.getSupportedCipherSuites(), options);
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

    protected Boolean verifySSLHostname(HostnameVerifier hostNameVerifier, SSLSession session, String host) throws IOException {
        return HTTPConnectionUtils.verifySSLHostname(hostNameVerifier, session, host);
    }

    public interface JSSESSLSocketStreamInterface extends SSLSocketStreamInterface {
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
        return new JSSESSLSocketStreamInterface() {
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
                return JavaSSLSocketStreamFactory.this;
            }

            @Override
            public SSLSession getSSLSession() {
                return sslSocket.getSession();
            }
        };
    }

    @Override
    public String retry(SSLSocketStreamOptions options, Exception e) {
        if (isTLSSupported(TLS.TLS_1_3, options, null) && options.isTLSConfigurationException(e)) {
            // https://www.bouncycastle.org/docs/tlsdocs1.5on/org/bouncycastle/tls/AlertDescription.html
            if (options.getCustomFactorySettings().add(TLS13_ENABLED)) {
                // retry with TLS1.3 enabled
                return options.addRetryReason("(TLS)enable TLS1.3");
            }
            final String jsseRetry = options.enableNextDisabledCipher("GCM");
            if (jsseRetry != null) {
                // retry with TLS1.3 and GCM
                return options.addRetryReason("(TLS)enable " + jsseRetry + " for TLS1.3");
            }
        }
        if (options.isHandshakeException(e)) {
            final String jsseRetry = options.enableNextDisabledCipher("GCM");
            if (jsseRetry != null) {
                // retry with TLS1.2 GCM
                return options.addRetryReason("(Handshake)enable " + jsseRetry + " for TLS1.2/TLS1.3");
            } else if (options.getCustomFactorySettings().add(TLS10_11_DISABLED)) {
                // disable old TLS1.0 and TLS1.1 and retry with TLS1.2
                return options.addRetryReason("(Handshake)disable TLS1.0/TLS1.1");
            } else if (isTLSSupported(TLS.TLS_1_3, options, null) && options.getCustomFactorySettings().add(TLS13_ENABLED)) {
                // retry with TLS1.3 enabled
                return options.addRetryReason("(Handshake)enable TLS1.3");
            }
        }
        if (options.isConnectionResetException(e)) {
            final String jsseRetry = options.enableNextDisabledCipher("GCM");
            if (jsseRetry != null) {
                // retry with TLS1.2 GCM
                return options.addRetryReason("(Reset)enable " + jsseRetry + " for TLS1.2/TLS1.3");
            } else if (options.getCustomFactorySettings().add(TLS10_11_DISABLED)) {
                // disable old TLS1.0 and TLS1.1 and retry with TLS1.2
                return options.addRetryReason("(Reset)disable TLS1.0/TLS1.1");
            } else if (isTLSSupported(TLS.TLS_1_3, options, null) && options.getCustomFactorySettings().add(TLS13_ENABLED)) {
                // retry with TLS1.3 enabled
                return options.addRetryReason("(Reset)enable TLS1.3");
            }
        }
        // always check for TLS1.3
        if (isTLSSupported(TLS.TLS_1_3, options, null) && options.getCustomFactorySettings().add(TLS13_ENABLED)) {
            // retry with TLS1.3 enabled
            return options.addRetryReason("(Retry)Enable TLS1.3");
        }
        return null;
    }
}
