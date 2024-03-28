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

import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.List;

import javax.net.ssl.SNIHostName;
import javax.net.ssl.SNIServerName;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.X509ExtendedTrustManager;

import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;

/**
 * @author daniel
 * @date Nov 16, 2022
 *
 */
public class SSLSocketStreamFactory18 {
    protected static X509TrustManagerBridge bridge(final X509ExtendedTrustManager trustMaster) {
        return new X509TrustManagerBridge() {
            @Override
            public X509Certificate[] getAcceptedIssuers() {
                return trustMaster.getAcceptedIssuers();
            }

            @Override
            public void checkServerTrusted(X509Certificate[] chain, String authType, Socket socket, SSLEngine engine) throws CertificateException {
                if (socket != null) {
                    trustMaster.checkServerTrusted(chain, authType, socket);
                } else if (engine != null) {
                    trustMaster.checkServerTrusted(chain, authType, engine);
                } else {
                    trustMaster.checkServerTrusted(chain, authType);
                }
            }

            @Override
            public void checkClientTrusted(X509Certificate[] chain, String authType, Socket socket, SSLEngine engine) throws CertificateException {
                if (socket != null) {
                    trustMaster.checkClientTrusted(chain, authType, socket);
                } else if (engine != null) {
                    trustMaster.checkClientTrusted(chain, authType, engine);
                } else {
                    trustMaster.checkClientTrusted(chain, authType);
                }
            }

            @Override
            public X509ExtendedTrustManager getTrustManager() {
                return trustMaster;
            }
        };
    }

    protected static X509ExtendedTrustManager bridge(final X509TrustManagerBridge trustManagerBridge) throws SSLException {
        return new X509ExtendedTrustManager() {
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

            @Override
            public void checkClientTrusted(X509Certificate[] chain, String authType, Socket socket) throws CertificateException {
                trustManagerBridge.checkClientTrusted(chain, authType, socket, null);
            }

            @Override
            public void checkServerTrusted(X509Certificate[] chain, String authType, Socket socket) throws CertificateException {
                trustManagerBridge.checkServerTrusted(chain, authType, socket, null);
            }

            @Override
            public void checkClientTrusted(X509Certificate[] chain, String authType, SSLEngine engine) throws CertificateException {
                trustManagerBridge.checkClientTrusted(chain, authType, null, engine);
            }

            @Override
            public void checkServerTrusted(X509Certificate[] chain, String authType, SSLEngine engine) throws CertificateException {
                trustManagerBridge.checkServerTrusted(chain, authType, null, engine);
            }
        };
    }

    public static void setSNIServerName(final SSLSocketStreamOptions options, SSLSocket sslSocket, final String sniHostName) {
        if (options.isSNIEnabled() && StringUtils.isNotEmpty(sniHostName)) {
            final SSLParameters sslParams = sslSocket.getSSLParameters();
            List<SNIServerName> serverNames = sslParams.getServerNames();
            if (serverNames == null || serverNames.size() == 0) {
                try {
                    SNIServerName sniServerName = null;
                    try {
                        sniServerName = new SNIHostName(sniHostName);
                    } catch (IllegalArgumentException e) {
                        // workaround for hostname with underscore
                        try {
                            sniServerName = new SNIHostName(java.net.IDN.toASCII(sniHostName).getBytes("UTF-8"));
                        } catch (UnsupportedEncodingException e2) {
                            throw Exceptions.addSuppressed(e, e2);
                        }
                    }
                    serverNames = Arrays.asList(new SNIServerName[] { sniServerName });
                    sslParams.setServerNames(serverNames);
                    sslSocket.setSSLParameters(sslParams);
                } catch (IllegalArgumentException ignore) {
                    // see sun.security.ssl.Utilities.rawToSNIHostName(String)
                }
            }
        } else if (!options.isSNIEnabled()) {
            final SSLParameters sslParams = sslSocket.getSSLParameters();
            sslParams.setServerNames(Arrays.asList(new SNIServerName[0]));
            sslSocket.setSSLParameters(sslParams);
        }
    }
    // AlgorithmConstraints are checked ONLY (without default jdk constraints) for X509ExtendedTrustManager !
    // public static void removeAlgorithmConstraints(final SSLSocketStreamOptions options, SSLSocket sslSocket) {
    // final SSLParameters sslParams = sslSocket.getSSLParameters();
    // final AlgorithmConstraints algorithmConstraints = new AlgorithmConstraints() {
    // @Override
    // public boolean permits(Set<CryptoPrimitive> primitives, String algorithm, AlgorithmParameters parameters) {
    // return true;
    // }
    //
    // @Override
    // public boolean permits(Set<CryptoPrimitive> primitives, Key key) {
    // return true;
    // }
    //
    // @Override
    // public boolean permits(Set<CryptoPrimitive> primitives, String algorithm, Key key, AlgorithmParameters parameters) {
    // return true;
    // }
    // };
    // sslParams.setAlgorithmConstraints(algorithmConstraints);
    // sslSocket.setSSLParameters(sslParams);
    // }
}
