/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.utils.net.httpconnection.trust;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Collection;

import javax.net.ssl.SSLException;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.appwork.utils.net.httpconnection.trust.interfaces.OSTrustProviderInterface;
import org.appwork.utils.os.CrossSystem;

/**
 * Trust provider using the macOS system CA store. Only available when running on macOS.
 * <p>
 * <b>Strategy (Java 14+):</b> Uses the native {@value #KEYCHAIN_STORE} KeyStore type, which reads from the macOS Keychain (system and user
 * trusted roots). This is the preferred path when the JRE provides KeychainStore (OpenJDK 14+ on macOS, or legacy Apple JDK).
 * <p>
 * <b>Fallback (Java &lt; 14 or when KeychainStore is unavailable):</b> Loads CA certificates from the system PEM file {@value #MAC_CA_PEM}.
 * That file is installed with macOS and used by PHP, curl and similar; it is a symbolic link to {@value #MAC_CA_PEM_PRIVATE}. Note: this
 * file is not automatically updated with macOS keychain changes and may be outdated; prefer Java 14+ with KeychainStore for up-to-date
 * trust.
 * <p>
 * <b>References:</b>
 * <ul>
 * <li>KeychainStore (macOS Keychain): e.g. <a href="https://marschall.github.io/2018/11/23/java-native-truststore-types.html">Using Native
 * Truststores with Java</a>, JVM option {@code -Djavax.net.ssl.trustStoreType=KeychainStore}.</li>
 * <li>macOS CA file at /etc/ssl: e.g. <a href="https://www.herongyang.com/PKI/macOS-CA-Certificate-at-etc-ssl.html">CA Certificates at
 * /etc/ssl</a>; <code>/etc/ssl/cert.pem</code> → <code>/private/etc/ssl/cert.pem</code>.</li>
 * <li>Known KeychainStore issues (incomplete cert loading, prompts): e.g. Stack Overflow “KeychainStore doesn't provide certificates to
 * Java on Mac OS X”; OpenJDK bug trackers (JDK-8303465, JDK-8325935).</li>
 * </ul>
 *
 * @see org.appwork.utils.net.httpconnection.trust.TrustUtils#getOSProvider()
 */
public class MacTrustProvider extends AbstractTrustProvider implements OSTrustProviderInterface {
    /** KeychainStore: system and user keychains on macOS (Java 14+ / Apple JDK). */
    protected static final String         KEYCHAIN_STORE     = "KeychainStore";
    /** macOS system CA PEM (symlink); fallback when KeychainStore is not available (e.g. Java &lt; 14). */
    protected static final String         MAC_CA_PEM         = "/etc/ssl/cert.pem";
    /** Actual path of system CA PEM on macOS. */
    protected static final String         MAC_CA_PEM_PRIVATE = "/private/etc/ssl/cert.pem";
    private static final MacTrustProvider INSTANCE           = new MacTrustProvider();
    private volatile X509TrustManager     trustManager;
    private volatile KeyStore             trustStore;

    public static MacTrustProvider getInstance() {
        return INSTANCE;
    }

    public MacTrustProvider() {
    }

    public void reload() {
        super.reload();
        trustManager = null;
        trustStore = null;
    }

    /**
     * Tries KeychainStore first (Java 14+). If unavailable, loads from {@value #MAC_CA_PEM} / {@value #MAC_CA_PEM_PRIVATE}.
     */
    protected KeyStore createMacTrustStore() throws SSLException, UnsupportedOperationException {
        if (!CrossSystem.isMac()) {
            throw new SSLException("TrustMacProvider is only available on macOS");
        }
        try {
            final KeyStore ks = KeyStore.getInstance(KEYCHAIN_STORE, "Apple");
            ks.load(null, null);
            return ks;
        } catch (final Exception e) {
            // Fallback for Java < 14 or when KeychainStore is not provided: load from system PEM file
            final File caFile = findMacCAPem();
            if (caFile == null) {
                throw new UnsupportedOperationException("TrustMacProvider: KeychainStore unavailable (" + e.getMessage() + ") and no system CA file found at " + MAC_CA_PEM + " or " + MAC_CA_PEM_PRIVATE, e);
            }
            try {
                final KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
                ks.load(null, null);
                final CertificateFactory cf = CertificateFactory.getInstance("X.509");
                InputStream is = null;
                try {
                    is = new FileInputStream(caFile);
                    final Collection<? extends Certificate> certs = cf.generateCertificates(is);
                    int i = 0;
                    for (final Certificate cert : certs) {
                        if (cert instanceof X509Certificate) {
                            ks.setCertificateEntry("mac-ca-" + (i++), cert);
                        }
                    }
                } finally {
                    if (is != null) {
                        is.close();
                    }
                }
                if (ks.size() == 0) {
                    throw new SSLException("No certificates loaded from " + caFile.getAbsolutePath());
                }
                return ks;
            } catch (final SSLException se) {
                throw se;
            } catch (final Exception ex) {
                throw new SSLException("TrustMacProvider: failed to load from " + caFile.getAbsolutePath(), ex);
            }
        }
    }

    protected File findMacCAPem() {
        for (final String path : new String[] { MAC_CA_PEM_PRIVATE, MAC_CA_PEM }) {
            final File f = new File(path);
            if (f.exists() && f.canRead()) {
                return f;
            }
        }
        return null;
    }

    protected void ensureEnvironment() throws UnsupportedOperationException {
        if (!CrossSystem.isMac()) {
            throw new UnsupportedOperationException(getClass() + " is only available on macOS");
        }
    }

    @Override
    public X509TrustManager getTrustManager() throws SSLException, UnsupportedOperationException {
        ensureEnvironment();
        if (trustManager == null) {
            synchronized (this) {
                if (trustManager == null) {
                    try {
                        final KeyStore ks = getTrustStore();
                        final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                        tmf.init(ks);
                        final TrustManager[] managers = tmf.getTrustManagers();
                        if (managers != null) {
                            for (final TrustManager m : managers) {
                                if (m instanceof X509TrustManager) {
                                    trustManager = (X509TrustManager) m;
                                    break;
                                }
                            }
                        }
                        if (trustManager == null) {
                            throw new SSLException("No X509TrustManager from macOS Keychain");
                        }
                    } catch (final SSLException e) {
                        throw e;
                    } catch (final Exception e) {
                        throw new SSLException(e);
                    }
                }
            }
        }
        return trustManager;
    }

    @Override
    public KeyStore getTrustStore() throws SSLException {
        if (trustStore == null) {
            synchronized (this) {
                if (trustStore == null) {
                    trustStore = createMacTrustStore();
                }
            }
        }
        return trustStore;
    }
}
