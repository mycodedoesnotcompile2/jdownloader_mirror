/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.io.File;
import java.io.FileInputStream;
import java.security.KeyStore;

import javax.net.ssl.SSLException;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

/**
 * Trust provider using the JRE default CA keystore (cacerts).
 *
 * Automatically selects the appropriate revocation handler based on Java version: - Java 8+: Uses TrustRevocationHandler1_8
 * (PKIXRevocationChecker) - Java 1.6+: Uses TrustRevocationHandler1_6 (global properties)
 */
public class TrustCurrentJREProvider extends AbstractTrustProvider {
    private static final TrustCurrentJREProvider INSTANCE = new TrustCurrentJREProvider();

    public static TrustCurrentJREProvider getInstance() {
        return INSTANCE;
    }

    private volatile X509TrustManager trustManager;
    /** KeyStore used for PKIX validation (created lazily, cached for revocation checks). */
    private volatile KeyStore         trustStore;

    private TrustCurrentJREProvider() {
    }

    protected X509TrustManager getTrustManager() throws SSLException {
        if (trustManager == null) {
            synchronized (this) {
                if (trustManager == null) {
                    try {
                        final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                        // Use default JRE keystore
                        tmf.init((KeyStore) null);
                        // For default JRE keystore, we need to load it explicitly for revocation checks
                        KeyStore keyStore;
                        try {
                            keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
                            final String javaHome = System.getProperty("java.home");
                            final File cacertsFile = new File(javaHome, "lib/security/cacerts");
                            if (cacertsFile.exists()) {
                                try (FileInputStream fis = new FileInputStream(cacertsFile)) {
                                    keyStore.load(fis, "changeit".toCharArray());
                                }
                            } else {
                                // Fallback: create empty keystore, PKIX will use system defaults
                                keyStore.load(null, null);
                            }
                        } catch (final Exception e) {
                            // Fallback: create empty keystore
                            keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
                            keyStore.load(null, null);
                        }
                        // Store the keystore for revocation checks
                        trustStore = keyStore;
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
                            throw new SSLException("No X509TrustManager found");
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

    /**
     * Gets the trust store for PKIX validation. Returns the cached store or creates it if needed.
     */
    public KeyStore getTrustStore() throws SSLException {
        if (trustStore == null) {
            // Trigger TrustManager creation which also initializes trustStore
            getTrustManager();
        }
        return trustStore;
    }

    public void reload() {
        super.reload();
        trustManager = null;
        trustStore = null;
    }

    /**
     * @return Provider ID for caching
     */
    @Override
    public String getId() {
        return "TrustCurrentJREProvider";
    }
}
