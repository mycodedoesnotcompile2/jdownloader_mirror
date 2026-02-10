/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust;

import java.security.KeyStore;
import java.security.cert.CertificateException;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

/**
 * Trust provider using the JRE default CA keystore (cacerts).
 */
public class CurrentJRETrustProvider extends AbstractTrustProvider {
    private static final CurrentJRETrustProvider INSTANCE = new CurrentJRETrustProvider();

    public static CurrentJRETrustProvider getInstance() {
        return INSTANCE;
    }

    private volatile X509TrustManager trustManager;

    protected CurrentJRETrustProvider() {
    }

    protected X509TrustManager getTrustManager() throws Exception {
        X509TrustManager ret = trustManager;
        if (ret != null) {
            return ret;
        }
        synchronized (this) {
            ret = trustManager;
            if (ret != null) {
                return ret;
            }
            final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
            // jre default keystore
            tmf.init((KeyStore) null);
            final TrustManager[] managers = tmf.getTrustManagers();
            if (managers != null) {
                for (final TrustManager m : managers) {
                    if (m instanceof X509TrustManager) {
                        trustManager = ret = (X509TrustManager) m;
                        return ret;
                    }
                }
            }
            throw new CertificateException("No X509TrustManager found");
        }
    }

    @Override
    public KeyStore getTrustStore() throws CertificateException {
        return null;
    }

    public void reload() {
        super.reload();
        trustManager = null;
    }

    /**
     * @return Provider ID for caching
     */
    @Override
    public String getId() {
        return "TrustCurrentJREProvider";
    }
}
