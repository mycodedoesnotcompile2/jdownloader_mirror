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

import org.appwork.utils.os.CrossSystem;

/**
 * Trust provider using the Linux system CA store (PEM bundle). Only available when running on Linux.
 * <p>
 * Tries the following paths in order (first existing readable file wins):
 * <ul>
 *   <li>{@value #LINUX_CA_DEBIAN} – Debian, Ubuntu and derivatives (maintained by package {@code ca-certificates})</li>
 *   <li>{@value #LINUX_CA_FEDORA} – Fedora, RHEL, CentOS and derivatives</li>
 *   <li>{@value #LINUX_CA_OPENSUSE} – OpenSUSE and others</li>
 * </ul>
 * Certificates are loaded via {@link java.security.cert.CertificateFactory} (X.509 PEM).
 * <p>
 * <b>References:</b>
 * <ul>
 *   <li>Debian/Ubuntu: <code>ca-certificates</code> package, default path
 *       <br><a href="https://wiki.debian.org/CA-Certificate">https://wiki.debian.org/CA-Certificate</a></li>
 *   <li>Fedora/RHEL: System-wide CA bundle
 *       <br><a href="https://fedoraproject.org/wiki/CA-Certificate_Overview">https://fedoraproject.org/wiki/CA-Certificate_Overview</a></li>
 *   <li>OpenSUSE: <code>ca-certificates-mozilla</code> / <code>ca-certificates</code>, typical path
 *       <br><a href="https://en.opensuse.org/openSUSE:Package signing">openSUSE package signing</a></li>
 * </ul>
 *
 * @see org.appwork.utils.net.httpconnection.trust.TrustUtils#getOSProvider()
 */
public class TrustLinuxProvider extends AbstractTrustProvider {
    /** Debian/Ubuntu and derivatives (ca-certificates package). */
    protected static final String LINUX_CA_DEBIAN = "/etc/ssl/certs/ca-certificates.crt";
    /** Fedora/RHEL and derivatives. */
    protected static final String LINUX_CA_FEDORA = "/etc/pki/tls/certs/ca-bundle.crt";
    /** OpenSUSE and others. */
    protected static final String LINUX_CA_OPENSUSE = "/etc/ssl/certs/ca-bundle.crt";

    private static final TrustLinuxProvider INSTANCE = new TrustLinuxProvider();
    private volatile X509TrustManager trustManager;
    private volatile KeyStore trustStore;

    public static TrustLinuxProvider getInstance() {
        return INSTANCE;
    }

    private TrustLinuxProvider() {
    }

    public void reload() {
        super.reload();
        trustManager = null;
        trustStore = null;
    }

    private static File findSystemCaBundle() {
        for (final String path : new String[] { LINUX_CA_DEBIAN, LINUX_CA_FEDORA, LINUX_CA_OPENSUSE }) {
            final File f = new File(path);
            if (f.exists() && f.canRead()) {
                return f;
            }
        }
        return null;
    }

    private KeyStore createLinuxTrustStore() throws SSLException {
        if (!CrossSystem.isLinux()) {
            throw new SSLException("TrustLinuxProvider is only available on Linux");
        }
        final File caFile = findSystemCaBundle();
        if (caFile == null) {
            throw new SSLException("No system CA bundle found (tried " + LINUX_CA_DEBIAN + ", " + LINUX_CA_FEDORA + ", " + LINUX_CA_OPENSUSE + ")");
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
                        ks.setCertificateEntry("linux-ca-" + (i++), cert);
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
        } catch (final SSLException e) {
            throw e;
        } catch (final Exception e) {
            throw new SSLException(e);
        }
    }

    @Override
    public X509TrustManager getTrustManager() throws SSLException {
        if (!CrossSystem.isLinux()) {
            throw new SSLException("TrustLinuxProvider is only available on Linux");
        }
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
                            throw new SSLException("No X509TrustManager from Linux system CA store");
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
    public String getId() {
        return "TrustLinuxProvider";
    }

    @Override
    public KeyStore getTrustStore() throws SSLException {
        if (trustStore == null) {
            synchronized (this) {
                if (trustStore == null) {
                    trustStore = createLinuxTrustStore();
                }
            }
        }
        return trustStore;
    }
}
