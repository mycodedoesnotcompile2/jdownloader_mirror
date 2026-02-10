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
import java.io.IOException;
import java.security.KeyStore;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

/**
 * Trust provider using custom CA certificates. Can load certificates from:
 * <ul>
 * <li>PEM files (single or multiple certificates)</li>
 * <li>PKCS12/JKS keystores (extracts CA certificates)</li>
 * <li>Direct X509Certificate instances</li>
 * </ul>
 *
 * <p>
 * This provider can also be used to load JRE cacerts files directly:
 *
 * <pre>
 * File cacertsFile = new File(System.getProperty(&quot;java.home&quot;), &quot;lib/security/cacerts&quot;);
 *                                                                                       CustomTrustProvider provider = new CustomTrustProvider(cacertsFile, &quot;changeit&quot;.toCharArray(), &quot;JKS&quot;);
 * </pre>
 *
 * </p>
 */
public class CustomTrustProvider extends AbstractTrustProvider {
    private final X509Certificate[]   caCertificates;
    private final String              id;
    private volatile X509TrustManager trustManager;

    /**
     * Creates a provider with custom CA certificates.
     *
     * @param caCertificates
     *            Array of CA certificates to trust. Must not be null or empty.
     */
    public CustomTrustProvider(final X509Certificate... caCertificates) {
        this(null, caCertificates);
    }

    protected CustomTrustProvider(final String id, final X509Certificate... caCertificates) {
        if (caCertificates == null || caCertificates.length == 0) {
            throw new IllegalArgumentException("CA certificates must not be null or empty");
        }
        this.caCertificates = caCertificates.clone();
        if (id != null) {
            this.id = id;
        } else {
            final StringBuilder sb = new StringBuilder("CustomTrustProvider:");
            for (int i = 0; i < caCertificates.length; i++) {
                if (i > 0) {
                    sb.append(",");
                }
                final String subject = caCertificates[i].getSubjectX500Principal() != null ? caCertificates[i].getSubjectX500Principal().getName() : "unknown";
                sb.append(subject.substring(0, Math.min(20, subject.length())));
            }
            this.id = sb.toString();
        }
    }

    /**
     * Creates a provider from a file. Automatically detects if it's a PEM file or keystore file. PEM files are detected by extension (.crt,
     * .pem, .cer, .cert). All other files are treated as keystores. For keystore files, uses default password "changeit".
     *
     * @param file
     *            PEM file (extension .crt, .pem, .cer, .cert) or keystore file (PKCS12, JKS, etc.)
     */
    public CustomTrustProvider(final File file) throws IOException, CertificateException {
        this(file, null);
    }

    /**
     * Creates a provider from a file with password. Automatically detects if it's a PEM file or keystore file. PEM files are detected by
     * extension (.crt, .pem, .cer, .cert). All other files are treated as keystores. Password is only used for keystore files, ignored for
     * PEM files.
     *
     * @param file
     *            PEM file (extension .crt, .pem, .cer, .cert) or keystore file (PKCS12, JKS, etc.)
     * @param password
     *            Password for keystore files (ignored for PEM files)
     */
    public CustomTrustProvider(final File file, final char[] password) throws IOException, CertificateException {
        this(TrustUtils.isPEMFile(file) ? TrustUtils.loadCertificatesFromPEM(file) : TrustUtils.loadCertificatesFromKeystore(file, password, null));
    }

    /**
     * Creates a provider from a keystore file, extracting CA certificates.
     *
     * @param keystoreFile
     *            Keystore file (JKS, PKCS12, etc.)
     * @param keystorePassword
     *            Keystore password (may be null)
     * @param keystoreType
     *            Keystore type (e.g. "JKS", "PKCS12"). If null, tries common types.
     */
    public CustomTrustProvider(final File keystoreFile, final char[] keystorePassword, final String keystoreType) throws IOException, CertificateException {
        this(TrustUtils.loadCertificatesFromKeystore(keystoreFile, keystorePassword, keystoreType));
    }

    @Override
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
            final KeyStore keyStore = getTrustStore();
            final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
            tmf.init(keyStore);
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

    /**
     * Creates a KeyStore containing the custom CA certificates.
     *
     * @return KeyStore with CA certificates
     * @throws Exception
     *             if KeyStore creation fails
     */
    protected KeyStore createTrustStore() throws Exception {
        final KeyStore keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        keyStore.load(null, null);
        final X509Certificate[] caCertificates = getCaCertificates();
        for (int i = 0; i < caCertificates.length; i++) {
            keyStore.setCertificateEntry("ca-" + i, caCertificates[i]);
        }
        return keyStore;
    }

    /**
     * Gets the trust store for PKIX validation. Returns the cached store or creates it if needed.
     */
    /** KeyStore used for PKIX validation (created lazily, cached for revocation checks). */
    private KeyStore trustStore;

    @Override
    public synchronized KeyStore getTrustStore() throws Exception {
        KeyStore ret = trustStore;
        if (ret != null) {
            return ret;
        }
        trustStore = ret = createTrustStore();
        return ret;
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
        return id;
    }

    /**
     * @return The CA certificates used by this provider
     */
    public X509Certificate[] getCaCertificates() {
        return caCertificates.clone();
    }
}
