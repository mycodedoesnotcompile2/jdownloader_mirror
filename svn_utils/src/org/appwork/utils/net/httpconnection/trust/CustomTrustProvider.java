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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.net.ssl.SSLException;
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
 * File cacertsFile = new File(System.getProperty("java.home"), "lib/security/cacerts");
 * CustomTrustProvider provider = new CustomTrustProvider(cacertsFile, "changeit".toCharArray(), "JKS");
 * </pre>
 * </p>
 */
public class CustomTrustProvider extends AbstractTrustProvider {
    private final X509Certificate[]   caCertificates;
    private final String              id;
    private volatile X509TrustManager trustManager;
    /** KeyStore used for PKIX validation (created lazily, cached for revocation checks). */
    private volatile KeyStore         trustStore;

    /**
     * Creates a provider with custom CA certificates.
     *
     * @param caCertificates
     *            Array of CA certificates to trust. Must not be null or empty.
     */
    public CustomTrustProvider(final X509Certificate... caCertificates) {
        if (caCertificates == null || caCertificates.length == 0) {
            throw new IllegalArgumentException("CA certificates must not be null or empty");
        }
        this.caCertificates = caCertificates.clone();
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

    /**
     * Creates a provider from a file. Automatically detects if it's a PEM file or keystore file. PEM files are detected by extension (.crt,
     * .pem, .cer, .cert). All other files are treated as keystores. For keystore files, uses default password "changeit".
     *
     * @param file
     *            PEM file (extension .crt, .pem, .cer, .cert) or keystore file (PKCS12, JKS, etc.)
     */
    public CustomTrustProvider(final File file) throws SSLException {
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
    public CustomTrustProvider(final File file, final char[] password) throws SSLException {
        this(isPEMFile(file) ? loadCertificatesFromPEM(file) : loadCertificatesFromKeystore(file, password, null));
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
    public CustomTrustProvider(final File keystoreFile, final char[] keystorePassword, final String keystoreType) throws SSLException {
        this(loadCertificatesFromKeystore(keystoreFile, keystorePassword, keystoreType));
    }

    private static boolean isPEMFile(final File file) {
        if (file == null) {
            return false;
        }
        final String name = file.getName().toLowerCase();
        return name.endsWith(".crt") || name.endsWith(".pem") || name.endsWith(".cer") || name.endsWith(".cert");
    }

    private static X509Certificate[] loadCertificatesFromPEM(final File pemFile) throws SSLException {
        if (pemFile == null || !pemFile.exists() || !pemFile.isFile() || !pemFile.canRead()) {
            throw new SSLException("PEM file not found or not readable: " + (pemFile != null ? pemFile.getAbsolutePath() : "null"));
        }
        try {
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            final List<X509Certificate> certs = new ArrayList<X509Certificate>();
            InputStream is = null;
            try {
                is = new FileInputStream(pemFile);
                final Collection<? extends Certificate> loaded = cf.generateCertificates(is);
                for (final Iterator<? extends Certificate> it = loaded.iterator(); it.hasNext();) {
                    final Certificate cert = it.next();
                    if (cert instanceof X509Certificate) {
                        certs.add((X509Certificate) cert);
                    }
                }
            } finally {
                if (is != null) {
                    try {
                        is.close();
                    } catch (final Exception ignored) {
                    }
                }
            }
            if (certs.isEmpty()) {
                throw new SSLException("No X509 certificates found in PEM file: " + pemFile.getAbsolutePath());
            }
            return certs.toArray(new X509Certificate[0]);
        } catch (final Exception e) {
            throw new SSLException("Failed to load certificates from PEM file: " + pemFile.getAbsolutePath(), e);
        }
    }

    private static X509Certificate[] loadCertificatesFromKeystore(final File keystoreFile, final char[] keystorePassword, final String keystoreType) throws SSLException {
        if (keystoreFile == null || !keystoreFile.exists() || !keystoreFile.isFile() || !keystoreFile.canRead()) {
            throw new SSLException("Keystore file not found or not readable: " + (keystoreFile != null ? keystoreFile.getAbsolutePath() : "null"));
        }
        try {
            KeyStore keyStore = null;
            final String[] typesToTry = keystoreType != null ? new String[] { keystoreType } : new String[] { "PKCS12", "JKS" };
            Exception lastException = null;
            for (final String type : typesToTry) {
                InputStream is = null;
                try {
                    keyStore = KeyStore.getInstance(type);
                    is = new FileInputStream(keystoreFile);
                    keyStore.load(is, keystorePassword);
                    break;
                } catch (final Exception e) {
                    keyStore = null;
                    lastException = e;
                    if (is != null) {
                        try {
                            is.close();
                        } catch (final Exception ignored) {
                        }
                        is = null;
                    }
                }
            }
            if (keyStore == null) {
                throw new SSLException("Failed to load keystore: " + keystoreFile.getAbsolutePath(), lastException);
            }
            final List<X509Certificate> certs = new ArrayList<X509Certificate>();
            final java.util.Enumeration<String> aliases = keyStore.aliases();
            while (aliases.hasMoreElements()) {
                final String alias = aliases.nextElement();
                if (keyStore.isCertificateEntry(alias)) {
                    final Certificate cert = keyStore.getCertificate(alias);
                    if (cert instanceof X509Certificate) {
                        certs.add((X509Certificate) cert);
                    }
                }
            }
            if (certs.isEmpty()) {
                throw new SSLException("No CA certificates found in keystore: " + keystoreFile.getAbsolutePath());
            }
            return certs.toArray(new X509Certificate[0]);
        } catch (final SSLException e) {
            throw e;
        } catch (final Exception e) {
            throw new SSLException("Failed to load certificates from keystore: " + keystoreFile.getAbsolutePath(), e);
        }
    }

    protected X509TrustManager getTrustManager() throws SSLException {
        if (trustManager == null) {
            synchronized (this) {
                if (trustManager == null) {
                    try {
                        final KeyStore keyStore = createTrustStore();
                        // Store the keystore for revocation checks
                        trustStore = keyStore;
                        final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                        tmf.init(keyStore);
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
     * Creates a KeyStore containing the custom CA certificates.
     *
     * @return KeyStore with CA certificates
     * @throws Exception
     *             if KeyStore creation fails
     */
    private KeyStore createTrustStore() throws Exception {
        final KeyStore keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        keyStore.load(null, null);
        for (int i = 0; i < caCertificates.length; i++) {
            keyStore.setCertificateEntry("ca-" + i, caCertificates[i]);
        }
        return keyStore;
    }

    /**
     * Gets the trust store for PKIX validation. Returns the cached store or creates it if needed.
     */
    @Override
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
        return id;
    }

    /**
     * @return The CA certificates used by this provider
     */
    public X509Certificate[] getCaCertificates() {
        return caCertificates.clone();
    }
}
