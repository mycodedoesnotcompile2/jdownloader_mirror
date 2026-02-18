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
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;

import javax.net.ssl.SSLException;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.appwork.utils.DebugMode;
import org.appwork.utils.os.CrossSystem;

/**
 * Trust provider using Windows trusted root stores (user + system) with version-specific revocation checking.
 *
 * Automatically selects the appropriate revocation handler based on Java version: - Java 8+: Uses TrustRevocationHandler1_8
 * (PKIXRevocationChecker) - Java 1.6+: Uses TrustRevocationHandler1_6 (global properties)
 */
public class WindowsTrustProvider extends AbstractTrustProvider {
    protected static final String             WINDOWS_ROOT_USER    = "Windows-ROOT";
    /** Windows-ROOT-LOCALMACHINE: available from Java 11.0.20 / 17.0.8 (SunMSCAPI). May require elevated access. */
    protected static final String             WINDOWS_ROOT_MACHINE = "Windows-ROOT-LOCALMACHINE";
    private static final WindowsTrustProvider INSTANCE             = new WindowsTrustProvider();
    private volatile X509TrustManager         trustManager;
    /** Merged Windows trust store (roots) used as PKIX trust anchors (created lazily). */
    private volatile KeyStore                 mergedTrustStore;

    public static WindowsTrustProvider getInstance() {
        return INSTANCE;
    }

    private WindowsTrustProvider() {
    }

    public void reload() {
        super.reload();
        trustManager = null;
        mergedTrustStore = null;
    }

    /**
     * Copies all certificate entries from source into target, using prefix for aliases.
     *
     * @return number of entries added
     */
    protected static int copyCertificateEntries(final KeyStore source, final KeyStore target, final String prefix) throws Exception {
        int count = 0;
        for (final String alias : new ArrayList<String>(java.util.Collections.list(source.aliases()))) {
            if (source.isCertificateEntry(alias)) {
                final Certificate cert = source.getCertificate(alias);
                if (cert == null) {
                    DebugMode.debugger();
                    continue;
                }
                if (cert instanceof X509Certificate) {
                    boolean isAcceptableCa = TrustUtils.isAcceptableCaTrustAnchorForSsl((X509Certificate) cert);
                    if (isAcceptableCa) {
                        target.setCertificateEntry(prefix + alias, cert);
                        count++;
                    } else {
                        // DebugMode.debugger();
                    }
                } else {
                    DebugMode.debugger();
                }
            }
        }
        return count;
    }

    private KeyStore createMergedWindowsTrustStore() throws SSLException {
        try {
            final KeyStore merged = KeyStore.getInstance(KeyStore.getDefaultType());
            merged.load(null, null);
            int total = 0;
            // 1) Current user trusted roots
            final KeyStore userRoot = KeyStore.getInstance(WINDOWS_ROOT_USER);
            userRoot.load(null, null);
            total += copyCertificateEntries(userRoot, merged, "user-");
            // 2) Local machine trusted roots (optional / may not exist / may require admin)
            try {
                final KeyStore machineRoot = KeyStore.getInstance(WINDOWS_ROOT_MACHINE);
                machineRoot.load(null, null);
                total += copyCertificateEntries(machineRoot, merged, "machine-");
            } catch (final Exception e) {
                // Ignore: older Java, or "Access is denied" without admin
            }
            if (total == 0) {
                throw new SSLException("No certificates in Windows root store(s)");
            }
            return merged;
        } catch (final SSLException e) {
            throw e;
        } catch (final Exception e) {
            throw new SSLException(e);
        }
    }

    @Override
    public X509TrustManager getTrustManager() throws SSLException {
        if (!CrossSystem.isWindows()) {
            throw new SSLException("TrustWindowsProvider is only available on Windows");
        }
        if (trustManager == null) {
            synchronized (this) {
                if (trustManager == null) {
                    try {
                        final KeyStore merged = getTrustStore();
                        final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                        tmf.init(merged);
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
                            throw new SSLException("No X509TrustManager from Windows root store(s)");
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
        return "TrustWindowsProvider";
    }

    /**
     * @see org.appwork.utils.net.httpconnection.trust.AbstractTrustProvider#getTrustStore()
     */
    @Override
    public KeyStore getTrustStore() throws SSLException {
        if (mergedTrustStore == null) {
            synchronized (this) {
                if (mergedTrustStore == null) {
                    mergedTrustStore = createMergedWindowsTrustStore();
                }
            }
        }
        return mergedTrustStore;
    }
}
