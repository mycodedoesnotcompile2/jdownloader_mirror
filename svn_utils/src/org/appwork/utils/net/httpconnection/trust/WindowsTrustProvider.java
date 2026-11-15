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

import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;

import javax.net.ssl.SSLException;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.appwork.utils.DebugMode;
import org.appwork.utils.net.httpconnection.trust.interfaces.OSTrustProviderInterface;
import org.appwork.utils.os.CrossSystem;

/**
 * Trust provider using Windows trusted root stores (user + system) with version-specific revocation checking.
 *
 * Automatically selects the appropriate revocation handler based on Java version: - Java 8+: Uses TrustRevocationHandler1_8
 * (PKIXRevocationChecker) - Java 1.6+: Uses TrustRevocationHandler1_6 (global properties)
 */
public class WindowsTrustProvider extends AbstractTrustProvider implements OSTrustProviderInterface {
    /** https://marschall.github.io/2018/11/23/java-native-truststore-types.html **/
    protected static final String             WINDOWS_USER_ROOT    = "Windows-ROOT";
    /** Windows-ROOT-LOCALMACHINE: available from Java 11.0.20 / 17.0.8 (SunMSCAPI). May require elevated access. */
    protected static final String             WINDOWS_MACHINE_ROOT = "Windows-ROOT-LOCALMACHINE";
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
            // Current user trusted roots
            total += load(merged, WINDOWS_USER_ROOT, "user-");
            // Local machine trusted roots (optional / may not exist / may require admin)
            total += load(merged, WINDOWS_MACHINE_ROOT, "machine-");
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

    private int load(KeyStore merged, final String type, final String prefix) {
        try {
            final KeyStore keyStore = KeyStore.getInstance(type);
            keyStore.load(null, null);
            return copyCertificateEntries(keyStore, merged, prefix);
        } catch (final Exception e) {
            // Ignore: older Java, or "Access is denied" without admin
        }
        return -1;
    }

    protected void ensureEnvironment() throws UnsupportedOperationException {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException(getId() + " is only available on Windows");
        }
    }

    @Override
    public X509TrustManager getTrustManager() throws UnsupportedOperationException, SSLException {
        ensureEnvironment();
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
