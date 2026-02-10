/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils.net.httpconnection.trust;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.trust.ccadb.CCADBTrustProvider;
import org.appwork.utils.os.CrossSystem;

/**
 * @author thomas
 * @date 30.01.2026
 *
 */
public class TrustUtils {
    private static TrustProviderInterface defaultProvider = create();

    /**
     * @return the default trust provider
     */
    public static TrustProviderInterface getDefaultProvider() {
        return defaultProvider;
    }

    /**
     * @param provider
     *            the default trust provider to set
     */
    public static void setDefaultProvider(final TrustProviderInterface provider) {
        if (provider == null) {
            throw new IllegalArgumentException();
        }
        defaultProvider = provider;
    }

    /**
     * Returns the OS-specific trust provider: Windows -> TrustWindowsProvider, Linux -> TrustLinuxProvider, Mac -> TrustMacProvider, else
     * -> TrustCurrentJREProvider. Never returns null.
     *
     * @return the appropriate TrustProviderInterface for the current OS
     */
    public static TrustProviderInterface getOSProvider() {
        switch (CrossSystem.getOSFamily()) {
        case WINDOWS:
            return WindowsTrustProvider.getInstance();
        case LINUX:
            return TrustLinuxProvider.getInstance();
        case MAC:
            return ExperimentalMacTrustProvider.getInstance();
        default:
            return CurrentJRETrustProvider.getInstance();
        }
    }

    /**
     * @return
     */
    private static TrustProviderInterface create() {
        final String bySystem = System.getProperty("TRUST_PROVIDER");
        if (StringUtils.isNotEmpty(bySystem)) {
            try {
                return (TrustProviderInterface) Class.forName(bySystem).getConstructor(new Class[] {}).newInstance();
            } catch (Exception e) {
                LogV3.log(e);
            }
        }
        final Set<TrustProviderInterface> ret = new LinkedHashSet<TrustProviderInterface>();
        ret.add(getOSProvider());
        try {
            ret.add(new CCADBTrustProvider());
        } catch (Exception e) {
            LogV3.log(e);
        }
        ret.add(CurrentJRETrustProvider.getInstance());
        ret.add(AllTrustProvider.getInstance());
        return new CompositeTrustProvider(ret.toArray(new TrustProviderInterface[0]));
    }

    public static X509Certificate[] loadCertificatesFromPEM(final InputStream is) throws IOException, CertificateException {
        try {
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            final List<X509Certificate> certs = new ArrayList<X509Certificate>();
            final Collection<? extends Certificate> loaded = cf.generateCertificates(is);
            for (final Certificate cert : loaded) {
                if (cert instanceof X509Certificate) {
                    certs.add((X509Certificate) cert);
                }
            }
            if (certs.isEmpty()) {
                throw new CertificateException("No X509 certificates found in PEM file");
            }
            return certs.toArray(new X509Certificate[0]);
        } finally {
            is.close();
        }
    }

    public static X509Certificate[] loadCertificatesFromPEM(final File pemFile) throws IOException, CertificateException {
        try {
            final FileInputStream is = new FileInputStream(pemFile);
            try {
                return loadCertificatesFromPEM(is);
            } finally {
                is.close();
            }
        } catch (IOException e) {
            throw new CertificateException("Failed to load certificates from PEM file:" + pemFile, e);
        } catch (CertificateException e) {
            throw new CertificateException("Failed to load certificates from PEM file:" + pemFile, e);
        }
    }

    public static boolean isPEMFile(final File file) {
        if (file == null || !file.isFile()) {
            return false;
        }
        final String name = file.getName().toLowerCase(Locale.ROOT);
        return name.endsWith(".crt") || name.endsWith(".pem") || name.endsWith(".cer") || name.endsWith(".cert");
    }

    public static X509Certificate[] loadCertificatesFromKeystore(final File keystoreFile, final char[] keystorePassword, final String keystoreType) throws IOException, CertificateException {
        try {
            final String[] typesToTry = keystoreType != null ? new String[] { keystoreType } : new String[] { "PKCS12", "JKS" };
            Exception lastException = null;
            KeyStore loadedKeyStore = null;
            for (final String type : typesToTry) {
                final FileInputStream is = new FileInputStream(keystoreFile);
                try {
                    final KeyStore keyStore = KeyStore.getInstance(type);
                    keyStore.load(is, keystorePassword);
                    loadedKeyStore = keyStore;
                    break;
                } catch (final Exception e) {
                    lastException = e;
                } finally {
                    is.close();
                }
            }
            if (loadedKeyStore == null) {
                throw new CertificateException("Failed to load keystore: " + keystoreFile.getAbsolutePath(), lastException);
            }
            final List<X509Certificate> certs = new ArrayList<X509Certificate>();
            final java.util.Enumeration<String> aliases = loadedKeyStore.aliases();
            while (aliases.hasMoreElements()) {
                final String alias = aliases.nextElement();
                if (loadedKeyStore.isCertificateEntry(alias)) {
                    final Certificate cert = loadedKeyStore.getCertificate(alias);
                    if (cert instanceof X509Certificate) {
                        certs.add((X509Certificate) cert);
                    }
                }
            }
            if (certs.isEmpty()) {
                throw new CertificateException("No CA certificates found in keystore: " + keystoreFile.getAbsolutePath());
            }
            return certs.toArray(new X509Certificate[0]);
        } catch (CertificateException e) {
            throw e;
        } catch (IOException e) {
            throw e;
        } catch (final Exception e) {
            throw new CertificateException("Failed to load certificates from keystore: " + keystoreFile.getAbsolutePath(), e);
        }
    }
}
