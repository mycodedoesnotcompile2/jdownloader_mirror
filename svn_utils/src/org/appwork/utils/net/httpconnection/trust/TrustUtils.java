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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
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

    /**
     * Parses the keys (SHA-256 fingerprints) from a JSON object stream of the form {@code {"fingerprint":"reason", ...}}. Used by
     * {@link #loadCertificatesFromPEM(InputStream, InputStream)} to obtain the set of rejected fingerprints. Keys are normalized to 64
     * lowercase hex characters; only keys matching that format are included.
     *
     * @param rejectedJsonStream
     *            JSON stream (e.g. appwork-merged-cadb-rejected.json); may be null
     * @return set of rejected fingerprints (lowercase hex, 64 chars); never null
     */
    public static Set<String> parseRejectedFingerprintsFromJson(final InputStream rejectedJsonStream) throws IOException {
        final Set<String> out = new LinkedHashSet<String>();
        if (rejectedJsonStream == null) {
            return out;
        }
        try {
            final byte[] bytes = IO.readStream(-1, rejectedJsonStream);
            final String json = bytes != null ? new String(bytes, "UTF-8") : "";
            // Match JSON keys that look like SHA-256 fingerprints (64 hex chars)
            final Pattern p = Pattern.compile("\"([0-9a-fA-F]{64})\"\\s*:");
            final Matcher m = p.matcher(json);
            while (m.find()) {
                out.add(m.group(1).toLowerCase(Locale.ROOT));
            }
        } catch (final IOException e) {
            LogV3.log(e);
        }
        return out;
    }

    public static X509Certificate[] loadCertificatesFromPEM(final InputStream pemStream, final Set<String> rejected) throws IOException, CertificateException {
        final X509Certificate[] all = loadCertificatesFromPEM(pemStream);
        if (rejected.isEmpty()) {
            return all;
        }
        final List<X509Certificate> accepted = new ArrayList<X509Certificate>();
        for (int i = 0; i < all.length; i++) {
            try {
                final String fp = Hash.getSHA256(all[i].getEncoded());
                if (fp == null || !rejected.contains(fp.toLowerCase(Locale.ROOT))) {
                    accepted.add(all[i]);
                }
            } catch (final CertificateEncodingException e) {
                accepted.add(all[i]);
            }
        }
        return accepted.toArray(new X509Certificate[accepted.size()]);
    }

    public static X509Certificate[] loadCertificatesFromPEM(final File pemFile) throws IOException, CertificateException {
        try {
            final byte[] data = IO.readFile(pemFile);
            return loadCertificatesFromPEM(new ByteArrayInputStream(data));
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
            final byte[] keyStoreBytes = IO.readFile(keystoreFile);
            for (final String type : typesToTry) {
                try {
                    final KeyStore keyStore = KeyStore.getInstance(type);
                    keyStore.load(new ByteArrayInputStream(keyStoreBytes), keystorePassword);
                    loadedKeyStore = keyStore;
                    break;
                } catch (final Exception e) {
                    lastException = e;
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

    public static X509Certificate[] loadCertificatesFromPEM(final URL url) throws CertificateException {
        try {
            final InputStream is = url.openStream();
            try {
                return loadCertificatesFromPEM(is);
            } finally {
                is.close();
            }
        } catch (IOException e) {
            throw new CertificateException("Failed to load certificates from PEM url:" + url, e);
        } catch (CertificateException e) {
            throw new CertificateException("Failed to load certificates from PEM url:" + url, e);
        }
    }

    /** Extended Key Usage OID for TLS server authentication (id-kp-serverAuth). */
    public static final String EKU_SERVER_AUTH         = "1.3.6.1.5.5.7.3.1";
    /** Key Usage bit index: keyCertSign (CA may sign certs). */
    public static final int    KEY_USAGE_KEY_CERT_SIGN = 5;
    /** Key Usage bit index: cRLSign (CA may sign CRLs). */
    public static final int    KEY_USAGE_CRL_SIGN      = 6;

    /**
     * Returns true if the certificate is a CA that is acceptable as a trust anchor for verifying SSL server certificates (e.g. in an
     * HttpClient). Only CA certificates are accepted; end-entity certificates are rejected. Per RFC 5280, a cert is a CA iff Basic
     * Constraints has cA=true, which in Java is {@code getBasicConstraints() >= 0} (negative means not a CA). If the Extended Key Usage
     * extension is present, it must contain serverAuth (RFC 5280: when EKU is absent, the cert may be used for any purpose). If a Key Usage
     * extension is present, keyCertSign and/or cRLSign must be set.
     *
     * @param cert
     *            certificate to check (e.g. from a Windows or JRE root store)
     * @return true if the cert is a CA usable as trust anchor for SSL server verification
     */
    public static boolean isAcceptableCaTrustAnchorForSsl(final X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        try {
            // Only CAs: getBasicConstraints() >= 0 means cA=true (RFC 5280); < 0 means end-entity, reject
            if (cert.getBasicConstraints() < 0) {
                return false;
            }
            final List<String> eku = cert.getExtendedKeyUsage();
            if (eku != null && !eku.isEmpty() && !eku.contains(EKU_SERVER_AUTH)) {
                return false;
            }
            final boolean[] keyUsage = cert.getKeyUsage();
            // RFC 5280, 4.2.1.3: if keyUsage extension is absent, the key may be used for any purpose; accept
            if (keyUsage == null) {
                return true;
            }
            boolean hasKeyCertSign = keyUsage.length > KEY_USAGE_KEY_CERT_SIGN && keyUsage[KEY_USAGE_KEY_CERT_SIGN];
            boolean hasCrlSign = keyUsage.length > KEY_USAGE_CRL_SIGN && keyUsage[KEY_USAGE_CRL_SIGN];
            return hasKeyCertSign || hasCrlSign;
        } catch (final Exception ignored) {
            return false;
        }
    }
}
