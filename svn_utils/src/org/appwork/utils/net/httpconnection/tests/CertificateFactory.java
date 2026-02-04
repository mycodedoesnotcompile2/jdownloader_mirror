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
package org.appwork.utils.net.httpconnection.tests;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.spec.ECGenParameterSpec;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Vector;

import org.appwork.utils.encoding.Base64;

import sun.security.util.ObjectIdentifier;
import sun.security.x509.AlgorithmId;
import sun.security.x509.AuthorityKeyIdentifierExtension;
import sun.security.x509.BasicConstraintsExtension;
import sun.security.x509.CertificateAlgorithmId;
import sun.security.x509.CertificateExtensions;
import sun.security.x509.CertificateSerialNumber;
import sun.security.x509.CertificateValidity;
import sun.security.x509.CertificateVersion;
import sun.security.x509.CertificateX509Key;
import sun.security.x509.DNSName;
import sun.security.x509.ExtendedKeyUsageExtension;
import sun.security.x509.GeneralName;
import sun.security.x509.GeneralNames;
import sun.security.x509.IPAddressName;
import sun.security.x509.KeyIdentifier;
import sun.security.x509.KeyUsageExtension;
import sun.security.x509.SubjectAlternativeNameExtension;
import sun.security.x509.SubjectKeyIdentifierExtension;
import sun.security.x509.X500Name;
import sun.security.x509.X509CertImpl;
import sun.security.x509.X509CertInfo;

/**
 * Factory class for creating SSL certificates on the fly (Java 8+).
 *
 * <p>
 * <b>Notes:</b>
 * </p>
 * <ul>
 * <li>Uses internal JDK classes (sun.security.x509) -> Java 9+ needs --add-exports.</li>
 * <li>Uses SHA256withRSA (avoid SHA1).</li>
 * </ul>
 */
public class CertificateFactory {
    /**
     * Result class containing CA certificate and key pair.
     */
    public static class CACertificateResult {
        private final X509Certificate caCertificate;
        private final KeyPair         caKeyPair;

        public CACertificateResult(final X509Certificate caCertificate, final KeyPair caKeyPair) {
            this.caCertificate = caCertificate;
            this.caKeyPair = caKeyPair;
        }

        public X509Certificate getCaCertificate() {
            return caCertificate;
        }

        public KeyPair getCaKeyPair() {
            return caKeyPair;
        }
    }

    /**
     * Result class containing server certificate, key pair, and CA certificate.
     */
    public static class ServerCertificateResult {
        private final X509Certificate serverCertificate;
        private final KeyPair         serverKeyPair;
        private final X509Certificate caCertificate;

        public ServerCertificateResult(final X509Certificate serverCertificate, final KeyPair serverKeyPair, final X509Certificate caCertificate) {
            this.serverCertificate = serverCertificate;
            this.serverKeyPair = serverKeyPair;
            this.caCertificate = caCertificate;
        }

        public X509Certificate getServerCertificate() {
            return serverCertificate;
        }

        public KeyPair getServerKeyPair() {
            return serverKeyPair;
        }

        public X509Certificate getCaCertificate() {
            return caCertificate;
        }
    }

    /**
     * Result class containing client certificate and key pair.
     */
    public static class ClientCertificateResult {
        private final X509Certificate clientCertificate;
        private final KeyPair         clientKeyPair;

        public ClientCertificateResult(final X509Certificate clientCertificate, final KeyPair clientKeyPair) {
            this.clientCertificate = clientCertificate;
            this.clientKeyPair = clientKeyPair;
        }

        public X509Certificate getClientCertificate() {
            return clientCertificate;
        }

        public KeyPair getClientKeyPair() {
            return clientKeyPair;
        }
    }

    /**
     * Configuration for Subject Alternative Names (SANs).
     */
    public static class SANConfig {
        private final List<String> dnsNames;
        private final List<String> ipAddresses;

        public SANConfig() {
            this.dnsNames = new ArrayList<String>();
            this.ipAddresses = new ArrayList<String>();
        }

        public SANConfig(final List<String> dnsNames, final List<String> ipAddresses) {
            this.dnsNames = dnsNames != null ? new ArrayList<String>(dnsNames) : new ArrayList<String>();
            this.ipAddresses = ipAddresses != null ? new ArrayList<String>(ipAddresses) : new ArrayList<String>();
        }

        public List<String> getDnsNames() {
            return dnsNames;
        }

        public List<String> getIpAddresses() {
            return ipAddresses;
        }

        /**
         * Creates a SANConfig with default localhost SANs (localhost, 127.0.0.1, ::1).
         */
        public static SANConfig createDefaultLocalhost() {
            final SANConfig config = new SANConfig();
            config.dnsNames.add("localhost");
            config.ipAddresses.add("127.0.0.1");
            config.ipAddresses.add("::1");
            return config;
        }

        /**
         * Creates an empty SANConfig (no SANs).
         */
        public static SANConfig createEmpty() {
            return new SANConfig();
        }
    }

    // =========================================================================================
    // Public API
    // =========================================================================================

    /**
     * Creates a CA certificate (self-signed).
     *
     * @param cn
     *            Common Name (e.g. "Test CA")
     * @param keySize
     *            RSA key size (e.g. 2048/4096)
     * @param validityDays
     *            Validity in days
     */
    public static CACertificateResult createCACertificate(final String cn, final int keySize, final int validityDays) throws Exception {
        final KeyPair caKeyPair = generateRsaKeyPair(keySize);
        final long now = System.currentTimeMillis();
        final Date notBefore = new Date(now - 60_000L); // allow clock skew
        final Date notAfter = new Date(now + (validityDays * 24L * 60L * 60L * 1000L));
        final X500Name subject = new X500Name("CN=" + cn);
        final X509CertInfo info = new X509CertInfo();
        info.set(X509CertInfo.VERSION, new CertificateVersion(CertificateVersion.V3));
        info.set(X509CertInfo.SERIAL_NUMBER, new CertificateSerialNumber(randomSerial()));
        info.set(X509CertInfo.SUBJECT, subject);
        info.set(X509CertInfo.ISSUER, subject);
        info.set(X509CertInfo.VALIDITY, new CertificateValidity(notBefore, notAfter));
        info.set(X509CertInfo.KEY, new CertificateX509Key(caKeyPair.getPublic()));
        info.set(X509CertInfo.ALGORITHM_ID, new CertificateAlgorithmId(AlgorithmId.get("SHA256withRSA")));
        final CertificateExtensions exts = new CertificateExtensions();
        // Basic Constraints: CA=true (critical)
        exts.set(BasicConstraintsExtension.NAME, new BasicConstraintsExtension(true, true, 0));
        // Key Usage: keyCertSign, cRLSign (critical)
        final boolean[] keyUsageBits = new boolean[9];
        keyUsageBits[5] = true; // keyCertSign
        keyUsageBits[6] = true; // cRLSign
        exts.set(KeyUsageExtension.NAME, new KeyUsageExtension(keyUsageBits));
        // Extended Key Usage: CA for TLS server/client auth (Zweck sichtbar in Windows)
        final Vector<ObjectIdentifier> caEku = new Vector<ObjectIdentifier>();
        caEku.add(new ObjectIdentifier(new int[] { 1, 3, 6, 1, 5, 5, 7, 3, 1 })); // id-kp-serverAuth
        caEku.add(new ObjectIdentifier(new int[] { 1, 3, 6, 1, 5, 5, 7, 3, 2 })); // id-kp-clientAuth
        exts.set(ExtendedKeyUsageExtension.NAME, new ExtendedKeyUsageExtension(caEku));
        // Subject Key Identifier (non-critical)
        exts.set(SubjectKeyIdentifierExtension.NAME, new SubjectKeyIdentifierExtension(new KeyIdentifier(caKeyPair.getPublic()).getIdentifier()));
        info.set(X509CertInfo.EXTENSIONS, exts);
        final X509Certificate caCert = signTwice(info, caKeyPair.getPrivate(), "SHA256withRSA");
        return new CACertificateResult(caCert, caKeyPair);
    }

    /**
     * Convenience defaults for CA: 2048 bit, 3650 days (~10 years).
     */
    public static CACertificateResult createCACertificate(final String cn) throws Exception {
        return createCACertificate(cn, 2048, 3650);
    }

    /**
     * Creates a server certificate signed by the given CA with configurable SANs.
     *
     * @param caCertificate
     *            CA cert
     * @param caPrivateKey
     *            CA private key
     * @param cn
     *            Common Name (e.g. "localhost")
     * @param keySize
     *            RSA key size
     * @param validityDays
     *            Validity in days (recommend <= 825)
     * @param sanConfig
     *            Subject Alternative Names configuration (DNS names and IP addresses)
     */
    public static ServerCertificateResult createServerCertificate(final X509Certificate caCertificate, final PrivateKey caPrivateKey, final String cn, final int keySize, final int validityDays, final SANConfig sanConfig) throws Exception {
        final KeyPair serverKeyPair = generateRsaKeyPair(keySize);
        final long now = System.currentTimeMillis();
        final Date notBefore = new Date(now - 60_000L);
        final Date notAfter = new Date(now + (validityDays * 24L * 60L * 60L * 1000L));
        final X500Name subject = new X500Name("CN=" + cn);
        final X500Name issuer = X500Name.asX500Name(caCertificate.getSubjectX500Principal());
        final X509CertInfo info = new X509CertInfo();
        info.set(X509CertInfo.VERSION, new CertificateVersion(CertificateVersion.V3));
        info.set(X509CertInfo.SERIAL_NUMBER, new CertificateSerialNumber(randomSerial()));
        info.set(X509CertInfo.SUBJECT, subject);
        info.set(X509CertInfo.ISSUER, issuer);
        info.set(X509CertInfo.VALIDITY, new CertificateValidity(notBefore, notAfter));
        info.set(X509CertInfo.KEY, new CertificateX509Key(serverKeyPair.getPublic()));
        info.set(X509CertInfo.ALGORITHM_ID, new CertificateAlgorithmId(AlgorithmId.get("SHA256withRSA")));
        final CertificateExtensions exts = new CertificateExtensions();
        // Basic Constraints: CA=false (critical)
        exts.set(BasicConstraintsExtension.NAME, new BasicConstraintsExtension(false, true, -1));
        // Key Usage: digitalSignature + keyEncipherment (critical)
        final boolean[] keyUsageBits = new boolean[9];
        keyUsageBits[0] = true; // digitalSignature
        keyUsageBits[2] = true; // keyEncipherment
        exts.set(KeyUsageExtension.NAME, new KeyUsageExtension(keyUsageBits));
        // Extended Key Usage: serverAuth (non-critical but recommended)
        final Vector<ObjectIdentifier> eku = new Vector<ObjectIdentifier>();
        eku.add(new ObjectIdentifier(new int[] { 1, 3, 6, 1, 5, 5, 7, 3, 1 }));
        exts.set(ExtendedKeyUsageExtension.NAME, new ExtendedKeyUsageExtension(eku));
        // Subject Alternative Names: configurable DNS names and IP addresses
        if (sanConfig != null && (!sanConfig.getDnsNames().isEmpty() || !sanConfig.getIpAddresses().isEmpty())) {
            final GeneralNames generalNames = new GeneralNames();
            // Add DNS names
            for (final String dnsName : sanConfig.getDnsNames()) {
                if (dnsName != null && !dnsName.trim().isEmpty()) {
                    generalNames.add(new GeneralName(new DNSName(dnsName.trim())));
                }
            }
            // Add IP addresses
            for (final String ipAddress : sanConfig.getIpAddresses()) {
                if (ipAddress != null && !ipAddress.trim().isEmpty()) {
                    try {
                        final InetAddress addr = InetAddress.getByName(ipAddress.trim());
                        generalNames.add(new GeneralName(new IPAddressName(addr.getAddress())));
                    } catch (final Exception e) {
                        // Skip invalid IP addresses
                    }
                }
            }
            if (generalNames.size() > 0) {
                exts.set(SubjectAlternativeNameExtension.NAME, new SubjectAlternativeNameExtension(false, generalNames));
            }
        }
        // Subject Key Identifier
        final PublicKey serverPub = serverKeyPair.getPublic();
        exts.set(SubjectKeyIdentifierExtension.NAME, new SubjectKeyIdentifierExtension(new KeyIdentifier(serverPub).getIdentifier()));
        // Authority Key Identifier (from CA)
        final KeyIdentifier caKeyId = new KeyIdentifier(caCertificate.getPublicKey());
        exts.set(AuthorityKeyIdentifierExtension.NAME, new AuthorityKeyIdentifierExtension(caKeyId, null, null));
        info.set(X509CertInfo.EXTENSIONS, exts);
        final X509Certificate serverCert = signTwice(info, caPrivateKey, "SHA256withRSA");
        return new ServerCertificateResult(serverCert, serverKeyPair, caCertificate);
    }

    /**
     * Creates a server certificate signed by the given CA with default localhost SANs (localhost, 127.0.0.1, ::1).
     *
     * @param caCertificate
     *            CA cert
     * @param caPrivateKey
     *            CA private key
     * @param cn
     *            Common Name (e.g. "localhost")
     * @param keySize
     *            RSA key size
     * @param validityDays
     *            Validity in days (recommend <= 825)
     */
    public static ServerCertificateResult createServerCertificate(final X509Certificate caCertificate, final PrivateKey caPrivateKey, final String cn, final int keySize, final int validityDays) throws Exception {
        return createServerCertificate(caCertificate, caPrivateKey, cn, keySize, validityDays, SANConfig.createDefaultLocalhost());
    }

    /**
     * Convenience defaults for server: 2048 bit, 825 days, default localhost SANs.
     */
    public static ServerCertificateResult createServerCertificate(final X509Certificate caCertificate, final PrivateKey caPrivateKey, final String cn) throws Exception {
        return createServerCertificate(caCertificate, caPrivateKey, cn, 2048, 825);
    }

    /**
     * Creates both CA and server cert with default localhost SANs.
     */
    public static ServerCertificateResult createCACertificateAndServerCertificate(final String caCn, final String serverCn) throws Exception {
        return createCACertificateAndServerCertificate(caCn, serverCn, SANConfig.createDefaultLocalhost());
    }

    /**
     * Creates both CA and server cert with configurable SANs.
     */
    public static ServerCertificateResult createCACertificateAndServerCertificate(final String caCn, final String serverCn, final SANConfig sanConfig) throws Exception {
        final CACertificateResult caResult = createCACertificate(caCn);
        return createServerCertificate(caResult.getCaCertificate(), caResult.getCaKeyPair().getPrivate(), serverCn, 2048, 825, sanConfig);
    }

    /**
     * Creates a client certificate signed by the given CA.
     *
     * @param caCertificate
     *            CA cert
     * @param caPrivateKey
     *            CA private key
     * @param cn
     *            Common Name (e.g. "client")
     * @param keySize
     *            RSA key size
     * @param validityDays
     *            Validity in days (recommend <= 825)
     */
    public static ClientCertificateResult createClientCertificate(final X509Certificate caCertificate, final PrivateKey caPrivateKey, final String cn, final int keySize, final int validityDays) throws Exception {
        final KeyPair clientKeyPair = generateRsaKeyPair(keySize);
        final long now = System.currentTimeMillis();
        final Date notBefore = new Date(now - 60_000L);
        final Date notAfter = new Date(now + (validityDays * 24L * 60L * 60L * 1000L));
        final X500Name subject = new X500Name("CN=" + cn);
        final X500Name issuer = X500Name.asX500Name(caCertificate.getSubjectX500Principal());
        final X509CertInfo info = new X509CertInfo();
        info.set(X509CertInfo.VERSION, new CertificateVersion(CertificateVersion.V3));
        info.set(X509CertInfo.SERIAL_NUMBER, new CertificateSerialNumber(randomSerial()));
        info.set(X509CertInfo.SUBJECT, subject);
        info.set(X509CertInfo.ISSUER, issuer);
        info.set(X509CertInfo.VALIDITY, new CertificateValidity(notBefore, notAfter));
        info.set(X509CertInfo.KEY, new CertificateX509Key(clientKeyPair.getPublic()));
        info.set(X509CertInfo.ALGORITHM_ID, new CertificateAlgorithmId(AlgorithmId.get("SHA256withRSA")));
        final CertificateExtensions exts = new CertificateExtensions();
        // Basic Constraints: CA=false (critical)
        exts.set(BasicConstraintsExtension.NAME, new BasicConstraintsExtension(false, true, -1));
        // Key Usage: digitalSignature + keyEncipherment (critical)
        final boolean[] keyUsageBits = new boolean[9];
        keyUsageBits[0] = true; // digitalSignature
        keyUsageBits[2] = true; // keyEncipherment
        exts.set(KeyUsageExtension.NAME, new KeyUsageExtension(keyUsageBits));
        // Extended Key Usage: clientAuth (non-critical but recommended)
        final Vector<ObjectIdentifier> eku = new Vector<ObjectIdentifier>();
        eku.add(new ObjectIdentifier(new int[] { 1, 3, 6, 1, 5, 5, 7, 3, 2 })); // clientAuth
        exts.set(ExtendedKeyUsageExtension.NAME, new ExtendedKeyUsageExtension(eku));
        // Subject Key Identifier
        final PublicKey clientPub = clientKeyPair.getPublic();
        exts.set(SubjectKeyIdentifierExtension.NAME, new SubjectKeyIdentifierExtension(new KeyIdentifier(clientPub).getIdentifier()));
        // Authority Key Identifier (from CA)
        final KeyIdentifier caKeyId = new KeyIdentifier(caCertificate.getPublicKey());
        exts.set(AuthorityKeyIdentifierExtension.NAME, new AuthorityKeyIdentifierExtension(caKeyId, null, null));
        info.set(X509CertInfo.EXTENSIONS, exts);
        final X509Certificate clientCert = signTwice(info, caPrivateKey, "SHA256withRSA");
        return new ClientCertificateResult(clientCert, clientKeyPair);
    }

    /**
     * Convenience defaults for client: 2048 bit, 825 days.
     */
    public static ClientCertificateResult createClientCertificate(final X509Certificate caCertificate, final PrivateKey caPrivateKey, final String cn) throws Exception {
        return createClientCertificate(caCertificate, caPrivateKey, cn, 2048, 825);
    }

    /**
     * Creates a client certificate with an EC (elliptic curve) key, signed by the given CA.
     * Use this when the server's CertificateRequest asks for EC (e.g. "No X.509 cert selected for EC");
     * RSA client certs are then not sent by the JSSE.
     *
     * @param caCertificate CA cert
     * @param caPrivateKey  CA private key
     * @param cn            Common Name (e.g. "testclient")
     * @return client cert and EC key pair
     */
    public static ClientCertificateResult createClientCertificateEC(final X509Certificate caCertificate, final PrivateKey caPrivateKey, final String cn) throws Exception {
        final KeyPair clientKeyPair = generateECKeyPair();
        final long now = System.currentTimeMillis();
        final Date notBefore = new Date(now - 60_000L);
        final Date notAfter = new Date(now + (825L * 24L * 60L * 60L * 1000L));
        final X500Name subject = new X500Name("CN=" + cn);
        final X500Name issuer = X500Name.asX500Name(caCertificate.getSubjectX500Principal());
        final X509CertInfo info = new X509CertInfo();
        info.set(X509CertInfo.VERSION, new CertificateVersion(CertificateVersion.V3));
        info.set(X509CertInfo.SERIAL_NUMBER, new CertificateSerialNumber(randomSerial()));
        info.set(X509CertInfo.SUBJECT, subject);
        info.set(X509CertInfo.ISSUER, issuer);
        info.set(X509CertInfo.VALIDITY, new CertificateValidity(notBefore, notAfter));
        info.set(X509CertInfo.KEY, new CertificateX509Key(clientKeyPair.getPublic()));
        info.set(X509CertInfo.ALGORITHM_ID, new CertificateAlgorithmId(AlgorithmId.get("SHA256withRSA")));
        final CertificateExtensions exts = new CertificateExtensions();
        exts.set(BasicConstraintsExtension.NAME, new BasicConstraintsExtension(false, true, -1));
        final boolean[] keyUsageBits = new boolean[9];
        keyUsageBits[0] = true; // digitalSignature
        keyUsageBits[4] = true; // keyAgreement (typical for EC)
        exts.set(KeyUsageExtension.NAME, new KeyUsageExtension(keyUsageBits));
        final Vector<ObjectIdentifier> eku = new Vector<ObjectIdentifier>();
        eku.add(new ObjectIdentifier(new int[] { 1, 3, 6, 1, 5, 5, 7, 3, 2 })); // clientAuth
        exts.set(ExtendedKeyUsageExtension.NAME, new ExtendedKeyUsageExtension(eku));
        exts.set(SubjectKeyIdentifierExtension.NAME, new SubjectKeyIdentifierExtension(new KeyIdentifier(clientKeyPair.getPublic()).getIdentifier()));
        exts.set(AuthorityKeyIdentifierExtension.NAME, new AuthorityKeyIdentifierExtension(new KeyIdentifier(caCertificate.getPublicKey()), null, null));
        info.set(X509CertInfo.EXTENSIONS, exts);
        final X509Certificate clientCert = signTwice(info, caPrivateKey, "SHA256withRSA");
        return new ClientCertificateResult(clientCert, clientKeyPair);
    }

    /**
     * Creates a PKCS12 keystore file containing the server certificate and private key.
     */
    public static void createPKCS12Keystore(final X509Certificate serverCertificate, final PrivateKey serverPrivateKey, final File keystoreFile, final char[] password, final String alias) throws Exception {
        createPKCS12Keystore(serverCertificate, serverPrivateKey, keystoreFile, password, alias, null);
    }

    /**
     * Creates an in-memory PKCS12 KeyStore with the given certificate, private key, and optionally the CA cert.
     * Use this when you do not need to persist to a file (e.g. in tests).
     */
    public static KeyStore createPKCS12KeyStore(final X509Certificate certificate, final PrivateKey privateKey, final char[] password, final String alias, final X509Certificate caCertificate) throws Exception {
        final KeyStore keyStore = KeyStore.getInstance("PKCS12");
        keyStore.load(null, null);
        final Certificate[] chain = (caCertificate != null) ? new Certificate[] { certificate, caCertificate } : new Certificate[] { certificate };
        keyStore.setKeyEntry(alias, privateKey, password, chain);
        return keyStore;
    }

    /**
     * Creates a PKCS12 keystore file containing the server certificate, private key, and optionally the CA cert.
     */
    public static void createPKCS12Keystore(final X509Certificate serverCertificate, final PrivateKey serverPrivateKey, final File keystoreFile, final char[] password, final String alias, final X509Certificate caCertificate) throws Exception {
        final KeyStore keyStore = createPKCS12KeyStore(serverCertificate, serverPrivateKey, password, alias, caCertificate);
        try (FileOutputStream fos = new FileOutputStream(keystoreFile)) {
            keyStore.store(fos, password);
        }
    }

    /**
     * Default alias "server".
     */
    public static void createPKCS12Keystore(final X509Certificate serverCertificate, final PrivateKey serverPrivateKey, final File keystoreFile, final char[] password) throws Exception {
        createPKCS12Keystore(serverCertificate, serverPrivateKey, keystoreFile, password, "server");
    }

    /**
     * Saves a certificate to PEM.
     */
    public static void saveCertificateToPEM(final X509Certificate certificate, final File certFile) throws IOException, CertificateEncodingException {
        try (FileOutputStream fos = new FileOutputStream(certFile)) {
            fos.write("-----BEGIN CERTIFICATE-----\n".getBytes(StandardCharsets.US_ASCII));
            final byte[] encoded = certificate.getEncoded();
            final String base64 = Base64.encodeToString(encoded);
            // Write base64 in 64-character lines
            for (int i = 0; i < base64.length(); i += 64) {
                final int end = Math.min(i + 64, base64.length());
                fos.write(base64.substring(i, end).getBytes(StandardCharsets.US_ASCII));
                fos.write('\n');
            }
            fos.write("-----END CERTIFICATE-----\n".getBytes(StandardCharsets.US_ASCII));
        }
    }

    // =========================================================================================
    // Internals
    // =========================================================================================

    private static KeyPair generateRsaKeyPair(final int keySize) throws Exception {
        final KeyPairGenerator kpg = KeyPairGenerator.getInstance("RSA");
        kpg.initialize(keySize, new SecureRandom());
        return kpg.generateKeyPair();
    }

    private static KeyPair generateECKeyPair() throws Exception {
        final KeyPairGenerator kpg = KeyPairGenerator.getInstance("EC");
        kpg.initialize(new ECGenParameterSpec("secp256r1"), new SecureRandom());
        return kpg.generateKeyPair();
    }

    private static BigInteger randomSerial() {
        // 128-bit random serial (positive)
        final byte[] bytes = new byte[16];
        new SecureRandom().nextBytes(bytes);
        bytes[0] &= 0x7F;
        return new BigInteger(bytes);
    }

    /**
     * Signs a certificate twice to work around JDK bug with inconsistent AlgorithmId.
     * 
     * <p><b>Why this is necessary:</b>
     * <ul>
     *   <li>During the first {@code cert.sign(...)}, the JDK computes the actual AlgorithmId</li>
     *   <li>However, the AlgorithmId in {@code X509CertInfo} is not updated automatically</li>
     *   <li>This causes inconsistency between {@code tbsCertificate.signature.algorithm} and 
     *       {@code signatureAlgorithm} in the final certificate</li>
     *   <li>Result: Certificate looks OK but verification/TLS fails sporadically with 
     *       "Signature algorithm mismatch" errors</li>
     * </ul>
     * 
     * <p><b>What this method does:</b>
     * <ol>
     *   <li>First sign: JDK computes the real AlgorithmId</li>
     *   <li>Copy the actual AlgorithmId back into CertInfo</li>
     *   <li>Sign again with consistent AlgorithmId</li>
     * </ol>
     * 
     * <p><b>When needed:</b>
     * <ul>
     *   <li>Required for {@code sun.security.x509} usage</li>
     *   <li>Critical for Java 8u &lt; ~242</li>
     *   <li>Recommended for Java 11/17 (doesn't hurt, prevents edge cases)</li>
     *   <li>Not needed for BouncyCastle or keytool</li>
     * </ul>
     * 
     * <p>This is a defensive workaround for broken JDK behavior - ugly but correct.
     * Removing this can cause intermittent TLS failures that are hard to debug.
     * 
     * @param info Certificate info to sign
     * @param signingKey Private key for signing
     * @param algo Signature algorithm (e.g., "SHA256withRSA")
     * @return Properly signed certificate with consistent AlgorithmId
     */
    private static X509Certificate signTwice(final X509CertInfo info, final PrivateKey signingKey, final String algo) throws Exception {
        // Step 1: First sign -> JDK computes the real AlgorithmId
        X509CertImpl cert = new X509CertImpl(info);
        cert.sign(signingKey, algo);
        
        // Step 2: Copy the actual AlgorithmId back into CertInfo
        // This ensures consistency between tbsCertificate.signature.algorithm and signatureAlgorithm
        final AlgorithmId actualAlgId = (AlgorithmId) cert.get(X509CertImpl.SIG_ALG);
        info.set(CertificateAlgorithmId.NAME + "." + CertificateAlgorithmId.ALGORITHM, actualAlgId);
        
        // Step 3: Sign again with consistent AlgorithmId
        cert = new X509CertImpl(info);
        cert.sign(signingKey, algo);
        return cert;
    }
}
