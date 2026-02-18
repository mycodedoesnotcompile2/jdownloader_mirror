/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         ====================================================================================================================================================
 *         ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust.ide;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.security.KeyStore;
import java.security.cert.CertPath;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.utils.Hash;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.CurrentJRETrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;
import org.appwork.utils.net.httpconnection.trust.ccadb.CCADBTrustProvider;
import org.appwork.utils.os.WindowsCertUtils;

/**
 * Validiert Root-Zertifikate gegen mehrere Trust-Provider (JRE, OS, Mozilla/CCADB, CT Sycamore, Apple, Google Chrome, Android, Cisco TRS,
 * optional Microsoft Windows Update bzw. Microsoft CCADB). Ausgabe: pro Zertifikat eine Liste der Provider, die es als vertrauenswürdige
 * Root-CA akzeptieren.
 * <p>
 * Apple Root Store: Siehe <a href="https://support.apple.com/en-us/103272">Available root certificates for Apple operating systems</a>;
 * Fingerprints werden aus der veröffentlichten HTML-Liste (z. B. iOS/macOS Root Store) geparst.
 * <p>
 * Microsoft (Windows Update): Root-Store als PFX laden, erzeugt mit certutil (nur Windows):<br>
 * {@code certutil -generateSSTFromWU roots.sst}<br>
 * {@code certutil -dump roots.sst} (optional, zur Kontrolle)<br>
 * {@code certutil -exportPFX roots.sst roots.pfx}<br>
 * Dann {@link #addMicrosoftRootStoreFromPfx(File) addMicrosoftRootStoreFromPfx}(new File("roots.pfx")) oder
 * {@link #addTrustStoreFromPfx(String, File, char[]) addTrustStoreFromPfx}("Microsoft (Windows Update)", roots.pfx, null). Alternativ
 * {@link #addMicrosoftRootStoreViaCertutil()} bzw. {@link #addMicrosoftRootStoreViaCertutil(File)}: führt certutil automatisch aus und
 * liest die erzeugte PFX ein (nur unter Windows). Zusätzlich: {@link #enableMicrosoftCCADB()} lädt die Microsoft-Liste aus dem CCADB-CSV
 * (plattformunabhängig).
 * <p>
 * Android Root Store: {@link #enableAndroidRootStore()} lädt google/roots.pem von Android Googlesource. Cisco Trusted Root Store (TRS):
 * {@link #enableCiscoTRS()} lädt das Intersect-P7B (Java 9+ für PKCS7). Mozilla Included Roots (Websites):
 * {@link #enableMozillaIncludedRootsPem()} lädt die PEM von CCADB (TrustBitsInclude=Websites).
 * <p>
 * Kompilieren nach bin-cursor (nur diese Klasse, Rest aus Eclipse bin):<br>
 * javac -source 1.6 -target 1.6 -encoding UTF-8 -cp "bin;libs/*;dev_libs/*" -d bin-cursor
 * src/org/appwork/utils/net/httpconnection/trust/RootCertificateTrustValidator.java
 * <p>
 * Ausführen (ohne Eingabe-PEM; Kandidaten kommen aus den Stores, Ausgabe = Schnittmenge aller aktivierten Stores):<br>
 * java ... RootCertificateTrustValidator [ausgabe.pem] [roots.pfx]
 */
public final class RootCertificateTrustValidator {
    /** Ergebnis pro Zertifikat: Fingerprint, Subject und die Namen der Provider, die es akzeptieren. */
    public static final class CertTrustResult {
        private final String       fingerprintSha256;
        private final String       subject;
        private final List<String> acceptedByProviders;

        public CertTrustResult(final String fingerprintSha256, final String subject, final List<String> acceptedByProviders) {
            this.fingerprintSha256 = fingerprintSha256;
            this.subject = subject != null ? subject : "";
            this.acceptedByProviders = acceptedByProviders != null ? new ArrayList<String>(acceptedByProviders) : new ArrayList<String>();
        }

        public String getFingerprintSha256() {
            return fingerprintSha256;
        }

        public String getSubject() {
            return subject;
        }

        public List<String> getAcceptedByProviders() {
            return Collections.unmodifiableList(acceptedByProviders);
        }

        @Override
        public String toString() {
            return fingerprintSha256 + " | " + subject + " | " + acceptedByProviders;
        }
    }

    /** Einzelner Trust-Check: Name + Provider oder Fingerprint-Set (für CT/Mozilla-URL). */
    private static final class ProviderCheck {
        final String                 name;
        final TrustProviderInterface provider;
        final Set<String>            fingerprintSet; // wenn nicht null, wird nur Fingerprint-Abgleich gemacht

        ProviderCheck(final String name, final TrustProviderInterface provider) {
            this.name = name;
            this.provider = provider;
            this.fingerprintSet = null;
        }

        ProviderCheck(final String name, final Set<String> fingerprintSet) {
            this.name = name;
            this.provider = null;
            this.fingerprintSet = fingerprintSet;
        }
    }

    private static final String       AUTH_TYPE                       = "RSA";
    private final List<ProviderCheck> checks                          = new ArrayList<ProviderCheck>();
    /** Cache für Sycamore-CT-Roots (Fingerprints). */
    private static final String       SYCAMORE_GET_ROOTS_URL          = "https://log.sycamore.ct.letsencrypt.org/2025h2d/ct/v1/get-roots";
    private static Set<String>        sycamoreRootFingerprintsCache;
    private static final Object       SYCAMORE_LOCK                   = new Object();
    /**
     * Apple Root Store – veröffentlichte Liste (z. B. iOS 18 / macOS 15). Siehe https://support.apple.com/en-us/103272
     */
    private static final String       APPLE_ROOT_STORE_URL            = "https://support.apple.com/en-us/126047";
    private static Set<String>        appleRootFingerprintsCache;
    private static final Object       APPLE_LOCK                      = new Object();
    /** Google Chrome Root Store (Chromium). root_store.certs enthält Zeilen "# <sha256>" pro Zertifikat. */
    private static final String       GOOGLE_CHROME_ROOT_STORE_URL    = "https://raw.githubusercontent.com/chromium/chromium/main/net/data/ssl/chrome_root_store/root_store.certs";
    private static Set<String>        googleChromeRootFingerprintsCache;
    private static final Object       GOOGLE_CHROME_LOCK              = new Object();
    /**
     * Android Root Store (platform/system/ca-certificates, google/roots.pem). Use a concrete revision; refs/heads/main does not serve file
     * content. format=TEXT returns base64.
     */
    private static final String       ANDROID_ROOTS_PEM_URL           = "https://android.googlesource.com/platform/system/ca-certificates/+/66ef0b3293faacd95d21ba2d05c4187f0a7e8175/google/roots.pem?format=TEXT";
    private static Set<String>        androidRootFingerprintsCache;
    private static final Object       ANDROID_LOCK                    = new Object();
    /** Cisco Trusted Root Store (TRS), Intersect-Bundle für öffentliches Internet (P7B). */
    private static final String       CISCO_TRS_P7B_URL               = "https://www.cisco.com/security/pki/trs/current/ios/ios.p7b";
    private static Set<String>        ciscoTRSFingerprintsCache;
    private static final Object       CISCO_TRS_LOCK                  = new Object();
    /** Microsoft Trusted Root Program – CCADB-Report (CSV mit SHA-256-Fingerprints). */
    private static final String       MICROSOFT_CCADB_CSV_URL         = "https://ccadb.my.salesforce-sites.com/microsoft/IncludedCACertificateReportForMSFTCSV";
    private static Set<String>        microsoftCCADBFingerprintsCache;
    private static final Object       MICROSOFT_CCADB_LOCK            = new Object();
    /** Mozilla Included Roots PEM (CCADB, TrustBitsInclude=Websites). */
    private static final String       MOZILLA_INCLUDED_ROOTS_PEM_URL  = "https://ccadb.my.salesforce-sites.com/mozilla/IncludedRootsPEMTxt?TrustBitsInclude=Websites";
    private static Set<String>        mozillaIncludedRootsPemFingerprintsCache;
    private static final Object       MOZILLA_INCLUDED_ROOTS_PEM_LOCK = new Object();

    /**
     * Erstellt einen Validator mit JRE-, OS- und CCADB-Provider (für Validierungsberichte gegen alle Quellen).
     */
    public RootCertificateTrustValidator() {
        this(true);
    }

    /**
     * Erstellt einen Validator. Mit {@code includeJreOsCcadb == false} werden JRE, OS und Mozilla (CCADB) nicht hinzugefügt; dann zählen
     * nur die per {@code enable...} aktivierten Stores (z. B. für reine Schnittmengen-Ausgabe).
     */
    public RootCertificateTrustValidator(final boolean includeJreOsCcadb) {
        if (includeJreOsCcadb) {
            try {
                checks.add(new ProviderCheck("JRE", CurrentJRETrustProvider.getInstance()));
            } catch (final Exception e) {
                // skip
            }
            try {
                checks.add(new ProviderCheck("OS", TrustUtils.getOSProvider()));
            } catch (final Exception e) {
                // skip
            }
            try {
                checks.add(new ProviderCheck("Mozilla (CCADB)", new CCADBTrustProvider()));
            } catch (final Exception e) {
                // skip
            }
        }
    }

    /**
     * Fügt eine Trust-Quelle aus einer PFX-/Keystore-Datei hinzu (z. B. Microsoft Roots von certutil). Alle Zertifikate aus dem Keystore
     * werden gelesen; deren SHA-256-Fingerprints werden mit den zu prüfenden Zertifikaten abgeglichen.
     *
     * @param providerName
     *            Anzeigename der Quelle (z. B. "Microsoft (Windows Update)")
     * @param pfxFile
     *            PFX- oder Keystore-Datei (PKCS12, ggf. JKS)
     * @param password
     *            Keystore-Passwort; null oder leer für kein Passwort (typisch bei certutil -exportPFX)
     * @return true wenn mindestens ein Zertifikat geladen wurde, sonst false
     */
    /**
     * Lädt alle X509-Zertifikate aus einer PFX-Datei (Certificate-Entries und Zertifikate aus Key-Entry-Ketten). certutil-exportierte PFX
     * enthalten die Roots oft nur in Key-Entries.
     */
    private static X509Certificate[] loadAllCertificatesFromPfx(final File pfxFile, final char[] password) throws Exception {
        final KeyStore ks = KeyStore.getInstance("PKCS12");
        final InputStream is = new java.io.FileInputStream(pfxFile);
        try {
            ks.load(is, password);
        } finally {
            is.close();
        }
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        final Set<String> seen = new TreeSet<String>();
        for (final java.util.Enumeration<String> e = ks.aliases(); e.hasMoreElements();) {
            final String alias = e.nextElement();
            if (ks.isCertificateEntry(alias)) {
                final Certificate c = ks.getCertificate(alias);
                if (c instanceof X509Certificate) {
                    addCertIfNew((X509Certificate) c, list, seen);
                }
            } else if (ks.isKeyEntry(alias)) {
                final Certificate[] chain = ks.getCertificateChain(alias);
                if (chain != null) {
                    for (int i = 0; i < chain.length; i++) {
                        if (chain[i] instanceof X509Certificate) {
                            addCertIfNew((X509Certificate) chain[i], list, seen);
                        }
                    }
                }
            }
        }
        return list.toArray(new X509Certificate[list.size()]);
    }

    private static void addCertIfNew(final X509Certificate cert, final List<X509Certificate> list, final Set<String> seen) {
        try {
            final String fp = Hash.getSHA256(cert.getEncoded());
            if (fp != null && seen.add(fp.toLowerCase(Locale.ROOT))) {
                list.add(cert);
            }
        } catch (final CertificateEncodingException ignored) {
        }
    }

    public boolean addTrustStoreFromPfx(final String providerName, final File pfxFile, final char[] password) {
        if (providerName == null || pfxFile == null || !pfxFile.isFile()) {
            return false;
        }
        try {
            final X509Certificate[] certs = loadAllCertificatesFromPfx(pfxFile, password);
            if (certs == null || certs.length == 0) {
                return false;
            }
            final Set<String> fingerprints = new TreeSet<String>();
            for (int i = 0; i < certs.length; i++) {
                try {
                    final String fp = Hash.getSHA256(certs[i].getEncoded());
                    if (fp != null) {
                        fingerprints.add(fp.toLowerCase(Locale.ROOT));
                    }
                } catch (final CertificateEncodingException ignored) {
                    // skip
                }
            }
            if (fingerprints.isEmpty()) {
                return false;
            }
            checks.add(new ProviderCheck(providerName, fingerprints));
            return true;
        } catch (final Exception e) {
            return false;
        }
    }

    /**
     * Adds the Microsoft Root Store (Windows Update) from a PFX file produced by certutil. On Windows, run first: certutil
     * -generateSSTFromWU roots.sst, then certutil -exportPFX roots.sst roots.pfx.
     *
     * @param pfxFile
     *            PFX file (e.g. roots.pfx); password is typically empty (null)
     * @return true if the file could be loaded and contains at least one root certificate
     */
    public boolean addMicrosoftRootStoreFromPfx(final File pfxFile) {
        return addMicrosoftRootStoreFromPfx(pfxFile, null);
    }

    /**
     * Same as {@link #addMicrosoftRootStoreFromPfx(File)}, with explicit keystore password. Only CAs acceptable for SSL
     * (TrustUtils.isAcceptableCaTrustAnchorForSsl) are included, so the store reflects intended purpose.
     */
    public boolean addMicrosoftRootStoreFromPfx(final File pfxFile, final char[] password) {
        if (pfxFile == null || !pfxFile.isFile()) {
            return false;
        }
        try {
            final X509Certificate[] certs = loadCertificatesFromPfx(pfxFile, password);
            if (certs == null || certs.length == 0) {
                return false;
            }
            final Set<String> fingerprints = new TreeSet<String>();
            for (int i = 0; i < certs.length; i++) {
                if (!TrustUtils.isAcceptableCaTrustAnchorForSsl(certs[i])) {
                    continue;
                }
                try {
                    final String fp = Hash.getSHA256(certs[i].getEncoded());
                    if (fp != null) {
                        fingerprints.add(fp.toLowerCase(Locale.ROOT));
                    }
                } catch (final CertificateEncodingException ignored) {
                }
            }
            if (!fingerprints.isEmpty()) {
                checks.add(new ProviderCheck("Microsoft (Windows Update)", fingerprints));
                return true;
            }
        } catch (final Exception e) {
            // ignore
        }
        return false;
    }

    /**
     * Reads the Windows "Root" store (Trusted Root Certification Authorities) directly via the Windows Crypto API (JNA). Contains the roots
     * that Windows trusts (including Windows Update). No certutil/PFX required. Windows only; requires JNA/WindowsCertUtils on the
     * classpath.
     *
     * @return true if at least one root was loaded
     */
    public boolean addMicrosoftRootStoreFromWindowsAPI() {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return false;
        }
        try {
            final List<X509Certificate> certs = WindowsCertUtils.getRootStoreCertificates();
            if (certs == null || certs.isEmpty()) {
                return false;
            }
            final Set<String> fingerprints = new TreeSet<String>();
            for (final X509Certificate c : certs) {
                if (!TrustUtils.isAcceptableCaTrustAnchorForSsl(c)) {
                    continue;
                }
                try {
                    final String fp = Hash.getSHA256(c.getEncoded());
                    if (fp != null) {
                        fingerprints.add(fp.toLowerCase(Locale.ROOT));
                    }
                } catch (final CertificateEncodingException ignored) {
                }
            }
            if (!fingerprints.isEmpty()) {
                checks.add(new ProviderCheck("Microsoft (Windows Update)", fingerprints));
                return true;
            }
        } catch (final NoClassDefFoundError e) {
            // JNA / WindowsCertUtils nicht im Classpath
        } catch (final Throwable t) {
            // e.g. UnsatisfiedLinkError when crypt32 cannot be loaded
        }
        return false;
    }

    /**
     * Loads the Microsoft Root Store from Windows Update: runs certutil -generateSSTFromWU and reads the resulting roots.sst via the
     * Windows Crypto API (CertOpenStore with CERT_STORE_PROV_FILENAME). No certutil -exportPFX required. On failure (e.g. JNA not
     * available), falls back to certutil -exportPFX and reading the PFX. Windows only.
     * <p>
     * The SST contains the <i>full</i> WU root catalog (all roots Microsoft can deploy via Windows Update), so the count is typically
     * much higher (e.g. 500+) than the local "Root" store ({@link #addMicrosoftRootStoreFromWindowsAPI()}), which only has the roots
     * actually installed on this machine (e.g. ~177).
     *
     * @param workDir
     *            Directory for roots.sst; null = temporary directory
     * @return true if the SST yielded at least one root certificate (or fallback PFX succeeded)
     */
    public boolean addMicrosoftRootStoreViaCertutil(final File workDir) {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return false;
        }
        File dir = workDir;
        if (dir == null) {
            final String tmp = System.getProperty("java.io.tmpdir", "");
            dir = new File(tmp, "ms-roots-" + Long.toString(System.currentTimeMillis()));
            if (!dir.mkdirs()) {
                return false;
            }
        } else if (!dir.isDirectory()) {
            if (!dir.mkdirs()) {
                return false;
            }
        }
        final File sstFile = new File(dir, "roots.sst");
        final String sstName = sstFile.getName();
        final int exit1 = runProcess(new String[] { "certutil", "-generateSSTFromWU", sstName }, dir);
        if (exit1 != 0 || !sstFile.isFile()) {
            return false;
        }
        // Read SST directly via Windows API (no certutil -exportPFX needed)
        try {
            final List<X509Certificate> certs = WindowsCertUtils.getRootStoreCertificatesFromSstFile(sstFile);
            if (certs != null && !certs.isEmpty()) {
                final Set<String> fingerprints = new TreeSet<String>();
                for (int i = 0; i < certs.size(); i++) {
                    final X509Certificate c = certs.get(i);
                    if (!TrustUtils.isAcceptableCaTrustAnchorForSsl(c)) {
                        continue;
                    }
                    try {
                        final String fp = Hash.getSHA256(c.getEncoded());
                        if (fp != null) {
                            fingerprints.add(fp.toLowerCase(Locale.ROOT));
                        }
                    } catch (final CertificateEncodingException ignored) {
                    }
                }
                if (!fingerprints.isEmpty()) {
                    checks.add(new ProviderCheck("Microsoft (Windows Update)", fingerprints));
                    return true;
                }
            }
        } catch (final NoClassDefFoundError e) {
            // JNA not available
        } catch (final Throwable t) {
            // e.g. crypt32 error
        }
        // Fallback: try PFX export (may fail on some Windows versions)
        final File pfxFile = new File(dir, "roots.pfx");
        final String pfxName = pfxFile.getName();
        final int exit2 = runProcess(new String[] { "certutil", "-exportPFX", sstName, pfxName }, dir);
        if (exit2 != 0 || !pfxFile.isFile()) {
            return false;
        }
        return addMicrosoftRootStoreFromPfx(pfxFile);
    }

    /**
     * Same as {@link #addMicrosoftRootStoreViaCertutil(File)}, uses a temporary directory.
     */
    public boolean addMicrosoftRootStoreViaCertutil() {
        return addMicrosoftRootStoreViaCertutil(null);
    }

    /**
     * Führt einen externen Befehl im angegebenen Arbeitsverzeichnis aus und gibt den Exit-Code zurück. Stdout und Stderr werden gelesen,
     * damit der Prozess nicht blockiert; Stderr wird bei Nicht-Null auf System.err ausgegeben.
     */
    private static int runProcess(final String[] cmd, final File workDir) {
        Process proc = null;
        final ByteArrayOutputStream errOut = new ByteArrayOutputStream();
        try {
            proc = Runtime.getRuntime().exec(cmd, null, workDir);
            final InputStream out = proc.getInputStream();
            final InputStream err = proc.getErrorStream();
            final byte[] buf = new byte[4096];
            final Thread tOut = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        int n;
                        while ((n = out.read(buf)) >= 0) {
                            if (n == 0) {
                                continue;
                            }
                        }
                    } catch (IOException ignored) {
                    } finally {
                        try {
                            out.close();
                        } catch (IOException ignored) {
                        }
                    }
                }
            });
            final Thread tErr = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        int n;
                        while ((n = err.read(buf)) >= 0) {
                            if (n > 0) {
                                errOut.write(buf, 0, n);
                            }
                        }
                    } catch (IOException ignored) {
                    } finally {
                        try {
                            err.close();
                        } catch (IOException ignored) {
                        }
                    }
                }
            });
            tOut.start();
            tErr.start();
            tOut.join();
            tErr.join();
            final int exit = proc.waitFor();
            try {
                final String errStr = new String(errOut.toByteArray(), "UTF-8");
                if (errStr != null && errStr.length() > 0) {
                    System.err.print(errStr);
                }
            } catch (java.io.UnsupportedEncodingException ignored) {
                System.err.print(errOut.toString());
            }
            return exit;
        } catch (Exception e) {
            return -1;
        } finally {
            if (proc != null) {
                try {
                    proc.getInputStream().close();
                } catch (Exception ignored) {
                }
                try {
                    proc.getErrorStream().close();
                } catch (Exception ignored) {
                }
            }
        }
    }

    /**
     * Aktiviert den Abgleich gegen den Google Chrome Root Store (Chromium). Fingerprints werden aus root_store.certs geladen. Funktioniert
     * plattformunabhängig.
     */
    public void enableGoogleChromeRootStore() {
        final Set<String> set = loadGoogleChromeRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Google (Chrome)", set));
        }
    }

    /**
     * Lädt die SHA-256-Fingerprints aus der Chromium root_store.certs (Zeilen "# <64 hex chars>").
     */
    public static Set<String> loadGoogleChromeRootFingerprints() {
        synchronized (GOOGLE_CHROME_LOCK) {
            if (googleChromeRootFingerprintsCache != null) {
                return googleChromeRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String content = readUrlToString(GOOGLE_CHROME_ROOT_STORE_URL);
            if (content == null) {
                return fingerprints;
            }
            final Pattern p = Pattern.compile("#\\s+([0-9a-fA-F]{64})");
            final Matcher m = p.matcher(content);
            while (m.find()) {
                fingerprints.add(m.group(1).toLowerCase(Locale.ROOT));
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (GOOGLE_CHROME_LOCK) {
            googleChromeRootFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Aktiviert den Abgleich gegen die veröffentlichte Apple Root-Store-Liste (SHA-256-Fingerprints aus
     * https://support.apple.com/en-us/103272). Funktioniert plattformunabhängig (nicht nur unter macOS).
     */
    public void enableAppleRootStore() {
        final Set<String> set = loadAppleRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Apple (Root Store)", set));
        }
    }

    /**
     * Lädt die SHA-256-Fingerprints aus der Apple Support-HTML-Seite (Tabellen-Spalte "Fingerprint (SHA-256)"). Format in der Tabelle: "D7
     * A7 A0 FB 5D 7E ..." (32 Bytes, Leerzeichen getrennt).
     */
    public static Set<String> loadAppleRootFingerprints() {
        synchronized (APPLE_LOCK) {
            if (appleRootFingerprintsCache != null) {
                return appleRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String html = readUrlToString(APPLE_ROOT_STORE_URL);
            if (html == null) {
                return fingerprints;
            }
            // Apple: "Fingerprint (SHA-256)"-Spalte enthält 32 Hex-Bytes mit Leerzeichen: "D7 A7 A0 FB ..."
            final Pattern p = Pattern.compile("[0-9A-Fa-f]{2}(?:\\s+[0-9A-Fa-f]{2}){31}");
            final Matcher m = p.matcher(html);
            while (m.find()) {
                final String withSpaces = m.group();
                final String hex = withSpaces.replaceAll("\\s+", "").trim();
                if (hex.length() == 64) {
                    fingerprints.add(hex.toLowerCase(Locale.ROOT));
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (APPLE_LOCK) {
            appleRootFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Aktiviert den Abgleich gegen den Certificate-Transparency-Log Sycamore (Let's Encrypt). Roots, die in diesem Log vorkommen, gelten
     * als "CT (Sycamore)".
     */
    public void enableSycamoreCT() {
        final Set<String> set = loadSycamoreRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("CT (Sycamore)", set));
        }
    }

    /**
     * Lädt die Root-Fingerprints aus dem Sycamore-CT-Log (get-roots API).
     */
    public static Set<String> loadSycamoreRootFingerprints() {
        synchronized (SYCAMORE_LOCK) {
            if (sycamoreRootFingerprintsCache != null) {
                return sycamoreRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String json = readUrlToString(SYCAMORE_GET_ROOTS_URL);
            if (json == null) {
                return fingerprints;
            }
            // "certificates":["base64...","base64...",...]
            final Pattern p = Pattern.compile("\"([A-Za-z0-9+/=]{100,})\"");
            final Matcher m = p.matcher(json);
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            while (m.find()) {
                final String b64 = m.group(1);
                try {
                    final byte[] der = Base64.decode(b64);
                    if (der == null || der.length == 0) {
                        continue;
                    }
                    final java.util.Collection<? extends java.security.cert.Certificate> coll = cf.generateCertificates(new ByteArrayInputStream(der));
                    for (final java.security.cert.Certificate c : coll) {
                        if (c instanceof X509Certificate) {
                            final String fp = Hash.getSHA256(c.getEncoded());
                            if (fp != null) {
                                fingerprints.add(fp.toLowerCase(Locale.ROOT));
                            }
                        }
                    }
                } catch (final Exception ignored) {
                    // skip invalid entry
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (SYCAMORE_LOCK) {
            sycamoreRootFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Aktiviert den Abgleich gegen den Android Root Store (google/roots.pem aus platform/system/ca-certificates). Plattformunabhängig; lädt
     * über Android Googlesource (format=TEXT = Base64).
     */
    public void enableAndroidRootStore() {
        final Set<String> set = loadAndroidRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Android (Root Store)", set));
        }
    }

    /**
     * Lädt die SHA-256-Fingerprints aus der Android roots.pem (Base64-kodiert von Googlesource, entpackt zu PEM).
     */
    public static Set<String> loadAndroidRootFingerprints() {
        synchronized (ANDROID_LOCK) {
            if (androidRootFingerprintsCache != null) {
                return androidRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String base64 = readUrlToString(ANDROID_ROOTS_PEM_URL);
            if (base64 == null || base64.length() == 0) {
                return fingerprints;
            }
            final String base64Clean = base64.replaceAll("\\s+", "");
            final byte[] pemBytes = Base64.decode(base64Clean);
            if (pemBytes == null || pemBytes.length == 0) {
                return fingerprints;
            }
            final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(pemBytes));
            if (certs != null) {
                for (int i = 0; i < certs.length; i++) {
                    try {
                        final String fp = Hash.getSHA256(certs[i].getEncoded());
                        if (fp != null) {
                            fingerprints.add(fp.toLowerCase(Locale.ROOT));
                        }
                    } catch (final CertificateEncodingException ignored) {
                    }
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (ANDROID_LOCK) {
            androidRootFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Lädt die Root-Zertifikate aus der Android roots.pem (für Kandidaten-Union ohne Eingabe-PEM).
     */
    public static X509Certificate[] loadAndroidRootCertificates() {
        try {
            final String base64 = readUrlToString(ANDROID_ROOTS_PEM_URL);
            if (base64 == null || base64.length() == 0) {
                return new X509Certificate[0];
            }
            final byte[] pemBytes = Base64.decode(base64.replaceAll("\\s+", ""));
            if (pemBytes == null || pemBytes.length == 0) {
                return new X509Certificate[0];
            }
            final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(pemBytes));
            return certs != null ? certs : new X509Certificate[0];
        } catch (final Exception e) {
            return new X509Certificate[0];
        }
    }

    /**
     * Activates validation against the Cisco Trusted Root Store (TRS), Intersect bundle (public internet). P7B is parsed via
     * X.509 {@link CertificateFactory} and {@link CertPath} with encoding "PKCS7" (standard in Java SE).
     */
    public void enableCiscoTRS() {
        final Set<String> set = loadCiscoTRSFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Cisco (TRS)", set));
        }
    }

    /**
     * Lädt die SHA-256-Fingerprints aus dem Cisco TRS P7B (ios.p7b).
     */
    public static Set<String> loadCiscoTRSFingerprints() {
        synchronized (CISCO_TRS_LOCK) {
            if (ciscoTRSFingerprintsCache != null) {
                return ciscoTRSFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final byte[] p7b = readUrlToBytes(CISCO_TRS_P7B_URL);
            System.err.println("[Cisco TRS] p7b: " + (p7b == null ? "null" : p7b.length + " bytes"));
            if (p7b == null || p7b.length == 0) {
                return fingerprints;
            }
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            java.util.Collection<? extends Certificate> coll = null;
            try {
                coll = cf.generateCertificates(new ByteArrayInputStream(p7b));
            } catch (final Exception e1) {
                System.err.println("[Cisco TRS] generateCertificates failed: " + e1.getMessage());
            }
            if (coll == null || coll.isEmpty()) {
                try {
                    final CertPath path = cf.generateCertPath(new ByteArrayInputStream(p7b), "PKCS7");
                    coll = path.getCertificates();
                } catch (final Exception e2) {
                    System.err.println("[Cisco TRS] generateCertPath(PKCS7) failed: " + e2.getMessage());
                }
            }
            if (coll == null) {
                coll = Collections.emptyList();
            }
            System.err.println("[Cisco TRS] parsed " + coll.size() + " certs");
            for (final Certificate c : coll) {
                if (c instanceof X509Certificate) {
                    try {
                        final String fp = Hash.getSHA256(c.getEncoded());
                        if (fp != null) {
                            fingerprints.add(fp.toLowerCase(Locale.ROOT));
                        }
                    } catch (final CertificateEncodingException ignored) {
                    }
                }
            }
        } catch (final Exception e) {
            System.err.println("[Cisco TRS] error: " + e.getClass().getSimpleName() + ": " + e.getMessage());
        }
        System.err.println("[Cisco TRS] fingerprints: " + fingerprints.size());
        synchronized (CISCO_TRS_LOCK) {
            ciscoTRSFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Loads root certificates from the Cisco TRS P7B (for candidate union). Uses X.509 factory and CertPath "PKCS7" encoding.
     */
    public static X509Certificate[] loadCiscoTRSCertificates() {
        try {
            final byte[] p7b = readUrlToBytes(CISCO_TRS_P7B_URL);
            if (p7b == null || p7b.length == 0) {
                return new X509Certificate[0];
            }
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            java.util.Collection<? extends Certificate> coll = null;
            try {
                coll = cf.generateCertificates(new ByteArrayInputStream(p7b));
            } catch (final Exception ignored) {
            }
            if (coll == null || coll.isEmpty()) {
                try {
                    final CertPath path = cf.generateCertPath(new ByteArrayInputStream(p7b), "PKCS7");
                    coll = path.getCertificates();
                } catch (final Exception ignored) {
                }
            }
            if (coll == null) {
                coll = Collections.emptyList();
            }
            final List<X509Certificate> list = new ArrayList<X509Certificate>();
            for (final Certificate c : coll) {
                if (c instanceof X509Certificate) {
                    list.add((X509Certificate) c);
                }
            }
            return list.toArray(new X509Certificate[list.size()]);
        } catch (final Exception e) {
            return new X509Certificate[0];
        }
    }

    /**
     * Lädt die Root-Zertifikate aus dem Sycamore-CT get-roots (für Kandidaten-Union).
     */
    public static X509Certificate[] loadSycamoreRootCertificates() {
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        try {
            final String json = readUrlToString(SYCAMORE_GET_ROOTS_URL);
            if (json == null) {
                return new X509Certificate[0];
            }
            final Pattern p = Pattern.compile("\"([A-Za-z0-9+/=]{100,})\"");
            final Matcher m = p.matcher(json);
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            while (m.find()) {
                try {
                    final byte[] der = Base64.decode(m.group(1));
                    if (der == null || der.length == 0) {
                        continue;
                    }
                    final java.util.Collection<? extends Certificate> coll = cf.generateCertificates(new ByteArrayInputStream(der));
                    for (final Certificate c : coll) {
                        if (c instanceof X509Certificate) {
                            list.add((X509Certificate) c);
                        }
                    }
                } catch (final Exception ignored) {
                }
            }
        } catch (final Exception e) {
            // return empty
        }
        return list.toArray(new X509Certificate[list.size()]);
    }

    /**
     * Lädt alle X509-Zertifikate aus einer PFX-Datei (z. B. Microsoft roots.pfx). Passwort null = kein Passwort.
     */
    public static X509Certificate[] loadCertificatesFromPfx(final File pfxFile, final char[] password) {
        if (pfxFile == null || !pfxFile.isFile()) {
            return new X509Certificate[0];
        }
        try {
            return loadAllCertificatesFromPfx(pfxFile, password);
        } catch (final Exception e) {
            return new X509Certificate[0];
        }
    }

    /**
     * Aktiviert den Abgleich gegen die Microsoft-Trusted-Root-Liste aus dem CCADB-CSV-Report (plattformunabhängig, ohne certutil/PFX).
     * Ergänzt oder ersetzt die Microsoft-Quelle aus PFX/certutil.
     */
    public void enableMicrosoftCCADB() {
        final Set<String> set = loadMicrosoftCCADBFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Microsoft (CCADB)", set));
        }
    }

    /**
     * Lädt die SHA-256-Fingerprints aus dem CCADB Microsoft Included CA Certificate Report (CSV). Erwartet eine Spalte "SHA-256
     * Fingerprint" (oder ähnlich); Fingerprints mit Doppelpunkten werden normalisiert.
     */
    public static Set<String> loadMicrosoftCCADBFingerprints() {
        synchronized (MICROSOFT_CCADB_LOCK) {
            if (microsoftCCADBFingerprintsCache != null) {
                return microsoftCCADBFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String csv = readUrlToString(MICROSOFT_CCADB_CSV_URL);
            if (csv == null || csv.length() == 0) {
                return fingerprints;
            }
            final String[] lines = csv.split("\\r?\\n");
            int fpColumnIndex = -1;
            for (int i = 0; i < lines.length; i++) {
                final String line = lines[i];
                if (i == 0) {
                    final String[] headers = line.split(",");
                    for (int j = 0; j < headers.length; j++) {
                        final String h = headers[j].replace("\"", "").trim();
                        if (h != null && (h.equals("SHA-256 Fingerprint") || h.equals("SHA256 Fingerprint") || h.toLowerCase(Locale.ROOT).contains("sha-256") && h.toLowerCase(Locale.ROOT).contains("fingerprint"))) {
                            fpColumnIndex = j;
                            break;
                        }
                    }
                    if (fpColumnIndex < 0) {
                        break;
                    }
                    continue;
                }
                if (fpColumnIndex < 0) {
                    break;
                }
                final String[] cols = line.split(",");
                if (cols.length <= fpColumnIndex) {
                    continue;
                }
                String fp = cols[fpColumnIndex].replace("\"", "").replaceAll("[^0-9a-fA-F]", "").trim();
                if (fp.length() == 64) {
                    fingerprints.add(fp.toLowerCase(Locale.ROOT));
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (MICROSOFT_CCADB_LOCK) {
            microsoftCCADBFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Aktiviert den Abgleich gegen die Mozilla Included Roots (PEM von CCADB, TrustBitsInclude=Websites). Plattformunabhängig; lädt PEM
     * direkt von der CCADB-URL.
     */
    public void enableMozillaIncludedRootsPem() {
        final Set<String> set = loadMozillaIncludedRootsPemFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Mozilla (Included Roots PEM)", set));
        }
    }

    /**
     * Lädt die SHA-256-Fingerprints aus der Mozilla Included Roots PEM (CCADB, Websites).
     */
    public static Set<String> loadMozillaIncludedRootsPemFingerprints() {
        synchronized (MOZILLA_INCLUDED_ROOTS_PEM_LOCK) {
            if (mozillaIncludedRootsPemFingerprintsCache != null) {
                return mozillaIncludedRootsPemFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String pem = readUrlToString(MOZILLA_INCLUDED_ROOTS_PEM_URL);
            if (pem == null || pem.length() == 0) {
                return fingerprints;
            }
            final byte[] pemBytes = pem.getBytes("UTF-8");
            final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(pemBytes));
            if (certs != null) {
                for (int i = 0; i < certs.length; i++) {
                    try {
                        final String fp = Hash.getSHA256(certs[i].getEncoded());
                        if (fp != null) {
                            fingerprints.add(fp.toLowerCase(Locale.ROOT));
                        }
                    } catch (final CertificateEncodingException ignored) {
                    }
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (MOZILLA_INCLUDED_ROOTS_PEM_LOCK) {
            mozillaIncludedRootsPemFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Lädt die Zertifikate aus der Mozilla Included Roots PEM (für Kandidaten-Union).
     */
    public static X509Certificate[] loadMozillaIncludedRootsPemCertificates() {
        try {
            final String pem = readUrlToString(MOZILLA_INCLUDED_ROOTS_PEM_URL);
            if (pem == null || pem.length() == 0) {
                return new X509Certificate[0];
            }
            final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(pem.getBytes("UTF-8")));
            return certs != null ? certs : new X509Certificate[0];
        } catch (final Exception e) {
            return new X509Certificate[0];
        }
    }

    private static String readUrlToString(final String urlString) {
        InputStream is = null;
        try {
            final URL url = new URL(urlString);
            final StringBuilder sb = new StringBuilder();
            is = url.openStream();
            final byte[] buf = new byte[8192];
            int n;
            while ((n = is.read(buf)) > 0) {
                sb.append(new String(buf, 0, n, "UTF-8"));
            }
            return sb.toString();
        } catch (final Exception e) {
            return null;
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (final Exception ignored) {
                }
            }
        }
    }

    /** Liest eine URL binär ein (z. B. für P7B). */
    private static byte[] readUrlToBytes(final String urlString) {
        InputStream is = null;
        try {
            final URL url = new URL(urlString);
            is = url.openStream();
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final byte[] buf = new byte[8192];
            int n;
            while ((n = is.read(buf)) > 0) {
                baos.write(buf, 0, n);
            }
            return baos.toByteArray();
        } catch (final Exception e) {
            return null;
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (final Exception ignored) {
                }
            }
        }
    }

    /**
     * Validiert alle Zertifikate aus dem PEM-Stream und gibt pro Zertifikat die Liste der Trust-Provider zurück, die es akzeptieren.
     *
     * @param pemStream
     *            PEM mit einem oder mehreren Zertifikaten
     * @return Liste der Ergebnisse (ein Eintrag pro Zertifikat)
     */
    public List<CertTrustResult> validateFromPEM(final InputStream pemStream) throws CertificateException, java.io.IOException {
        final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(pemStream);
        return validate(certs);
    }

    /**
     * Validiert die übergebenen Zertifikate gegen alle konfigurierten Trust-Provider.
     *
     * @param certs
     *            Zertifikate (typisch Root-CAs)
     * @return Liste der Ergebnisse (ein Eintrag pro Zertifikat)
     */
    public List<CertTrustResult> validate(final X509Certificate[] certs) {
        if (certs == null || certs.length == 0) {
            return Collections.emptyList();
        }
        final List<CertTrustResult> results = new ArrayList<CertTrustResult>(certs.length);
        for (final X509Certificate cert : certs) {
            results.add(validateOne(cert));
        }
        return results;
    }

    /**
     * Validiert ein einzelnes Zertifikat.
     */
    public CertTrustResult validateOne(final X509Certificate cert) {
        String fingerprint = null;
        try {
            fingerprint = Hash.getSHA256(cert.getEncoded());
        } catch (final CertificateEncodingException e) {
            // use empty fingerprint
        }
        final String fpLower = fingerprint != null ? fingerprint.toLowerCase(Locale.ROOT) : "";
        final String subject = cert.getSubjectX500Principal() != null ? cert.getSubjectX500Principal().getName() : "";
        final List<String> acceptedBy = new ArrayList<String>();
        final X509Certificate[] chain = new X509Certificate[] { cert };
        for (final ProviderCheck check : checks) {
            if (check.fingerprintSet != null) {
                if (check.fingerprintSet.contains(fpLower)) {
                    acceptedBy.add(check.name);
                }
                continue;
            }
            try {
                final TrustResult result = check.provider.checkServerTrusted(chain, AUTH_TYPE, null);
                if (result != null && result.isTrusted()) {
                    acceptedBy.add(check.name);
                }
            } catch (final Exception ignored) {
                // nicht vertrauenswürdig bei diesem Provider
            }
        }
        return new CertTrustResult(fpLower, subject, acceptedBy);
    }

    /** Provider-Namen, die für den Konsens nicht zählen (nur lokale/System-Stores). */
    private static final Set<String> EXCLUDED_FOR_CONSENSUS = new TreeSet<String>(java.util.Arrays.asList(new String[] { "JRE", "OS" }));

    /**
     * Gibt die Namen aller Trust-Provider zurück, die für den „Konsens“ zählen (alle außer JRE und OS). Ein Zertifikat gilt als „von allen
     * akzeptiert“, wenn es von jedem dieser Provider akzeptiert wird.
     */
    public List<String> getConsensusProviderNames() {
        final List<String> names = new ArrayList<String>();
        for (final ProviderCheck check : checks) {
            if (!EXCLUDED_FOR_CONSENSUS.contains(check.name)) {
                names.add(check.name);
            }
        }
        return names;
    }

    /**
     * Gibt die Anzahl der CAs pro Store zurück. Keys = Store-Name, Value = Anzahl (oder -1 wenn nicht ermittelbar, z. B. bei JRE/OS).
     */
    public Map<String, Integer> getStoreSizes() {
        final Map<String, Integer> out = new LinkedHashMap<String, Integer>();
        for (final ProviderCheck check : checks) {
            if (check.fingerprintSet != null) {
                out.put(check.name, Integer.valueOf(check.fingerprintSet.size()));
            } else {
                out.put(check.name, Integer.valueOf(-1));
            }
        }
        return out;
    }

    /**
     * Filtert die Zertifikate auf solche, die von allen Konsens-Providern (alle außer JRE, OS) akzeptiert werden.
     *
     * @param certs
     *            Eingabe-Zertifikate (Reihenfolge wie bei validate())
     * @param results
     *            zugehörige Validierungsergebnisse
     * @return Liste der Zertifikate, die von allen Konsens-Providern akzeptiert werden
     */
    public List<X509Certificate> getCertsAcceptedByAllConsensusProviders(final X509Certificate[] certs, final List<CertTrustResult> results) {
        if (certs == null || results == null || certs.length != results.size()) {
            return Collections.emptyList();
        }
        final List<String> consensus = getConsensusProviderNames();
        if (consensus.isEmpty()) {
            return Collections.emptyList();
        }
        final List<X509Certificate> accepted = new ArrayList<X509Certificate>();
        for (int i = 0; i < certs.length; i++) {
            final List<String> by = results.get(i).getAcceptedByProviders();
            if (by.containsAll(consensus)) {
                accepted.add(certs[i]);
            }
        }
        return accepted;
    }

    /**
     * Returns certificates that are accepted by at least {@code minProviders} consensus providers.
     */
    public static List<X509Certificate> getCertsAcceptedByAtLeastKProviders(final X509Certificate[] certs, final List<CertTrustResult> results, final int minProviders) {
        if (certs == null || results == null || certs.length != results.size() || minProviders < 1) {
            return Collections.emptyList();
        }
        final List<X509Certificate> accepted = new ArrayList<X509Certificate>();
        for (int i = 0; i < certs.length; i++) {
            final int count = results.get(i).getAcceptedByProviders().size();
            if (count >= minProviders) {
                accepted.add(certs[i]);
            }
        }
        return accepted;
    }

    private static final String PEM_HEADER      = "-----BEGIN CERTIFICATE-----";
    private static final String PEM_FOOTER      = "-----END CERTIFICATE-----";
    private static final int    PEM_LINE_LENGTH = 64;

    /**
     * Schreibt die Zertifikate im PEM-Format (Base64, 64 Zeichen pro Zeile).
     */
    public static void writePEM(final List<X509Certificate> certs, final OutputStream out) throws IOException, CertificateEncodingException {
        if (certs == null) {
            return;
        }
        for (final X509Certificate cert : certs) {
            final String b64 = org.appwork.utils.encoding.Base64.encodeToString(cert.getEncoded(), false);
            out.write((PEM_HEADER + "\n").getBytes("UTF-8"));
            for (int i = 0; i < b64.length(); i += PEM_LINE_LENGTH) {
                final int end = Math.min(i + PEM_LINE_LENGTH, b64.length());
                out.write(b64.substring(i, end).getBytes("UTF-8"));
                out.write('\n');
            }
            out.write((PEM_FOOTER + "\n").getBytes("UTF-8"));
        }
    }

    /**
     * Keeps only certificates that are acceptable as CA trust anchors for TLS web server verification (see
     * {@link TrustUtils#isAcceptableCaTrustAnchorForSsl}).
     */
    public static X509Certificate[] filterToTlsServerAuthOnly(final X509Certificate[] certs) {
        if (certs == null || certs.length == 0) {
            return certs;
        }
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        for (final X509Certificate c : certs) {
            if (TrustUtils.isAcceptableCaTrustAnchorForSsl(c)) {
                list.add(c);
            }
        }
        return list.toArray(new X509Certificate[list.size()]);
    }

    /**
     * Bildet die Vereinigung mehrerer Zertifikats-Arrays und dedupliziert anhand SHA-256-Fingerprint.
     */
    public static X509Certificate[] unionByFingerprint(final X509Certificate[]... certArrays) {
        final Set<String> seen = new TreeSet<String>();
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        for (int a = 0; a < certArrays.length; a++) {
            if (certArrays[a] == null) {
                continue;
            }
            for (int i = 0; i < certArrays[a].length; i++) {
                final X509Certificate c = certArrays[a][i];
                try {
                    final String fp = Hash.getSHA256(c.getEncoded());
                    if (fp != null && seen.add(fp.toLowerCase(Locale.ROOT))) {
                        list.add(c);
                    }
                } catch (final CertificateEncodingException ignored) {
                }
            }
        }
        return list.toArray(new X509Certificate[list.size()]);
    }

    /**
     * Counts how many validation results are accepted by all of the given stores (intersection).
     */
    public static int countAcceptedByStoreSubset(final List<CertTrustResult> results, final List<String> storeSubset) {
        if (results == null || storeSubset == null || storeSubset.isEmpty()) {
            return 0;
        }
        int count = 0;
        for (final CertTrustResult r : results) {
            if (r.getAcceptedByProviders().containsAll(storeSubset)) {
                count++;
            }
        }
        return count;
    }

    /** Builds all subsets of storeNames of the given size; each subset is a list of names in original order. */
    private static void collectSubsetsOfSize(final List<String> storeNames, final int size, final int start, final List<String> current, final List<List<String>> out) {
        if (current.size() == size) {
            out.add(new ArrayList<String>(current));
            return;
        }
        for (int i = start; i < storeNames.size(); i++) {
            current.add(storeNames.get(i));
            collectSubsetsOfSize(storeNames, size, i + 1, current, out);
            current.remove(current.size() - 1);
        }
    }

    /**
     * Prints for each non-empty subset of storeNames how many candidates are accepted by all stores in that subset (intersection size).
     * Sorted by count descending (most CAs in common at the top).
     */
    public static void printIntersectionCountsByCombination(final List<CertTrustResult> results, final List<String> storeNames) {
        if (storeNames == null || storeNames.isEmpty()) {
            return;
        }
        final List<Object[]> entries = new ArrayList<Object[]>();
        for (int size = 1; size <= storeNames.size(); size++) {
            final List<List<String>> subsets = new ArrayList<List<String>>();
            collectSubsetsOfSize(storeNames, size, 0, new ArrayList<String>(), subsets);
            for (final List<String> subset : subsets) {
                final int count = countAcceptedByStoreSubset(results, subset);
                final String combo = subset.size() == 1 ? subset.get(0) : String.join(" + ", subset);
                entries.add(new Object[] { Integer.valueOf(count), Integer.valueOf(size), combo });
            }
        }
        Collections.sort(entries, new Comparator<Object[]>() {
            @Override
            public int compare(final Object[] a, final Object[] b) {
                final int c1 = ((Integer) a[0]).intValue();
                final int c2 = ((Integer) b[0]).intValue();
                if (c1 != c2) {
                    return c2 - c1;
                }
                return ((Integer) a[1]).intValue() - ((Integer) b[1]).intValue();
            }
        });
        System.out.println("--- CAs in common by store combination (intersection, most first) ---");
        for (final Object[] e : entries) {
            final int count = ((Integer) e[0]).intValue();
            final int size = ((Integer) e[1]).intValue();
            final String combo = (String) e[2];
            System.out.println("  " + count + "  [" + size + "] " + combo);
        }
    }

    /**
     * Gibt eine lesbare Übersicht aller Ergebnisse aus (z.B. für Konsole).
     */
    public static String formatResults(final List<CertTrustResult> results) {
        final StringBuilder sb = new StringBuilder();
        sb.append("Fingerprint (SHA-256)                    | Subject (CN/...) | Accepted by (Trust Provider)\n");
        sb.append("------------------------------------------|------------------|-----------------------------------\n");
        for (final CertTrustResult r : results) {
            sb.append(r.getFingerprintSha256()).append(" | ");
            final String sub = r.getSubject().length() > 60 ? r.getSubject().substring(0, 57) + "..." : r.getSubject();
            sb.append(sub).append(" | ");
            sb.append(r.getAcceptedByProviders().isEmpty() ? "(none)" : String.join(", ", r.getAcceptedByProviders()));
            sb.append("\n");
        }
        return sb.toString();
    }

    /**
     * Hauptmethode: Keine Eingabe-PEM. Lädt Kandidaten aus den verlinkten Stores (Android, Cisco, Sycamore, ggf. Microsoft PFX), ermittelt
     * die Schnittmenge aller per {@code enable...} aktivierten Stores und schreibt nur diese CAs in die Ausgabe-PEM. JRE-, OS- und
     * CCADB-Provider werden nicht verwendet.
     * <p>
     * Aufruf: java ... RootCertificateTrustValidator [ausgabe.pem] [roots.pfx]
     * <ul>
     * <li>1. Argument: Ausgabe-PEM (Default: accepted_by_all.pem)</li>
     * <li>2. Argument: optional Microsoft roots.pfx; otherwise on Windows: certutil (SST from WU, read via API), then local Root store</li>
     * </ul>
     */
    public static void main(final String[] args) {
        final RootCertificateTrustValidator validator = new RootCertificateTrustValidator(false);
        validator.enableSycamoreCT();
        validator.enableAppleRootStore();
        validator.enableGoogleChromeRootStore();
        validator.enableAndroidRootStore();
        validator.enableCiscoTRS();
        // validator.enableMicrosoftCCADB();
        validator.enableMozillaIncludedRootsPem();
        File msPfx = null;
        if (args != null && args.length >= 2 && args[1] != null && !args[1].isEmpty()) {
            msPfx = new File(args[1]);
        }
        if (msPfx == null || !msPfx.isFile()) {
            final File defaultPfx = new File(System.getProperty("user.dir", ""), "roots.pfx");
            if (defaultPfx.isFile()) {
                msPfx = defaultPfx;
            }
        }
        if (msPfx != null && msPfx.isFile() && validator.addMicrosoftRootStoreFromPfx(msPfx)) {
            System.err.println("Microsoft (Windows Update) roots loaded from: " + msPfx.getPath());
        } else if (validator.addMicrosoftRootStoreViaCertutil()) {
            System.err.println("Microsoft (Windows Update) roots loaded via certutil (SST from WU). SST = full WU catalog (often 500+); local Root store = installed only (e.g. ~177).");
        } else if (validator.addMicrosoftRootStoreFromWindowsAPI()) {
            System.err.println("Microsoft (Windows Update) roots loaded from local Windows Root store (installed roots only).");
        } else {
            System.err.println("Microsoft (Windows Update): not loaded (no PFX, certutil+SST or local store succeeded).");
        }
        final Map<String, Integer> storeSizes = validator.getStoreSizes();
        System.out.println("Filter: only CAs acceptable for TLS server auth (TrustUtils.isAcceptableCaTrustAnchorForSsl). CAs for code signing, email, etc. only are excluded.");
        // Order and full list of all stores (including when not loaded, e.g. Cisco TRS, Microsoft WU)
        final String[] storeOrder = new String[] { "CT (Sycamore)", "Apple (Root Store)", "Google (Chrome)", "Android (Root Store)", "Cisco (TRS)", "Microsoft (CCADB)", "Mozilla (Included Roots PEM)", "Microsoft (Windows Update)" };
        System.out.println("CAs per store:");
        for (final String name : storeOrder) {
            final Integer n = storeSizes.get(name);
            if (n != null) {
                System.out.println("  " + name + ": " + (n.intValue() >= 0 ? n : "(n/a)"));
            } else {
                System.out.println("  " + name + ": (not loaded)");
            }
        }
        for (final Map.Entry<String, Integer> e : storeSizes.entrySet()) {
            final String name = e.getKey();
            boolean inOrder = false;
            for (int i = 0; i < storeOrder.length; i++) {
                if (name.equals(storeOrder[i])) {
                    inOrder = true;
                    break;
                }
            }
            if (!inOrder) {
                final int n = e.getValue().intValue();
                System.out.println("  " + name + ": " + (n >= 0 ? n : "(n/a)"));
            }
        }
        final X509Certificate[] androidCerts = loadAndroidRootCertificates();
        final X509Certificate[] ciscoCerts = loadCiscoTRSCertificates();
        final X509Certificate[] sycamoreCerts = loadSycamoreRootCertificates();
        final X509Certificate[] mozillaCerts = loadMozillaIncludedRootsPemCertificates();
        X509Certificate[] msCerts = new X509Certificate[0];
        if (msPfx != null && msPfx.isFile()) {
            msCerts = loadCertificatesFromPfx(msPfx, null);
        }
        X509Certificate[] candidates = unionByFingerprint(androidCerts, ciscoCerts, sycamoreCerts, mozillaCerts, msCerts);
        if (candidates.length == 0) {
            System.err.println("No candidate certificates loaded from stores.");
            System.exit(1);
        }
        final int unionCount = candidates.length;
        System.out.println("Candidate certificates (union from stores): " + unionCount);
        candidates = filterToTlsServerAuthOnly(candidates);
        System.out.println("Candidate certificates (filtered by TrustUtils.isAcceptableCaTrustAnchorForSsl): " + candidates.length + (unionCount > candidates.length ? " (filtered out " + (unionCount - candidates.length) + " CAs)" : ""));
        if (candidates.length == 0) {
            System.err.println("No candidates left after filtering (TrustUtils.isAcceptableCaTrustAnchorForSsl: CA + serverAuth + keyCertSign/cRLSign).");
            System.exit(1);
        }
        final List<CertTrustResult> results = validator.validate(candidates);
        final List<String> storeNames = validator.getConsensusProviderNames();
        // Per-store: how many of the candidates does each store accept? (helps explain a small intersection)
        System.out.println("Candidates accepted per store (consensus):");
        for (final String storeName : storeNames) {
            int count = 0;
            for (final CertTrustResult r : results) {
                if (r.getAcceptedByProviders().contains(storeName)) {
                    count++;
                }
            }
            System.out.println("  " + storeName + ": " + count + " of " + candidates.length);
        }
        final List<X509Certificate> acceptedByAll = validator.getCertsAcceptedByAllConsensusProviders(candidates, results);
        System.out.println("Stores (all must accept): " + storeNames);
        System.out.println("CAs in all stores (intersection): " + acceptedByAll.size() + " — only roots present in every store; different store policies explain a small number.");
        printIntersectionCountsByCombination(results, storeNames);
        final String outPath = (args != null && args.length > 0 && args[0] != null && !args[0].isEmpty()) ? args[0] : "accepted_by_all.pem";
        final File outFile = new File(outPath);
        try {
            final FileOutputStream fos = new FileOutputStream(outFile);
            try {
                writePEM(acceptedByAll, fos);
            } finally {
                fos.close();
            }
            System.out.println("PEM written to: " + outFile.getAbsolutePath());
        } catch (final Exception e) {
            System.err.println("Failed to write PEM: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
        final String basePath = outPath.endsWith(".pem") ? outPath.substring(0, outPath.length() - 4) : outPath;
        final int numStores = storeNames.size();
        for (int k = 1; k <= numStores; k++) {
            final List<X509Certificate> atLeastK = getCertsAcceptedByAtLeastKProviders(candidates, results, k);
            final File kFile = new File(basePath + "." + k + ".pem");
            try {
                final FileOutputStream fos = new FileOutputStream(kFile);
                try {
                    writePEM(atLeastK, fos);
                } finally {
                    fos.close();
                }
                System.out.println("PEM (>= " + k + " providers): " + kFile.getAbsolutePath() + " (" + atLeastK.size() + " CAs)");
            } catch (final Exception e) {
                System.err.println("Failed to write " + kFile.getName() + ": " + e.getMessage());
            }
        }
    }
}
