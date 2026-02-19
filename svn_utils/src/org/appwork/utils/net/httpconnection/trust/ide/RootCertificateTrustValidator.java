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
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Hash;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;
import org.appwork.utils.os.WindowsCertUtils;

/**
 * Validates root certificates against multiple trust providers (JRE, OS, Mozilla/CCADB, CT Sycamore, Apple, Google Chrome, Android,
 * optionally Microsoft Windows Update or Microsoft CCADB). Output: per certificate, the list of providers that accept it as a trusted root
 * CA.
 * <p>
 * Apple Root Store: See <a href="https://support.apple.com/en-us/103272">Available root certificates for Apple operating systems</a>;
 * fingerprints are parsed from the published HTML list (e.g. iOS/macOS Root Store).
 * <p>
 * Microsoft (Windows Update): Load root store as PFX, created with certutil (Windows only):<br>
 * {@code certutil -generateSSTFromWU roots.sst}<br>
 * {@code certutil -dump roots.sst} (optional, for verification)<br>
 * {@code certutil -exportPFX roots.sst roots.pfx}<br>
 * To enable the Microsoft (Windows Update) store call {@link #enableMicrosoftWindowsUpdate()} or
 * {@link #enableMicrosoftWindowsUpdate(File)} (tries PFX, then certutil, then Windows API). Alternatively use
 * {@link #addMicrosoftRootStoreFromPfx(File)}, {@link #addMicrosoftRootStoreViaCertutil()}, or
 * {@link #addMicrosoftRootStoreFromWindowsAPI()} directly. {@link #enableMicrosoftCCADB()} loads the Microsoft list from the CCADB CSV
 * (cross-platform).
 * <p>
 * Android Root Store: {@link #enableAndroidRootStore()} loads google/roots.pem from Android Googlesource. Mozilla Included Roots
 * (Websites): {@link #enableMozillaIncludedRootsPem()} loads the PEM from CCADB (TrustBitsInclude=Websites). Mozilla NSS certdata (trust
 * bits): {@link #enableMozillaCertdata()} loads certdata.txt and uses CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR for TLS/Websites.
 * <p>
 * <b>Certificate sources vs. fingerprint-only:</b> Apple and Google Chrome supply only SHA-256 fingerprints (from HTML/root_store.certs);
 * they do not supply certificate bytes. The actual certificates used as candidates and written to the output PEM come only from stores that
 * provide full certs: Android (PEM), Sycamore CT (get-roots), Mozilla (PEM), and Microsoft (PFX/certutil/Windows API). Apple and Chrome are
 * used only during validation to mark which of those candidates they would accept; they never contribute cert objects.
 * <p>
 * <b>EKU (Extended Key Usage) and validity:</b> Only roots suitable for TLS server authentication are included. Where the source provides
 * that information it is enforced: <b>Microsoft CCADB</b> CSV uses columns "Microsoft Status" (Included), "Microsoft EKUs" (must contain
 * "Server Authentication"), "Valid To [GMT]" (not expired). <b>Mozilla certdata</b> uses CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR.
 * <b>Mozilla Included Roots PEM</b> is pre-filtered by CCADB (Websites). For sources that supply full certificates (Android, Sycamore,
 * Mozilla PEM, Microsoft PFX/API) EKU is enforced via {@link TrustUtils#isAcceptableCaTrustAnchorForSsl} (CA + serverAuth when EKU
 * present). <b>Expired</b> certificates (notAfter before start of today GMT) are excluded everywhere via
 * {@link #filterOutExpiredCertificates}.
 * <p>
 * Compile to bin-cursor (this class only, rest from Eclipse bin):<br>
 * javac -source 1.6 -target 1.6 -encoding UTF-8 -cp "bin;libs/*;dev_libs/*" -d bin-cursor
 * src/org/appwork/utils/net/httpconnection/trust/RootCertificateTrustValidator.java
 * <p>
 * Run (no input PEM; candidates come from stores, output = intersection of all enabled stores):<br>
 * java ... RootCertificateTrustValidator [output.pem] [roots.pfx]
 * <p>
 * Optional: {@code -DRootCertificateTrustValidator.fillMissingFromCrtsh=true} fills candidates that exist only in Apple/Chrome
 * (fingerprint-only stores) by loading the cert from <a href="https://crt.sh/">crt.sh</a> by SHA-256; the downloaded cert is verified by
 * fingerprint before use (max 200 fetches).
 */
public final class RootCertificateTrustValidator {
    /** Result per certificate: fingerprint, subject, and the names of providers that accept it. */
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

    /** Single trust check: name + provider or fingerprint set (for CT/Mozilla URL). */
    private static final class ProviderCheck {
        final String                 name;
        final TrustProviderInterface provider;
        final Set<String>            fingerprintSet; // if not null, only fingerprint comparison is done

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
    /** Cache for Sycamore CT roots (fingerprints). */
    private static final String       SYCAMORE_GET_ROOTS_URL          = "https://log.sycamore.ct.letsencrypt.org/2025h2d/ct/v1/get-roots";
    private static Set<String>        sycamoreRootFingerprintsCache;
    private static final Object       SYCAMORE_LOCK                   = new Object();
    /**
     * Apple Root Store – published list (e.g. iOS 18 / macOS 15). See https://support.apple.com/en-us/103272
     */
    private static final String       APPLE_ROOT_STORE_URL            = "https://support.apple.com/en-us/126047";
    private static Set<String>        appleRootFingerprintsCache;
    private static final Object       APPLE_LOCK                      = new Object();
    /** Google Chrome Root Store (Chromium). root_store.certs contains lines "# <sha256>" per certificate. */
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
    /** Microsoft Trusted Root Program – CCADB report (CSV with SHA-256 fingerprints). */
    private static final String       MICROSOFT_CCADB_CSV_URL         = "https://ccadb.my.salesforce-sites.com/microsoft/IncludedCACertificateReportForMSFTCSV";
    private static Set<String>        microsoftCCADBFingerprintsCache;
    private static Set<String>        microsoftCCADBDisabledFingerprintsCache;
    private static final Object       MICROSOFT_CCADB_LOCK            = new Object();
    /** Mozilla Included Roots PEM (CCADB, TrustBitsInclude=Websites). */
    private static final String       MOZILLA_INCLUDED_ROOTS_PEM_URL  = "https://ccadb.my.salesforce-sites.com/mozilla/IncludedRootsPEMTxt?TrustBitsInclude=Websites";
    private static Set<String>        mozillaIncludedRootsPemFingerprintsCache;
    private static final Object       MOZILLA_INCLUDED_ROOTS_PEM_LOCK = new Object();
    /** Mozilla NSS certdata.txt (CKA_TRUST_SERVER_AUTH etc.). */
    private static final String       MOZILLA_CERTDATA_URL            = "https://hg.mozilla.org/projects/nss/raw-file/tip/lib/ckfw/builtins/certdata.txt";
    private static Set<String>        mozillaCertdataFingerprintsCache;
    private static final Object       MOZILLA_CERTDATA_LOCK           = new Object();
    /** crt.sh: search by SHA-256 fingerprint (returns HTML); download cert by id (returns PEM). */
    private static final String       CRTSH_QUERY_URL                 = "https://crt.sh/?q=";
    private static final String       CRTSH_DOWNLOAD_URL              = "https://crt.sh/?d=";

    /**
     * Creates a validator. With {@code includeJreOsCcadb == false}, JRE, OS and Mozilla (CCADB) are not added; only stores enabled via
     * {@code enable...} are used (e.g. for intersection-only output).
     */
    public RootCertificateTrustValidator() {
    }

    /**
     * Adds a trust source from a PFX/keystore file (e.g. Microsoft roots from certutil). All certificates from the keystore are read; their
     * SHA-256 fingerprints are matched against the certificates to be validated.
     *
     * @param providerName
     *            Display name of the source (e.g. "Microsoft (Windows Update)")
     * @param pfxFile
     *            PFX or keystore file (PKCS12, or JKS)
     * @param password
     *            Keystore password; null or empty for no password (typical for certutil -exportPFX)
     * @return true if at least one certificate was loaded, false otherwise
     */
    /**
     * Loads all X509 certificates from a PFX file (certificate entries and certificates from key-entry chains). certutil-exported PFX often
     * contains roots only in key entries.
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
                if (isCertificateExpired(certs[i])) {
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
                if (isCertificateExpired(c)) {
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
     * The SST contains the <i>full</i> WU root catalog (all roots Microsoft can deploy via Windows Update), so the count is typically much
     * higher (e.g. 500+) than the local "Root" store ({@link #addMicrosoftRootStoreFromWindowsAPI()}), which only has the roots actually
     * installed on this machine (e.g. ~177).
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
                    if (isCertificateExpired(c)) {
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
     * Enables validation against the Microsoft Root Store (Windows Update). Tries to load the store in order: from the given PFX file (if
     * non-null and existing), then from default {@code roots.pfx} in the current directory, then via certutil (SST from Windows Update),
     * then from the local Windows Root store (Windows API). Stops at the first successful source and adds one check named "Microsoft
     * (Windows Update)". Call this to enable the Microsoft WU store; do not call it to leave it disabled.
     *
     * @param optionalPfxFile
     *            Optional PFX file (e.g. from 2nd command-line arg); may be null
     * @return true if the store was loaded and added, false otherwise
     */
    public boolean enableMicrosoftWindowsUpdate(final File optionalPfxFile) {
        if (optionalPfxFile != null && optionalPfxFile.isFile() && addMicrosoftRootStoreFromPfx(optionalPfxFile)) {
            return true;
        }
        final File defaultPfx = new File(System.getProperty("user.dir", ""), "roots.pfx");
        if (defaultPfx.isFile() && addMicrosoftRootStoreFromPfx(defaultPfx)) {
            return true;
        }
        if (addMicrosoftRootStoreViaCertutil()) {
            return true;
        }
        if (addMicrosoftRootStoreFromWindowsAPI()) {
            return true;
        }
        return false;
    }

    /**
     * Same as {@link #enableMicrosoftWindowsUpdate(File)} with null (tries default roots.pfx, then certutil, then Windows API).
     */
    public boolean enableMicrosoftWindowsUpdate() {
        return enableMicrosoftWindowsUpdate(null);
    }

    /**
     * Runs an external command in the given working directory and returns the exit code. Stdout and stderr are read so the process does not
     * block; stderr is printed to System.err when non-null.
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
     * Enables validation against the Google Chrome Root Store (Chromium). Fingerprints are loaded from root_store.certs. Works
     * cross-platform.
     */
    public void enableGoogleChromeRootStore() {
        final Set<String> set = loadGoogleChromeRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Google (Chrome)", set));
        }
    }

    /**
     * Loads SHA-256 fingerprints from Chromium root_store.certs (lines "# <64 hex chars>").
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
     * Enables validation against the published Apple Root Store list (SHA-256 fingerprints from https://support.apple.com/en-us/103272).
     * Works cross-platform (not only on macOS).
     */
    public void enableAppleRootStore() {
        final Set<String> set = loadAppleRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Apple (Root Store)", set));
        }
    }

    /**
     * Loads SHA-256 fingerprints from the Apple Support HTML page (table column "Fingerprint (SHA-256)"). Table format: "D7 A7 A0 FB 5D 7E
     * ..." (32 bytes, space-separated).
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
            // Apple: "Fingerprint (SHA-256)" column contains 32 hex bytes with spaces: "D7 A7 A0 FB ..."
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
     * Enables validation against the Certificate Transparency log Sycamore (Let's Encrypt). Roots present in this log are treated as "CT
     * (Sycamore)".
     */
    public void enableSycamoreCT() {
        final Set<String> set = loadSycamoreRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("CT (Sycamore)", set));
        }
    }

    /**
     * Loads root fingerprints from the Sycamore CT log (get-roots API).
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
     * Enables validation against the Android Root Store (google/roots.pem from platform/system/ca-certificates). Cross-platform; loads via
     * Android Googlesource (format=TEXT = Base64).
     */
    public void enableAndroidRootStore() {
        final Set<String> set = loadAndroidRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Android (Root Store)", set));
        }
    }

    /**
     * Loads SHA-256 fingerprints from the Android roots.pem (Base64-encoded from Googlesource, decoded to PEM).
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
     * Loads root certificates from the Android roots.pem (for candidate union when no input PEM is used).
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
     * Loads root certificates from the Sycamore CT get-roots API (for candidate union).
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
     * Loads all X509 certificates from a PFX file (e.g. Microsoft roots.pfx). Password null = no password.
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
     * Enables validation against the Microsoft Trusted Root list from the CCADB CSV report (cross-platform, no certutil/PFX). Complements
     * or replaces the Microsoft source from PFX/certutil.
     */
    public void enableMicrosoftCCADB() {
        final Set<String> set = loadMicrosoftCCADBFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Microsoft (CCADB)", set));
        }
    }

    /**
     * Parses a CSV line respecting double-quoted fields (fields may contain commas). Escaped quote "" inside a quoted field becomes one ".
     *
     * @param line
     *            one line of CSV
     * @return list of field values (unquoted, trimmed)
     */
    private static List<String> parseCsvLine(final String line) {
        final List<String> out = new ArrayList<String>();
        if (line == null) {
            return out;
        }
        int i = 0;
        while (i < line.length()) {
            if (line.charAt(i) == '"') {
                final StringBuilder sb = new StringBuilder();
                i++;
                while (i < line.length()) {
                    if (line.charAt(i) == '"') {
                        i++;
                        if (i < line.length() && line.charAt(i) == '"') {
                            sb.append('"');
                            i++;
                        } else {
                            break;
                        }
                    } else {
                        sb.append(line.charAt(i));
                        i++;
                    }
                }
                out.add(sb.toString().trim());
                while (i < line.length() && line.charAt(i) == ',') {
                    i++;
                }
            } else {
                int start = i;
                while (i < line.length() && line.charAt(i) != ',') {
                    i++;
                }
                out.add(line.substring(start, i).trim());
                if (i < line.length()) {
                    i++;
                }
            }
        }
        return out;
    }

    /** Date format used in Microsoft CCADB CSV "Valid To [GMT]" column (e.g. "2025 Jul 23"). */
    private static final String MICROSOFT_CCADB_VALID_TO_FORMAT = "yyyy MMM dd";

    /**
     * Loads SHA-256 fingerprints from the CCADB Microsoft Included CA Certificate Report (CSV). Only rows that are currently trusted for
     * TLS server authentication are included:
     * <ul>
     * <li><b>Microsoft Status</b> must be "Included" (not "Disabled"). The report lists both; "Disabled" means Microsoft has removed or
     * restricted the root (e.g. distrust, policy change), so only "Included" roots are accepted.</li>
     * <li><b>Microsoft EKUs</b> must contain "Server Authentication" (website/TLS use).</li>
     * <li><b>Valid To [GMT]</b> must be today or in the future (expired roots are excluded).</li>
     * </ul>
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
            if (lines.length < 2) {
                return fingerprints;
            }
            final List<String> header = parseCsvLine(lines[0]);
            int statusIdx = -1;
            int fpIdx = -1;
            int ekusIdx = -1;
            int validToIdx = -1;
            for (int j = 0; j < header.size(); j++) {
                final String h = header.get(j);
                if ("Microsoft Status".equals(h)) {
                    statusIdx = j;
                } else if ("SHA-256 Fingerprint".equals(h) || "SHA256 Fingerprint".equals(h) || (h != null && h.toLowerCase(Locale.ROOT).contains("sha-256") && h.toLowerCase(Locale.ROOT).contains("fingerprint"))) {
                    fpIdx = j;
                } else if ("Microsoft EKUs".equals(h)) {
                    ekusIdx = j;
                } else if ("Valid To [GMT]".equals(h)) {
                    validToIdx = j;
                }
            }
            if (fpIdx < 0) {
                return fingerprints;
            }
            final SimpleDateFormat df = new SimpleDateFormat(MICROSOFT_CCADB_VALID_TO_FORMAT, Locale.ROOT);
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            final Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            now.set(Calendar.HOUR_OF_DAY, 0);
            now.set(Calendar.MINUTE, 0);
            now.set(Calendar.SECOND, 0);
            now.set(Calendar.MILLISECOND, 0);
            final long todayStart = now.getTimeInMillis();
            for (int i = 1; i < lines.length; i++) {
                final List<String> cols = parseCsvLine(lines[i]);
                if (cols.size() <= fpIdx) {
                    continue;
                }
                if (statusIdx >= 0 && statusIdx < cols.size()) {
                    final String status = cols.get(statusIdx).trim();
                    if (!"Included".equalsIgnoreCase(status)) {
                        continue;
                    }
                }
                if (ekusIdx >= 0 && ekusIdx < cols.size()) {
                    final String ekus = cols.get(ekusIdx);
                    if (ekus == null || !ekus.contains("Server Authentication")) {
                        continue;
                    }
                }
                if (validToIdx >= 0 && validToIdx < cols.size()) {
                    final String validToStr = cols.get(validToIdx).trim();
                    if (validToStr.length() > 0) {
                        try {
                            final Date validTo = df.parse(validToStr);
                            if (validTo != null && validTo.getTime() < todayStart) {
                                continue;
                            }
                        } catch (final Exception ignored) {
                        }
                    }
                }
                final String fp = cols.get(fpIdx).replaceAll("[^0-9a-fA-F]", "").trim();
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
     * Loads SHA-256 fingerprints of roots that are explicitly <b>Disabled</b> in the CCADB Microsoft report (Microsoft Status = "Disabled").
     * Used for the rejected/disabled output (appwork-merged-cadb-rejected.*.pem). No EKU or Valid To filter so all disabled roots are included.
     */
    public static Set<String> loadMicrosoftCCADBDisabledFingerprints() {
        synchronized (MICROSOFT_CCADB_LOCK) {
            if (microsoftCCADBDisabledFingerprintsCache != null) {
                return microsoftCCADBDisabledFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String csv = readUrlToString(MICROSOFT_CCADB_CSV_URL);
            if (csv == null || csv.length() == 0) {
                return fingerprints;
            }
            final String[] lines = csv.split("\\r?\\n");
            if (lines.length < 2) {
                return fingerprints;
            }
            final List<String> header = parseCsvLine(lines[0]);
            int statusIdx = -1;
            int fpIdx = -1;
            for (int j = 0; j < header.size(); j++) {
                final String h = header.get(j);
                if ("Microsoft Status".equals(h)) {
                    statusIdx = j;
                } else if ("SHA-256 Fingerprint".equals(h) || "SHA256 Fingerprint".equals(h) || (h != null && h.toLowerCase(Locale.ROOT).contains("sha-256") && h.toLowerCase(Locale.ROOT).contains("fingerprint"))) {
                    fpIdx = j;
                }
            }
            if (fpIdx < 0 || statusIdx < 0) {
                return fingerprints;
            }
            for (int i = 1; i < lines.length; i++) {
                final List<String> cols = parseCsvLine(lines[i]);
                if (cols.size() <= fpIdx || cols.size() <= statusIdx) {
                    continue;
                }
                if (!"Disabled".equalsIgnoreCase(cols.get(statusIdx).trim())) {
                    continue;
                }
                final String fp = cols.get(fpIdx).replaceAll("[^0-9a-fA-F]", "").trim();
                if (fp.length() == 64) {
                    fingerprints.add(fp.toLowerCase(Locale.ROOT));
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (MICROSOFT_CCADB_LOCK) {
            microsoftCCADBDisabledFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Enables validation against the Mozilla Included Roots (PEM from CCADB, TrustBitsInclude=Websites). Cross-platform; loads PEM directly
     * from the CCADB URL.
     */
    public void enableMozillaIncludedRootsPem() {
        final Set<String> set = loadMozillaIncludedRootsPemFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Mozilla (Included Roots PEM)", set));
        }
    }

    /**
     * Loads SHA-256 fingerprints from the Mozilla Included Roots PEM (CCADB, Websites).
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
     * Loads certificates from the Mozilla Included Roots PEM (for candidate union).
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

    /**
     * Enables validation against the Mozilla NSS certdata.txt store. Uses trust bits: only roots with CKA_TRUST_SERVER_AUTH =
     * CKT_NSS_TRUSTED_DELEGATOR are considered trusted for TLS/Websites (same semantics as the Included Roots PEM, but from the
     * authoritative NSS certdata source).
     */
    public void enableMozillaCertdata() {
        final Set<String> set = loadMozillaCertdataFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Mozilla (certdata)", set));
        }
    }

    /**
     * Loads SHA-256 fingerprints from Mozilla certdata.txt (roots with CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR).
     */
    public static Set<String> loadMozillaCertdataFingerprints() {
        synchronized (MOZILLA_CERTDATA_LOCK) {
            if (mozillaCertdataFingerprintsCache != null) {
                return mozillaCertdataFingerprintsCache;
            }
        }
        Set<String> fingerprints = new TreeSet<String>();
        try {
            final InputStream is = new URL(MOZILLA_CERTDATA_URL).openStream();
            try {
                fingerprints = MozillaCertdataParser.getTrustedForServerAuthFingerprints(is);
            } finally {
                is.close();
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (MOZILLA_CERTDATA_LOCK) {
            mozillaCertdataFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Loads certificates from Mozilla certdata.txt (trusted for server auth only) for candidate union.
     */
    public static X509Certificate[] loadMozillaCertdataCertificates() {
        try {
            final InputStream is = new URL(MOZILLA_CERTDATA_URL).openStream();
            try {
                return MozillaCertdataParser.getTrustedForServerAuthCertificates(is);
            } finally {
                is.close();
            }
        } catch (final Exception e) {
            return new X509Certificate[0];
        }
    }

    /**
     * Loads SHA-256 fingerprints of roots explicitly distrusted for server auth (CKA_TRUST_SERVER_AUTH = CKT_NSS_NOT_TRUSTED) from
     * Mozilla certdata.txt. Used for appwork-merged-cadb-rejected.*.pem.
     */
    public static Set<String> loadMozillaCertdataDistrustedFingerprints() {
        try {
            final InputStream is = new URL(MOZILLA_CERTDATA_URL).openStream();
            try {
                return MozillaCertdataParser.getExplicitlyDistrustedForServerAuthFingerprints(is);
            } finally {
                is.close();
            }
        } catch (final Exception e) {
            return new TreeSet<String>();
        }
    }

    /**
     * Loads certificates explicitly distrusted for server auth from Mozilla certdata.txt (for rejected PEM output).
     */
    public static X509Certificate[] loadMozillaCertdataDistrustedCertificates() {
        try {
            final InputStream is = new URL(MOZILLA_CERTDATA_URL).openStream();
            try {
                return MozillaCertdataParser.getExplicitlyDistrustedForServerAuthCertificates(is);
            } finally {
                is.close();
            }
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

    /** Reads a URL as binary (e.g. for P7B). */
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
     * Validates all certificates from the PEM stream and returns per certificate the list of trust providers that accept it.
     *
     * @param pemStream
     *            PEM with one or more certificates
     * @return list of results (one entry per certificate)
     */
    public List<CertTrustResult> validateFromPEM(final InputStream pemStream) throws CertificateException, java.io.IOException {
        final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(pemStream);
        return validate(certs);
    }

    /**
     * Validates the given certificates against all configured trust providers.
     *
     * @param certs
     *            certificates (typically root CAs)
     * @return list of results (one entry per certificate)
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
     * Validates a single certificate.
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
                // not trusted by this provider
            }
        }
        return new CertTrustResult(fpLower, subject, acceptedBy);
    }

    /** Provider names that do not count for consensus (local/system stores only). */
    private static final Set<String> EXCLUDED_FOR_CONSENSUS = new TreeSet<String>(java.util.Arrays.asList(new String[] { "JRE", "OS" }));

    /**
     * Returns the names of all trust providers that count for "consensus" (all except JRE and OS). A certificate is considered "accepted by
     * all" if it is accepted by each of these providers.
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
     * Returns the number of CAs per store. Keys = store name, Value = count (or -1 if not determinable, e.g. for JRE/OS).
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
     * Filters certificates to those accepted by all consensus providers (all except JRE, OS).
     *
     * @param certs
     *            input certificates (same order as for validate())
     * @param results
     *            corresponding validation results
     * @return list of certificates accepted by all consensus providers
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

    /**
     * Returns certificates from {@code rejectedCerts} that appear in at least {@code minProviders} of the disabled fingerprint sets (used for
     * appwork-merged-cadb-rejected.k.pem).
     */
    public static List<X509Certificate> getCertsRejectedByAtLeastKProviders(final List<X509Certificate> rejectedCerts, final List<Set<String>> disabledFingerprintSets, final int minProviders) {
        if (rejectedCerts == null || disabledFingerprintSets == null || minProviders < 1) {
            return Collections.emptyList();
        }
        final List<X509Certificate> out = new ArrayList<X509Certificate>();
        for (final X509Certificate cert : rejectedCerts) {
            try {
                final String fp = Hash.getSHA256(cert.getEncoded());
                if (fp == null) {
                    continue;
                }
                final String fpLower = fp.toLowerCase(Locale.ROOT);
                int count = 0;
                for (int i = 0; i < disabledFingerprintSets.size(); i++) {
                    if (disabledFingerprintSets.get(i) != null && disabledFingerprintSets.get(i).contains(fpLower)) {
                        count++;
                    }
                }
                if (count >= minProviders) {
                    out.add(cert);
                }
            } catch (final CertificateEncodingException ignored) {
            }
        }
        return out;
    }

    /**
     * Returns certificates that are in every disabled fingerprint set (rejected by all providers that have a disabled list).
     */
    public static List<X509Certificate> getCertsRejectedByAllDisabledProviders(final List<X509Certificate> rejectedCerts, final List<Set<String>> disabledFingerprintSets) {
        if (disabledFingerprintSets == null || disabledFingerprintSets.isEmpty()) {
            return Collections.emptyList();
        }
        return getCertsRejectedByAtLeastKProviders(rejectedCerts, disabledFingerprintSets, disabledFingerprintSets.size());
    }

    private static final String PEM_HEADER      = "-----BEGIN CERTIFICATE-----";
    private static final String PEM_FOOTER      = "-----END CERTIFICATE-----";
    private static final int    PEM_LINE_LENGTH = 64;

    /**
     * Writes the certificates in PEM format (Base64, 64 characters per line).
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
     * {@link TrustUtils#isAcceptableCaTrustAnchorForSsl}). EKU: when present, must contain serverAuth (enforced there).
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

    /** Start of today in GMT (00:00:00.000) for expiry checks. Certs valid until this date are still considered valid. */
    private static long getTodayStartGmt() {
        final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal.getTimeInMillis();
    }

    /**
     * Returns true if the certificate is expired (notAfter before start of today GMT). Null notAfter is treated as not expired.
     */
    private static boolean isCertificateExpired(final X509Certificate cert) {
        if (cert == null) {
            return true;
        }
        try {
            final Date notAfter = cert.getNotAfter();
            return notAfter != null && notAfter.getTime() < getTodayStartGmt();
        } catch (final Exception e) {
            return true;
        }
    }

    /**
     * Removes expired certificates (notAfter before start of today GMT). Applied globally so no source contributes expired roots to the
     * candidate set or store fingerprint sets.
     */
    public static X509Certificate[] filterOutExpiredCertificates(final X509Certificate[] certs) {
        if (certs == null || certs.length == 0) {
            return certs;
        }
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        for (final X509Certificate c : certs) {
            if (!isCertificateExpired(c)) {
                list.add(c);
            }
        }
        return list.toArray(new X509Certificate[list.size()]);
    }

    /**
     * Loads a single certificate from crt.sh by SHA-256 fingerprint. Fetches the search page, parses the certificate id, downloads PEM via
     * ?d=id, verifies the cert's SHA-256 matches the requested fingerprint, and returns the cert. Returns null if not found, parse error,
     * or fingerprint mismatch.
     *
     * @param sha256Hex
     *            64 hex characters (lower or upper case), no colons
     * @return the certificate, or null
     */
    public static X509Certificate loadCertificateFromCrtshByFingerprint(final String sha256Hex) {
        if (sha256Hex == null || sha256Hex.length() != 64 || !sha256Hex.matches("[0-9a-fA-F]{64}")) {
            return null;
        }
        final String normalized = sha256Hex.toLowerCase(Locale.ROOT).replaceAll(":", "");
        final String html = readUrlToString(CRTSH_QUERY_URL + normalized);
        if (html == null || html.isEmpty() || html.toLowerCase(Locale.ROOT).contains("no records found") || html.toLowerCase(Locale.ROOT).contains("0 results")) {
            return null;
        }
        final Matcher idMatcher = Pattern.compile("\\?id=(\\d+)").matcher(html);
        if (!idMatcher.find()) {
            return null;
        }
        final String id = idMatcher.group(1);
        final String pem = readUrlToString(CRTSH_DOWNLOAD_URL + id);
        if (pem == null || !pem.contains("-----BEGIN CERTIFICATE-----")) {
            return null;
        }
        try {
            final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(pem.getBytes("UTF-8")));
            if (certs == null || certs.length == 0) {
                return null;
            }
            final X509Certificate cert = certs[0];
            final String actualFp = Hash.getSHA256(cert.getEncoded());
            if (actualFp == null || !actualFp.toLowerCase(Locale.ROOT).equals(normalized)) {
                return null;
            }
            return cert;
        } catch (final Exception e) {
            return null;
        }
    }

    /**
     * Loads certificates from crt.sh for the given fingerprints. Each cert is verified by SHA-256 before inclusion. Stops after
     * {@code maxToLoad} successful loads to limit network use (use Integer.MAX_VALUE for no limit).
     *
     * @param fingerprints
     *            set of SHA-256 hex strings (64 chars, no colons)
     * @param maxToLoad
     *            maximum number of certs to fetch from crt.sh
     * @return array of certificates successfully loaded and verified (may be empty)
     */
    public static X509Certificate[] loadCertificatesFromCrtshByFingerprints(final Set<String> fingerprints, final int maxToLoad) {
        if (fingerprints == null || fingerprints.isEmpty() || maxToLoad <= 0) {
            return new X509Certificate[0];
        }
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        int loaded = 0;
        for (final String fp : fingerprints) {
            if (loaded >= maxToLoad) {
                break;
            }
            final X509Certificate cert = loadCertificateFromCrtshByFingerprint(fp);
            if (cert != null) {
                list.add(cert);
                loaded++;
            }
        }
        return list.toArray(new X509Certificate[list.size()]);
    }

    /**
     * Builds the union of multiple certificate arrays and deduplicates by SHA-256 fingerprint.
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
     * Prints to the console for each CA that is not accepted by all consensus stores: which stores have it (present in) and in which stores
     * it is missing. Helps identify which store provides a CA that is missing from the intersection.
     *
     * @param results
     *            validation results (one per candidate cert)
     * @param consensusStoreNames
     *            store names that define "all must accept" (e.g. from getConsensusProviderNames())
     */
    public static void printMissingCaReport(final List<CertTrustResult> results, final List<String> consensusStoreNames) {
        if (results == null || consensusStoreNames == null || consensusStoreNames.isEmpty()) {
            return;
        }
        System.out.println("--- CAs not in all stores: which store has / misses the CA ---");
        int missingCount = 0;
        for (final CertTrustResult r : results) {
            final List<String> acceptedBy = r.getAcceptedByProviders();
            if (acceptedBy.containsAll(consensusStoreNames)) {
                continue;
            }
            missingCount++;
            final List<String> missingIn = new ArrayList<String>();
            for (final String storeName : consensusStoreNames) {
                if (!acceptedBy.contains(storeName)) {
                    missingIn.add(storeName);
                }
            }
            final String subject = r.getSubject().length() > 70 ? r.getSubject().substring(0, 67) + "..." : r.getSubject();
            System.out.println("CA: " + subject);
            System.out.println("  SHA-256: " + r.getFingerprintSha256());
            System.out.println("  Present in: " + (acceptedBy.isEmpty() ? "(none)" : join(acceptedBy, ", ")));
            System.out.println("  Missing in: " + (missingIn.isEmpty() ? "(none)" : join(missingIn, ", ")));
            System.out.println();
        }
        if (missingCount == 0) {
            System.out.println("(all candidate CAs are in every store)");
        } else {
            System.out.println("Total CAs missing in at least one store: " + missingCount);
        }
    }

    private static String join(final List<String> list, final String sep) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < list.size(); i++) {
            if (i > 0) {
                sb.append(sep);
            }
            sb.append(list.get(i));
        }
        return sb.toString();
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
     * Returns a readable overview of all results (e.g. for console).
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
     * Main entry: No input PEM. Loads candidates from the linked stores (Android, Sycamore, optionally Microsoft PFX), computes the
     * intersection of all stores enabled via {@code enable...}, and writes only those CAs to the output PEM. JRE, OS and CCADB providers
     * are not used.
     * <p>
     * Invocation: java ... RootCertificateTrustValidator [output.pem] [roots.pfx]
     * <ul>
     * <li>1st argument: optional output PEM path; if omitted, files are written to the same directory as {@code common-ca-database.pem}
     * (ccadb package) with base name {@value #DEFAULT_MERGED_PEM_BASENAME}</li>
     * <li>2nd argument: optional Microsoft roots.pfx; otherwise on Windows: certutil (SST from WU, read via API), then local Root
     * store</li>
     * </ul>
     */
    private static final String DEFAULT_MERGED_PEM_BASENAME       = "appwork-merged-cadb";
    private static final String DEFAULT_MERGED_PEM_REJECTED_BASE = "appwork-merged-cadb-rejected";

    public static void main(final String[] args) {
        final RootCertificateTrustValidator validator = new RootCertificateTrustValidator();
        // validator.enableSycamoreCT();
        validator.enableAppleRootStore();
        validator.enableGoogleChromeRootStore();
        validator.enableAndroidRootStore();
        validator.enableMicrosoftCCADB();
        validator.enableMozillaIncludedRootsPem();
        validator.enableMozillaCertdata();
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
        if (validator.enableMicrosoftWindowsUpdate(msPfx)) {
            System.err.println("Microsoft (Windows Update) roots loaded.");
        } else {
            System.err.println("Microsoft (Windows Update): not loaded (omit enableMicrosoftWindowsUpdate() to disable).");
        }
        final Map<String, Integer> storeSizes = validator.getStoreSizes();
        System.out.println("Filter: only CAs acceptable for TLS server auth (TrustUtils.isAcceptableCaTrustAnchorForSsl). CAs for code signing, email, etc. only are excluded.");
        // Order and full list of all stores (including when not loaded, e.g. Microsoft WU)
        final String[] storeOrder = new String[] { "CT (Sycamore)", "Apple (Root Store)", "Google (Chrome)", "Android (Root Store)", "Microsoft (CCADB)", "Mozilla (Included Roots PEM)", "Mozilla (certdata)", "Microsoft (Windows Update)" };
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
        // Only these stores supply full certificates; Apple and Google Chrome supply fingerprints only (used later in validate()).
        final X509Certificate[] androidCerts = loadAndroidRootCertificates();
        final X509Certificate[] sycamoreCerts = loadSycamoreRootCertificates();
        final X509Certificate[] mozillaCerts = loadMozillaIncludedRootsPemCertificates();
        X509Certificate[] msCerts = new X509Certificate[0];
        if (msPfx != null && msPfx.isFile()) {
            msCerts = loadCertificatesFromPfx(msPfx, null);
        }
        X509Certificate[] candidates = unionByFingerprint(androidCerts, sycamoreCerts, mozillaCerts, msCerts);
        if (candidates.length == 0) {
            System.err.println("No candidate certificates loaded from stores.");
            System.exit(1);
        }
        // Optional: fill certs that exist only in Apple/Chrome (fingerprint-only) from crt.sh; verify fingerprint after download.
        if (true) {
            final Set<String> haveFp = new TreeSet<String>();
            for (int i = 0; i < candidates.length; i++) {
                try {
                    final String fp = Hash.getSHA256(candidates[i].getEncoded());
                    if (fp != null) {
                        haveFp.add(fp.toLowerCase(Locale.ROOT));
                    }
                } catch (final CertificateEncodingException ignored) {
                }
            }
            final Set<String> apple = loadAppleRootFingerprints();
            final Set<String> chrome = loadGoogleChromeRootFingerprints();
            final Set<String> missing = new TreeSet<String>();
            if (apple != null) {
                missing.addAll(apple);
            }
            if (chrome != null) {
                missing.addAll(chrome);
            }
            missing.removeAll(haveFp);
            final int maxFill = 200;
            final X509Certificate[] fromCrtsh = loadCertificatesFromCrtshByFingerprints(missing, maxFill);
            if (fromCrtsh.length > 0) {
                candidates = unionByFingerprint(candidates, fromCrtsh);
                System.out.println("Filled " + fromCrtsh.length + " missing cert(s) from crt.sh (fingerprint verified); total candidates: " + candidates.length);
            }
        }
        final int unionCount = candidates.length;
        System.out.println("Candidate certificates (union from stores): " + unionCount);
        candidates = filterToTlsServerAuthOnly(candidates);
        final int afterEku = candidates.length;
        System.out.println("Candidate certificates (filtered by TrustUtils.isAcceptableCaTrustAnchorForSsl, EKU serverAuth): " + afterEku + (unionCount > afterEku ? " (filtered out " + (unionCount - afterEku) + " CAs)" : ""));
        candidates = filterOutExpiredCertificates(candidates);
        System.out.println("Candidate certificates (after excluding expired): " + candidates.length + (afterEku > candidates.length ? " (filtered out " + (afterEku - candidates.length) + " expired)" : ""));
        if (candidates.length == 0) {
            System.err.println("No candidates left after filtering (CA + serverAuth EKU + keyCertSign/cRLSign + not expired).");
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
        printMissingCaReport(results, storeNames);
        printIntersectionCountsByCombination(results, storeNames);
        final File outDir;
        final String baseName;
        outDir = new File(AWTest.getWorkspace(), "AppWorkutils/src/org/appwork/utils/net/httpconnection/trust/ccadb");
        baseName = DEFAULT_MERGED_PEM_BASENAME;
        final File outFile = new File(outDir, baseName + ".pem");
        if (!outDir.isDirectory() && !outDir.mkdirs()) {
            System.err.println("Failed to create output directory: " + outDir.getAbsolutePath());
            System.exit(1);
        }
        final int numStores = storeNames.size();
        for (File f : outDir.listFiles()) {
            if (f.getName().startsWith(baseName)) {
                f.delete();
            }
        }
        for (int k = 1; k <= numStores; k++) {
            final List<X509Certificate> atLeastK = getCertsAcceptedByAtLeastKProviders(candidates, results, k);
            if (atLeastK.size() == 0) {
                continue;
            }
            final File kFile = new File(outDir, baseName + "." + k + ".pem");
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
