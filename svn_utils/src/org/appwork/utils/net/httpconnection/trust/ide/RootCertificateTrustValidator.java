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
import java.io.FileInputStream;
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
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.stringify.FlexiJSonPrettyStringify;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.AllTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;
import org.appwork.utils.net.httpconnection.trust.ccadb.tests.CCADBCertificateVerificationTest;
import org.appwork.utils.os.WindowsCertUtils;

/**
 * Validates root certificates against multiple trust providers (JRE, OS, Mozilla/CCADB, CT Sycamore, Apple, Google Chrome, Android,
 * optionally Microsoft Windows Update or Microsoft CCADB). Output: per certificate, the list of providers that accept it as a trusted root
 * CA.
 * <p>
 * Apple Root Store: See <a href="https://support.apple.com/en-us/103272">Available root certificates for Apple operating systems</a>;
 * fingerprints are parsed from the published HTML list (e.g. iOS/macOS Root Store).
 * <p>
 * Microsoft (Windows Update): SST from certutil, read via JNA (Windows only):<br>
 * {@code certutil -generateSSTFromWU roots.sst}<br>
 * SST is read via Windows Crypto API (no PFX). To enable the Microsoft (Windows Update) store call {@link #enableMicrosoftWindowsUpdate()}
 * or {@link #enableMicrosoftWindowsUpdate(File)} (certutil SST then Windows API). Alternatively use
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
 * provide full certs: Android (PEM), Sycamore CT (get-roots), Mozilla (PEM), and Microsoft (certutil SST + JNA / Windows API). Apple and
 * Chrome are used only during validation to mark which of those candidates they would accept; they never contribute cert objects.
 * <p>
 * <b>EKU (Extended Key Usage) and validity:</b> Only roots suitable for TLS server authentication are included. Where the source provides
 * that information it is enforced: <b>Microsoft CCADB</b> CSV uses columns "Microsoft Status" (Included), "Microsoft EKUs" (must contain
 * "Server Authentication"), "Valid To [GMT]" (not expired). <b>Mozilla certdata</b> uses CKA_TRUST_SERVER_AUTH = CKT_NSS_TRUSTED_DELEGATOR.
 * <b>Mozilla Included Roots PEM</b> is pre-filtered by CCADB (Websites). For sources that supply full certificates (Android, Sycamore,
 * Mozilla PEM, Microsoft certutil/JNA/API) EKU is enforced via {@link TrustUtils#isAcceptableCaTrustAnchorForSsl} (CA + serverAuth when EKU
 * present). <b>Expired</b> certificates (notAfter before start of today GMT) are excluded everywhere via
 * {@link #filterOutExpiredCertificates}.
 * <p>
 * Compile to bin-cursor (this class only, rest from Eclipse bin):<br>
 * javac -source 1.6 -target 1.6 -encoding UTF-8 -cp "bin;libs/*;dev_libs/*" -d bin-cursor
 * src/org/appwork/utils/net/httpconnection/trust/RootCertificateTrustValidator.java
 * <p>
 * Run (no input PEM; candidates come from stores, output = intersection of all enabled stores):<br>
 * java ... RootCertificateTrustValidator [output.pem]
 * <p>
 * Optional: {@code -DRootCertificateTrustValidator.fillMissingFromCrtsh=true} fills candidates that exist only in Apple/Chrome
 * (fingerprint-only stores) by loading the cert from <a href="https://crt.sh/">crt.sh</a> by SHA-256; the downloaded cert is verified by
 * fingerprint before use (max 200 fetches).
 * <p>
 * <b>Reference: zmap/rootfetch</b> (https://github.com/zmap/rootfetch) – Python scripts to fetch root stores from Apple, Microsoft, Mozilla
 * NSS, Java (keytool), Android (git), and Google CT (get-roots). Comparison: we use <i>Mozilla</i> certdata from hg.mozilla.org (NSS tip),
 * same idea; <i>Microsoft</i> we use certutil/SST and Disallowed store, they use authroot.stl + per-cert .crt from
 * download.windowsupdate.com; <i>Apple</i> we use support.apple.com lists, they use opensource.apple.com tarballs; <i>Android</i> we use
 * googlesource roots.pem (pinned rev), they clone git and use latest stable tag; <i>CT</i> we use Sycamore and Google Pilot as separate
 * providers (RFC 6962 get-roots); they use multiple Google CT logs and a union. Useful URLs from rootfetch: Microsoft authroot.stl =
 * {@code http://www.download.windowsupdate.com/msdownload/update/v3/static/trustedr/en/authroot.stl} (alternative to SST); Google CT
 * get-roots e.g. {@code https://ct.googleapis.com/pilot/ct/v1/get-roots}.
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

    /** Reason why a certificate is in a rejected/disabled list. Used in rejected JSON as array of enum names per fingerprint. */
    public enum RejectionReason {
        MICROSOFT_DISABLED_CCADB,
        MOZILLA_DISTRUSTED,
        APPLE_BLOCKED_CONSTRAINED,
        CHROME_BLOCKLIST,
        CRTSH_REVOKED,
        MICROSOFT_DISALLOWED_STORE
    }

    /**
     * Returns a short description and optional URL for the rejection reason (for console output).
     */
    public static String getRejectionReasonDescription(final RejectionReason reason) {
        if (reason == null) {
            return "";
        }
        switch (reason) {
        case MICROSOFT_DISABLED_CCADB:
            return "Microsoft Trusted Root Program: root is in CCADB report with status 'Disabled' (no longer trusted by Microsoft).";
        case MOZILLA_DISTRUSTED:
            return "Mozilla NSS certdata: root is explicitly distrusted for server auth (CKT_NSS_NOT_TRUSTED).";
        case APPLE_BLOCKED_CONSTRAINED:
            return "Apple: root is Blocked or TLS constrained (cannot be used for TLS). S/MIME, Timestamp, Code Signing constrained are not included. See https://support.apple.com/en-us/103255";
        case CHROME_BLOCKLIST:
            return "Chromium/Chrome blocklist: root (or cert) is listed as compromised or misissued. See Chromium net/data/ssl/blocklist.";
        case CRTSH_REVOKED:
            return "crt.sh: certificate is reported as revoked or rejected in the crt.sh database.";
        case MICROSOFT_DISALLOWED_STORE:
            return "Microsoft Windows: root is in the Disallowed Certificates store (distrusted by Windows Update / enterprise).";
        default:
            return reason.name();
        }
    }

    private static final String       AUTH_TYPE                       = "RSA";
    private final List<ProviderCheck> checks                          = new ArrayList<ProviderCheck>();
    /** CT get-roots URLs (RFC 6962); see rootfetch ct.py. */
    private static final String       SYCAMORE_GET_ROOTS_URL          = "https://log.sycamore.ct.letsencrypt.org/2025h2d/ct/v1/get-roots";
    private static final String       GOOGLE_CT_PILOT_GET_ROOTS_URL   = "https://ct.googleapis.com/pilot/ct/v1/get-roots";
    private static Set<String>        sycamoreRootFingerprintsCache;
    private static final Object       SYCAMORE_LOCK                   = new Object();
    private static Set<String>        googlePilotRootFingerprintsCache;
    private static final Object       GOOGLE_PILOT_LOCK               = new Object();
    /**
     * Apple Root Store from opensource tarball (certificates/roots as DER). List of tags from GitHub API; latest tag used for download. See
     * https://github.com/apple-oss-distributions/security_certificates
     */
    private static final String       APPLE_TARBALL_TAGS_API          = "https://api.github.com/repos/apple-oss-distributions/security_certificates/tags";
    private static final String       APPLE_TARBALL_ARCHIVE_BASE      = "https://github.com/apple-oss-distributions/security_certificates/archive/refs/tags/";
    private static Set<String>        appleRootFingerprintsCache;
    private static final Object       APPLE_LOCK                      = new Object();
    /**
     * Apple CA certificates with additional constraints (blocked or constrained for TLS/S/MIME/etc.). See
     * https://support.apple.com/en-us/103255
     */
    private static final String       APPLE_RESTRICTED_URL            = "https://support.apple.com/en-us/103255";
    private static Set<String>        appleRestrictedFingerprintsCache;
    private static final Object       APPLE_RESTRICTED_LOCK           = new Object();
    /**
     * Chromium net/data/ssl tree at main (latest). See https://chromium.googlesource.com/chromium/src/+/main/net/data/ssl Used so all
     * Chromium SSL data URLs point to the same revision; change this to pin a commit or use another branch.
     */
    private static final String       CHROMIUM_SSL_DATA_BASE_URL      = "https://raw.githubusercontent.com/chromium/chromium/main/net/data/ssl";
    /**
     * Chromium/Chrome certificate blocklist (compromised/misissued certs). README lists .pem filenames = cert SHA-256.
     */
    private static final String       CHROMIUM_BLOCKLIST_README_URL   = CHROMIUM_SSL_DATA_BASE_URL + "/blocklist/README.md";
    private static Set<String>        chromiumBlocklistFingerprintsCache;
    private static final Object       CHROMIUM_BLOCKLIST_LOCK         = new Object();
    /** Google Chrome Root Store (Chromium). root_store.certs contains lines "# <sha256>" per certificate. */
    private static final String       GOOGLE_CHROME_ROOT_STORE_URL    = CHROMIUM_SSL_DATA_BASE_URL + "/chrome_root_store/root_store.certs";
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
    /** Microsoft AuthRoot from authroot.stl (CTL) + per-cert .crt; cross-platform. See rootfetch microsoft.py. */
    private static final String       MICROSOFT_AUTHROOT_STL_URL      = "http://www.download.windowsupdate.com/msdownload/update/v3/static/trustedr/en/authroot.stl";
    private static final String       MICROSOFT_AUTHROOT_CRT_BASE     = "http://www.download.windowsupdate.com/msdownload/update/v3/static/trustedr/en/";
    private static Set<String>        microsoftAuthRootStlFingerprintsCache;
    private static final Object       MICROSOFT_AUTHROOT_STL_LOCK     = new Object();
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
    /** Retry crt.sh requests for up to 5 minutes before giving up. */
    private static final long         CRTSH_RETRY_TIMEOUT_MS          = 5L * 60 * 1000;
    /** Delay between retries (ms). */
    private static final long         CRTSH_RETRY_DELAY_MS            = 5000;

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
        final byte[] bytes = IO.readFile(pfxFile);
        ks.load(new ByteArrayInputStream(bytes), password);
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
    /**
     * Returns the set of SHA-256 fingerprints (lowercase) of roots in the local Windows Root store (same semantics as
     * {@link org.appwork.utils.net.httpconnection.trust.JNAWindowsTrustProvider}). Used to restrict SST-loaded certs to those that are also
     * trusted by the local machine. Returns null on non-Windows or when the store cannot be read.
     */
    private static Set<String> getLocalWindowsRootFingerprints() {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return null;
        }
        try {
            final List<X509Certificate> certs = WindowsCertUtils.getRootStoreCertificates();
            if (certs == null || certs.isEmpty()) {
                return null;
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
            return fingerprints;
        } catch (final NoClassDefFoundError e) {
            return null;
        } catch (final Throwable t) {
            return null;
        }
    }

    public boolean addMicrosoftRootStoreFromWindowsAPI() {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return false;
        }
        try {
            final Set<String> fingerprints = getLocalWindowsRootFingerprints();
            if (fingerprints == null || fingerprints.isEmpty()) {
                return false;
            }
            checks.add(new ProviderCheck("Microsoft (Windows Update)", fingerprints));
            return true;
        } catch (final NoClassDefFoundError e) {
            return false;
        } catch (final Throwable t) {
            return false;
        }
    }

    /**
     * Loads the Microsoft Root Store from Windows Update: runs certutil -generateSSTFromWU and reads the resulting roots.sst via the
     * Windows Crypto API (CertOpenStore with CERT_STORE_PROV_FILENAME). SST is always loaded via certutil and read via JNA; no PFX
     * fallback. Only certs that are also in the local Windows Root store (same as
     * {@link org.appwork.utils.net.httpconnection.trust.JNAWindowsTrustProvider}) are used; validation semantics match WindowsTrustProvider
     * / JNAWindowsTrustProvider. Windows only.
     * <p>
     * When reading the SST, roots marked <i>Disabled</i> or <i>Not Before</i> (via CERT_FRIENDLY_NAME_PROP_ID) are excluded by
     * {@link org.appwork.utils.os.WindowsCertUtils#getRootStoreCertificatesFromSstFile(java.io.File)}. Only roots present in both the SST
     * and the local Windows Root store are added.
     *
     * @param workDir
     *            Directory for roots.sst; null = temporary directory
     * @return true if the SST yielded at least one root certificate
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
        // Read SST via Windows API (JNA); no PFX.
        // Only use certs that are also in the local Windows Root store (same set as JNAWindowsTrustProvider).
        try {
            final Set<String> localRootFingerprints = getLocalWindowsRootFingerprints();
            final List<X509Certificate> certs = WindowsCertUtils.getRootStoreCertificatesFromSstFile(sstFile);
            if (certs != null && !certs.isEmpty() && localRootFingerprints != null && !localRootFingerprints.isEmpty()) {
                for (int i = 0; i < certs.size(); i++) {
                    final X509Certificate c = certs.get(i);
                    try {
                        final String subject = c.getSubjectDN() != null ? c.getSubjectDN().getName() : "";
                        final List<String> eku = c.getExtendedKeyUsage();
                        // System.out.println("SST EKU [" + (i + 1) + "/" + certs.size() + "] " + subject + " => " +
                        // formatEkuForConsole(eku));
                    } catch (final Exception e) {
                        // System.out.println("SST EKU [" + (i + 1) + "/" + certs.size() + "] (error reading cert: " + e.getMessage() +
                        // ")");
                    }
                }
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
                            final String fpLower = fp.toLowerCase(Locale.ROOT);
                            if (localRootFingerprints.contains(fpLower)) {
                                fingerprints.add(fpLower);
                            }
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
        return false;
    }

    /**
     * Same as {@link #addMicrosoftRootStoreViaCertutil(File)}, uses a temporary directory.
     */
    public boolean addMicrosoftRootStoreViaCertutil() {
        return addMicrosoftRootStoreViaCertutil(null);
    }

    /**
     * Loads Microsoft root certificates from SST (certutil -generateSSTFromWU, read via JNA), restricted to those also in the local Windows
     * Root store. Used to build the candidate list in main(); no PFX. Returns null on non-Windows, certutil failure, or when no certs pass
     * the filter.
     *
     * @return array of X509Certificate, or null
     */
    static X509Certificate[] loadMicrosoftWindowsUpdateCertificatesFromSst() {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return null;
        }
        File dir = null;
        try {
            final String tmp = System.getProperty("java.io.tmpdir", "");
            dir = new File(tmp, "ms-roots-" + Long.toString(System.currentTimeMillis()));
            if (!dir.mkdirs()) {
                return null;
            }
            final File sstFile = new File(dir, "roots.sst");
            final String sstName = sstFile.getName();
            final int exit1 = runProcess(new String[] { "certutil", "-generateSSTFromWU", sstName }, dir);
            if (exit1 != 0 || !sstFile.isFile()) {
                return null;
            }
            final Set<String> localRootFingerprints = getLocalWindowsRootFingerprints();
            final List<X509Certificate> certs = WindowsCertUtils.getRootStoreCertificatesFromSstFile(sstFile);
            if (certs == null || certs.isEmpty() || localRootFingerprints == null || localRootFingerprints.isEmpty()) {
                return null;
            }
            final List<X509Certificate> out = new ArrayList<X509Certificate>();
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
                    if (fp != null && localRootFingerprints.contains(fp.toLowerCase(Locale.ROOT))) {
                        out.add(c);
                    }
                } catch (final CertificateEncodingException ignored) {
                }
            }
            return out.isEmpty() ? null : out.toArray(new X509Certificate[out.size()]);
        } catch (final NoClassDefFoundError e) {
            return null;
        } catch (final Throwable t) {
            return null;
        }
    }

    /**
     * Enables validation against the Microsoft Root Store (Windows Update). Load order: via certutil (SST from WU, read via JNA), then from
     * the local Windows Root store (Windows API). No PFX; SST is always loaded via certutil and read via JNA; validation matches
     * WindowsTrustProvider / JNAWindowsTrustProvider. Stops at the first successful source and adds one check named "Microsoft (Windows
     * Update)". Call this to enable the Microsoft WU store; do not call it to leave it disabled.
     *
     * @param optionalPfxFile
     *            Ignored (no PFX path); reserved for API compatibility, may be null
     * @return true if the store was loaded and added, false otherwise
     */
    public boolean enableMicrosoftWindowsUpdate(final File optionalPfxFile) {
        if (addMicrosoftRootStoreViaCertutil()) {
            return true;
        }
        if (addMicrosoftRootStoreFromWindowsAPI()) {
            return true;
        }
        return false;
    }

    /**
     * Same as {@link #enableMicrosoftWindowsUpdate(File)} with null (certutil SST then Windows API).
     */
    public boolean enableMicrosoftWindowsUpdate() {
        return enableMicrosoftWindowsUpdate(null);
    }

    /**
     * Runs {@code certutil -syncWithWU [dir]} to sync the local Windows certificate stores (Root, Disallowed, etc.) from Windows Update and
     * optionally write files (e.g. disallowedcert.sst, authrootstl.cab) to the given directory. Call before reading the Disallowed store so
     * the bad-cert list is up to date. Windows only; no-op on other platforms.
     *
     * @param workDir
     *            directory for certutil output (disallowedcert.sst, .crt files, etc.); if non-null, certutil writes files here and we can
     *            read disallowedcert.sst from it
     * @return the directory that was used for sync (workDir if non-null, else null); null on non-Windows or when sync failed
     */
    private static File runCertutilSyncWithWU(final File workDir) {
        final String os = System.getProperty("os.name", "");
        if (os == null || !os.toLowerCase(Locale.ROOT).contains("win")) {
            return null;
        }
        File dir = workDir;
        if (dir == null) {
            final String tmp = System.getProperty("java.io.tmpdir", "");
            dir = new File(tmp, "ms-wu-sync-" + Long.toString(System.currentTimeMillis()));
            if (!dir.mkdirs()) {
                return null;
            }
        } else if (!dir.isDirectory()) {
            if (!dir.mkdirs()) {
                return null;
            }
        }
        final String path = dir.getAbsolutePath();
        final int exit = runProcess(new String[] { "certutil", "-syncWithWU", path }, dir);
        return exit == 0 ? dir : null;
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
     * Loads SHA-256 fingerprints from the Apple opensource security_certificates tarball (latest tag from GitHub). Extracts
     * certificates/roots/* as DER and parses each as X.509. Fallback: if tarball fails, returns empty set.
     */
    public static Set<String> loadAppleRootFingerprints() {
        synchronized (APPLE_LOCK) {
            if (appleRootFingerprintsCache != null) {
                return appleRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String tagsJson = readUrlToString(APPLE_TARBALL_TAGS_API);
            if (tagsJson == null) {
                return fingerprints;
            }
            // JSON: [ {"name":"security_certificates-55349.60.3", ...}, ... ]; first element = latest
            final Pattern tagPattern = Pattern.compile("\"name\"\\s*:\\s*\"(security_certificates-[^\"]+)\"");
            final Matcher tagMatcher = tagPattern.matcher(tagsJson);
            if (!tagMatcher.find()) {
                return fingerprints;
            }
            final String tagName = tagMatcher.group(1);
            final String tarballUrl = APPLE_TARBALL_ARCHIVE_BASE + tagName + ".tar.gz";
            final byte[] tarGz = readUrlToBytes(tarballUrl);
            if (tarGz == null || tarGz.length == 0) {
                return fingerprints;
            }
            final Set<String> fromTar = parseAppleTarballForRootFingerprints(new ByteArrayInputStream(tarGz));
            fingerprints.addAll(fromTar);
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (APPLE_LOCK) {
            appleRootFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Parses a gzip-compressed tar stream (Apple security_certificates archive). Reads entries under certificates/roots/ (DER format),
     * parses each as X.509 and returns SHA-256 fingerprints. Tar format: 512-byte header (filename 0-99, size octal at 124-135), then file
     * content padded to 512-byte blocks.
     */
    private static Set<String> parseAppleTarballForRootFingerprints(final InputStream tarGzStream) {
        final Set<String> fingerprints = new TreeSet<String>();
        GZIPInputStream gzip = null;
        try {
            gzip = new GZIPInputStream(tarGzStream);
            final byte[] header = new byte[512];
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            while (true) {
                int off = 0;
                while (off < header.length) {
                    final int r = gzip.read(header, off, header.length - off);
                    if (r <= 0) {
                        return fingerprints;
                    }
                    off += r;
                }
                final String name = new String(header, 0, 100, "ISO-8859-1").trim();
                if (name.length() == 0) {
                    break;
                }
                long size = 0;
                try {
                    String sizeStr = new String(header, 124, 12, "ISO-8859-1").trim();
                    sizeStr = sizeStr.replaceAll("[^0-7]", "");
                    if (sizeStr.length() > 0) {
                        size = Long.parseLong(sizeStr, 8);
                    }
                } catch (final NumberFormatException e) {
                    size = 0;
                }
                if (size > 0 && size <= 65536 && name.indexOf("certificates/roots/") >= 0 && !name.endsWith("/") && !name.substring(name.lastIndexOf('/') + 1).startsWith(".")) {
                    final byte[] der = new byte[(int) size];
                    int read = 0;
                    while (read < der.length) {
                        final int r = gzip.read(der, read, der.length - read);
                        if (r <= 0) {
                            break;
                        }
                        read += r;
                    }
                    if (read == der.length) {
                        try {
                            final Certificate cert = cf.generateCertificate(new ByteArrayInputStream(der));
                            if (cert instanceof X509Certificate) {
                                final String fp = Hash.getSHA256(cert.getEncoded());
                                if (fp != null) {
                                    fingerprints.add(fp.toLowerCase(Locale.ROOT));
                                }
                            }
                        } catch (final Exception ignored) {
                        }
                    }
                }
                if (size > 0) {
                    final long pad = (512 - (size % 512)) % 512;
                    for (long i = 0; i < pad; i++) {
                        if (gzip.read() < 0) {
                            break;
                        }
                    }
                }
            }
        } catch (final Exception e) {
            // return what we have
        } finally {
            if (gzip != null) {
                try {
                    gzip.close();
                } catch (final Exception ignored) {
                }
            }
        }
        return fingerprints;
    }

    /**
     * Loads SHA-256 fingerprints from the Apple "CA certificates with additional constraints" page
     * (https://support.apple.com/en-us/103255). Only <b>Blocked Certificates</b> and <b>TLS constrained</b> are included.
     * S/MIME constrained, Timestamp constrained, and Code Signing constrained are explicitly excluded. Used for the rejected list
     * so that roots Apple blocks or restricts for TLS are not trusted for server auth.
     */
    public static Set<String> loadAppleRestrictedFingerprints() {
        synchronized (APPLE_RESTRICTED_LOCK) {
            if (appleRestrictedFingerprintsCache != null) {
                return appleRestrictedFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String html = readUrlToString(APPLE_RESTRICTED_URL);
            if (html == null) {
                return fingerprints;
            }
            final String htmlLower = html.toLowerCase(Locale.ROOT);
            // Only Blocked Certificates and TLS constrained; exclude S/MIME, Timestamp, Code Signing constrained.
            final int blockedIdx = htmlLower.indexOf("blocked certificates");
            final int constrainedIdx = htmlLower.indexOf("constrained certificates");
            final int tlsIdx = htmlLower.indexOf("tls constrained");
            final int smimeIdx = htmlLower.indexOf("s/mime constrained");
            final int timestampIdx = htmlLower.indexOf("timestamp constrained");
            final int codeSigningIdx = htmlLower.indexOf("code signing constrained");
            StringBuilder section = new StringBuilder();
            if (blockedIdx >= 0) {
                final int blockEnd = (tlsIdx >= 0 && tlsIdx > blockedIdx) ? tlsIdx : (constrainedIdx >= 0 && constrainedIdx > blockedIdx ? constrainedIdx : html.length());
                section.append(html.substring(blockedIdx, blockEnd));
            }
            if (tlsIdx >= 0) {
                final int tlsEnd = (smimeIdx > tlsIdx) ? smimeIdx : (timestampIdx > tlsIdx ? timestampIdx : (codeSigningIdx > tlsIdx ? codeSigningIdx : html.length()));
                section.append(html.substring(tlsIdx, tlsEnd));
            }
            final Pattern p = Pattern.compile("[0-9a-fA-F]{64}");
            final Matcher m = p.matcher(section.toString());
            while (m.find()) {
                fingerprints.add(m.group().toLowerCase(Locale.ROOT));
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (APPLE_RESTRICTED_LOCK) {
            appleRestrictedFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Loads SHA-256 fingerprints from the Chromium certificate blocklist README (from {@value #CHROMIUM_SSL_DATA_BASE_URL}). Contains
     * compromised and misissued certificates (and some keys) blocked in Chrome/Chromium. Only .pem entries (certificate hashes) are
     * included; .key entries are skipped.
     */
    public static Set<String> loadChromeBlocklistFingerprints() {
        synchronized (CHROMIUM_BLOCKLIST_LOCK) {
            if (chromiumBlocklistFingerprintsCache != null) {
                return chromiumBlocklistFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String text = readUrlToString(CHROMIUM_BLOCKLIST_README_URL);
            if (text == null) {
                return fingerprints;
            }
            // README has links like [hash.pem](...) or (hash.pem). Only include .pem (cert hashes), not .key
            final Pattern p = Pattern.compile("([0-9a-fA-F]{64})\\.pem");
            final Matcher m = p.matcher(text);
            while (m.find()) {
                fingerprints.add(m.group(1).toLowerCase(Locale.ROOT));
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (CHROMIUM_BLOCKLIST_LOCK) {
            chromiumBlocklistFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Loads SHA-256 fingerprints of certificates that are revoked (or otherwise rejected) on crt.sh. Uses
     * {@link CCADBCertificateVerificationTest#loadCrtshRevokedOrRejectedFingerprints(X509Certificate[], int)} with cache and a thread pool
     * (parallel lookups; pool size via system property {@code ccadb.crtsh.poolSize}, default 6). Used to extend the rejected list. Returns
     * null if the test class is unavailable or on error.
     *
     * @param candidates
     *            candidate certificates to check against crt.sh
     * @return set of revoked/rejected fingerprints (lowercase), or null
     */
    static Set<String> loadCrtshRevokedFingerprints(final X509Certificate[] candidates) {
        if (candidates == null || candidates.length == 0) {
            return null;
        }
        try {
            final Set<String> set = CCADBCertificateVerificationTest.loadCrtshRevokedOrRejectedFingerprints(candidates, 200);
            return (set != null && !set.isEmpty()) ? set : null;
        } catch (final NoClassDefFoundError e) {
            return null;
        } catch (final Throwable t) {
            LogV3.warning("crt.sh revoked check failed: " + (t.getMessage() != null ? t.getMessage() : t.getClass().getName()));
            return null;
        }
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
     * Enables validation against the Certificate Transparency log Google Pilot. Roots present in this log are treated as "CT (Google
     * Pilot)".
     */
    public void enableGooglePilotCT() {
        final Set<String> set = loadGooglePilotRootFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("CT (Google Pilot)", set));
        }
    }

    /**
     * Parses RFC 6962 get-roots JSON from a URL ("certificates":["base64",...]) and returns SHA-256 fingerprints. Used for Sycamore and
     * Google CT (see rootfetch ct.py). Returns empty set on error.
     */
    private static Set<String> loadGetRootsFingerprintsFromUrl(final String url) {
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final String json = readUrlToString(url);
            if (json == null) {
                return fingerprints;
            }
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
        return fingerprints;
    }

    /**
     * Loads root fingerprints from the Sycamore CT log (Let's Encrypt) get-roots API only.
     */
    public static Set<String> loadSycamoreRootFingerprints() {
        synchronized (SYCAMORE_LOCK) {
            if (sycamoreRootFingerprintsCache != null) {
                return sycamoreRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = loadGetRootsFingerprintsFromUrl(SYCAMORE_GET_ROOTS_URL);
        synchronized (SYCAMORE_LOCK) {
            sycamoreRootFingerprintsCache = fingerprints;
        }
        return fingerprints;
    }

    /**
     * Loads root fingerprints from the Google Pilot CT log get-roots API only.
     */
    public static Set<String> loadGooglePilotRootFingerprints() {
        synchronized (GOOGLE_PILOT_LOCK) {
            if (googlePilotRootFingerprintsCache != null) {
                return googlePilotRootFingerprintsCache;
            }
        }
        final Set<String> fingerprints = loadGetRootsFingerprintsFromUrl(GOOGLE_CT_PILOT_GET_ROOTS_URL);
        synchronized (GOOGLE_PILOT_LOCK) {
            googlePilotRootFingerprintsCache = fingerprints;
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
     * Parses RFC 6962 get-roots JSON from a URL and returns X509Certificate list. Used for candidate union from CT logs.
     */
    private static List<X509Certificate> loadGetRootsCertificatesFromUrl(final String url) {
        final List<X509Certificate> list = new ArrayList<X509Certificate>();
        try {
            final String json = readUrlToString(url);
            if (json == null) {
                return list;
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
        return list;
    }

    /**
     * Loads root certificates from CT get-roots APIs: Sycamore and Google Pilot (union, for candidate union).
     */
    public static X509Certificate[] loadSycamoreRootCertificates() {
        final X509Certificate[] sycamore = loadGetRootsCertificatesFromUrl(SYCAMORE_GET_ROOTS_URL).toArray(new X509Certificate[0]);
        final X509Certificate[] pilot = loadGetRootsCertificatesFromUrl(GOOGLE_CT_PILOT_GET_ROOTS_URL).toArray(new X509Certificate[0]);
        return unionByFingerprint(sycamore, pilot);
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
     * Enables validation against the Microsoft AuthRoot list from authroot.stl (CTL) + per-cert .crt from Windows Update. Cross-platform
     * (no certutil). Complements certutil/SST and CCADB. See rootfetch microsoft.py.
     */
    public void enableMicrosoftAuthRootStl() {
        final Set<String> set = loadMicrosoftAuthRootStlFingerprints();
        if (set != null && !set.isEmpty()) {
            checks.add(new ProviderCheck("Microsoft (AuthRoot STL)", set));
        }
    }

    /**
     * Extracts 20-byte cert IDs from authroot.stl (BER: OCTET STRING tag 0x04, length 0x14). Cert IDs are SHA-1 hashes used as .crt
     * filenames. Returns set of 40-char lowercase hex strings.
     */
    private static Set<String> extractCertIdsFromAuthrootStl(final byte[] stl) {
        final Set<String> ids = new TreeSet<String>();
        if (stl == null || stl.length < 22) {
            return ids;
        }
        for (int i = 0; i <= stl.length - 22; i++) {
            if (stl[i] == 0x04 && stl[i + 1] == 0x14) {
                final StringBuilder hex = new StringBuilder(40);
                for (int j = 0; j < 20; j++) {
                    final int b = stl[i + 2 + j] & 0xff;
                    hex.append(Integer.toHexString(b >> 4)).append(Integer.toHexString(b & 0x0f));
                }
                ids.add(hex.toString().toLowerCase(Locale.ROOT));
            }
        }
        return ids;
    }

    /**
     * Loads SHA-256 fingerprints from Microsoft AuthRoot: download authroot.stl, parse CTL for cert IDs (SHA-1), fetch each .crt from
     * Windows Update, parse as X.509. Cross-platform. Returns empty set on error.
     */
    public static Set<String> loadMicrosoftAuthRootStlFingerprints() {
        synchronized (MICROSOFT_AUTHROOT_STL_LOCK) {
            if (microsoftAuthRootStlFingerprintsCache != null) {
                return microsoftAuthRootStlFingerprintsCache;
            }
        }
        final Set<String> fingerprints = new TreeSet<String>();
        try {
            final byte[] stl = readUrlToBytes(MICROSOFT_AUTHROOT_STL_URL);
            if (stl == null || stl.length == 0) {
                return fingerprints;
            }
            final Set<String> certIds = extractCertIdsFromAuthrootStl(stl);
            final CertificateFactory cf = CertificateFactory.getInstance("X.509");
            final int maxCerts = 600;
            int tried = 0;
            for (final String idHex : certIds) {
                if (tried >= maxCerts) {
                    break;
                }
                if (idHex.length() != 40) {
                    continue;
                }
                tried++;
                try {
                    final String crtUrl = MICROSOFT_AUTHROOT_CRT_BASE + idHex + ".crt";
                    final byte[] der = readUrlToBytes(crtUrl);
                    if (der == null || der.length == 0) {
                        continue;
                    }
                    final Certificate cert = cf.generateCertificate(new ByteArrayInputStream(der));
                    if (cert instanceof X509Certificate) {
                        final String fp = Hash.getSHA256(cert.getEncoded());
                        if (fp != null) {
                            fingerprints.add(fp.toLowerCase(Locale.ROOT));
                        }
                    }
                } catch (final Exception ignored) {
                }
            }
        } catch (final Exception e) {
            // return empty set on error
        }
        synchronized (MICROSOFT_AUTHROOT_STL_LOCK) {
            microsoftAuthRootStlFingerprintsCache = fingerprints;
        }
        return fingerprints;
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
     * Loads SHA-256 fingerprints of roots that are explicitly <b>Disabled</b> in the CCADB Microsoft report (Microsoft Status =
     * "Disabled"). Used for the rejected/disabled output (appwork-merged-cadb-rejected.*.pem). No EKU or Valid To filter so all disabled
     * roots are included.
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
     * Loads SHA-256 fingerprints of roots explicitly distrusted for server auth (CKA_TRUST_SERVER_AUTH = CKT_NSS_NOT_TRUSTED) from Mozilla
     * certdata.txt. Used for appwork-merged-cadb-rejected.*.pem.
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
        try {
            return new HttpClient().get(urlString).getResponseString();
        } catch (final Exception e) {
            return null;
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
     * Validates a single certificate (fingerprint in store / single-cert provider check). Use {@link #validateChain(X509Certificate[])} to
     * check whether the full chain is trusted by each store (TLS-style).
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

    /**
     * Validates the full certificate chain (as presented by the server, optionally with resolved root). For each store: reports whether
     * that store <i>trusts</i> the chain (TLS-style: chain is valid for that store) or, for fingerprint-only stores, whether the chain's
     * root CA is in the store. Rejected lists are not evaluated here; they indicate explicit block/revocation of the root.
     *
     * @param fullChain
     *            full chain leaf-first, root last (e.g. [leaf, intermediate, root])
     * @return result for the root cert: which stores trust this chain
     */
    public CertTrustResult validateChain(final X509Certificate[] fullChain) {
        if (fullChain == null || fullChain.length == 0) {
            return new CertTrustResult("", "", Collections.<String> emptyList());
        }
        final X509Certificate root = fullChain[fullChain.length - 1];
        String fingerprint = null;
        try {
            fingerprint = Hash.getSHA256(root.getEncoded());
        } catch (final CertificateEncodingException e) {
            // use empty fingerprint
        }
        final String fpLower = fingerprint != null ? fingerprint.toLowerCase(Locale.ROOT) : "";
        final String subject = root.getSubjectX500Principal() != null ? root.getSubjectX500Principal().getName() : "";
        final List<String> acceptedBy = new ArrayList<String>();
        for (final ProviderCheck check : checks) {
            if (check.fingerprintSet != null) {
                if (check.fingerprintSet.contains(fpLower)) {
                    acceptedBy.add(check.name);
                }
                continue;
            }
            try {
                final TrustResult result = check.provider.checkServerTrusted(fullChain, AUTH_TYPE, null);
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
     * Returns a new list containing only certificates whose SHA-256 fingerprint is not in {@code rejectedFingerprints}. Used to build PEM
     * files that contain only CAs not in any provider's rejected/disabled list.
     *
     * @param certs
     *            list of certificates (not modified)
     * @param rejectedFingerprints
     *            set of SHA-256 fingerprints (lowercase) to exclude
     * @return new list of certs not in rejected set; empty if certs is null or rejected set is null/empty and all are to be kept
     */
    public static List<X509Certificate> filterOutRejectedFingerprints(final List<X509Certificate> certs, final Set<String> rejectedFingerprints) {
        if (certs == null || certs.isEmpty()) {
            return Collections.emptyList();
        }
        if (rejectedFingerprints == null || rejectedFingerprints.isEmpty()) {
            return new ArrayList<X509Certificate>(certs);
        }
        final List<X509Certificate> out = new ArrayList<X509Certificate>(certs.size());
        for (int i = 0; i < certs.size(); i++) {
            try {
                final String fp = Hash.getSHA256(certs.get(i).getEncoded());
                if (fp != null && !rejectedFingerprints.contains(fp.toLowerCase(Locale.ROOT))) {
                    out.add(certs.get(i));
                }
            } catch (final CertificateEncodingException ignored) {
            }
        }
        return out;
    }

    /**
     * Returns certificates from {@code rejectedCerts} that appear in at least {@code minProviders} of the disabled fingerprint sets (used
     * for appwork-merged-cadb-rejected.k.pem).
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
     * Writes the certificates in PEM format (Base64, 64 characters per line). Certificates are written in sorted order by SHA-256
     * fingerprint (lowercase hex).
     */
    public static void writePEM(final List<X509Certificate> certs, final OutputStream out) throws IOException, CertificateEncodingException {
        if (certs == null) {
            return;
        }
        final List<X509Certificate> sorted = new ArrayList<X509Certificate>(certs);
        Collections.sort(sorted, new Comparator<X509Certificate>() {
            @Override
            public int compare(final X509Certificate a, final X509Certificate b) {
                String fa = "";
                String fb = "";
                try {
                    final String s = Hash.getSHA256(a.getEncoded());
                    fa = s != null ? s.toLowerCase(Locale.ROOT) : "";
                } catch (final CertificateEncodingException ignored) {
                }
                try {
                    final String s = Hash.getSHA256(b.getEncoded());
                    fb = s != null ? s.toLowerCase(Locale.ROOT) : "";
                } catch (final CertificateEncodingException ignored) {
                }
                return fa.compareTo(fb);
            }
        });
        for (final X509Certificate cert : sorted) {
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
     * Builds a map from SHA-256 fingerprint (64 hex lower case) to the set of {@link RejectionReason}s (which disabled provider list(s) it
     * is in). Used to write appwork-merged-cadb-rejected.*.json with format {@code {"fingerprint":["REASON1","REASON2"], ...}}. Uses the
     * full set of rejected fingerprints so that e.g. crt.sh-revoked entries appear even when the cert is not in candidates.
     */
    private static Map<String, Set<RejectionReason>> buildRejectedFingerprintToReasonsFromFingerprints(final Set<String> rejectedFps, final List<Set<String>> disabledFingerprintSets, final List<RejectionReason> disabledProviderReasons) {
        final Map<String, Set<RejectionReason>> fpToReasons = new LinkedHashMap<String, Set<RejectionReason>>();
        if (rejectedFps == null || disabledFingerprintSets == null || disabledProviderReasons == null) {
            return fpToReasons;
        }
        for (final String fpLower : rejectedFps) {
            if (fpLower == null || fpLower.length() != 64) {
                continue;
            }
            final Set<RejectionReason> reasons = new TreeSet<RejectionReason>();
            for (int i = 0; i < disabledFingerprintSets.size() && i < disabledProviderReasons.size(); i++) {
                final Set<String> set = disabledFingerprintSets.get(i);
                if (set != null && set.contains(fpLower)) {
                    reasons.add(disabledProviderReasons.get(i));
                }
            }
            if (!reasons.isEmpty()) {
                fpToReasons.put(fpLower, reasons);
            }
        }
        return fpToReasons;
    }

    /** Well-known EKU OIDs for console display (RFC 5280 / common practice). */
    private static final String EKU_OID_ANY              = "2.5.29.37.0";
    private static final String EKU_OID_SERVER_AUTH      = "1.3.6.1.5.5.7.3.1";
    private static final String EKU_OID_CLIENT_AUTH      = "1.3.6.1.5.5.7.3.2";
    private static final String EKU_OID_CODE_SIGNING     = "1.3.6.1.5.5.7.3.3";
    private static final String EKU_OID_EMAIL_PROTECTION = "1.3.6.1.5.5.7.3.4";
    private static final String EKU_OID_TIMESTAMPING     = "1.3.6.1.5.5.7.3.8";

    /**
     * Formats extended key usage OIDs for console output (well-known OIDs as short names, others as OID string).
     *
     * @param eku
     *            list of EKU OIDs from {@link X509Certificate#getExtendedKeyUsage()}, may be null
     * @return comma-separated string, or "none" if null/empty
     */
    private static String formatEkuForConsole(final List<String> eku) {
        if (eku == null || eku.isEmpty()) {
            return "none";
        }
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < eku.size(); i++) {
            final String oid = eku.get(i);
            if (oid == null) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append(", ");
            }
            if (EKU_OID_ANY.equals(oid)) {
                sb.append("anyExtendedKeyUsage");
            } else if (EKU_OID_SERVER_AUTH.equals(oid)) {
                sb.append("serverAuth");
            } else if (EKU_OID_CLIENT_AUTH.equals(oid)) {
                sb.append("clientAuth");
            } else if (EKU_OID_CODE_SIGNING.equals(oid)) {
                sb.append("codeSigning");
            } else if (EKU_OID_EMAIL_PROTECTION.equals(oid)) {
                sb.append("emailProtection");
            } else if (EKU_OID_TIMESTAMPING.equals(oid)) {
                sb.append("timeStamping");
            } else {
                sb.append(oid);
            }
        }
        return sb.length() > 0 ? sb.toString() : "none";
    }

    /** Escapes a string for use as JSON value (backslash and quote). */
    private static String escapeJsonString(final String s) {
        if (s == null) {
            return "";
        }
        final StringBuilder sb = new StringBuilder(s.length() + 8);
        for (int i = 0; i < s.length(); i++) {
            final char c = s.charAt(i);
            if (c == '\\') {
                sb.append("\\\\");
            } else if (c == '"') {
                sb.append("\\\"");
            } else if (c == '\n') {
                sb.append("\\n");
            } else if (c == '\r') {
                sb.append("\\r");
            } else if (c == '\t') {
                sb.append("\\t");
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * Returns a map containing only entries whose fingerprint appears in at least {@code minProviders} of the disabled fingerprint sets
     * (for writing rejected.k.json).
     */
    private static Map<String, Set<RejectionReason>> filterFpToReasonsByMinProviders(final Map<String, Set<RejectionReason>> fpToReasons, final List<Set<String>> disabledFingerprintSets, final int minProviders) {
        if (fpToReasons == null || disabledFingerprintSets == null || minProviders < 1) {
            return new LinkedHashMap<String, Set<RejectionReason>>();
        }
        final Map<String, Set<RejectionReason>> out = new LinkedHashMap<String, Set<RejectionReason>>();
        for (final Map.Entry<String, Set<RejectionReason>> e : fpToReasons.entrySet()) {
            final String fp = e.getKey();
            int count = 0;
            for (int i = 0; i < disabledFingerprintSets.size(); i++) {
                final Set<String> set = disabledFingerprintSets.get(i);
                if (set != null && set.contains(fp)) {
                    count++;
                }
            }
            if (count >= minProviders) {
                out.put(fp, e.getValue());
            }
        }
        return out;
    }

    /**
     * Writes a JSON object {@code {"fingerprint":["REASON1","REASON2"], ...}} from the supplied map. All entries are written; keys
     * (fingerprints) and array elements (enum names) are in sorted order so that the output JSON is deterministically ordered.
     */
    private static void writeRejectedJson(final Map<String, Set<RejectionReason>> fpToReasons, final File file) throws IOException {
        if (fpToReasons == null) {
            return;
        }
        final List<String> sortedKeys = new ArrayList<String>(fpToReasons.keySet());
        Collections.sort(sortedKeys);
        final FlexiJSonObject obj = new FlexiJSonObject();
        for (final String fp : sortedKeys) {
            final Set<RejectionReason> reasons = fpToReasons.get(fp);
            if (reasons != null && !reasons.isEmpty()) {
                final FlexiJSonArray arr = new FlexiJSonArray();
                for (final RejectionReason r : reasons) {
                    arr.add(new FlexiJSonValue(r.name()));
                }
                obj.add(new KeyValueElement(obj, fp, arr));
            }
        }
        final String json = new FlexiJSonPrettyStringify().toJSONString(obj);
        IO.writeStringToFile(file, json);
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
     * Directory for caching certificates loaded from crt.sh. Each file is named {@code <sha256-fingerprint>.pem}. Default:
     * {@code user.home/.ccadb-crtsh-cache}. Override with system property {@code ccadb.crtsh.cache.dir}.
     */
    public static File getCrtshCacheDirectory() {
        final String dir = System.getProperty("ccadb.crtsh.cache.dir");
        if (dir != null && dir.length() > 0) {
            return new File(dir);
        }
        return new File(System.getProperty("user.home", ""), ".ccadb-crtsh-cache");
    }

    /**
     * Loads a single certificate from crt.sh by SHA-256 fingerprint. Uses a file cache: if {@code <fingerprint>.pem} exists in
     * {@link #getCrtshCacheDirectory()}, the cert is loaded from there and verified by fingerprint; otherwise the cert is fetched from
     * crt.sh and stored in the cache. Returns null if not found, parse error, or fingerprint mismatch.
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
        final File cacheDir = getCrtshCacheDirectory();
        final File cacheFile = new File(cacheDir, normalized + ".pem");
        if (cacheFile.isFile()) {
            try {
                final byte[] bytes = IO.readFile(cacheFile);
                if (bytes != null && bytes.length > 0) {
                    final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(bytes));
                    if (certs != null && certs.length > 0) {
                        final X509Certificate cert = certs[0];
                        final String actualFp = Hash.getSHA256(cert.getEncoded());
                        if (actualFp != null && actualFp.toLowerCase(Locale.ROOT).equals(normalized)) {
                            return cert;
                        }
                    }
                }
            } catch (final Exception ignored) {
            }
        }
        final long deadline = System.currentTimeMillis() + CRTSH_RETRY_TIMEOUT_MS;
        int attempt = 0;
        while (System.currentTimeMillis() < deadline) {
            attempt++;
            try {
                final String html = readUrlToString(CRTSH_QUERY_URL + normalized);
                if (html == null || html.isEmpty()) {
                    logCrtshRetry("query empty/null", normalized, attempt);
                } else if (html.toLowerCase(Locale.ROOT).contains("no records found") || html.toLowerCase(Locale.ROOT).contains("0 results")) {
                    logCrtshRetry("no records", normalized, attempt);
                } else {
                    final Matcher idMatcher = Pattern.compile("\\?id=(\\d+)").matcher(html);
                    if (!idMatcher.find()) {
                        logCrtshRetry("no certificate id in response", normalized, attempt);
                    } else {
                        final String id = idMatcher.group(1);
                        final String pem = readUrlToString(CRTSH_DOWNLOAD_URL + id);
                        if (pem == null || !pem.contains("-----BEGIN CERTIFICATE-----")) {
                            logCrtshRetry("download null or no PEM (id=" + id + ")", normalized, attempt);
                        } else {
                            final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(pem.getBytes("UTF-8")));
                            if (certs == null || certs.length == 0) {
                                logCrtshRetry("failed to parse PEM", normalized, attempt);
                            } else {
                                final X509Certificate cert = certs[0];
                                final String actualFp = Hash.getSHA256(cert.getEncoded());
                                if (actualFp == null || !actualFp.toLowerCase(Locale.ROOT).equals(normalized)) {
                                    logCrtshRetry("fingerprint mismatch", normalized, attempt);
                                } else {
                                    if (!cacheDir.exists()) {
                                        cacheDir.mkdirs();
                                    }
                                    if (cacheDir.isDirectory()) {
                                        IO.writeToFile(cacheFile, pem.getBytes("UTF-8"));
                                    }
                                    return cert;
                                }
                            }
                        }
                    }
                }
            } catch (final Exception e) {
                logCrtshRetry(e.getMessage(), normalized, attempt);
                try {
                    LogV3.log(e);
                } catch (final Exception ignored) {
                }
            }
            final long remaining = deadline - System.currentTimeMillis();
            if (remaining <= 0) {
                break;
            }
            final long sleepMs = remaining < CRTSH_RETRY_DELAY_MS ? remaining : CRTSH_RETRY_DELAY_MS;
            try {
                Thread.sleep(sleepMs);
            } catch (final InterruptedException ie) {
                Thread.currentThread().interrupt();
                logCrtshRetry("interrupted", normalized, attempt);
                return null;
            }
        }
        logCrtshRetry("gave up after " + (CRTSH_RETRY_TIMEOUT_MS / 1000) + "s (" + attempt + " attempts)", normalized, attempt);
        return null;
    }

    private static void logCrtshRetry(final String reason, final String fingerprint, final int attempt) {
        final String msg = "crt.sh " + reason + " for " + fingerprint + ", attempt " + attempt;
        System.err.println(msg);
        try {
            LogV3.warning(msg);
        } catch (final Exception ignored) {
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

    /**
     * Fetches the server certificate chain from the given HTTPS URL (using {@link HttpClient} with {@link AllTrustProvider} so the chain is
     * returned even when the certificate would be distrusted), then validates the root CA against all configured stores and prints detailed
     * per-store status: which store trusts the root, which rejects it (with reason), and which does not contain the root.
     * <p>
     * Example: {@code RootCertificateTrustValidator.printStoreDetailed("https://appwork.org")}
     *
     * @param urlString
     *            HTTPS URL (e.g. "https://appwork.org"); if no scheme is given, "https://" is prepended
     */
    public static void printStoreDetailed(final String urlString) {
        String requestUrl = urlString != null ? urlString.trim() : "";
        if (requestUrl.isEmpty()) {
            System.err.println("printStoreDetailed: URL is empty.");
            return;
        }
        if (!requestUrl.contains("://")) {
            requestUrl = "https://" + requestUrl;
        }
        if (!requestUrl.toLowerCase(Locale.ROOT).startsWith("https://")) {
            System.err.println("printStoreDetailed: URL must be HTTPS (got: " + requestUrl + ").");
            return;
        }
        X509Certificate[] chain = null;
        try {
            final HttpClient client = new HttpClient();
            client.setTrustProvider(AllTrustProvider.getInstance());
            final HttpClient.RequestContext req = HttpClient.RequestContext.get(requestUrl);
            client.execute(req);
            final TrustResult tr = req.getTrustResult();
            chain = tr != null ? tr.getChain() : null;
        } catch (final Throwable e) {
            System.err.println("printStoreDetailed: Failed to connect to " + requestUrl + ": " + (e.getMessage() != null ? e.getMessage() : e.getClass().getName()));
            return;
        }
        if (chain == null || chain.length == 0) {
            System.err.println("printStoreDetailed: No certificate chain received from " + requestUrl + ".");
            return;
        }
        final X509Certificate lastInChain = chain[chain.length - 1];
        final X509Certificate rootCert = resolveRootFromChain(chain);
        final boolean chainWasIncomplete = (rootCert != lastInChain);
        if (chainWasIncomplete && rootCert != null) {
            System.out.println("(Server sent incomplete chain; root was resolved from JRE trust store.)");
        }
        String rootFp = null;
        try {
            rootFp = Hash.getSHA256(rootCert.getEncoded());
        } catch (final CertificateEncodingException e) {
            // ignore
        }
        final String rootFpLower = rootFp != null ? rootFp.toLowerCase(Locale.ROOT) : "";
        final String rootSubject = rootCert.getSubjectX500Principal() != null ? rootCert.getSubjectX500Principal().getName() : "";
        final RootCertificateTrustValidator validator = new RootCertificateTrustValidator();
        validator.enableAppleRootStore();
        validator.enableGoogleChromeRootStore();
        validator.enableAndroidRootStore();
        validator.enableMicrosoftCCADB();
        validator.enableMozillaIncludedRootsPem();
        validator.enableMozillaCertdata();
        validator.enableMicrosoftWindowsUpdate();
        final X509Certificate[] fullChain = (rootCert != lastInChain) ? buildFullChainWithRoot(chain, rootCert) : chain;
        final CertTrustResult trustResult = validator.validateChain(fullChain);
        final List<String> acceptedBy = trustResult.getAcceptedByProviders();
        final Map<String, Integer> storeSizes = validator.getStoreSizes();
        final List<String> disabledProviderNames = new ArrayList<String>();
        final List<RejectionReason> disabledProviderReasons = new ArrayList<RejectionReason>();
        final List<Set<String>> disabledFingerprintSets = new ArrayList<Set<String>>();
        Set<String> microsoftDisabled = loadMicrosoftCCADBDisabledFingerprints();
        if (microsoftDisabled != null && !microsoftDisabled.isEmpty()) {
            disabledProviderNames.add("Microsoft: disabled (CCADB)");
            disabledProviderReasons.add(RejectionReason.MICROSOFT_DISABLED_CCADB);
            disabledFingerprintSets.add(microsoftDisabled);
        }
        final Set<String> mozillaDistrustedFps = loadMozillaCertdataDistrustedFingerprints();
        if (mozillaDistrustedFps != null && !mozillaDistrustedFps.isEmpty()) {
            disabledProviderNames.add("Mozilla: distrusted (certdata)");
            disabledProviderReasons.add(RejectionReason.MOZILLA_DISTRUSTED);
            disabledFingerprintSets.add(mozillaDistrustedFps);
        }
        final Set<String> appleRestrictedFps = loadAppleRestrictedFingerprints();
        if (appleRestrictedFps != null && !appleRestrictedFps.isEmpty()) {
            disabledProviderNames.add("Apple: blocked/constrained");
            disabledProviderReasons.add(RejectionReason.APPLE_BLOCKED_CONSTRAINED);
            disabledFingerprintSets.add(appleRestrictedFps);
        }
        final Set<String> chromeBlocklistFps = loadChromeBlocklistFingerprints();
        if (chromeBlocklistFps != null && !chromeBlocklistFps.isEmpty()) {
            disabledProviderNames.add("Chrome: blocklist");
            disabledProviderReasons.add(RejectionReason.CHROME_BLOCKLIST);
            disabledFingerprintSets.add(chromeBlocklistFps);
        }
        final Set<String> crtshRevoked = loadCrtshRevokedFingerprints(new X509Certificate[] { rootCert });
        if (crtshRevoked != null && !crtshRevoked.isEmpty()) {
            disabledProviderNames.add("crt.sh: revoked");
            disabledProviderReasons.add(RejectionReason.CRTSH_REVOKED);
            disabledFingerprintSets.add(crtshRevoked);
        }
        Set<String> microsoftDisallowedFps = WindowsCertUtils.getDisallowedStoreFingerprintsSha256();
        if (microsoftDisallowedFps != null && !microsoftDisallowedFps.isEmpty()) {
            disabledProviderNames.add("Microsoft: Disallowed store");
            disabledProviderReasons.add(RejectionReason.MICROSOFT_DISALLOWED_STORE);
            disabledFingerprintSets.add(microsoftDisallowedFps);
        }
        System.out.println("--- Store detail for URL: " + requestUrl + " ---");
        System.out.println("Root CA subject: " + (rootSubject.length() > 80 ? rootSubject.substring(0, 77) + "..." : rootSubject));
        System.out.println("Root CA SHA-256: " + (rootFpLower.length() == 64 ? rootFpLower : "(unknown)"));
        System.out.println();
        System.out.println("Per-store: does this store TRUST the chain? (chain validated TLS-style); count = CAs in that store:");
        for (final Map.Entry<String, Integer> e : storeSizes.entrySet()) {
            final String storeName = e.getKey();
            final int count = e.getValue() != null ? e.getValue().intValue() : -1;
            final String countStr = count >= 0 ? count + " CAs" : "n/a";
            String status;
            if (acceptedBy.contains(storeName)) {
                status = "TRUSTED";
            } else {
                RejectionReason rejectionReason = null;
                for (int i = 0; i < disabledFingerprintSets.size() && i < disabledProviderReasons.size(); i++) {
                    if (disabledFingerprintSets.get(i) != null && disabledFingerprintSets.get(i).contains(rootFpLower)) {
                        rejectionReason = disabledProviderReasons.get(i);
                        break;
                    }
                }
                if (rejectionReason != null) {
                    status = "REJECTED (" + rejectionReason.name() + ")";
                } else {
                    status = "NOT_IN_STORE";
                }
            }
            System.out.println("  " + storeName + ": " + status + " (" + countStr + " in store)");
        }
        System.out.println("Rejected/block lists: is the chain's root explicitly listed (JA)? count = entries in that list:");
        for (int i = 0; i < disabledProviderNames.size(); i++) {
            final String name = disabledProviderNames.get(i);
            if (storeSizes.containsKey(name)) {
                continue;
            }
            final Set<String> set = i < disabledFingerprintSets.size() ? disabledFingerprintSets.get(i) : null;
            final int listSize = set != null ? set.size() : 0;
            final RejectionReason reason = i < disabledProviderReasons.size() ? disabledProviderReasons.get(i) : null;
            final String status = (set != null && set.contains(rootFpLower)) ? "REJECTED (" + (reason != null ? reason.name() : "?") + ")" : "NOT_IN_STORE";
            System.out.println("  " + name + ": " + status + " (" + listSize + " entries in list)");
        }
        final List<String> rejectedListNames = new ArrayList<String>();
        final List<RejectionReason> rejectedReasonsForRoot = new ArrayList<RejectionReason>();
        for (int i = 0; i < disabledProviderNames.size() && i < disabledFingerprintSets.size(); i++) {
            final Set<String> set = disabledFingerprintSets.get(i);
            if (set != null && set.contains(rootFpLower)) {
                rejectedListNames.add(disabledProviderNames.get(i));
                rejectedReasonsForRoot.add(i < disabledProviderReasons.size() ? disabledProviderReasons.get(i) : null);
            }
        }
        System.out.println();
        System.out.println("Rejected lists containing this root:");
        if (rejectedListNames.isEmpty()) {
            System.out.println("  (none)");
        } else {
            for (int r = 0; r < rejectedListNames.size(); r++) {
                final String name = rejectedListNames.get(r);
                final RejectionReason reason = r < rejectedReasonsForRoot.size() ? rejectedReasonsForRoot.get(r) : null;
                System.out.println("  " + name);
                if (rootFpLower.length() == 64) {
                    System.out.println("    Rejected CA SHA-256: " + rootFpLower + " (use for search)");
                }
                System.out.println("    -> " + getRejectionReasonDescription(reason));
            }
        }
        System.out.println();
        System.out.println("Rejection reason legend (what each list means):");
        for (final RejectionReason r : RejectionReason.values()) {
            System.out.println("  " + r.name() + ": " + getRejectionReasonDescription(r));
        }
        final int acceptedCount = acceptedBy.size();
        final boolean inRejectedList = !rejectedListNames.isEmpty();
        System.out.println();
        System.out.println("Merged PEM files (this root would appear in):");
        if (acceptedCount <= 0) {
            System.out.println("  (none – root not accepted by any store)");
        } else {
            final List<String> mergedNames = new ArrayList<String>();
            for (int k = 1; k <= acceptedCount; k++) {
                mergedNames.add(DEFAULT_MERGED_PEM_BASENAME + "." + k + ".pem");
            }
            System.out.println("  " + join(mergedNames, ", "));
            if (!inRejectedList) {
                final List<String> noRejNames = new ArrayList<String>();
                for (int k = 1; k <= acceptedCount; k++) {
                    noRejNames.add(DEFAULT_MERGED_PEM_BASENAME + "." + k + "-no-rejected.pem");
                }
                System.out.println("  Also (not in any rejected list): " + join(noRejNames, ", "));
            } else {
                System.out.println("  (root is in a rejected/blocklist → not in *-no-rejected.pem)");
            }
        }
        final List<File> foundIn = findMergedPemFilesContainingFingerprint(rootFpLower);
        if (!foundIn.isEmpty()) {
            System.out.println();
            System.out.println("Found in (on disk):");
            for (final File f : foundIn) {
                System.out.println("  " + f.getAbsolutePath());
            }
        }
        System.out.println("--- end ---");
    }

    /**
     * Resolves the effective root CA from the server chain. If the last certificate is self-signed (subject equals issuer), it is returned.
     * Otherwise the JRE default cacerts is searched for a CA whose subject equals the issuer of the last cert (handles incomplete chains
     * where the server did not send the root).
     *
     * @param chain
     *            server certificate chain (leaf first, root last; may be incomplete)
     * @return the root CA to use for store checks (never null if chain is non-empty)
     */
    private static X509Certificate resolveRootFromChain(final X509Certificate[] chain) {
        if (chain == null || chain.length == 0) {
            return null;
        }
        final X509Certificate last = chain[chain.length - 1];
        if (isSelfSigned(last)) {
            return last;
        }
        final String issuerName = last.getIssuerDN() != null ? last.getIssuerDN().getName() : null;
        if (issuerName == null || issuerName.length() == 0) {
            return last;
        }
        java.io.FileInputStream fis = null;
        try {
            final File cacertsFile = new File(System.getProperty("java.home"), "lib/security/cacerts");
            if (!cacertsFile.isFile()) {
                return last;
            }
            final KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
            final char[] password = "changeit".toCharArray();
            try {
                fis = new FileInputStream(cacertsFile);
                ks.load(fis, password);
            } catch (final Exception e1) {
                try {
                    if (fis != null) {
                        try {
                            fis.close();
                        } catch (final IOException ignored) {
                        }
                        fis = null;
                    }
                    fis = new FileInputStream(cacertsFile);
                    ks.load(fis, null);
                } catch (final Exception e2) {
                    return last;
                }
            }
            final java.util.Enumeration<String> aliases = ks.aliases();
            if (aliases == null) {
                return last;
            }
            while (aliases.hasMoreElements()) {
                final String alias = aliases.nextElement();
                if (!ks.isCertificateEntry(alias)) {
                    continue;
                }
                final Certificate c = ks.getCertificate(alias);
                if (!(c instanceof X509Certificate)) {
                    continue;
                }
                final X509Certificate ca = (X509Certificate) c;
                final String subjectName = ca.getSubjectDN() != null ? ca.getSubjectDN().getName() : null;
                if (subjectName != null && subjectName.equals(issuerName)) {
                    return ca;
                }
            }
        } catch (final Throwable ignored) {
            // fall through to return last
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (final IOException ignored) {
                }
            }
        }
        return last;
    }

    private static boolean isSelfSigned(final X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        final String sub = cert.getSubjectDN() != null ? cert.getSubjectDN().getName() : null;
        final String iss = cert.getIssuerDN() != null ? cert.getIssuerDN().getName() : null;
        return sub != null && sub.length() > 0 && sub.equals(iss);
    }

    /**
     * Builds full chain by appending the resolved root to the server chain (for incomplete chains).
     */
    private static X509Certificate[] buildFullChainWithRoot(final X509Certificate[] serverChain, final X509Certificate rootCert) {
        if (serverChain == null || rootCert == null) {
            return serverChain != null ? serverChain : new X509Certificate[0];
        }
        final X509Certificate[] full = new X509Certificate[serverChain.length + 1];
        for (int i = 0; i < serverChain.length; i++) {
            full[i] = serverChain[i];
        }
        full[serverChain.length] = rootCert;
        return full;
    }

    /**
     * Scans default merged-PEM locations and current dir for PEM files whose certificate set contains the given SHA-256 fingerprint
     * (lowercase 64 hex). Returns list of files that contain the fingerprint.
     */
    private static List<File> findMergedPemFilesContainingFingerprint(final String fingerprintSha256Lower) {
        final List<File> out = new ArrayList<File>();
        if (fingerprintSha256Lower == null || fingerprintSha256Lower.length() != 64) {
            return out;
        }
        final List<File> dirsToScan = new ArrayList<File>();
        try {
            final File defaultOut = new File(AWTest.getWorkspace(), "AppWorkutils/src/org/appwork/utils/net/httpconnection/trust/ccadb");
            if (defaultOut.isDirectory()) {
                dirsToScan.add(defaultOut);
                final File tempDir = new File(defaultOut, "temp");
                if (tempDir.isDirectory()) {
                    dirsToScan.add(tempDir);
                }
            }
        } catch (final Throwable ignored) {
            // AWTest.getWorkspace() not available
        }
        dirsToScan.add(new File("."));
        for (final File dir : dirsToScan) {
            final File[] files = dir.listFiles();
            if (files == null) {
                continue;
            }
            for (final File f : files) {
                final String name = f.getName();
                if (!f.isFile() || (!name.startsWith(DEFAULT_MERGED_PEM_BASENAME) && !name.startsWith(DEFAULT_MERGED_PEM_REJECTED_BASE))) {
                    continue;
                }
                if (!name.endsWith(".pem")) {
                    continue;
                }
                try {
                    final byte[] bytes = IO.readFile(f);
                    if (bytes == null || bytes.length == 0) {
                        continue;
                    }
                    final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(new ByteArrayInputStream(bytes));
                    if (certs == null) {
                        continue;
                    }
                    for (int i = 0; i < certs.length; i++) {
                        try {
                            final String fp = Hash.getSHA256(certs[i].getEncoded());
                            if (fp != null && fingerprintSha256Lower.equals(fp.toLowerCase(Locale.ROOT))) {
                                out.add(f);
                                break;
                            }
                        } catch (final CertificateEncodingException ignored) {
                        }
                    }
                } catch (final Throwable ignored) {
                    // skip unreadable or invalid PEM
                }
            }
        }
        return out;
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
     * Main entry: No input PEM. Loads candidates from the linked stores (Android, Sycamore, Microsoft via certutil SST + JNA, etc.),
     * computes the intersection of all stores enabled via {@code enable...}, and writes only those CAs to the output PEM. JRE, OS and CCADB
     * providers are not used.
     * <p>
     * Invocation: java ... RootCertificateTrustValidator [output.pem]
     * <ul>
     * <li>1st argument: optional output PEM path; if omitted, files are written to the same directory as {@code common-ca-database.pem}
     * (ccadb package) with base name {@value #DEFAULT_MERGED_PEM_BASENAME}</li>
     * <li>On Windows, Microsoft (Windows Update) is loaded via certutil (SST from WU), read via JNA, then local Root store</li>
     * </ul>
     */
    private static final String DEFAULT_MERGED_PEM_BASENAME      = "appwork-merged-cadb";
    private static final String DEFAULT_MERGED_PEM_REJECTED_BASE = "appwork-merged-cadb-rejected";
    /**
     * k used for appwork-merged-cadb-rejected.<k>.json consumed by CCADBTrustProvider (must match
     * CCADBTrustProvider.ONLY_CAS_ACCEPTED_BY_AT_LEAST).
     */
    private static final String REJECTED_JSON_K_FOR_CCADB        = "3";

    public static void main(final String[] args) {
        RootCertificateTrustValidator.printStoreDetailed("https://appwork.org");
        final File wuSyncDir = runCertutilSyncWithWU(null);
        if (wuSyncDir != null) {
            System.out.println("certutil -syncWithWU completed (Root/Disallowed stores synced; output in " + wuSyncDir.getAbsolutePath() + ").");
        }
        final RootCertificateTrustValidator validator = new RootCertificateTrustValidator();
        // validator.enableSycamoreCT();
        // validator.enableGooglePilotCT();
        validator.enableAppleRootStore();
        validator.enableGoogleChromeRootStore();
        validator.enableAndroidRootStore();
        validator.enableMicrosoftCCADB();
        // validator.enableMicrosoftAuthRootStl(); // cross-platform AuthRoot from authroot.stl + .crt
        validator.enableMozillaIncludedRootsPem();
        validator.enableMozillaCertdata();
        if (validator.enableMicrosoftWindowsUpdate()) {
            System.err.println("Microsoft (Windows Update) roots loaded.");
        } else {
            System.err.println("Microsoft (Windows Update): not loaded (omit enableMicrosoftWindowsUpdate() to disable).");
        }
        final Map<String, Integer> storeSizes = validator.getStoreSizes();
        System.out.println("Filter: only CAs acceptable for TLS server auth (TrustUtils.isAcceptableCaTrustAnchorForSsl). CAs for code signing, email, etc. only are excluded.");
        // Order and full list of all stores (including when not loaded, e.g. Microsoft WU)
        final String[] storeOrder = new String[] { "CT (Sycamore)", "CT (Google Pilot)", "Apple (Root Store)", "Google (Chrome)", "Android (Root Store)", "Microsoft (CCADB)", "Microsoft (AuthRoot STL)", "Mozilla (Included Roots PEM)", "Mozilla (certdata)", "Microsoft (Windows Update)" };
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
        X509Certificate[] msCerts = loadMicrosoftWindowsUpdateCertificatesFromSst();
        if (msCerts == null) {
            msCerts = new X509Certificate[0];
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
        final File tempDir = new File(outDir, "temp");
        if (!outDir.isDirectory() && !outDir.mkdirs()) {
            System.err.println("Failed to create output directory: " + outDir.getAbsolutePath());
            System.exit(1);
        }
        if (!tempDir.isDirectory() && !tempDir.mkdirs()) {
            System.err.println("Failed to create temp directory: " + tempDir.getAbsolutePath());
            System.exit(1);
        }
        final int numStores = storeNames.size();
        for (File f : outDir.listFiles()) {
            if (f.getName().startsWith(baseName)) {
                f.delete();
            }
        }
        for (File f : tempDir.listFiles()) {
            if (f.getName().startsWith(baseName) || f.getName().startsWith(DEFAULT_MERGED_PEM_REJECTED_BASE)) {
                f.delete();
            }
        }
        for (int k = 1; k <= numStores; k++) {
            final List<X509Certificate> atLeastK = getCertsAcceptedByAtLeastKProviders(candidates, results, k);
            if (atLeastK.size() == 0) {
                continue;
            }
            final File kFile = new File(tempDir, baseName + "." + k + ".pem");
            try {
                final ByteArrayOutputStream bos = new ByteArrayOutputStream();
                writePEM(atLeastK, bos);
                IO.writeToFile(kFile, bos.toByteArray());
                System.out.println("PEM (>= " + k + " providers): " + kFile.getAbsolutePath() + " (" + atLeastK.size() + " CAs)");
            } catch (final Exception e) {
                System.err.println("Failed to write " + kFile.getName() + ": " + e.getMessage());
            }
        }
        // Rejected/disabled: same logic as accepted, but for certs explicitly disabled by providers that support it (Microsoft CCADB,
        // Mozilla certdata).
        final List<String> disabledProviderNames = new ArrayList<String>();
        final List<RejectionReason> disabledProviderReasons = new ArrayList<RejectionReason>();
        final List<Set<String>> disabledFingerprintSets = new ArrayList<Set<String>>();
        final Set<String> microsoftDisabled = loadMicrosoftCCADBDisabledFingerprints();
        if (microsoftDisabled != null && !microsoftDisabled.isEmpty()) {
            disabledProviderNames.add("Microsoft: disabled (CCADB)");
            disabledProviderReasons.add(RejectionReason.MICROSOFT_DISABLED_CCADB);
            disabledFingerprintSets.add(microsoftDisabled);
        }
        final Set<String> mozillaDistrustedFps = loadMozillaCertdataDistrustedFingerprints();
        if (mozillaDistrustedFps != null && !mozillaDistrustedFps.isEmpty()) {
            disabledProviderNames.add("Mozilla: distrusted (certdata)");
            disabledProviderReasons.add(RejectionReason.MOZILLA_DISTRUSTED);
            disabledFingerprintSets.add(mozillaDistrustedFps);
        }
        final Set<String> appleRestrictedFps = loadAppleRestrictedFingerprints();
        if (appleRestrictedFps != null && !appleRestrictedFps.isEmpty()) {
            disabledProviderNames.add("Apple: blocked/constrained");
            disabledProviderReasons.add(RejectionReason.APPLE_BLOCKED_CONSTRAINED);
            disabledFingerprintSets.add(appleRestrictedFps);
        }
        final Set<String> chromeBlocklistFps = loadChromeBlocklistFingerprints();
        if (chromeBlocklistFps != null && !chromeBlocklistFps.isEmpty()) {
            disabledProviderNames.add("Chrome: blocklist");
            disabledProviderReasons.add(RejectionReason.CHROME_BLOCKLIST);
            disabledFingerprintSets.add(chromeBlocklistFps);
        }
        final Set<String> crtshRevoked = loadCrtshRevokedFingerprints(candidates);
        if (crtshRevoked != null && !crtshRevoked.isEmpty()) {
            disabledProviderNames.add("crt.sh: revoked");
            disabledProviderReasons.add(RejectionReason.CRTSH_REVOKED);
            disabledFingerprintSets.add(crtshRevoked);
        }
        Set<String> microsoftDisallowedFps = null;
        List<X509Certificate> microsoftDisallowedCertsList = null;
        if (wuSyncDir != null) {
            final File disallowedSst = new File(wuSyncDir, "disallowedcert.sst");
            if (disallowedSst.isFile()) {
                final List<X509Certificate> fromSst = WindowsCertUtils.getRootStoreCertificatesFromSstFile(disallowedSst);
                if (fromSst != null && !fromSst.isEmpty()) {
                    microsoftDisallowedFps = new TreeSet<String>();
                    for (int i = 0; i < fromSst.size(); i++) {
                        try {
                            final String fp = Hash.getSHA256(fromSst.get(i).getEncoded());
                            if (fp != null) {
                                microsoftDisallowedFps.add(fp.toLowerCase(Locale.ROOT));
                            }
                        } catch (final CertificateEncodingException ignored) {
                        }
                    }
                    microsoftDisallowedCertsList = fromSst;
                }
            }
        }
        if (microsoftDisallowedFps == null || microsoftDisallowedFps.isEmpty()) {
            microsoftDisallowedFps = WindowsCertUtils.getDisallowedStoreFingerprintsSha256();
            final List<X509Certificate> storeCerts = WindowsCertUtils.getDisallowedStoreCertificates();
            microsoftDisallowedCertsList = (storeCerts != null && !storeCerts.isEmpty()) ? storeCerts : null;
        }
        if (microsoftDisallowedFps != null && !microsoftDisallowedFps.isEmpty()) {
            disabledProviderNames.add("Microsoft: Disallowed store");
            disabledProviderReasons.add(RejectionReason.MICROSOFT_DISALLOWED_STORE);
            disabledFingerprintSets.add(microsoftDisallowedFps);
        }
        if (!disabledFingerprintSets.isEmpty()) {
            final Set<String> rejectedFps = new TreeSet<String>();
            if (microsoftDisabled != null) {
                rejectedFps.addAll(microsoftDisabled);
            }
            if (mozillaDistrustedFps != null) {
                rejectedFps.addAll(mozillaDistrustedFps);
            }
            if (appleRestrictedFps != null) {
                rejectedFps.addAll(appleRestrictedFps);
            }
            if (chromeBlocklistFps != null) {
                rejectedFps.addAll(chromeBlocklistFps);
            }
            if (crtshRevoked != null) {
                rejectedFps.addAll(crtshRevoked);
            }
            if (microsoftDisallowedFps != null) {
                rejectedFps.addAll(microsoftDisallowedFps);
            }
            // PEM files containing only CAs not in any rejected list (same k as accepted PEMs, but with rejected fingerprints removed)
            final int noRejectedPemKAtMainOut = 3;
            for (int k = 1; k <= numStores; k++) {
                final List<X509Certificate> atLeastK = getCertsAcceptedByAtLeastKProviders(candidates, results, k);
                final List<X509Certificate> notRejected = filterOutRejectedFingerprints(atLeastK, rejectedFps);
                if (notRejected.isEmpty()) {
                    continue;
                }
                final File noRejFile = (k == noRejectedPemKAtMainOut) ? new File(outDir, baseName + "." + k + "-no-rejected.pem") : new File(tempDir, baseName + "." + k + "-no-rejected.pem");
                try {
                    final ByteArrayOutputStream bos = new ByteArrayOutputStream();
                    writePEM(notRejected, bos);
                    IO.writeToFile(noRejFile, bos.toByteArray());
                    System.out.println("PEM (>= " + k + " providers, not in any rejected list): " + noRejFile.getAbsolutePath() + " (" + notRejected.size() + " CAs)");
                } catch (final Exception e) {
                    System.err.println("Failed to write " + noRejFile.getName() + ": " + e.getMessage());
                }
            }
            final List<X509Certificate> rejectedCerts = new ArrayList<X509Certificate>();
            final Set<String> addedFp = new TreeSet<String>();
            for (int i = 0; i < candidates.length; i++) {
                try {
                    final String fp = Hash.getSHA256(candidates[i].getEncoded());
                    if (fp != null && rejectedFps.contains(fp.toLowerCase(Locale.ROOT)) && addedFp.add(fp.toLowerCase(Locale.ROOT))) {
                        rejectedCerts.add(candidates[i]);
                    }
                } catch (final CertificateEncodingException ignored) {
                }
            }
            final X509Certificate[] mozillaDistrustedCerts = loadMozillaCertdataDistrustedCertificates();
            if (mozillaDistrustedCerts != null) {
                for (int i = 0; i < mozillaDistrustedCerts.length; i++) {
                    try {
                        final String fp = Hash.getSHA256(mozillaDistrustedCerts[i].getEncoded());
                        if (fp != null && addedFp.add(fp.toLowerCase(Locale.ROOT))) {
                            rejectedCerts.add(mozillaDistrustedCerts[i]);
                        }
                    } catch (final CertificateEncodingException ignored) {
                    }
                }
            }
            if (microsoftDisallowedCertsList != null) {
                for (int i = 0; i < microsoftDisallowedCertsList.size(); i++) {
                    try {
                        final String fp = Hash.getSHA256(microsoftDisallowedCertsList.get(i).getEncoded());
                        if (fp != null && addedFp.add(fp.toLowerCase(Locale.ROOT))) {
                            rejectedCerts.add(microsoftDisallowedCertsList.get(i));
                        }
                    } catch (final CertificateEncodingException ignored) {
                    }
                }
            }
            System.out.println("Explicitly disabled/rejected (from " + disabledProviderNames + "): " + rejectedCerts.size() + " certs, " + rejectedFps.size() + " fingerprints");
            final int numDisabledStores = disabledFingerprintSets.size();
            final Map<String, Set<RejectionReason>> fpToReasons = buildRejectedFingerprintToReasonsFromFingerprints(rejectedFps, disabledFingerprintSets, disabledProviderReasons);
            final File[] tempFiles = tempDir.listFiles();
            if (tempFiles != null) {
                for (int i = 0; i < tempFiles.length; i++) {
                    if (tempFiles[i].getName().startsWith(DEFAULT_MERGED_PEM_REJECTED_BASE)) {
                        tempFiles[i].delete();
                    }
                }
            }
            for (int k = 1; k <= numDisabledStores; k++) {
                final List<X509Certificate> rejectedByK = getCertsRejectedByAtLeastKProviders(rejectedCerts, disabledFingerprintSets, k);
                if (rejectedByK.isEmpty()) {
                    continue;
                }
                // final File kFile = new File(outDir, DEFAULT_MERGED_PEM_REJECTED_BASE + "." + k + ".pem");
                // try {
                // final ByteArrayOutputStream bos = new ByteArrayOutputStream();
                // writePEM(rejectedByK, bos);
                // IO.writeToFile(kFile, bos.toByteArray());
                // System.out.println("PEM rejected (>= " + k + " disabled lists): " + kFile.getAbsolutePath() + " (" + rejectedByK.size() +
                // " CAs)");
                // } catch (final Exception e) {
                // System.err.println("Failed to write " + kFile.getName() + ": " + e.getMessage());
                // }
                final File kJsonFile = new File(tempDir, DEFAULT_MERGED_PEM_REJECTED_BASE + "." + k + ".json");
                try {
                    writeRejectedJson(filterFpToReasonsByMinProviders(fpToReasons, disabledFingerprintSets, k), kJsonFile);
                    System.out.println("JSON rejected (>= " + k + " disabled lists): " + kJsonFile.getAbsolutePath());
                } catch (final Exception e) {
                    System.err.println("Failed to write " + kJsonFile.getName() + ": " + e.getMessage());
                }
            }
        }
    }
}
