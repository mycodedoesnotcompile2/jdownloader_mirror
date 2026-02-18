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
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
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
package org.appwork.utils.net.httpconnection.trust.ccadb;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.cert.X509Certificate;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpconnection.trust.CurrentJRETrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;

/**
 * AWTest that reads the CCADB common-ca-database.pem, loads all certificates, and verifies each certificate against trusted external
 * platforms (crt.sh, optionally others). Uses SHA-256 fingerprint to query crt.sh; certificates found in Certificate Transparency logs are
 * considered cross-checked. Loads https://crt.sh/?q=&lt;sha256&gt; and parses key info (crt.sh ID, Summary, fingerprints, revocation). Same
 * fingerprint is only queried once per calendar day (in-memory cache).
 */
public class CCADBCertificateVerificationTest extends AWTest {
    private static final long                                       MILLIS_PER_DAY = 86400_000L;
    private static final ConcurrentHashMap<String, CrtshCacheEntry> CRTSH_CACHE    = new ConcurrentHashMap<String, CrtshCacheEntry>();

    /** Cache entry: day index + result (null = not found on crt.sh). */
    private static final class CrtshCacheEntry {
        final long      dayIndex;
        final CrtshInfo info;

        CrtshCacheEntry(final long dayIndex, final CrtshInfo info) {
            this.dayIndex = dayIndex;
            this.info = info;
        }
    }

    /** Parsed info from a crt.sh certificate page (https://crt.sh/?q=sha256). */
    public static class CrtshInfo {
        public final String  crtShId;
        public final String  summary;
        public final String  sha256;
        public final String  sha1;
        public final String  revocationStatus;
        public final boolean ccadbDisclosed;

        public CrtshInfo(final String crtShId, final String summary, final String sha256, final String sha1, final String revocationStatus, final boolean ccadbDisclosed) {
            this.crtShId = crtShId;
            this.summary = summary;
            this.sha256 = sha256;
            this.sha1 = sha1;
            this.revocationStatus = revocationStatus;
            this.ccadbDisclosed = ccadbDisclosed;
        }

        @Override
        public String toString() {
            return "CrtshInfo[id=" + crtShId + ", summary=" + summary + ", sha256=" + sha256 + ", sha1=" + sha1 + ", revocation=" + revocationStatus + ", ccadb=" + ccadbDisclosed + "]";
        }
    }

    private static final String PEM_RESOURCE              = "common-ca-database.pem";
    /** crt.sh: output=json is not supported for sha256 lookup, only for domain search (q=). We use default HTML. */
    private static final String CRTSH_BASE                = "https://crt.sh/?q=";
    /** Max certs to check; default all. Set e.g. CCADB_VERIFY_LIMIT=10 for quick run. */
    private static final String SYS_PROP_LIMIT            = "CCADB_VERIFY_LIMIT";
    private static final int    DELAY_MS_BETWEEN_REQUESTS = 200;

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (Application.isJared(null)) {
            return;
        }
        // Load PEM from same location as CCADBTrustProvider
        final java.io.InputStream is = CCADBTrustProvider.class.getResourceAsStream(PEM_RESOURCE);
        assertNotNull(is, "PEM resource " + PEM_RESOURCE + " must exist");
        final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(is);
        assertNotNull(certs, "Loaded certificates must not be null");
        assertTrue(certs.length > 0, "PEM must contain at least one certificate, got " + certs.length);
        final int limit = getVerifyLimit();
        final int toCheck = limit > 0 ? Math.min(limit, certs.length) : certs.length;
        LogV3.info("Loaded " + certs.length + " certificates from " + PEM_RESOURCE + ", verifying " + toCheck + " against crt.sh (cache: same fingerprint at most once per day) ...");
        final AtomicInteger found = new AtomicInteger(0);
        final AtomicInteger notFound = new AtomicInteger(0);
        for (int i = 0; i < toCheck; i++) {
            if (i > 0 && DELAY_MS_BETWEEN_REQUESTS > 0) {
                Thread.sleep(DELAY_MS_BETWEEN_REQUESTS);
            }
            final X509Certificate cert = certs[i];
            final String subject = cert.getSubjectX500Principal() != null ? cert.getSubjectX500Principal().getName() : "unknown";
            final String sha256 = sha256Fingerprint(cert);
            final CrtshInfo info = fetchAndParseCrtsh(sha256);
            if (info != null) {
                found.incrementAndGet();
                LogV3.info("[" + (i + 1) + "/" + toCheck + "] FOUND  " + subject);
                LogV3.info("  crt.sh: " + info.toString());
            } else {
                notFound.incrementAndGet();
                LogV3.info("[" + (i + 1) + "/" + toCheck + "] not in crt.sh: " + subject + " sha256=" + sha256);
            }
        }
        LogV3.info("Verification result: " + found.get() + " found on crt.sh, " + notFound.get() + " not found (of " + toCheck + " checked)");
        // At least one certificate must be found on crt.sh to prove the check and network work
        assertTrue(found.get() > 0, "At least one certificate from CCADB must be found on crt.sh (platform check); found=" + found.get() + ", notFound=" + notFound.get());
    }

    private static int getVerifyLimit() {
        try {
            final String v = System.getProperty(SYS_PROP_LIMIT);
            if (v != null && !v.isEmpty()) {
                return Integer.parseInt(v);
            }
        } catch (final NumberFormatException ignored) {
        }
        return 0; // 0 = no limit
    }

    private static String sha256Fingerprint(final X509Certificate cert) throws Exception {
        final MessageDigest md = MessageDigest.getInstance("SHA-256");
        md.update(cert.getEncoded());
        final byte[] digest = md.digest();
        final StringBuilder sb = new StringBuilder(digest.length * 2);
        for (final byte b : digest) {
            sb.append(String.format("%02x", b & 0xff));
        }
        return sb.toString();
    }

    /**
     * Load https://crt.sh/?q=&lt;sha256&gt; and parse the main certificate information. Returns null if not found or on error.
     */
    private static CrtshInfo fetchAndParseCrtsh(final String sha256Hex) {
        final String url = CRTSH_BASE + sha256Hex;
        try {
            final RequestContext ctx = new HttpClient().trust(CurrentJRETrustProvider.getInstance()).get(url);
            if (ctx.getCode() != 200) {
                return null;
            }
            final Object target = ctx.getTarget();
            if (!(target instanceof ByteArrayOutputStream)) {
                return null;
            }
            final String body = ((ByteArrayOutputStream) target).toString(StandardCharsets.UTF_8.name());
            if (body.isEmpty()) {
                return null;
            }
            if (body.contains("Unsupported output type")) {
                return null;
            }
            if (body.toLowerCase().contains("no records found") || body.toLowerCase().contains("0 results")) {
                return null;
            }
            return parseCrtshPage(body);
        } catch (final Exception e) {
            LogV3.fine("crt.sh fetch failed for " + sha256Hex + ": " + e.getMessage());
            return null;
        }
    }

    /**
     * Parse key information from a crt.sh certificate HTML page (e.g.
     * https://crt.sh/?q=2e7bf16cc22485a7bbe2aa8696750761b0ae39be3b2fe9d0cc6d4ef73491425c).
     */
    private static CrtshInfo parseCrtshPage(final String html) {
        String crtShId = null;
        String summary = null;
        String sha256 = null;
        String sha1 = null;
        String revocationStatus = null;
        final boolean ccadbDisclosed = html.contains("CCADB") && html.contains("Disclosed");
        // crt.sh ID: ?id=163978581
        final Matcher idMatcher = Pattern.compile("\\?id=(\\d+)").matcher(html);
        if (idMatcher.find()) {
            crtShId = idMatcher.group(1);
        }
        // Summary: e.g. "Root certificate" after Summary header
        final Matcher summaryMatcher = Pattern.compile("Summary\\s*</TH>\\s*<TD[^>]*>\\s*([^<]+?)\\s*</TD>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL).matcher(html);
        if (summaryMatcher.find()) {
            summary = summaryMatcher.group(1).replaceAll("\\s+", " ").trim();
        }
        // Certificate Fingerprints: <TH>SHA-256</TH><TD><A href="...">HEX64</A></TD> or <TD>HEX64</TD>
        final Matcher sha256Matcher = Pattern.compile("SHA-256</TH>\\s*<TD[^>]*>(?:<A[^>]*>)?\\s*([0-9A-Fa-f]{64})\\s*(?:</A>)?", Pattern.CASE_INSENSITIVE).matcher(html);
        if (sha256Matcher.find()) {
            sha256 = sha256Matcher.group(1);
        }
        // SHA-1: <TH>SHA-1</TH><TD>HEX40</TD>
        final Matcher sha1Matcher = Pattern.compile("SHA-1</TH>\\s*<TD[^>]*>\\s*([0-9A-Fa-f]{40})\\s*</TD>", Pattern.CASE_INSENSITIVE).matcher(html);
        if (sha1Matcher.find()) {
            sha1 = sha1Matcher.group(1);
        }
        // Revocation: first "Not Revoked" or "Revoked" after Revocation / CRL section
        final Matcher revMatcher = Pattern.compile("(Not Revoked|Revoked)").matcher(html);
        if (revMatcher.find()) {
            revocationStatus = revMatcher.group(1);
        }
        return new CrtshInfo(crtShId != null ? crtShId : "", summary != null ? summary : "", sha256 != null ? sha256 : "", sha1 != null ? sha1 : "", revocationStatus != null ? revocationStatus : "", ccadbDisclosed);
    }
}
