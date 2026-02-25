/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         ====================================================================================================================================================
 *         ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.trust.ccadb.tests;

import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.Hash;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.CurrentJRETrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;
import org.appwork.utils.net.httpconnection.trust.ccadb.CCADBTrustProvider;

/**
 * Verifies all certificates from the CCADB PEM against JRE- and OS-TrustProvider and crt.sh. Certificates not found in any truststore are
 * checked via crt.sh. The test fails only when at least one certificate fails all three checks (JRE, OS, CRTSH). In that case these are
 * output as {@code <fingerprint> FailedIn: JRE,OS,CRTSH}.
 */
public class CCADBPEMTrustProviderTest extends AWTest {
    private static final String PEM_RESOURCE = org.appwork.utils.net.httpconnection.trust.ccadb.CCADBTrustProvider.APPWORK_MERGED_PEM;

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (Application.isJared(null)) {
            return;
        }
        final InputStream is = CCADBTrustProvider.class.getResourceAsStream(PEM_RESOURCE);
        assertNotNull(is, "PEM resource " + PEM_RESOURCE + " must exist");
        final X509Certificate[] certs = TrustUtils.loadCertificatesFromPEM(is);
        assertNotNull(certs, "Loaded certificates must not be null");
        assertTrue(certs.length > 0, "PEM must contain at least one certificate, found: " + certs.length);
        CCADBCertificateVerificationTest.loadPersistedCache();
        final TrustProviderInterface jreProvider = CurrentJRETrustProvider.getInstance();
        final TrustProviderInterface osProvider = TrustUtils.getOSProvider();
        final List<String> problemList = new ArrayList<String>();
        int trustedByBoth = 0;
        boolean revokedTestsDone = false;
        LogV3.info("Verifying " + certs.length + " certificates from " + PEM_RESOURCE + " against JRE-, OS-TrustProvider and crt.sh (with cache) ...");
        for (int i = 0; i < certs.length; i++) {
            final X509Certificate cert = certs[i];
            final String fingerprint = Hash.getSHA256(cert.getEncoded());
            final X509Certificate[] chain = new X509Certificate[] { cert };
            final String authType = "RSA";
            final TrustResult jreResult = jreProvider.checkServerTrusted(chain, authType, null);
            final TrustResult osResult = osProvider.checkServerTrusted(chain, authType, null);
            final boolean jreOk = jreResult != null && jreResult.isTrusted();
            final boolean osOk = osResult != null && osResult.isTrusted();
            boolean crtShOk = true;
            boolean crtShChecked = false;
            LogV3.info(i + "/" + certs.length + " " + fingerprint + " OS: " + osOk + " JRE: " + jreOk);
            if (!jreOk && !osOk) {
                crtShChecked = true;
                if (!CCADBCertificateVerificationTest.wouldHitCrtshCache(fingerprint) && !revokedTestsDone) {
                    LogV3.info("HTTP to crt.sh required – running revoked validation first.");
                    CCADBCertificateVerificationTest.runRevokedValidationTests();
                    revokedTestsDone = true;
                }
                final CCADBCertificateVerificationTest.CrtshInfo crtShInfo = CCADBCertificateVerificationTest.fetchAndParseCrtsh(fingerprint);
                crtShOk = crtShInfo != null && (crtShInfo.revocationStatus == null || !crtShInfo.revocationStatus.toLowerCase().contains("revoked"));
            }
            if (jreOk && osOk) {
                trustedByBoth++;
            }
            if (crtShChecked && !crtShOk) {
                problemList.add(fingerprint + " FailedIn: JRE,OS,CRTSH");
            }
        }
        LogV3.info("Result: " + trustedByBoth + " of " + certs.length + " certificates trusted by JRE and OS; " + problemList.size() + " certificate(s) failed all checks (JRE,OS,CRTSH).");
        assertTrue(trustedByBoth > 0, "At least one PEM certificate must be accepted by JRE- and OS-TrustProvider (sanity check); trustedByBoth=" + trustedByBoth);
        if (!problemList.isEmpty()) {
            LogV3.severe("--- Certificates that failed all checks (JRE, OS, CRTSH) ---");
            for (final String line : problemList) {
                LogV3.severe(line);
            }
            assertTrue(false, problemList.size() + " certificate(s) failed JRE, OS and CRTSH: " + String.join("; ", problemList.subList(0, Math.min(5, problemList.size()))) + (problemList.size() > 5 ? " ..." : ""));
        }
    }
}
