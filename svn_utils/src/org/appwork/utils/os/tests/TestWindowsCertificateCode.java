package org.appwork.utils.os.tests;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Date;
import java.util.Map;

import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.windows.signature.CodeSignature;
import org.appwork.utils.os.windows.signature.WindowsSignature;

@TestDependency({ "org.appwork.utils.os.WindowsUtils" })
public class TestWindowsCertificateCode extends AWTest {
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("Test must run on Windows!");
            return;
        }
        String winDir = System.getenv("SystemRoot") + "\\";
        String sys32 = winDir + "System32\\";
        // Create temporary signed test file
        File file = Application.getTempUniqueResource("appworktests");
        file.deleteOnExit();
        final FileOutputStream out = new FileOutputStream(file);
        try {
            IO.readStreamToOutputStream(-1, TestWindowsCertificateCode.class.getResource("test.exe").openStream(), out, true);
        } finally {
            out.close();
        }
        TestTarget testExeTarget;
        // ------------------------------------------------------------
        // Test targets (prevent Eclipse auto-formatting)
        // ------------------------------------------------------------
        //@formatter:off
        TestTarget[] targets = new TestTarget[] {
            testExeTarget= new TestTarget(file.getAbsolutePath(), true,    "Appwork GmbH", "GlobalSign GCC R45 EV CodeSigning CA 2020", DateMapper.parse("2025-09-20T19:24:38Z"), DateMapper.parse("2026-10-22T14:36:28Z")),
            new TestTarget(CrossSystem.getJavaBinary(),  true,    "Eclipse.org Foundation, Inc.", "DigiCert Trusted G4 Code Signing RSA4096 SHA384 2021 CA1", null, null),
            new TestTarget(winDir + "explorer.exe",      true,    "Microsoft Windows", "Microsoft Windows Production PCA 2011", null, null),
            new TestTarget(sys32 + "taskmgr.exe",        true,    "Microsoft Windows", "Microsoft Windows Production PCA 2011", null, null),
            new TestTarget(winDir + "regedit.exe",       false,  null, null, null, null),
            new TestTarget(sys32 + "mmc.exe",            false, null, null, null, null),
            new TestTarget(sys32 + "services.exe",       true,    "Microsoft Windows Publisher", "Microsoft Windows Production PCA 2011", null, null),
            new TestTarget(sys32 + "svchost.exe",        true,    "Microsoft Windows Publisher", "Microsoft Windows Production PCA 2011", null, null),
            new TestTarget(sys32 + "wuauclt.exe",        true,    "Microsoft Windows", "Microsoft Windows Production PCA 2011", null, null)
        };
        //@formatter:on
        // ------------------------------------------------------------
        // Run all checks
        // ------------------------------------------------------------
        for (TestTarget t : targets) {
            File f = new File(t.path);
            System.out.println("--------------------------------------------------");
            System.out.println("File: " + f.getAbsolutePath());
            CodeSignature info = WindowsSignature.readCodeSignSignature(f);
            boolean valid = WindowsSignature.verifySignature(f);
            if (info == null) {
                System.out.println("No certificate info found.");
            } else {
                System.out.println("Fingerprint: " + info.getFingerprint());
                System.out.println("Signing Date: " + info.getSigningDate());
                System.out.println("Valid Until: " + info.getValidUntil());
                System.out.println("Issued To:");
                for (Map.Entry<String, String> e : info.getIssuedTo().entrySet()) {
                    System.out.println("  " + e.getKey() + " = " + e.getValue());
                }
                System.out.println("Issued By:");
                for (Map.Entry<String, String> e : info.getIssuedBy().entrySet()) {
                    System.out.println("  " + e.getKey() + " = " + e.getValue());
                }
            }
            System.out.println("Signature valid: " + valid);
            validateResult(t, info, valid);
        }
        // ----------------------------------------------------------------
        // 🔥 Tampering test — intentionally corrupt the signed file
        // ----------------------------------------------------------------
        System.out.println("--------------------------------------------------");
        System.out.println("⚠ Tampering test: corrupting file and verifying failure...");
        byte[] bytes = IO.readFile(file);
        // Flip one byte in the middle of the file to invalidate signature
        int mid = bytes.length / 2;
        bytes[mid] ^= 0xFF;
        IO.secureWrite(file, bytes);
        boolean stillValid = WindowsSignature.verifySignature(file);
        System.out.println("Signature valid after tampering: " + stillValid);
        boolean failed = false;
        try {
            CodeSignature info = WindowsSignature.readCodeSignSignature(file);
            validateResult(testExeTarget, info, stillValid);
        } catch (Exception e) {
            failed = true;
            System.out.println("✅ Expected failure detected after tampering: " + e.getMessage());
        }
        if (!failed) {
            throw new IllegalStateException("❌ Tampering test failed — validateResult did not throw as expected.");
        }
        System.out.println("✅ All signature checks passed (including tampering test).");
        file.delete();
    }

    /**
     * Validates a single test result. Includes CN, issuer, fingerprint, and date consistency checks.
     */
    private void validateResult(TestTarget target, CodeSignature info, boolean valid) {
        String name = new File(target.path).getName();
        if (target.mustExist && (info == null || info.getIssuedTo().isEmpty())) {
            throw new IllegalStateException(name + ": certificate info expected but missing.");
        }
        if (!target.mustExist && info == null) {
            return;
        }
        if (!valid) {
            throw new IllegalStateException(name + ": signature expected to be valid.");
        }
        if (info == null) {
            return;
        }
        // ---- Issued To CN ----
        if (target.expectedCN != null) {
            String cn = info.getIssuedTo().get("CN");
            if (cn == null) {
                throw new IllegalStateException(name + ": missing CN (issuedTo).");
            }
            if (!cn.toLowerCase().contains(target.expectedCN.toLowerCase())) {
                throw new IllegalStateException(name + ": CN mismatch, expected '" + target.expectedCN + "', got '" + cn + "'");
            }
        }
        // ---- Issued By CN ----
        if (target.expectedIssuerCN != null) {
            String issuerCN = info.getIssuedBy().get("CN");
            if (issuerCN == null) {
                throw new IllegalStateException(name + ": missing CN (issuedBy).");
            }
            if (!issuerCN.toLowerCase().contains(target.expectedIssuerCN.toLowerCase())) {
                throw new IllegalStateException(name + ": Issuer CN mismatch, expected '" + target.expectedIssuerCN + "', got '" + issuerCN + "'");
            }
        }
        // ---- Date validity checks ----
        Date now = new Date();
        Date signDate = info.getSigningDate();
        Date validUntil = info.getValidUntil();
        if (signDate != null && validUntil != null && signDate.after(validUntil)) {
            throw new IllegalStateException(name + ": signingDate is after validUntil — invalid timeline.");
        }
        if (validUntil != null && validUntil.before(now)) {
            System.out.println("⚠ Warning: certificate expired for " + name + " (expired " + validUntil + ")");
        }
        if (signDate != null && signDate.after(now)) {
            System.out.println("⚠ Warning: signing date is in the future for " + name + " (" + signDate + ")");
        }
        // ---- Optional expected date range checks ----
        if (target.expectedSigningDate != null && signDate != null) {
            if (Math.abs(signDate.getTime() - target.expectedSigningDate.getTime()) > (60 * 1000l)) {
                throw new IllegalStateException(name + ": signingDate differs more than 1min from expected.");
            }
        }
        if (target.expectedValidUntil != null && validUntil != null) {
            long diff = Math.abs(validUntil.getTime() - target.expectedValidUntil.getTime());
            if (diff > 86400000l) { // >
                                    // 1
                                    // day
                                    // difference
                throw new IllegalStateException(name + ": validUntil differs more than 1 day from expected.");
            }
        }
        // ---- Fingerprint presence ----
        if (info.getFingerprint() == null || info.getFingerprint().isEmpty()) {
            System.out.println("⚠ Warning: missing fingerprint for " + name);
        }
    }

    /**
     * Test definition for one executable. Allows optional date validation and expected issuer.
     */
    private static final class TestTarget {
        final String  path;
        final boolean mustExist;
        final String  expectedCN;
        final String  expectedIssuerCN;
        final Date    expectedSigningDate;
        final Date    expectedValidUntil;

        TestTarget(String path, boolean mustExist, String expectedCN, String expectedIssuerCN, Date expectedSigningDate, Date expectedValidUntil) {
            this.path = path;
            this.mustExist = mustExist;
            this.expectedCN = expectedCN;
            this.expectedIssuerCN = expectedIssuerCN;
            this.expectedSigningDate = expectedSigningDate;
            this.expectedValidUntil = expectedValidUntil;
        }
    }

    public static void main(String[] args) {
        run();
    }
}
