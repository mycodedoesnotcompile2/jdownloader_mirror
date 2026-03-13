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
        file.getParentFile().mkdirs();
        final FileOutputStream out = new FileOutputStream(file);
        try {
            IO.readStreamToOutputStream(-1, TestWindowsCertificateCode.class.getResource("test.exe").openStream(), out, true);
        } finally {
            out.close();
        }
        File javaExeSigndateFromCounterSig = Application.getTempUniqueResource("appworktests");
        javaExeSigndateFromCounterSig.deleteOnExit();
        javaExeSigndateFromCounterSig.getParentFile().mkdirs();
        final FileOutputStream out2 = new FileOutputStream(javaExeSigndateFromCounterSig);
        try {
            IO.readStreamToOutputStream(-1, TestWindowsCertificateCode.class.getResource("javaw.exe").openStream(), out2, true);
        } finally {
            out2.close();
        }
        TestTarget testExeTarget;
        // ------------------------------------------------------------
        // Test targets (prevent Eclipse auto-formatting)
        // ------------------------------------------------------------
        //@formatter:off
        TestTarget[] targets = new TestTarget[] {
            new TestTarget(javaExeSigndateFromCounterSig.getAbsolutePath(), true, new ExpectedResult("Eclipse.org Foundation, Inc.", "DigiCert Trusted G4 Code Signing RSA4096 SHA384 2021 CA1", DateMapper.parse("2025-01-22T02:57:12Z"), DateMapper.parse("2025-07-22T01:59:59Z"))),
            testExeTarget = new TestTarget(file.getAbsolutePath(), true, new ExpectedResult("Appwork GmbH", "GlobalSign GCC R45 EV CodeSigning CA 2020", DateMapper.parse("2025-09-20T19:24:38Z"), DateMapper.parse("2026-10-22T14:36:28Z"))),

            new TestTarget(CrossSystem.getJavaBinary(), true,
                new ExpectedResult("Eclipse.org Foundation, Inc.", "DigiCert Trusted G4 Code Signing RSA4096 SHA384 2021 CA1", null, null),
                new ExpectedResult("Eclipse.org Foundation, Inc.", "DigiCert SHA2 Assured ID Code Signing CA", null, null)),
            new TestTarget(winDir + "explorer.exe", true, new ExpectedResult("Microsoft Windows", "Microsoft Windows Production PCA 2011", null, null)),
            new TestTarget(sys32 + "taskmgr.exe", true, new ExpectedResult("Microsoft Windows", "Microsoft Windows Production PCA 2011", null, null)),
            new TestTarget(winDir + "regedit.exe", false),
            new TestTarget(sys32 + "mmc.exe", false),
            new TestTarget(sys32 + "services.exe", true, new ExpectedResult("Microsoft Windows Publisher", "Microsoft Windows Production PCA 2011", null, null)),
            new TestTarget(sys32 + "svchost.exe", true, new ExpectedResult("Microsoft Windows Publisher", "Microsoft Windows Production PCA 2011", null, null)),
            new TestTarget(sys32 + "wuauclt.exe", true, new ExpectedResult("Microsoft Windows", "Microsoft Windows Production PCA 2011", null, null))
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
     * Passes if at least one of the target's expected results matches the actual certificate info.
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
        // ---- Date validity checks (independent of expected result) ----
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
        // ---- Fingerprint presence ----
        if (info.getFingerprint() == null || info.getFingerprint().isEmpty()) {
            System.out.println("⚠ Warning: missing fingerprint for " + name);
        }
        // ---- Match against list of expected results: at least one must match ----
        if (target.expectedResults == null || target.expectedResults.length == 0) {
            return;
        }
        for (ExpectedResult expected : target.expectedResults) {
            if (matchesExpected(info, signDate, validUntil, expected)) {
                return;
            }
        }
        StringBuilder sb = new StringBuilder(name + ": no expected result matched. Actual: CN=");
        sb.append(info.getIssuedTo().get("CN"));
        sb.append(", Issuer CN=");
        sb.append(info.getIssuedBy().get("CN"));
        sb.append(". Expected one of: ");
        for (int i = 0; i < target.expectedResults.length; i++) {
            ExpectedResult e = target.expectedResults[i];
            if (i > 0) {
                sb.append(" | ");
            }
            sb.append("CN=").append(e.cn).append(", IssuerCN=").append(e.issuerCn);
        }
        throw new IllegalStateException(sb.toString());
    }

    /**
     * Returns true if the actual certificate info matches this expected result (all non-null fields match).
     */
    private boolean matchesExpected(CodeSignature info, Date signDate, Date validUntil, ExpectedResult expected) {
        if (expected.cn != null) {
            String cn = info.getIssuedTo().get("CN");
            if (cn == null || !cn.toLowerCase().contains(expected.cn.toLowerCase())) {
                return false;
            }
        }
        if (expected.issuerCn != null) {
            String issuerCN = info.getIssuedBy().get("CN");
            if (issuerCN == null || !issuerCN.toLowerCase().contains(expected.issuerCn.toLowerCase())) {
                return false;
            }
        }
        if (expected.expectedSigningDate != null && signDate != null) {
            if (Math.abs(signDate.getTime() - expected.expectedSigningDate.getTime()) > (60 * 1000L)) {
                return false;
            }
        }
        if (expected.expectedValidUntil != null && validUntil != null) {
            if (Math.abs(validUntil.getTime() - expected.expectedValidUntil.getTime()) > 86400000L) {
                return false;
            }
        }
        return true;
    }

    /**
     * One possible expected certificate result. All non-null fields must match for this result to match.
     */
    private static final class ExpectedResult {
        final String cn;
        final String issuerCn;
        final Date   expectedSigningDate;
        final Date   expectedValidUntil;

        ExpectedResult(String cn, String issuerCn, Date expectedSigningDate, Date expectedValidUntil) {
            this.cn = cn;
            this.issuerCn = issuerCn;
            this.expectedSigningDate = expectedSigningDate;
            this.expectedValidUntil = expectedValidUntil;
        }
    }

    /**
     * Test definition for one executable. Validation passes if at least one of the expected results matches.
     */
    private static final class TestTarget {
        final String           path;
        final boolean          mustExist;
        final ExpectedResult[] expectedResults;

        TestTarget(String path, boolean mustExist, ExpectedResult... expectedResults) {
            this.path = path;
            this.mustExist = mustExist;
            this.expectedResults = expectedResults;
        }
    }

    public static void main(String[] args) {
        run();
    }
}
