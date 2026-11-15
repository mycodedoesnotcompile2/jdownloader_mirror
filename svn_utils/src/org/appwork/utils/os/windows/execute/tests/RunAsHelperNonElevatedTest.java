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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
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
package org.appwork.utils.os.windows.execute.tests;

import java.io.Serializable;
import java.util.EnumSet;
import java.util.Set;

import org.appwork.JNAHelper;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestTag;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.RunAsHelper;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

/**
 * Tests for {@link RunAsHelper#runNonElevated}: same-user admin downgrade via WinSafer (also covered by
 * {@link org.appwork.testframework.tests.TestRunAsNonElevatedUserFromAdmin} via AdminExecuter).
 *
 * <pre>
 * Flow
 *  |
 *  +-- Skip if not Windows/JNA
 *  |
 *  +-- Host already unelevated
 *  |     Run runNonElevated
 *  |     We expect child SID == host SID and elev=0
 *  |
 *  +-- Execute task as elevated admin
 *  |     Run runNonElevated identity probe + net session
 *  |     We expect child SID == elevated JVM SID, elev=0, net session fails
 *  |
 *  +-- Execute task as LocalSystem
 *        Run runNonElevated
 *        We expect IllegalStateException (use runInSession)
 * </pre>
 */
@TestDependency({ "org.appwork.utils.os.windows.execute.RunAsHelper", "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class RunAsHelperNonElevatedTest extends AWTest {
    /**
     * Machine line {@code session|sid|elev} from powershell child ({@code elev} {@code 1} = high mandatory label S-1-16-12288).
     */
    private static final String PS_PROBE_SESSION_SID_ELEV = "$s=[int]([Diagnostics.Process]::GetCurrentProcess().SessionId);"
            + "$id=[System.Security.Principal.WindowsIdentity]::GetCurrent().User.Value;"
            + "$pr=New-Object System.Security.Principal.WindowsPrincipal([System.Security.Principal.WindowsIdentity]::GetCurrent());"
            + "$hi=$pr.IsInRole([System.Security.Principal.SecurityIdentifier]::new('S-1-16-12288'));"
            + "$e=if($hi){'1'}else{'0'};"
            + "Write-Output ($s.ToString()+'|'+$id+'|'+$e)";
    private static final TypeRef<NonElevatedProbeReport> TYPE_REPORT = new TypeRef<NonElevatedProbeReport>() {
                                                                     };

    public static void main(String[] args) {
        run();
    }

    @Override
    public boolean isMaintenance() {
        return false;
    }

    @Override
    public Set<TestTag> getTags() {
        return EnumSet.of(TestTag.UAC);
    }

    @Override
    public void runTest() throws Exception {
        // Step 1: Skip if not Windows/JNA
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            logInfoAnyway("RunAsHelperNonElevatedTest: Windows + JNA required, skipped.");
            return;
        }
        testHostUnelevated();
        testElevatedSafer();
        testLocalSystemRejected();
    }

    /**
     * Host already unelevated: call {@link RunAsHelper#runNonElevated} from this JVM (no AdminExecuter).
     *
     * <pre>
     * Preconditions (otherwise skipped, not failed):
     *   - Host must NOT be elevated ({@link WindowsUtils#isElevated()} == false)
     *   - Host must NOT be LocalSystem
     *   → under those skips, runNonElevated would not exercise the ProcessBuilder / same-user path we assert here
     *
     * Run:
     *   RunAsHelper.runNonElevated(powershell identity probe)
     *   Probe stdout first line: session|sid|elev  (elev 1 = high integrity S-1-16-12288)
     *
     * Pass when:
     *   1. Host SID is non-empty
     *   2. Child process exit code == 0
     *   3. Child SID equals host SID (case-insensitive) — same account, not a session-owner switch
     *   4. Child elev == "0" — not high integrity (unelevated / medium-or-lower)
     *
     * Note: from an already unelevated host, runNonElevated uses ProcessBuilder (no Safer token). This check still
     * verifies the public contract: child stays same SID and non-high-integrity.
     * </pre>
     */
    private void testHostUnelevated() throws Exception {
        if (WindowsUtils.isElevated() || WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("RunAsHelperNonElevatedTest: skip host unelevated (host elevated or LocalSystem).");
            return;
        }
        final String hostSid = WindowsUtils.getCurrentUserSID();
        // Baseline: host must have a resolvable SID to compare against the child.
        assertTrue(hostSid != null && hostSid.length() > 0, "host SID required");
        final ProcessOutput out = RunAsHelper.runNonElevated(powershellIdentityArgv(), RunAsLaunchOptions.DEFAULT);
        final ParsedIdentityLine child = parseIdentityProbe("host", out);
        // Same account: unelevated host path must not switch identity (ProcessBuilder, not session-owner).
        assertEqualsIgnoreCase(hostSid, child.sid, "host: child SID must match host");
        // Host is already unelevated — child must stay non-high-integrity (elev=0).
        assertEquals("0", child.elev, "host: child must not be high integrity");
    }

    /**
     * Elevated admin: WinSafer path via {@link AdminExecuter#runAsAdmin} then {@link RunAsHelper#runNonElevated}.
     */
    private void testElevatedSafer() throws Exception {
        // Task JVM is elevated; inside it, runNonElevated must Safer-downgrade to same SID / non-high integrity.
        final NonElevatedProbeReport report = AdminExecuter.runAsAdmin(new ElevatedNonElevatedProbeTask(), TYPE_REPORT, ProcessOptions.DEFAULT);
        // Task must have completed and returned a probe report (identity + net session exit).
        assertTrue(report != null, "report null");
        // Parent context of runNonElevated must be elevated — otherwise we would not hit the Safer token path.
        assertTrue(report.launchingElevated, "elevated task JVM must be elevated");
        // Need the elevated JVM's SID as baseline for the same-user downgrade check.
        assertTrue(report.launchingSid != null && report.launchingSid.length() > 0, "launching SID required");
        // Child identity probe must have been parsed (session|sid|elev from powershell).
        assertTrue(report.childIdentity != null, "elevated-SAFER: identity null");
        // Same account: Safer downgrades integrity, it must not switch to another interactive session user.
        assertEqualsIgnoreCase(report.launchingSid, report.childIdentity.sid, "elevated-SAFER: child SID must match launching JVM (same-user downgrade)");
        // Unelevated: child must not hold high integrity (S-1-16-12288); medium/lower is required.
        assertEquals("0", report.childIdentity.elev, "elevated-SAFER: child must not be high integrity");
        // net session succeeds only when elevated — non-null exit code required before checking failure.
        assertTrue(report.netSessionExit != null, "elevated-SAFER: net session exit null");
        // exit != 0 proves the Safer child lost admin rights (net session requires elevation).
        assertTrue(report.netSessionExit.intValue() != 0, "elevated-SAFER: net session must fail when non-elevated (exit=" + report.netSessionExit + ")");
    }

    /**
     * LocalSystem must reject {@link RunAsHelper#runNonElevated} (same-user downgrade would stay SYSTEM).
     */
    private void testLocalSystemRejected() throws Exception {
        // Task must run as LocalSystem, then call runNonElevated and return the IllegalStateException message.
        final String message = AdminExecuter.runAsLocalSystem(new LocalSystemRejectNonElevatedTask(), TypeRef.STRING, ProcessOptions.DEFAULT);
        // Expect a real reject message — empty means the task did not surface the failure correctly.
        assertTrue(message != null && message.length() > 0, "LocalSystem reject message null");
        final String lower = message.toLowerCase();
        // Message must name LocalSystem and/or point callers at runInSession (the supported API).
        assertTrue(lower.indexOf("localsystem") >= 0 || lower.indexOf("runinsession") >= 0, "LocalSystem reject message unexpected: " + message);
    }

    private static void assertEqualsIgnoreCase(final String expected, final String actual, final String message) throws Exception {
        final String e = expected != null ? expected.trim() : "";
        final String a = actual != null ? actual.trim() : "";
        // Case-insensitive SID/string compare used by identity checks above.
        assertTrue(e.equalsIgnoreCase(a), message + " expected=" + e + " actual=" + a);
    }

    private static String[] powershellIdentityArgv() {
        return new String[] { "powershell.exe", "-NoProfile", "-NonInteractive", "-Command", PS_PROBE_SESSION_SID_ELEV };
    }

    private static ParsedIdentityLine parseIdentityProbe(final String label, final ProcessOutput out) throws Exception {
        // Probe must produce a ProcessOutput object (launch succeeded enough to capture streams).
        assertTrue(out != null, label + ": ProcessOutput null");
        // Identity probe powershell must exit 0 — non-zero means we cannot trust session|sid|elev.
        assertEquals(0, out.getExitCode(), label + ": exit, stderr=" + out.getErrOutString());
        final String stdout = out.getStdOutString() != null ? out.getStdOutString() : "";
        final String firstLine = firstNonEmptyLine(stdout);
        return parseIdentityFirstLine(label, firstLine);
    }

    private static ParsedIdentityLine parseIdentityFirstLine(final String label, final String firstLine) throws Exception {
        if (firstLine == null || firstLine.trim().length() == 0) {
            throw new Exception(label + ": empty identity first line");
        }
        final String[] parts = firstLine.trim().split("\\|", -1);
        if (parts.length != 3) {
            throw new Exception(label + ": expected session|sid|elev, got: " + firstLine);
        }
        final int session;
        try {
            session = Integer.parseInt(parts[0].trim());
        } catch (NumberFormatException e) {
            throw new Exception(label + ": bad session: " + parts[0], e);
        }
        final String sid = parts[1].trim();
        final String elev = parts[2].trim();
        if (!"0".equals(elev) && !"1".equals(elev)) {
            throw new Exception(label + ": elev must be 0 or 1, got: " + elev);
        }
        return new ParsedIdentityLine(session, sid, elev);
    }

    private static String firstNonEmptyLine(final String text) {
        if (text == null) {
            return "";
        }
        final String[] lines = text.split("\\r?\\n");
        for (int i = 0; i < lines.length; i++) {
            if (lines[i] != null && lines[i].trim().length() > 0) {
                return lines[i].trim();
            }
        }
        return "";
    }

    public static final class ParsedIdentityLine implements Serializable {
        private static final long serialVersionUID = 1L;
        public int                session;
        public String             sid;
        public String             elev;

        public ParsedIdentityLine() {
        }

        ParsedIdentityLine(final int session, final String sid, final String elev) {
            this.session = session;
            this.sid = sid != null ? sid : "";
            this.elev = elev != null ? elev : "";
        }
    }

    public static final class NonElevatedProbeReport implements Serializable {
        private static final long serialVersionUID = 1L;
        public boolean            launchingElevated;
        public String             launchingSid;
        public ParsedIdentityLine childIdentity;
        public Integer            netSessionExit;

        public NonElevatedProbeReport() {
        }
    }

    private static final class ElevatedNonElevatedProbeTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            final NonElevatedProbeReport report = new NonElevatedProbeReport();
            report.launchingElevated = WindowsUtils.isElevated();
            report.launchingSid = WindowsUtils.getCurrentUserSID();
            final ProcessOutput launchingProbe = ProcessBuilderFactory.runCommand(powershellIdentityArgv());
            parseIdentityProbe("launchingJvm", launchingProbe);
            final ProcessOutput identityOut = RunAsHelper.runNonElevated(powershellIdentityArgv(), RunAsLaunchOptions.DEFAULT);
            report.childIdentity = parseIdentityProbe("SAFER", identityOut);
            final ProcessOutput netOut = RunAsHelper.runNonElevated(new String[] { "cmd.exe", "/c", "net", "session" }, RunAsLaunchOptions.DEFAULT);
            report.netSessionExit = Integer.valueOf(netOut.getExitCode());
            return report;
        }
    }

    private static final class LocalSystemRejectNonElevatedTask implements ElevatedTestTask {
        private static final long serialVersionUID = 1L;

        @Override
        public Serializable run() throws Exception {
            // Must actually be LocalSystem — otherwise we would not be testing the SYSTEM reject path.
            AWTest.assertTrue(Boolean.valueOf(WindowsUtils.isRunningAsLocalSystem()), "LocalSystemRejectNonElevatedTask must run as LocalSystem");
            try {
                RunAsHelper.runNonElevated(new String[] { "cmd.exe", "/c", "echo", "should-fail" }, RunAsLaunchOptions.DEFAULT);
                return "NO_EXCEPTION";
            } catch (IllegalStateException e) {
                return e.getMessage() != null ? e.getMessage() : e.getClass().getName();
            } catch (Throwable t) {
                return "UNEXPECTED:" + (t.getMessage() != null ? t.getMessage() : t.getClass().getName());
            }
        }
    }
}
