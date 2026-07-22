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
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.testframework;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;

/**
 * Collects sub-test results during a run and writes JSON/HTML reports. Used by {@link IDETestRunner} and {@link PostBuildRunner}.
 */
public final class TestCaseReporter {
    /** System property: absolute path to export the current {@link TestClassReport} as JSON (post-build subprocess). */
    public static final String REPORT_EXPORT_FILE_PROPERTY = "awtest.report.export.file";
    /** System property: when {@code true}, do not open the HTML report in the default browser after the run. */
    public static final String NO_OPEN_REPORT_PROPERTY     = "awtest.noOpenReport";

    private static final ThreadLocal<TestClassReport> CURRENT_CLASS = new ThreadLocal<TestClassReport>();
    private static TestRunReport                      RUN_REPORT;
    private static File                               REPORT_DIR;

    private TestCaseReporter() {
    }

    public static void beginRun(final String runnerLabel, final File reportDirectory) {
        beginRun(runnerLabel, reportDirectory, null, null);
    }

    /**
     * Starts a new run report. {@code projectInfo} is typically {@code "-projectinfo=Project setup"} (project name, space, setup).
     *
     * @param buildId
     *            optional {@code -buildid=} value
     * @param projectInfo
     *            optional {@code -projectinfo=} value ({@code Project} or {@code Project Setup})
     */
    public static void beginRun(final String runnerLabel, final File reportDirectory, final String buildId, final String projectInfo) {
        RUN_REPORT = new TestRunReport();
        RUN_REPORT.setRunnerLabel(runnerLabel);
        RUN_REPORT.setBuildId(buildId);
        applyProjectInfo(RUN_REPORT, projectInfo);
        RUN_REPORT.setStartedAt(Time.systemIndependentCurrentJVMTimeMillis());
        REPORT_DIR = reportDirectory;
        if (REPORT_DIR != null && !REPORT_DIR.exists()) {
            REPORT_DIR.mkdirs();
        }
    }

    /**
     * Splits {@code projectInfo} into project (first token) and setup (remainder).
     */
    private static void applyProjectInfo(final TestRunReport report, final String projectInfo) {
        if (report == null || StringUtils.isEmpty(projectInfo)) {
            return;
        }
        final String trimmed = projectInfo.trim();
        final int space = trimmed.indexOf(' ');
        if (space < 0) {
            report.setProject(trimmed);
            return;
        }
        report.setProject(trimmed.substring(0, space).trim());
        final String setup = trimmed.substring(space + 1).trim();
        if (StringUtils.isNotEmpty(setup)) {
            report.setSetup(setup);
        }
    }

    public static void beginTestClass(final String className) {
        final TestClassReport report = new TestClassReport();
        report.setClassName(className);
        report.setCases(new ArrayList<TestCaseEntry>());
        CURRENT_CLASS.set(report);
    }

    public static void testSucceeded(final String testCaseName) {
        addCase(testCaseName, TestCaseEntry.STATUS_SUCCESS, null);
    }

    public static void testSkipped(final String testCaseName, final String reason) {
        addCase(testCaseName, TestCaseEntry.STATUS_SKIPPED, reason);
    }

    private static void addCase(final String testCaseName, final String status, final String reason) {
        final TestClassReport current = CURRENT_CLASS.get();
        if (current == null || StringUtils.isEmpty(testCaseName)) {
            return;
        }
        final List<TestCaseEntry> cases = current.getCases();
        if (cases == null) {
            current.setCases(new ArrayList<TestCaseEntry>());
        }
        current.getCases().add(new TestCaseEntry(testCaseName, status, reason));
    }

    public static void endTestClassSuccess() {
        finishCurrentClass(TestClassReport.CLASS_SUCCESS, null, null);
    }

    public static void endTestClassFailed(final Throwable error) {
        final String msg = error != null ? error.toString() : "failed";
        finishCurrentClass(TestClassReport.CLASS_FAILED, null, msg);
    }

    public static void endTestClassMaintenance() {
        finishCurrentClass(TestClassReport.CLASS_MAINTENANCE, "maintenance", null);
    }

    /**
     * Records a test class that was not executed (dependency skip, runner skip, etc.).
     */
    public static void recordClassSkippedWithoutRun(final String className, final String reason) {
        if (RUN_REPORT == null || StringUtils.isEmpty(className)) {
            return;
        }
        final TestClassReport report = new TestClassReport();
        report.setClassName(className);
        report.setClassStatus(TestClassReport.CLASS_SKIPPED);
        report.setClassReason(reason);
        report.setCases(new ArrayList<TestCaseEntry>());
        RUN_REPORT.getClasses().add(report);
    }

    private static void finishCurrentClass(final String classStatus, final String classReason, final String failureMessage) {
        final TestClassReport current = CURRENT_CLASS.get();
        CURRENT_CLASS.set(null);
        if (current == null || RUN_REPORT == null) {
            return;
        }
        current.setClassStatus(classStatus);
        current.setClassReason(classReason);
        current.setFailureMessage(failureMessage);
        if (current.getCases() == null) {
            current.setCases(new ArrayList<TestCaseEntry>());
        }
        if (current.getCases().isEmpty() && TestClassReport.CLASS_SUCCESS.equals(classStatus)) {
            current.getCases().add(new TestCaseEntry("(runTest)", TestCaseEntry.STATUS_SUCCESS, null));
        }
        RUN_REPORT.getClasses().add(current);
        exportCurrentClassIfRequested(current);
    }

    /**
     * Writes the given class report to the file from {@link #REPORT_EXPORT_FILE_PROPERTY} (post-build subprocess).
     */
    public static void exportClassReport(final TestClassReport report) {
        exportCurrentClassIfRequested(report);
    }

    private static void exportCurrentClassIfRequested(final TestClassReport report) {
        if (report == null) {
            return;
        }
        final String path = System.getProperty(REPORT_EXPORT_FILE_PROPERTY);
        if (StringUtils.isEmpty(path)) {
            return;
        }
        try {
            final File file = new File(path);
            final File parent = file.getParentFile();
            if (parent != null && !parent.exists()) {
                parent.mkdirs();
            }
            IO.secureWrite(file, FlexiUtils.serializeToPrettyJson(report), SYNC.META_AND_DATA);
        } catch (Throwable e) {
            LogV3.warning("TestCaseReporter: could not export class report: " + e.getMessage());
        }
    }

    /**
     * Merges a class report JSON file produced by a post-build subprocess into the current run report.
     */
    public static void mergeExportedClassReport(final File exportFile) {
        if (RUN_REPORT == null || exportFile == null || !exportFile.isFile()) {
            return;
        }
        try {
            final String json = IO.readFileToString(exportFile);
            final TestClassReport imported = FlexiUtils.jsonToObject(json, new SimpleTypeRef<TestClassReport>(TestClassReport.class));
            if (imported != null && StringUtils.isNotEmpty(imported.getClassName())) {
                RUN_REPORT.getClasses().add(imported);
            }
        } catch (Throwable e) {
            LogV3.warning("TestCaseReporter: could not merge class report from " + exportFile + ": " + e.getMessage());
        }
    }

    public static File finishRunAndWriteReports() {
        if (RUN_REPORT == null) {
            return null;
        }
        RUN_REPORT.setFinishedAt(Time.systemIndependentCurrentJVMTimeMillis());
        File htmlFile = null;
        try {
            if (REPORT_DIR == null) {
                REPORT_DIR = resolveDefaultReportDir();
            }
            if (REPORT_DIR != null) {
                if (!REPORT_DIR.exists()) {
                    REPORT_DIR.mkdirs();
                }
                final File jsonFile = new File(REPORT_DIR, "test-report.json");
                IO.secureWrite(jsonFile, FlexiUtils.serializeToPrettyJson(RUN_REPORT), SYNC.META_AND_DATA);
                htmlFile = new File(REPORT_DIR, "test-report.html");
                IO.secureWrite(htmlFile, TestReportHtmlWriter.toHtml(RUN_REPORT), SYNC.META_AND_DATA);
                AWTest.logInfoAnyway("Test report: " + htmlFile.getAbsolutePath());
                openHtmlReportInBrowser(htmlFile);
            }
        } catch (Throwable e) {
            LogV3.warning("TestCaseReporter: could not write report: " + e.getMessage());
        }
        RUN_REPORT = null;
        CURRENT_CLASS.set(null);
        return htmlFile;
    }

    /**
     * Opens the post-build HTML report for {@code cacheKey} (e.g. {@code UpdateProviderNG_IDE}) if the file exists.
     */
    public static void openPostBuildReport(final String cacheKey) {
        openReportFile(PostBuildRunner.getReportHtmlFile(cacheKey));
    }

    /**
     * Opens the given HTML report with the system default handler (typically the browser). Skipped when
     * {@link #NO_OPEN_REPORT_PROPERTY} is {@code true}.
     */
    public static void openReportFile(final File htmlFile) {
        if (htmlFile == null || !htmlFile.isFile()) {
            return;
        }
        if ("true".equalsIgnoreCase(System.getProperty(NO_OPEN_REPORT_PROPERTY, "false"))) {
            return;
        }
        try {
            CrossSystem.openURL(htmlFile.toURI().toURL());
            AWTest.logInfoAnyway("Opened test report: " + htmlFile.getAbsolutePath());
        } catch (Throwable t) {
            LogV3.warning("TestCaseReporter: could not open HTML report: " + t.getMessage());
        }
    }

    private static void openHtmlReportInBrowser(final File htmlFile) {
        openReportFile(htmlFile);
    }

    private static File resolveDefaultReportDir() {
        File dir = Application.getResource("reports");
        if (dir == null) {
            dir = new File(System.getProperty("user.home", "."), ".appworktest/reports");
        }
        return dir;
    }

    /**
     * @param reportDirectory
     *            optional override; when null uses {@link Application#getResource(String)} {@code reports}
     */
    public static File getReportDirectory(final File reportDirectory) {
        return reportDirectory != null ? reportDirectory : resolveDefaultReportDir();
    }
}
