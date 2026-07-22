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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.StringUtils;

/**
 * Renders a {@link TestRunReport} as a standalone HTML document.
 */
public final class TestReportHtmlWriter {
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);

    private TestReportHtmlWriter() {
    }

    public static String toHtml(final TestRunReport report) {
        if (report == null) {
            return "<!DOCTYPE html><html><body><p>No report data.</p></body></html>";
        }
        int classesSuccess = 0;
        int classesFailed = 0;
        int classesSkipped = 0;
        int casesSuccess = 0;
        int casesSkipped = 0;
        int casesFailed = 0;
        final List<TestClassReport> classes = report.getClasses();
        if (classes != null) {
            for (final TestClassReport cls : classes) {
                if (TestClassReport.CLASS_SUCCESS.equals(cls.getClassStatus())) {
                    classesSuccess++;
                } else if (TestClassReport.CLASS_FAILED.equals(cls.getClassStatus())) {
                    classesFailed++;
                } else {
                    classesSkipped++;
                }
                final List<TestCaseEntry> cases = cls.getCases();
                if (cases != null) {
                    for (final TestCaseEntry c : cases) {
                        if (TestCaseEntry.STATUS_SUCCESS.equals(c.getStatus())) {
                            casesSuccess++;
                        } else if (TestCaseEntry.STATUS_SKIPPED.equals(c.getStatus())) {
                            casesSkipped++;
                        } else if (TestCaseEntry.STATUS_FAILED.equals(c.getStatus())) {
                            casesFailed++;
                        }
                    }
                }
            }
        }
        final StringBuilder sb = new StringBuilder(8192);
        sb.append("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
        sb.append("<meta charset=\"UTF-8\"/>\n");
        sb.append("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>\n");
        sb.append("<title>Test Report — ").append(escape(titleSuffix(report))).append("</title>\n");
        appendStyles(sb);
        sb.append("</head>\n<body>\n<div class=\"wrap\">\n");
        sb.append("<h1>Test Report</h1>\n");
        sb.append("<div class=\"meta\">");
        sb.append("Runner: <strong>").append(escape(report.getRunnerLabel())).append("</strong>");
        if (StringUtils.isNotEmpty(report.getBuildId())) {
            sb.append(" &middot; Build: <strong>").append(escape(report.getBuildId())).append("</strong>");
        }
        if (StringUtils.isNotEmpty(report.getProject())) {
            sb.append(" &middot; Project: <strong>").append(escape(report.getProject())).append("</strong>");
        }
        if (StringUtils.isNotEmpty(report.getSetup())) {
            sb.append(" &middot; Setup: <strong>").append(escape(report.getSetup())).append("</strong>");
        }
        sb.append(" &middot; Started: ").append(formatTime(report.getStartedAt()));
        sb.append(" &middot; Finished: ").append(formatTime(report.getFinishedAt()));
        sb.append("</div>\n");
        sb.append("<div class=\"summary\">\n");
        appendSummaryCard(sb, "Classes passed", classesSuccess, "ok");
        appendSummaryCard(sb, "Classes failed", classesFailed, "fail");
        appendSummaryCard(sb, "Classes skipped", classesSkipped, "skip");
        appendSummaryCard(sb, "Cases passed", casesSuccess, "ok");
        appendSummaryCard(sb, "Cases skipped", casesSkipped, "skip");
        appendSummaryCard(sb, "Cases failed", casesFailed, "fail");
        sb.append("</div>\n");
        sb.append("<table>\n<thead><tr><th>Test class</th><th>Status</th><th>Details</th></tr></thead>\n<tbody>\n");
        if (classes != null) {
            for (final TestClassReport cls : classes) {
                appendClassRow(sb, cls);
            }
        }
        sb.append("</tbody>\n</table>\n</div>\n</body>\n</html>\n");
        return sb.toString();
    }

    private static void appendSummaryCard(final StringBuilder sb, final String label, final int count, final String cssClass) {
        sb.append("<div class=\"card ").append(cssClass).append("\"><strong>").append(count).append("</strong>").append(escape(label)).append("</div>\n");
    }

    private static void appendClassRow(final StringBuilder sb, final TestClassReport cls) {
        sb.append("<tr><td class=\"class-name\">").append(escape(cls.getClassName())).append("</td>\n");
        sb.append("<td>").append(classStatusBadge(cls.getClassStatus())).append("</td>\n<td>");
        if (StringUtils.isNotEmpty(cls.getClassReason())) {
            sb.append("<div class=\"reason\">").append(escape(cls.getClassReason())).append("</div>");
        }
        if (StringUtils.isNotEmpty(cls.getFailureMessage())) {
            sb.append("<div class=\"reason\">").append(escape(cls.getFailureMessage())).append("</div>");
        }
        final List<TestCaseEntry> cases = cls.getCases();
        if (cases != null && !cases.isEmpty()) {
            sb.append("<ul class=\"subcases\">\n");
            for (final TestCaseEntry c : cases) {
                sb.append("<li>").append(caseStatusBadge(c.getStatus())).append(" ").append(escape(c.getName()));
                if (StringUtils.isNotEmpty(c.getReason())) {
                    sb.append(" <span class=\"reason\">— ").append(escape(c.getReason())).append("</span>");
                }
                sb.append("</li>\n");
            }
            sb.append("</ul>");
        }
        sb.append("</td></tr>\n");
    }

    private static void appendStyles(final StringBuilder sb) {
        sb.append("<style>\n");
        sb.append("body{font-family:Segoe UI,system-ui,sans-serif;margin:0;background:#f4f6f8;color:#1a1a1a;}\n");
        sb.append(".wrap{max-width:1100px;margin:0 auto;padding:24px;}\n");
        sb.append("h1{margin:0 0 8px;font-size:1.6rem;}\n");
        sb.append(".meta{color:#555;font-size:.9rem;margin-bottom:20px;}\n");
        sb.append(".summary{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:24px;}\n");
        sb.append(".card{background:#fff;border-radius:8px;padding:14px 18px;min-width:120px;box-shadow:0 1px 3px rgba(0,0,0,.08);}\n");
        sb.append(".card strong{display:block;font-size:1.4rem;}\n");
        sb.append(".ok strong{color:#1b7f3a;}.fail strong{color:#c62828;}.skip strong{color:#b8860b;}\n");
        sb.append("table{width:100%;border-collapse:collapse;background:#fff;border-radius:8px;overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,.08);margin-bottom:20px;}\n");
        sb.append("th,td{padding:10px 12px;text-align:left;border-bottom:1px solid #e8eaed;vertical-align:top;}\n");
        sb.append("th{background:#eef1f5;font-weight:600;font-size:.85rem;text-transform:uppercase;letter-spacing:.03em;}\n");
        sb.append("tr:last-child td{border-bottom:none;}\n");
        sb.append(".badge{display:inline-block;padding:2px 10px;border-radius:12px;font-size:.75rem;font-weight:600;}\n");
        sb.append(".badge-success{background:#d4edda;color:#155724;}\n");
        sb.append(".badge-skipped{background:#fff3cd;color:#856404;}\n");
        sb.append(".badge-failed{background:#f8d7da;color:#721c24;}\n");
        sb.append(".badge-maintenance{background:#e2e3e5;color:#383d41;}\n");
        sb.append(".class-name{font-family:Consolas,monospace;font-size:.9rem;}\n");
        sb.append(".reason{color:#666;font-size:.85rem;}\n");
        sb.append(".subcases{margin:8px 0 0 0;padding:0;list-style:none;}\n");
        sb.append(".subcases li{padding:4px 0;border-top:1px dashed #e0e0e0;}\n");
        sb.append(".subcases li:first-child{border-top:none;}\n");
        sb.append("</style>\n");
    }

    private static String classStatusBadge(final String status) {
        if (TestClassReport.CLASS_SUCCESS.equals(status)) {
            return "<span class=\"badge badge-success\">SUCCESS</span>";
        }
        if (TestClassReport.CLASS_FAILED.equals(status)) {
            return "<span class=\"badge badge-failed\">FAILED</span>";
        }
        if (TestClassReport.CLASS_MAINTENANCE.equals(status)) {
            return "<span class=\"badge badge-maintenance\">MAINTENANCE</span>";
        }
        return "<span class=\"badge badge-skipped\">SKIPPED</span>";
    }

    private static String caseStatusBadge(final String status) {
        if (TestCaseEntry.STATUS_SUCCESS.equals(status)) {
            return "<span class=\"badge badge-success\">OK</span>";
        }
        if (TestCaseEntry.STATUS_FAILED.equals(status)) {
            return "<span class=\"badge badge-failed\">FAIL</span>";
        }
        return "<span class=\"badge badge-skipped\">SKIP</span>";
    }

    private static String titleSuffix(final TestRunReport report) {
        if (StringUtils.isNotEmpty(report.getBuildId())) {
            return report.getBuildId();
        }
        if (StringUtils.isNotEmpty(report.getProject()) && StringUtils.isNotEmpty(report.getSetup())) {
            return report.getProject() + " / " + report.getSetup();
        }
        if (StringUtils.isNotEmpty(report.getProject())) {
            return report.getProject();
        }
        return report.getRunnerLabel();
    }

    private static String formatTime(final long millis) {
        if (millis <= 0) {
            return "—";
        }
        synchronized (DATE_FORMAT) {
            return DATE_FORMAT.format(new Date(millis));
        }
    }

    private static String escape(final String text) {
        if (text == null) {
            return "";
        }
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;");
    }
}
