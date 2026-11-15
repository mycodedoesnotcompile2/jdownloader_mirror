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
package org.appwork.testframework;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;

/**
 * Full test run report (all test classes in one IDE or post-build execution).
 */
@StorableDoc("Full test run report written as JSON and rendered to HTML.")
@StorableExample("{\"runnerLabel\":\"PostBuild\",\"buildId\":\"UpdateProviderNG_UpdateServer\",\"project\":\"UpdateProviderNG\",\"setup\":\"UpdateServer\",\"startedAt\":1700000000000,\"finishedAt\":1700000001000,\"localLinks\":[{\"label\":\"Dist\",\"path\":\"C:\\\\workspace\\\\UpdateProviderNG\\\\dist\"},{\"label\":\"Project\",\"path\":\"C:\\\\workspace\\\\UpdateProviderNG\"}],\"classes\":[{\"className\":\"org.example.MyTest\",\"classStatus\":\"SUCCESS\",\"cases\":[]}]}")
public class TestRunReport implements Storable {
    private String                     runnerLabel;
    /** Build cache id from {@code -buildid=} (e.g. {@code UpdateProviderNG_UpdateServer}). */
    private String                     buildId;
    /** Project name from {@code -projectinfo=} (first token). */
    private String                     project;
    /** Setup parameter from {@code -projectinfo=} (remainder after first token). */
    private String                     setup;
    private long                       startedAt;
    private long                       finishedAt;
    /** Quick links to Dist, project, build archive, workspace, etc. (file:// in HTML). */
    private List<TestReportLocalLink>  localLinks = new ArrayList<TestReportLocalLink>();
    private List<TestClassReport>      classes    = new ArrayList<TestClassReport>();

    public TestRunReport() {
    }

    public String getRunnerLabel() {
        return this.runnerLabel;
    }

    public void setRunnerLabel(final String runnerLabel) {
        this.runnerLabel = runnerLabel;
    }

    public String getBuildId() {
        return this.buildId;
    }

    public void setBuildId(final String buildId) {
        this.buildId = buildId;
    }

    public String getProject() {
        return this.project;
    }

    public void setProject(final String project) {
        this.project = project;
    }

    public String getSetup() {
        return this.setup;
    }

    public void setSetup(final String setup) {
        this.setup = setup;
    }

    public long getStartedAt() {
        return this.startedAt;
    }

    public void setStartedAt(final long startedAt) {
        this.startedAt = startedAt;
    }

    public long getFinishedAt() {
        return this.finishedAt;
    }

    public void setFinishedAt(final long finishedAt) {
        this.finishedAt = finishedAt;
    }

    public List<TestReportLocalLink> getLocalLinks() {
        return this.localLinks;
    }

    public void setLocalLinks(final List<TestReportLocalLink> localLinks) {
        this.localLinks = localLinks != null ? localLinks : new ArrayList<TestReportLocalLink>();
    }

    public List<TestClassReport> getClasses() {
        return this.classes;
    }

    public void setClasses(final List<TestClassReport> classes) {
        this.classes = classes != null ? classes : new ArrayList<TestClassReport>();
    }
}
