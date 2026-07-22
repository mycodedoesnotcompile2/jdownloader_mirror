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

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;

/**
 * Full test run report (all test classes in one IDE or post-build execution).
 */
@StorableDoc("Full test run report written as JSON and rendered to HTML.")
@StorableExample("{\"runnerLabel\":\"PostBuild\",\"buildId\":\"UpdateProviderNG_UpdateServer\",\"project\":\"UpdateProviderNG\",\"setup\":\"UpdateServer\",\"startedAt\":1700000000000,\"finishedAt\":1700000001000,\"classes\":[{\"className\":\"org.example.MyTest\",\"classStatus\":\"SUCCESS\",\"cases\":[]}]}")
public class TestRunReport implements Storable {
    private String                runnerLabel;
    /** Build cache id from {@code -buildid=} (e.g. {@code UpdateProviderNG_UpdateServer}). */
    private String                buildId;
    /** Project name from {@code -projectinfo=} (first token). */
    private String                project;
    /** Setup parameter from {@code -projectinfo=} (remainder after first token). */
    private String                setup;
    private long                  startedAt;
    private long                  finishedAt;
    private List<TestClassReport> classes = new ArrayList<TestClassReport>();

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

    public List<TestClassReport> getClasses() {
        return this.classes;
    }

    public void setClasses(final List<TestClassReport> classes) {
        this.classes = classes != null ? classes : new ArrayList<TestClassReport>();
    }
}
