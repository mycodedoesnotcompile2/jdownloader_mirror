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
 * Report for one test class execution (including optional sub-test cases).
 */
@StorableDoc("Aggregated result of one test class for HTML test reports.")
@StorableExample("{\"className\":\"org.example.MyTest\",\"classStatus\":\"SUCCESS\",\"classReason\":null,\"cases\":[{\"name\":\"case1\",\"status\":\"SUCCESS\",\"reason\":null}]}")
public class TestClassReport implements Storable {
    public static final String CLASS_SUCCESS    = "SUCCESS";
    public static final String CLASS_FAILED     = "FAILED";
    public static final String CLASS_SKIPPED    = "SKIPPED";
    public static final String CLASS_MAINTENANCE = "MAINTENANCE";

    private String              className;
    private String              classStatus;
    private String              classReason;
    private String              failureMessage;
    private List<TestCaseEntry> cases = new ArrayList<TestCaseEntry>();

    public TestClassReport() {
    }

    public String getClassName() {
        return this.className;
    }

    public void setClassName(final String className) {
        this.className = className;
    }

    public String getClassStatus() {
        return this.classStatus;
    }

    public void setClassStatus(final String classStatus) {
        this.classStatus = classStatus;
    }

    public String getClassReason() {
        return this.classReason;
    }

    public void setClassReason(final String classReason) {
        this.classReason = classReason;
    }

    public String getFailureMessage() {
        return this.failureMessage;
    }

    public void setFailureMessage(final String failureMessage) {
        this.failureMessage = failureMessage;
    }

    public List<TestCaseEntry> getCases() {
        return this.cases;
    }

    public void setCases(final List<TestCaseEntry> cases) {
        this.cases = cases != null ? cases : new ArrayList<TestCaseEntry>();
    }
}
