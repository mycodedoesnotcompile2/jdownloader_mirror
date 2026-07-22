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

import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;

/**
 * One reported sub-test case inside a test class run.
 */
@StorableDoc("Single sub-test case result for HTML test reports.")
@StorableExample("{\"name\":\"LocalSystem->Owner\",\"status\":\"SUCCESS\",\"reason\":null}")
public class TestCaseEntry implements Storable {
    public static final String STATUS_SUCCESS = "SUCCESS";
    public static final String STATUS_SKIPPED = "SKIPPED";
    public static final String STATUS_FAILED  = "FAILED";

    private String name;
    private String status;
    private String reason;

    public TestCaseEntry() {
    }

    public TestCaseEntry(final String name, final String status, final String reason) {
        this.name = name;
        this.status = status;
        this.reason = reason;
    }

    public String getName() {
        return this.name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public String getStatus() {
        return this.status;
    }

    public void setStatus(final String status) {
        this.status = status;
    }

    public String getReason() {
        return this.reason;
    }

    public void setReason(final String reason) {
        this.reason = reason;
    }
}
