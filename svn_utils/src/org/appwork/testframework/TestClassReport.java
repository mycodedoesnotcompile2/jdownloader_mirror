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
