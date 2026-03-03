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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.testframework;

import java.util.HashMap;
import java.util.Map;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;

/**
 * Persisted status for a single post-build test. Used to sort tests by last failure (failures first) and to store additional run history.
 * Stored as JSON under user.home/.appworktest/postbuild/{baseHash}/.
 */
@StorableDoc("Status object for one post-build test class. Persisted per test and per BASE-hash.")
@StorableExample("{\"lastFailureTimestamp\":1700000000000,\"lastRunTimestamp\":1700000001000,\"lastSuccessTimestamp\":null,\"runCount\":5,\"failureCount\":1}")
public class PostBuildTestStatus implements Storable {
    /**
     * Default constructor for Storable deserialization.
     */
    public PostBuildTestStatus() {
    }

    /** Timestamp (ms) of the last run of this test. */
    private long                lastRunTimestamp;
    /** Timestamp (ms) of the last failure; null if last run was success or never failed. */
    private Long                lastFailureTimestamp;
    /** Timestamp (ms) of the last success; null if never passed. */
    private Long                lastSuccessTimestamp;
    /** Exit code of the last run; null if not recorded. */
    private Integer             lastExitCode;
    /** Short error message of the last failure; null if not recorded. */
    private String              lastErrorMessage;
    /** Total number of runs recorded. */
    private int                 runCount;
    /** Total number of failures recorded. */
    private int                 failureCount;
    /**
     * SHA256 hash of the test's dependency set (required classes/resources) as identified by ASM (ClassCollector). If unchanged since last
     * run and last run passed, the test may be skipped.
     */
    private String              resourceHash;
    /**
     * Full map of dependency class name to hash from last run. Used to log which classes changed when test is re-run.
     */
    private Map<String, String> resourceHashes;

    public long getLastRunTimestamp() {
        return lastRunTimestamp;
    }

    public void setLastRunTimestamp(long lastRunTimestamp) {
        this.lastRunTimestamp = lastRunTimestamp;
    }

    public Long getLastFailureTimestamp() {
        return lastFailureTimestamp;
    }

    public void setLastFailureTimestamp(Long lastFailureTimestamp) {
        this.lastFailureTimestamp = lastFailureTimestamp;
    }

    public Long getLastSuccessTimestamp() {
        return lastSuccessTimestamp;
    }

    public void setLastSuccessTimestamp(Long lastSuccessTimestamp) {
        this.lastSuccessTimestamp = lastSuccessTimestamp;
    }

    public Integer getLastExitCode() {
        return lastExitCode;
    }

    public void setLastExitCode(Integer lastExitCode) {
        this.lastExitCode = lastExitCode;
    }

    public String getLastErrorMessage() {
        return lastErrorMessage;
    }

    public void setLastErrorMessage(String lastErrorMessage) {
        this.lastErrorMessage = lastErrorMessage;
    }

    public int getRunCount() {
        return runCount;
    }

    public void setRunCount(int runCount) {
        this.runCount = runCount;
    }

    public int getFailureCount() {
        return failureCount;
    }

    public void setFailureCount(int failureCount) {
        this.failureCount = failureCount;
    }

    public String getResourceHash() {
        return resourceHash;
    }

    public void setResourceHash(String resourceHash) {
        this.resourceHash = resourceHash;
    }

    public Map<String, String> getResourceHashes() {
        return resourceHashes == null ? null : new HashMap<String, String>(resourceHashes);
    }

    public void setResourceHashes(Map<String, String> resourceHashes) {
        this.resourceHashes = resourceHashes == null ? null : new HashMap<String, String>(resourceHashes);
    }
}
