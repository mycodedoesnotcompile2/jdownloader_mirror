/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

/**
 * Callback to evaluate stdout/stderr lines from a remote process started via
 * {@link AdminExecuter#runAsAdmin(File, String[], ProcessOptions)} or
 * {@link AdminExecuter#runAsLocalSystem(File, String[], ProcessOptions)}. Set via
 * {@link ProcessOptions.Builder#logCallback(LogCallback)}.
 */
public interface LogCallback {

    /**
     * Called for each non-empty line from the process stdout.
     *
     * @param line
     *            one line (may include trailing whitespace, no newline)
     */
    void onStdOut(String line);

    /**
     * Called for each non-empty line from the process stderr.
     *
     * @param line
     *            one line (may include trailing whitespace, no newline)
     */
    void onStdErr(String line);
}
