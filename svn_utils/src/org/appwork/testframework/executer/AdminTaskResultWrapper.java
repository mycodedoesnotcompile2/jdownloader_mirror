/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 * ====================================================================================================================================================
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import java.io.Serializable;

/**
 * Wrapper sent back from the elevated helper with the task's return value plus any stdout/stderr produced during {@link ElevatedTestTask#run}.
 * The client forwards stdout/stderr to the calling process's System.out/System.err and returns only the return value.
 */
public final class AdminTaskResultWrapper implements Serializable {
    private static final long serialVersionUID = 1L;

    private final Serializable returnValue;
    private final String       stdout;
    private final String       stderr;

    public AdminTaskResultWrapper(Serializable returnValue, String stdout, String stderr) {
        this.returnValue = returnValue;
        this.stdout = stdout != null ? stdout : "";
        this.stderr = stderr != null ? stderr : "";
    }

    public Serializable getReturnValue() {
        return returnValue;
    }

    public String getStdout() {
        return stdout;
    }

    public String getStderr() {
        return stderr;
    }
}
