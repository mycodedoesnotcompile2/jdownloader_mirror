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
 * When the task threw an exception, {@link #getExceptionStackTrace()} is set so the client can re-throw a proper error message.
 */
public final class AdminTaskResultWrapper implements Serializable {
    private static final long serialVersionUID = 1L;

    private final Serializable returnValue;
    private final String       stdout;
    private final String       stderr;
    /** When non-null, the task failed with this stack trace; client should re-throw instead of returning getReturnValue(). */
    private final String       exceptionStackTrace;

    public AdminTaskResultWrapper(Serializable returnValue, String stdout, String stderr) {
        this(returnValue, stdout, stderr, null);
    }

    public AdminTaskResultWrapper(Serializable returnValue, String stdout, String stderr, String exceptionStackTrace) {
        this.returnValue = returnValue;
        this.stdout = stdout != null ? stdout : "";
        this.stderr = stderr != null ? stderr : "";
        this.exceptionStackTrace = exceptionStackTrace != null ? exceptionStackTrace : "";
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

    /** When non-empty, the task failed; message contains the exception stack trace for the caller to re-throw. */
    public String getExceptionStackTrace() {
        return exceptionStackTrace != null ? exceptionStackTrace : "";
    }

    /** True when the task threw an exception (wrapper was written by RunTaskAsSystemMain in catch block). */
    public boolean hasTaskFailure() {
        return exceptionStackTrace != null && exceptionStackTrace.length() > 0;
    }
}
