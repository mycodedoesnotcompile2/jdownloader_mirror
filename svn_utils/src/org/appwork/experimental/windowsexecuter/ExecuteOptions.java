/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.experimental.windowsexecuter;

import java.io.File;
import java.util.Collections;
import java.util.Map;

import org.appwork.utils.LogCallback;
import org.appwork.utils.os.WindowsUtils;

/**
 * Options for process execution used by {@link WindowsExecuter#runAsNonElevatedUser(ExecuteOptions)}.
 * showWindow and noErrorUI are for use when integrating with elevation (e.g. {@link WindowsUtils#startElevatedProcess}).
 * Use {@link #builder()} to create instances.
 */
public final class ExecuteOptions {

    private final String[]           cmd;
    private final File               workingDir;
    private final boolean            waitFor;
    private final Map<String, String> env;
    /** When true, show the process window (e.g. elevated process via {@link WindowsUtils#startElevatedProcess}). */
    private final boolean            showWindow;
    /** When true, suppress error message boxes (ShellExecuteEx SEE_MASK_FLAG_NO_UI). Used by {@link WindowsUtils#startElevatedProcess}. */
    private final boolean            noErrorUI;
    /** Optional callback for line-by-line stdout/stderr. */
    private final LogCallback        logCallback;
    /** Optional SID to run the process under (e.g. active console user). When null, run with lowest privilege. */
    private final String             sid;
    /** When true, run process in active console user session (no SID passed). Used by runViaWindowsScheduler fallback when requested SID matches active console. */
    private final boolean           runInActiveSession;
    /**
     * WTS session id as decimal string (e.g. {@code "3"}), serializable for IPC. When the caller is LocalSystem,
     * {@link WindowsExecuter#runAsNonElevatedUser} requires this (no active-console auto-pick). Otherwise, when set, the user token is resolved via
     * {@link WindowsUtils#getUserTokenForSessionId(int)} instead of falling back to the active physical console — needed for RDP vs console.
     */
    private final String            wtsSessionId;

    private ExecuteOptions(String[] cmd, File workingDir, boolean waitFor, Map<String, String> env, boolean showWindow, boolean noErrorUI, LogCallback logCallback, String sid, boolean runInActiveSession, String wtsSessionId) {
        this.cmd = cmd;
        this.workingDir = workingDir;
        this.waitFor = waitFor;
        this.env = env;
        this.showWindow = showWindow;
        this.noErrorUI = noErrorUI;
        this.logCallback = logCallback;
        this.sid = sid;
        this.runInActiveSession = runInActiveSession;
        this.wtsSessionId = wtsSessionId;
    }

    public String[] getCmd() {
        return cmd;
    }

    public File getWorkingDir() {
        return workingDir;
    }

    public boolean isWaitFor() {
        return waitFor;
    }

    /**
     * Optional environment for the process. Null or empty = inherit from parent.
     */
    public Map<String, String> getEnv() {
        return env == null ? null : Collections.unmodifiableMap(env);
    }

    /**
     * Whether to show the process window (e.g. for {@link WindowsUtils#startElevatedProcess}). Default true.
     */
    public boolean isShowWindow() {
        return showWindow;
    }

    /**
     * When true, suppress error message boxes (SEE_MASK_FLAG_NO_UI). Used by {@link WindowsUtils#startElevatedProcess}. Default false.
     */
    public boolean isNoErrorUI() {
        return noErrorUI;
    }

    /**
     * Optional callback to receive stdout/stderr line by line. Used by runAsNonElevatedUser when reading pipes.
     */
    public LogCallback getLogCallback() {
        return logCallback;
    }

    /**
     * Optional SID (e.g. S-1-5-21-...) to run the process under that user. When null or empty, process runs with lowest privilege (current user / medium integrity).
     */
    public String getSid() {
        return sid;
    }

    /**
     * When true, run process in active console user session. Set by runViaWindowsScheduler fallback when requested SID equals active console; no SID is passed in that case.
     */
    public boolean isRunInActiveSession() {
        return runInActiveSession;
    }

    /**
     * Decimal string of the target WTS session id (e.g. from {@link WindowsUtils#getCurrentProcessSessionId()}). Required when the caller is
     * LocalSystem for {@link WindowsExecuter#runAsNonElevatedUser}; otherwise null or empty selects the active physical console session token
     * when no explicit session is needed.
     */
    public String getWtsSessionId() {
        return wtsSessionId;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {
        private String[]            cmd;
        private File                workingDir;
        private boolean             waitFor = true;
        private Map<String, String> env;
        private boolean             showWindow = true;
        private boolean             noErrorUI = false;
        private LogCallback         logCallback;
        private String             sid;
        private boolean            runInActiveSession = false;
        private String             wtsSessionId;

        public Builder cmd(String... cmd) {
            this.cmd = cmd;
            return this;
        }

        public Builder workingDir(File workingDir) {
            this.workingDir = workingDir;
            return this;
        }

        public Builder workingDir(String workingDirPath) {
            this.workingDir = workingDirPath != null ? new File(workingDirPath) : null;
            return this;
        }

        public Builder waitFor(boolean waitFor) {
            this.waitFor = waitFor;
            return this;
        }

        public Builder env(Map<String, String> env) {
            this.env = env;
            return this;
        }

        /**
         * Whether to show the process window (e.g. for UAC-started process). Default true.
         */
        public Builder showWindow(boolean showWindow) {
            this.showWindow = showWindow;
            return this;
        }

        /**
         * When true, suppress error message boxes (SEE_MASK_FLAG_NO_UI). Default false.
         */
        public Builder noErrorUI(boolean noErrorUI) {
            this.noErrorUI = noErrorUI;
            return this;
        }

        /**
         * Optional callback to receive stdout/stderr line by line.
         */
        public Builder logCallback(LogCallback logCallback) {
            this.logCallback = logCallback;
            return this;
        }

        /**
         * Optional SID to run the process under that user (e.g. active console user). When null, run with lowest privilege.
         */
        public Builder sid(String sid) {
            this.sid = sid;
            return this;
        }

        /**
         * When true, run process in active console user session. Used by runViaWindowsScheduler fallback when requested SID matches active console.
         */
        public Builder runInActiveSession(boolean runInActiveSession) {
            this.runInActiveSession = runInActiveSession;
            return this;
        }

        /**
         * Target WTS session id as decimal string (serializable). Mandatory for LocalSystem + {@link WindowsExecuter#runAsNonElevatedUser}; also
         * use when the interactive session is not the physical console (e.g. RDP).
         */
        public Builder wtsSessionId(String wtsSessionId) {
            this.wtsSessionId = wtsSessionId;
            return this;
        }

        public ExecuteOptions build() {
            if (cmd == null || cmd.length == 0) {
                throw new IllegalArgumentException("cmd cannot be null or empty");
            }
            return new ExecuteOptions(cmd, workingDir, waitFor, env, showWindow, noErrorUI, logCallback, sid, runInActiveSession, wtsSessionId);
        }
    }
}
