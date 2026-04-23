/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.storage.TypeRef;
import org.appwork.utils.LogCallback;

/**
 * Options for process launch used by {@link AdminExecuter#runAsAdmin(File, String[], ProcessOptions)} and
 * {@link AdminExecuter#runAsLocalSystem(File, String[], ProcessOptions)}. Also used by
 * {@link AdminExecuter#runAsAdmin(Class, String, TypeRef, ProcessOptions, Object...)} for method-invocation runs that return
 * {@link ResultWithCancel} (e.g. start server in background). Use {@link #builder()} or default instance {@link #DEFAULT}.
 * <p>
 * When the elevated helper starts a separate process (not running in the helper), it reports the PID via SingleInstance; the caller's
 * options are updated with {@link #setTaskPID(Integer)}. Each request carries a {@link #getRequestId() request ID} (UniqueAlltimeID); the
 * helper associates it with the started process. Call {@link #terminate()} anytime to terminate the remote process (sends the request ID
 * with a terminate command to the helper).
 * <p>
 * <b>keepRunning</b> (tasks/method invocations only): When true, the process is left running after the task or method completes (e.g. a
 * started server); return values are still returned. For CMD line invocation
 * ({@link AdminRunner#runCommand(java.io.File, List, ProcessOptions)}), keepRunning has no effect.
 * <p>
 * <b>waitFor</b>: Tasks/method invocations: we wait for the process to end; the return value is returned when the process has ended by
 * itself; exit code is also transmitted. CMD line: waitFor true = wait for process to end; false = only start the process and return
 * immediately (caller does not wait for the process to finish).
 * <p>
 * The options relevant for the helper (waitFor, keepRunning, requestId) are sent as a serialized object (JSON).
 */
public final class ProcessOptions {
    /** Default: wait for process to finish, no extra env, no log callback, no cancel callback, keepRunning false. */
    public static final ProcessOptions DEFAULT = new ProcessOptions(true, null, null, false);
    private final boolean              waitFor;
    private final Map<String, String>  env;
    private final LogCallback          logCallback;
    private final boolean              keepRunning;
    /** Set by the framework when the elevated process reports the started task/process PID. Mutable. */
    private Integer                    taskPID;
    /** Request ID (UniqueAlltimeID) sent with the request; set by the framework before sending. Used by {@link #terminate()}. */
    private String                     requestId;
    /** Set by the framework when PROCESS_STARTED is received; invoked by {@link #terminate()}. */
    private Runnable                   terminateAction;

    private ProcessOptions(boolean waitFor, Map<String, String> env, LogCallback logCallback, boolean keepRunning) {
        this.waitFor = waitFor;
        this.env = env;
        this.logCallback = logCallback;
        this.keepRunning = keepRunning;
    }

    /**
     * Tasks/method invocations: when true, we wait for the process to end; the return value is returned when the process has ended by
     * itself; exit code is also transmitted. CMD line: true = wait for process to end; false = only start the process, caller returns
     * immediately.
     */
    public boolean isWaitFor() {
        return waitFor;
    }

    /**
     * Tasks/method invocations: when true, the process is left running after the task or method completes (e.g. a started server); return
     * values are still returned. For CMD line invocation, keepRunning has no effect.
     */
    public boolean isKeepRunning() {
        return keepRunning;
    }

    /**
     * PID of the started task/process, set by the framework when the elevated helper reports PROCESS_STARTED. Null until set.
     */
    public Integer getTaskPID() {
        return taskPID;
    }

    /**
     * Set by the framework when the elevated helper reports the started process PID. Do not set from application code.
     */
    public void setTaskPID(Integer taskPID) {
        this.taskPID = taskPID;
    }

    /**
     * Returns true if the remote process (reported via {@link #getTaskPID()}) is still running. Uses the system process handler (e.g.
     * GetExitCodeProcess on Windows). Returns false if taskPID is null, or if the process check fails (e.g. process exited, or not
     * supported on this OS).
     */
    public boolean isAlive() {
        Integer pid = taskPID;
        if (pid == null || pid.intValue() <= 0) {
            return false;
        }
        try {
            return ProcessHandlerFactory.getProcessHandler().isProcessesAlive(new ProcessInfo(pid.intValue()));
        } catch (Throwable t) {
            org.appwork.loggingv3.LogV3.log(t);
            return false;
        }
    }

    /**
     * Request ID (UniqueAlltimeID) for this request. Set by the framework before sending to the helper; included in the payload so the
     * helper can associate the started process with this ID for {@link #terminate()}.
     */
    public String getRequestId() {
        return requestId;
    }

    /**
     * Set by the framework before sending the request. Do not set from application code.
     */
    public void setRequestId(String requestId) {
        this.requestId = requestId;
    }

    /**
     * Set by the framework when PROCESS_STARTED is received; used by {@link #terminate()}. Do not set from application code.
     */
    public void setTerminateAction(Runnable terminateAction) {
        this.terminateAction = terminateAction;
    }

    /**
     * Terminates the remote process started for this request. Sends the request ID with a terminate command to the helper. No-op if no
     * terminate action was set (e.g. process not yet started or already finished). Safe to call multiple times.
     */
    public void terminate() {
        if (terminateAction != null) {
            try {
                terminateAction.run();
            } catch (Throwable t) {
                org.appwork.loggingv3.LogV3.log(t);
            }
        }
    }

    /**
     * Optional environment variables for the launched process. Null or empty = use inheritted env.
     */
    public Map<String, String> getEnv() {
        return env == null ? null : Collections.unmodifiableMap(env);
    }

    /**
     * Optional callback to evaluate stdout/stderr lines from the remote process. Null = no callback.
     */
    public LogCallback getLogCallback() {
        return logCallback;
    }

    /**
     * Serializes the options relevant for the helper (waitFor, keepRunning, requestId) to JSON. The helper deserializes this and registers
     * the requestId for the started process so it can be terminated via a terminate command.
     */
    public String toHelperPayloadJson() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("waitFor", Boolean.valueOf(waitFor));
        map.put("keepRunning", Boolean.valueOf(keepRunning));
        if (requestId != null && requestId.length() > 0) {
            map.put("requestId", requestId);
        }
        return org.appwork.serializer.Deser.toString(map);
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {
        private boolean             waitFor     = true;
        private Map<String, String> env;
        private LogCallback         logCallback;
        private boolean             keepRunning = false;

        /**
         * Task/method: wait for process to end and then return value/exit code. CMD: true = wait for process end; false = start only,
         * return immediately.
         */
        public Builder waitFor(boolean waitFor) {
            this.waitFor = waitFor;
            return this;
        }

        public Builder env(Map<String, String> env) {
            this.env = env;
            return this;
        }

        /**
         * Optional callback to evaluate stdout/stderr lines from the remote process.
         */
        public Builder logCallback(LogCallback logCallback) {
            this.logCallback = logCallback;
            return this;
        }

        /**
         * When true, the started process (or child) is not terminated when the task/function completes; return value is still returned. For
         * pure process invocation, keepRunning has no effect.
         */
        public Builder keepRunning(boolean keepRunning) {
            this.keepRunning = keepRunning;
            return this;
        }

        public ProcessOptions build() {
            return new ProcessOptions(this.waitFor, this.env, this.logCallback, this.keepRunning);
        }
    }
}
