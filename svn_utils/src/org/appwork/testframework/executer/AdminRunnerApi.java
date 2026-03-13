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

import java.io.File;
import java.util.List;

import org.appwork.utils.processes.ProcessOutput;

/**
 * API for running commands (either in the elevated helper via IPC, or locally in the same process). Implemented by {@link AdminRunner}
 * (client, sends to helper) and {@link AdminRunnerLocal} (helper, runs locally).
 */
public interface AdminRunnerApi {
    /**
     * Run a command (no working directory).
     */
    ProcessOutput runCommand(String[] command) throws Exception;

    /**
     * Run a command with the given working directory.
     */
    ProcessOutput runCommand(File workDir, String[] command) throws Exception;

    /**
     * Run a command (no working directory).
     */
    ProcessOutput runCommand(List<String> command) throws Exception;

    /**
     * Run a command with the given working directory.
     */
    ProcessOutput runCommand(File workDir, List<String> command) throws Exception;
}
