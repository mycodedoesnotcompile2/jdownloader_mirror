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
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

/**
 * Runs commands locally in the current process (elevated helper or LocalSystem). Implements {@link AdminRunnerApi} for API
 * compatibility. The helper handles RUN_CMD via {@link org.appwork.utils.processes.ProcessBuilderFactory} directly.
 */
public final class AdminRunnerLocal implements AdminRunnerApi {
    @Override
    public ProcessOutput runCommand(String[] command) throws Exception {
        return runCommand((File) null, command);
    }

    @Override
    public ProcessOutput runCommand(File workDir, String[] command) throws Exception {
        return runCommand(workDir, Arrays.asList(command));
    }

    @Override
    public ProcessOutput runCommand(List<String> command) throws Exception {
        return runCommand((File) null, command);
    }

    @Override
    public ProcessOutput runCommand(File workDir, List<String> command) throws Exception {
        if (command == null || command.isEmpty()) {
            throw new IllegalArgumentException("command must not be null or empty");
        }
        java.lang.ProcessBuilder pb = ProcessBuilderFactory.create(command);
        if (workDir != null && workDir.isDirectory()) {
            pb.directory(workDir);
        }
        try {
            return ProcessBuilderFactory.runCommand(pb);
        } catch (IOException e) {
            throw new Exception("runCommand failed: " + e.getMessage(), e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new Exception("runCommand interrupted", e);
        }
    }
}
