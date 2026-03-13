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

/**
 * Use {@link AdminExecuter#runAsAdmin(ElevatedTestTask, org.appwork.storage.TypeRef)}. The task is serialized and executed in the elevated helper process (or as
 * LocalSystem via {@link AdminExecuter#runAsLocalSystem(ElevatedTestTask, org.appwork.storage.TypeRef)}). The task already runs with elevated privileges; use
 * {@link org.appwork.utils.processes.ProcessBuilderFactory#runCommand} (or similar) directly if you need to run subprocess commands.
 * The task may return a {@link Serializable} result that is sent back to the client.
 */
public interface ElevatedTestTask extends java.io.Serializable {
    /**
     * Run the task in the elevated helper (or LocalSystem) process. This method already runs with elevated privileges.
     * Return a serializable result (or null) that will be sent back to the client.
     *
     * @return optional serializable result (sent back to client); may be null
     * @throws Exception
     *             if the task fails
     */
    java.io.Serializable run() throws Exception;
}
