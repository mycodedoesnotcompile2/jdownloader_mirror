/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.processes;

import java.io.IOException;
import java.util.List;

import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.NotSupportedException;

/**
 * @author thomas
 * @date 19.11.2024
 *
 */
public interface ProcessHandler {
    /**
     * path = null will return all processes
     *
     * @param pathStartsWith
     * @return
     * @throws NotSupportedException
     * @throws IOException
     * @throws InterruptedException
     */
    List<ProcessInfo> listByPath(String pathStartsWith) throws NotSupportedException, IOException, InterruptedException;

    List<ProcessInfo> listByPids(int... pids) throws NotSupportedException, IOException, InterruptedException;

    /**
     * Forced Kill Returns false if the process is not running any more.
     *
     * @param exitCode
     *            TODO
     * @param instance
     *
     * @throws InterruptedException
     */
    boolean terminateForced(ProcessInfo process, int exitCode) throws NotSupportedException, IOException;

    /**
     * Try to soft kill. The process may ignore this. Returns false if the process is not running any more.
     *
     * @param found
     * @throws InterruptedException
     */
    boolean terminateRequest(ProcessInfo process) throws NotSupportedException, IOException;

    List<ProcessInfo> listByProcessInfo(ProcessInfo... processes) throws NotSupportedException, IOException, InterruptedException;

    /**
     * tries a soft close, waits the timeout and force kills remaining processes<br>
     * Returns false if at least one process is not running any more
     *
     * @param exitCode
     *            TODO
     * @param processes
     * @param timeout
     *
     * @throws InterruptedException
     */
    boolean terminateForcedAfterRequest(TimeSpan waitForSoftClose, int exitCode, ProcessInfo... processes) throws IOException, NotSupportedException, InterruptedException;

    /**
     * @param instance
     * @param timeSpan
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    List<ProcessInfo> waitForExit(TimeSpan maxWait, ProcessInfo... processes) throws IOException, NotSupportedException, InterruptedException;
}
