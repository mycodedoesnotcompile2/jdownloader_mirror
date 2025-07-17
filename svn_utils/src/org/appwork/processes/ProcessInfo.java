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

import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;

/**
 * @author thomas
 * @date 19.11.2024
 *
 */
public class ProcessInfo {
    private String commandLine;
    private long   creationTime;
    /**
     * this is not the pid. it is a internal id that is as uniqe as possible - NOTE: IT IS NOT ensured that this id is actually unique.
     */
    private String id;
    private int    pid;
    private int    parentPid = -1;

    public int getParentPid() {
        return parentPid;
    }

    public void setParentPid(int parentPid) {
        this.parentPid = parentPid;
    }

    /**
     *
     */
    public ProcessInfo() {
    }

    /**
     * @param pid2
     */
    public ProcessInfo(int pid) {
        this.pid = pid;
    }

    /**
     * like c:/programfiles/java/javaw.exe
     */
    private String executablePath;

    /**
     * @return the executablePath
     */
    public String getExecutablePath() {
        return executablePath;
    }

    /**
     * @param executablePath
     *            the executablePath to set
     */
    public void setExecutablePath(String executablePath) {
        this.executablePath = executablePath;
    }

    /**
     * like javaw.exe
     */
    private String executableName;

    /**
     * @return the executableName
     */
    public String getExecutableName() {
        return executableName;
    }

    /**
     * @param executableName
     *            the executableName to set
     */
    public void setExecutableName(String executableName) {
        this.executableName = executableName;
    }

    /**
     * @param intValue
     * @param string
     */
    public ProcessInfo(int pid, String commandline) {
        this.pid = pid;
        this.commandLine = commandline;
    }

    public String getCommandLine() {
        return commandLine;
    }

    public long getCreationTime() {
        return creationTime;
    }

    public String getId() {
        return id;
    }

    public int getPid() {
        return pid;
    }

    public void setCommandLine(String commandLine) {
        this.commandLine = commandLine;
    }

    public void setCreationTime(long creationTime) {
        this.creationTime = creationTime;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setPid(int pid) {
        this.pid = pid;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "Process PID:'" + getPid() + "' CMD:'" + getCommandLine() + "' Started: " + DateMapper.formatJsonDefault(getCreationTime());
    }

    /**
     * @param processInfo
     */
    public void fill(ProcessInfo processInfo) {
        this.id = processInfo.getId();
        this.commandLine = processInfo.getCommandLine();
        this.creationTime = processInfo.getCreationTime();
    }
}
