/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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

import org.appwork.loggingv3.LogV3;

/**
 * {@link ProcessInfo} that loads expensive fields on first access via optional loaders.
 * <ul>
 * <li><b>CommandLine</b> – {@link CommandLineLoader} (e.g. WMI or NtQueryInformationProcess on Windows). Loaded when
 * {@link #getCommandLine()} is first called.</li>
 * </ul>
 * Use this when listing many processes and only a few need command line: the list stays fast and the lookup runs only
 * when {@link #getCommandLine()} is used. Creation time and other fields are set by the handler at list time (e.g. for
 * process id).
 *
 * @author thomas
 * @date 08.03.2026
 */
public class LazyProcessInfo extends ProcessInfo {

    private final CommandLineLoader commandLineLoader;

    /**
     * Create a lazy process info that will use the given loader when {@link #getCommandLine()} is first called. If
     * loader is null, {@link #getCommandLine()} always returns null (no lazy load).
     *
     * @param pid
     *            process ID
     * @param commandLineLoader
     *            loader for command line (e.g. WMI or NtQueryInformationProcess on Windows), or null
     */
    public LazyProcessInfo(int pid, CommandLineLoader commandLineLoader) {
        super(pid);
        this.commandLineLoader = commandLineLoader;
    }

    @Override
    public String getCommandLine() {
        String existing = super.getCommandLine();
        if (existing != null) {
            return existing;
        }
        if (commandLineLoader == null) {
            return null;
        }
        try {
            String loaded = commandLineLoader.getCommandLine(getPid());
            if (loaded != null) {
                setCommandLine(loaded);
            }
            return loaded;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            LogV3.log(e);
            return null;
        } catch (Exception e) {
            LogV3.log(e);
            return null;
        }
    }
}
