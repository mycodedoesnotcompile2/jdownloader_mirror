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
package org.appwork.utils.io.filechange;

import java.io.File;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.OSFamily;
import org.appwork.utils.os.CrossSystem.OperatingSystem;

/**
 * Fluent logger for file create/modify/delete events.
 * <p>
 * Abstract class, not an interface: AppWorkUtils is compiled as Java 6, and a Java 6 interface cannot hold {@link #getByOs()}.
 * </p>
 * Call {@code FileChangeLogObserver.getByOs().observe(file).logTo(logger)} and {@link #close()} when finished. A file target watches its
 * parent for that name (the file does not have to exist yet). {@link #observe(File)} on a directory watches only its direct children.
 * {@link #observeTree(File)} watches that directory and every subdirectory, and registers folders created later. The same log line is written
 * at most once every 5 seconds.
 */
public abstract class FileChangeLogObserver {
    private static final FileChangeLogObserver NOT_SUPPORTED = new NotSupportedObserver();
    /**
     * Java 7 WatchService implementation. Loaded only after the version gate so this class stays usable on Java 6.
     */
    private static final String                J7_IMPL       = "org.appwork.utils.io.filechange.J7FileChangeLogObserver";

    /**
     * Observer for this OS, or {@link NotSupportedObserver} when file-change notifications are not available.
     * <p>
     * Java: {@link JavaVersion#JVM_1_7} or newer ({@code java.nio.file.WatchService}). Older JVMs get {@link NotSupportedObserver}. The J7
     * class is loaded via {@link Class#forName(String)} so this type does not reference it.
     * </p>
     * <p>
     * Windows: Windows 2000 or newer. The JDK uses {@code ReadDirectoryChangesW} (present since Windows 2000; not on Windows 9x / NT). No
     * admin rights and no {@code SeBackupPrivilege}. The process only needs to be allowed to list the directory. Access denied is logged
     * and that path is skipped. Unknown Windows versions are accepted: a JVM that can run this code is not Windows NT.
     * </p>
     * <p>
     * Linux: same Java 7 gate, via inotify. Read/list access to the directory is enough. No extra capability. If inotify is disabled, the
     * observer logs that and further calls do nothing.
     * </p>
     * <p>
     * macOS, BSD and other systems: {@link NotSupportedObserver}. The JDK there polls instead of using an OS change journal.
     * </p>
     */
    /**
     * @return true when {@link #getByOs()} can use the Java 7 watch service on this OS
     */
    public static boolean isSupported() {
        if (!JavaVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
            return false;
        }
        final OSFamily family = CrossSystem.getOSFamily();
        if (family == OSFamily.WINDOWS) {
            final OperatingSystem os = CrossSystem.getOS();
            return os != OperatingSystem.WINDOWS_NT;
        } else {
            return family == OSFamily.LINUX;
        }
    }

    public static FileChangeLogObserver getByOs() {
        if (!FileChangeLogObserver.isSupported()) {
            return FileChangeLogObserver.NOT_SUPPORTED;
        }
        return FileChangeLogObserver.loadJ7();
    }

    private static FileChangeLogObserver loadJ7() {
        try {
            return (FileChangeLogObserver) Class.forName(FileChangeLogObserver.J7_IMPL).getConstructor(new Class[0]).newInstance(new Object[0]);
        } catch (final Throwable e) {
            LogV3.log(e);
            return FileChangeLogObserver.NOT_SUPPORTED;
        }
    }

    public abstract FileChangeLogObserver observe(File file);

    /**
     * Watches {@code directory} and its subfolders. A folder created later is registered as well. The standard watch service has no recursive
     * flag, so each directory is registered on its own.
     */
    public FileChangeLogObserver observeTree(final File directory) {
        return this.observeTree(directory, null);
    }

    /**
     * Same as {@link #observeTree(File)}. Each pattern is matched against the resolved log path of the changed file, not the absolute path.
     * {@code null} logs every change. Any matching pattern is enough. The tree is still registered completely, so a later file in a new
     * subfolder can match.
     */
    public abstract FileChangeLogObserver observeTree(File directory, Pattern[] patterns);

    public abstract FileChangeLogObserver logTo(LogInterface logger);

    /**
     * Closes watch handles for {@code directory} and everything under it until {@link #resumeTree(File)}. On Windows a directory watch
     * ({@code ReadDirectoryChangesW}) keeps a handle that blocks renaming or deleting that directory.
     */
    public void suspendTree(final File directory) {
    }

    /**
     * Registers {@code directory} again after {@link #suspendTree(File)}, when it still exists.
     */
    public void resumeTree(final File directory) {
    }

    public abstract void close();
}
