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
import java.io.IOException;
import java.nio.file.ClosedWatchServiceException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.CrossSystem;

/**
 * Java 7 {@link WatchService} observer. Do not reference this class from code that must load on Java 6. {@link FileChangeLogObserver}
 * loads it with {@link Class#forName(String)} after the version check.
 * <p>
 * Windows: {@code ReadDirectoryChangesW}. The JDK opens the directory with {@code FILE_FLAG_BACKUP_SEMANTICS}; that flag is not
 * {@code SeBackupPrivilege} and does not elevate. Linux: inotify. One registration covers one directory. {@link #observeTree(File)} registers
 * each subdirectory and adds folders that are created later.
 * </p>
 */
public class J7FileChangeLogObserver extends FileChangeLogObserver implements Runnable {
    /** The same log line is written at most once in this interval. */
    private static final long SAME_LINE_MILLIS = 5000L;

    private static final class Target {
        private final File    file;
        private final boolean directory;
        private final boolean recursive;
        private final Pattern[] patterns;

        private Target(final File file, final boolean directory, final boolean recursive, final Pattern[] patterns) {
            this.file = file;
            this.directory = directory;
            this.recursive = recursive;
            this.patterns = patterns;
        }
    }

    private final Object         lock        = new Object();
    private final List<Target>   targets     = new ArrayList<Target>();
    private final Map<WatchKey, File> keyToDir = new HashMap<WatchKey, File>();
    private final Set<String>    watchedDirs = new HashSet<String>();
    /** Directories whose watch handles were dropped so this process can rename or delete them. */
    private final Set<String>    suspended   = new HashSet<String>();
    private String                lastLine;
    private long                  lastLineAt;
    /** Last deleted watched folder. MOD events for files inside it are the delete itself. */
    private File                  lastDeletedDir;
    private volatile LogInterface logger;
    private volatile boolean     closed;
    private Thread               thread;
    private WatchService         watcher;

    /**
     * Path text written to the log. The default is the absolute path. Subclasses can return another form, for example a logical path.
     */
    protected String resolvePath(final File file) {
        return file.getAbsolutePath();
    }

    @Override
    public FileChangeLogObserver observe(final File file) {
        if (file == null) {
            return this;
        }
        final File absolute = file.getAbsoluteFile();
        return this.addTarget(absolute, absolute.isDirectory(), false, null);
    }

    @Override
    public FileChangeLogObserver observeTree(final File directory, final Pattern[] patterns) {
        if (directory == null) {
            return this;
        }
        return this.addTarget(directory.getAbsoluteFile(), true, true, patterns);
    }

    private FileChangeLogObserver addTarget(final File absolute, final boolean directory, final boolean recursive, final Pattern[] patterns) {
        final List<String> problems = new ArrayList<String>();
        synchronized (this.lock) {
            if (this.closed) {
                return this;
            }
            final Target target = new Target(absolute, directory, recursive, patterns);
            this.targets.add(target);
            this.registerLocked(target, problems);
            this.ensureStartedLocked();
        }
        this.logAll(true, problems);
        return this;
    }

    @Override
    public FileChangeLogObserver logTo(final LogInterface logger) {
        if (logger == null) {
            return this;
        }
        synchronized (this.lock) {
            if (this.closed) {
                return this;
            }
            this.logger = logger;
            this.ensureStartedLocked();
        }
        return this;
    }

    @Override
    public void suspendTree(final File directory) {
        if (directory == null) {
            return;
        }
        final List<WatchKey> cancelled = new ArrayList<WatchKey>();
        synchronized (this.lock) {
            if (this.closed) {
                return;
            }
            final String path = directory.getAbsolutePath();
            this.suspended.add(path);
            final Iterator<Map.Entry<WatchKey, File>> it = this.keyToDir.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<WatchKey, File> entry = it.next();
                final File watched = entry.getValue();
                if (watched == null) {
                    continue;
                }
                final String watchedPath = watched.getAbsolutePath();
                if (!J7FileChangeLogObserver.samePath(watchedPath, path) && !J7FileChangeLogObserver.isUnder(directory, watched)) {
                    continue;
                }
                it.remove();
                this.watchedDirs.remove(watchedPath);
                final WatchKey key = entry.getKey();
                // cancel() only queues the close. The watch thread drops the directory handle afterwards.
                key.cancel();
                cancelled.add(key);
            }
        }
        final long deadline = System.currentTimeMillis() + 2000L;
        for (int i = 0; i < cancelled.size(); i++) {
            final WatchKey key = cancelled.get(i);
            while (key.isValid() && System.currentTimeMillis() < deadline) {
                try {
                    Thread.sleep(20L);
                } catch (final InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }
        if (cancelled.size() > 0) {
            this.log(false, "Released directory watch on " + this.resolvePath(directory) + " so this process does not block renaming it");
        }
    }

    @Override
    public void resumeTree(final File directory) {
        if (directory == null) {
            return;
        }
        final List<String> problems = new ArrayList<String>();
        synchronized (this.lock) {
            final String path = directory.getAbsolutePath();
            final Iterator<String> it = this.suspended.iterator();
            while (it.hasNext()) {
                if (J7FileChangeLogObserver.samePath(it.next(), path)) {
                    it.remove();
                }
            }
            if (!this.closed && directory.isDirectory() && this.recursiveRoot(directory) != null) {
                this.registerTreeLocked(directory, problems);
            }
        }
        this.logAll(true, problems);
    }

    @Override
    public void close() {
        final WatchService toClose;
        final Thread toInterrupt;
        synchronized (this.lock) {
            this.closed = true;
            toClose = this.watcher;
            this.watcher = null;
            toInterrupt = this.thread;
        }
        if (toClose != null) {
            try {
                toClose.close();
            } catch (final IOException ignore) {
            }
        }
        if (toInterrupt != null) {
            toInterrupt.interrupt();
        }
    }

    @Override
    public void run() {
        final List<String> problems = new ArrayList<String>();
        WatchService local = null;
        try {
            local = FileSystems.getDefault().newWatchService();
        } catch (final IOException e) {
            // inotify disabled, or the OS refused the watch. Further observe/logTo calls do nothing.
            this.log(true, "File change watch is not available: " + e.getMessage());
            synchronized (this.lock) {
                this.closed = true;
                this.watcher = null;
            }
            return;
        }
        synchronized (this.lock) {
            if (this.closed) {
                this.closeQuiet(local);
                return;
            }
            this.watcher = local;
            for (int i = 0; i < this.targets.size(); i++) {
                this.registerLocked(this.targets.get(i), problems);
            }
        }
        this.logAll(true, problems);
        try {
            while (!this.closed) {
                final WatchKey key;
                try {
                    key = local.take();
                } catch (final InterruptedException e) {
                    return;
                } catch (final ClosedWatchServiceException e) {
                    return;
                }
                final List<String> lines = new ArrayList<String>();
                final List<String> warnings = new ArrayList<String>();
                synchronized (this.lock) {
                    if (this.closed) {
                        break;
                    }
                    final File dir = this.keyToDir.get(key);
                    final List<WatchEvent<?>> events = key.pollEvents();
                    for (int i = 0; i < events.size(); i++) {
                        this.collect(dir, events.get(i), lines, warnings);
                    }
                    if (!key.reset()) {
                        final File gone = this.keyToDir.remove(key);
                        if (gone != null) {
                            this.watchedDirs.remove(gone.getAbsolutePath());
                        }
                        warnings.add("Stopped watching a directory because it is no longer available");
                    }
                }
                this.logAll(false, lines);
                this.logAll(true, warnings);
            }
        } finally {
            synchronized (this.lock) {
                if (this.watcher == local) {
                    this.watcher = null;
                }
            }
            this.closeQuiet(local);
        }
    }

    private void ensureStartedLocked() {
        if (this.closed || this.thread != null || this.logger == null || this.targets.isEmpty()) {
            return;
        }
        this.thread = new Thread(this, "FileChangeLogObserver");
        this.thread.setDaemon(true);
        this.thread.start();
    }

    private void registerLocked(final Target target, final List<String> problems) {
        if (this.watcher == null) {
            return;
        }
        final File absolute = target.file;
        // A file, or a tree root that does not exist yet, is noticed via its parent directory.
        final File dir = target.directory ? absolute : absolute.getParentFile();
        if (target.recursive && (dir == null || !dir.isDirectory())) {
            final File parent = absolute.getParentFile();
            if (parent != null && parent.isDirectory()) {
                this.registerDirLocked(parent, problems);
                return;
            }
        }
        if (dir == null || !dir.isDirectory()) {
            problems.add("Cannot watch " + this.resolvePath(absolute) + ". Directory does not exist: " + (dir == null ? "null" : this.resolvePath(dir)));
            return;
        }
        if (target.recursive) {
            this.registerTreeLocked(dir, problems);
        } else {
            this.registerDirLocked(dir, problems);
        }
    }

    private void registerTreeLocked(final File root, final List<String> problems) {
        final HashSet<String> seen = new HashSet<String>();
        seen.add(this.canonicalId(root));
        this.registerDirLocked(root, problems);
        this.registerChildrenLocked(root, problems, seen);
    }

    private void registerChildrenLocked(final File dir, final List<String> problems, final Set<String> seen) {
        final File[] children = dir.listFiles();
        if (children == null) {
            return;
        }
        for (int i = 0; i < children.length; i++) {
            final File child = children[i];
            if (!child.isDirectory()) {
                continue;
            }
            // Canonical path stops a symlink that points back at an ancestor.
            if (!seen.add(this.canonicalId(child))) {
                continue;
            }
            this.registerDirLocked(child, problems);
            this.registerChildrenLocked(child, problems, seen);
        }
    }

    private void registerDirLocked(final File dir, final List<String> problems) {
        if (this.watcher == null || dir == null || !dir.isDirectory()) {
            return;
        }
        final String dirPath = dir.getAbsolutePath();
        final Iterator<String> suspendedRoots = this.suspended.iterator();
        while (suspendedRoots.hasNext()) {
            final String root = suspendedRoots.next();
            if (J7FileChangeLogObserver.samePath(dirPath, root) || J7FileChangeLogObserver.isUnder(new File(root), dir)) {
                return;
            }
        }
        if (!this.watchedDirs.add(dirPath)) {
            return;
        }
        try {
            final WatchKey key = dir.toPath().register(this.watcher, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_DELETE);
            this.keyToDir.put(key, dir);
        } catch (final IOException e) {
            this.watchedDirs.remove(dirPath);
            // AccessDeniedException: the process cannot list this directory. Do not elevate.
            problems.add("Cannot watch " + this.resolvePath(dir) + ": " + e.getMessage());
        }
    }

    private String canonicalId(final File file) {
        try {
            return file.getCanonicalPath();
        } catch (final IOException e) {
            return file.getAbsolutePath();
        }
    }

    private void extendTreeLocked(final File dir, final List<String> warnings) {
        if (!dir.isDirectory() || this.recursiveRoot(dir) == null || this.watchedDirs.contains(dir.getAbsolutePath())) {
            return;
        }
        final List<String> problems = new ArrayList<String>();
        final HashSet<String> seen = new HashSet<String>();
        seen.add(this.canonicalId(dir));
        this.registerDirLocked(dir, problems);
        this.registerChildrenLocked(dir, problems, seen);
        for (int i = 0; i < problems.size(); i++) {
            warnings.add(problems.get(i));
        }
    }

    private void collect(final File dir, final WatchEvent<?> event, final List<String> lines, final List<String> warnings) {
        if (dir == null) {
            return;
        }
        final WatchEvent.Kind<?> kind = event.kind();
        if (kind == StandardWatchEventKinds.OVERFLOW) {
            warnings.add("File change events were lost (watch overflow)");
            return;
        }
        final Object context = event.context();
        if (!(context instanceof Path)) {
            return;
        }
        final File changed = new File(dir, ((Path) context).toString());
        if (changed.isDirectory()) {
            // A new folder is its own watch registration. The parent only reports the create event.
            this.extendTreeLocked(changed, warnings);
        }
        if (!this.matches(dir, changed)) {
            return;
        }
        // Windows reports the directory itself as modified when a child is created, changed, or removed. The child event is the one that matters.
        if (kind == StandardWatchEventKinds.ENTRY_MODIFY && changed.isDirectory()) {
            return;
        }
        // A removed folder is already gone, so it is not isDirectory(). It is still in watchedDirs. Later MOD events for its files are that delete.
        if (kind == StandardWatchEventKinds.ENTRY_DELETE && this.watchedDirs.contains(changed.getAbsolutePath())) {
            this.lastDeletedDir = changed;
        } else if (kind == StandardWatchEventKinds.ENTRY_MODIFY && this.lastDeletedDir != null && J7FileChangeLogObserver.isUnder(this.lastDeletedDir, changed)) {
            return;
        }
        final String path = this.resolvePath(changed);
        final String kindName;
        if (kind == StandardWatchEventKinds.ENTRY_CREATE) {
            kindName = "CREATE";
        } else if (kind == StandardWatchEventKinds.ENTRY_DELETE) {
            kindName = "DELETE";
        } else if (kind == StandardWatchEventKinds.ENTRY_MODIFY) {
            kindName = "MOD";
        } else {
            kindName = kind.name();
        }
        final String line = "FileObserve>" + path + " > " + kindName;
        final long now = System.currentTimeMillis();
        if (line.equals(this.lastLine) && now - this.lastLineAt < J7FileChangeLogObserver.SAME_LINE_MILLIS) {
            return;
        }
        this.lastLine = line;
        this.lastLineAt = now;
        lines.add(line);
    }

    private boolean matches(final File watchedDir, final File changed) {
        for (int i = 0; i < this.targets.size(); i++) {
            final Target target = this.targets.get(i);
            final boolean inTree;
            if (target.directory) {
                inTree = J7FileChangeLogObserver.samePath(target.file.getAbsolutePath(), watchedDir.getAbsolutePath()) || (target.recursive && (J7FileChangeLogObserver.isUnder(target.file, watchedDir) || J7FileChangeLogObserver.samePath(target.file.getAbsolutePath(), changed.getAbsolutePath()) || J7FileChangeLogObserver.isUnder(target.file, changed)));
            } else {
                inTree = J7FileChangeLogObserver.samePath(target.file.getAbsolutePath(), changed.getAbsolutePath());
            }
            if (!inTree) {
                continue;
            }
            if (target.patterns == null) {
                return true;
            }
            // Patterns see the resolved path (usually a .% link), not the absolute file path.
            final String logical = this.resolvePath(changed);
            for (int p = 0; p < target.patterns.length; p++) {
                final Pattern pattern = target.patterns[p];
                if (pattern != null && pattern.matcher(logical).matches()) {
                    return true;
                }
            }
        }
        return false;
    }

    private File recursiveRoot(final File path) {
        for (int i = 0; i < this.targets.size(); i++) {
            final Target target = this.targets.get(i);
            if (target.recursive && (J7FileChangeLogObserver.samePath(target.file.getAbsolutePath(), path.getAbsolutePath()) || J7FileChangeLogObserver.isUnder(target.file, path))) {
                return target.file;
            }
        }
        return null;
    }

    private static boolean isUnder(final File root, final File path) {
        String prefix = root.getAbsolutePath();
        if (!prefix.endsWith(File.separator)) {
            prefix = prefix + File.separator;
        }
        final String child = path.getAbsolutePath();
        if (CrossSystem.isWindows() || CrossSystem.isMac()) {
            return child.regionMatches(true, 0, prefix, 0, prefix.length());
        } else {
            return child.startsWith(prefix);
        }
    }

    private static boolean samePath(final String a, final String b) {
        if (CrossSystem.isWindows() || CrossSystem.isMac()) {
            return a.equalsIgnoreCase(b);
        } else {
            return a.equals(b);
        }
    }

    private void logAll(final boolean warning, final List<String> lines) {
        for (int i = 0; i < lines.size(); i++) {
            this.log(warning, lines.get(i));
        }
    }

    private void log(final boolean warning, final String message) {
        final LogInterface log = this.logger;
        if (log == null) {
            return;
        } else if (warning) {
            log.warning(message);
        } else {
            log.info(message);
        }
    }

    private void closeQuiet(final WatchService service) {
        if (service != null) {
            try {
                service.close();
            } catch (final IOException ignore) {
            }
        }
    }
}
