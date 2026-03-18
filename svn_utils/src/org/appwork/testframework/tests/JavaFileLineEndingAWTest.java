/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
 *     If anybody or any organization is generating income (directly or indirectly) by using the [The Product] or if there's any commercial interest or aspect in what they are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use the [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use the [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.TestInterface;
import org.appwork.utils.Application;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.ide.IDEUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

/**
 * AWTest that scans all text files (excluding binary files), detects LF vs CRLF, stores state in cache, and if a file's line ending
 * changed compared to cache, asks via dialog whether to correct it. Includes install4j files (e.g. .install4j config and templates
 * like template_i4j10.install4j). Does NOT implement PostBuildTestInterface so it never runs in PostBuild (IDE only).
 */
public class JavaFileLineEndingAWTest implements TestInterface {
    private static final String   CACHE_FILENAME = "cfg/lineEndingCache.json";
    /** Extensions of files that are always treated as binary and skipped (no content read). */
    private static final String[] BINARY_EXTENSIONS = { ".jar", ".zip", ".class", ".dll", ".exe", ".so", ".dylib", ".png", ".jpg", ".jpeg", ".gif", ".ico", ".pdf", ".woff", ".woff2", ".ttf", ".eot", ".pyc", ".bin" };
    /** Extensions of files that must be treated as text (e.g. install4j config/templates). Always checked for line endings. */
    private static final String[] TEXT_EXTENSIONS_ALWAYS_INCLUDE = { ".install4j" };
    private static final String LF             = "LF";
    private static final String CRLF           = "CRLF";
    private static final byte   CR             = (byte) 0x0D;
    private static final byte   NL             = (byte) 0x0A;
    private static final long   POLL_INTERVAL  = 10000;
    private static final long   RETRY_SLEEP_MS = 2000;

    @Override
    public boolean isMaintenance() {
        return false;
    }

    @Override
    public boolean isSkipOnUnchangedDependencies() {
        return false;
    }

    @Override
    public void runTest() throws Exception {
        File workspace = IDEUtils.getWorkSpace();
        if (workspace == null || !workspace.isDirectory()) {
            LogV3.info("JavaFileLineEndingAWTest: no workspace found, skip");
            return;
        }
        File cacheFile = Application.getResource(CACHE_FILENAME);
        if (cacheFile == null) {
            LogV3.info("JavaFileLineEndingAWTest: no cache path, skip");
            return;
        }
        Map<String, String> cache = loadCache(cacheFile);
        List<File> allFiles = Files.getFiles(false, true, workspace);
        // First pass: collect all text files whose line ending changed vs cache (binary files skipped)
        List<FileLineEndingEntry> toAsk = new ArrayList<FileLineEndingEntry>();
        for (File file : allFiles) {
            if (isLikelyBinaryByExtension(file.getName()) && !isAlwaysIncludeTextExtension(file.getName())) {
                continue;
            }
            String relPath = Files.getRelativePath(workspace, file);
            if (relPath == null) {
                continue;
            }
            relPath = relPath.replace(File.separatorChar, '/');
            String current = detectLineEnding(file);
            if (current == null) {
                continue;
            }
            String cached = cache.get(relPath);
            if (cached == null) {
                cache.put(relPath, current);
                continue;
            }
            if (current.equals(cached)) {
                continue;
            }
            toAsk.add(new FileLineEndingEntry(file, relPath, current, cached));
        }
        // Single dialog for all collected files
        if (!toAsk.isEmpty()) {
            boolean doCorrect = askCorrectAll(toAsk);
            if (doCorrect) {
                for (FileLineEndingEntry e : toAsk) {
                    try {
                        correctLineEnding(e.file, e.cached);
                        cache.put(e.relPath, e.cached);
                    } catch (IOException ex) {
                        LogV3.log(ex);
                    }
                }
                LogV3.info("JavaFileLineEndingAWTest: corrected " + toAsk.size() + " text file(s)");
            } else {
                for (FileLineEndingEntry e : toAsk) {
                    cache.put(e.relPath, e.current);
                }
            }
        }
        saveCache(cacheFile, cache);
    }

    /**
     * Returns true if the filename has an extension that typically indicates a binary file (no content read).
     */
    private boolean isLikelyBinaryByExtension(String filename) {
        if (filename == null) {
            return true;
        }
        String lower = filename.toLowerCase();
        for (String ext : BINARY_EXTENSIONS) {
            if (lower.endsWith(ext)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns true if the file should always be treated as text (e.g. install4j config/template files).
     */
    private boolean isAlwaysIncludeTextExtension(String filename) {
        if (filename == null) {
            return false;
        }
        String lower = filename.toLowerCase();
        for (String ext : TEXT_EXTENSIONS_ALWAYS_INCLUDE) {
            if (lower.endsWith(ext)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Detect line ending of file from raw bytes. Returns "LF", "CRLF", or null if no newline found or file appears binary (contains null byte).
     */
    private String detectLineEnding(File file) throws IOException {
        byte[] bytes = IO.readFile(file, 64 * 1024);
        if (bytes == null || bytes.length == 0) {
            return null;
        }
        for (int i = 0; i < bytes.length; i++) {
            if (bytes[i] == 0) {
                return null; // binary file
            }
        }
        boolean seenCrlf = false;
        boolean seenLf = false;
        for (int i = 0; i < bytes.length; i++) {
            if (bytes[i] == NL) {
                if (i > 0 && bytes[i - 1] == CR) {
                    seenCrlf = true;
                } else {
                    seenLf = true;
                }
            }
        }
        if (seenCrlf) {
            return CRLF;
        }
        if (seenLf) {
            return LF;
        }
        return null;
    }

    /**
     * Convert file content to target line ending (LF or CRLF) and write back.
     */
    private void correctLineEnding(File file, String target) throws IOException {
        byte[] bytes = IO.readFile(file);
        if (bytes == null) {
            return;
        }
        byte[] converted = target.equals(CRLF) ? toCRLF(bytes) : toLF(bytes);
        IO.secureWrite(file, converted);
    }

    private byte[] toLF(byte[] bytes) {
        int outLen = bytes.length;
        for (int i = 0; i < bytes.length; i++) {
            if (bytes[i] == CR && i + 1 < bytes.length && bytes[i + 1] == NL) {
                outLen--;
                i++;
            } else if (bytes[i] == CR) {
                // lone \r -> \n, same length
            }
        }
        byte[] out = new byte[outLen];
        int j = 0;
        for (int i = 0; i < bytes.length; i++) {
            if (bytes[i] == CR && i + 1 < bytes.length && bytes[i + 1] == NL) {
                out[j++] = NL;
                i++;
            } else if (bytes[i] == CR) {
                out[j++] = NL;
            } else {
                out[j++] = bytes[i];
            }
        }
        return out;
    }

    private byte[] toCRLF(byte[] bytes) {
        int len = 0;
        for (int i = 0; i < bytes.length; i++) {
            if (bytes[i] == NL && (i == 0 || bytes[i - 1] != CR)) {
                len += 2;
            } else {
                len++;
            }
        }
        byte[] out = new byte[len];
        int j = 0;
        for (int i = 0; i < bytes.length; i++) {
            if (bytes[i] == NL && (i == 0 || bytes[i - 1] != CR)) {
                out[j++] = CR;
                out[j++] = NL;
            } else {
                out[j++] = bytes[i];
            }
        }
        return out;
    }

    /**
     * Show one dialog for all collected files: list paths and ask whether to correct all to cached line ending.
     */
    private boolean askCorrectAll(List<FileLineEndingEntry> entries) {
        StringBuilder msg = new StringBuilder();
        msg.append(entries.size()).append(" file(s) have line ending changed compared to cache.\n\n");
        int maxList = 30;
        for (int i = 0; i < entries.size() && i < maxList; i++) {
            FileLineEndingEntry e = entries.get(i);
            msg.append("  ").append(e.relPath).append("  (").append(e.current).append(" -> ").append(e.cached).append(")\n");
        }
        if (entries.size() > maxList) {
            msg.append("  ... and ").append(entries.size() - maxList).append(" more.\n");
        }
        msg.append("\nCorrect all to cached line ending?");
        try {
            int ret = Dialog.getInstance().showConfirmDialog(0, "Line ending changed", msg.toString(), null, "Yes", "No");
            return BinaryLogic.containsSome(ret, Dialog.RETURN_OK);
        } catch (DialogCanceledException e) {
            return false;
        } catch (DialogClosedException e) {
            return false;
        } catch (Throwable e) {
            LogV3.log(e);
            return false;
        }
    }

    private static final class FileLineEndingEntry {
        final File   file;
        final String relPath;
        final String current;
        final String cached;

        FileLineEndingEntry(File file, String relPath, String current, String cached) {
            this.file = file;
            this.relPath = relPath;
            this.current = current;
            this.cached = cached;
        }
    }

    private Map<String, String> loadCache(File cacheFile) {
        if (cacheFile == null || !cacheFile.isFile()) {
            return new HashMap<String, String>();
        }
        try {
            String json = IO.readFileToTrimmedString(cacheFile);
            if (json == null || json.length() == 0) {
                return new HashMap<String, String>();
            }
            Map<String, String> map = JSonStorage.restoreFromString(json, TypeRef.HASHMAP_STRING);
            return map != null ? map : new HashMap<String, String>();
        } catch (IOException e) {
            LogV3.log(e);
            return new HashMap<String, String>();
        }
    }

    private void saveCache(File cacheFile, Map<String, String> cache) throws IOException {
        if (cacheFile == null) {
            return;
        }
        File parent = cacheFile.getParentFile();
        if (parent != null && !parent.exists()) {
            parent.mkdirs();
        }
        String json = JSonStorage.serializeToJson(cache);
        IO.secureWrite(cacheFile, json.getBytes(Charset.forName("UTF-8")));
    }

    public static void main(String[] args) throws Exception {
        BuildDecisions.setEnabled(false);
        Application.setApplication(".tests");
        JavaFileLineEndingAWTest test = new JavaFileLineEndingAWTest();
        if (!test.runObserverModeFromMain()) {
            test.runPollingModeFromMain();
        }
    }

    private void runPollingModeFromMain() throws Exception {
        LogV3.info("JavaFileLineEndingAWTest: polling mode active");
        while (true) {
            runOnceFromMain();
            Thread.sleep(POLL_INTERVAL);
        }
    }

    private void runOnceFromMain() {
        try {
            LogV3.info("Run");
            runTest();
        } catch (Throwable e) {
            LogV3.log(e);
        }
    }

    private boolean runObserverModeFromMain() {
        File workspace = IDEUtils.getWorkSpace();
        if (workspace == null || !workspace.isDirectory()) {
            LogV3.info("JavaFileLineEndingAWTest: no workspace found for observer mode");
            return false;
        }
        File cacheFile = Application.getResource(CACHE_FILENAME);
        if (cacheFile == null) {
            LogV3.info("JavaFileLineEndingAWTest: no cache path for observer mode");
            return false;
        }
        Map<String, String> cache = loadCache(cacheFile);
        final Path workspacePath = workspace.toPath();
        WatchService watchService = null;
        try {
            watchService = FileSystems.getDefault().newWatchService();
            Map<WatchKey, Path> key2Directory = new HashMap<WatchKey, Path>();
            registerDirectoryRecursive(workspacePath, watchService, key2Directory);
            LogV3.info("JavaFileLineEndingAWTest: observer mode active for " + workspace.getAbsolutePath());
            while (!Thread.currentThread().isInterrupted()) {
                Set<Path> changedFiles = new LinkedHashSet<Path>();
                WatchKey key = watchService.take();
                boolean rescanAll = processWatchKey(workspacePath, watchService, key2Directory, key, changedFiles);
                rescanAll |= drainPendingKeys(workspacePath, watchService, key2Directory, changedFiles);
                if (rescanAll) {
                    runOnceFromMain();
                    cache = loadCache(cacheFile);
                } else if (!changedFiles.isEmpty()) {
                    processChangedTextFiles(workspacePath, cacheFile, cache, changedFiles);
                }
            }
            return true;
        } catch (Throwable e) {
            LogV3.log(e);
            LogV3.info("JavaFileLineEndingAWTest: observer mode failed, fallback to polling");
            return false;
        } finally {
            if (watchService != null) {
                try {
                    watchService.close();
                } catch (IOException e) {
                    LogV3.log(e);
                }
            }
        }
    }

    private boolean drainPendingKeys(Path workspace, WatchService watchService, Map<WatchKey, Path> key2Directory, Set<Path> changedFiles) throws Exception {
        boolean rescanAll = false;
        while (true) {
            WatchKey key = watchService.poll();
            if (key == null) {
                return rescanAll;
            }
            rescanAll |= processWatchKey(workspace, watchService, key2Directory, key, changedFiles);
        }
    }

    private boolean processWatchKey(Path workspace, WatchService watchService, Map<WatchKey, Path> key2Directory, WatchKey key, Set<Path> changedFiles) throws Exception {
        if (key == null) {
            return false;
        }
        boolean rescanAll = false;
        try {
            Path parent = key2Directory.get(key);
            if (parent == null) {
                return true;
            }
            List<WatchEvent<?>> events = key.pollEvents();
            for (WatchEvent<?> event : events) {
                WatchEvent.Kind<?> kind = event.kind();
                if (StandardWatchEventKinds.OVERFLOW == kind) {
                    rescanAll = true;
                    continue;
                }
                Object contextObject = event.context();
                if (!(contextObject instanceof Path)) {
                    rescanAll = true;
                    continue;
                }
                Path changed = parent.resolve((Path) contextObject);
                if (StandardWatchEventKinds.ENTRY_CREATE == kind && java.nio.file.Files.isDirectory(changed)) {
                    registerDirectoryRecursive(changed, watchService, key2Directory);
                }
                if (isWorkspaceTrackedFile(workspace, changed)) {
                    changedFiles.add(changed);
                }
            }
        } finally {
            if (!key.reset()) {
                key2Directory.remove(key);
            }
        }
        return rescanAll;
    }

    //
    private void registerDirectoryRecursive(Path directory, WatchService watchService, Map<WatchKey, Path> key2Directory) throws Exception {
        if (directory == null || !java.nio.file.Files.isDirectory(directory)) {
            return;
        }
        WatchKey key = directory.register(watchService, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_DELETE);
        key2Directory.put(key, directory);
        File[] children = directory.toFile().listFiles();
        if (children != null) {
            for (File child : children) {
                if (child.isDirectory()) {
                    registerDirectoryRecursive(child.toPath(), watchService, key2Directory);
                }
            }
        }
    }

    /**
     * Returns true if the path is a regular file under the workspace (any file; binary filtering happens when reading).
     */
    private boolean isWorkspaceTrackedFile(Path workspace, Path file) {
        if (file == null || !java.nio.file.Files.isRegularFile(file)) {
            return false;
        }
        try {
            Path relPath = workspace.relativize(file);
            return relPath != null;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private void processChangedTextFiles(Path workspace, File cacheFile, Map<String, String> cache, Set<Path> changedFiles) throws IOException {
        List<FileLineEndingEntry> toAsk = new ArrayList<FileLineEndingEntry>();
        for (Path changed : changedFiles) {
            String relPath = workspace.relativize(changed).toString().replace(File.separatorChar, '/');
            File file = changed.toFile();
            if (!file.isFile()) {
                cache.remove(relPath);
                continue;
            }
            if (isLikelyBinaryByExtension(file.getName()) && !isAlwaysIncludeTextExtension(file.getName())) {
                continue;
            }
            String current = detectLineEndingWithRetry(file);
            if (current == null) {
                continue;
            }
            String cached = cache.get(relPath);
            if (cached == null) {
                cache.put(relPath, current);
                continue;
            }
            if (!current.equals(cached)) {
                toAsk.add(new FileLineEndingEntry(file, relPath, current, cached));
            }
        }
        if (!toAsk.isEmpty()) {
            boolean doCorrect = askCorrectAll(toAsk);
            if (doCorrect) {
                for (FileLineEndingEntry e : toAsk) {
                    try {
                        correctLineEnding(e.file, e.cached);
                        cache.put(e.relPath, e.cached);
                    } catch (IOException ex) {
                        LogV3.log(ex);
                    }
                }
                LogV3.info("JavaFileLineEndingAWTest: corrected " + toAsk.size() + " changed text file(s)");
            } else {
                for (FileLineEndingEntry e : toAsk) {
                    cache.put(e.relPath, e.current);
                }
            }
        }
        saveCache(cacheFile, cache);
    }

    private String detectLineEndingWithRetry(File file) throws IOException {
        while (true) {
            try {
                return detectLineEnding(file);
            } catch (IOException e) {
                if (!file.isFile()) {
                    return null;
                }
                try {
                    Thread.sleep(RETRY_SLEEP_MS);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw e;
                }
            }
        }
    }
}
