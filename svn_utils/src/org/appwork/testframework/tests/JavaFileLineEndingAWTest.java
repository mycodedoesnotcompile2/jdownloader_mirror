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

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.table.TableCellEditor;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.testframework.TestCaseReporter;
import org.appwork.testframework.TestInterface;
import org.appwork.testframework.TestTag;
import org.appwork.utils.Application;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.IO;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.ide.IDEUtils;
import org.appwork.utils.swing.dialog.ContainerDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

/**
 * AWTest that scans all text files (excluding binary files), detects LF vs CRLF, stores state in cache, and if a file's line ending changed
 * compared to cache, asks via dialog (ExtTable with per-file correct checkbox, all checked by default). Includes install4j files (e.g.
 * .install4j config and templates like template_i4j10.install4j). Does NOT implement PostBuildTestInterface so it never runs in PostBuild
 * (IDE only). When started from main in observer mode, a full workspace scan runs once before file watching begins. Paths under excluded
 * directory names (VCS/IDE/build/tooling noise, e.g. .metadata, .svn, .settings, .cursor) are not scanned or watched.
 */
public class JavaFileLineEndingAWTest implements TestInterface {
    private static final String     CACHE_FILENAME                 = "cfg/lineEndingCache.json";
    /**
     * Single path segment (directory name): do not recurse, scan, or register a watch under these (any depth).
     */
    private static final String[]   EXCLUDED_DIRECTORY_NAMES       = { ".metadata", ".svn", ".settings", ".git", ".hg", ".bzr", ".idea", ".cursor", "node_modules" };
    /** Extensions of files that are always treated as binary and skipped (no content read). */
    private static final String[]   BINARY_EXTENSIONS              = { ".jar", ".zip", ".class", ".dll", ".exe", ".so", ".dylib", ".png", ".jpg", ".jpeg", ".gif", ".ico", ".pdf", ".woff", ".woff2", ".ttf", ".eot", ".pyc", ".bin" };
    /** Extensions of files that must be treated as text (e.g. install4j config/templates). Always checked for line endings. */
    private static final String[]   TEXT_EXTENSIONS_ALWAYS_INCLUDE = { ".install4j" };
    private static final String     LF                             = "LF";
    private static final String     CRLF                           = "CRLF";
    private static final byte       CR                             = (byte) 0x0D;
    private static final byte       NL                             = (byte) 0x0A;
    private static final long       POLL_INTERVAL                  = 10000;
    private static final long       RETRY_SLEEP_MS                 = 2000;
    /**
     * Rows added to the open dialog within this time before it was closed are not applied but asked again (avoids confirming unseen rows).
     */
    private static final long       CONFIRM_GRACE_MS               = 500;
    /** Program argument for {@link #main(String[])}: do not show the warning dialog when the cache is corrupt (it is still logged). */
    private static final String     SKIP_CORRUPT_CACHE_WARNING_ARG = "-skipCorruptCacheWarning";
    /** Shown by default; set via {@link #SKIP_CORRUPT_CACHE_WARNING_ARG}. */
    private boolean                 skipCorruptCacheWarning        = false;
    /** Cross-process lock on the cache, held until JVM exit. Prevents concurrent instances from corrupting the shared cache file. */
    private static FileLock         INSTANCE_LOCK                  = null;
    /** Guards {@link #openDialogSession}. */
    private final Object            dialogLock                     = new Object();
    /** The currently open correction dialog, or null. New changes are added to it instead of opening a second dialog. */
    private LineEndingDialogSession openDialogSession              = null;

    /**
     * Acquires the cross-process instance lock (lock file next to the cache). Returns true if this JVM holds it, false if another process
     * does.
     */
    private static synchronized boolean acquireInstanceLock(File cacheFile) {
        if (INSTANCE_LOCK != null && INSTANCE_LOCK.isValid()) {
            return true;
        }
        File lockFile = new File(cacheFile.getAbsolutePath() + ".lock");
        File parent = lockFile.getParentFile();
        if (parent != null && !parent.exists()) {
            parent.mkdirs();
        }
        FileChannel channel = null;
        try {
            channel = new RandomAccessFile(lockFile, "rw").getChannel();
            FileLock lock = channel.tryLock();
            if (lock == null) {
                channel.close();
                return false;
            }
            INSTANCE_LOCK = lock;
            return true;
        } catch (IOException e) {
            LogV3.log(e);
            if (channel != null) {
                try {
                    channel.close();
                } catch (IOException ignore) {
                }
            }
            return false;
        }
    }

    @Override
    public boolean isMaintenance() {
        return false;
    }

    @Override
    public boolean isSkipOnUnchangedDependencies() {
        return false;
    }

    @Override
    public Set<TestTag> getTags() {
        return EnumSet.of(TestTag.INTERACTIVE);
    }

    @Override
    public void testSucceeded(final String testCaseName) {
        TestCaseReporter.testSucceeded(testCaseName);
    }

    @Override
    public void testSkipped(final String testCaseName, final String reason) {
        TestCaseReporter.testSkipped(testCaseName, reason);
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
        if (!acquireInstanceLock(cacheFile)) {
            LogV3.info("JavaFileLineEndingAWTest: another instance holds the line-ending cache lock, skip");
            return;
        }
        Map<String, String> cache = loadCache(cacheFile);
        scanWorkspace(workspace, cacheFile, cache, true);
    }

    /**
     * Full workspace scan: checks all scannable files against the cache, presents the correction dialog for changed ones and saves the
     * cache.
     */
    private void scanWorkspace(File workspace, File cacheFile, Map<String, String> cache, boolean blocking) {
        List<File> allFiles = new ArrayList<File>();
        collectScannableWorkspaceFiles(workspace, allFiles);
        List<FileLineEndingEntry> toAsk = collectChangedEntries(workspace, allFiles, cache);
        if (!toAsk.isEmpty()) {
            presentLineEndingCorrectionDialog(toAsk, cache, cacheFile, blocking);
        }
        saveCacheLogged(cacheFile, cache);
    }

    /**
     * Compares the given files against the cache. Unknown files are added to the cache, deleted files are removed. Returns the text files
     * whose line ending differs from the cached one (binary files skipped).
     */
    private List<FileLineEndingEntry> collectChangedEntries(File workspace, Iterable<File> files, Map<String, String> cache) {
        List<FileLineEndingEntry> toAsk = new ArrayList<FileLineEndingEntry>();
        synchronized (cache) {
            for (File file : files) {
                String relPath = toWorkspaceRelativePath(workspace, file);
                if (relPath == null || relativePathContainsExcludedDirectorySegment(relPath)) {
                    continue;
                }
                if (!file.isFile()) {
                    cache.remove(relPath);
                    continue;
                }
                if (isLikelyBinaryByExtension(file.getName()) && !isAlwaysIncludeTextExtension(file.getName())) {
                    continue;
                }
                String current = detectLineEndingWithRetry(file, relPath);
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
        }
        return toAsk;
    }

    /**
     * Workspace-relative path with forward slashes, or null if the file is not below the workspace.
     */
    private static String toWorkspaceRelativePath(File workspace, File file) {
        try {
            Path relPath = workspace.getAbsoluteFile().toPath().relativize(file.getAbsoluteFile().toPath());
            String rel = relPath.toString().replace(File.separatorChar, '/');
            if (rel.length() == 0 || rel.equals("..") || rel.startsWith("../")) {
                return null;
            }
            return rel;
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    private static boolean isExcludedWorkspaceDirectoryName(String dirName) {
        if (dirName == null) {
            return false;
        }
        for (int i = 0; i < EXCLUDED_DIRECTORY_NAMES.length; i++) {
            if (EXCLUDED_DIRECTORY_NAMES[i].equals(dirName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * True if any path segment of a workspace-relative path (forward slashes) is an excluded directory name.
     */
    private static boolean relativePathContainsExcludedDirectorySegment(String relPathForwardSlashes) {
        if (relPathForwardSlashes == null || relPathForwardSlashes.length() == 0) {
            return false;
        }
        int start = 0;
        for (int p = 0; p <= relPathForwardSlashes.length(); p++) {
            if (p == relPathForwardSlashes.length() || relPathForwardSlashes.charAt(p) == '/') {
                if (p > start) {
                    String seg = relPathForwardSlashes.substring(start, p);
                    if (isExcludedWorkspaceDirectoryName(seg)) {
                        return true;
                    }
                }
                start = p + 1;
            }
        }
        return false;
    }

    /**
     * Lists all files under {@code root} without descending into excluded directory names.
     */
    private void collectScannableWorkspaceFiles(File root, List<File> out) {
        File[] children = root.listFiles();
        if (children == null) {
            return;
        }
        for (int i = 0; i < children.length; i++) {
            File child = children[i];
            if (child.isDirectory()) {
                if (isExcludedWorkspaceDirectoryName(child.getName())) {
                    continue;
                }
                collectScannableWorkspaceFiles(child, out);
            } else if (child.isFile()) {
                out.add(child);
            }
        }
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
     * Detect line ending of file from raw bytes. Returns "LF", "CRLF", or null if no newline found or file appears binary (contains null
     * byte).
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
     * Shows the correction dialog for the given files. Only one dialog is open at a time: while it is open, further entries are added to it
     * live instead of opening another one. With {@code blocking} the dialog runs in the calling thread, otherwise in its own thread so a
     * watcher can keep feeding it.
     */
    private void presentLineEndingCorrectionDialog(final List<FileLineEndingEntry> entries, final Map<String, String> cache, final File cacheFile, final boolean blocking) {
        final LineEndingDialogSession session;
        synchronized (dialogLock) {
            if (openDialogSession != null) {
                openDialogSession.addEntries(entries);
                return;
            }
            session = new LineEndingDialogSession(cache, cacheFile);
            session.addEntries(entries);
            openDialogSession = session;
        }
        if (blocking) {
            session.run();
        } else {
            final Thread thread = new Thread(session, "JavaFileLineEndingAWTest.Dialog");
            thread.setDaemon(true);
            thread.start();
        }
    }

    /**
     * One row per file: checkbox (default: correct to cached) and path / line-ending change. OK applies choices; Cancel keeps on-disk files
     * and updates cache to current for all.
     */
    private final class LineEndingDialogSession implements Runnable {
        private final Map<String, String>                cache;
        private final File                               cacheFile;
        /** All rows of this session by relPath; guarded by dialogLock. */
        private final Map<String, LineEndingChoiceRow>   rows = new LinkedHashMap<String, LineEndingChoiceRow>();
        private final ExtTableModel<LineEndingChoiceRow> model;
        private final ExtTable<LineEndingChoiceRow>      table;
        private final JLabel                             label;
        private final ContainerDialog                    dlg;

        private LineEndingDialogSession(final Map<String, String> cache, final File cacheFile) {
            this.cache = cache;
            this.cacheFile = cacheFile;
            model = new ExtTableModel<LineEndingChoiceRow>(JavaFileLineEndingAWTest.class.getName() + ".LineEndingChoice") {
                @Override
                protected void initColumns() {
                    this.addColumn(new ExtCheckColumn<LineEndingChoiceRow>("Correct to cached", this) {
                        @Override
                        protected boolean getBooleanValue(final LineEndingChoiceRow value) {
                            return value.correctToCached;
                        }

                        @Override
                        protected void setBooleanValue(final boolean value, final LineEndingChoiceRow object) {
                            object.correctToCached = value;
                        }

                        @Override
                        public boolean isEditable(final LineEndingChoiceRow obj) {
                            return true;
                        }
                    });
                    this.addColumn(new ExtTextColumn<LineEndingChoiceRow>("File", this) {
                        @Override
                        public String getStringValue(final LineEndingChoiceRow value) {
                            return value.entry.relPath;
                        }

                        @Override
                        public int getDefaultWidth() {
                            return 320;
                        }
                    });
                    this.addColumn(new ExtTextColumn<LineEndingChoiceRow>("Line ending (now -> cached)", this) {
                        @Override
                        public String getStringValue(final LineEndingChoiceRow value) {
                            return value.entry.current + " -> " + value.entry.cached;
                        }

                        @Override
                        public int getDefaultWidth() {
                            return 160;
                        }
                    });
                }
            };
            table = new ExtTable<LineEndingChoiceRow>(model);
            final JScrollPane scroll = new JScrollPane(table);
            scroll.setPreferredSize(new Dimension(720, 400));
            final MigPanel panel = new MigPanel("ins 10,wrap 1", "[grow,fill]", "[][]");
            label = new JLabel(getLabelText(0));
            panel.add(label, "growx,wrap");
            panel.add(scroll, "push,grow,width 480:720:,height 240:400:");
            // like OK for the whole table, but ignores the checkboxes: every row is reverted to the cached line ending (e.g. tool started too late)
            final JButton revertAll = new JButton("Revert to line ending from cache");
            revertAll.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    final TableCellEditor ed = table.getCellEditor();
                    if (ed != null) {
                        ed.stopCellEditing();
                    }
                    for (final LineEndingChoiceRow row : model.getElements()) {
                        row.correctToCached = true;
                    }
                    final JButton ok = LineEndingDialogSession.this.dlg.getOkButton();
                    if (ok != null) {
                        ok.doClick();
                    }
                }
            });
            dlg = new ContainerDialog(0, "Line ending changed", panel, null, null, null) {
                @Override
                protected MigPanel createBottomPanel() {
                    // bottom left in the button bar, away from OK/Cancel so it is not clicked by accident
                    final MigPanel bottom = new MigPanel("ins 0", "[]20[]20[grow,fill][]", "[]");
                    bottom.add(revertAll);
                    return bottom;
                }
            };
        }

        private String getLabelText(final int count) {
            return count + " file(s) have line ending changed compared to cache. Uncheck to keep the file as-is and only update the cache.";
        }

        /**
         * Adds entries not yet in the table (deduplicated by relPath). Caller must hold dialogLock.
         */
        private void addEntries(final List<FileLineEndingEntry> entries) {
            boolean added = false;
            for (int i = 0; i < entries.size(); i++) {
                final FileLineEndingEntry e = entries.get(i);
                if (!rows.containsKey(e.relPath)) {
                    rows.put(e.relPath, new LineEndingChoiceRow(e));
                    added = true;
                }
            }
            if (!added) {
                return;
            }
            // Replace the whole table data: addElement()/addAllElements() from a non-EDT thread race (each call sees stale tableData).
            model._fireTableStructureChanged(new ArrayList<LineEndingChoiceRow>(rows.values()), true);
            final String text = getLabelText(rows.size());
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    label.setText(text);
                }
            });
        }

        @Override
        public void run() {
            boolean ok = false;
            try {
                final Integer ret = Dialog.getInstance().showDialog(dlg);
                ok = ret != null && BinaryLogic.containsSome(ret.intValue(), Dialog.RETURN_OK);
                if (ok) {
                    final TableCellEditor ed = table.getCellEditor();
                    if (ed != null) {
                        ed.stopCellEditing();
                    }
                }
            } catch (DialogCanceledException e) {
            } catch (DialogClosedException e) {
            } catch (Throwable e) {
                LogV3.log(e);
            }
            final long closedAt = Time.systemIndependentCurrentJVMTimeMillis();
            // rows the user has seen: in the table and added at least CONFIRM_GRACE_MS before closing; all others are asked again in a new
            // dialog
            final List<LineEndingChoiceRow> shown = model.getElements();
            final List<FileLineEndingEntry> notShown = new ArrayList<FileLineEndingEntry>();
            synchronized (dialogLock) {
                if (openDialogSession == this) {
                    openDialogSession = null;
                }
                for (final LineEndingChoiceRow row : rows.values()) {
                    if (!shown.contains(row)) {
                        notShown.add(row.entry);
                    } else if (closedAt - row.addedAt < CONFIRM_GRACE_MS) {
                        shown.remove(row);
                        notShown.add(row.entry);
                    }
                }
            }
            applyChoices(shown, ok);
            if (!notShown.isEmpty()) {
                presentLineEndingCorrectionDialog(notShown, cache, cacheFile, false);
            }
        }

        private void applyChoices(final List<LineEndingChoiceRow> choiceRows, final boolean ok) {
            int corrected = 0;
            synchronized (cache) {
                for (int i = 0; i < choiceRows.size(); i++) {
                    final LineEndingChoiceRow row = choiceRows.get(i);
                    final FileLineEndingEntry e = row.entry;
                    if (!e.file.isFile()) {
                        cache.remove(e.relPath);
                    } else if (ok && row.correctToCached) {
                        try {
                            correctLineEnding(e.file, e.cached);
                            cache.put(e.relPath, e.cached);
                            corrected++;
                        } catch (IOException ex) {
                            LogV3.log(ex);
                        }
                    } else {
                        cache.put(e.relPath, e.current);
                    }
                }
            }
            if (corrected > 0) {
                LogV3.info("JavaFileLineEndingAWTest: corrected " + corrected + " text file(s)");
            }
            saveCacheLogged(cacheFile, cache);
        }
    }

    private static final class LineEndingChoiceRow {
        final FileLineEndingEntry entry;
        boolean                   correctToCached = true;
        /** When the row was added to the dialog (see CONFIRM_GRACE_MS). */
        final long                addedAt;

        LineEndingChoiceRow(final FileLineEndingEntry entry) {
            this.entry = entry;
            this.addedAt = Time.systemIndependentCurrentJVMTimeMillis();
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
        if (cacheFile == null) {
            return new HashMap<String, String>();
        }
        if (!cacheFile.isFile()) {
            LogV3.info("JavaFileLineEndingAWTest: no line-ending cache found, the full scan will create it: " + cacheFile);
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
        } catch (RuntimeException e) {
            // corrupt cache (e.g. concurrent writers) - rebuild instead of crashing
            LogV3.info("JavaFileLineEndingAWTest: line-ending cache is corrupt, rebuilding: " + cacheFile);
            LogV3.log(e);
            // keep the first corrupt cache for analysis; never overwrite an existing copy
            final File corruptCopy = new File(cacheFile.getParentFile(), "lineEndingCacheCorrupt.json");
            if (!corruptCopy.exists()) {
                try {
                    java.nio.file.Files.copy(cacheFile.toPath(), corruptCopy.toPath());
                    LogV3.info("JavaFileLineEndingAWTest: saved corrupt cache as " + corruptCopy);
                } catch (Throwable copyError) {
                    LogV3.log(copyError);
                }
            }
            if (skipCorruptCacheWarning) {
                LogV3.info("JavaFileLineEndingAWTest: corrupt cache warning dialog skipped (" + SKIP_CORRUPT_CACHE_WARNING_ARG + ")");
            } else {
                showCorruptCacheWarning(cacheFile, corruptCopy);
            }
            return new HashMap<String, String>();
        }
    }

    /**
     * Blocking warning that the cache was corrupt and is rebuilt. Shown before the scan starts.
     */
    private void showCorruptCacheWarning(File cacheFile, File corruptCopy) {
        try {
            final StringBuilder sb = new StringBuilder();
            sb.append("The line-ending cache is corrupt and will be rebuilt by a full scan:\r\n");
            sb.append(cacheFile.getAbsolutePath());
            sb.append("\r\n\r\nLine-ending changes since the last valid cache state will NOT be detected in this run.");
            if (corruptCopy.isFile()) {
                sb.append("\r\n\r\nA copy of the (first) corrupt cache is kept at:\r\n");
                sb.append(corruptCopy.getAbsolutePath());
            }
            sb.append("\r\n\r\nStart with " + SKIP_CORRUPT_CACHE_WARNING_ARG + " to skip this dialog.");
            Dialog.getInstance().showMessageDialog("Line-ending cache corrupt", sb.toString());
        } catch (Throwable e) {
            LogV3.log(e);
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
        // watcher and dialog thread share the map; serialize and write under its lock so two saves never write the .bac file concurrently
        synchronized (cache) {
            final boolean created = !cacheFile.exists();
            String json = JSonStorage.serializeToJson(cache);
            IO.secureWrite(cacheFile, json.getBytes(Charset.forName("UTF-8")));
            if (created) {
                LogV3.info("JavaFileLineEndingAWTest: line-ending cache created with " + cache.size() + " entries: " + cacheFile);
            }
        }
    }

    private void saveCacheLogged(File cacheFile, Map<String, String> cache) {
        try {
            saveCache(cacheFile, cache);
        } catch (IOException ex) {
            LogV3.info("JavaFileLineEndingAWTest: could not write line-ending cache");
            LogV3.log(ex);
        }
    }

    public static void main(String[] args) throws Exception {
        Application.setApplication(".tests");
        File cacheFile = Application.getResource(CACHE_FILENAME);
        if (cacheFile != null && !acquireInstanceLock(cacheFile)) {
            LogV3.info("JavaFileLineEndingAWTest: another instance is already running, exit");
            return;
        }
        JavaFileLineEndingAWTest test = new JavaFileLineEndingAWTest();
        if (args != null) {
            for (String arg : args) {
                if (SKIP_CORRUPT_CACHE_WARNING_ARG.equalsIgnoreCase(arg)) {
                    test.skipCorruptCacheWarning = true;
                }
            }
        }
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
        if (!acquireInstanceLock(cacheFile)) {
            LogV3.info("JavaFileLineEndingAWTest: another instance holds the line-ending cache lock, no observer mode");
            return false;
        }
        // one cache instance for the whole observer run; shared with the (non-blocking) dialog thread
        final Map<String, String> cache = loadCache(cacheFile);
        final Path workspacePath = workspace.toPath();
        WatchService watchService = null;
        try {
            watchService = FileSystems.getDefault().newWatchService();
            Map<WatchKey, Path> key2Directory = new HashMap<WatchKey, Path>();
            registerDirectoryRecursive(workspacePath, watchService, key2Directory);
            LogV3.info("JavaFileLineEndingAWTest: observer mode active for " + workspace.getAbsolutePath());
            LogV3.info("JavaFileLineEndingAWTest: initial full workspace scan");
            final long scanStarted = Time.systemIndependentCurrentJVMTimeMillis();
            scanWorkspaceFromObserver(workspace, cacheFile, cache);
            final long scanDuration = Time.systemIndependentCurrentJVMTimeMillis() - scanStarted;
            LogV3.info("JavaFileLineEndingAWTest: initial full workspace scan took " + TimeFormatter.formatMilliSeconds(scanDuration, 0));
            while (!Thread.currentThread().isInterrupted()) {
                Set<Path> changedFiles = new LinkedHashSet<Path>();
                WatchKey key = watchService.take();
                boolean rescanAll = processWatchKey(workspacePath, watchService, key2Directory, key, changedFiles);
                rescanAll |= drainPendingKeys(workspacePath, watchService, key2Directory, changedFiles);
                if (rescanAll) {
                    scanWorkspaceFromObserver(workspace, cacheFile, cache);
                } else if (!changedFiles.isEmpty()) {
                    processChangedTextFiles(workspace, cacheFile, cache, changedFiles);
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

    /**
     * Full scan in observer mode: non-blocking dialog, so the watcher keeps running and adds further changes to the open dialog.
     */
    private void scanWorkspaceFromObserver(File workspace, File cacheFile, Map<String, String> cache) {
        try {
            scanWorkspace(workspace, cacheFile, cache, false);
        } catch (Throwable e) {
            LogV3.log(e);
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
        Path fileName = directory.getFileName();
        if (fileName != null && isExcludedWorkspaceDirectoryName(fileName.toString())) {
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
            if (relPath == null) {
                return false;
            }
            String rel = relPath.toString().replace(File.separatorChar, '/');
            if (relativePathContainsExcludedDirectorySegment(rel)) {
                return false;
            }
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private void processChangedTextFiles(File workspace, File cacheFile, Map<String, String> cache, Set<Path> changedFiles) {
        List<File> files = new ArrayList<File>(changedFiles.size());
        for (Path changed : changedFiles) {
            files.add(changed.toFile());
        }
        List<FileLineEndingEntry> toAsk = collectChangedEntries(workspace, files, cache);
        if (!toAsk.isEmpty()) {
            presentLineEndingCorrectionDialog(toAsk, cache, cacheFile, false);
        }
        saveCacheLogged(cacheFile, cache);
    }

    /**
     * Reads line ending with a few retries for transient locks; logs and returns null if reading still fails.
     */
    private String detectLineEndingWithRetry(File file, String relPathForLog) {
        final int maxAttempts = 3;
        IOException last = null;
        for (int attempt = 0; attempt < maxAttempts; attempt++) {
            try {
                return detectLineEnding(file);
            } catch (IOException e) {
                last = e;
                if (!file.isFile()) {
                    return null;
                }
                if (attempt + 1 < maxAttempts) {
                    try {
                        Thread.sleep(RETRY_SLEEP_MS);
                    } catch (InterruptedException interrupted) {
                        Thread.currentThread().interrupt();
                        return null;
                    }
                }
            }
        }
        if (last != null) {
            LogV3.info("JavaFileLineEndingAWTest: skip line-ending check after retries (IO): " + relPathForLog);
            LogV3.log(last);
        }
        return null;
    }
}
