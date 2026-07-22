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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.testframework;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;

/**
 * Resolves {@code .java} sources for dependency class names, fingerprints them, and compares to persisted hashes. Uses a per-JVM run cache
 * keyed by canonical {@code .java} path so the same file is not re-hashed many times when several tests share dependencies.
 */
public final class DependencyJavaSourceFingerprint {
    /**
     * Default cap for dependency classes to consider for {@code .java} fingerprinting (matches log listing in runners).
     */
    public static final int DEFAULT_MAX_CLASSES = 10;

    private static final Map<String, JavaFileHashCacheEntry> RUN_CANONICAL_JAVA_PATH_TO_HASH = new HashMap<String, JavaFileHashCacheEntry>();

    private DependencyJavaSourceFingerprint() {
    }

    /**
     * Clears the in-memory cache of {@code .java} file hashes for the current process run (call once at the start of a full test sweep).
     */
    public static void clearRunFileHashCache() {
        synchronized (RUN_CANONICAL_JAVA_PATH_TO_HASH) {
            RUN_CANONICAL_JAVA_PATH_TO_HASH.clear();
        }
    }

    /**
     * Same change list algorithm as post-build / IDE runners: {@code class (new|changed|removed)}.
     */
    public static ArrayList<String> computeChangeDescriptions(final Map<String, String> prev, final Map<String, String> current) {
        final Map<String, String> safePrev = prev != null ? prev : Collections.<String, String> emptyMap();
        final Map<String, String> safeCur = current != null ? current : Collections.<String, String> emptyMap();
        final ArrayList<String> changes = new ArrayList<String>();
        for (final Entry<String, String> e : safeCur.entrySet()) {
            final String c = e.getKey();
            if (!safePrev.containsKey(c)) {
                changes.add(c + " (new)");
            } else if (!StringUtils.equals(e.getValue(), safePrev.get(c))) {
                changes.add(c + " (changed)");
            }
        }
        for (final String c : safePrev.keySet()) {
            if (!safeCur.containsKey(c)) {
                changes.add(c + " (removed)");
            }
        }
        return changes;
    }

    public static String parseFqcnFromChangeDescription(final String line) {
        if (line == null) {
            return null;
        }
        final int idx = line.lastIndexOf(" (");
        if (idx <= 0) {
            return line;
        }
        return line.substring(0, idx);
    }

    public static boolean isRemovedChange(final String line) {
        return line != null && line.endsWith(" (removed)");
    }

    public static String toOuterClassName(final String fqcn) {
        if (fqcn == null) {
            return null;
        }
        final int d = fqcn.indexOf('$');
        if (d > 0) {
            return fqcn.substring(0, d);
        }
        return fqcn;
    }

    /**
     * Optional extra roots from Ant ({@code -source=}) first, then classpath-derived roots (same heuristics as {@link JREExecuter}).
     */
    public static List<File> resolveSourceRootsFromClasspathAndExtra(final String semiColonSeparatedExtraRoots) {
        final ArrayList<File> sourcePaths = new ArrayList<File>();
        final Set<String> seen = new HashSet<String>();
        if (StringUtils.isNotEmpty(semiColonSeparatedExtraRoots)) {
            for (final String entry : semiColonSeparatedExtraRoots.split(File.pathSeparator)) {
                if (StringUtils.isNotEmpty(entry)) {
                    final File f = new File(entry);
                    if (f.isDirectory()) {
                        addCanonicalIfDir(f, sourcePaths, seen);
                    }
                }
            }
        }
        final String classpath = System.getProperty("java.class.path");
        if (classpath != null) {
            for (final String entry : classpath.split(File.pathSeparator)) {
                final File entryFile = new File(entry);
                if (entryFile.isDirectory()) {
                    File parent = entryFile.getParentFile();
                    while (parent != null) {
                        addSourcePathIfExists(parent, "src", sourcePaths, seen);
                        addSourcePathIfExists(parent, "src/main/java", sourcePaths, seen);
                        addSourcePathIfExists(parent, "src/test/java", sourcePaths, seen);
                        addSourcePathIfExists(parent, "source", sourcePaths, seen);
                        parent = parent.getParentFile();
                    }
                }
            }
        }
        final String userDir = System.getProperty("user.dir");
        if (userDir != null) {
            final File workDir = new File(userDir);
            addSourcePathIfExists(workDir, "src", sourcePaths, seen);
            addSourcePathIfExists(workDir, "src/main/java", sourcePaths, seen);
            addSourcePathIfExists(workDir, "src/test/java", sourcePaths, seen);
        }
        return sourcePaths;
    }

    private static void addCanonicalIfDir(final File dir, final List<File> sourcePaths, final Set<String> seen) {
        if (dir == null || !dir.isDirectory()) {
            return;
        }
        try {
            final String canonical = dir.getCanonicalPath();
            if (!seen.contains(canonical)) {
                seen.add(canonical);
                sourcePaths.add(dir);
            }
        } catch (final IOException e) {
            return;
        }
    }

    private static void addSourcePathIfExists(final File parent, final String subPath, final List<File> sourcePaths, final Set<String> seen) {
        final File srcDir = new File(parent, subPath);
        if (srcDir.exists() && srcDir.isDirectory()) {
            addCanonicalIfDir(srcDir, sourcePaths, seen);
        }
    }

    public static File findJavaFileForOuterClass(final String outerFqcn, final List<File> roots) {
        if (outerFqcn == null || roots == null) {
            return null;
        }
        final String relativePath = outerFqcn.replace('.', '/') + ".java";
        for (final File sourcePath : roots) {
            final File sourceFile = new File(sourcePath, relativePath);
            if (sourceFile.isFile()) {
                return sourceFile;
            }
        }
        return null;
    }

    /**
     * @return SHA256 of the {@code .java} file, or {@code null} if not found / unreadable
     */
    public static String computeCurrentJavaFileHash(final String fqcn, final List<File> roots) {
        final String outer = toOuterClassName(fqcn);
        final File f = findJavaFileForOuterClass(outer, roots);
        if (f == null || !f.isFile()) {
            return null;
        }
        String canon;
        try {
            canon = f.getCanonicalPath();
        } catch (final IOException e) {
            return null;
        }
        final long lastModified = f.lastModified();
        final long length = f.length();
        synchronized (RUN_CANONICAL_JAVA_PATH_TO_HASH) {
            final JavaFileHashCacheEntry runCached = RUN_CANONICAL_JAVA_PATH_TO_HASH.get(canon);
            if (isUsable(runCached, lastModified, length)) {
                return runCached.sha256;
            }
            final byte[] content;
            try {
                content = IO.readFile(f);
            } catch (final IOException e) {
                return null;
            }
            final String h = content != null ? Hash.getSHA256(content) : null;
            if (h != null) {
                final JavaFileHashCacheEntry newEntry = new JavaFileHashCacheEntry(lastModified, length, h);
                RUN_CANONICAL_JAVA_PATH_TO_HASH.put(canon, newEntry);
            }
            return h;
        }
    }

    private static boolean isUsable(final JavaFileHashCacheEntry entry, final long lastModified, final long length) {
        if (entry == null || StringUtils.isEmpty(entry.sha256)) {
            return false;
        }
        return entry.lastModified == lastModified && entry.size == length;
    }

    private static final class JavaFileHashCacheEntry {
        private final long   lastModified;
        private final long   size;
        private final String sha256;

        private JavaFileHashCacheEntry(final long lastModified, final long size, final String sha256) {
            this.lastModified = lastModified;
            this.size = size;
            this.sha256 = sha256;
        }
    }

    /**
     * When bytecode dependency maps differ but there are at most {@code maxClassesToCheck} changes and none are removals: if every
     * changed dependency's outer {@code .java} exists and matches the previously stored hash for that outer class, treat as no meaningful
     * source change (skip running the test, still advance stored bytecode maps).
     */
    public static boolean shouldSkipBecauseJavaSourcesUnchanged(final List<String> changeDescriptions, final Map<String, String> previousJavaHashesByOuterClass, final List<File> roots, final int maxClassesToCheck) {
        if (changeDescriptions == null || changeDescriptions.isEmpty() || roots == null || roots.isEmpty()) {
            return false;
        }
        if (changeDescriptions.size() > maxClassesToCheck) {
            return false;
        }
        final Map<String, String> prev = previousJavaHashesByOuterClass != null ? previousJavaHashesByOuterClass : Collections.<String, String> emptyMap();
        for (final String line : changeDescriptions) {
            if (isRemovedChange(line)) {
                return false;
            }
            final String fqcn = parseFqcnFromChangeDescription(line);
            final String outer = toOuterClassName(fqcn);
            final String cur = computeCurrentJavaFileHash(fqcn, roots);
            if (cur == null) {
                return false;
            }
            final String old = prev.get(outer);
            if (old == null || !StringUtils.equals(old, cur)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Updates {@code target} with current {@code .java} hashes for the outer classes of the first {@code max} change lines.
     */
    public static void mergeJavaHashesForTopChanges(final Map<String, String> target, final List<String> changeDescriptions, final List<File> roots, final int max) {
        if (target == null || changeDescriptions == null || roots == null || roots.isEmpty()) {
            return;
        }
        final int n = Math.min(max, changeDescriptions.size());
        for (int i = 0; i < n; i++) {
            final String line = changeDescriptions.get(i);
            if (isRemovedChange(line)) {
                continue;
            }
            final String fqcn = parseFqcnFromChangeDescription(line);
            final String outer = toOuterClassName(fqcn);
            final String cur = computeCurrentJavaFileHash(fqcn, roots);
            if (cur != null) {
                target.put(outer, cur);
            }
        }
    }

    /**
     * Short annotation for logs, e.g. {@code unchanged}, {@code changed}, {@code new}, {@code n/a}.
     */
    public static String describeJavaDeltaForChangeLine(final String line, final Map<String, String> previousJavaHashesByOuterClass, final List<File> roots) {
        if (line == null) {
            return "n/a";
        }
        if (isRemovedChange(line)) {
            return "removed";
        }
        if (roots == null || roots.isEmpty()) {
            return "n/a";
        }
        final String fqcn = parseFqcnFromChangeDescription(line);
        final String outer = toOuterClassName(fqcn);
        final String cur = computeCurrentJavaFileHash(fqcn, roots);
        if (cur == null) {
            return "n/a";
        }
        final Map<String, String> prev = previousJavaHashesByOuterClass != null ? previousJavaHashesByOuterClass : Collections.<String, String> emptyMap();
        final String old = prev.get(outer);
        if (old == null) {
            return "new";
        }
        if (StringUtils.equals(old, cur)) {
            return "unchanged";
        }
        return "changed";
    }
}
