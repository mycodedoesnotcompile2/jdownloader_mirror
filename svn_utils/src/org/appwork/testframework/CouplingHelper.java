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
package org.appwork.testframework;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.utils.os.CrossSystem;
import org.objectweb.asm.ClassReader;

/**
 * Reverse dependency helper. Builds a tree of classes that reference a given class.
 *
 * @author thomas
 * @date 23.02.2026
 */
public class CouplingHelper {
    private static volatile Map<String, Set<String>> REFERENCE_INDEX_CACHE = null;

    public static void main(String[] args) throws IOException {
        System.out.println(CouplingHelper.findReferenceTree(CrossSystem.class));
    }

    public static class ReferenceNode {
        private final String              className;
        private final boolean             cycle;
        private final List<ReferenceNode> referencedBy = new ArrayList<ReferenceNode>();

        public ReferenceNode(final String className) {
            this(className, false);
        }

        private ReferenceNode(final String className, final boolean cycle) {
            this.className = className;
            this.cycle = cycle;
        }

        public String getClassName() {
            return className;
        }

        public boolean isCycle() {
            return cycle;
        }

        public List<ReferenceNode> getReferencedBy() {
            return Collections.unmodifiableList(referencedBy);
        }

        private void addReferencedBy(final ReferenceNode node) {
            referencedBy.add(node);
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            append(sb, "", true, true);
            return sb.toString();
        }

        private void append(final StringBuilder sb, final String prefix, final boolean last, final boolean root) {
            if (root) {
                sb.append(className);
            } else {
                sb.append(prefix).append(last ? "\\- " : "+- ").append(className);
            }
            if (cycle) {
                sb.append(" (cycle)");
            }
            sb.append('\n');
            for (int i = 0; i < referencedBy.size(); i++) {
                final ReferenceNode child = referencedBy.get(i);
                final String childPrefix = root ? "" : prefix + (last ? "   " : "|  ");
                child.append(sb, childPrefix, i == referencedBy.size() - 1, false);
            }
        }
    }

    public static ReferenceNode findReferenceTree(final Class<?> targetClass) throws IOException {
        if (targetClass == null) {
            throw new IllegalArgumentException("targetClass must not be null");
        }
        return findReferenceTree(targetClass.getName());
    }

    public static ReferenceNode findReferenceTree(final String targetClassName) throws IOException {
        if (targetClassName == null || targetClassName.trim().length() == 0) {
            throw new IllegalArgumentException("targetClassName must not be empty");
        }
        final Map<String, Set<String>> referenceIndex = getOrBuildReferenceIndex();
        return buildTree(targetClassName.trim(), referenceIndex, new LinkedHashSet<String>());
    }

    public static void clearCache() {
        REFERENCE_INDEX_CACHE = null;
    }

    private static Map<String, Set<String>> getOrBuildReferenceIndex() throws IOException {
        final Map<String, Set<String>> cached = REFERENCE_INDEX_CACHE;
        if (cached != null) {
            return cached;
        }
        synchronized (CouplingHelper.class) {
            if (REFERENCE_INDEX_CACHE == null) {
                REFERENCE_INDEX_CACHE = buildReferenceIndex();
            }
            return REFERENCE_INDEX_CACHE;
        }
    }

    private static ReferenceNode buildTree(final String className, final Map<String, Set<String>> referenceIndex, final LinkedHashSet<String> stack) {
        final ReferenceNode root = new ReferenceNode(className);
        if (!stack.add(className)) {
            return new ReferenceNode(className, true);
        }
        final Set<String> directReferencedBy = referenceIndex.get(className);
        if (directReferencedBy != null && !directReferencedBy.isEmpty()) {
            final List<String> sorted = new ArrayList<String>(directReferencedBy);
            Collections.sort(sorted);
            for (final String refBy : sorted) {
                if (stack.contains(refBy)) {
                    root.addReferencedBy(new ReferenceNode(refBy, true));
                } else {
                    root.addReferencedBy(buildTree(refBy, referenceIndex, stack));
                }
            }
        }
        stack.remove(className);
        return root;
    }

    private static Map<String, Set<String>> buildReferenceIndex() throws IOException {
        final Map<String, Set<String>> referenceIndex = new HashMap<String, Set<String>>();
        final String classPath = System.getProperty("java.class.path");
        if (classPath == null || classPath.length() == 0) {
            return referenceIndex;
        }
        final String[] classPathEntries = classPath.split(File.pathSeparator);
        for (final String entry : classPathEntries) {
            if (entry == null || entry.length() == 0) {
                continue;
            }
            final File file = new File(entry);
            if (!file.exists()) {
                continue;
            }
            if (file.isDirectory()) {
                scanDirectory(file, file, referenceIndex);
            } else if (isJar(file.getName())) {
                scanJar(file, referenceIndex);
            }
        }
        return referenceIndex;
    }

    private static void scanDirectory(final File root, final File current, final Map<String, Set<String>> referenceIndex) throws IOException {
        final File[] children = current.listFiles();
        if (children == null) {
            return;
        }
        for (final File child : children) {
            if (child.isDirectory()) {
                scanDirectory(root, child, referenceIndex);
            } else if (child.getName().endsWith(".class")) {
                final String className = toClassName(root, child);
                if (!skip(className)) {
                    final FileInputStream fis = new FileInputStream(child);
                    try {
                        addClassReferences(className, fis, referenceIndex);
                    } finally {
                        fis.close();
                    }
                }
            }
        }
    }

    private static void scanJar(final File jarFile, final Map<String, Set<String>> referenceIndex) throws IOException {
        final FileInputStream fis = new FileInputStream(jarFile);
        try {
            final ZipInputStream zis = new ZipInputStream(fis);
            try {
                ZipEntry entry;
                while ((entry = zis.getNextEntry()) != null) {
                    if (!entry.isDirectory() && entry.getName().endsWith(".class")) {
                        final String className = entry.getName().replace('/', '.').substring(0, entry.getName().length() - ".class".length());
                        if (!skip(className)) {
                            addClassReferences(className, zis, referenceIndex);
                        }
                    }
                    zis.closeEntry();
                }
            } finally {
                zis.close();
            }
        } finally {
            fis.close();
        }
    }

    private static void addClassReferences(final String className, final InputStream classInput, final Map<String, Set<String>> referenceIndex) throws IOException {
        try {
            final ClassReader reader = new ClassReader(classInput);
            final ClassCollector2.CachedClassReferenceCollector collector = new ClassCollector2.CachedClassReferenceCollector();
            reader.accept(collector, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
            for (final String referencedClass : collector.getReferencedClasses()) {
                if (skip(referencedClass)) {
                    continue;
                }
                Set<String> referencedBySet = referenceIndex.get(referencedClass);
                if (referencedBySet == null) {
                    referencedBySet = new LinkedHashSet<String>();
                    referenceIndex.put(referencedClass, referencedBySet);
                }
                referencedBySet.add(className);
            }
        } catch (Throwable ignore) {
        }
    }

    private static String toClassName(final File root, final File classFile) {
        final String rootPath = root.getAbsolutePath();
        final String classPath = classFile.getAbsolutePath();
        String rel = classPath.substring(rootPath.length() + 1);
        rel = rel.replace(File.separatorChar, '.');
        rel = rel.substring(0, rel.length() - ".class".length());
        return rel;
    }

    private static boolean isJar(final String name) {
        return name.endsWith(".jar") || name.endsWith(".JAR");
    }

    private static boolean skip(final String className) {
        if (className == null || className.length() == 0) {
            return true;
        }
        if (className.startsWith("java.")) {
            return true;
        }
        if (className.startsWith("javax.")) {
            return true;
        }
        if (className.startsWith("sun.")) {
            return true;
        }
        if (className.startsWith("com.sun.")) {
            return true;
        }
        if (className.startsWith("[")) {
            return true;
        }
        return false;
    }
}
