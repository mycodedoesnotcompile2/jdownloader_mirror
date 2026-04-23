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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
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
package org.appwork.testframework.tests;

import static org.appwork.testframework.AWTest.logInfoAnyway;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.ClassCollector2;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;

/**
 * Scans all classes on the classpath with ASM and ensures that production code does not depend on
 * code in test/tests/ide/testframework packages.
 */
public class AssertNoNormalCodeReferencesToTestIdeTest extends AWTest {
    public static void main(final String[] args) {
        run();
    }

    @Override
    public boolean isSkipOnUnchangedDependencies() {
        return false;
    }

    @Override
    public void runTest() throws Exception {
        final Set<String> allClasses = collectClassNames();
        final ClassCollector2 collector = new ClassCollector2();
        final Map<String, Set<String>> violations = new LinkedHashMap<String, Set<String>>();
        int scanned = 0;
        for (final String className : allClasses) {
            if (isIgnoredSourceClass(className)) {
                continue;
            }
            scanned++;
            final Set<String> references;
            try {
                references = collector.getReferencedClasses(className);
            } catch (final IOException e) {
                throw new Exception("Could not scan class " + className, e);
            }
            for (final String reference : references) {
                if (isForbiddenReference(reference)) {
                    Set<String> refs = violations.get(className);
                    if (refs == null) {
                        refs = new LinkedHashSet<String>();
                        violations.put(className, refs);
                    }
                    refs.add(reference);
                }
            }
        }
        if (!violations.isEmpty()) {
            final StringBuilder sb = new StringBuilder();
            sb.append("Found forbidden references from normal code to test/ide code:\n");
            for (final Map.Entry<String, Set<String>> entry : violations.entrySet()) {
                sb.append("  ").append(entry.getKey()).append(" -> ");
                sb.append(join(entry.getValue()));
                sb.append('\n');
            }
            throw new Exception(sb.toString());
        }
        logInfoAnyway("Scanned " + scanned + " production classes and found no forbidden references.");
    }

    private static Set<String> collectClassNames() throws Exception {
        final Set<String> classNames = new LinkedHashSet<String>();
        for (final URL url : ClassPathScanner.getClassPath()) {
            final File root;
            try {
                final URI uri = url.toURI();
                root = new File(uri);
            } catch (final Exception e) {
                continue;
            }
            if (root.isDirectory()) {
                collectFromDirectory(root, classNames);
            } else if (root.isFile() && root.getName().toLowerCase(Locale.ROOT).endsWith(".jar")) {
                collectFromJar(root, classNames);
            }
        }
        return classNames;
    }

    private static void collectFromDirectory(final File root, final Set<String> classNames) throws Exception {
        for (final File file : Files.getFiles(true, true, root)) {
            if (!file.isFile() || !file.getName().toLowerCase(Locale.ROOT).endsWith(".class")) {
                continue;
            }
            final String rel = Files.getRelativePath(root, file);
            if (StringUtils.isEmpty(rel)) {
                continue;
            }
            final String className = rel.replace('\\', '.').replace('/', '.').substring(0, rel.length() - ".class".length());
            if (!isSkippableClassFile(className)) {
                classNames.add(className);
            }
        }
    }

    private static void collectFromJar(final File root, final Set<String> classNames) throws IOException {
        final FileInputStream fis = new FileInputStream(root);
        try {
            final ZipInputStream zis = new ZipInputStream(fis);
            try {
                ZipEntry entry;
                while ((entry = zis.getNextEntry()) != null) {
                    if (entry.isDirectory()) {
                        continue;
                    }
                    final String name = entry.getName();
                    if (!name.toLowerCase(Locale.ROOT).endsWith(".class")) {
                        continue;
                    }
                    final String className = name.replace('/', '.').replace('\\', '.').substring(0, name.length() - ".class".length());
                    if (!isSkippableClassFile(className)) {
                        classNames.add(className);
                    }
                }
            } finally {
                zis.close();
            }
        } finally {
            fis.close();
        }
    }

    private static boolean isIgnoredSourceClass(final String className) {
        return !isOwnCode(className) || isPackageForbidden(className);
    }

    private static boolean isForbiddenReference(final String className) {
        return isPackageForbidden(className);
    }

    private static boolean isOwnCode(final String className) {
        return className != null && (className.startsWith("org.appwork.") || className.startsWith("ide."));
    }

    private static boolean isPackageForbidden(final String className) {
        if (StringUtils.isEmpty(className)) {
            return false;
        }
        final String lower = className.toLowerCase(Locale.ROOT);
        return lower.matches("(^|.*\\.)testframework(\\..*|$)") || lower.matches("(^|.*\\.)tests?(\\..*|$)") || lower.matches("(^|.*\\.)ide(\\..*|$)");
    }

    private static boolean isSkippableClassFile(final String className) {
        return "module-info".equals(className) || "package-info".equals(className) || className.endsWith(".module-info") || className.endsWith(".package-info");
    }

    private static String join(final Set<String> values) {
        final StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (final String value : values) {
            if (!first) {
                sb.append(", ");
            }
            sb.append(value);
            first = false;
        }
        return sb.toString();
    }
}
