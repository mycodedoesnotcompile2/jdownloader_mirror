/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.Files;
import org.appwork.utils.ZipContentHasher;
import org.appwork.utils.ZipContentHasher.Customizer;

/**
 * @author thomas
 * @date 24.02.2022
 *
 */
public abstract class ClassPathScanner<E extends Throwable> {
    public abstract void handle(Class<?> type) throws E;

    public static List<URL> getClassPath() {
        return getClassPath(Thread.currentThread().getContextClassLoader());
    }

    public static List<URL> getBootClassPath() {
        final List<URL> urls = new ArrayList<URL>();
        final String classPath = System.getProperty("sun.boot.class.path");
        if (classPath != null) {
            final String classPathEntries[] = classPath.split(File.pathSeparator);
            for (final String classPathEntry : classPathEntries) {
                try {
                    urls.add(new File(classPathEntry).toURI().toURL());
                } catch (MalformedURLException e) {
                    throw new WTFException(e);
                }
            }
        }
        return urls;
    }

    public static List<URL> getClassPath(final ClassLoader cl) {
        final List<URL> urls = new ArrayList<URL>();
        if (cl instanceof URLClassLoader) {
            urls.addAll(Arrays.asList(((URLClassLoader) cl).getURLs()));
        } else {
            final String classPath = System.getProperty("java.class.path");
            final String classPathEntries[] = classPath.split(File.pathSeparator);
            for (final String classPathEntry : classPathEntries) {
                try {
                    urls.add(new File(classPathEntry).toURI().toURL());
                } catch (MalformedURLException e) {
                    throw new WTFException(e);
                }
            }
        }
        if (urls.size() == 0) {
            throw new WTFException("empty classPath?!");
        } else {
            return urls;
        }
    }

    public void run() throws E {
        for (URL url : getClassPath()) {
            // System.out.println(url);
            File root;
            try {
                root = new File(url.toURI());
                if (root.isDirectory()) {
                    // AWTest.logInfoAnyway("Scan " + root);
                    List<File> files = Files.getFiles(true, true, root);
                    for (File f : files) {
                        String rel = Files.getRelativePath(root, f);
                        // LogV3.info("Scan File: " + f);
                        if (f.isFile()) {
                            if (rel.toUpperCase(Locale.ROOT).endsWith(".CLASS")) {
                                Class<?> cls = null;
                                try {
                                    cls = Class.forName(rel.replace("/", ".").substring(0, rel.length() - ".class".length()), false, Thread.currentThread().getContextClassLoader());
                                } catch (Throwable e) {
                                }
                                if (cls != null) {
                                    handle(cls);
                                }
                            }
                        }
                    }
                } else if (root.isFile() && root.getName().toUpperCase(Locale.ROOT).endsWith(".JAR")) {
                    FileInputStream fis = null;
                    try {
                        fis = new FileInputStream(root);
                        ;
                        for (Entry<String, byte[]> hash : ZipContentHasher.getHashes("", fis, new Customizer() {
                            public boolean handle(String path, ZipInputStream zipStream, ZipEntry entry, HashMap<String, byte[]> results) throws IOException {
                                // TODO Auto-generated method stub
                                return path.endsWith(".class");
                            }
                        }, false).entrySet()) {
                            if (hash.getKey().endsWith(".class")) {
                                String classname = hash.getKey().replaceAll("[\\/\\\\]", ".").replaceAll("\\.class$", "");
                                if (classname.startsWith("net.miginfocom.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.slf4j.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.tmatesoft.")) {
                                    continue;
                                }
                                if (classname.startsWith("com.")) {
                                    continue;
                                }
                                if (classname.startsWith("de.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.eclipse.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.tukaani.")) {
                                    continue;
                                }
                                if (classname.startsWith("cx.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.freedesktop.")) {
                                    continue;
                                }
                                if (classname.startsWith("net.sf.sevenzipjbinding.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.junit.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.fourthline.")) {
                                    continue;
                                }
                                if (classname.startsWith("META-INF.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.bouncycastle.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.mozilla.")) {
                                    continue;
                                }
                                if (classname.startsWith("net.sourceforge.")) {
                                    continue;
                                }
                                if (classname.startsWith("net.sf.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.antlr.")) {
                                    continue;
                                }
                                if (classname.startsWith("jsyntaxpane.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.seamless.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.brotli.")) {
                                    continue;
                                }
                                if (classname.startsWith("org.hamcrest.")) {
                                    continue;
                                }
                                if (classname.startsWith("junit.")) {
                                    continue;
                                }
                                if (classname.startsWith("dbus")) {
                                    continue;
                                }
                                if (classname.startsWith("org.schwering.")) {
                                    continue;
                                }
                                if (classname.startsWith("net.lingala.")) {
                                    continue;
                                }
                                if (classname.startsWith("module-info")) {
                                    continue;
                                }
                                Class<?> cls = null;
                                try {
                                    cls = Class.forName(classname);
                                } catch (Throwable e) {
                                    continue;
                                }
                                handle(cls);
                            }
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    } finally {
                        try {
                            if (fis != null) {
                                fis.close();
                            }
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            } catch (URISyntaxException e1) {
                e1.printStackTrace();
            }
        }
    }
}
