/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.builddecision;

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.builddecision.BuildDecisionData.Option;
import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Application;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Joiner;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.logging2.LogInterface;

/**
 * @author thomas
 * @date Nov 28, 2024
 *
 */
public class BuildDecisions {
    private static class BuildDecisionInfo {
        private Class<?>[] loadedImports;

        public Class<?>[] getLoadedImports() {
            return loadedImports;
        }

        public void setLoadedImports(Class<?>[] loadedImports) {
            this.loadedImports = loadedImports;
        }

        /**
         * @param loadedImports
         */
        public BuildDecisionInfo(Class<?>[] loadedImports) {
            this.loadedImports = loadedImports;
        }
    }

    /**
     *
     */
    private BuildDecisions() {
    }

    private final static Map<String, BuildDecisionInfo> INSTANCE = new HashMap<String, BuildDecisionInfo>();

    public static boolean add(final String tag, final Class<?>... loadedImports) {
        return INSTANCE.put(tag, new BuildDecisionInfo(loadedImports)) == null;
    }

    public static boolean contains(final String tag) {
        return INSTANCE.containsKey(tag);
    }

    public static boolean remove(final String tag) {
        return INSTANCE.remove(tag) != null;
    }

    public static File getProjectFolder(final Class<?> cls) {
        final URL url = Application.class.getResource("/" + cls.getName().replace(".", "/") + ".class");
        try {
            File file = new File(url.toURI()).getParentFile();
            for (int i = 0; i < cls.getName().split("\\.").length; i++) {
                file = file.getParentFile();
            }
            // Intellij
            if ("production".equals(file.getName())) {
                if ("bin".equals(file.getParentFile().getName())) {
                    return file.getParentFile().getParentFile();
                }
            }
            return file;
        } catch (final Throwable e) {
            throw new WTFException(e);
        }
    }

    /**
     *
     */
    public static void validate() {
        if (!ENABLED) {
            return;
        }
        if (System.getProperty("AWTEST") != null) {
            // skip this for test runs
            return;
        }
        final ArrayList<Exception> exceptions = new ArrayList<Exception>();
        try {
            final RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
            // Start-Command abrufen
            final String cp = runtimeMXBean.getClassPath();
            final String command = runtimeMXBean.getSystemProperties().get("sun.java.command");
            LogV3.info("Classpath: " + cp);
            LogV3.info("Command: " + command);
            if (command != null && !command.isEmpty()) {
                long started = Time.systemIndependentCurrentJVMTimeMillis();
                final String mainClass = command.split(" ")[0];
                final File projectFolder = getProjectFolder(Class.forName(mainClass));
                try {
                    // final File javaFile = new File(projectFolder, "src/" + mainClass.replace(".", "/") + ".java");
                    // final String java = IO.readFileToString(javaFile);
                    // final File mainProject = new File(cp.split(File.pathSeparator)[0]).getParentFile();
                    new ClassPathScanner<Throwable>() {
                        @Override
                        public void handle(final Class<?> type) throws Throwable {
                            try {
                                final BuildDecisionRequired anno = type.getAnnotation(BuildDecisionRequired.class);
                                if (anno != null) {
                                    BuildDecisionData s = new BuildDecisionData(anno, type);
                                    final List<Option> options = s.getOptions();
                                    Option usedTag = null;
                                    for (final Option o : options) {
                                        BuildDecisionInfo d = BuildDecisions.get(o.getTag());
                                        if (d != null) {
                                            usedTag = o;
                                            for (String imp : o.getImports()) {
                                                if (StringUtils.isEmpty(imp)) {
                                                    continue;
                                                }
                                                Class<?> cls = null;
                                                try {
                                                    cls = Class.forName(imp, false, BuildDecisions.class.getClassLoader());
                                                } catch (final Exception e) {
                                                    throw new Exception("Cannot load required class. Launcher: " + mainClass + "!\r\nEnsure that  " + imp + " is imported!" + "\r\nRequired by BuildTag " + usedTag + "\r\nIn " + s.getClass() + "", e);
                                                }
                                                if (!Arrays.asList(d.loadedImports).contains(cls)) {
                                                    throw new Exception("The class-reference fails in BuildDecisions.add(...) " + mainClass + "!\r\nEnsure that  " + imp + " is added as parameter \r\nRequired by BuildTag " + usedTag + "\r\nIn " + s.getClass() + "");
                                                }
                                                // if (!java.contains(imp)) {
                                                // throw new Exception("Could not find any required import in " + mainClass + "!\r\nEnsure
                                                // that " + imp + " is imported!" + "\r\nRequired by BuildTag " + usedTag + "\r\nIn " +
                                                // s.getClass() + "");
                                                // }
                                            }
                                            final String[] deps = o.getDependsOn();
                                            if (deps != null) {
                                                for (final String dep : deps) {
                                                    if (StringUtils.isEmpty(dep)) {
                                                        continue;
                                                    }
                                                    if (!contains(dep)) {
                                                        throw new Exception("The  BuildDecision Tag requires a dependency: Launcher:" + mainClass + " (" + Class.forName(mainClass).getSimpleName() + ".java:1)" + ":\r\nMissing Tag: " + dep);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if (usedTag == null) {
                                        throw new Exception("Could not find any BuildDecision Tag in " + mainClass + " (" + Class.forName(mainClass).getSimpleName() + ".java:1)" + ":\r\nChoose one:" + new Joiner("\r\n").prefix(true).join(options));
                                    }
                                }
                            } catch (Exception e) {
                                exceptions.add(e);
                            }
                        }
                    }.run();
                } catch (Throwable e) {
                    throw new Exception(e);
                } finally {
                    System.out.println("BuildDecision Scan took: " + (Time.systemIndependentCurrentJVMTimeMillis() - started));
                }
            } else {
                // throw new WTFException("Main not found");
            }
        } catch (final Exception e) {
            exceptions.add(e);
        } finally {
            for (Exception e : exceptions) {
                System.err.println(e.getMessage());
            }
            if (exceptions.size() > 0) {
                DebugMode.debugger();
            }
        }
    }

    /**
     * @param tag
     * @return
     */
    protected static BuildDecisionInfo get(String tag) {
        return INSTANCE.get(tag);
    }

    private static boolean ENABLED = true;

    /**
     * @param enabled
     *            TODO
     *
     */
    public static void setEnabled(boolean enabled) {
        ENABLED = enabled;
    }

    /**
     * @param defaultLogger
     */
    public static void status(LogInterface logger) {
        try {
            if (logger == null) {
                return;
            }
            RuntimeMXBean bean = ManagementFactory.getRuntimeMXBean();
            if (bean != null) {
                String cp = bean.getClassPath();
                if (cp != null) {
                    for (String s : cp.split(File.pathSeparator)) {
                        logger.info("ClathPath: " + s);
                    }
                }
            }
            for (Entry<String, BuildDecisionInfo> es : INSTANCE.entrySet()) {
                String str = "Build Decision: " + es.getKey();
                Class<?>[] imports = es.getValue().getLoadedImports();
                if (imports == null || imports.length == 0) {
                    str += "\r\n-no-classes-";
                } else {
                    // is this really 1.6? check!
                    for (Class<?> cls : imports) {
                        final String version = cls.getPackage().getImplementationVersion();
                        URL url = cls.getClassLoader().getResource(cls.getName().replace(".", "/") + ".class");
                        // No Logger init here!
                        str += ("\r\nLoaded library: " + cls.getPackage().getImplementationTitle() + "/" + cls.getPackage().getImplementationVendor() + "\r\nClass: " + cls + "\r\nVersion: " + version + "\r\nLoaded from: " + url);
                    }
                }
                logger.info(str);
            }
        } catch (Throwable e) {
            if (e instanceof InterruptedException) {
                DebugMode.debugger();
                Thread.currentThread().interrupt();
            }
            if (logger != null) {
                logger.log(e);
            }
        }
    }

    /**
     * @return
     */
    public static boolean isEnabled() {
        return ENABLED;
    }

    /**
     * @return
     */
    public static boolean isEmpty() {
        return INSTANCE.isEmpty();
    }
}
