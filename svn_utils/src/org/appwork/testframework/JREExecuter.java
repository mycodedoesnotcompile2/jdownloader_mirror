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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.serializer.Deser;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.ARCHFamily;
import org.appwork.utils.os.CrossSystem.OSFamily;
import org.appwork.testframework.executer.AdminExecuter;

/**
 * Executes code in a specific JRE. Uses {@link JREProvider} for JRE lookup and download; this class handles only execution (process spawn,
 * classpath, serialization, etc.).
 *
 * @author Thomas
 * @date 20.02.2026
 */
public class JREExecuter {
    /**
     * Options for JRE execution. Use the builder pattern to configure.
     * <p>
     * Example:
     *
     * <pre>
     * JreOptions.version(JavaVersion.JVM_1_6).bit(JREProvider.Bitness.BIT_32).jvmArgs("-Xmx512m")
     * JreOptions.version(JavaVersion.JVM_11_0).detailedVersion("11.0.2")
     * </pre>
     */
    public static class JreOptions {
        private JavaVersion           version;
        private JREProvider.Bitness   bitness;
        private List<String>   jvmArgs;
        private String         detailedVersion;
        private boolean        bitnessExplicit = false;
        private Map<String, String> env;

        private JreOptions() {
        }

        /**
         * Creates options with the specified Java version. Bitness is null (auto-detect from cache or download with fallback).
         */
        public static JreOptions version(final JavaVersion version) {
            final JreOptions opts = new JreOptions();
            opts.version = version;
            opts.bitness = null;
            return opts;
        }

        /**
         * Sets the bitness explicitly. No fallback will occur if this bitness is not available.
         */
        public JreOptions bit(final JREProvider.Bitness bitness) {
            this.bitness = bitness;
            this.bitnessExplicit = bitness != null;
            return this;
        }

        /**
         * Sets additional JVM arguments.
         */
        public JreOptions jvmArgs(final String... args) {
            if (args != null && args.length > 0) {
                this.jvmArgs = new ArrayList<String>();
                for (final String arg : args) {
                    this.jvmArgs.add(arg);
                }
            }
            return this;
        }

        /**
         * Sets additional JVM arguments.
         */
        public JreOptions jvmArgs(final List<String> args) {
            this.jvmArgs = args;
            return this;
        }

        /**
         * Sets a detailed version string (e.g. "1.6.0_26", "11.0.2"). When set, the system will search for a JRE that matches this exact
         * version in the cache and when prompting for a local path.
         */
        public JreOptions detailedVersion(final String detailedVersion) {
            this.detailedVersion = detailedVersion;
            return this;
        }

        /**
         * Sets environment variables to add to the child process (on top of inherited env). Used e.g. to pass
         * AdminExecuter helper connection env so the child can reuse the same helper.
         */
        public JreOptions env(final Map<String, String> env) {
            this.env = env;
            return this;
        }

        public JavaVersion getVersion() {
            return this.version;
        }

        public JREProvider.Bitness getBitness() {
            return this.bitness;
        }

        public List<String> getJvmArgs() {
            return this.jvmArgs;
        }

        public String getDetailedVersion() {
            return this.detailedVersion;
        }

        public boolean isBitnessExplicit() {
            return this.bitnessExplicit;
        }

        public Map<String, String> getEnv() {
            return this.env;
        }
    }

    /**
     * Result of JRE lookup containing the java binary path and the actual bitness used.
     */
    public static class JreResult {
        private final File                 javaBinary;
        private final JREProvider.Bitness  bitness;
        private final File                 cacheFolder;

        public JreResult(final File javaBinary, final JREProvider.Bitness bitness, final File cacheFolder) {
            this.javaBinary = javaBinary;
            this.bitness = bitness;
            this.cacheFolder = cacheFolder;
        }

        public File getJavaBinary() {
            return this.javaBinary;
        }

        public JREProvider.Bitness getBitness() {
            return this.bitness;
        }

        public File getCacheFolder() {
            return this.cacheFolder;
        }
    }

    /**
     * Central JRE lookup: delegates to {@link JREProvider#findOrDownload}.
     */
    public static JreResult findOrDownloadJRE(final JreOptions options) throws Exception {
        if (options == null) {
            throw new IllegalArgumentException("options must not be null");
        }
        final JavaVersion version = options.getVersion();
        if (version == null || version == JavaVersion.UNKNOWN) {
            throw new IllegalArgumentException("version must not be null or UNKNOWN");
        }
        final OSFamily osFamily = CrossSystem.getOSFamily();
        final JREProvider.JreLookupResult result = JREProvider.findOrDownload(version, osFamily, options.getBitness(), options.isBitnessExplicit(), options.getDetailedVersion());
        return new JreResult(result.getJavaBinary(), result.getBitness(), result.getCacheFolder());
    }

    public static void main(final String[] args) {
        try {
            System.out.println(JREProvider.ensureJRE(JavaVersion.JVM_11_0));
        } catch (final Exception e) {
            LogV3.log(e);
        }
    }

    // ==================== Execution API ====================
    /**
     * Executes a method on a class in a JRE with the specified options. The class is loaded directly in the child JRE (not serialized).
     * <p>
     * Example:
     *
     * <pre>
     * JREExecuter.executeInJRE(JreOptions.version(JavaVersion.JVM_11_0), MyTest.class, &quot;testMethod&quot;);
     * JREExecuter.executeInJRE(JreOptions.version(JavaVersion.JVM_1_8).jvmArgs(&quot;-Xmx512m&quot;), MyTest.class, &quot;testWithParams&quot;, &quot;arg1&quot;, 42);
     * JREExecuter.executeInJRE(JreOptions.version(JavaVersion.JVM_1_6).bit(JREProvider.Bitness.BIT_32).detailedVersion(&quot;1.6.0_45&quot;), MyTest.class, &quot;oldTest&quot;);
     * </pre>
     *
     * @param options
     *            JRE configuration options
     * @param clazz
     *            The class containing the method to invoke
     * @param methodName
     *            The name of the method to invoke (static or instance)
     * @param params
     *            Parameters to pass to the method (serialized via Deser/JSON)
     * @throws Exception
     *             If execution fails or the method throws an exception
     */
    public static void runInJRE(final JreOptions options, final Class<?> clazz, final String methodName, final Object... params) throws Exception {
        if (options == null) {
            throw new IllegalArgumentException("options must not be null");
        }
        if (options.getVersion() == null || options.getVersion() == JavaVersion.UNKNOWN) {
            throw new IllegalArgumentException("version must not be null or UNKNOWN");
        }
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        if (methodName == null || methodName.isEmpty()) {
            throw new IllegalArgumentException("methodName must not be null or empty");
        }
        if (System.getProperty(PostBuildRunner.POSTBUILDTEST) != null) {
            final boolean needMinimalRunner = getClassVersionForJava(options.getVersion()) < getClassFileVersion(JREMethodRunner.class);
            if (needMinimalRunner) {
                LogV3.info("Skipping JREExecuter execution in PostBuild mode (no source available for " + MinimalMethodRunner.class.getSimpleName() + " compilation).");
                return;
            }
        }
        final JreResult jreResult = findOrDownloadJRE(options);
        Map<String, String> envForChild = options.getEnv() != null ? new java.util.HashMap<String, String>(options.getEnv()) : new java.util.HashMap<String, String>();
        Map<String, String> helperEnv = AdminExecuter.getHelperConnectionEnv();
        if (helperEnv != null && !helperEnv.isEmpty()) {
            envForChild.putAll(helperEnv);
        }
        runInJRE(jreResult.getJavaBinary(), jreResult.getCacheFolder(), options.getJvmArgs(), envForChild.isEmpty() ? null : envForChild, clazz, methodName, params, options.getVersion(), jreResult.getBitness());
    }

    /**
     * Executes a {@link JRETestTask} in a JRE with the specified options. The task is serialized and passed to a child process that runs
     * {@link JRETaskRunner}. If the target JRE is older than the current runtime, the task's class (and dependencies) may be recompiled for
     * the target version so the child can deserialize it.
     * <p>
     * Example:
     *
     * <pre>
     * JREExecuter.runInJRE(JreOptions.version(JavaVersion.JVM_11_0), new JRETestTask() {
     *     &#64;Override
     *     public void run() throws Exception {
     *         System.out.println("Running in Java " + System.getProperty("java.version"));
     *     }
     * });
     * </pre>
     *
     * @param options
     *            JRE configuration options
     * @param task
     *            The task to run in the child JRE (must be {@link java.io.Serializable}; all referenced objects must be serializable)
     * @throws Exception
     *             If execution fails or the task throws an exception
     */
    public static void runInJRE(final JreOptions options, final JRETestTask task) throws Exception {
        if (options == null) {
            throw new IllegalArgumentException("options must not be null");
        }
        if (options.getVersion() == null || options.getVersion() == JavaVersion.UNKNOWN) {
            throw new IllegalArgumentException("version must not be null or UNKNOWN");
        }
        if (task == null) {
            throw new IllegalArgumentException("task must not be null");
        }
        if (System.getProperty(PostBuildRunner.POSTBUILDTEST) != null) {
            final int targetClassVersion = getClassVersionForJava(options.getVersion());
            final int runnerVersion = getClassFileVersion(JRETaskRunner.class);
            if (targetClassVersion < runnerVersion) {
                LogV3.info("Skipping JREExecuter.runInJRE in PostBuild mode (target JRE " + JREProvider.getFeatureVersion(options.getVersion()) + " older than " + JRETaskRunner.class.getSimpleName() + " class version).");
                return;
            }
        }
        final JreResult jreResult = findOrDownloadJRE(options);
        Map<String, String> envForChild = options.getEnv() != null ? new java.util.HashMap<String, String>(options.getEnv()) : new java.util.HashMap<String, String>();
        Map<String, String> helperEnv = AdminExecuter.getHelperConnectionEnv();
        if (helperEnv != null && !helperEnv.isEmpty()) {
            envForChild.putAll(helperEnv);
        }
        runTaskInJRE(jreResult.getJavaBinary(), jreResult.getCacheFolder(), options.getJvmArgs(), envForChild.isEmpty() ? null : envForChild, task, options.getVersion(), jreResult.getBitness());
    }

    /**
     * Executes a serialized JRETestTask in a child JRE process via JRETaskRunner.
     */
    private static void runTaskInJRE(final File javaBinary, final File workFolder, final List<String> jvmArgs, final Map<String, String> envForChild, final JRETestTask task, final JavaVersion version, final JREProvider.Bitness bitness) throws Exception {
        final int targetClassVersion = getClassVersionForJava(version);
        checkClassVersionCompatibility(version, JRETaskRunner.class);
        final Class<?> taskClass = task.getClass();
        File compiledClassesDir = null;
        if (getClassFileVersion(taskClass) > targetClassVersion) {
            compiledClassesDir = ensureCompatibleUserClass(version, workFolder, taskClass);
        } else {
            checkClassVersionCompatibility(version, taskClass);
        }
        workFolder.mkdirs();
        final File taskFile = File.createTempFile("jretask_", ".bin", workFolder);
        final File exceptionFile = new File(taskFile.getAbsolutePath() + JRETaskRunner.EXCEPTION_FILE_SUFFIX);
        try {
            final FileOutputStream fos = new FileOutputStream(taskFile);
            try {
                final ObjectOutputStream oos = new ObjectOutputStream(fos);
                try {
                    oos.writeObject(task);
                } finally {
                    oos.close();
                }
            } finally {
                fos.close();
            }
            String classpath = System.getProperty("java.class.path");
            if (compiledClassesDir != null) {
                classpath = compiledClassesDir.getAbsolutePath() + File.pathSeparator + classpath;
            }
            final List<String> command = new ArrayList<String>();
            command.add(javaBinary.getAbsolutePath());
            if (jvmArgs != null) {
                command.addAll(jvmArgs);
            }
            command.add("-cp");
            command.add(classpath);
            command.add(JRETaskRunner.class.getName());
            command.add(taskFile.getAbsolutePath());
            LogV3.info("Starting JRE task " + taskClass.getName() + " in " + javaBinary.getAbsolutePath());
            final ProcessBuilder pb = new ProcessBuilder(command);
            pb.redirectErrorStream(false);
            if (envForChild != null && !envForChild.isEmpty()) {
                pb.environment().putAll(envForChild);
            }
            final Process process = pb.start();
            final Thread stdoutThread = createOutputForwarder(process.getInputStream(), false, version, bitness);
            final Thread stderrThread = createOutputForwarder(process.getErrorStream(), true, version, bitness);
            stdoutThread.start();
            stderrThread.start();
            final int exitCode = process.waitFor();
            stdoutThread.join(5000);
            stderrThread.join(5000);
            if (exitCode != 0) {
                final JRETaskRunner.SerializedException serializedException = readExceptionFromFileDirectly(exceptionFile);
                if (serializedException != null) {
                    final Throwable original = serializedException.getOriginalException();
                    if (original != null) {
                        if (original instanceof Exception) {
                            throw (Exception) original;
                        } else if (original instanceof Error) {
                            throw (Error) original;
                        } else {
                            throw new JRETaskExecutionException(serializedException, exitCode);
                        }
                    } else {
                        throw new JRETaskExecutionException(serializedException, exitCode);
                    }
                }
                String errorMessage;
                switch (exitCode) {
                case JRETaskRunner.EXIT_TASK_EXCEPTION:
                    errorMessage = "Task threw an exception in child JRE (no details available)";
                    break;
                case JRETaskRunner.EXIT_DESERIALIZATION_FAIL:
                    errorMessage = "Failed to deserialize task in child JRE. Ensure the task class and all its fields are serializable and compatible with the target JRE.";
                    break;
                case JRETaskRunner.EXIT_INVALID_ARGS:
                    errorMessage = "Invalid arguments passed to " + JRETaskRunner.class.getSimpleName();
                    break;
                default:
                    errorMessage = "Child JRE process exited with code " + exitCode;
                }
                throw new JRETaskExecutionException(errorMessage, exitCode);
            }
        } finally {
            taskFile.delete();
            if (exceptionFile.exists()) {
                exceptionFile.delete();
            }
        }
    }
    private static void runInJRE(final File javaBinary, final File workFolder, final List<String> jvmArgs, final Map<String, String> envForChild, final Class<?> clazz, final String methodName, final Object[] params, final JavaVersion version, final JREProvider.Bitness bitness) throws Exception {
        final int targetClassVersion = getClassVersionForJava(version);
        final boolean useMinimalRunner = targetClassVersion < getClassFileVersion(JREMethodRunner.class);
        File compiledClassesDir = null;
        File minimalRunnerDir = null;
        if (useMinimalRunner) {
            minimalRunnerDir = ensureCompatibleMinimalRunner(version, workFolder);
            compiledClassesDir = ensureCompatibleUserClass(version, workFolder, clazz);
        } else {
            checkClassVersionCompatibility(version, JREMethodRunner.class);
            checkClassVersionCompatibility(version, clazz);
        }
        if (useMinimalRunner && params != null && params.length > 0) {
            throw new IllegalArgumentException(MinimalMethodRunner.class.getSimpleName() + " does not support parameters. Either compile your project with lower Java compliance level, or use a method without parameters for Java " + JREProvider.getFeatureVersion(version) + " tests.");
        }
        workFolder.mkdirs();
        File paramsFile = null;
        final File exceptionFile = File.createTempFile("jremethod_", ".exception", workFolder);
        try {
            if (!useMinimalRunner && params != null && params.length > 0) {
                paramsFile = File.createTempFile("jremethod_", ".params", workFolder);
                final String json = Deser.toString(params);
                IO.secureWrite(paramsFile, json, SYNC.META_AND_DATA);
            }
            String classpath = System.getProperty("java.class.path");
            if (useMinimalRunner && minimalRunnerDir != null) {
                classpath = minimalRunnerDir.getAbsolutePath() + File.pathSeparator + classpath;
            }
            if (useMinimalRunner && compiledClassesDir != null) {
                classpath = compiledClassesDir.getAbsolutePath() + File.pathSeparator + classpath;
            }
            final List<String> command = new ArrayList<String>();
            command.add(javaBinary.getAbsolutePath());
            if (jvmArgs != null) {
                command.addAll(jvmArgs);
            }
            command.add("-cp");
            command.add(classpath);
            if (useMinimalRunner) {
                command.add(MinimalMethodRunner.class.getName());
                command.add(clazz.getName());
                command.add(methodName);
                command.add(exceptionFile.getAbsolutePath());
            } else {
                command.add(JREMethodRunner.class.getName());
                command.add(clazz.getName());
                command.add(methodName);
                command.add(paramsFile != null ? paramsFile.getAbsolutePath() : "null");
                command.add(exceptionFile.getAbsolutePath());
            }
            LogV3.info("Starting JRE method: " + clazz.getName() + "." + methodName + "() in " + javaBinary.getAbsolutePath() + (useMinimalRunner ? " (using " + MinimalMethodRunner.class.getSimpleName() + ")" : ""));
            final ProcessBuilder pb = new ProcessBuilder(command);
            pb.redirectErrorStream(false);
            if (envForChild != null && !envForChild.isEmpty()) {
                pb.environment().putAll(envForChild);
            }
            final Process process = pb.start();
            final Thread stdoutThread = createOutputForwarder(process.getInputStream(), false, version, bitness);
            final Thread stderrThread = createOutputForwarder(process.getErrorStream(), true, version, bitness);
            stdoutThread.start();
            stderrThread.start();
            final int exitCode = process.waitFor();
            stdoutThread.join(5000);
            stderrThread.join(5000);
            if (exitCode != 0) {
                if (useMinimalRunner) {
                    final String textException = readTextExceptionFromFile(exceptionFile);
                    if (textException != null && !textException.isEmpty()) {
                        throw new JRETaskExecutionException("Method threw an exception in child JRE:\n" + textException, exitCode);
                    }
                    String errorMessage;
                    switch (exitCode) {
                    case 1:
                        errorMessage = "Method threw an exception in child JRE (no details available)";
                        break;
                    case 2:
                        errorMessage = "Class or method not found in child JRE: " + clazz.getName() + "." + methodName;
                        break;
                    case 3:
                        errorMessage = "Invalid arguments passed to " + MinimalMethodRunner.class.getSimpleName();
                        break;
                    default:
                        errorMessage = "Child JRE process exited with code " + exitCode;
                    }
                    throw new JRETaskExecutionException(errorMessage, exitCode);
                } else {
                    final JRETaskRunner.SerializedException serializedException = readExceptionFromFileDirectly(exceptionFile);
                    if (serializedException != null) {
                        final Throwable original = serializedException.getOriginalException();
                        if (original != null) {
                            if (original instanceof Exception) {
                                throw (Exception) original;
                            } else if (original instanceof Error) {
                                throw (Error) original;
                            } else {
                                throw new JRETaskExecutionException(serializedException, exitCode);
                            }
                        } else {
                            throw new JRETaskExecutionException(serializedException, exitCode);
                        }
                    }
                    String errorMessage;
                    switch (exitCode) {
                    case JREMethodRunner.EXIT_METHOD_EXCEPTION:
                        errorMessage = "Method threw an exception in child JRE (no details available)";
                        break;
                    case JREMethodRunner.EXIT_CLASS_NOT_FOUND:
                        errorMessage = "Class or method not found in child JRE: " + clazz.getName() + "." + methodName;
                        break;
                    case JREMethodRunner.EXIT_PARAM_DESER_FAIL:
                        errorMessage = "Failed to deserialize parameters in child JRE";
                        break;
                    case JREMethodRunner.EXIT_INSTANTIATION_FAIL:
                        errorMessage = "Failed to instantiate class in child JRE: " + clazz.getName() + ". Ensure it has a no-arg constructor.";
                        break;
                    case JREMethodRunner.EXIT_INVALID_ARGS:
                        errorMessage = "Invalid arguments passed to JREMethodRunner";
                        break;
                    default:
                        errorMessage = "Child JRE process exited with code " + exitCode;
                    }
                    throw new JRETaskExecutionException(errorMessage, exitCode);
                }
            }
        } finally {
            if (paramsFile != null) {
                paramsFile.delete();
            }
            exceptionFile.delete();
        }
    }

    /**
     * Reads the serialized exception from the given file if it exists.
     */
    private static JRETaskRunner.SerializedException readExceptionFromFileDirectly(final File exceptionFile) {
        if (!exceptionFile.exists() || exceptionFile.length() == 0) {
            return null;
        }
        try {
            final FileInputStream fis = new FileInputStream(exceptionFile);
            try {
                final ObjectInputStream ois = new ObjectInputStream(fis);
                try {
                    return (JRETaskRunner.SerializedException) ois.readObject();
                } finally {
                    ois.close();
                }
            } finally {
                fis.close();
            }
        } catch (final Exception e) {
            LogV3.warning("Failed to read exception file: " + e.getMessage());
            return null;
        }
    }

    /**
     * Reads exception information from a text file written by {@link MinimalMethodRunner}.
     */
    private static String readTextExceptionFromFile(final File exceptionFile) {
        if (!exceptionFile.exists() || exceptionFile.length() == 0) {
            return null;
        }
        try {
            return IO.readFileToString(exceptionFile);
        } catch (final Exception e) {
            LogV3.warning("Failed to read text exception file: " + e.getMessage());
            return null;
        }
    }

    /**
     * Returns the cache folder for JRE installations using current system OS and bitness.
     */
    public static File getJRECacheFolder(final JavaVersion version) {
        return JREProvider.getJRECacheFolder(version);
    }

    /**
     * Returns the cache folder for JRE installations with specified bitness and current system OS.
     */
    public static File getJRECacheFolder(final JavaVersion version, final JREProvider.Bitness bitness) {
        return JREProvider.getJRECacheFolder(version, bitness);
    }

    /**
     * Returns the cache folder for JRE installations with specified OS and bitness.
     * <p>
     * Path format: tmp/jre_cache/{version}/{os}/{bitness}/
     */
    public static File getJRECacheFolder(final JavaVersion version, final OSFamily osFamily, final JREProvider.Bitness bitness) {
        return JREProvider.getJRECacheFolder(version, osFamily, bitness);
    }

    /**
     * Ensures a MinimalMethodRunner compatible with the target JRE is available. If the in-repo MinimalMethodRunner.class is too new (e.g.
     * project compiled with Java 8 but target is Java 7), compiles MinimalMethodRunner.java for the target version.
     *
     * @return Directory containing the compatible MinimalMethodRunner.class, or null to use the class from the classpath
     */
    private static File ensureCompatibleMinimalRunner(final JavaVersion targetVersion, final File workFolder) throws Exception {
        final int targetClassVersion = getClassVersionForJava(targetVersion);
        final int runnerVersion;
        try {
            runnerVersion = getClassFileVersion(MinimalMethodRunner.class);
        } catch (final Exception e) {
            LogV3.warning("Could not read MinimalMethodRunner class version: " + e.getMessage());
            return null;
        }
        if (runnerVersion <= targetClassVersion) {
            return null;
        }
        final File runnerDir = new File(workFolder, "compiled_minimal_runner");
        final File runnerClassFile = new File(runnerDir, MinimalMethodRunner.class.getName().replace('.', '/') + ".class");
        if (runnerClassFile.exists()) {
            try {
                final int compiledVersion = getClassFileVersionFromFile(runnerClassFile);
                if (compiledVersion <= targetClassVersion) {
                    LogV3.info("Using pre-compiled " + MinimalMethodRunner.class.getSimpleName() + " for Java " + JREProvider.getFeatureVersion(targetVersion));
                    return runnerDir;
                }
            } catch (final Exception e) {
                // Recompile
            }
        }
        final File javacBinary = findSystemJavac();
        if (javacBinary == null || !javacBinary.exists()) {
            throw new UnsupportedClassVersionError(MinimalMethodRunner.class.getSimpleName() + " was compiled for Java " + getJavaVersionForClassVersion(runnerVersion) + " but target is Java " + getJavaVersionForClassVersion(targetClassVersion) + ". No javac found in system JDK to compile a compatible runner. Ensure you are running from a JDK (not JRE) or set project compiler compliance to " + getJavaVersionForClassVersion(targetClassVersion) + ".");
        }
        final List<File> sourcePaths = findSourcePaths();
        if (sourcePaths.isEmpty()) {
            throw new IOException("No source paths found to compile " + MinimalMethodRunner.class.getSimpleName());
        }
        final File runnerSourceFile = findSourceFileForClass(MinimalMethodRunner.class.getName(), sourcePaths);
        if (runnerSourceFile == null || !runnerSourceFile.exists()) {
            throw new UnsupportedClassVersionError(MinimalMethodRunner.class.getSimpleName() + " was compiled for Java " + getJavaVersionForClassVersion(runnerVersion) + " but target is Java " + getJavaVersionForClassVersion(targetClassVersion) + ". Source file not found for on-the-fly compilation. Set project compiler compliance to " + getJavaVersionForClassVersion(targetClassVersion) + " or ensure " + MinimalMethodRunner.class.getName() + ".java is on the source path.");
        }
        LogV3.info("Compiling " + MinimalMethodRunner.class.getSimpleName() + " for Java " + JREProvider.getFeatureVersion(targetVersion) + " (in-repo class is Java " + getJavaVersionForClassVersion(runnerVersion) + ")...");
        deleteDirectory(runnerDir);
        runnerDir.mkdirs();
        // Source path root: directory that contains package root "org" (e.g. .../src/main/java for org.appwork.testframework)
        File sourceRoot = runnerSourceFile.getParentFile();
        for (int i = 0; i < 3; i++) {
            sourceRoot = sourceRoot.getParentFile();
            if (sourceRoot == null) {
                throw new IOException("Could not determine source root for " + runnerSourceFile.getAbsolutePath());
            }
        }
        final String targetVersionStr = JREProvider.getFeatureVersion(targetVersion) <= 8 ? "1." + JREProvider.getFeatureVersion(targetVersion) : String.valueOf(JREProvider.getFeatureVersion(targetVersion));
        final List<File> sourceFiles = new ArrayList<File>();
        sourceFiles.add(runnerSourceFile);
        final StringBuilder compileOutput = new StringBuilder();
        final int exitCode = compileWithArgFile(javacBinary, runnerDir, targetVersionStr, sourceRoot.getAbsolutePath(), sourceFiles, compileOutput);
        if (exitCode != 0 || !runnerClassFile.exists()) {
            LogV3.severe("Failed to compile " + MinimalMethodRunner.class.getSimpleName() + " for Java " + JREProvider.getFeatureVersion(targetVersion) + ":\n" + compileOutput.toString());
            throw new IOException("Failed to compile " + MinimalMethodRunner.class.getSimpleName() + " for Java " + JREProvider.getFeatureVersion(targetVersion) + ". See log output above for details.");
        }
        LogV3.info(MinimalMethodRunner.class.getSimpleName() + " compiled successfully for Java " + JREProvider.getFeatureVersion(targetVersion));
        return runnerDir;
    }

    /**
     * Checks if the given class is compatible with the target JRE version. Throws an exception with a helpful message if the class was
     * compiled for a newer Java version than the target.
     */
    private static void checkClassVersionCompatibility(final JavaVersion targetVersion, final Class<?> clazz) throws Exception {
        final int targetClassVersion = getClassVersionForJava(targetVersion);
        final int actualClassVersion = getClassFileVersion(clazz);
        if (actualClassVersion > targetClassVersion) {
            final String targetJavaVersion = getJavaVersionForClassVersion(targetClassVersion);
            final String actualJavaVersion = getJavaVersionForClassVersion(actualClassVersion);
            throw new UnsupportedClassVersionError("Class " + clazz.getName() + " was compiled for Java " + actualJavaVersion + " (class version " + actualClassVersion + ") but target JRE is Java " + targetJavaVersion + " (class version " + targetClassVersion + "). " + "To run tests in older JREs, you need to compile your project with a lower compiler compliance level (e.g. 1.6) in Eclipse: " + "Project Properties -> Java Compiler -> Compiler compliance level");
        }
    }

    /**
     * Ensures the user class and all its dependencies are compiled for the target JRE version. Uses ASM to find all referenced classes and
     * compiles those that need recompilation.
     *
     * @return Directory containing compiled classes, or null if class is already compatible
     */
    private static File ensureCompatibleUserClass(final JavaVersion targetVersion, final File workFolder, final Class<?> clazz) throws Exception {
        final int targetClassVersion = getClassVersionForJava(targetVersion);
        final int classVersion = getClassFileVersion(clazz);
        if (classVersion <= targetClassVersion) {
            return null;
        }
        final File compiledDir = new File(workFolder, "compiled_user_" + clazz.getSimpleName());
        final File javacBinary = findSystemJavac();
        if (javacBinary == null || !javacBinary.exists()) {
            throw new UnsupportedClassVersionError("Class " + clazz.getName() + " was compiled for Java " + getJavaVersionForClassVersion(classVersion) + " but target is Java " + getJavaVersionForClassVersion(targetClassVersion) + ". No javac found in system JDK to recompile.");
        }
        LogV3.info("Recompiling " + clazz.getName() + " for Java " + JREProvider.getFeatureVersion(targetVersion) + "...");
        LogV3.info("Searching for source paths (classpath and project directories)...");
        final List<File> sourcePaths = findSourcePaths();
        if (sourcePaths.isEmpty()) {
            throw new IOException("No source paths found for recompilation");
        }
        LogV3.info("Found " + sourcePaths.size() + " source path(s). Resolving dependencies for " + clazz.getName() + " (target class version " + targetClassVersion + ")...");
        Set<String> classesToCompile = findClassesToRecompile(clazz.getName(), targetClassVersion);
        if (classesToCompile.isEmpty()) {
            return null;
        }
        LogV3.info("Found " + classesToCompile.size() + " class(es) to recompile. Resolving source files...");
        deleteDirectory(compiledDir);
        compiledDir.mkdirs();
        final String targetVersionStr = JREProvider.getFeatureVersion(targetVersion) <= 8 ? "1." + JREProvider.getFeatureVersion(targetVersion) : String.valueOf(JREProvider.getFeatureVersion(targetVersion));
        final StringBuilder sourcePathStr = new StringBuilder();
        for (int i = 0; i < sourcePaths.size(); i++) {
            if (i > 0) {
                sourcePathStr.append(File.pathSeparator);
            }
            sourcePathStr.append(sourcePaths.get(i).getAbsolutePath());
        }
        List<File> sourceFiles = collectSourceFiles(classesToCompile, sourcePaths);
        LogV3.info("Using compiler: " + javacBinary.getAbsolutePath() + " (compiling " + sourceFiles.size() + " source file(s) with -target " + JREProvider.getFeatureVersion(targetVersion) + ")...");
        StringBuilder compileOutput = new StringBuilder();
        int exitCode = compileWithArgFile(javacBinary, compiledDir, targetVersionStr, sourcePathStr.toString(), sourceFiles, compileOutput);
        if (exitCode != 0) {
            LogV3.severe("Compilation failed for Java " + JREProvider.getFeatureVersion(targetVersion) + ":\n" + compileOutput.toString());
            throw new IOException("Compilation failed for Java " + JREProvider.getFeatureVersion(targetVersion) + ". See log output above for details.");
        }
        final String classFileName = clazz.getName().replace('.', '/') + ".class";
        final File compiledClassFile = new File(compiledDir, classFileName);
        if (!compiledClassFile.exists()) {
            throw new IOException("Compilation succeeded but main class file not found: " + compiledClassFile.getAbsolutePath());
        }
        LogV3.info("Compiled " + sourceFiles.size() + " classes successfully for Java " + JREProvider.getFeatureVersion(targetVersion));
        return compiledDir;
    }

    private static List<File> collectSourceFiles(final Set<String> classesToCompile, final List<File> sourcePaths) {
        final List<File> sourceFiles = new ArrayList<File>();
        final Set<String> addedSourceFiles = new HashSet<String>();
        final List<String> missingSourceClasses = new ArrayList<String>();
        for (final String className : classesToCompile) {
            final File sourceFile = findSourceFileForClass(className, sourcePaths);
            if (sourceFile != null && sourceFile.exists()) {
                final String absolutePath = sourceFile.getAbsolutePath();
                if (!addedSourceFiles.contains(absolutePath)) {
                    addedSourceFiles.add(absolutePath);
                    sourceFiles.add(sourceFile);
                }
            } else {
                missingSourceClasses.add(className);
            }
        }
        if (sourceFiles.isEmpty()) {
            throw new RuntimeException("No source files found for recompilation. Missing sources for: " + missingSourceClasses);
        }
        if (!missingSourceClasses.isEmpty()) {
            LogV3.warning("Some dependencies have no source available (will use existing .class files): " + missingSourceClasses);
        }
        return sourceFiles;
    }

    /**
     * Compiles source files using javac with an argfile to avoid command line length limits.
     *
     * @param outputCollector
     *            if not null, compiler output is appended here
     * @return exit code (0 = success)
     */
    private static int compileWithArgFile(final File javacBinary, final File compiledDir, final String targetVersionStr, final String sourcePathStr, final List<File> sourceFiles, final StringBuilder outputCollector) throws Exception {
        final File argFile = new File(compiledDir, "javac_args.txt");
        final StringBuilder argFileContent = new StringBuilder();
        argFileContent.append("-encoding\n");
        argFileContent.append("UTF-8\n");
        argFileContent.append("-source\n");
        argFileContent.append(targetVersionStr).append("\n");
        argFileContent.append("-target\n");
        argFileContent.append(targetVersionStr).append("\n");
        argFileContent.append("-sourcepath\n");
        argFileContent.append("\"").append(sourcePathStr.replace("\\", "\\\\")).append("\"\n");
        argFileContent.append("-cp\n");
        argFileContent.append("\"").append(System.getProperty("java.class.path").replace("\\", "\\\\")).append("\"\n");
        argFileContent.append("-d\n");
        argFileContent.append("\"").append(compiledDir.getAbsolutePath().replace("\\", "\\\\")).append("\"\n");
        for (final File sourceFile : sourceFiles) {
            argFileContent.append("\"").append(sourceFile.getAbsolutePath().replace("\\", "\\\\")).append("\"\n");
        }
        IO.secureWrite(argFile, argFileContent.toString(), SYNC.META_AND_DATA);
        final List<String> javacArgs = new ArrayList<String>();
        javacArgs.add(javacBinary.getAbsolutePath());
        javacArgs.add("@" + argFile.getAbsolutePath());
        final ProcessBuilder pb = new ProcessBuilder(javacArgs);
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        while ((line = reader.readLine()) != null) {
            if (outputCollector != null) {
                outputCollector.append(line).append("\n");
            }
        }
        final int exitCode = process.waitFor();
        reader.close();
        return exitCode;
    }

    /**
     * Uses ClassCollector2 (cached, fast) to find all classes that need to be recompiled for the target version.
     */
    private static Set<String> findClassesToRecompile(final String className, final int targetClassVersion) throws Exception {
        final Set<String> needsRecompile = new HashSet<String>();
        final ClassLoader loader = JREExecuter.class.getClassLoader();
        final Map<String, String> allClasses = new ClassCollector2().getClasses(className, true);
        for (final String clazz : allClasses.keySet()) {
            try {
                final int version = getClassFileVersionFromClassName(clazz, loader);
                if (version > targetClassVersion) {
                    needsRecompile.add(clazz);
                }
            } catch (final Error e) {
                needsRecompile.add(clazz);
            } catch (final Exception e) {
                e.printStackTrace();
                // Skip classes that can't be loaded or read
            }
        }
        needsRecompile.add(className);
        return needsRecompile;
    }

    /**
     * Reads the class file major version from the classloader resource (no Class.forName). Returns -1 if not readable. Reads only the first
     * 8 bytes (magic + minor + major) so unknown future Java versions are still compared correctly.
     */
    private static int getClassFileVersionFromClassName(final String className, final ClassLoader loader) throws IOException {
        final String path = className.replace('.', '/') + ".class";
        final InputStream is = loader.getResourceAsStream(path);
        if (is == null) {
            return -1;
        }
        try {
            return JavaVersion.readClassJVMVersion(is).classID;
        } finally {
            is.close();
        }
    }

    /**
     * Finds the source file for a class name.
     */
    private static File findSourceFileForClass(final String className, final List<File> sourcePaths) {
        String outerClassName = className;
        final int dollarIndex = className.indexOf('$');
        if (dollarIndex > 0) {
            outerClassName = className.substring(0, dollarIndex);
        }
        final String relativePath = outerClassName.replace('.', '/') + ".java";
        for (final File sourcePath : sourcePaths) {
            final File sourceFile = new File(sourcePath, relativePath);
            if (sourceFile.exists()) {
                return sourceFile;
            }
        }
        return null;
    }

    /**
     * Finds the source file for a class by searching common source directories.
     */
    /**
     * Finds all source paths by looking at the classpath and finding corresponding source directories.
     */
    private static List<File> findSourcePaths() {
        final List<File> sourcePaths = new ArrayList<File>();
        final Set<String> seen = new HashSet<String>();
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

    private static void addSourcePathIfExists(final File parent, final String subPath, final List<File> sourcePaths, final Set<String> seen) {
        final File srcDir = new File(parent, subPath);
        if (srcDir.exists() && srcDir.isDirectory()) {
            final String canonical;
            try {
                canonical = srcDir.getCanonicalPath();
            } catch (final IOException e) {
                return;
            }
            if (!seen.contains(canonical)) {
                seen.add(canonical);
                sourcePaths.add(srcDir);
            }
        }
    }

    /**
     * Recursively deletes a directory.
     */
    private static void deleteDirectory(final File dir) {
        if (dir.exists()) {
            final File[] files = dir.listFiles();
            if (files != null) {
                for (final File file : files) {
                    if (file.isDirectory()) {
                        deleteDirectory(file);
                    } else {
                        file.delete();
                    }
                }
            }
            dir.delete();
        }
    }

    /**
     * Finds the javac binary from the current system JDK.
     */
    private static File findSystemJavac() {
        final String javaHome = System.getProperty("java.home");
        if (javaHome == null) {
            return null;
        }
        File javaHomeDir = new File(javaHome);
        final String javacName = CrossSystem.getOSFamily() == OSFamily.WINDOWS ? "javac.exe" : "javac";
        File javac = new File(javaHomeDir, "bin/" + javacName);
        if (javac.exists()) {
            return javac;
        }
        if (javaHomeDir.getName().equals("jre")) {
            javaHomeDir = javaHomeDir.getParentFile();
            javac = new File(javaHomeDir, "bin/" + javacName);
            if (javac.exists()) {
                return javac;
            }
        }
        return null;
    }

    /**
     * Reads class file version from a .class file.
     */
    private static int getClassFileVersionFromFile(final File classFile) throws IOException {
        final FileInputStream fis = new FileInputStream(classFile);
        try {
            return JavaVersion.readClassJVMVersion(fis).classID;
        } finally {
            fis.close();
        }
    }

    /**
     * Searches for javac in the JRE/JDK folder.
     */
    /**
     * Returns the class file major version for a JavaVersion.
     */
    private static int getClassVersionForJava(final JavaVersion version) {
        return version.classID;
    }

    /**
     * Returns a human-readable Java version string for a class file version.
     */
    private static String getJavaVersionForClassVersion(final int classVersion) {
        for (JavaVersion version : JavaVersion.values()) {
            if (version.classID == classVersion) {
                return version.getVersionString();
            }
        }
        throw new WTFException("Unknown classVersion:" + classVersion);
    }

    /**
     * Reads the class file major version from a class.
     */
    private static int getClassFileVersion(final Class<?> clazz) throws IOException {
        final String resourceName = "/" + clazz.getName().replace('.', '/') + ".class";
        final InputStream is = clazz.getResourceAsStream(resourceName);
        try {
            return JavaVersion.readClassJVMVersion(is).classID;
        } finally {
            is.close();
        }
    }

    /**
     * Creates a thread that forwards process output to LogV3.
     */
    private static Thread createOutputForwarder(final InputStream inputStream, final boolean isError, final JavaVersion jreVersion, final JREProvider.Bitness bitness) {
        final String bitSuffix = bitness == JREProvider.Bitness.BIT_64 ? "_64" : "_32";
        final String versionStr = jreVersion != null && jreVersion != JavaVersion.UNKNOWN ? jreVersion.getVersionString() : String.valueOf(JREProvider.getFeatureVersion(jreVersion));
        final String prefix = "[JRE" + versionStr + bitSuffix + "] ";
        return new Thread("JRETask-" + (isError ? "stderr" : "stdout")) {
            @Override
            public void run() {
                try {
                    final BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                    String line;
                    while ((line = reader.readLine()) != null) {
                        if (isError) {
                            LogV3.warning(prefix + line);
                        } else {
                            LogV3.info(prefix + line);
                        }
                    }
                } catch (final IOException e) {
                    // Stream closed, ignore
                }
            }
        };
    }

    /**
     * Exception thrown when a JRE task execution fails. Contains detailed information about the exception that occurred in the child JRE
     * process.
     */
    public static class JRETaskExecutionException extends Exception {
        private static final long                       serialVersionUID = 1L;
        private final int                               exitCode;
        private final JRETaskRunner.SerializedException serializedException;

        public JRETaskExecutionException(final String message, final int exitCode) {
            super(message);
            this.exitCode = exitCode;
            this.serializedException = null;
        }

        public JRETaskExecutionException(final JRETaskRunner.SerializedException serializedException, final int exitCode) {
            super(serializedException.getExceptionClass() + ": " + serializedException.getMessage());
            this.exitCode = exitCode;
            this.serializedException = serializedException;
        }

        public int getExitCode() {
            return this.exitCode;
        }

        /**
         * Returns the original exception class name from the child JRE, or null if not available.
         */
        public String getRemoteExceptionClass() {
            return this.serializedException != null ? this.serializedException.getExceptionClass() : null;
        }

        /**
         * Returns the original exception message from the child JRE, or null if not available.
         */
        public String getRemoteMessage() {
            return this.serializedException != null ? this.serializedException.getMessage() : null;
        }

        /**
         * Returns the stack trace from the child JRE as a string, or null if not available.
         */
        public String getRemoteStackTrace() {
            return this.serializedException != null ? this.serializedException.getStackTrace() : null;
        }

        @Override
        public String toString() {
            if (this.serializedException != null) {
                return "JRETaskExecutionException (exit code " + this.exitCode + "):\n" + this.serializedException.toString();
            }
            return super.toString();
        }
    }

    /**
     * Ensures a JRE of the specified version is available for the current system. Delegates to {@link JREProvider#ensureJRE(JavaVersion)}.
     */
    public static File ensureJRE(final JavaVersion version) throws Exception {
        return JREProvider.ensureJRE(version);
    }

    /**
     * Ensures a JRE of the specified version and bitness. Delegates to {@link JREProvider#ensureJRE(JavaVersion, JREProvider.Bitness)}.
     */
    public static File ensureJRE(final JavaVersion version, final JREProvider.Bitness bitness) throws Exception {
        return JREProvider.ensureJRE(version, bitness);
    }

    /**
     * Ensures a JRE of the specified version for the given OS. Delegates to {@link JREProvider#ensureJRE(JavaVersion, OSFamily)}.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily) throws Exception {
        return JREProvider.ensureJRE(version, osFamily);
    }

    /**
     * Ensures a JRE of the specified version and bitness for the given OS. Delegates to {@link JREProvider#ensureJRE(JavaVersion, OSFamily, JREProvider.Bitness)}.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final JREProvider.Bitness bitness) throws Exception {
        return JREProvider.ensureJRE(version, osFamily, bitness);
    }

    /**
     * Ensures a JRE in the target folder. Delegates to {@link JREProvider#ensureJRE(JavaVersion, OSFamily, JREProvider.Bitness, File)}.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final JREProvider.Bitness bitness, final File targetFolder) throws Exception {
        return JREProvider.ensureJRE(version, osFamily, bitness, targetFolder);
    }

    /**
     * @deprecated Use {@link #ensureJRE(JavaVersion, OSFamily, JREProvider.Bitness, File)} instead
     */
    @Deprecated
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final File targetFolder) throws Exception {
        return JREProvider.ensureJRE(version, osFamily, archFamily, targetFolder);
    }

    /**
     * Ensures a JRE with architecture and bitness in the target folder. Delegates to {@link JREProvider#ensureJRE(JavaVersion, OSFamily, ARCHFamily, JREProvider.Bitness, File)}.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final JREProvider.Bitness bitness, final File targetFolder) throws Exception {
        return JREProvider.ensureJRE(version, osFamily, archFamily, bitness, targetFolder);
    }
}
