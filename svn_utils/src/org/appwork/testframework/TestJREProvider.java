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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.zip.GZIPInputStream;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.serializer.Deser;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.DownloadProgress;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.ARCHFamily;
import org.appwork.utils.os.CrossSystem.OSFamily;
import org.appwork.utils.zip.ZipIOReader;

/**
 * Provides JRE download and installation functionality for tests. Downloads JRE binaries from multiple sources if not already present in
 * the target folder:
 * <ul>
 * <li>Adoptium (Eclipse Temurin) - Primary source for Java 8, 11, 17, 21+</li>
 * <li>Azul Zulu API - Fallback source with wider version coverage</li>
 * <li>Azul Zulu CDN (cdn.azul.com/zulu/bin/) - Direct downloads with known stable versions for Java 6-25</li>
 * </ul>
 *
 * @author Thomas
 * @date 20.02.2026
 */
public class TestJREProvider {
    private static final String JRE_CACHE_FOLDER = "tmp/jre_cache";

    /**
     * Enum representing the bitness (32-bit or 64-bit) of a JRE.
     */
    public static enum Bitness {
        /**
         * 32-bit JRE
         */
        BIT_32,
        /**
         * 64-bit JRE
         */
        BIT_64;
        /**
         * Returns the current system's bitness.
         */
        public static Bitness getSystemBitness() {
            return CrossSystem.is64BitOperatingSystem() ? BIT_64 : BIT_32;
        }
    }

    /**
     * Options for JRE execution. Use the builder pattern to configure.
     * <p>
     * Example:
     *
     * <pre>
     * JreOptions.version(JavaVersion.JVM_1_6).bit(Bitness.BIT_32).jvmArgs("-Xmx512m")
     * JreOptions.version(JavaVersion.JVM_11_0).detailedVersion("11.0.2")
     * </pre>
     */
    public static class JreOptions {
        private JavaVersion  version;
        private Bitness      bitness;
        private List<String> jvmArgs;
        private String       detailedVersion;
        private boolean      bitnessExplicit = false;

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
        public JreOptions bit(final Bitness bitness) {
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

        public JavaVersion getVersion() {
            return this.version;
        }

        public Bitness getBitness() {
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
    }

    /**
     * Result of JRE lookup containing the java binary path and the actual bitness used.
     */
    public static class JreResult {
        private final File    javaBinary;
        private final Bitness bitness;
        private final File    cacheFolder;

        public JreResult(final File javaBinary, final Bitness bitness, final File cacheFolder) {
            this.javaBinary = javaBinary;
            this.bitness = bitness;
            this.cacheFolder = cacheFolder;
        }

        public File getJavaBinary() {
            return this.javaBinary;
        }

        public Bitness getBitness() {
            return this.bitness;
        }

        public File getCacheFolder() {
            return this.cacheFolder;
        }
    }

    /**
     * Central JRE lookup method that handles all cases:
     * <ul>
     * <li>If bitness is null: search cache for any matching JRE, then try download with system bitness, then fallback, then prompt</li>
     * <li>If bitness is explicit: cache → download → prompt → verify</li>
     * <li>If detailedVersion is set: search for exact version match</li>
     * </ul>
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
        final String detailedVersion = options.getDetailedVersion();
        if (options.isBitnessExplicit()) {
            return findOrDownloadJREExplicitBitness(version, osFamily, options.getBitness(), detailedVersion);
        } else {
            return findOrDownloadJREAutoBitness(version, osFamily, detailedVersion);
        }
    }

    /**
     * Lookup with explicit bitness: cache → download → prompt → verify.
     */
    private static JreResult findOrDownloadJREExplicitBitness(final JavaVersion version, final OSFamily osFamily, final Bitness bitness, final String detailedVersion) throws Exception {
        final File cacheFolder = getJRECacheFolder(version, osFamily, bitness);
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        File javaBinary = findJavaBinaryInCache(cacheFolder, javaBinaryName, detailedVersion);
        if (javaBinary != null) {
            LogV3.info("JRE found in cache: " + javaBinary.getAbsolutePath());
            return new JreResult(javaBinary, bitness, cacheFolder);
        }
        final int featureVersion = getFeatureVersion(version);
        final String os = getAdoptiumOS(osFamily);
        final ARCHFamily archFamily = CrossSystem.getARCHFamily();
        final String arch = getAdoptiumArch(archFamily, bitness);
        try {
            javaBinary = downloadAndExtractJRE(featureVersion, os, arch, cacheFolder, osFamily, javaBinaryName);
            if (javaBinary != null) {
                if (detailedVersion != null) {
                    final String actualVersion = detectJavaVersionString(javaBinary);
                    if (actualVersion != null && !actualVersion.contains(detailedVersion) && !detailedVersion.contains(actualVersion.split("_")[0])) {
                        LogV3.info("Downloaded JRE version " + actualVersion + " does not match requested " + detailedVersion);
                    }
                }
                return new JreResult(javaBinary, bitness, cacheFolder);
            }
        } catch (final Exception e) {
            LogV3.info("Download failed for " + bitness + ": " + e.getMessage());
        }
        javaBinary = promptAndCopyLocalJRE(featureVersion, cacheFolder, osFamily, detailedVersion);
        return new JreResult(javaBinary, bitness, cacheFolder);
    }

    /**
     * Lookup with auto bitness: search cache (both bitnesses) → download with system bitness → fallback → prompt.
     */
    private static JreResult findOrDownloadJREAutoBitness(final JavaVersion version, final OSFamily osFamily, final String detailedVersion) throws Exception {
        final Bitness systemBitness = Bitness.getSystemBitness();
        final Bitness alternateBitness = systemBitness == Bitness.BIT_64 ? Bitness.BIT_32 : Bitness.BIT_64;
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        final int featureVersion = getFeatureVersion(version);
        final String os = getAdoptiumOS(osFamily);
        final ARCHFamily archFamily = CrossSystem.getARCHFamily();
        final File cacheFolder64 = getJRECacheFolder(version, osFamily, Bitness.BIT_64);
        File javaBinary = findJavaBinaryInCache(cacheFolder64, javaBinaryName, detailedVersion);
        if (javaBinary != null) {
            LogV3.info("JRE found in cache (64-bit): " + javaBinary.getAbsolutePath());
            return new JreResult(javaBinary, Bitness.BIT_64, cacheFolder64);
        }
        final File cacheFolder32 = getJRECacheFolder(version, osFamily, Bitness.BIT_32);
        javaBinary = findJavaBinaryInCache(cacheFolder32, javaBinaryName, detailedVersion);
        if (javaBinary != null) {
            LogV3.info("JRE found in cache (32-bit): " + javaBinary.getAbsolutePath());
            return new JreResult(javaBinary, Bitness.BIT_32, cacheFolder32);
        }
        final File systemCacheFolder = systemBitness == Bitness.BIT_64 ? cacheFolder64 : cacheFolder32;
        final String systemArch = getAdoptiumArch(archFamily, systemBitness);
        try {
            javaBinary = downloadAndExtractJRE(featureVersion, os, systemArch, systemCacheFolder, osFamily, javaBinaryName);
            if (javaBinary != null) {
                LogV3.info("Downloaded JRE (" + systemBitness + "): " + javaBinary.getAbsolutePath());
                return new JreResult(javaBinary, systemBitness, systemCacheFolder);
            }
        } catch (final Exception e) {
            LogV3.info("Download failed for " + systemBitness + ": " + e.getMessage() + ", trying " + alternateBitness + "...");
        }
        final File alternateCacheFolder = alternateBitness == Bitness.BIT_64 ? cacheFolder64 : cacheFolder32;
        final String alternateArch = getAdoptiumArch(archFamily, alternateBitness);
        try {
            javaBinary = downloadAndExtractJRE(featureVersion, os, alternateArch, alternateCacheFolder, osFamily, javaBinaryName);
            if (javaBinary != null) {
                LogV3.info("Downloaded JRE (" + alternateBitness + "): " + javaBinary.getAbsolutePath());
                return new JreResult(javaBinary, alternateBitness, alternateCacheFolder);
            }
        } catch (final Exception e) {
            LogV3.info("Download failed for " + alternateBitness + ": " + e.getMessage());
        }
        LogV3.info("No JRE download available, prompting for local path...");
        javaBinary = promptAndCopyLocalJRE(featureVersion, systemCacheFolder, osFamily, detailedVersion);
        final Bitness promptedBitness = detectBitnessFromJava(javaBinary);
        return new JreResult(javaBinary, promptedBitness != null ? promptedBitness : systemBitness, systemCacheFolder);
    }

    /**
     * Searches for a java binary in the cache folder. If detailedVersion is specified, checks if the cached JRE matches.
     */
    private static File findJavaBinaryInCache(final File cacheFolder, final String javaBinaryName, final String detailedVersion) {
        if (!cacheFolder.exists() || !cacheFolder.isDirectory()) {
            return null;
        }
        final File javaBinary = findJavaBinaryInFolder(cacheFolder, javaBinaryName);
        if (javaBinary == null || !javaBinary.exists()) {
            return null;
        }
        if (detailedVersion != null && !detailedVersion.isEmpty()) {
            try {
                final String actualVersion = detectJavaVersionString(javaBinary);
                if (actualVersion != null && !actualVersion.contains(detailedVersion) && !detailedVersion.startsWith(actualVersion.split("_")[0].split("-")[0])) {
                    LogV3.info("Cached JRE version " + actualVersion + " does not match requested " + detailedVersion + ", skipping cache");
                    return null;
                }
            } catch (final Exception e) {
                LogV3.warning("Failed to detect cached JRE version: " + e.getMessage());
            }
        }
        return javaBinary;
    }

    /**
     * Detects the bitness of a Java installation by checking for native libraries.
     */
    private static Bitness detectBitnessFromJava(final File javaBinary) {
        if (javaBinary == null) {
            return null;
        }
        try {
            final ProcessBuilder pb = new ProcessBuilder(javaBinary.getAbsolutePath(), "-XshowSettings:all", "-version");
            pb.redirectErrorStream(true);
            final Process process = pb.start();
            final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                final String lower = line.toLowerCase(Locale.ROOT);
                if (lower.contains("sun.arch.data.model") || lower.contains("os.arch")) {
                    if (lower.contains("64")) {
                        reader.close();
                        process.waitFor();
                        return Bitness.BIT_64;
                    } else if (lower.contains("32") || lower.contains("x86") || lower.contains("i386")) {
                        reader.close();
                        process.waitFor();
                        return Bitness.BIT_32;
                    }
                }
            }
            reader.close();
            process.waitFor();
        } catch (final Exception e) {
            LogV3.warning("Failed to detect JRE bitness: " + e.getMessage());
        }
        return null;
    }

    public static void main(final String[] args) {
        try {
            // Simple usage - downloads to tmp/jre/11/windows/x64/ (or similar based on system)
            System.out.println(ensureJRE(JavaVersion.JVM_11_0));
        } catch (final Exception e) {
            LogV3.log(e);
        }
    }

    // ==================== NEW API with JreOptions ====================
    /**
     * Executes a method on a class in a JRE with the specified options. The class is loaded directly in the child JRE (not serialized).
     * <p>
     * Example:
     *
     * <pre>
     * TestJREProvider.executeInJRE(JreOptions.version(JavaVersion.JVM_11_0), MyTest.class, &quot;testMethod&quot;);
     * TestJREProvider.executeInJRE(JreOptions.version(JavaVersion.JVM_1_8).jvmArgs(&quot;-Xmx512m&quot;), MyTest.class, &quot;testWithParams&quot;, &quot;arg1&quot;, 42);
     * TestJREProvider.executeInJRE(JreOptions.version(JavaVersion.JVM_1_6).bit(Bitness.BIT_32).detailedVersion(&quot;1.6.0_45&quot;), MyTest.class, &quot;oldTest&quot;);
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
    public static void executeInJRE(final JreOptions options, final Class<?> clazz, final String methodName, final Object... params) throws Exception {
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
                LogV3.info("Skipping TestJREProvider execution in PostBuild mode (no source available for " + MinimalMethodRunner.class.getSimpleName() + " compilation).");
                return;
            }
        }
        final JreResult jreResult = findOrDownloadJRE(options);
        executeMethodInJRE(jreResult.getJavaBinary(), jreResult.getCacheFolder(), options.getJvmArgs(), clazz, methodName, params, options.getVersion(), jreResult.getBitness());
    }

    /**
     * Executes a static method in a child JRE process.
     */
    private static void executeMethodInJRE(final File javaBinary, final File workFolder, final List<String> jvmArgs, final Class<?> clazz, final String methodName, final Object[] params, final JavaVersion version, final Bitness bitness) throws Exception {
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
            throw new IllegalArgumentException(MinimalMethodRunner.class.getSimpleName() + " does not support parameters. Either compile your project with lower Java compliance level, or use a method without parameters for Java " + getFeatureVersion(version) + " tests.");
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
        return getJRECacheFolder(version, CrossSystem.getOSFamily(), Bitness.getSystemBitness());
    }

    /**
     * Returns the cache folder for JRE installations with specified bitness and current system OS.
     */
    public static File getJRECacheFolder(final JavaVersion version, final Bitness bitness) {
        return getJRECacheFolder(version, CrossSystem.getOSFamily(), bitness);
    }

    /**
     * Returns the cache folder for JRE installations with specified OS and bitness.
     * <p>
     * Path format: tmp/jre/{version}/{os}/{bitness}/
     */
    public static File getJRECacheFolder(final JavaVersion version, final OSFamily osFamily, final Bitness bitness) {
        final int featureVersion = getFeatureVersion(version);
        final String osString = getOSFolderName(osFamily);
        final String bitnessString = bitness == Bitness.BIT_32 ? "x86" : "x64";
        return Application.getResource(JRE_CACHE_FOLDER + "/" + featureVersion + "/" + osString + "/" + bitnessString);
    }

    /**
     * Returns a folder name for the given OS family.
     */
    private static String getOSFolderName(final OSFamily osFamily) {
        return osFamily.name().toLowerCase(Locale.ROOT);
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
                    LogV3.info("Using pre-compiled " + MinimalMethodRunner.class.getSimpleName() + " for Java " + getFeatureVersion(targetVersion));
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
        LogV3.info("Compiling " + MinimalMethodRunner.class.getSimpleName() + " for Java " + getFeatureVersion(targetVersion) + " (in-repo class is Java " + getJavaVersionForClassVersion(runnerVersion) + ")...");
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
        final String targetVersionStr = getFeatureVersion(targetVersion) <= 8 ? "1." + getFeatureVersion(targetVersion) : String.valueOf(getFeatureVersion(targetVersion));
        final List<File> sourceFiles = new ArrayList<File>();
        sourceFiles.add(runnerSourceFile);
        final StringBuilder compileOutput = new StringBuilder();
        final int exitCode = compileWithArgFile(javacBinary, runnerDir, targetVersionStr, sourceRoot.getAbsolutePath(), sourceFiles, compileOutput);
        if (exitCode != 0 || !runnerClassFile.exists()) {
            LogV3.severe("Failed to compile " + MinimalMethodRunner.class.getSimpleName() + " for Java " + getFeatureVersion(targetVersion) + ":\n" + compileOutput.toString());
            throw new IOException("Failed to compile " + MinimalMethodRunner.class.getSimpleName() + " for Java " + getFeatureVersion(targetVersion) + ". See log output above for details.");
        }
        LogV3.info(MinimalMethodRunner.class.getSimpleName() + " compiled successfully for Java " + getFeatureVersion(targetVersion));
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
        LogV3.info("Recompiling " + clazz.getName() + " for Java " + getFeatureVersion(targetVersion) + "...");
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
        final String targetVersionStr = getFeatureVersion(targetVersion) <= 8 ? "1." + getFeatureVersion(targetVersion) : String.valueOf(getFeatureVersion(targetVersion));
        final StringBuilder sourcePathStr = new StringBuilder();
        for (int i = 0; i < sourcePaths.size(); i++) {
            if (i > 0) {
                sourcePathStr.append(File.pathSeparator);
            }
            sourcePathStr.append(sourcePaths.get(i).getAbsolutePath());
        }
        List<File> sourceFiles = collectSourceFiles(classesToCompile, sourcePaths);
        LogV3.info("Using compiler: " + javacBinary.getAbsolutePath() + " (compiling " + sourceFiles.size() + " source file(s) with -target " + getFeatureVersion(targetVersion) + ")...");
        StringBuilder compileOutput = new StringBuilder();
        int exitCode = compileWithArgFile(javacBinary, compiledDir, targetVersionStr, sourcePathStr.toString(), sourceFiles, compileOutput);
        if (exitCode != 0) {
            LogV3.severe("Compilation failed for Java " + getFeatureVersion(targetVersion) + ":\n" + compileOutput.toString());
            throw new IOException("Compilation failed for Java " + getFeatureVersion(targetVersion) + ". See log output above for details.");
        }
        final String classFileName = clazz.getName().replace('.', '/') + ".class";
        final File compiledClassFile = new File(compiledDir, classFileName);
        if (!compiledClassFile.exists()) {
            throw new IOException("Compilation succeeded but main class file not found: " + compiledClassFile.getAbsolutePath());
        }
        LogV3.info("Compiled " + sourceFiles.size() + " classes successfully for Java " + getFeatureVersion(targetVersion));
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
        final ClassLoader loader = TestJREProvider.class.getClassLoader();
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
    private static Thread createOutputForwarder(final InputStream inputStream, final boolean isError, final JavaVersion jreVersion, final Bitness bitness) {
        final String bitSuffix = bitness == Bitness.BIT_64 ? "_64" : "_32";
        final String versionStr = jreVersion != null && jreVersion != JavaVersion.UNKNOWN ? jreVersion.getVersionString() : String.valueOf(getFeatureVersion(jreVersion));
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
     * Ensures a JRE of the specified version is available for the current system. Downloads from Adoptium/Azul if not present. Uses current
     * system OS and bitness. If 64-bit is not available (e.g. for Java 6/7), falls back to 32-bit. JRE is cached in
     * tmp/jre/{version}/{os}/{bitness}/.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails
     */
    public static File ensureJRE(final JavaVersion version) throws Exception {
        return ensureJREWithBitnessFallback(version, CrossSystem.getOSFamily(), null, Bitness.getSystemBitness());
    }

    /**
     * Ensures a JRE of the specified version and bitness is available for the current system. Downloads from Adoptium/Azul if not present.
     * JRE is cached in tmp/jre/{version}/{os}/{bitness}/. Throws exception if the requested bitness is not available.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @param bitness
     *            The bitness (32 or 64-bit) of the JRE - must be available, no fallback
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails, or if the requested bitness is not available
     */
    public static File ensureJRE(final JavaVersion version, final Bitness bitness) throws Exception {
        return ensureJRE(version, CrossSystem.getOSFamily(), bitness);
    }

    /**
     * Ensures a JRE of the specified version is available for the given OS. Downloads from Adoptium/Azul if not present. Uses system
     * bitness. If 64-bit is not available (e.g. for Java 6/7), falls back to 32-bit. JRE is cached in tmp/jre/{version}/{os}/{bitness}/.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @param osFamily
     *            The target operating system family (WINDOWS, LINUX, MAC)
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily) throws Exception {
        return ensureJREWithBitnessFallback(version, osFamily, null, Bitness.getSystemBitness());
    }

    /**
     * Ensures a JRE of the specified version and bitness is available for the given OS. Downloads from Adoptium/Azul if not present. JRE is
     * cached in tmp/jre/{version}/{os}/{bitness}/. Throws exception if the requested bitness is not available.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @param osFamily
     *            The target operating system family (WINDOWS, LINUX, MAC)
     * @param bitness
     *            The bitness (32 or 64-bit) of the JRE - must be available, no fallback
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails, or if the requested bitness is not available
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final Bitness bitness) throws Exception {
        final File targetFolder = getJRECacheFolder(version, osFamily, bitness);
        return ensureJRE(version, osFamily, bitness, targetFolder);
    }

    /**
     * Internal method that tries the preferred bitness first, then falls back to 32-bit if not available.
     */
    private static File ensureJREWithBitnessFallback(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final Bitness preferredBitness) throws Exception {
        final File targetFolder64 = getJRECacheFolder(version, osFamily, preferredBitness);
        try {
            return ensureJREInternal(version, osFamily, archFamily, preferredBitness, targetFolder64);
        } catch (final IOException e) {
            if (preferredBitness == Bitness.BIT_64) {
                LogV3.info("64-bit JRE not available for Java " + getFeatureVersion(version) + ", trying 32-bit fallback...");
                final File targetFolder32 = getJRECacheFolder(version, osFamily, Bitness.BIT_32);
                return ensureJREInternal(version, osFamily, archFamily, Bitness.BIT_32, targetFolder32);
            }
            throw e;
        }
    }

    /**
     * Ensures a JRE of the specified version and bitness is available in the target folder. Downloads from Adoptium/Azul if not present.
     * Throws exception if the requested bitness is not available.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @param osFamily
     *            The target operating system family (WINDOWS, LINUX, MAC)
     * @param bitness
     *            The bitness (32 or 64-bit) of the JRE - must be available, no fallback
     * @param targetFolder
     *            The folder where the JRE should be installed
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails, or if the requested bitness is not available
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final Bitness bitness, final File targetFolder) throws Exception {
        return ensureJRE(version, osFamily, null, bitness, targetFolder);
    }

    /**
     * Ensures a JRE of the specified version is available in the target folder. Downloads from Adoptium if not present.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @param osFamily
     *            The target operating system family (WINDOWS, LINUX, MAC)
     * @param archFamily
     *            The target architecture family (X86, ARM), or null to use current system architecture
     * @param targetFolder
     *            The folder where the JRE should be installed
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails
     * @deprecated Use {@link #ensureJRE(JavaVersion, OSFamily, Bitness, File)} instead
     */
    @Deprecated
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final File targetFolder) throws Exception {
        return ensureJRE(version, osFamily, archFamily, Bitness.getSystemBitness(), targetFolder);
    }

    /**
     * Ensures a JRE of the specified version, architecture and bitness is available in the target folder. Downloads from Adoptium/Azul if
     * not present. Throws exception if the requested bitness is not available.
     *
     * @param version
     *            The Java version to ensure (e.g., JVM_1_6, JVM_1_8, JVM_11_0, JVM_17_0, JVM_21_0)
     * @param osFamily
     *            The target operating system family (WINDOWS, LINUX, MAC)
     * @param archFamily
     *            The target architecture family (X86, ARM), or null to use current system architecture
     * @param bitness
     *            The bitness (32 or 64-bit) of the JRE - must be available, no fallback
     * @param targetFolder
     *            The folder where the JRE should be installed
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     * @throws Exception
     *             If download or extraction fails, or if the requested bitness is not available
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final Bitness bitness, final File targetFolder) throws Exception {
        return ensureJREInternal(version, osFamily, archFamily, bitness, targetFolder);
    }

    /**
     * Internal method that performs the actual JRE download and installation.
     */
    private static File ensureJREInternal(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final Bitness bitness, final File targetFolder) throws Exception {
        if (version == null || version == JavaVersion.UNKNOWN) {
            throw new IllegalArgumentException("version must not be null or UNKNOWN");
        }
        if (osFamily == null) {
            throw new IllegalArgumentException("osFamily must not be null");
        }
        if (bitness == null) {
            throw new IllegalArgumentException("bitness must not be null");
        }
        if (targetFolder == null) {
            throw new IllegalArgumentException("targetFolder must not be null");
        }
        final int featureVersion = getFeatureVersion(version);
        final String osString = getAdoptiumOS(osFamily);
        final String archString = getAdoptiumArch(archFamily != null ? archFamily : CrossSystem.getARCHFamily(), bitness);
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        final File javaBinary = findJavaBinaryInFolder(targetFolder, javaBinaryName);
        if (javaBinary != null && javaBinary.exists()) {
            LogV3.info("JRE already exists at: " + javaBinary.getAbsolutePath());
            return javaBinary;
        }
        targetFolder.mkdirs();
        final File tempArchive = downloadJREFromAvailableSource(featureVersion, osString, archString, targetFolder, osFamily);
        if (!tempArchive.equals(targetFolder)) {
            try {
                LogV3.info("Extracting JRE to: " + targetFolder.getAbsolutePath());
                extractArchive(tempArchive, targetFolder, osFamily);
            } finally {
                tempArchive.delete();
            }
        }
        final File extractedJavaBinary = findJavaBinaryInFolder(targetFolder, javaBinaryName);
        if (extractedJavaBinary == null || !extractedJavaBinary.exists()) {
            throw new IOException("Failed to find java binary after extraction in: " + targetFolder);
        }
        if (osFamily != OSFamily.WINDOWS) {
            extractedJavaBinary.setExecutable(true);
        }
        LogV3.info("JRE installed at: " + extractedJavaBinary.getAbsolutePath());
        return extractedJavaBinary;
    }

    /**
     * Maps JavaVersion to feature version number (e.g. JVM_1_8 -> 8, JVM_11_0 -> 11).
     */
    private static int getFeatureVersion(final JavaVersion version) {
        final String versionString = version.getBase().getVersionString();
        if (versionString.startsWith("1.")) {
            return Integer.parseInt(versionString.substring(2));
        }
        final int dotIndex = versionString.indexOf('.');
        if (dotIndex > 0) {
            return Integer.parseInt(versionString.substring(0, dotIndex));
        }
        return Integer.parseInt(versionString);
    }

    /**
     * Maps OSFamily to Adoptium OS string.
     */
    private static String getAdoptiumOS(final OSFamily osFamily) {
        switch (osFamily) {
        case WINDOWS:
            return "windows";
        case LINUX:
            return "linux";
        case MAC:
            return "mac";
        default:
            throw new IllegalArgumentException("Unsupported OS family for Adoptium: " + osFamily);
        }
    }

    /**
     * Maps ARCHFamily and Bitness to Adoptium architecture string.
     */
    private static String getAdoptiumArch(final ARCHFamily archFamily, final Bitness bitness) {
        switch (archFamily) {
        case X86:
            if (bitness == Bitness.BIT_64) {
                return "x64";
            } else {
                return "x86";
            }
        case ARM:
            if (bitness == Bitness.BIT_64) {
                return "aarch64";
            } else {
                return "arm";
            }
        case PPC:
            if (bitness == Bitness.BIT_64) {
                return "ppc64le";
            } else {
                return "ppc64le";
            }
        default:
            return bitness == Bitness.BIT_64 ? "x64" : "x86";
        }
    }

    /**
     * Downloads a JRE archive, extracts it, and returns the path to the java binary.
     *
     * @return The java binary file, or null if download failed
     */
    private static File downloadAndExtractJRE(final int featureVersion, final String os, final String arch, final File targetFolder, final OSFamily osFamily, final String javaBinaryName) throws Exception {
        final File tempArchive = downloadJREFromAvailableSource(featureVersion, os, arch, targetFolder, osFamily);
        if (tempArchive == null) {
            return null;
        }
        try {
            LogV3.info("Extracting JRE to: " + targetFolder.getAbsolutePath());
            extractArchive(tempArchive, targetFolder, osFamily);
        } finally {
            tempArchive.delete();
        }
        final File extractedJavaBinary = findJavaBinaryInFolder(targetFolder, javaBinaryName);
        if (extractedJavaBinary == null || !extractedJavaBinary.exists()) {
            throw new IOException("Failed to find java binary after extraction in: " + targetFolder);
        }
        if (osFamily != OSFamily.WINDOWS) {
            extractedJavaBinary.setExecutable(true);
        }
        LogV3.info("JRE installed at: " + extractedJavaBinary.getAbsolutePath());
        return extractedJavaBinary;
    }

    /**
     * Tries to download a JRE from multiple sources: Adoptium JRE, Adoptium JDK, Azul Zulu API (JRE/JDK), Azul Zulu CDN (JRE/JDK). If all
     * downloads fail, returns null (does NOT prompt for local path).
     */
    private static File downloadJREFromAvailableSource(final int featureVersion, final String os, final String arch, final File targetFolder, final OSFamily osFamily) throws Exception {
        final List<String> urls = new ArrayList<String>();
        urls.add(buildAdoptiumJREUrl(featureVersion, os, arch));
        urls.add(buildAdoptiumJDKUrl(featureVersion, os, arch));
        urls.add(buildAzulZuluUrl(featureVersion, os, arch, osFamily, "jre"));
        urls.add(buildAzulZuluUrl(featureVersion, os, arch, osFamily, "jdk"));
        urls.addAll(buildAzulZuluCDNUrls(featureVersion, os, arch, osFamily, "jre"));
        urls.addAll(buildAzulZuluCDNUrls(featureVersion, os, arch, osFamily, "jdk"));
        for (final String url : urls) {
            if (url == null) {
                continue;
            }
            LogV3.info("Trying to download JRE from: " + url);
            try {
                final File result = tryDownloadJRE(url, targetFolder, osFamily);
                if (result != null) {
                    LogV3.info("Download successful from: " + url);
                    return result;
                }
            } catch (final Exception e) {
                LogV3.info("Download failed from " + url + ": " + e.getMessage());
            }
        }
        return null;
    }

    /**
     * Prompts user for a local JRE path, validates it, and copies to target folder.
     */
    private static File promptAndCopyLocalJRE(final int featureVersion, final File targetFolder, final OSFamily osFamily) throws Exception {
        return promptAndCopyLocalJRE(featureVersion, targetFolder, osFamily, null);
    }

    private static File promptAndCopyLocalJRE(final int featureVersion, final File targetFolder, final OSFamily osFamily, final String detailedVersion) throws Exception {
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        final String versionDisplay = detailedVersion != null ? detailedVersion : String.valueOf(featureVersion);
        System.out.println();
        System.out.println("=======================================================================");
        System.out.println("  No JRE download available for Java " + versionDisplay);
        System.out.println("  Please provide a local JRE/JDK path for Java " + versionDisplay);
        System.out.println("  (Enter path to the JRE root folder, e.g. C:\\Program Files\\Java\\jre1.6.0_45)");
        System.out.println("  Or press Enter to abort.");
        System.out.println("=======================================================================");
        System.out.print("JRE path: ");
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final String inputPath = reader.readLine();
        if (inputPath == null || inputPath.trim().isEmpty()) {
            throw new IOException("No JRE download source available for Java " + versionDisplay + " and no local path provided");
        }
        final File sourceJreFolder = new File(inputPath.trim());
        if (!sourceJreFolder.exists() || !sourceJreFolder.isDirectory()) {
            throw new IOException("Provided path does not exist or is not a directory: " + sourceJreFolder.getAbsolutePath());
        }
        final File sourceJavaBinary = findJavaBinaryInFolder(sourceJreFolder, javaBinaryName);
        if (sourceJavaBinary == null || !sourceJavaBinary.exists()) {
            final File directBin = new File(sourceJreFolder, "bin/" + javaBinaryName);
            throw new IOException("No java binary found in provided path. Expected at: " + directBin.getAbsolutePath());
        }
        final int detectedFeatureVersion = detectJavaVersion(sourceJavaBinary);
        if (detectedFeatureVersion != featureVersion) {
            throw new IOException("Java version mismatch. Expected Java " + featureVersion + " but found Java " + detectedFeatureVersion + " at: " + sourceJavaBinary.getAbsolutePath());
        }
        if (detailedVersion != null) {
            final String detectedFullVersion = detectJavaVersionString(sourceJavaBinary);
            if (detectedFullVersion != null && !detectedFullVersion.contains(detailedVersion)) {
                LogV3.warning("Detailed version mismatch. Expected " + detailedVersion + " but found " + detectedFullVersion + ". Continuing anyway.");
            }
        }
        LogV3.info("Valid JRE found at: " + sourceJavaBinary.getAbsolutePath());
        LogV3.info("Copying JRE to cache: " + targetFolder.getAbsolutePath());
        final File sourceRootFolder = sourceJavaBinary.getParentFile().getParentFile();
        copyDirectory(sourceRootFolder, new File(targetFolder, sourceRootFolder.getName()));
        LogV3.info("JRE copied successfully");
        return findJavaBinaryInFolder(targetFolder, javaBinaryName);
    }

    /**
     * Detects the Java version by running java -version.
     */
    private static int detectJavaVersion(final File javaBinary) throws Exception {
        final ProcessBuilder pb = new ProcessBuilder(javaBinary.getAbsolutePath(), "-version");
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        int version = -1;
        while ((line = reader.readLine()) != null) {
            version = parseVersionFromOutput(line);
            if (version > 0) {
                break;
            }
        }
        process.waitFor();
        reader.close();
        if (version <= 0) {
            throw new IOException("Could not determine Java version from: " + javaBinary.getAbsolutePath());
        }
        return version;
    }

    /**
     * Detects the full Java version string by running java -version.
     */
    private static String detectJavaVersionString(final File javaBinary) throws Exception {
        final ProcessBuilder pb = new ProcessBuilder(javaBinary.getAbsolutePath(), "-version");
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        String versionString = null;
        while ((line = reader.readLine()) != null) {
            final int quoteStart = line.indexOf('"');
            if (quoteStart >= 0) {
                final int quoteEnd = line.indexOf('"', quoteStart + 1);
                if (quoteEnd > quoteStart) {
                    versionString = line.substring(quoteStart + 1, quoteEnd);
                    break;
                }
            }
        }
        process.waitFor();
        reader.close();
        return versionString;
    }

    /**
     * Parses Java version from java -version output line.
     */
    private static int parseVersionFromOutput(final String line) {
        if (line == null) {
            return -1;
        }
        final String lower = line.toLowerCase(Locale.ROOT);
        if (!lower.contains("version")) {
            return -1;
        }
        final int quoteStart = line.indexOf('"');
        if (quoteStart < 0) {
            return -1;
        }
        final int quoteEnd = line.indexOf('"', quoteStart + 1);
        if (quoteEnd < 0) {
            return -1;
        }
        final String versionStr = line.substring(quoteStart + 1, quoteEnd);
        if (versionStr.startsWith("1.")) {
            final int dotIndex = versionStr.indexOf('.', 2);
            if (dotIndex > 2) {
                try {
                    return Integer.parseInt(versionStr.substring(2, dotIndex));
                } catch (final NumberFormatException e) {
                    return -1;
                }
            }
        } else {
            final int dotIndex = versionStr.indexOf('.');
            if (dotIndex > 0) {
                try {
                    return Integer.parseInt(versionStr.substring(0, dotIndex));
                } catch (final NumberFormatException e) {
                    return -1;
                }
            }
            try {
                return Integer.parseInt(versionStr.split("[^0-9]")[0]);
            } catch (final NumberFormatException e) {
                return -1;
            }
        }
        return -1;
    }

    /**
     * Recursively copies a directory.
     */
    private static void copyDirectory(final File source, final File target) throws IOException {
        if (source.isDirectory()) {
            target.mkdirs();
            final String[] children = source.list();
            if (children != null) {
                for (final String child : children) {
                    copyDirectory(new File(source, child), new File(target, child));
                }
            }
        } else {
            target.getParentFile().mkdirs();
            IO.copyFile(source, target);
        }
    }

    /**
     * Builds the Adoptium download URL for the latest JRE.
     */
    private static String buildAdoptiumJREUrl(final int featureVersion, final String os, final String arch) {
        return "https://api.adoptium.net/v3/binary/latest/" + featureVersion + "/ga/" + os + "/" + arch + "/jre/hotspot/normal/eclipse";
    }

    /**
     * Builds the Adoptium download URL for the latest JDK (fallback if JRE not available).
     */
    private static String buildAdoptiumJDKUrl(final int featureVersion, final String os, final String arch) {
        return "https://api.adoptium.net/v3/binary/latest/" + featureVersion + "/ga/" + os + "/" + arch + "/jdk/hotspot/normal/eclipse";
    }

    /**
     * Builds the Azul Zulu download URL for the latest JRE or JDK. Azul provides more Java versions than Adoptium (including Java 9, 10,
     * etc.).
     *
     * @param packageType
     *            "jre" or "jdk"
     */
    private static String buildAzulZuluUrl(final int featureVersion, final String os, final String arch, final OSFamily osFamily, final String packageType) {
        final String zuluOs = getAzulOS(os);
        final String zuluArch = getAzulArch(arch);
        final String archiveType = osFamily == OSFamily.WINDOWS ? "zip" : "tar.gz";
        return "https://api.azul.com/metadata/v1/zulu/packages/?java_version=" + featureVersion + "&os=" + zuluOs + "&arch=" + zuluArch + "&java_package_type=" + packageType + "&archive_type=" + archiveType + "&latest=true&availability_types=CA";
    }

    /**
     * Maps Adoptium OS string to Azul OS string.
     */
    private static String getAzulOS(final String adoptiumOs) {
        if ("windows".equals(adoptiumOs)) {
            return "windows";
        } else if ("linux".equals(adoptiumOs)) {
            return "linux";
        } else if ("mac".equals(adoptiumOs)) {
            return "macos";
        }
        return adoptiumOs;
    }

    /**
     * Maps Adoptium arch string to Azul API arch string.
     */
    private static String getAzulArch(final String adoptiumArch) {
        if ("x64".equals(adoptiumArch)) {
            return "x64";
        } else if ("x86".equals(adoptiumArch) || "x32".equals(adoptiumArch)) {
            return "i686";
        } else if ("aarch64".equals(adoptiumArch)) {
            return "aarch64";
        }
        return adoptiumArch;
    }

    /**
     * Maps Adoptium arch string to Azul CDN arch string. CDN uses slightly different naming than the API.
     */
    private static String getAzulCDNArch(final String adoptiumArch) {
        if ("x64".equals(adoptiumArch)) {
            return "x64";
        } else if ("x86".equals(adoptiumArch) || "x32".equals(adoptiumArch)) {
            return "i686";
        } else if ("aarch64".equals(adoptiumArch)) {
            return "aarch64";
        }
        return adoptiumArch;
    }

    /**
     * Maps Adoptium OS string to Azul CDN OS string.
     */
    private static String getAzulCDNOS(final String adoptiumOs) {
        if ("windows".equals(adoptiumOs)) {
            return "win";
        } else if ("linux".equals(adoptiumOs)) {
            return "linux";
        } else if ("mac".equals(adoptiumOs)) {
            return "macosx";
        }
        return adoptiumOs;
    }

    /**
     * Known stable Zulu versions for each Java major version. Format: [zuluVersion, javaUpdateVersion] These are updated periodically and
     * serve as fallback when the API is unavailable.
     */
    private static final String[][] ZULU_KNOWN_VERSIONS = {
                                                        // Java 6
        { "6", "6.22.0.3", "6.0.119" },
        // Java 7
        { "7", "7.56.0.11", "7.0.352" },
        // Java 8
        { "8", "8.82.0.21", "8.0.432" },
        // Java 9
        { "9", "9.0.7.1", "9.0.7" },
        // Java 10
        { "10", "10.3.5", "10.0.2" },
        // Java 11
        { "11", "11.74.15", "11.0.24" },
        // Java 12
        { "12", "12.3.11", "12.0.2" },
        // Java 13
        { "13", "13.54.17", "13.0.14" },
        // Java 14
        { "14", "14.29.23", "14.0.2" },
        // Java 15
        { "15", "15.46.17", "15.0.10" },
        // Java 16
        { "16", "16.32.15", "16.0.2" },
        // Java 17
        { "17", "17.54.17", "17.0.13" },
        // Java 18
        { "18", "18.32.13", "18.0.2.1" },
        // Java 19
        { "19", "19.32.13", "19.0.2" },
        // Java 20
        { "20", "20.32.11", "20.0.2" },
        // Java 21
        { "21", "21.38.21", "21.0.5" },
        // Java 22
        { "22", "22.32.15", "22.0.2" },
        // Java 23
        { "23", "23.32.11", "23.0.1" },
        // Java 24
        { "24", "24.28.79", "24" },
        // Java 25
        { "25", "25.32.21", "25.0.2" }                 };

    /**
     * Finds known Zulu version info for a Java feature version.
     *
     * @return String array [zuluVersion, javaUpdateVersion] or null if not found
     */
    private static String[] findKnownZuluVersion(final int featureVersion) {
        for (final String[] entry : ZULU_KNOWN_VERSIONS) {
            if (Integer.parseInt(entry[0]) == featureVersion) {
                return new String[] { entry[1], entry[2] };
            }
        }
        return null;
    }

    /**
     * Builds direct CDN download URLs for Azul Zulu. Uses known version mappings to construct URLs. Format:
     * https://cdn.azul.com/zulu/bin/zulu{zuluVersion}-ca-{packageType}{javaVersion}-{os}_{arch}.{extension}
     *
     * @param packageType
     *            "jre" or "jdk"
     * @return List of possible CDN URLs to try, or empty list if version not known
     */
    private static List<String> buildAzulZuluCDNUrls(final int featureVersion, final String os, final String arch, final OSFamily osFamily, final String packageType) {
        final List<String> urls = new ArrayList<String>();
        final String[] versionInfo = findKnownZuluVersion(featureVersion);
        if (versionInfo == null) {
            return urls;
        }
        final String zuluVersion = versionInfo[0];
        final String javaVersion = versionInfo[1];
        final String cdnOs = getAzulCDNOS(os);
        final String cdnArch = getAzulCDNArch(arch);
        final String extension = osFamily == OSFamily.WINDOWS ? "zip" : "tar.gz";
        final String baseUrl = "https://cdn.azul.com/zulu/bin/zulu" + zuluVersion + "-ca-" + packageType + javaVersion + "-" + cdnOs + "_" + cdnArch + "." + extension;
        urls.add(baseUrl);
        if ("i686".equals(cdnArch)) {
            final String altUrl = "https://cdn.azul.com/zulu/bin/zulu" + zuluVersion + "-ca-" + packageType + javaVersion + "-" + cdnOs + "_i386." + extension;
            urls.add(altUrl);
        }
        return urls;
    }

    /**
     * Creates a simple download progress handler that prints progress to the console.
     */
    private static DownloadProgress createDownloadProgress(final String url) {
        return new DownloadProgress() {
            private long   lastPrintTime    = 0;
            private int    lastPrintPercent = -1;
            private long   totalSize        = -1;
            private String shortUrl         = url.length() > 60 ? "..." + url.substring(url.length() - 57) : url;

            @Override
            public void setTotal(final long total) {
                super.setTotal(total);
                if (total > 0) {
                    this.totalSize = total;
                    LogV3.info("Downloading: " + shortUrl + " (" + SizeFormatter.formatBytes(total) + ")");
                }
            }

            @Override
            public void onBytesLoaded(final byte[] b, final int len) {
                super.onBytesLoaded(b, len);
                final long now = System.currentTimeMillis();
                // LogV3.info(" Pro (" + SizeFormatter.formatBytes(getLoaded()) + " / " + SizeFormatter.formatBytes(this.totalSize) + ") " +
                // len);
                if (this.totalSize > 0) {
                    final int percent = (int) ((getLoaded() * 100) / this.totalSize);
                    if (percent != this.lastPrintPercent && (now - this.lastPrintTime > 1000 || percent >= 100)) {
                        this.lastPrintTime = now;
                        this.lastPrintPercent = percent;
                        LogV3.info("  Progress: " + Math.min(percent, 100) + "% (" + SizeFormatter.formatBytes(getLoaded()) + " / " + SizeFormatter.formatBytes(this.totalSize) + ")");
                    }
                } else if (now - this.lastPrintTime > 2000) {
                    this.lastPrintTime = now;
                    LogV3.info("  Downloaded: " + SizeFormatter.formatBytes(getLoaded()));
                }
            }
        };
    }

    /**
     * Tries to download a JRE archive. Returns null if download fails with 404. Supports: - Adoptium API (api.adoptium.net) - Azul API
     * (api.azul.com) - Azul CDN (cdn.azul.com) - Direct download URLs
     */
    private static File tryDownloadJRE(final String url, final File targetFolder, final OSFamily osFamily) throws Exception {
        if (url.contains("api.azul.com")) {
            return downloadFromAzulAPI(url, targetFolder, osFamily);
        }
        final String extension = osFamily == OSFamily.WINDOWS ? ".zip" : ".tar.gz";
        final File tempFile = new File(targetFolder, "jre_download" + extension);
        final HttpClient client = new HttpClient();
        client.setConnectTimeout(30000);
        client.setReadTimeout(120000);
        tempFile.getParentFile().mkdirs();
        final DownloadProgress progress = createDownloadProgress(url);
        final RequestContext context = RequestContext.get(url).setTarget(tempFile).setDownloadProgress(progress);
        try {
            context.execute();
        } finally {
            System.out.println(context);
        }
        final int responseCode = context.getCode();
        if (responseCode == 404 || responseCode == 403) {
            tempFile.delete();
            return null;
        }
        if (responseCode < 200 || responseCode >= 300) {
            tempFile.delete();
            throw new IOException("Failed to download JRE. HTTP response code: " + responseCode);
        }
        if (tempFile.length() < 1000) {
            final String content = IO.readFileToString(tempFile);
            if (content != null && (content.contains("<!DOCTYPE") || content.contains("<html") || content.contains("Not Found"))) {
                tempFile.delete();
                return null;
            }
        }
        LogV3.info("  Download complete: " + SizeFormatter.formatBytes(tempFile.length()));
        return tempFile;
    }

    /**
     * Downloads JRE from Azul Zulu API. The Azul API returns JSON with download URL.
     */
    private static File downloadFromAzulAPI(final String apiUrl, final File targetFolder, final OSFamily osFamily) throws Exception {
        final HttpClient client = new HttpClient();
        client.setConnectTimeout(30000);
        client.setReadTimeout(30000);
        final RequestContext apiContext = RequestContext.get(apiUrl);
        apiContext.execute();
        final int apiResponseCode = apiContext.getCode();
        if (apiResponseCode == 404 || apiResponseCode == 204) {
            return null;
        }
        if (apiResponseCode < 200 || apiResponseCode >= 300) {
            throw new IOException("Azul API request failed. HTTP response code: " + apiResponseCode);
        }
        final String jsonResponse = apiContext.getResponseString();
        if (jsonResponse == null || jsonResponse.trim().isEmpty() || "[]".equals(jsonResponse.trim())) {
            return null;
        }
        final String downloadUrl = extractAzulDownloadUrl(jsonResponse);
        if (downloadUrl == null) {
            return null;
        }
        LogV3.info("Azul download URL: " + downloadUrl);
        final String extension = osFamily == OSFamily.WINDOWS ? ".zip" : ".tar.gz";
        final File tempFile = new File(targetFolder, "jre_download" + extension);
        tempFile.getParentFile().mkdirs();
        final DownloadProgress progress = createDownloadProgress(downloadUrl);
        final RequestContext downloadContext = RequestContext.get(downloadUrl).setTarget(tempFile).setDownloadProgress(progress);
        downloadContext.execute();
        final int downloadResponseCode = downloadContext.getCode();
        if (downloadResponseCode < 200 || downloadResponseCode >= 300) {
            tempFile.delete();
            throw new IOException("Failed to download from Azul. HTTP response code: " + downloadResponseCode);
        }
        LogV3.info("  Download complete: " + SizeFormatter.formatBytes(tempFile.length()));
        return tempFile;
    }

    /**
     * Extracts the download URL from Azul API JSON response.
     */
    private static String extractAzulDownloadUrl(final String json) {
        final int downloadUrlIndex = json.indexOf("\"download_url\"");
        if (downloadUrlIndex < 0) {
            return null;
        }
        final int colonIndex = json.indexOf(":", downloadUrlIndex);
        if (colonIndex < 0) {
            return null;
        }
        final int startQuote = json.indexOf("\"", colonIndex);
        if (startQuote < 0) {
            return null;
        }
        final int endQuote = json.indexOf("\"", startQuote + 1);
        if (endQuote < 0) {
            return null;
        }
        return json.substring(startQuote + 1, endQuote);
    }

    /**
     * Extracts the downloaded archive to the target folder.
     */
    private static void extractArchive(final File archive, final File targetFolder, final OSFamily osFamily) throws Exception {
        if (osFamily == OSFamily.WINDOWS) {
            extractZip(archive, targetFolder);
        } else {
            extractTarGz(archive, targetFolder);
        }
    }

    /**
     * Extracts a ZIP archive.
     */
    private static void extractZip(final File zipFile, final File targetFolder) throws Exception {
        final ZipIOReader reader = new ZipIOReader(zipFile);
        try {
            reader.extractTo(targetFolder);
        } finally {
            reader.close();
        }
    }

    /**
     * Extracts a tar.gz archive using TarArchiveInputStream if available, otherwise falls back to command line.
     */
    private static void extractTarGz(final File tarGzFile, final File targetFolder) throws Exception {
        try {
            extractTarGzNative(tarGzFile, targetFolder);
        } catch (final ClassNotFoundException e) {
            extractTarGzCommandLine(tarGzFile, targetFolder);
        }
    }

    /**
     * Extracts tar.gz using Apache Commons Compress TarArchiveInputStream (if available).
     */
    private static void extractTarGzNative(final File tarGzFile, final File targetFolder) throws Exception {
        final Class<?> tarInputStreamClass = Class.forName("org.apache.commons.compress.archivers.tar.TarArchiveInputStream");
        final Class<?> tarEntryClass = Class.forName("org.apache.commons.compress.archivers.tar.TarArchiveEntry");
        final InputStream fis = new FileInputStream(tarGzFile);
        try {
            final InputStream gzis = new GZIPInputStream(fis);
            try {
                final Object tarIn = tarInputStreamClass.getConstructor(InputStream.class).newInstance(gzis);
                try {
                    final java.lang.reflect.Method getNextEntry = tarInputStreamClass.getMethod("getNextTarEntry");
                    final java.lang.reflect.Method isDirectory = tarEntryClass.getMethod("isDirectory");
                    final java.lang.reflect.Method getName = tarEntryClass.getMethod("getName");
                    final java.lang.reflect.Method read = tarInputStreamClass.getMethod("read", byte[].class, int.class, int.class);
                    Object entry;
                    while ((entry = getNextEntry.invoke(tarIn)) != null) {
                        final String entryName = (String) getName.invoke(entry);
                        final File outputFile = new File(targetFolder, entryName);
                        if ((Boolean) isDirectory.invoke(entry)) {
                            outputFile.mkdirs();
                        } else {
                            outputFile.getParentFile().mkdirs();
                            final FileOutputStream fos = new FileOutputStream(outputFile);
                            try {
                                final byte[] buffer = new byte[32768];
                                int len;
                                while ((len = (Integer) read.invoke(tarIn, buffer, 0, buffer.length)) != -1) {
                                    fos.write(buffer, 0, len);
                                }
                            } finally {
                                fos.close();
                            }
                        }
                    }
                } finally {
                    if (tarIn instanceof java.io.Closeable) {
                        ((java.io.Closeable) tarIn).close();
                    }
                }
            } finally {
                gzis.close();
            }
        } finally {
            fis.close();
        }
    }

    /**
     * Extracts tar.gz using command line tar (fallback for systems without Apache Commons Compress).
     */
    private static void extractTarGzCommandLine(final File tarGzFile, final File targetFolder) throws Exception {
        final ProcessBuilder pb = new ProcessBuilder("tar", "-xzf", tarGzFile.getAbsolutePath(), "-C", targetFolder.getAbsolutePath());
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final int exitCode = process.waitFor();
        if (exitCode != 0) {
            throw new IOException("tar extraction failed with exit code: " + exitCode);
        }
    }

    /**
     * Searches for the java binary in the target folder (handles Adoptium/Zulu nested directory structure).
     */
    private static File findJavaBinaryInFolder(final File folder, final String javaBinaryName) {
        if (!folder.exists() || !folder.isDirectory()) {
            return null;
        }
        final File directBin = new File(folder, "bin/" + javaBinaryName);
        if (directBin.exists()) {
            return directBin;
        }
        final File[] subFolders = folder.listFiles();
        if (subFolders != null) {
            for (final File subFolder : subFolders) {
                if (subFolder.isDirectory()) {
                    final String name = subFolder.getName().toLowerCase(Locale.ROOT);
                    if (name.startsWith("jdk") || name.startsWith("jre") || name.startsWith("zulu") || name.contains("temurin") || name.contains("adoptium")) {
                        final File binDir = new File(subFolder, "bin/" + javaBinaryName);
                        if (binDir.exists()) {
                            return binDir;
                        }
                        if (CrossSystem.getOSFamily() == OSFamily.MAC) {
                            final File macBin = new File(subFolder, "Contents/Home/bin/" + javaBinaryName);
                            if (macBin.exists()) {
                                return macBin;
                            }
                        }
                    }
                }
            }
        }
        return null;
    }
}
