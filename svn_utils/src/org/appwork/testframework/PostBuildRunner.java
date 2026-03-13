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

import static org.appwork.testframework.AWTest.logInfoAnyway;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.sink.AbstractSink;
import org.appwork.loggingv3.simple.sink.SimpleFormatter;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.utils.Application;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

/**
 * @author thomas
 * @date 10.12.2021
 *
 */
public class PostBuildRunner {
    /**
     *
     */
    private static final int              EXIT_ERROR_BUT_NO_BREAK_JUST_LOG           = 3;
    /**
     *
     */
    private static final int              EXIT_ERROR                                 = 1;
    /**
     *
     */
    private static final int              EXIT_NO_CLASS_DEF_FOUND_3                  = 5;
    /**
     *
     */
    private static final int              EXIT_NO_CLASS_DEF_FOUND_2                  = 4;
    /**
     *
     */
    private static final int              EXIT_SUCCESS                               = 0;
    /**
     *
     */
    public static final String            POSTBUILDTEST                              = "POSTBUILDTEST";
    /**
     *
     */
    private static final String           SOURCE                                     = "-source=";
    /**
     *
     */
    private static final String           DO_NOT_TRY_TO_RUN_MARKER                   = "-skip=";
    /**
     *
     */
    private static final String           MUST_RUN_WITHOUT_CLASSLOADER_ERRORS_MARKER = "-force=";
    private static final int              EXIT_NO_CLASS_DEF_FOUND_1                  = 2;
    private static final String           BUILD_ID_MARKER                            = "-buildid=";
    private static final String           MAY_FAIL_MARKER                            = "-mayfail=";
    private static final String           BUILDSCRIPT_MARKER                         = "-buildscript=";
    /** Subdir under user.home for post-build test status cache: .appworktest/postbuild/{cacheKey}/ */
    private static final String           POSTBUILD_STATUS_CACHE_SUBDIR              = ".appworktest" + File.separator + "postbuild";
    /** Class name of AdminExecuter; used to detect tests that need the elevated helper (ASM dependency check). */
    private static final String           ADMIN_EXECUTER_CLASS                       = "org.appwork.testframework.executer.AdminExecuter";
    public static HashMap<String, Object> CONFIG                                     = null;

    /**
     * @author thomas
     * @date 16.12.2021
     *
     */
    public static class LogCache extends AbstractSink {
        private ArrayList<LogRecord2> buffer = new ArrayList<LogRecord2>();

        public ArrayList<LogRecord2> getBuffer() {
            return buffer;
        }

        @Override
        public void publish(LogRecord2 record) {
            synchronized (buffer) {
                buffer.add(record);
            }
        }
    }

    static File                            BASE;
    private static HashSet<String>         MUST_RUN_WITHOUT_CLASSLOADER_ERRORS;
    private static HashSet<String>         DO_NOT_TRY_TO_RUN;
    private static HashSet<String>         MAY_FAIL_ON_MISSING_CLASS;
    private static String                  BUILDSCRIPT_PATH;
    private static boolean                 PRINT_CLASSLOADER_ERRORS;
    private static ArrayList<String>       TESTS_OK;
    private static HashMap<String, String> TESTS_FAILED = new HashMap<String, String>();
    /** Env vars for tests that use AdminExecuter (lock dir, private key); set once after starting the helper. */
    private static Map<String, String>     ADMIN_HELPER_ENV                          = null;
    private static boolean                 VERBOSE;

    /**
     * SHA256 hash of the BASE folder (sorted relative paths + file hashes). Used to separate status cache per base.
     */
    protected static String getBaseHash(File base) {
        return Hash.getSHA256(base.toString());
    }

    /** Cache dir for this cache key (buildId or baseHash): user.home/.appworktest/postbuild/{cacheKey}/ */
    protected static File getStatusCacheDir(String cacheKey) {
        return new File(System.getProperty("user.home", ""), POSTBUILD_STATUS_CACHE_SUBDIR + File.separator + cacheKey);
    }

    /** Sanitize buildId for use as directory name (replace path and invalid chars). */
    protected static String sanitizeBuildId(String buildId) {
        if (buildId == null) {
            return "";
        }
        return buildId.replace('\\', '_').replace('/', '_').replace(':', '_').replace('*', '_').replace('?', '_').replace('"', '_').replace('<', '_').replace('>', '_').replace('|', '_').trim();
    }

    /** Name of the cache info file in the status cache dir. */
    private static final String CACHE_INFO_FILENAME = "cache-info.json";

    /**
     * Writes or updates the cache-info.json in the given status cache dir with current build/project info.
     */
    protected static void writeCacheInfo(File statusCacheDir, String cacheKey, String buildId, File baseDir, String projectInfo) {
        if (statusCacheDir == null) {
            return;
        }
        try {
            if (!statusCacheDir.exists()) {
                statusCacheDir.mkdirs();
            }
            PostBuildCacheInfo info = new PostBuildCacheInfo();
            info.setCacheKey(cacheKey);
            info.setBuildId(buildId);
            info.setBasePath(baseDir != null ? baseDir.getAbsolutePath() : null);
            info.setProjectInfo(projectInfo);
            info.setLastUpdated(Time.systemIndependentCurrentJVMTimeMillis());
            info.setJavaVersion(System.getProperty("java.version"));
            info.setUserName(System.getProperty("user.name"));
            File infoFile = new File(statusCacheDir, CACHE_INFO_FILENAME);
            String json = FlexiUtils.serializeToPrettyJson(info);
            IO.secureWrite(infoFile, json, SYNC.META_AND_DATA);
        } catch (Throwable e) {
            LogV3.warning("Could not write cache info to " + statusCacheDir + ": " + e.getMessage());
        }
    }

    /** One JSON file per test class; safe filename from class name. */
    protected static File getStatusFile(File cacheDir, String testClassName) {
        String safe = testClassName.replace(".", "_").replace(File.separatorChar, '_');
        return new File(cacheDir, safe + ".json");
    }

    protected static PostBuildTestStatus loadStatus(File statusFile) {
        if (statusFile == null || !statusFile.isFile()) {
            return new PostBuildTestStatus();
        }
        try {
            String json = IO.readFileToString(statusFile);
            PostBuildTestStatus s = FlexiUtils.jsonToObject(json, new SimpleTypeRef<PostBuildTestStatus>(PostBuildTestStatus.class));
            return s != null ? s : new PostBuildTestStatus();
        } catch (Throwable e) {
            return new PostBuildTestStatus();
        }
    }

    protected static void saveStatus(File statusFile, PostBuildTestStatus status) {
        if (statusFile == null || status == null) {
            return;
        }
        try {
            File parent = statusFile.getParentFile();
            if (parent != null && !parent.exists()) {
                parent.mkdirs();
            }
            String json = FlexiUtils.serializeToPrettyJson(status);
            IO.secureWrite(statusFile, json, SYNC.META_AND_DATA);
        } catch (Throwable e) {
            LogV3.warning("Could not save post-build test status to " + statusFile + ": " + e.getMessage());
        }
    }

    /**
     * Deterministic SHA256 hash of the dependency set (class name -> class bytecode hash). Same as IDETestRunner resource check.
     */
    protected static String getResourceHashFromDependencies(Map<String, String> classToHash) {
        if (classToHash == null || classToHash.isEmpty()) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        for (String key : new TreeSet<String>(classToHash.keySet())) {
            String val = classToHash.get(key);
            sb.append(key).append("\n").append(val != null ? val : "").append("\n");
        }
        return Hash.getSHA256(sb.toString());
    }

    /** True if the last run of this test was a failure (so we should run again). */
    protected static boolean isLastRunFailure(PostBuildTestStatus status) {
        if (status == null || status.getLastFailureTimestamp() == null) {
            return false;
        }
        Long lastSuccess = status.getLastSuccessTimestamp();
        return lastSuccess == null || status.getLastFailureTimestamp().longValue() > lastSuccess.longValue();
    }

    /** Max number of changed classes to list when logging why a test is run. */
    private static final int MAX_CHANGED_CLASSES_TO_LOG = 10;

    /**
     * Logs to console why the test is being executed and up to {@link #MAX_CHANGED_CLASSES_TO_LOG} changed classes.
     */
    protected static void logRunReason(String testClassName, PostBuildTestStatus status, Map<String, String> currentRefs, String currentResourceHash) {
        String reason;
        if (isLastRunFailure(status)) {
            reason = "last run failed";
            LogV3.info("  >>" + header("run") + testClassName + " - Running because: " + reason);
            return;
        }
        if (status.getResourceHash() == null || status.getResourceHashes() == null || status.getResourceHashes().isEmpty()) {
            reason = "first run (no cached dependencies)";
            LogV3.info("  >>" + header("run") + testClassName + " - Running because: " + reason);
            return;
        }
        Map<String, String> prev = status.getResourceHashes();
        ArrayList<String> changes = new ArrayList<String>();
        for (Entry<String, String> e : currentRefs.entrySet()) {
            String c = e.getKey();
            if (!prev.containsKey(c)) {
                changes.add(c + " (new)");
            } else if (!e.getValue().equals(prev.get(c))) {
                changes.add(c + " (changed)");
            }
        }
        for (String c : prev.keySet()) {
            if (!currentRefs.containsKey(c)) {
                changes.add(c + " (removed)");
            }
        }
        reason = "dependencies changed";
        LogV3.info("  >>" + header("run") + testClassName + " - Running because: " + reason);
        int max = Math.min(MAX_CHANGED_CLASSES_TO_LOG, changes.size());
        for (int i = 0; i < max; i++) {
            LogV3.info("       " + (i + 1) + ". " + changes.get(i));
        }
        if (changes.size() > MAX_CHANGED_CLASSES_TO_LOG) {
            LogV3.info("       ... and " + (changes.size() - MAX_CHANGED_CLASSES_TO_LOG) + " more");
        }
    }

    public static void main(final String[] args) throws Exception {
        BuildDecisions.setEnabled(false);
        Application.setApplication(".appwork-tests");
        AWTest.initLogger(new SimpleFormatter() {
            @Override
            protected String createPre(LogRecord2 record, String sourceString) {
                return "";
            }
        });
        if (args.length >= 1 && args[0].equals("-test")) {
            runSingleTest(args);
        } else {
            runAllTestsInClasspath(args);
        }
    }

    protected static void runAllTestsInClasspath(final String[] args) throws Exception {
        BASE = new File(args[0]);
        TESTS_OK = new ArrayList<String>();
        TESTS_FAILED = new HashMap<String, String>();
        MUST_RUN_WITHOUT_CLASSLOADER_ERRORS = new HashSet<String>();
        DO_NOT_TRY_TO_RUN = new HashSet<String>();
        MAY_FAIL_ON_MISSING_CLASS = new HashSet<String>();
        BUILDSCRIPT_PATH = null;
        String sourceFolder = null;
        String buildId = null;
        String projectInfo = null;
        LogV3.info("Parameters " + Arrays.toString(args));
        for (int i = 1; i < args.length; i++) {
            LogV3.info("Parameter " + i + ": " + args[i]);
            if (args[i].startsWith(MUST_RUN_WITHOUT_CLASSLOADER_ERRORS_MARKER)) {
                MUST_RUN_WITHOUT_CLASSLOADER_ERRORS.add(args[i].substring(MUST_RUN_WITHOUT_CLASSLOADER_ERRORS_MARKER.length()));
            } else if (args[i].startsWith(DO_NOT_TRY_TO_RUN_MARKER)) {
                DO_NOT_TRY_TO_RUN.add(args[i].substring(DO_NOT_TRY_TO_RUN_MARKER.length()));
            } else if (args[i].startsWith(SOURCE)) {
                sourceFolder = args[i].substring(SOURCE.length());
            } else if (args[i].startsWith(BUILD_ID_MARKER)) {
                buildId = args[i].substring(BUILD_ID_MARKER.length()).trim();
            } else if (args[i].startsWith(MAY_FAIL_MARKER)) {
                MAY_FAIL_ON_MISSING_CLASS.add(args[i].substring(MAY_FAIL_MARKER.length()));
            } else if (args[i].startsWith(BUILDSCRIPT_MARKER)) {
                BUILDSCRIPT_PATH = args[i].substring(BUILDSCRIPT_MARKER.length()).trim();
            } else if (args[i].startsWith("-projectinfo=")) {
                projectInfo = args[i].substring("-projectinfo=".length()).trim();
            } else if (StringUtils.equalsIgnoreCase(args[i], "-print_classloader_errors")) {
                PRINT_CLASSLOADER_ERRORS = true;
            } else if (StringUtils.equalsIgnoreCase(args[i], "-verbose")) {
                AWTest.VERBOSE = true;
            } else {
                throw new WTFException("Unknown Parameter: " + args[i]);
            }
        }
        // if(StringUtils.isEmpty(sourceFolder)) {
        // throw new WTFException("-source is missing");
        // }
        if (!BASE.isDirectory()) {
            throw new WTFException("The Base directory passed from the ant script does not exist: " + BASE);
        } else {
            if (!new File(BASE, "application").isDirectory()) {
                throw new WTFException("base/application Folder does not exist.");
            }
            if (!new File(BASE, "tests").isDirectory()) {
                throw new WTFException("base/tests Folder does not exist.");
            }
        }
        LogV3.info(header("START") + "Post Build Tests");
        LogV3.info(header("BASE") + BASE);
        final List<Class<?>> testClasses = new ArrayList<Class<?>>();
        main: for (File f : Files.getFiles(true, true, new File(BASE, "tests"))) {
            // LogV3.info("Scan File: " + f);
            if (f.isFile()) {
                String relative = Files.getRelativePath(new File(BASE, "tests"), f);
                if (relative.toUpperCase(Locale.ROOT).endsWith(".CLASS") && !relative.contains("$")) {
                    String classname = relative.replace("/", ".").substring(0, relative.length() - ".class".length());
                    try {
                        for (String p : DO_NOT_TRY_TO_RUN) {
                            if (classname.matches(p)) {
                                continue main;
                            }
                        }
                        LogV3.info("Try Test " + classname);
                        Class<?> cls = Class.forName(classname, false, Thread.currentThread().getContextClassLoader());
                        if (Modifier.isAbstract(cls.getModifiers())) {
                            continue main;
                        }
                        if (PostBuildTestInterface.class.isAssignableFrom(cls) && PostBuildTestInterface.class != cls) {
                            testClasses.add(cls);
                        }
                    } catch (NoClassDefFoundError e) {
                        LogV3.info("Skipped Test (Classloader Error):" + relative);
                        if (mustRunWithoutClassloaderErrors(classname)) {
                            throw new Exception("Failed test with classloader error although it is marked with -force in the build.xml", e);
                        }
                    } catch (ClassNotFoundException e) {
                        LogV3.info("Skipped Test (Classloader Error):" + relative);
                        if (mustRunWithoutClassloaderErrors(classname)) {
                            throw new Exception("Failed test with classloader error although it is marked with -force in the build.xml", e);
                        }
                    }
                }
            }
        }
        boolean anyTest = testClasses.size() > 0;
        if (!anyTest) {
            throw new Exception("No Tests found. This is probably a build error. Check folder " + new File(BASE, "tests") + " for tests classes");
        }
        String baseHash = getBaseHash(BASE);
        if (baseHash == null) {
            baseHash = "unknown";
        }
        String cacheKey = (buildId != null && buildId.length() > 0) ? sanitizeBuildId(buildId) : baseHash;
        File statusCacheDir = getStatusCacheDir(cacheKey);
        writeCacheInfo(statusCacheDir, cacheKey, buildId, BASE, projectInfo);
        final HashMap<String, PostBuildTestStatus> statusMap = new HashMap<String, PostBuildTestStatus>();
        for (Class<?> cls : testClasses) {
            File sf = getStatusFile(statusCacheDir, cls.getName());
            statusMap.put(cls.getName(), loadStatus(sf));
        }
        Collections.sort(testClasses, new Comparator<Class<?>>() {
            @Override
            public int compare(Class<?> a, Class<?> b) {
                Long fa = statusMap.get(a.getName()).getLastFailureTimestamp();
                Long fb = statusMap.get(b.getName()).getLastFailureTimestamp();
                if (fa == null && fb == null) {
                    return 0;
                }
                if (fa == null) {
                    return 1;
                }
                if (fb == null) {
                    return -1;
                }
                return fb.compareTo(fa);
            }
        });
        final HashMap<String, String> testResourceHashes = new HashMap<String, String>();
        final HashMap<String, Map<String, String>> testResourceRefs = new HashMap<String, Map<String, String>>();
        for (Class<?> cls : testClasses) {
            try {
                Map<String, String> refs = new ClassCollector2().getClasses(cls.getName(), true);
                String h = getResourceHashFromDependencies(refs);
                testResourceHashes.put(cls.getName(), h);
                testResourceRefs.put(cls.getName(), refs);
            } catch (Throwable e) {
                LogV3.info("  >>" + header("resource") + " Could not collect deps for " + cls.getName() + ", will run: " + e.getMessage());
                testResourceHashes.put(cls.getName(), null);
                testResourceRefs.put(cls.getName(), null);
            }
        }
        boolean testsNeedAdminHelper = false;
        for (Map<String, String> refs : testResourceRefs.values()) {
            if (refs != null && refs.containsKey(ADMIN_EXECUTER_CLASS)) {
                testsNeedAdminHelper = true;
                break;
            }
        }
        if (testsNeedAdminHelper && CrossSystem.isWindows()) {
            try {
                Class<?> adminExecuterClass = Class.forName(ADMIN_EXECUTER_CLASS);
                adminExecuterClass.getMethod("ensureHelperRunning").invoke(null);
                Object envObj = adminExecuterClass.getMethod("getHelperConnectionEnv").invoke(null);
                if (envObj instanceof Map) {
                    @SuppressWarnings("unchecked")
                    Map<String, String> env = (Map<String, String>) envObj;
                    if (env != null && !env.isEmpty()) {
                        ADMIN_HELPER_ENV = env;
                        LogV3.info(header("helper") + "Admin helper started; will pass connection params to tests that use AdminExecuter.");
                    }
                }
            } catch (Throwable t) {
                LogV3.warning("Could not start AdminExecuter helper for post-build tests: " + t.getMessage());
            }
        }
        for (Class<?> cls : testClasses) {
            PostBuildTestStatus status = statusMap.get(cls.getName());
            String currentResourceHash = testResourceHashes.get(cls.getName());
            boolean skip = currentResourceHash != null && currentResourceHash.equals(status.getResourceHash()) && !isLastRunFailure(status);
            if (skip) {
                try {
                    PostBuildTestInterface inst = (PostBuildTestInterface) cls.getConstructor(new Class[] {}).newInstance(new Object[] {});
                    if (inst != null && !inst.isSkipOnUnchangedDependencies()) {
                        skip = false;
                        LogV3.info("  >>" + header("run") + cls.getName() + " (always run: isSkipOnUnchangedDependencies=false)");
                    }
                } catch (Throwable t) {
                    // keep skip as true
                }
            }
            if (skip) {
                TESTS_OK.add(cls.getName());
                LogV3.info("  >>" + header("skipped") + cls.getName() + " (deps unchanged, last passed)");
                continue;
            }
            Map<String, String> refsForSave = testResourceRefs.get(cls.getName());
            logRunReason(cls.getName(), status, refsForSave != null ? refsForSave : new HashMap<String, String>(), currentResourceHash);
            runTestClass(cls, sourceFolder, args, getStatusFile(statusCacheDir, cls.getName()), currentResourceHash, refsForSave);
        }
        for (Entry<String, String> es : TESTS_FAILED.entrySet()) {
            LogV3.info(header("FAILED") + es.getKey() + ": " + es.getValue());
        }
        int i = 0;
        main: for (String p : MUST_RUN_WITHOUT_CLASSLOADER_ERRORS) {
            for (String test : TESTS_OK) {
                if (test.matches(p)) {
                    continue main;
                }
            }
            LogV3.info(header("error") + "-force Test not found: " + p);
            i++;
        }
        if (i > 0 || TESTS_FAILED.size() > 0) {
            throw new Exception("Could not find tests for at least 1 -force pattern");
        }
        LogV3.info(header("SUCCESS") + "Finished Post Build Tests");
        LogV3.info(header("FINISHED") + "PostBuildRunnerFinished");
    }

    /**
     * @param classname
     * @return
     */
    private static boolean mustRunWithoutClassloaderErrors(String classname) {
        for (String p : MUST_RUN_WITHOUT_CLASSLOADER_ERRORS) {
            if (classname.matches(p)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param testClassName
     * @return true if this test is on the may-fail list (skip on missing class, no prompt)
     */
    private static boolean isMayFailOnMissingClass(String testClassName) {
        if (MAY_FAIL_ON_MISSING_CLASS == null) {
            return false;
        }
        for (String p : MAY_FAIL_ON_MISSING_CLASS) {
            if (testClassName.matches(p)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns true if the class should be skipped when collecting dependencies (system/third-party).
     */
    private static boolean skipClassForResource(String clazz) {
        if (clazz == null) {
            return true;
        }
        if (clazz.startsWith("java.") || clazz.startsWith("javax.") || clazz.startsWith("sun.") || clazz.startsWith("com.sun.")) {
            return true;
        }
        if (clazz.startsWith("org.bouncycastle.") || clazz.startsWith("de.javasoft.") || clazz.startsWith("org.mozilla.")) {
            return true;
        }
        if (clazz.startsWith("[") /* array */) {
            return true;
        }
        return false;
    }

    /**
     * When a test fails due to missing class and -buildscript= was passed: prompt user to add
     * missing resource(s) to the build script (with ASM-collected dependencies) or add test to -mayfail list.
     */
    private static void handleMissingClassInBuildScript(String testClassName, String missingClass, String stdOut, String errOut) {
        Set<String> classesToAdd = new HashSet<String>();
        classesToAdd.add(missingClass);
        try {
            ClassCollector2 collector = new ClassCollector2();
            Map<String, String> deps = collector.getClasses(missingClass, true);
            if (deps != null) {
                for (String c : deps.keySet()) {
                    if (!skipClassForResource(c)) {
                        classesToAdd.add(c);
                    }
                }
            }
        } catch (Throwable e) {
            LogV3.info("Could not collect dependencies for " + missingClass + ", adding only that class: " + e.getMessage());
        }
        LogV3.info("  >>" + header("missing") + "Test " + testClassName + " needs: " + StringUtils.join(classesToAdd, ", "));
        logInfoAnyway("Add these " + classesToAdd.size() + " resource(s) to build script for test " + testClassName + "? [y/n]");
        String answer = readLineFromStdin();
        if (answer != null && answer.trim().toLowerCase(Locale.ROOT).startsWith("y")) {
            addIncludesToBuildScript(BUILDSCRIPT_PATH, testClassName, classesToAdd);
            logInfoAnyway("Added include(s) to " + BUILDSCRIPT_PATH + ". Re-run the build to run the test.");
        } else {
            addMayFailToBuildScript(BUILDSCRIPT_PATH, testClassName);
            logInfoAnyway("Added -mayfail=" + testClassName + " to " + BUILDSCRIPT_PATH + ". Test will be skipped on missing class in future.");
        }
    }

    /**
     * Read a single line from stdin (for interactive prompt when run from Ant).
     */
    private static String readLineFromStdin() {
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
            return br.readLine();
        } catch (IOException e) {
            LogV3.warning("Could not read from stdin: " + e.getMessage());
            return null;
        }
    }

    private static final String WRITE_MARKER = "<!--WRITE-->";

    /**
     * Insert include lines before <!--WRITE--> in the build script. Comment marks resources as required for the given test only.
     */
    private static void addIncludesToBuildScript(String buildScriptPath, String testClassName, Set<String> classNames) {
        File buildFile = new File(buildScriptPath);
        if (!buildFile.isFile()) {
            LogV3.warning("Build script not found: " + buildScriptPath);
            return;
        }
        try {
            String content = IO.readFileToString(buildFile);
            if (!content.contains(WRITE_MARKER)) {
                LogV3.warning("Build script does not contain " + WRITE_MARKER);
                return;
            }
            StringBuilder block = new StringBuilder();
            block.append("\r\n\t\t\t\t<!-- Required by Test: ").append(testClassName).append(" (missing in JAR) -->\r\n");
            for (String cn : new TreeSet<String>(classNames)) {
                String path = cn.replace(".", "/") + ".java";
                block.append("\t\t\t\t<include name=\"").append(path).append("\" />\r\n");
            }
            block.append("\t\t\t\t");
            content = content.replace(WRITE_MARKER, block.toString() + WRITE_MARKER);
            IO.secureWrite(buildFile, content, SYNC.META_AND_DATA);
        } catch (Throwable e) {
            LogV3.warning("Failed to add includes to build script: " + e.getMessage());
        }
    }

    /**
     * Add -mayfail=testClassName to the build script so this test is not prompted again on missing class.
     * Inserts after the last <arg value="-skip= or <arg value="-force= line.
     */
    private static void addMayFailToBuildScript(String buildScriptPath, String testClassName) {
        File buildFile = new File(buildScriptPath);
        if (!buildFile.isFile()) {
            LogV3.warning("Build script not found: " + buildScriptPath);
            return;
        }
        try {
            String content = IO.readFileToString(buildFile);
            String newArg = "\t\t\t\t<arg value=\"-mayfail=" + testClassName + "\" />\r\n";
            if (content.contains("-mayfail=" + testClassName)) {
                return;
            }
            int insert = -1;
            int idx = 0;
            while (true) {
                int nextSkip = content.indexOf("<arg value=\"-skip=", idx);
                int nextForce = content.indexOf("<arg value=\"-force=", idx);
                int next = nextSkip >= 0 && nextForce >= 0 ? Math.min(nextSkip, nextForce) : (nextSkip >= 0 ? nextSkip : nextForce);
                if (next < 0) {
                    break;
                }
                insert = content.indexOf("\r\n", next);
                if (insert < 0) {
                    insert = content.indexOf("\n", next);
                }
                if (insert >= 0) {
                    insert += 1;
                }
                idx = next + 1;
            }
            if (insert < 0) {
                int javaClose = content.indexOf("</java>");
                if (javaClose >= 0) {
                    insert = content.lastIndexOf("\r\n", javaClose);
                    if (insert < 0) {
                        insert = content.lastIndexOf("\n", javaClose);
                    }
                    if (insert >= 0) {
                        insert += 1;
                    } else {
                        insert = javaClose;
                    }
                }
            }
            if (insert >= 0) {
                content = content.substring(0, insert) + newArg + content.substring(insert);
                IO.secureWrite(buildFile, content, SYNC.META_AND_DATA);
            } else {
                LogV3.warning("Could not find insertion point for -mayfail in build script.");
            }
        } catch (Throwable e) {
            LogV3.warning("Failed to add -mayfail to build script: " + e.getMessage());
        }
    }

    protected static void runSingleTest(final String[] args) {
        String testClass = args[1];
        logInfoAnyway("Test " + testClass);
        try {
            TestDependency dependencies = Class.forName(testClass, false, PostBuildRunner.class.getClassLoader()).getAnnotation(TestDependency.class);
            if (dependencies != null) {
                for (String str : dependencies.value()) {
                    logInfoAnyway("Test Dependency: " + str);
                    Class.forName(str, false, PostBuildRunner.class.getClassLoader());
                }
            }
        } catch (Throwable e) {
            if (Exceptions.getInstanceof(e, NoClassDefFoundError.class) != null || Exceptions.getInstanceof(e, ClassNotFoundException.class) != null) {
                AWTest.setLoggerSilent(false, false);
                LogV3.info(header("ERROR") + "Class file(s) not in JAR: " + e.getMessage());
                LogV3.disableSysout();
                // LogV3.log(e);
                System.exit(EXIT_NO_CLASS_DEF_FOUND_1);
            }
            // Force to cached error log.
            AWTest.setLoggerSilent(false, true);
            LogV3.log(e);
            LogV3.disableSysout();
            System.exit(1);
        }
        try {
            Class.forName(testClass);
            // test if the class is a valid path and can get loaded.
            String folderWithTestWorkspace = args[2];
            if (!new File(folderWithTestWorkspace).isDirectory()) {
                throw new WTFException("Workspace Folder does not exist.");
            } else {
                if (!new File(new File(folderWithTestWorkspace), "application").isDirectory()) {
                    throw new WTFException("Workspace/application Folder does not exist.");
                }
                if (!new File(new File(folderWithTestWorkspace), "tests").isDirectory()) {
                    throw new WTFException("Workspace/tests Folder does not exist.");
                }
            }
            // all further arguments are arguments from the ant-build script that might be required for some tests.
            runTest(args[1], args[2], args);
            LogV3.info(header("SUCCESS") + "Test Finished Successfuly");
            LogV3.disableSysout();
            System.exit(EXIT_SUCCESS);
            // } catch (java.lang.InstantiationException e) {
        } catch (Throwable e) {
            // Force to cached error log.
            AWTest.setLoggerSilent(false, true);
            LogV3.log(e);
            if (Exceptions.getInstanceof(e, NoClassDefFoundError.class) != null) {
                logInfoAnyway("Add @TestDependency({\"" + Exceptions.getInstanceof(e, NoClassDefFoundError.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                System.err.println("Add @TestDependency({\"" + Exceptions.getInstanceof(e, NoClassDefFoundError.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                LogV3.disableSysout();
                System.exit(EXIT_NO_CLASS_DEF_FOUND_2);
            } else if (Exceptions.getInstanceof(e, ClassNotFoundException.class) != null) {
                logInfoAnyway("Add @TestDependency({\"" + Exceptions.getInstanceof(e, ClassNotFoundException.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                System.err.println("Add @TestDependency({\"" + Exceptions.getInstanceof(e, ClassNotFoundException.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                LogV3.disableSysout();
                System.exit(EXIT_NO_CLASS_DEF_FOUND_3);
            }
            LogV3.disableSysout();
            System.exit(EXIT_ERROR);
        }
    }

    public static String header(String msg) {
        return "*** " + StringUtils.fillPost(msg.toUpperCase(Locale.ROOT), " ", 10) + "";
    }

    /**
     * @param string
     * @param args
     * @return
     * @throws Exception
     */
    private static void runTest(String clz, String base, String[] args) throws Exception {
        System.setProperty(POSTBUILDTEST, clz);
        PostBuildTestInterface instance;
        instance = (PostBuildTestInterface) Class.forName(clz).getConstructor(new Class[] {}).newInstance(new Object[] {});
        String[] paras = new String[args.length - 2];
        System.arraycopy(args, 2, paras, 0, paras.length);
        final File testConfigFile = new File(new File(base, "application"), "test_resources/config.json");
        if (CONFIG == null) {
            CONFIG = new HashMap<String, Object>();
            if (testConfigFile.isFile()) {
                CONFIG = JSonStorage.restoreFromString(IO.readFileToString(testConfigFile), TypeRef.HASHMAP);
            }
        }
        instance.runPostBuildTest(paras, new File(base, "application"));
    }

    protected static void runTestClass(Class<?> cls, String sourceFolder, String[] args, File statusFile, String resourceHashAfterRun, Map<String, String> resourceHashesForSave) throws Exception {
        final boolean[] successHolder = new boolean[] { false };
        final int[] exitCodeHolder = new int[] { -1 };
        final String[] errorMessageHolder = new String[] { null };
        try {
            runTestClassImpl(cls, sourceFolder, args, successHolder, exitCodeHolder, errorMessageHolder, resourceHashesForSave);
        } finally {
            long now = Time.systemIndependentCurrentJVMTimeMillis();
            PostBuildTestStatus status = loadStatus(statusFile);
            status.setLastRunTimestamp(now);
            status.setRunCount(status.getRunCount() + 1);
            if (resourceHashAfterRun != null) {
                status.setResourceHash(resourceHashAfterRun);
            }
            if (resourceHashesForSave != null && !resourceHashesForSave.isEmpty()) {
                status.setResourceHashes(new HashMap<String, String>(resourceHashesForSave));
            }
            if (successHolder[0]) {
                status.setLastSuccessTimestamp(Long.valueOf(now));
                status.setLastExitCode(Integer.valueOf(EXIT_SUCCESS));
                status.setLastErrorMessage(null);
            } else {
                status.setLastFailureTimestamp(Long.valueOf(now));
                status.setFailureCount(status.getFailureCount() + 1);
                if (exitCodeHolder[0] >= 0) {
                    status.setLastExitCode(Integer.valueOf(exitCodeHolder[0]));
                }
                status.setLastErrorMessage(errorMessageHolder[0]);
            }
            saveStatus(statusFile, status);
            if (exitCodeHolder[0] >= 0) {
                System.exit(exitCodeHolder[0]);
            }
        }
    }

    private static void runTestClassImpl(Class<?> cls, String sourceFolder, String[] args, final boolean[] successHolder, final int[] exitCodeHolder, final String[] errorMessageHolder, Map<String, String> refsForThisTest) throws Exception {
        LogV3.info(header("run test") + cls.getName());
        for (String a : args) {
            if ("-verbose".equals(a)) {
                AWTest.VERBOSE = true;
            }
        }
        AWTest.setLoggerSilent(true, false);
        final File workingcopy = new File(BASE, "RUNNING_TEST");
        try {
            if (workingcopy.exists()) {
                Files.deleteRecursive(workingcopy, true);
            }
            // LogV3.info("Copy " + new File(BASE, "application") + " to " + new File(workingcopy, "application"));
            IO.copyFolderRecursive(Application.getResource(""), new File(workingcopy, "application"), false, new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    if (pathname.equals(workingcopy)) {
                        return false;
                    }
                    return true;
                }
            }, SYNC.META_AND_DATA);
            // LogV3.info("Copy " + new File(BASE, "tests") + " to " + new File(workingcopy, "tests"));
            IO.copyFolderRecursive(new File(BASE, "tests"), new File(workingcopy, "tests"), false, new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    if (pathname.equals(workingcopy)) {
                        return false;
                    }
                    return true;
                }
            }, SYNC.META_AND_DATA);
            StringBuilder sb = new StringBuilder();
            List<URL> urls = ClassPathScanner.getClassPath();
            for (URL url : urls) {
                // System.out.println(url);
                if (sb.length() > 0) {
                    sb.append(";");
                }
                // logInfoAnyway(url + "");
                File u = new File(url.toURI());
                String cp = Files.getRelativePath(BASE, u);
                if (cp != null) {
                    sb.append(cp);
                } else {
                    // ide
                    sb.append(u.getAbsolutePath());
                }
            }
            ArrayList<String> cmd = new ArrayList<String>();
            cmd.add(CrossSystem.getJavaBinary());
            RuntimeMXBean bean = ManagementFactory.getRuntimeMXBean();
            List<String> jvmArgs = bean.getInputArguments();
            for (int i = 0; i < jvmArgs.size(); i++) {
                if (jvmArgs.get(i).startsWith("-D")) {
                    cmd.add(jvmArgs.get(i));
                }
            }
            URL metaUrl = cls.getResource(cls.getSimpleName() + ".meta.json");
            PostTestMetaData meta = null;
            if (metaUrl != null) {
                meta = FlexiUtils.readObject(metaUrl.openStream(), new SimpleTypeRef<PostTestMetaData>(PostTestMetaData.class));
            }
            if (meta != null && meta.getArgs() != null) {
                HashMap<String, Object> matcher = new HashMap<String, Object>();
                matcher.put("jvm", JVMVersion.get());
                if (meta.getArgs().getJVM() != null) {
                    for (ConditionedArgs p : meta.getArgs().getJVM()) {
                        if (p.getCondition() == null || p.getCondition().matches(matcher)) {
                            cmd.addAll(Arrays.asList(p.getCommandline()));
                        }
                    }
                }
            }
            cmd.addAll(Arrays.asList(new String[] { "-cp", sb.toString(), PostBuildRunner.class.getName(), "-test", cls.getName(), workingcopy.getAbsolutePath() }));
            if (sourceFolder != null) {
                cmd.addAll(Arrays.asList(new String[] { "-source", sourceFolder }));
            }
            cmd.addAll(Arrays.asList(args));
            if (meta != null && meta.getArgs() != null) {
                HashMap<String, Object> matcher = new HashMap<String, Object>();
                matcher.put("jvm", JVMVersion.get());
                if (meta.getArgs().getCLI() != null) {
                    for (ConditionedArgs p : meta.getArgs().getCLI()) {
                        if (p.getCondition() == null || p.getCondition().matches(matcher)) {
                            cmd.addAll(Arrays.asList(p.getCommandline()));
                        }
                    }
                }
            }
            logInfoAnyway("Command: " + cmd.toString());
            ProcessBuilder pb = ProcessBuilderFactory.create(cmd);
            pb.directory(workingcopy);
            if (ADMIN_HELPER_ENV != null && refsForThisTest != null && refsForThisTest.containsKey(ADMIN_EXECUTER_CLASS)) {
                pb.environment().putAll(ADMIN_HELPER_ENV);
            }
            ProcessOutput result = ProcessBuilderFactory.runCommand(pb);
            if (result.getStdOutString().length() > 0) {
                LogV3.info(result.getStdOutString());
            }
            if (result.getErrOutString().length() > 0) {
                LogV3.info(result.getErrOutString());
            }
            int exit = result.getExitCode();
            AWTest.setLoggerSilent(false, false);
            if (exit == EXIT_ERROR_BUT_NO_BREAK_JUST_LOG) {
                // error, but do not stop tests - just log
                LogV3.info("  >>" + header("failed") + "Exit with ExitCode " + exit);
                if (result.getStdOutString().length() > 0) {
                    LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                if (result.getErrOutString().length() > 0) {
                    LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                TESTS_FAILED.put(cls.getName(), "Exit Code " + exit);
                successHolder[0] = false;
                errorMessageHolder[0] = "Exit Code " + exit;
                return;
            } else if (exit == EXIT_NO_CLASS_DEF_FOUND_1 || exit == EXIT_NO_CLASS_DEF_FOUND_2 || exit == EXIT_NO_CLASS_DEF_FOUND_3) {
                if (mustRunWithoutClassloaderErrors(cls.getName())) {
                    if (result.getStdOutString().length() > 0) {
                        LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                    }
                    if (result.getErrOutString().length() > 0) {
                        LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                    }
                    throw new Exception("Failed test with classloader error although it is marked with -force in the build.xml: " + cls.getName());
                }
                if (isMayFailOnMissingClass(cls.getName())) {
                    LogV3.info("  >>" + header("skipped") + cls.getName() + " (missing class, on -mayfail list)");
                    TESTS_OK.add(cls.getName());
                    successHolder[0] = true;
                    return;
                }
                String missingResource = new Regex(result.getStdOutString(), "not in JAR\\:\\s*([^\r\n]+)").getMatch(0);
                if (missingResource == null) {
                    missingResource = new Regex(result.getErrOutString(), "not in JAR\\:\\s*([^\r\n]+)").getMatch(0);
                }
                boolean fromNotInJar = missingResource != null && (result.getStdOutString().contains("not in JAR") || result.getErrOutString().contains("not in JAR"));
                if (missingResource == null) {
                    // Subprocess prints "Add @TestDependency({\"className\"}) to TestClass" to stderr on exit 4/5
                    missingResource = new Regex(result.getErrOutString(), "Add @TestDependency\\(\\{\"([^\"]+)\"\\}\\)").getMatch(0);
                }
                if (missingResource == null) {
                    // Fallback: first non-empty line from stderr or stdout that might contain the missing class or error
                    String err = result.getErrOutString().trim();
                    String out = result.getStdOutString().trim();
                    if (err.length() > 0) {
                        String firstLine = new Regex(err, "([^\r\n]+)").getMatch(0);
                        if (firstLine != null && firstLine.length() > 0) {
                            missingResource = firstLine.length() > 200 ? firstLine.substring(0, 197) + "..." : firstLine;
                        }
                    }
                    if (missingResource == null && out.length() > 0) {
                        String firstLine = new Regex(out, "([^\r\n]+)").getMatch(0);
                        if (firstLine != null && firstLine.length() > 0) {
                            missingResource = firstLine.length() > 200 ? firstLine.substring(0, 197) + "..." : firstLine;
                        }
                    }
                }
                if (BUILDSCRIPT_PATH != null && missingResource != null && missingResource.length() > 0) {
                    String missingClass = missingResource.replace("/", ".").trim();
                    handleMissingClassInBuildScript(cls.getName(), missingClass, result.getStdOutString(), result.getErrOutString());
                }
                if (missingResource != null) {
                    if (fromNotInJar) {
                        LogV3.info("  >>" + header("skipped") + "Missing Test Resource: " + missingResource);
                    } else {
                        LogV3.info("  >>" + header("skipped") + "Classloader Error: " + missingResource);
                    }
                } else {
                    LogV3.info("  >>" + header("skipped") + "Classloader Error.");
                }
                if (PRINT_CLASSLOADER_ERRORS) {
                    if (result.getStdOutString().length() > 0) {
                        LogV3.info("    " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n    "));
                    }
                    if (result.getErrOutString().length() > 0) {
                        LogV3.info("    " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n    "));
                    }
                }
                TESTS_OK.add(cls.getName());
                successHolder[0] = true;
                return;
            } else if (exit == EXIT_ERROR) {
                LogV3.info("  >>" + header("failed") + "Exit with ExitCode " + exit);
                if (result.getStdOutString().length() > 0) {
                    LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                if (result.getErrOutString().length() > 0) {
                    LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                exitCodeHolder[0] = exit;
                errorMessageHolder[0] = "Exit with ExitCode " + exit;
                return;
            } else if (exit != 0) {
                LogV3.info("  >>" + header("failed") + "Exit with ExitCode " + exit);
                if (result.getStdOutString().length() > 0) {
                    LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                if (result.getErrOutString().length() > 0) {
                    LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                exitCodeHolder[0] = exit;
                errorMessageHolder[0] = "Exit with ExitCode " + exit;
                return;
            } else {
                TESTS_OK.add(cls.getName());
                LogV3.info("  >>" + header("success"));
                successHolder[0] = true;
                return;
            }
        } finally {
            AWTest.setLoggerSilent(false, false);
            if (workingcopy.exists()) {
                long started = Time.systemIndependentCurrentJVMTimeMillis();
                while (true) {
                    try {
                        Files.deleteRecursive(workingcopy, true);
                        break;
                    } catch (IOException e) {
                        Thread.sleep(500);
                        if (Time.since(started) > 60000) {
                            throw e;
                        }
                    }
                }
            }
        }
        // instance.runTest(args);
    }
}
