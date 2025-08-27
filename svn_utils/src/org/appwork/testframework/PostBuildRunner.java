/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;

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
    private static boolean                 PRINT_CLASSLOADER_ERRORS;
    private static ArrayList<String>       TESTS_OK;
    private static HashMap<String, String> TESTS_FAILED = new HashMap<String, String>();
    private static boolean                 VERBOSE;

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
        String sourceFolder = null;
        LogV3.info("Parameters " + Arrays.toString(args));
        for (int i = 1; i < args.length; i++) {
            LogV3.info("Parameter " + i + ": " + args[i]);
            if (args[i].startsWith(MUST_RUN_WITHOUT_CLASSLOADER_ERRORS_MARKER)) {
                MUST_RUN_WITHOUT_CLASSLOADER_ERRORS.add(args[i].substring(MUST_RUN_WITHOUT_CLASSLOADER_ERRORS_MARKER.length()));
            } else if (args[i].startsWith(DO_NOT_TRY_TO_RUN_MARKER)) {
                DO_NOT_TRY_TO_RUN.add(args[i].substring(DO_NOT_TRY_TO_RUN_MARKER.length()));
            } else if (args[i].startsWith(SOURCE)) {
                sourceFolder = args[i].substring(SOURCE.length());
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
        boolean anyTest = false;
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
                            runTestClass(cls, sourceFolder, args);
                            anyTest = true;
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
        if (!anyTest) {
            throw new Exception("No Tests found. This is probably a build error. Check folder " + new File(BASE, "tests") + " for tests classes");
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
                System.exit(2);
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
            System.exit(0);
            // } catch (java.lang.InstantiationException e) {
        } catch (Throwable e) {
            // Force to cached error log.
            AWTest.setLoggerSilent(false, true);
            LogV3.log(e);
            if (Exceptions.getInstanceof(e, NoClassDefFoundError.class) != null) {
                logInfoAnyway("Add @TestDependency({\"" + Exceptions.getInstanceof(e, NoClassDefFoundError.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                System.err.println("Add @TestDependency({\"" + Exceptions.getInstanceof(e, NoClassDefFoundError.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                LogV3.disableSysout();
                System.exit(3);
            } else if (Exceptions.getInstanceof(e, ClassNotFoundException.class) != null) {
                logInfoAnyway("Add @TestDependency({\"" + Exceptions.getInstanceof(e, ClassNotFoundException.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                System.err.println("Add @TestDependency({\"" + Exceptions.getInstanceof(e, ClassNotFoundException.class).getMessage().replace("/", ".") + "\"}) to " + testClass);
                LogV3.disableSysout();
                System.exit(3);
            }
            LogV3.disableSysout();
            System.exit(1);
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

    protected static void runTestClass(Class<?> cls, String sourceFolder, String[] args) throws Exception {
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
            ProcessOutput result = ProcessBuilderFactory.runCommand(pb);
            if (result.getStdOutString().length() > 0) {
                LogV3.info(result.getStdOutString());
            }
            if (result.getErrOutString().length() > 0) {
                LogV3.info(result.getErrOutString());
            }
            int exit = result.getExitCode();
            AWTest.setLoggerSilent(false, false);
            if (exit == 3) {
                // error, but do not stop tests - just log
                LogV3.info("  >>" + header("failed") + "Exit with ExitCode " + exit);
                if (result.getStdOutString().length() > 0) {
                    LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                if (result.getErrOutString().length() > 0) {
                    LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                TESTS_FAILED.put(cls.getName(), "Exit Code " + exit);
            } else if (exit == 2) {
                if (mustRunWithoutClassloaderErrors(cls.getName())) {
                    if (result.getStdOutString().length() > 0) {
                        LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                    }
                    if (result.getErrOutString().length() > 0) {
                        LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                    }
                    throw new Exception("Failed test with classloader error although it is marked with -force in the build.xml");
                }
                String missingResource = new Regex(result.getStdOutString(), "not in JAR\\:\\s*([^\r\n]+)").getMatch(0);
                if (missingResource != null) {
                    LogV3.info("  >>" + header("skipped") + "Missing Test Resource: " + missingResource);
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
                return;
            } else if (exit == 1) {
                LogV3.info("  >>" + header("failed") + "Exit with ExitCode " + exit);
                if (result.getStdOutString().length() > 0) {
                    LogV3.info("      " + result.getStdOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                if (result.getErrOutString().length() > 0) {
                    LogV3.info("      " + result.getErrOutString().replaceAll("[\r\n]{1,2}", "\r\n      "));
                }
                System.exit(exit);
            } else {
                TESTS_OK.add(cls.getName());
                LogV3.info("  >>" + header("success"));
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
