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

import static org.appwork.testframework.AWTest.logInfoAnyway;

import java.awt.AWTException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.LogV3Factory;
import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.sink.SimpleFormatter;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.utils.Application;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.LineParsingOutputStream;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 10.12.2021
 *
 */
public class IDETestRunner {
    /**
     * @author thomas
     * @date 30.09.2024
     *
     */
    public static class MyPrintStream extends PrintStream {
        /**
         * @param out
         */
        public MyPrintStream(OutputStream out) {
            super(out);
        }
    }

    private static File lastExecutedTestsFile = null;

    private static void init() {
        Application.setApplication(".tests");
        lastExecutedTestsFile = Application.getResource("cfg/lastExecutedTests.classes");
        AWTest.initLogger(new SimpleFormatter() {
            {
                offsetForThrownAt = new IntByReference(20);
            }

            @Override
            protected String createPre(final LogRecord2 record, final String sourceString) {
                if ((IDETestRunner.class.getSimpleName() + ".java").equals(record.getThrownAt().getFileName())) {
                    return fillPre(longTimestamp.get().format(new Date(record.timestamp)), " ", offsetForTimestamp) + " - ";
                }
                return fillPre(longTimestamp.get().format(new Date(record.timestamp)), " ", offsetForTimestamp) + " - " + fillPost("(" + record.getThrownAt().getFileName() + ":" + record.getThrownAt().getLineNumber() + ")." + abbr(record.getThrownAt().getMethodName(), 30), " ", offsetForThrownAt) + " > ";
            }
        });
    }

    public static void main(final String[] args) {
        init();
        PrintStream outBefore = System.out;
        PrintStream errBefore = System.err;
        System.setOut(new MyPrintStream(new LineParsingOutputStream(Charset.forName("UTF-8")) {
            @Override
            protected void onNextLine(NEWLINE newLine, long line, StringBuilder sb, int startIndex, int endIndex) {
                LogV3.info("stdout> " + sb.substring(startIndex, endIndex));
            }
        }));
        System.setErr(new MyPrintStream(new LineParsingOutputStream(Charset.forName("UTF-8")) {
            @Override
            protected void onNextLine(NEWLINE newLine, long line, StringBuilder sb, int startIndex, int endIndex) {
                LogV3.severe("stderr> " + sb.substring(startIndex, endIndex));
            }
        }));
        try {
            runClasspathTests();
        } catch (Throwable e) {
            AWTest.setLoggerSilent(false, true);
            LogV3.log(e);
        } finally {
            System.setErr(errBefore);
            System.setOut(outBefore);
        }
        if (!Arrays.asList(args).contains("-noexit")) {
            LogV3.disableSysout();
            System.exit(0);
        }
    }

    public static void runClasspathTests() throws URISyntaxException, ClassNotFoundException {
        AWTest.pauseLogger();
        final ArrayList<String> testClasses = new ArrayList<String>();
        final ArrayList<String> lastExecutedTestClasses = new ArrayList<String>();
        String lastFailedClassName = null;
        if (lastExecutedTestsFile != null && lastExecutedTestsFile.isFile()) {
            try {
                final String lastExecutedTests[] = IO.readFileToTrimmedString(lastExecutedTestsFile).split("\r\n");
                for (String lastExecutedTest : lastExecutedTests) {
                    if (StringUtils.isNotEmpty(lastExecutedTest)) {
                        lastExecutedTestClasses.add(lastExecutedTest);
                    }
                }
            } catch (IOException e) {
                LogV3.log(e);
            }
        }
        for (final URL url : ClassPathScanner.getClassPath()) {
            // System.out.println(url);
            final File root = new File(url.toURI());
            if (root.isDirectory()) {
                AWTest.logInfoAnyway("Scan " + root);
                final List<File> files = Files.getFiles(true, true, root);
                int testsFound = 0;
                for (final File file : files) {
                    final String rel = Files.getRelativePath(root, file);
                    if (rel.matches("(?i).*(test|ide).*\\.class$")) {
                        if (file.isFile()) {
                            testClasses.add(rel.replace("/", ".").substring(0, rel.length() - ".class".length()));
                            testsFound++;
                        }
                    }
                }
                AWTest.logInfoAnyway("Scanned " + root + " and found " + files.size() + " files and " + testsFound + " test-related classes/files");
            }
        }
        // shuffle test classes, tests must not rely on specific order of execution
        Collections.shuffle(testClasses);
        final Iterator<String> it = lastExecutedTestClasses.iterator();
        while (it.hasNext()) {
            final String lastExecutedTestClass = it.next();
            if (testClasses.remove(lastExecutedTestClass)) {
                if (it.hasNext()) {
                    testClasses.add(lastExecutedTestClass);
                } else {
                    testClasses.add(0, lastExecutedTestClass);
                }
            }
        }
        int actualTestCount = 0;
        for (final String testClass : testClasses) {
            final Class<?> cls = Class.forName(testClass, false, Thread.currentThread().getContextClassLoader());
            if (TestInterface.class.isAssignableFrom(cls) && TestInterface.class != cls && cls != AWTest.class) {
                actualTestCount++;
            }
        }
        AWTest.logInfoAnyway("Found  " + testClasses.size() + " test-related classes/files - Actual Tests: " + actualTestCount);
        for (final String testClass : testClasses) {
            final Class<?> cls = Class.forName(testClass, false, Thread.currentThread().getContextClassLoader());
            if (TestInterface.class.isAssignableFrom(cls) && TestInterface.class != cls && cls != AWTest.class) {
                runTestInternal(cls);
            }
        }
        AWTest.logInfoAnyway("Finished IDE Build Tests");
        lastExecutedTestsFile.delete();
    }

    /**
     * @param cls
     */
    private static void runTestInternal(Class<?> cls) {
        try {
            if (Modifier.isAbstract(cls.getModifiers())) {
                return;
            } else if (cls.isAnonymousClass()) {
                return;
            }
            final File parent = lastExecutedTestsFile.getParentFile();
            if (!parent.exists()) {
                parent.mkdirs();
            }
            final FileOutputStream fos = new FileOutputStream(lastExecutedTestsFile, true);
            try {
                fos.write(("\r\n" + cls.getName()).getBytes("UTF-8"));
            } finally {
                fos.close();
            }
            AWTest.logInfoAnyway("[** START **]" + cls.getName());
            Thread.currentThread().setName("Run Test: " + cls.getName() + " Since " + DateMapper.formatJsonDefault(new Date()));
            System.setProperty("AWTEST.CLASS", cls.getName());
            LogV3Factory factory = LogV3.getFactory();
            try {
                ((TestInterface) ClassCache.getClassCache(cls).getInstance()).runTest();
            } finally {
                // restore factory-
                if (LogV3.getFactory() != factory) {
                    LogV3.setFactory(factory);
                    logInfoAnyway("Restore LogFactory!");
                }
            }
            if (!CompiledType.isThreadLocalCacheEmpty()) {
                throw new AWTException("CompiledType.ThreadLocalCache is not empty!");
            } else {
                AWTest.clearLoggerCache();
                AWTest.logInfoAnyway("    >> SUCCESS");
            }
        } catch (Exception e) {
            AWTest.setLoggerSilent(false, true);
            // Force to cached error log.
            LogV3.info("FAILED " + cls.getName());
            LogV3.log(e);
            LogV3.disableSysout();
            System.exit(1);
        }
    }

    /**
     * @param class1
     */
    public static void run(Class<? extends TestInterface> cls) {
        init();
        runTestInternal(cls);
    }
}
