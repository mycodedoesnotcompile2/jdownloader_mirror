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

import java.awt.Image;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.ParameterizedType;
import java.net.MalformedURLException;
import java.nio.charset.Charset;
import java.security.Permission;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.jar.JarInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.LoggerToSink;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.AbstractSink;
import org.appwork.loggingv3.simple.sink.LogToFileSink;
import org.appwork.loggingv3.simple.sink.LogToStdOutSink;
import org.appwork.loggingv3.simple.sink.SimpleFormatter;
import org.appwork.loggingv3.simple.sink.Sink;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.utils.Application;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.net.LineParsingOutputStream;
import org.appwork.utils.net.NoClosingInputStream;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 14.01.2022
 *
 */
public abstract class AWTest implements PostBuildTestInterface, TestInterface {
    // static import helper: Window > Preferences > Java > Editor > Content Assist > Favorites
    /**
     * @author thomas
     * @date 16.12.2021
     *
     */
    public static class LogCache extends AbstractSink {
        private final CopyOnWriteArrayList<LogRecord2> buffer = new CopyOnWriteArrayList<LogRecord2>();

        public List<LogRecord2> getBuffer() {
            return this.buffer;
        }

        @Override
        public void publish(final LogRecord2 record) {
            if (record != null) {
                this.buffer.add(record);
            }
        }

        public void clear() {
            this.buffer.clear();
        }
    }

    public static abstract class AssertAnException<ExceptionType extends Exception> {
        public static enum MODE {
            /**
             * true if the exception is instance of the target
             */
            INSTANCEOF,
            /**
             * true if the exception or any of it's causes is instance of the target
             */
            CONTAINS
        }

        private final CompiledType expectedExceptionType;
        private final MODE         mode;

        /**
         * @throws Exception
         *
         */
        public AssertAnException() throws Exception {
            this(MODE.INSTANCEOF);
        }

        public AssertAnException(final MODE mode) throws Exception {
            this.mode = mode != null ? mode : MODE.INSTANCEOF;
            this.expectedExceptionType = CompiledType.create(((ParameterizedType) this.getClass().getGenericSuperclass()).getActualTypeArguments()[0]);
            this.checkAssert();
        }

        protected abstract void run() throws Exception;

        private void checkAssert() throws Exception {
            try {
                this.run();
            } catch (final Exception e) {
                if (this.validateException(e)) {
                    LogV3.info("Exception as expected: " + Exceptions.toCauseChainClassString(e, true) + " - " + e.getMessage());
                    // fine;
                    return;
                } else {
                    throw e;
                }
            }
            throw new Exception("Expected an Exception : " + this.expectedExceptionType);
        }

        protected boolean validateException(final Exception e) {
            switch (this.mode) {
            case INSTANCEOF:
                return CompiledType.create(e.getClass()).isInstanceOf(this.expectedExceptionType.raw);
            case CONTAINS:
                return Exceptions.containsInstanceOf(e, (Class<? extends Throwable>) this.expectedExceptionType.raw);
            default:
                throw new WTFException("Unsupported mode:" + this.mode);
            }
        }
    }

    private static volatile boolean TOOLS_JAR_INIT_DONE = false;
    static SimpleFormatter          LOG_FORMATER;
    static AWTest.LogCache          CACHE;
    volatile static boolean         SILENT              = false;
    static LogToStdOutSink          CONSOLE_LOGGER;
    static SimpleLoggerFactory      LOGGER;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.PostBuildTestInterface#runPostBuildTest(java.lang.String[], java.io.File)
     */
    @Override
    public void runPostBuildTest(final String[] args, final File workingDirectory) throws Exception {
        this.runTest();
    }

    public static class AssertThat {
        private final Object object;

        /**
         * @param o
         */
        public AssertThat(final Object o) {
            this.object = o;
        }

        /**
         * @param d
         * @return
         * @throws Exception
         */
        public void is(final Object d) throws Exception {
            assertEquals(this.object, d);
        }

        /**
         * @param i
         * @throws Exception
         */
        public void isNumber(final Number n) throws Exception {
            if (this.object instanceof Number) {
                if (!CompareUtils.equalsNumber((Number) this.object, n)) {
                    throw new Exception("a does not equal b. " + this.object + "!=" + n);
                }
            } else {
                throw new Exception("a is not a number " + this.object);
            }
        }

        /**
         * @param strings
         * @throws Exception
         */
        public void equalsDeep(final Object other) throws Exception {
            assertEqualsDeep(this.object, other);
        }

        /**
         * @param strings
         * @throws Exception
         */
        @Override
        public boolean equals(final Object other) {
            throw new WTFException("Wrong Equals Method!");
        }

        /**
         * @param strings
         * @throws Exception
         */
        public void equals(final String other) {
            try {
                assertEquals(this.object, other);
            } catch (final Exception e) {
                throw new WTFException(e);
            }
        }

        /**
         * @param parse
         * @throws Exception
         */
        public void isHigherThan(final Comparable other) throws Exception {
            if (this.object instanceof Number) {
                assertTrue(CompareUtils.compareNumber((Number) this.object, (Number) other) > 0);
            } else if (this.object instanceof Comparable) {
                assertTrue(((Comparable) this.object).compareTo(other) > 0);
            } else {
                throw new Exception("object is not a comparable");
            }
        }

        public void isLowerThan(final Comparable other) throws Exception {
            if (this.object instanceof Number) {
                assertTrue(CompareUtils.compareNumber((Number) this.object, (Number) other) < 0);
            } else if (this.object instanceof Comparable) {
                assertTrue(((Comparable) this.object).compareTo(other) < 0);
            } else {
                throw new Exception("object is not a comparable");
            }
        }

        public void isSameAs(final Comparable other) throws Exception {
            if (this.object instanceof Number) {
                assertTrue(CompareUtils.compareNumber((Number) this.object, (Number) other) == 0);
            } else if (this.object instanceof Comparable) {
                assertTrue(((Comparable) this.object).compareTo(other) == 0);
            } else {
                throw new Exception("object is not a comparable");
            }
        }

        /**
         * @param string
         */
        public void equalsObject(final Object other) {
            try {
                assertEquals(this.object, other);
            } catch (final Exception e) {
                throw new WTFException(e);
            }
        }

        /**
         * @param object2
         * @throws Exception
         */
        public void isNot(final Object obj) throws Exception {
            assertEqualsNot(this.object, obj);
        }
    }

    public static AssertThat assertThat(final Object o) {
        return new AssertThat(o);
    }

    public static void assertEqualsDeep(final Object a, final Object b) throws Exception {
        if (!CompareUtils.equalsDeep(a, b)) {
            throw new Exception("a does not equal b. " + Deser.toString(a, SC.LOG_SINGLELINE) + "!=" + Deser.toString(b, SC.LOG_SINGLELINE));
        }
    }

    public static void assertNull(final Object b) throws Exception {
        assertEquals(null, b);
    }

    public static void assertEquals(final Object a, final Object b) throws Exception {
        if (a instanceof Number && b instanceof Number) {
            if (!CompareUtils.equalsNumber((Number) a, (Number) b)) {
                throw new Exception("a does not equal b. \r\n" + a + " != \r\n" + b);
            } else {
                return;
            }
        }
        if (!Objects.equals(a, b)) {
            throw new Exception("a does not equal b. \r\n" + a + " != \r\n" + b);
        } else {
            if (a != null && b != null) {
                if (a.hashCode() != b.hashCode()) {
                    throw new Exception("a equals b but hashCode is different");
                }
            }
        }
    }

    public static void assertEqualsNot(final Object a, final Object b) throws Exception {
        if (Objects.equals(a, b)) {
            throw new Exception("a equals b. " + a + "==" + b);
        }
    }

    public void assertEqualsDeepNot(final Object a, final Object b) throws Exception {
        if (CompareUtils.equalsDeep(a, b)) {
            throw new Exception("Test Failed. 'a' and 'b'  equal");
        }
    }

    public static void validateZipOrJar(String path, InputStream is) throws IOException {
        is = new NoClosingInputStream(is);
        final ZipInputStream zipStream = path.toLowerCase(Locale.ROOT).endsWith(".jar") ? new JarInputStream(is, true) : new ZipInputStream(is);
        try {
            is = null;
            final byte[] buffer = new byte[32767];
            if (path.length() > 0) {
                path += "!";
            }
            final HashSet<String> dupes = new HashSet<String>();
            while (true) {
                try {
                    final ZipEntry e = zipStream.getNextEntry();
                    if (e == null) {
                        break;
                    }
                    if (!dupes.add(e.getName())) {
                        throw new IOException("Path Dupe in " + path + e.getName());
                    }
                    if (e.isDirectory()) {
                    } else {
                        if (e.getName().toLowerCase(Locale.ROOT).endsWith(".jar") || e.getName().toLowerCase(Locale.ROOT).endsWith(".zip")) {
                            validateZipOrJar(path + e.getName(), zipStream);
                        } else {
                            int len;
                            while ((len = zipStream.read(buffer)) != -1) {
                                if (len > 0) {
                                }
                            }
                        }
                    }
                    zipStream.closeEntry();
                } catch (final IOException e) {
                    throw e;
                }
            }
        } finally {
            zipStream.close();
        }
    }

    private static boolean SINK_ACCESS_GRANTED = false;
    public static boolean  VERBOSE             = false;

    /**
     * Deletes all cached entries so far. This is used after a Sub-Test is successful, and we are sure that we don't need its debug output
     * any more
     */
    public static void clearLoggerCache() {
        final LogCache cache = CACHE;
        if (cache != null) {
            cache.clear();
        }
    }

    /**
     *
     */
    public static void initLogger(final SimpleFormatter formater) {
        LogV3.setFactory(new SimpleLoggerFactory() {
            /*
             * (non-Javadoc)
             *
             * @see org.appwork.loggingv3.simple.SimpleLoggerFactory#removeSink(org.appwork.loggingv3.simple.sink.Sink)
             */
            @Override
            public synchronized boolean removeSink(final Sink sink) {
                if (!SINK_ACCESS_GRANTED) {
                    return false;
                }
                return super.removeSink(sink);
            }

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.loggingv3.simple.SimpleLoggerFactory#setSinkToConsole(org.appwork.loggingv3.simple.sink.LogToStdOutSink)
             */
            @Override
            public void setSinkToConsole(final LogToStdOutSink sinkToConsole) {
                if (!SINK_ACCESS_GRANTED) {
                    return;
                }
                super.setSinkToConsole(sinkToConsole);
            }

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.loggingv3.simple.SimpleLoggerFactory#setSinkToFile(org.appwork.loggingv3.simple.sink.LogToFileSink)
             */
            @Override
            public void setSinkToFile(final LogToFileSink sinkToFile) {
                if (!SINK_ACCESS_GRANTED) {
                    return;
                }
                super.setSinkToFile(sinkToFile);
            }

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.loggingv3.simple.SimpleLoggerFactory#addSink(org.appwork.loggingv3.simple.sink.Sink)
             */
            @Override
            public boolean addSink(final Sink sink) {
                if (!SINK_ACCESS_GRANTED) {
                    return false;
                }
                return super.addSink(sink);
            }

            @Override
            protected LoggerToSink createLogger(final Object name) {
                return new LoggerToSink(this) {
                    /*
                     * (non-Javadoc)
                     *
                     * @see org.appwork.loggingv3.AbstractLogger#getThrownAt()
                     */
                    @Override
                    public StackTraceElement getThrownAt() {
                        StackTraceElement last = null;
                        final Exception localEx = new Exception();
                        StackTraceElement found = null;
                        for (final StackTraceElement es : localEx.getStackTrace()) {
                            last = es;
                            if ("java.io.PrintStream".equals(es.getClassName())) {
                                if ("println".equals(es.getMethodName())) {
                                    found = null;
                                    // everything so far is logging stack.. throw away
                                    continue;
                                }
                            }
                            if ("java.lang.Throwable".equals(es.getClassName())) {
                                if ("printStackTrace".equals(es.getMethodName())) {
                                    found = null;
                                    // everything so far is logging stack.. throw away
                                    continue;
                                }
                            }
                            if (this.filterThrownAtEntries(es)) {
                                continue;
                            }
                            if (found == null) {
                                found = es;
                            }
                        }
                        if (found == null) {
                            return last;
                        }
                        return found;
                    }

                    @Override
                    protected boolean filterThrownAtEntries(final StackTraceElement es) {
                        if (super.filterThrownAtEntries(es)) {
                            return true;
                        } else if (es.getClassName().equals(AWTest.class.getName())) {
                            return true;
                        } else if (es.getClassName().startsWith(AWTest.class.getPackage().getName() + ".")) {
                            return true;
                        } else {
                            return false;
                        }
                    }
                };
            }
        }.initDefaults());
        LOGGER = ((SimpleLoggerFactory) LogV3.getFactory());
        removeSink(LOGGER.getSinkToFile());
        CONSOLE_LOGGER = new LogToStdOutSink();
        LOG_FORMATER = formater;
        CONSOLE_LOGGER.setFormatter(LOG_FORMATER);
        setSinkToConsole(CONSOLE_LOGGER);
    }

    /**
     * @param cONSOLE_LOGGER2
     */
    private static void setSinkToConsole(final LogToStdOutSink sink) {
        try {
            SINK_ACCESS_GRANTED = true;
            LOGGER.setSinkToConsole(sink);
        } finally {
            SINK_ACCESS_GRANTED = false;
        }
    }

    /**
     * @param sinkToFile
     */
    private synchronized static void removeSink(final Sink sink) {
        try {
            SINK_ACCESS_GRANTED = true;
            LOGGER.removeSink(sink);
        } finally {
            SINK_ACCESS_GRANTED = false;
        }
    }

    /**
     * enableds Log to console and throws all cached log entries away
     */
    public static void releaseLogger() {
        setLoggerSilent(false, false);
    }

    /**
     * Disabled Log-console and caches all logs. Must get released via #releaseLogger
     */
    public static boolean pauseLogger() {
        return setLoggerSilent(true, true);
    }

    /**
     * Can be used to disable the logger during a test and push the log cache only if the test failed;
     *
     * @param b
     * @param c
     * @return
     */
    public synchronized static boolean setLoggerSilent(final boolean silent, boolean cache) {
        if (AWTest.SILENT == silent) {
            return false;
        } else if (AWTest.LOGGER == null) {
            // no logger set. ignore request
            return false;
        }
        if (VERBOSE) {
            cache = true;
        }
        AWTest.SILENT = silent;
        if (silent) {
            removeSink(AWTest.CONSOLE_LOGGER);
            if (cache) {
                AWTest.CACHE = new AWTest.LogCache();
                AWTest.CACHE.setFormatter(AWTest.LOG_FORMATER);
                addSink(AWTest.CACHE);
            }
        } else {
            setSinkToConsole(AWTest.CONSOLE_LOGGER);
            if (AWTest.CACHE != null) {
                removeSink(CACHE);
                if (cache) {
                    for (final LogRecord2 record : AWTest.CACHE.getBuffer()) {
                        AWTest.CONSOLE_LOGGER.publish(record);
                    }
                }
                AWTest.CACHE = null;
            }
        }
        return true;
    }

    public synchronized static List<LogRecord2> getCachedLogRecords() {
        if (AWTest.CACHE != null) {
            return new ArrayList<LogRecord2>(CACHE.getBuffer());
        } else {
            return new ArrayList<LogRecord2>();
        }
    }

    /**
     * @param cACHE2
     */
    private static void addSink(final Sink sink) {
        try {
            SINK_ACCESS_GRANTED = true;
            LOGGER.addSink(sink);
        } finally {
            SINK_ACCESS_GRANTED = false;
        }
    }

    public static void assertTrue(final Boolean b, final String message) throws Exception {
        if (!Boolean.TRUE.equals(b)) {
            throw new Exception(message);
        }
    }

    public static void assertFalse(final Boolean b, final String message) throws Exception {
        if (!Boolean.FALSE.equals(b)) {
            throw new Exception(message);
        }
    }

    public static void assertFalse(final Boolean b) throws Exception {
        assertFalse(b, "Value=" + b + " Expected=false");
    }

    public static void assertTrue(final Boolean b) throws Exception {
        assertTrue(b, "Value=" + b + " Expected=true");
    }

    public static File assertFileExists(final File file) throws Exception {
        if (!file.exists()) {
            throw new Exception("File/Folder does not exist: " + file);
        } else {
            return file;
        }
    }

    /**
     * lower than
     *
     * @param length
     * @param i
     * @throws Exception
     */
    public static void assertLt(final long length, final long i) throws Exception {
        if (length < i) {
            // fine
        } else {
            throw new Exception(length + " is not lower than " + i);
        }
    }

    public static void assertPathExistsInZip(final File zip, final String path) throws Exception {
        final ZipFile zipFile = new ZipFile(zip);
        try {
            if (zipFile.getEntry(path) == null) {
                throw new Exception("Path in Zip is missing: " + zip + "!" + path);
            }
        } finally {
            zipFile.close();
        }
    }

    public static void assertImageDimension(final File zip, final String path, final int w, final int h) throws Exception {
        final ZipFile zipFile = new ZipFile(zip);
        try {
            final ZipEntry entry = zipFile.getEntry(path);
            if (entry == null) {
                throw new Exception("Path in Zip is missing: " + zip + "!" + path);
            }
            final File tmp = Application.getResource("tmp/image" + UniqueAlltimeID.next() + "." + Files.getExtension(path, true));
            IO.secureWrite(tmp, IO.readStream(-1, zipFile.getInputStream(entry)));
            Icon icon = org.appwork.resources.Theme.getFACTORY().urlToVectorIcon(tmp.toURL(), -1, -1);
            if (icon == null) {
                Image image = org.appwork.resources.Theme.getFACTORY().urlToImage(tmp.toURL());
                if (image != null) {
                    icon = new ImageIcon(image);
                }
            }
            tmp.delete();
            tmp.deleteOnExit();
            if (icon.getIconWidth() < w || icon.getIconHeight() < h) {
                throw new Exception("Image " + zip + "!" + path + " has wrong dimensions: " + icon.getIconWidth() + "/" + icon.getIconHeight() + " (Expected: " + w + "/" + h + ")");
            }
        } finally {
            zipFile.close();
        }
    }

    public static void assertPathExistsNotInZip(final File zip, final String path) throws Exception {
        final ZipFile zipFile = new ZipFile(zip);
        try {
            if (zipFile.getEntry(path) != null) {
                throw new Exception("Path in Zip exists: " + zip + "!" + path);
            }
        } finally {
            zipFile.close();
        }
    }

    public static String readStringFromZip(final File zip, final String path) throws Exception {
        final ZipFile zipFile = new ZipFile(zip);
        try {
            final ZipEntry entry = zipFile.getEntry(path);
            if (entry == null) {
                throw new Exception("Path in Zip is missing: " + zip + "!" + path);
            }
            return IO.readStreamToString(zipFile.getInputStream(entry), -1);
        } finally {
            zipFile.close();
        }
    }

    public static void assertGt(final long length, final long i) throws Exception {
        if (length > i) {
            // finde
        } else {
            throw new Exception(length + " is not greater than " + i);
        }
    }

    public static void run() {
        BuildDecisions.setEnabled(false);
        IDETestRunner.run(getTestClass());
        LogV3.disableSysout();
    }

    public static Class<? extends TestInterface> getTestClass() {
        final StackTraceElement[] st = new Exception().getStackTrace();
        for (final StackTraceElement e : st) {
            if (!e.getClassName().startsWith(AWTest.class.getName())) {
                try {
                    final Class<? extends TestInterface> cls = (Class<? extends TestInterface>) Class.forName(e.getClassName());
                    if (TestInterface.class.isAssignableFrom(cls)) {
                        return cls;
                    }
                } catch (final ClassNotFoundException ex) {
                    throw new WTFException(ex);
                }
            }
        }
        throw new WTFException("TestClass not found");
    }

    /**
     * @param deserializedObject
     * @throws Exception
     */
    public static void assertNotNull(final Object obj) throws Exception {
        if (obj == null) {
            throw new Exception("Object is null");
        }
    }

    public static void asswertCachedLogsRecordMatches(final String pattern) throws Exception {
        for (final LogRecord2 lr : getCachedLogRecords()) {
            if (LOG_FORMATER.format(lr).matches(pattern)) {
                return;
            }
        }
        setLoggerSilent(false, true);
        throw new Exception("Cached Logs do not contain the pattern " + pattern);
    }

    public static void assertType(final Object obj, final Class<?> cls) throws Exception {
        if (obj == null) {
            throw new Exception("Object " + obj + " is null");
        } else if (!(cls.isAssignableFrom(obj.getClass()))) {
            throw new Exception("Object " + obj + " is not of type " + cls.getName());
        }
    }

    /**
     * @param string
     */
    public synchronized static void logInfoAnyway(final String string) {
        final boolean wasSilent = AWTest.SILENT;
        if (wasSilent) {
            final LogCache keepCache = CACHE;
            if (keepCache != null) {
                removeSink(keepCache);
            }
            CACHE = null;
            releaseLogger();
            LogV3.info(string);
            setLoggerSilent(true, false);
            if (keepCache != null) {
                AWTest.CACHE = keepCache;
                addSink(keepCache);
            }
        } else {
            LogV3.info(string);
        }
    }

    /**
     * @param f
     * @return
     * @throws Exception
     */
    public static String verifyJar(final File f) throws Exception {
        ensureToolsJar();
        final PrintStream out = System.out;
        final PrintStream err = System.err;
        final StringBuilder outBuffer = new StringBuilder();
        final StringBuilder errBuffer = new StringBuilder();
        System.setOut(new PrintStream(new LineParsingOutputStream(Charset.forName("UTF-8")) {
            @Override
            protected void onNextLine(final NEWLINE newLine, final long line, final StringBuilder sb, final int startIndex, final int endIndex) {
                outBuffer.append(sb.substring(startIndex, endIndex) + "\r\n");
            }
        }));
        System.setErr(new PrintStream(new LineParsingOutputStream(Charset.forName("UTF-8")) {
            @Override
            protected void onNextLine(final NEWLINE newLine, final long line, final StringBuilder sb, final int startIndex, final int endIndex) {
                errBuffer.append(sb.substring(startIndex, endIndex) + "\r\n");
            }
        }));
        Object securityManager = null;
        try {
            if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7) && JVMVersion.get() < JVMVersion.JAVA_18) {
                securityManager = System.getSecurityManager();
                System.setSecurityManager(new SecurityManager() {
                    @Override
                    public void checkExit(final int status) {
                        throw new WTFException("Unexpected Exit with code " + status);
                    }

                    @Override
                    public void checkPermission(final Permission perm) {
                        // System.out.println("Permission " + perm);
                    }

                    @Override
                    public void checkPackageAccess(final String pkg) {
                        // System.out.println("checkPackageAccess " + pkg);
                    }

                    @Override
                    public void checkPermission(final Permission perm, final Object context) {
                        // System.out.println("Permission " + perm);
                    }
                });
            }
            final Class<?> cls = Class.forName("sun.security.tools.jarsigner.Main");
            cls.getDeclaredMethod("main", new Class[] { String[].class }).invoke(null, new Object[] { new String[] { "-verify", f.getAbsolutePath() } });
        } catch (final Exception e) {
            throw new Exception(errBuffer + "\r\n" + outBuffer, e);
        } finally {
            if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7) && JVMVersion.get() < JVMVersion.JAVA_18) {
                System.setSecurityManager((SecurityManager) securityManager);
            }
            System.setOut(out);
            System.setErr(err);
        }
        if (errBuffer.length() > 0) {
            throw new Exception("Jar Verify failed: " + errBuffer + "\r\n+" + outBuffer);
        } else {
            return outBuffer.toString().trim();
        }
    }

    /**
     * @throws IOException
     * @throws MalformedURLException
     *
     */
    public synchronized static void ensureToolsJar() throws MalformedURLException, IOException {
        if (TOOLS_JAR_INIT_DONE) {
            return;
        }
        if (Application.getJavaVersion() >= JVMVersion.JAVA_9) {
            // throw new WTFException("This application musst run with java 1.8");
            // tools is a module and should be availabl
            TOOLS_JAR_INIT_DONE = true;
            return;
        }
        final String jvm = Application.getJarFile(String.class).getParentFile().getParentFile().getParentFile().getAbsolutePath();
        final File tools = new File(jvm, "lib/tools.jar");
        if (tools.exists()) {
            Application.addUrlToClassPath(tools.toURI().toURL(), AWTest.class.getClassLoader());
            TOOLS_JAR_INIT_DONE = true;
        }
    }

    /**
     * @param zip
     * @throws IOException
     */
    public static void validateZipOrJar(final File zip) throws IOException {
        final InputStream is = new BufferedInputStream(new FileInputStream(zip));
        try {
            validateZipOrJar(zip.getAbsolutePath(), is);
        } finally {
            is.close();
        }
    }
}
