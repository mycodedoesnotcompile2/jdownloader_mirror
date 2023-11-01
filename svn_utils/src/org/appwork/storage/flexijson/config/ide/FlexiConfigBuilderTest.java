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
package org.appwork.storage.flexijson.config.ide;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.ParsingError;
import org.appwork.storage.flexijson.config.FlexiConfigBuilder;
import org.appwork.storage.flexijson.config.FlexiConfigBuilder.Source;
import org.appwork.storage.flexijson.config.FlexiConfigListener;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiInterfaceDefault;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 09.11.2022
 *
 */
public class FlexiConfigBuilderTest extends AWTest {
    public static interface TestConfigInterface extends FlexiStorableInterface {
        @FlexiInterfaceDefault("12345")
        long getTest();

        void setTest(long v);

        @FlexiInterfaceDefault("[]")
        String[] getArray();

        void setArray(String[] value);
    }

    public static interface SubInterface extends FlexiStorableInterface {
        @FlexiInterfaceDefault("12345")
        long getTest();

        void setTest(long v);

        @FlexiInterfaceDefault("[]")
        String[] getArray();

        void setArray(String[] value);
    }

    public static interface TestInterfaceWithSubInterfaces extends FlexiStorableInterface {
        @FlexiInterfaceDefault("{}")
        TestConfigInterface getObject();

        void setObject(TestConfigInterface obj);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        try {
            testEventSenderAndSubEventSender();
            try {
                new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, new File("test")).setReadSources(Source.DISK);
                throw new Exception("Expected an IllegalArgumentException because test does not end with .json");
            } catch (IllegalArgumentException e) {
                // expected
            }
            testDamagedJson();
            testReadFromDisk();
            // these tests may fail in case of jar postbuild tests
            testReadFromClassPathRoot();
            testReadFromClassPathRelative();
        } finally {
            boolean allGone = true;
            for (File f : deleteme) {
                if (f.isFile()) {
                    allGone = false;
                }
            }
            if (!allGone) {
                Thread.sleep(5000);
                for (File f : deleteme) {
                    if (f.isFile()) {
                        throw new Exception("File should be gone " + f);
                    }
                }
            }
        }
    }

    public void testEventSenderAndSubEventSender() throws Exception {
        try {
            File res = deleteAfterTest(Application.getResource("test.json"));
            res.delete();
            assertThat(res.isFile()).is(false);
            final AtomicInteger count = new AtomicInteger();
            final AtomicInteger count2 = new AtomicInteger();
            final FlexiConfigBuilder<TestInterfaceWithSubInterfaces> builder = new FlexiConfigBuilder<TestInterfaceWithSubInterfaces>(TestInterfaceWithSubInterfaces.class, res) {
                public void onInterfaceValueSet(Object storage, String key, Object oldValue, Object newValue) {
                    super.onInterfaceValueSet(storage, key, oldValue, newValue);
                    count.incrementAndGet();
                };
            }.setReadSources(Source.DISK);
            builder.getEventSender().addListener(new FlexiConfigListener() {
                @Override
                public void onValueModified(FlexiConfigBuilder builder, Object storage, String key, Object oldValue, Object newValue) {
                    count2.incrementAndGet();
                }
            });
            TestInterfaceWithSubInterfaces storage = builder.getStorage();
            TestConfigInterface object = storage.getObject();
            int triggersCallback = 0;
            assertThat(object.getTest()).isNumber(12345);
            triggersCallback++;
            object.setTest(123);
            assertThat(count.get()).isNumber(count2.get());
            assertThat(count.get()).isNumber(triggersCallback);
            assertThat(object.getTest()).isNumber(123);
            object = (TestConfigInterface) new FlexiJSonMapper().jsonToObject(new FlexiJSonObject(), CompiledType.create(TestConfigInterface.class));
            assertThat(object.getTest()).isNumber(12345);
            triggersCallback++;
            storage.setObject(object);
            assertThat(count.get()).isNumber(count2.get());
            assertThat(count.get()).isNumber(triggersCallback);
            triggersCallback++;
            object.setTest(1234);
            assertThat(count.get()).isNumber(count2.get());
            assertThat(count.get()).isNumber(triggersCallback);
            FlexiConfigBuilder<?> builder2 = FlexiConfigBuilder.get(storage);
            builder2.writeToDisk();
        } finally {
            deleteFiles();
        }
    }

    private ArrayList<File> deleteme = new ArrayList<File>();

    public void deleteFiles() {
        for (File f : deleteme) {
            f.delete();
        }
        deleteme.clear();
    }

    /**
     * @param file
     * @return
     */
    private File deleteAfterTest(File file) {
        deleteme.add(file);
        return file;
    }

    /**
     * @throws Exception
     *
     */
    private void testDamagedJson() throws Exception {
        try {
            File res = Application.getResource("test.json");
            long now = System.currentTimeMillis();
            File root = deleteAfterTest(new File(res.getAbsolutePath()));
            root.delete();
            // missing }
            IO.secureWrite(root, "{\"test\":" + now + ",\"array\":[\"fromfile\"]", SYNC.META_AND_DATA);
            final FlexiConfigBuilder<TestConfigInterface> builder = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.DISK);
            new AssertAnException<org.appwork.storage.flexijson.FlexiParserException>() {
                @Override
                public void run() throws Exception {
                    builder.setAutoWriteEnabled(false).getStorage();
                }
            }.start();
            FlexiConfigBuilder<TestConfigInterface> builder3 = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res) {
                public org.appwork.storage.flexijson.FlexiJSONParser createParser(String json) {
                    FlexiJSONParser ret = super.createParser(json);
                    ret.addIgnoreIssues(ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG);
                    return ret;
                };
            };
            TestConfigInterface storage = builder3.setAutoWriteEnabled(false).getStorage();
            assertThat(storage.getTest()).isNumber(now);
            assertThat(storage.getArray()).equalsDeep(new String[] { "fromfile" });
            {
                IO.secureWrite(root = deleteAfterTest(Application.getResource("testFallback.json")), "{array:null}", null);
                // read via fallback
                res = deleteAfterTest(Application.getResource("notavailable.json"));
                res.delete();
                assertThat(res.isFile()).is(false);
                FlexiConfigBuilder<TestConfigInterface> builderFallBack = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res, root) {
                    public org.appwork.storage.flexijson.FlexiJSONParser createParser(String json) {
                        FlexiJSONParser ret = super.createParser(json);
                        ret.addIgnoreIssues(ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG, ParsingError.ERROR_STRING_TOKEN_WITHOUT_QUOTES, ParsingError.ERROR_KEY_WITHOUT_QUOTES);
                        return ret;
                    };
                };
                ;
                TestConfigInterface storageFallback = builderFallBack.setAutoWriteEnabled(false).getStorage();
                assertThat(storageFallback.getTest()).isNumber(12345);
                assertThat(storageFallback.getArray()).equalsDeep(null);
                builderFallBack.writeToDisk();
                assertThat(res.isFile()).is(true);
                assertThat(root.isFile()).is(false);
            }
            res = deleteAfterTest(Application.getResource("test.json"));
            storage.setTest(12);
            // get the builder that build storage.
            FlexiConfigBuilder<?> builder2 = FlexiConfigBuilder.get(storage);
            // deletes the files above
            builder2.writeToDisk();
            assertFileExists(res);
            assertThat(builder3).is(builder2);
            TestConfigInterface storage2 = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res) {
                public org.appwork.storage.flexijson.FlexiJSONParser createParser(String json) {
                    FlexiJSONParser ret = super.createParser(json);
                    ret.addIgnoreIssues(ParsingError.EXPECTED_COMMA_OR_OBJECT_END_TAG);
                    return ret;
                };
            }.setAutoWriteEnabled(false).getStorage();
            assertThat(storage2.getTest()).isNumber(12);
            assertThat(storage2.getArray()).equalsDeep(new String[] { "fromfile" });
        } finally {
            deleteFiles();
        }
    }

    /**
     * @throws Exception
     *
     */
    private void testReadFromClassPathRelative() throws Exception {
        try {
            File res = deleteAfterTest(Application.getResource("testRelative.json"));
            // URL location = TestConfigInterface.class.getProtectionDomain().getCodeSource().getLocation();
            URL location = TestConfigInterface.class.getClassLoader().getResource(TestConfigInterface.class.getName().replace(".", "/") + ".class");
            File classLocation = new File(location.toURI()).getParentFile();
            String rel = Files.getRelativePath(Application.getResource(""), res);
            long now = System.currentTimeMillis() + 193219;
            File root = deleteAfterTest(new File(classLocation, rel));
            root.delete();
            IO.secureWrite(root, "{\"test\":" + (now) + ",\"array\":[\"frsd2omfil3e2\"]}", null);
            TestConfigInterface storageFromClassPath = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.CLASSPATH_RELATIVE).setAutoWriteEnabled(false).getStorage();
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "frsd2omfil3e2" });
            File target = deleteAfterTest(Application.getResource("testRelativeFromFallback.json"));
            target.delete();
            TestConfigInterface storageFromClassPathFallBack = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, target, res).setReadSources(Source.CLASSPATH_RELATIVE).setAutoWriteEnabled(false).getStorage();
            assertThat(target.isFile()).is(false);
            FlexiConfigBuilder.get(storageFromClassPathFallBack).writeToDisk();
            assertThat(target.isFile()).is(true);
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "frsd2omfil3e2" });
        } finally {
            deleteFiles();
        }
    }

    private void testReadFromClassPathRoot() throws URISyntaxException, IOException, Exception {
        try {
            File res = deleteAfterTest(Application.getResource("test.json"));
            URL location = TestConfigInterface.class.getProtectionDomain().getCodeSource().getLocation();
            File classLocation = new File(location.toURI());
            String rel = Files.getRelativePath(Application.getResource(""), res);
            long now = System.currentTimeMillis() + 199;
            {
                File file = deleteAfterTest(new File(classLocation, rel));
                file.delete();
                IO.secureWrite(file, "{\"test\":" + (now) + ",\"array\":[\"fromfil3e2\"]}", null);
            }
            TestConfigInterface storageFromClassPath = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.CLASSPATH_ROOT).setAutoWriteEnabled(false).getStorage();
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "fromfil3e2" });
            File target = deleteAfterTest(Application.getResource("testRelativeFromFallback2.json"));
            target.delete();
            assertThat(target.isFile()).is(false);
            TestConfigInterface storageFromClassPathFallBack = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, target, res).setReadSources(Source.CLASSPATH_ROOT).setAutoWriteEnabled(false).getStorage();
            assertThat(target.isFile()).is(false);
            FlexiConfigBuilder.get(storageFromClassPathFallBack).writeToDisk();
            assertThat(target.isFile()).is(true);
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "fromfil3e2" });
        } finally {
            deleteFiles();
        }
    }

    /**
     * @throws Exception
     *
     */
    private void testReadFromDisk() throws Exception {
        try {
            File res = deleteAfterTest(Application.getResource("test.json"));
            long now = System.currentTimeMillis();
            File root = new File(res.getAbsolutePath());
            root.delete();
            deleteme.add(root);
            IO.secureWrite(root, "{\"test\":" + now + ",\"array\":[\"fromfile\"]}", null);
            FlexiConfigBuilder<TestConfigInterface> builder = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.DISK);
            TestConfigInterface storage = builder.setAutoWriteEnabled(false).getStorage();
            assertThat(storage.getTest()).isNumber(now);
            assertThat(storage.getArray()).equalsDeep(new String[] { "fromfile" });
            {
                IO.secureWrite(deleteAfterTest(Application.getResource("testFallback.json")), "{\"array\":null}", null);
                // read via fallback
                res = deleteAfterTest(Application.getResource("notavailable.json"));
                res.delete();
                assertThat(res.isFile()).is(false);
                FlexiConfigBuilder<TestConfigInterface> builderFallBack = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res, Application.getResource("testFallback.json")).setReadSources(Source.DISK);
                TestConfigInterface storageFallback = builderFallBack.setAutoWriteEnabled(false).getStorage();
                assertThat(storageFallback.getTest()).isNumber(12345);
                assertThat(storageFallback.getArray()).equalsDeep(null);
                builderFallBack.writeToDisk();
                assertThat(res.isFile()).is(true);
                assertThat(Application.getResource("testFallback.json").isFile()).is(false);
            }
            res = deleteAfterTest(Application.getResource("test.json"));
            storage.setTest(12);
            // get the builder that build storage.
            FlexiConfigBuilder<?> builder2 = FlexiConfigBuilder.get(storage);
            // deletes the files above
            builder2.writeToDisk();
            assertThat(root.isFile()).is(true);
            assertThat(builder).is(builder2);
            TestConfigInterface storage2 = new FlexiConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.DISK).setAutoWriteEnabled(false).getStorage();
            assertThat(storage2.getTest()).isNumber(12);
            assertThat(storage2.getArray()).equalsDeep(new String[] { "fromfile" });
        } finally {
            deleteFiles();
        }
    }

    public static void main(String[] args) {
        run();
    }
}
