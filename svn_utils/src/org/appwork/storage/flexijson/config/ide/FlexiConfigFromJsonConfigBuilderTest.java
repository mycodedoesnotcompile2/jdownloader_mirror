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

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.flexijson.config.FlexiConfigBuilder;
import org.appwork.storage.flexijson.config.FlexiConfigBuilder.Source;
import org.appwork.storage.flexijson.config.FlexiConfigFromJsonConfigBuilder;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiInterfaceDefault;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;

/**
 * @author thomas
 * @date 09.11.2022
 *
 */
public class FlexiConfigFromJsonConfigBuilderTest extends AWTest {
    public static interface TestConfigInterface extends FlexiStorableInterface {
        @FlexiInterfaceDefault("12345")
        long getTest();

        void setTest(long v);

        @FlexiInterfaceDefault("[]")
        String[] getArray();

        void setArray(String[] value);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        try {
            try {
                new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, new File("test")).setReadSources(Source.DISK);
                throw new Exception("Expected an IllegalArgumentException because test does not end with .json");
            } catch (IllegalArgumentException e) {
                // expected
            }
            testMigrationFromDisk();
            // these tests may fail in case of jar postbuild tests
            testMigrationFromClassPathRoot();
            testMigrationFromClassPathRelative();
        } catch (Exception e) {
            LogV3.log(e);
            throw e;
        } finally {
            boolean wait = false;
            for (File f : deleteme) {
                if (f.isFile()) {
                    wait = true;
                    break;
                }
            }
            if (wait) {
                Thread.sleep(5000);
                for (File f : deleteme) {
                    if (f.isFile()) {
                        throw new Exception("File should be gone " + f);
                    }
                }
            }
        }
    }

    /**
     * @throws Exception
     *
     */
    private void testMigrationFromClassPathRelative() throws Exception {
        try {
            File res = Application.getResource("testRelative.json");
            // URL location = TestConfigInterface.class.getProtectionDomain().getCodeSource().getLocation();
            URL location = TestConfigInterface.class.getClassLoader().getResource(TestConfigInterface.class.getName().replace(".", "/") + ".class");
            File classLocation = new File(location.toURI()).getParentFile();
            String rel = Files.getRelativePath(Application.getResource(""), res);
            long now = System.currentTimeMillis() + 193219;
            File root = deleteAfterTest(new File(classLocation, rel));
            root.delete();
            IO.secureWrite(root, "{\"test\":" + (now) + "}", null);
            File array = deleteAfterTest(new File(classLocation, rel.replace(".json", "") + ".array.json"));
            array.delete();
            IO.secureWrite(array, "[\"fromfile2\"]", null);
            TestConfigInterface storageFromClassPath = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.CLASSPATH_RELATIVE).setAutoWriteEnabled(false).getStorage();
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "fromfile2" });
            File target = deleteAfterTest(Application.getResource("testRelativeFromFallback.json"));
            target.delete();
            TestConfigInterface storageFromClassPathFallBack = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, target, res).setReadSources(Source.CLASSPATH_RELATIVE).setAutoWriteEnabled(false).getStorage();
            assertThat(target.isFile()).is(false);
            FlexiConfigFromJsonConfigBuilder.get(storageFromClassPathFallBack).writeToDisk();
            assertThat(target.isFile()).is(true);
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "fromfile2" });
            FlexiConfigFromJsonConfigBuilder.get(storageFromClassPathFallBack).setAutoWriteEnabled(false);
        } finally {
            deleteFiles();
        }
    }

    private ArrayList<File> deleteme = new ArrayList<File>();

    public void deleteFiles() throws Exception {
        for (File f : deleteme) {
            if (f.isFile()) {
                java.nio.file.Files.delete(f.toPath());
                assertThat(f.isFile()).is(false);
                // logInfoAnyway("Delete " + f);
            } else {
                // logInfoAnyway("not exists " + f);
            }
        }
    }

    /**
     * @param file
     * @return
     */
    private File deleteAfterTest(File file) {
        deleteme.add(file);
        return file;
    }

    private void testMigrationFromClassPathRoot() throws URISyntaxException, IOException, Exception {
        try {
            File res = deleteAfterTest(Application.getResource("test.json"));
            URL location = TestConfigInterface.class.getProtectionDomain().getCodeSource().getLocation();
            File classLocation = new File(location.toURI());
            String rel = Files.getRelativePath(Application.getResource(""), res);
            long now = System.currentTimeMillis() + 199;
            {
                File file = deleteAfterTest(new File(classLocation, rel));
                file.delete();
                IO.secureWrite(file, "{\"test\":" + (now) + "}", null);
            }
            {
                File file = deleteAfterTest(new File(classLocation, rel.replace(".json", "") + ".array.json"));
                file.delete();
                IO.secureWrite(file, "[\"fromfile2\"]", null);
            }
            TestConfigInterface storageFromClassPath = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.CLASSPATH_ROOT).setAutoWriteEnabled(false).setAutoWriteEnabled(false).getStorage();
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "fromfile2" });
            File target = deleteAfterTest(Application.getResource("testRelativeFromFallback2.json"));
            target.delete();
            assertThat(target.isFile()).is(false);
            TestConfigInterface storageFromClassPathFallBack = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, target, res).setReadSources(Source.CLASSPATH_ROOT).setAutoWriteEnabled(false).setAutoWriteEnabled(false).getStorage();
            assertThat(target.isFile()).is(false);
            FlexiConfigFromJsonConfigBuilder.get(storageFromClassPathFallBack).writeToDisk();
            assertThat(target.isFile()).is(true);
            assertThat(storageFromClassPath.getTest()).isNumber(now);
            assertThat(storageFromClassPath.getArray()).equalsDeep(new String[] { "fromfile2" });
        } finally {
            deleteFiles();
        }
    }

    /**
     * @throws Exception
     *
     */
    private void testMigrationFromDisk() throws Exception {
        try {
            File res = Application.getResource("test.json");
            long now = System.currentTimeMillis();
            File root = deleteAfterTest(new File(res.getAbsolutePath()));
            root.delete();
            IO.secureWrite(root, "{\"test\":" + now + "}", null);
            File array = deleteAfterTest(new File(res.getAbsolutePath().replace(".json", "") + ".array.json"));
            array.delete();
            IO.secureWrite(array, "[\"fromfile\"]", null);
            FlexiConfigFromJsonConfigBuilder<TestConfigInterface> builder = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setReadSources(Source.DISK).setAutoWriteEnabled(false);
            TestConfigInterface storage = builder.setAutoWriteEnabled(false).getStorage();
            assertThat(storage.getTest()).isNumber(now);
            assertThat(storage.getArray()).equalsDeep(new String[] { "fromfile" });
            {
                IO.secureWrite(deleteAfterTest(Application.getResource("testFallback.json")), "{}", null);
                IO.secureWrite(deleteAfterTest(Application.getResource("testFallback.array.json")), "null", null);
                // read via fallback
                res = deleteAfterTest(Application.getResource("notavailable.json"));
                res.delete();
                assertThat(res.isFile()).is(false);
                FlexiConfigFromJsonConfigBuilder<TestConfigInterface> builderFallBack = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res, Application.getResource("testFallback.json")).setAutoWriteEnabled(false).setReadSources(Source.DISK);
                TestConfigInterface storageFallback = builderFallBack.setAutoWriteEnabled(false).getStorage();
                assertThat(storageFallback.getTest()).isNumber(12345);
                assertThat(storageFallback.getArray()).equalsDeep(null);
                builderFallBack.writeToDisk();
                assertThat(res.isFile()).is(true);
                assertThat(Application.getResource("testFallback.array.json").isFile()).is(false);
            }
            res = deleteAfterTest(Application.getResource("test.json"));
            storage.setTest(12);
            // get the builder that build storage.
            FlexiConfigBuilder<?> builder2 = FlexiConfigBuilder.get(storage);
            // deletes the files above
            builder2.writeToDisk();
            assertThat(array.isFile()).is(false);
            assertThat(root.isFile()).is(true);
            assertThat(builder).is(builder2);
            TestConfigInterface storage2 = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, res).setAutoWriteEnabled(false).setReadSources(Source.DISK).setAutoWriteEnabled(false).getStorage();
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
