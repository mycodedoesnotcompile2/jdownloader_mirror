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
package org.appwork.loggingv3.simple.sink.test;

import java.io.File;
import java.io.IOException;

import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.sink.LogToFileSink;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.Time;

/**
 * @author daniel
 * @date Apr 28, 2023
 *
 */
public class LogToFileSinkTest extends AWTest {

    public static void main(final String[] args) throws IOException {
        AWTest.run();
    }

    public void runTest() throws Exception {
        final File tmp = Application.getHomeResource("tmp" + Time.timestamp());
        try {
            final LogToFileSink log1 = new LogToFileSink(tmp, "LogToFileSink-Test-\\d", 1) {

                @Override
                public void setEnabled(boolean enabled) {
                    super.setEnabled(enabled);
                    if (!enabled) {
                        onShutdown();
                    }
                }

                @Override
                protected void runPackToSingleFileOnShutdown() {

                }

                @Override
                public void runPostCleanupAndCompressionThread() {

                }
            };
            log1.publish(new LogRecord2(null, "Test", null));
            assertEquals(1, log1.getLogFilesOrFolders(false).size());
            assertEquals(0, log1.getLogFilesOrFolders(true).size());
            LogToFileSink log2 = new LogToFileSink(tmp, "test-\\d", 1) {

                @Override
                public void setEnabled(boolean enabled) {
                    super.setEnabled(enabled);
                    if (!enabled) {
                        onShutdown();
                    }
                }

                @Override
                protected void runPackToSingleFileOnShutdown() {

                }

                @Override
                public void runPostCleanupAndCompressionThread() {

                }
            };
            assertEquals(1, log1.getLogFilesOrFolders(false).size());
            assertEquals(0, log1.getLogFilesOrFolders(true).size());
            log2.publish(new LogRecord2(null, "Test", null));
            assertEquals(2, log1.getLogFilesOrFolders(false).size());
            assertEquals(2, log2.getLogFilesOrFolders(false).size());
            assertEquals(0, log1.getLogFilesOrFolders(true).size());
            assertEquals(0, log2.getLogFilesOrFolders(true).size());
            log1.setEnabled(false);
            assertEquals(2, log1.getLogFilesOrFolders(false).size());
            assertEquals(2, log2.getLogFilesOrFolders(false).size());
            assertEquals(0, log1.getLogFilesOrFolders(true).size());
            assertEquals(1, log2.getLogFilesOrFolders(true).size());
            log2.setEnabled(false);
            assertEquals(2, log1.getLogFilesOrFolders(false).size());
            assertEquals(2, log2.getLogFilesOrFolders(false).size());
            assertEquals(1, log1.getLogFilesOrFolders(true).size());
            assertEquals(1, log2.getLogFilesOrFolders(true).size());
        } finally {
            ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {

                @Override
                public void onShutdown(ShutdownRequest shutdownRequest) {
                    try {
                        Files.deleteRecursiv(tmp);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            });
        }
    }
}
