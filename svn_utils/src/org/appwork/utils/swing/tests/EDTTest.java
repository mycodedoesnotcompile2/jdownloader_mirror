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
package org.appwork.utils.swing.tests;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.swing.EDT;

/**
 * @author daniel
 * @date Mar 1, 2024
 *
 */
public class EDTTest extends AWTest {
    /**
     * @param args
     */
    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!Application.isHeadless()) {
            test_InvokeLater();
            test_RunTest();
            test_Exception();
            test_Interrupt();
        }
    }

    private static void test_Interrupt() throws Exception {
        final AtomicReference<Exception> exception = new AtomicReference<Exception>(null);
        final Object LOCK = new Object();
        try {
            final Thread thread = new Thread() {
                /**
                 * @see java.lang.Thread#run()
                 */
                @Override
                public void run() {
                    try {
                        new EDT<Void, Exception>() {
                            @Override
                            protected Void runInEDT() throws Exception {
                                synchronized (LOCK) {
                                    LOCK.wait();
                                }
                                return null;
                            }
                        }.waitFor();
                        throw new Exception();
                    } catch (Exception e) {
                        exception.set(e);
                    }
                }
            };
            thread.start();
            thread.join(100);
            thread.interrupt();
            thread.join(100);
            assertTrue(exception.get() instanceof InterruptedException);
        } finally {
            synchronized (LOCK) {
                LOCK.notifyAll();
            }
        }
    }

    private static void test_Exception() throws Exception {
        try {
            new EDT<Void, Exception>() {
                @Override
                protected Void runInEDT() throws Exception {
                    throw new IllegalStateException();
                }
            }.waitFor();
            throw new WTFException();
        } catch (IllegalStateException ignore) {
        }
        try {
            final EDT<Void, Exception> edt = new EDT<Void, Exception>() {
                @Override
                protected Void runInEDT() throws Exception {
                    throw new IllegalStateException();
                }
            };
            edt.invokeLater().waitFor();
            throw new WTFException();
        } catch (IllegalStateException ignore) {
        }
    }

    private static void test_RunTest() throws Exception {
        final List<Integer> ret = new CopyOnWriteArrayList<Integer>();
        final AtomicBoolean done = new AtomicBoolean(false);
        new EDT<Void, Exception>() {
            @Override
            protected Void runInEDT() throws Exception {
                for (int index = 0; index < 100; index++) {
                    final int num = index;
                    EDT.run(new Runnable() {
                        @Override
                        public void run() {
                            ret.add(num);
                        }
                    });
                    assertTrue(ret.size() == index + 1);
                }
                EDT.run(new Runnable() {
                    @Override
                    public void run() {
                        synchronized (done) {
                            done.set(true);
                            done.notifyAll();
                        }
                    }
                });
                return null;
            }
        }.waitFor();
        while (done.get() == false) {
            synchronized (done) {
                done.wait();
            }
        }
        for (int index = 0; index < 100; index++) {
            assertEquals(ret.get(index), index);
        }
    }

    private static void test_InvokeLater() throws Exception {
        final List<Integer> ret = new CopyOnWriteArrayList<Integer>();
        final AtomicBoolean done = new AtomicBoolean(false);
        new EDT<Void, Exception>() {
            @Override
            protected Void runInEDT() throws Exception {
                for (int index = 0; index < 100; index++) {
                    final int num = index;
                    EDT.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            ret.add(num);
                        }
                    });
                    assertTrue(ret.size() == 0);
                }
                EDT.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        synchronized (done) {
                            done.set(true);
                            done.notifyAll();
                        }
                    }
                });
                return null;
            }
        }.waitFor();
        while (done.get() == false) {
            synchronized (done) {
                done.wait();
            }
        }
        for (int index = 0; index < 100; index++) {
            assertEquals(ret.get(index), index);
        }
    }
}
