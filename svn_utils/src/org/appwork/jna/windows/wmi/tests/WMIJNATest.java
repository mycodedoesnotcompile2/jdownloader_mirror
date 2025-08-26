/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.jna.windows.wmi.tests;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.jna.windows.wmi.JNAWMIUtils;
import org.appwork.loggingv3.LogV3;
import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.os.CrossSystem;

/**
 * @author daniel
 * @date Aug 25, 2025
 *
 */
@TestDependency({ "org.appwork.jna.windows.wmi.JNAWMIUtils" })
public class WMIJNATest extends AWTest {
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
        if (!CrossSystem.isWindows()) {
            System.out.println("Windows Only Test");
            return;
        }
        final HashSet<String> expected = new HashSet<String>();
        expected.add(";DisplayName: Windows Defender - on,up_to_date,ms;{\"__NAMESPACE\":\"ROOT\\\\SecurityCenter2\",\"pathToSignedReportingExe\":\"%ProgramFiles%\\\\Windows Defender\\\\MsMpeng.exe\",\"displayName\":\"Windows Defender\",\"pathToSignedProductExe\":\"windowsdefender://\",\"productState\":397568,\"__PROPERTY_COUNT\":6,\"__DYNASTY\":\"AntiVirusProduct\",\"__DERIVATION\":[],\"__SERVER\":\"DESKTOP-7KP0VLD\",\"__CLASS\":\"AntiVirusProduct\",\"__RELPATH\":\"AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__PATH\":\"\\\\\\\\DESKTOP-7KP0VLD\\\\ROOT\\\\SecurityCenter2:AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__GENUS\":2,\"instanceGuid\":\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\",\"__SUPERCLASS\":null,\"timestamp\":\"Tue, 30 Jul 2024 15:06:30 GMT\"}");
        expected.add(";DisplayName: Windows Defender - on,up_to_date,ms;{\"__NAMESPACE\":\"ROOT\\\\SecurityCenter2\",\"pathToSignedReportingExe\":\"%ProgramFiles%\\\\Windows Defender\\\\MsMpeng.exe\",\"displayName\":\"Windows Defender\",\"pathToSignedProductExe\":\"windowsdefender://\",\"productState\":397568,\"__PROPERTY_COUNT\":6,\"__DYNASTY\":\"AntiVirusProduct\",\"__SERVER\":\"DESKTOP-7KP0VLD\",\"__CLASS\":\"AntiVirusProduct\",\"__RELPATH\":\"AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__PATH\":\"\\\\\\\\DESKTOP-7KP0VLD\\\\ROOT\\\\SecurityCenter2:AntiVirusProduct.instanceGuid=\\\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\\\"\",\"__GENUS\":2,\"instanceGuid\":\"{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}\",\"__SUPERCLASS\":null,\"timestamp\":\"Tue, 30 Jul 2024 15:06:30 GMT\"}");
        final AtomicLong lastGC = new AtomicLong();
        final AtomicInteger cc = new AtomicInteger(0);
        final AtomicReference<Throwable> exception = new AtomicReference<Throwable>();
        final AtomicBoolean stopFlag = new AtomicBoolean(false);
        try {
            for (int i = 0; i < 50; i++) {
                final int finalI = i;
                new Thread() {
                    private int counter = 0;

                    @Override
                    public void run() {
                        System.out.println("Start Thread:" + finalI);
                        while (exception.get() == null && !stopFlag.get()) {
                            // LogV3.info("AntiVirusProduct START");
                            try {
                                this.setName(new Date().toString() + " - " + this.counter);
                                String namespace;
                                switch (CrossSystem.getOS()) {
                                case WINDOWS_XP:
                                    namespace = "ROOT\\SecurityCenter";
                                    break;
                                default:
                                    namespace = "ROOT\\SecurityCenter2";
                                    break;
                                }
                                final String query = "SELECT * from AntiVirusProduct";
                                cc.addAndGet(1);
                                final ArrayList<Map<String, Object>> list = JNAWMIUtils.query(namespace, query);
                                final StringBuilder sb = new StringBuilder();
                                if (list.size() > 0) {
                                    for (final Map<String, Object> map : list) {
                                        final String displayName = StringUtils.valueOfOrNull(map.get(JNAWMIUtils.DISPLAY_NAME));
                                        final int state = ((Number) map.get(JNAWMIUtils.PRODUCT_STATE)).intValue();
                                        final String stateString = JNAWMIUtils.decodeProductState(state);
                                        sb.append(";" + "DisplayName: " + displayName + " - " + stateString);
                                        sb.append(";" + Deser.get().toString(map, SC.LOG_SINGLELINE));
                                    }
                                }
                                // Thread.sleep(100);
                                synchronized (lastGC) {
                                    if (Time.now() - lastGC.get() > 1000) {
                                        lastGC.set(Time.now());
                                        System.out.println("GC:" + cc.get());
                                        System.gc();
                                        Thread.sleep(200);
                                    }
                                }
                                this.counter++;
                                // if (!expected.contains(sb.toString())) {
                                // LogV3.info("Unexpected result:\r\n" + sb.toString());
                                // }
                            } catch (final Throwable e) {
                                synchronized (exception) {
                                    exception.set(e);
                                    exception.notify();
                                }
                                LogV3.log(e);
                            } finally {
                                // LogV3.info("AntiVirusProduct END");
                            }
                        }
                    };
                }.start();
            }
            synchronized (exception) {
                exception.wait(60 * 1000l);
            }
            assertEquals(exception.get(), null);
        } finally {
            stopFlag.set(true);
        }
    }
}
