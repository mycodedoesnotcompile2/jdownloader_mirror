/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.os.hardware;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;

/**
 * @author daniel
 * @date Sep 21, 2026
 *
 */
public class Apple {
    public static enum SystemOnChip {
        INTEL,
        M1,
        M1_PRO,
        M1_MAX,
        M1_ULTRA,
        M2,
        M2_PRO,
        M2_MAX,
        M2_ULTRA,
        M3,
        M3_PRO,
        M3_MAX,
        M3_ULTRA,
        M4,
        M4_PRO,
        M4_MAX,
        M5,
        M5_PRO,
        M5_MAX;
        public static SystemOnChip parse(final String brand_string) {
            SystemOnChip best = null;
            for (SystemOnChip soc : values()) {
                final String name = soc.name().replace("_", " ");
                if (!StringUtils.containsIgnoreCase(brand_string, name)) {
                    continue;
                }
                if (best == null || soc.name().length() > best.name().length()) {
                    best = soc;
                }
            }
            return best;
        }
    }

    private static SystemOnChip SoC = null;

    public static SystemOnChip getSoC() throws InterruptedException, Exception {
        final SystemOnChip ret = Apple.SoC;
        if (ret != null) {
            return ret;
        }
        if (!CrossSystem.isMac()) {
            return null;
        }
        if (!isAppleSilicon()) {
            return Apple.SoC = SystemOnChip.INTEL;
        }
        final String brand_string = sysctrl_query("machdep.cpu.brand_string", 5000);
        return Apple.SoC = SystemOnChip.parse(brand_string);
    }

    private static String sysctrl_query(final String oid, final int timeout) throws InterruptedException, Exception {
        final Object DUMMY = new Object();
        final AtomicReference<Object> ret = new AtomicReference<Object>(DUMMY);
        Thread thread = new Thread("sysctrl_query:" + oid) {
            @Override
            public void run() {
                try {
                    final Process process = new ProcessBuilder("sysctl", "-n", oid).start();
                    final InputStreamReader is = new InputStreamReader(process.getInputStream());
                    try {
                        final BufferedReader reader = new BufferedReader(is);
                        try {
                            final String line = reader.readLine();
                            synchronized (ret) {
                                ret.compareAndSet(DUMMY, line == null ? null : line.trim());
                                ret.notify();
                            }
                        } finally {
                            reader.close();
                        }
                    } finally {
                        is.close();
                    }
                } catch (Exception e) {
                    synchronized (ret) {
                        ret.compareAndSet(DUMMY, e);
                        ret.notify();
                    }
                } finally {
                    synchronized (ret) {
                        ret.compareAndSet(DUMMY, null);
                        ret.notify();
                    }
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
        synchronized (ret) {
            if (ret.get() == DUMMY && timeout >= 0) {
                ret.wait(timeout);
            }
            final Object result = ret.get();
            if (result == DUMMY) {
                return null;
            } else if (result instanceof String) {
                return (String) result;
            } else if (result instanceof Exception) {
                throw (Exception) result;
            } else {
                return null;
            }
        }
    }

    private static Boolean IS_ROSETTA = null;

    public static Boolean isRunningUnderRosetta() throws InterruptedException, Exception {
        final Boolean IS_ROSETTA = Apple.IS_ROSETTA;
        if (IS_ROSETTA != null) {
            return IS_ROSETTA;
        }
        if (!CrossSystem.isMac()) {
            return Apple.IS_ROSETTA = false;
        }
        final String proc_translated = sysctrl_query("sysctl.proc_translated", 5000);
        if ("1".equals(proc_translated)) {
            return Apple.IS_ROSETTA = true;
        } else if ("0".equals(proc_translated)) {
            return Apple.IS_ROSETTA = false;
        } else {
            return null;
        }
    }

    private static Boolean IS_APPLE_SILICON = null;

    public static Boolean isAppleSilicon() throws InterruptedException, Exception {
        final Boolean IS_APPLE_SILICON = Apple.IS_APPLE_SILICON;
        if (IS_APPLE_SILICON != null) {
            return IS_APPLE_SILICON;
        }
        if (!CrossSystem.isMac()) {
            return Apple.IS_APPLE_SILICON = false;
        }
        final String proc_translated = sysctrl_query("sysctl.proc_translated", 5000);
        if ("1".equals(proc_translated) || "0".equals(proc_translated)) {
            return Apple.IS_APPLE_SILICON = true;
        }
        final String brand_string = sysctrl_query("machdep.cpu.brand_string", 5000);
        if (StringUtils.containsIgnoreCase(brand_string, "Apple")) {
            return Apple.IS_APPLE_SILICON = true;
        } else if (StringUtils.containsIgnoreCase(brand_string, "Intel")) {
            return Apple.IS_APPLE_SILICON = false;
        } else {
            return null;
        }
    }
}
