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
package org.appwork.utils.os.hardware;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;

/**
 * @author daniel
 * @date Sep 10, 2020
 *
 */
public abstract class Synology implements HardwareTypeInterface {

    @Override
    public ID getHardwareType() {
        return ID.SYNOLOGY;
    }

    public static Synology getSynologyDetails() {
        if (CrossSystem.isLinux()) {
            final File cpuInfo = new File("/proc/sys/kernel/syno_hw_version");
            if (cpuInfo.isFile()) {
                FileInputStream fis = null;
                try {
                    fis = new FileInputStream(cpuInfo);
                    final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                    String line = null;
                    while ((line = is.readLine()) != null) {
                        if (StringUtils.isNotEmpty(line)) {
                            break;
                        }
                    }
                    is.close();
                    if (StringUtils.isNotEmpty(line)) {
                        return parse(line);
                    }
                } catch (final Throwable ignore) {
                    ignore.printStackTrace();
                } finally {
                    if (fis != null) {
                        try {
                            fis.close();
                        } catch (final Throwable ignore) {
                        }
                    }
                }
            }
        }
        return null;
    }

    private static Synology parse(final String hardware) {
        return new Synology() {

            @Override
            public String getModel() {
                return hardware;
            }

        };
    }

    public String toString() {
        return "Synology|Model:" + getModel();
    }

    private Synology() {
    }

    public abstract String getModel();

}
