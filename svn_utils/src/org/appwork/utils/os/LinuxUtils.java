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
package org.appwork.utils.os;

import java.io.File;

import org.appwork.JNAHelper;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.ReflectionUtils;

/**
 * @author daniel
 * @date May 27, 2025
 *
 */
public class LinuxUtils {
    public static boolean isRoot() {
        try {
            final Class<?> unixSystem = Class.forName("com.sun.security.auth.module.UnixSystem");
            final Object instance = unixSystem.getConstructor().newInstance();
            return ReflectionUtils.invoke(unixSystem, "getUid", instance, long.class) == 0;
        } catch (Throwable e) {
        }
        try {
            if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
                final File procSelf = new File("/proc/self");
                if (procSelf.isDirectory()) {
                    return ((Integer) java.nio.file.Files.getAttribute(procSelf.toPath(), "unix:uid")).intValue() == 0;
                }
            }
        } catch (UnsupportedOperationException e) {
            // if the attribute view is not available
        } catch (IllegalArgumentException e) {
            // if the attribute name is not specified or is not recognized
        } catch (Throwable e) {
        }
        try {
            if (JNAHelper.isJNAAvailable()) {
                final Class<?> libc = Class.forName("com.sun.jna.platform.unix.LibC");
                final Class<?> libcApi = Class.forName("com.sun.jna.platform.unix.LibCAPI");
                final Object instance = ReflectionUtils.getFieldValue(libc, "INSTANCE", libc, libc);
                return ReflectionUtils.invoke(libcApi, "getuid", instance, int.class) == 0;
            }
        } catch (Throwable e) {
        }
        return false;
    }
}
