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
package org.appwork.utils;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

/**
 * @author daniel
 * @date 21.04.2017
 *
 */
@Deprecated
public class JVMVersion {
    // https://en.wikipedia.org/wiki/Java_version_history
    // https://www.java.com/releases/matrix/
    // https://endoflife.date/java
    @Deprecated
    public final static long JAVA_1_5 = JavaVersion.JVM_1_5.longID;
    @Deprecated
    public final static long JAVA15   = JavaVersion.JVM_1_5.longID;
    @Deprecated
    public final static long JAVA_1_6 = JavaVersion.JVM_1_6.longID;
    @Deprecated
    public final static long JAVA16   = JavaVersion.JVM_1_6.longID;
    @Deprecated
    public final static long JAVA_1_7 = JavaVersion.JVM_1_7.longID;
    @Deprecated
    public final static long JAVA17   = JavaVersion.JVM_1_7.longID;
    @Deprecated
    public final static long JAVA_1_8 = JavaVersion.JVM_1_8.longID; // LTS Dec 2030
    @Deprecated
    public final static long JAVA18   = JavaVersion.JVM_1_8.longID;
    @Deprecated
    public final static long JAVA_9   = JavaVersion.JVM_9_0.longID;
    @Deprecated
    public final static long JAVA19   = JavaVersion.JVM_9_0.longID;
    @Deprecated
    public final static long JAVA_10  = JavaVersion.JVM_10_0.longID;
    @Deprecated
    public final static long JAVA_11  = JavaVersion.JVM_11_0.longID; // LTS Sep 2026
    @Deprecated
    public final static long JAVA_12  = JavaVersion.JVM_12_0.longID; // March 2019
    @Deprecated
    public final static long JAVA_13  = JavaVersion.JVM_13_0.longID; // September 2019
    @Deprecated
    public final static long JAVA_14  = JavaVersion.JVM_14_0.longID; // March 2020
    @Deprecated
    public final static long JAVA_15  = JavaVersion.JVM_15_0.longID; // September 2020
    @Deprecated
    public final static long JAVA_16  = JavaVersion.JVM_16_0.longID; // March 2021
    @Deprecated
    public final static long JAVA_17  = JavaVersion.JVM_17_0.longID; // September 2021, LTS Sep 2029
    @Deprecated
    public final static long JAVA_18  = JavaVersion.JVM_18_0.longID; // March 2022, EA
    @Deprecated
    public final static long JAVA_19  = JavaVersion.JVM_19_0.longID; // September 2022, EA
    @Deprecated
    public final static long JAVA_20  = JavaVersion.JVM_20_0.longID; // March 2023
    @Deprecated
    public final static long JAVA_21  = JavaVersion.JVM_21_0.longID; // September 2023, LTS Sep 2031
    @Deprecated
    public final static long JAVA_22  = JavaVersion.JVM_22_0.longID; // March 2024

    @Deprecated
    /**
     *
     * @deprecated Use JVMVersion.getVersion()
     */
    public static final long get() {
        return getVersion().getLongID();
    }

    @Deprecated
    public static final JavaVersionInterface getVersion() {
        return JavaVersion.getVersion();
    }

    @Deprecated
    /**
     *
     * @deprecated Use JVMVersion.getVersion().isMinimum()
     */
    public static final boolean isMinimum(final long version) {
        return getVersion().getLongID() >= version;
    }

    @Deprecated
    public static String getJVMVersion() {
        return JavaVersion.getJVMVersion();
    }

    /**
     * https://en.wikipedia.org/wiki/Java_class_file#General_layout
     *
     * @param url
     * @return
     * @throws IOException
     */
    public static JavaVersion readClassJVMVersion(final URL url) throws IOException {
        final InputStream is = url.openStream();
        try {
            return readClassJVMVersion(is);
        } finally {
            is.close();
        }
    }

    @Deprecated
    public static JavaVersion readClassJVMVersion(final InputStream is) throws IOException {
        return JavaVersion.readClassJVMVersion(is);
    }

    @Deprecated
    public static long parseJavaVersionString(String version) {
        return JavaVersion.parseJavaVersionString(version);
    }

    @Deprecated
    public static JavaVersionInterface toJavaVersion(final String versionString) {
        return JavaVersion.toJavaVersion(versionString);
    }

    /**
     * @param javaVersion
     * @return
     */
    @Deprecated
    /**
     *
     * @deprecated Use JVMVersion.getVersion().is()
     */
    public static boolean is(JavaVersion version) {
        return version != null && version.is(get());
    }

    /**
     * @param jvm6
     * @return
     */
    @Deprecated
    /**
     *
     * @deprecated Use JVMVersion.getVersion().isMinimum()
     */
    public static boolean isAtLeast(JavaVersion version) {
        return version != null && get() > version.getLongID();
    }
}
