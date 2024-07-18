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
package org.appwork.utils;

import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

/**
 * @author daniel
 * @date 21.04.2017
 *
 */
public class JVMVersion {
    public interface JavaVersionInterface {
        public boolean isHigherThan(final JavaVersionInterface v);

        public boolean isMinimum(final JavaVersionInterface v);

        public boolean isMaximum(final JavaVersionInterface v);

        public boolean isLowerThan(final JavaVersionInterface v);

        public boolean is(JavaVersionInterface version);

        public long getLongID();

        public int getClassID();

        public String getVersionString();

        public boolean isLTS();

        public JavaVersion getBase();
    }

    // https://en.wikipedia.org/wiki/Java_version_history
    // https://www.java.com/releases/matrix/
    // https://endoflife.date/java
    @Deprecated
    public final static long                  JAVA_1_5 = JavaVersion.JVM_1_5.longID;
    @Deprecated
    public final static long                  JAVA15   = JavaVersion.JVM_1_5.longID;
    @Deprecated
    public final static long                  JAVA_1_6 = JavaVersion.JVM_1_6.longID;
    @Deprecated
    public final static long                  JAVA16   = JavaVersion.JVM_1_6.longID;
    @Deprecated
    public final static long                  JAVA_1_7 = JavaVersion.JVM_1_7.longID;
    @Deprecated
    public final static long                  JAVA17   = JavaVersion.JVM_1_7.longID;
    @Deprecated
    public final static long                  JAVA_1_8 = JavaVersion.JVM_1_8.longID;  // LTS Dec 2030
    @Deprecated
    public final static long                  JAVA18   = JavaVersion.JVM_1_8.longID;
    @Deprecated
    public final static long                  JAVA_9   = JavaVersion.JVM_9_0.longID;
    @Deprecated
    public final static long                  JAVA19   = JavaVersion.JVM_9_0.longID;
    @Deprecated
    public final static long                  JAVA_10  = JavaVersion.JVM_10_0.longID;
    @Deprecated
    public final static long                  JAVA_11  = JavaVersion.JVM_11_0.longID; // LTS Sep 2026
    @Deprecated
    public final static long                  JAVA_12  = JavaVersion.JVM_12_0.longID; // March 2019
    @Deprecated
    public final static long                  JAVA_13  = JavaVersion.JVM_13_0.longID; // September 2019
    @Deprecated
    public final static long                  JAVA_14  = JavaVersion.JVM_14_0.longID; // March 2020
    @Deprecated
    public final static long                  JAVA_15  = JavaVersion.JVM_15_0.longID; // September 2020
    @Deprecated
    public final static long                  JAVA_16  = JavaVersion.JVM_16_0.longID; // March 2021
    @Deprecated
    public final static long                  JAVA_17  = JavaVersion.JVM_17_0.longID; // September 2021, LTS Sep 2029
    @Deprecated
    public final static long                  JAVA_18  = JavaVersion.JVM_18_0.longID; // March 2022, EA
    @Deprecated
    public final static long                  JAVA_19  = JavaVersion.JVM_19_0.longID; // September 2022, EA
    @Deprecated
    public final static long                  JAVA_20  = JavaVersion.JVM_20_0.longID; // March 2023
    @Deprecated
    public final static long                  JAVA_21  = JavaVersion.JVM_21_0.longID; // September 2023, LTS Sep 2031
    @Deprecated
    public final static long                  JAVA_22  = JavaVersion.JVM_22_0.longID; // March 2024
    private static final JavaVersionInterface VERSION;
    static {
        JavaVersionInterface version = JavaVersion.UNKNOWN;
        try {
            final String versionString = getJVMVersion();
            version = toJavaVersion(versionString);
        } catch (final Throwable ignore) {
            ignore.printStackTrace();
        }
        VERSION = version;
    }

    @Deprecated
    /**
     *
     * @deprecated Use JVMVersion.getVersion()
     */
    public static final long get() {
        return getVersion().getLongID();
    }

    public static final JavaVersionInterface getVersion() {
        return VERSION;
    }

    @Deprecated
    /**
     *
     * @deprecated Use JVMVersion.getVersion().isMinimum()
     */
    public static final boolean isMinimum(final long version) {
        return getVersion().getLongID() >= version;
    }

    public static String getJVMVersion() {
        /* this version info contains more information */
        final String version = System.getProperty("java.runtime.version");
        if (version == null || version.trim().length() == 0) {
            return System.getProperty("java.version");
        } else {
            return version;
        }
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

    public static JavaVersion readClassJVMVersion(final InputStream is) throws IOException {
        try {
            final DataInputStream dis = new DataInputStream(is);
            final byte[] magic = new byte[4];
            dis.readFully(magic);
            if (Arrays.equals(magic, new byte[] { (byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE })) {
                final byte[] minor = new byte[2];
                dis.readFully(minor);
                final byte[] major = new byte[2];
                final ByteBuffer byteBuffer = ByteBuffer.allocateDirect(4);
                byteBuffer.order(ByteOrder.BIG_ENDIAN);
                byteBuffer.put(major);
                dis.readFully(major);
                byteBuffer.put(major);
                JDK8BufferHelper.flip(byteBuffer);
                final int majorVersion = byteBuffer.getInt();
                for (final JavaVersion e : JavaVersion.values()) {
                    if (e.classID == majorVersion) {
                        return e;
                    }
                }
            }
            return JavaVersion.UNKNOWN;
        } catch (final EOFException e) {
            return JavaVersion.UNKNOWN;
        } catch (final IOException e) {
            return JavaVersion.UNKNOWN;
        }
    }

    public static long parseJavaVersionString(String version) {
        if (version != null) {
            // remove trailing LTS
            version = version.replaceFirst("\\s*-\\s*LTS$", "");
        }
        final String majorFeature = new Regex(version, "^(\\d+)").getMatch(0);
        final String minorInterim = new Regex(version, "^\\d+\\.(\\d+)").getMatch(0);
        final String securityUpdate = new Regex(version, "^\\d+\\.\\d+\\.(\\d+)").getMatch(0);
        // final String patch = new Regex(version, "^\\d+\\.\\d+\\.\\d+\\.(\\d+)").getMatch(0);
        long ret = 0;
        final String u;
        final String b;
        if ("1".equals(majorFeature) && minorInterim != null) {
            // java 1.5 - java 1.8
            ret = Long.parseLong(minorInterim) * 1000 * 1000 + 10000000;
            u = new Regex(version, "^.*?_(\\d+)").getMatch(0);
            b = new Regex(version, "^.*?(_|-)b(\\d+)$").getMatch(1);
            // ignore securityUpdate
        } else if (majorFeature != null) {
            // java 1.9, java 10, java 11...
            final long major = Long.parseLong(majorFeature);
            u = new Regex(version, "u(\\d+)").getMatch(0);
            b = new Regex(version, "\\+(\\d+)$").getMatch(0);
            // if (major < 5) {
            // // fallback to Java 1.5
            // return JAVA_1_5;
            // } else
            //
            if (major < 9) {
                // < java 9
                ret = major * 1000 * 1000 + 10000000;
            } else {
                // >= java 9
                ret = major * 1000 * 1000 * 1000 * 1000l;
                if (minorInterim != null) {
                    ret += Math.min(999, Long.parseLong(minorInterim)) * 1000 * 1000 * 1000l;
                }
                if (securityUpdate != null) {
                    ret += Math.min(999, Long.parseLong(securityUpdate)) * 1000 * 1000;
                }
            }
        } else {
            return -1;
        }
        if (u != null) {
            /* append update number */
            ret += Math.min(999, Long.parseLong(u)) * 1000;
        }
        if (b != null) {
            /* append build number */
            ret += Math.min(999, Long.parseLong(b));
        }
        return ret;
    }

    public static JavaVersionInterface toJavaVersion(final String versionString) {
        final long longID = parseJavaVersionString(versionString);
        final boolean isLTS = versionString != null && versionString.contains("LTS");
        JavaVersion base = JavaVersion.UNKNOWN;
        for (JavaVersion v : JavaVersion.values()) {
            if (v.is(longID)) {
                base = v;
                break;
            }
        }
        final JavaVersion finalBase = base;
        return new JavaVersionInterface() {
            public boolean is(JavaVersionInterface version) {
                return getBase().is(version);
            }

            public boolean isHigherThan(final JavaVersionInterface v) {
                return v != null && v != JavaVersion.UNKNOWN && getLongID() != -1 && v.getLongID() != -1 && getLongID() > v.getLongID();
            }

            public boolean isMinimum(final JavaVersionInterface v) {
                return v != null && v != JavaVersion.UNKNOWN && getLongID() != -1 && v.getLongID() != -1 && getLongID() >= v.getLongID();
            }

            public boolean isMaximum(final JavaVersionInterface v) {
                return v != null && v != JavaVersion.UNKNOWN && getLongID() != -1 && v.getLongID() != -1 && getLongID() <= v.getLongID();
            }

            public boolean isLowerThan(final JavaVersionInterface v) {
                return v != null && v != JavaVersion.UNKNOWN && getLongID() != -1 && v.getLongID() != -1 && getLongID() < v.getLongID();
            }

            @Override
            public long getLongID() {
                return longID;
            }

            @Override
            public JavaVersion getBase() {
                return finalBase;
            }

            @Override
            public int getClassID() {
                return getBase().getClassID();
            }

            @Override
            public boolean isLTS() {
                return isLTS || getBase().isLTS();
            }

            @Override
            public String getVersionString() {
                return versionString;
            }

            @Override
            public String toString() {
                return "Base:" + getBase().name() + (isLTS() ? "(LTS)" : "") + "|Version:" + getLongID() + "(" + getVersionString() + ")|ClassID:" + getClassID();
            }
        };
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
