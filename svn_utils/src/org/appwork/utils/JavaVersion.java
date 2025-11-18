package org.appwork.utils;

import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

public enum JavaVersion implements JavaVersionInterface {
    // https://en.wikipedia.org/wiki/Java_version_history
    // UNKNOWN IS ALWAYS FIRST!
    UNKNOWN(-1, "Unknown"),
    // ORDER IS IMPORTANT! ALWAYS ORDER ENUMS ACCORDING TO THEIR VERSION!
    JVM_1_0(45, "1.0"),
    JVM_1_1(45, "1.1"),
    JVM_1_2(46, "1.2"),
    JVM_1_3(47, "1.3"),
    JVM_1_4(48, "1.4"),
    JVM_1_5(49, "1.5"),
    JVM_1_6(50, "1.6"),
    JVM_1_7(51, "1.7"),
    JVM_1_8(52, "1.8", true),
    JVM_9_0(53, "9"),
    JVM_10_0(54, "10"),
    JVM_11_0(55, "11", true),
    JVM_12_0(56, "12"),
    JVM_13_0(57, "13"),
    JVM_14_0(58, "14"),
    JVM_15_0(59, "15"),
    JVM_16_0(60, "16"),
    JVM_17_0(61, "17", true),
    JVM_18_0(62, "18"),
    JVM_19_0(63, "19"),
    JVM_20_0(64, "20"),
    JVM_21_0(65, "21", true),
    JVM_22_0(66, "22"),
    JVM_23_0(67, "23"),
    JVM_24_0(68, "24"),
    JVM_25_0(69, "25", true),
    JVM_26_0(70, "26"),
    JVM_27_0(71, "27"/* planned */),
    JVM_28_0(72, "28"/* planned */),
    JVM_29_0(73, "29", true/* planned */) {
        private final long next;
        {
            // "next" 30 comes after 29
            this.next = parseJavaVersionString("30");
        }

        @Override
        public long nextVersionsLongID() {
            return this.next;
        }
    };
    public final long    longID;
    public final int     classID;
    public final String  string;
    public final boolean lts;

    /**
     * @param string2
     * @return
     */
    public boolean is(final String versionString) {
        return this.is(parseJavaVersionString(versionString));
    }

    public boolean is(final long id) {
        if (id >= this.longID) {
            final long next = this.nextVersionsLongID();
            if (id < next) {
                return true;
            }
        }
        return false;
    }

    public boolean is(JavaVersionInterface version) {
        return getBase() == version.getBase() || is(version.getLongID());
    }

    public long nextVersionsLongID() {
        final int index = this.ordinal() + 1;
        if (index < values().length) {
            return values()[index].longID;
        } else {
            return -1;
        }
    }

    private JavaVersion(final int classMajorVersion, final String stringID) {
        this(classMajorVersion, stringID, false);
    }

    private JavaVersion(final int classMajorVersion, final String stringID, final boolean lts) {
        this.longID = parseJavaVersionString(stringID);
        this.classID = classMajorVersion;
        this.string = stringID;
        this.lts = lts;
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

    public boolean isHigherThan(final JavaVersionInterface v) {
        if (getBase() == UNKNOWN) {
            return false;
        } else {
            return v != null && v != UNKNOWN && this.getLongID() != -1 && v.getLongID() != -1 && getLongID() > v.getLongID();
        }
    }

    private static final JavaVersionInterface VERSION;
    static {
        JavaVersionInterface version = JavaVersion.UNKNOWN;
        try {
            final String versionString = getJVMVersion();
            version = toJavaVersion(versionString);
        } catch (final Exception e) {
            DebugMode.debugger(e);
        }
        VERSION = version;
    }

    private static int getClassID(JavaVersion base) {
        final String version = System.getProperty("java.class.version");
        if (version != null) {
            try {
                final int ret = (int) Double.parseDouble(version);
                if (base.getClassID() != -1 && ret != base.getClassID()) {
                    DebugMode.debugger();
                }
                return ret;
            } catch (NumberFormatException ignore) {
                DebugMode.debugger(ignore);
            }
        }
        return base.getClassID();
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
            private final int classID = VERSION == null ? JavaVersion.getClassID(finalBase) : finalBase.getClassID();

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
                return classID;
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

    public static String getJVMVersion() {
        /* this version info contains more information */
        final String version = System.getProperty("java.runtime.version");
        if (version == null || version.trim().length() == 0) {
            return System.getProperty("java.version");
        } else {
            return version;
        }
    }

    public static JavaVersionInterface getVersion() {
        return VERSION;
    }

    public boolean isMinimum(final JavaVersionInterface v) {
        if (getBase() == UNKNOWN) {
            return false;
        } else {
            return v != null && v != UNKNOWN && this.getLongID() != -1 && v.getLongID() != -1 && getLongID() >= v.getLongID();
        }
    }

    public boolean isMaximum(final JavaVersionInterface v) {
        if (getBase() == UNKNOWN) {
            return false;
        } else {
            return v != null && v != UNKNOWN && this.getLongID() != -1 && v.getLongID() != -1 && getLongID() <= v.getLongID();
        }
    }

    public boolean isLowerThan(final JavaVersionInterface v) {
        if (getBase() == UNKNOWN) {
            return false;
        } else {
            return v != null && v != UNKNOWN && this.getLongID() != -1 && v.getLongID() != -1 && getLongID() < v.getLongID();
        }
    }

    @Override
    public long getLongID() {
        return longID;
    }

    @Override
    public JavaVersion getBase() {
        return this;
    }

    @Override
    public int getClassID() {
        return classID;
    }

    @Override
    public boolean isLTS() {
        return lts;
    }

    @Override
    public String getVersionString() {
        return string;
    }

    @Override
    public String toString() {
        return "Base:" + name() + (isLTS() ? "(LTS)" : "") + "|Version:" + getLongID() + "(" + getVersionString() + ")|ClassID:" + getClassID();
    }
}
