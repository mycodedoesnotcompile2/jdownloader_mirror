package org.appwork.utils;

import org.appwork.utils.JVMVersion.JavaVersionInterface;

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
    JVM_25_0(69, "25", true/* planned */) {
        private final long next;
        {
            this.next = JVMVersion.parseJavaVersionString("26");
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
        return this.is(JVMVersion.parseJavaVersionString(versionString));
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
        this.longID = JVMVersion.parseJavaVersionString(stringID);
        this.classID = classMajorVersion;
        this.string = stringID;
        this.lts = lts;
    }

    public boolean isHigherThan(final JavaVersionInterface v) {
        if (getBase() == UNKNOWN) {
            return false;
        } else {
            return v != null && v != UNKNOWN && this.getLongID() != -1 && v.getLongID() != -1 && getLongID() > v.getLongID();
        }
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
