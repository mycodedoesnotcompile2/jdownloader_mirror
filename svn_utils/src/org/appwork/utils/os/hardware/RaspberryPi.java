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

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.ARCHFamily;

/**
 * @author daniel
 * @date Sep 7, 2020
 *
 */
public abstract class RaspberryPi implements HardwareTypeInterface {
    public static enum MANUFACTURER {
        QISDA,
        SONY_UK,
        EGOMAN,
        EMBEST,
        SONY_JAPAN,
        STADIUM,
        UNKNOWN
    }

    public static enum PROCESSOR {
        BCM2835,
        BCM2836,
        BCM2837,
        BCM2711,
        BCM2712,
        UNKNOWN
    }

    public static enum MEMORY_SIZE {
        MB_256,
        MB_512,
        GB_1,
        GB_2,
        GB_4,
        GB_8,
        UNKNOWN
    }

    public static enum TYPE {
        A,
        B,
        A_PLUS("A+"),
        B_PLUS("B+"),
        TWO_B("2B"),
        ALPHA,
        CM1,
        THREE_B("3B"),
        ZERO,
        CM3,
        ZERO_W("ZeroW"),
        ZERO_2_W("Zero2W"),
        THREE_B_PLUS("3B+"),
        THREE_A_PLUS("3A+"),
        INTERNAL_USE_ONLY,
        CM3_PLUS("CM3+"),
        FOUR_B("4B"),
        FOUR_HUNDRET("400"),
        FIVE_B("5B"),
        CM4,
        CM4_S,
        UNKNOWN;
        private final String name;

        private TYPE() {
            this(null);
        };

        private TYPE(String name) {
            this.name = name;
        }

        public String toString() {
            if (name == null) {
                return name();
            } else {
                return name;
            }
        }
    }

    public static RaspberryPi getRaspberryPiDetails() {
        if (CrossSystem.isUnix() && ARCHFamily.ARM.equals(CrossSystem.getARCHFamily())) {
            final File cpuInfo = new File("/proc/cpuinfo");
            if (cpuInfo.isFile()) {
                try {
                    final FileInputStream fis = new FileInputStream(cpuInfo);
                    try {
                        String raspberryPIModel = null;
                        String raspberryPIHardware = null;
                        String revision = null;
                        final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                        try {
                            while (true) {
                                final String line = is.readLine();
                                if (line == null) {
                                    break;
                                } else if (StringUtils.startsWithCaseInsensitive(line, "Model")) {
                                    raspberryPIModel = new Regex(line, "(?i)^\\s*Model\\s*:\\s*(Raspberry.*?)(\r|\n|$)").getMatch(0);
                                } else if (StringUtils.startsWithCaseInsensitive(line, "Hardware")) {
                                    raspberryPIHardware = new Regex(line, "(?i)^\\s*Hardware\\s*:\\s*(BCM[0-9a-zA-Z]+)").getMatch(0);
                                } else if (StringUtils.startsWithCaseInsensitive(line, "Revision")) {
                                    revision = new Regex(line, "(?i)^\\s*Revision\\s*:\\s*([0-9a-fA-F]+)").getMatch(0);
                                }
                                if (raspberryPIModel != null && revision != null) {
                                    break;
                                }
                            }
                        } finally {
                            is.close();
                        }
                        if ((raspberryPIModel != null || raspberryPIHardware != null) && revision != null) {
                            return RaspberryPi.parse(revision);
                        }
                    } finally {
                        fis.close();
                    }
                } catch (final Throwable ignore) {
                    ignore.printStackTrace();
                }
            }
        }
        return null;
    }

    private RaspberryPi() {
    }

    public static void main(String[] args) {
        System.out.println(getRaspberryPiDetails());
    }

    /**
     * https://www.raspberrypi.org/documentation/hardware/raspberrypi/revision-codes/README.md
     *
     * https://github.com/AndrewFromMelbourne/raspberry_pi_revision/blob/master/raspberry_pi_revision.c
     */
    private static RaspberryPi parse(final String revision) {
        if (revision != null && revision.matches("^[a-fA-F0-9]+$")) {
            final boolean oldStyleRevision;
            final Long revisionLong;
            if (revision.matches("^10000[0-9a-fA-F]{2}$")) {
                // old sequential hex revision codes, over-volted
                revisionLong = Long.parseLong(revision.substring(3), 16);
                oldStyleRevision = true;
            } else if (revision.matches("^00[0-9a-fA-F]{2}$")) {
                // old sequential hex revision codes
                oldStyleRevision = true;
                revisionLong = Long.parseLong(revision, 16);
                if (!(revisionLong >= 2 && revisionLong <= 21)) {
                    return null;
                }
            } else if (revision.matches("^[0-9a-fA-F]{6,}$")) {
                // new style revision
                oldStyleRevision = false;
                revisionLong = Long.parseLong(revision, 16);
            } else {
                return null;
            }
            return new RaspberryPi() {
                @Override
                public String getRaw() {
                    return revision;
                }

                @Override
                public boolean isNewStyleRevision() {
                    if (oldStyleRevision) {
                        return false;
                    } else {
                        final int value = (int) ((revisionLong >>> 23) & 1);
                        return value == 1;
                    }
                }

                @Override
                public MANUFACTURER getManufaturer() {
                    if (revisionLong >= 2 && revisionLong <= 21) {
                        switch (revisionLong.intValue()) {
                        case 2:
                        case 3:
                            return MANUFACTURER.EGOMAN;
                        case 4:
                            return MANUFACTURER.SONY_UK;
                        case 5:
                            return MANUFACTURER.QISDA;
                        case 6:
                        case 7:
                            return MANUFACTURER.EGOMAN;
                        case 8:
                            return MANUFACTURER.SONY_UK;
                        case 9:
                            return MANUFACTURER.QISDA;
                        case 13:
                            return MANUFACTURER.EGOMAN;
                        case 14:
                            return MANUFACTURER.SONY_UK;
                        case 15:
                            return MANUFACTURER.EGOMAN;
                        case 16:
                        case 17:
                        case 18:
                            return MANUFACTURER.SONY_UK;
                        case 19:
                        case 20:
                        case 21:
                            return MANUFACTURER.EMBEST;
                        default:
                            return MANUFACTURER.UNKNOWN;
                        }
                    } else {
                        final int value = (int) ((revisionLong >>> 16) & 15);
                        switch (value) {
                        case 0:
                            return MANUFACTURER.SONY_UK;
                        case 1:
                            return MANUFACTURER.EGOMAN;
                        case 2:
                            return MANUFACTURER.EMBEST;
                        case 3:
                            return MANUFACTURER.SONY_JAPAN;
                        case 4:
                            return MANUFACTURER.EMBEST;
                        case 5:
                            return MANUFACTURER.STADIUM;
                        default:
                            return MANUFACTURER.UNKNOWN;
                        }
                    }
                }

                @Override
                public PROCESSOR getProcessor() {
                    if (revisionLong >= 2 && revisionLong <= 21) {
                        return PROCESSOR.UNKNOWN;
                    } else {
                        final int value = (int) (revisionLong >>> 12) & 15;
                        switch (value) {
                        case 0:
                            return PROCESSOR.BCM2835;
                        case 1:
                            return PROCESSOR.BCM2836;
                        case 2:
                            return PROCESSOR.BCM2837;
                        case 3:
                            return PROCESSOR.BCM2711;
                        case 4:
                            return PROCESSOR.BCM2712;
                        default:
                            return PROCESSOR.UNKNOWN;
                        }
                    }
                }

                @Override
                public MEMORY_SIZE getMemorySize() {
                    if (revisionLong >= 2 && revisionLong <= 21) {
                        switch (revisionLong.intValue()) {
                        case 2:
                        case 3:
                        case 4:
                        case 5:
                        case 6:
                        case 7:
                        case 8:
                        case 9:
                            return MEMORY_SIZE.MB_256;
                        case 13:
                        case 14:
                        case 15:
                        case 16:
                        case 17:
                            return MEMORY_SIZE.MB_512;
                        case 18:
                            return MEMORY_SIZE.MB_256;
                        case 19:
                        case 20:
                            return MEMORY_SIZE.MB_512;
                        case 21:
                            return MEMORY_SIZE.MB_512;// also 256
                        default:
                            return MEMORY_SIZE.UNKNOWN;
                        }
                    } else {
                        final int value = (int) (revisionLong >>> 20) & 7;
                        switch (value) {
                        case 0:
                            return MEMORY_SIZE.MB_256;
                        case 1:
                            return MEMORY_SIZE.MB_512;
                        case 2:
                            return MEMORY_SIZE.GB_1;
                        case 3:
                            return MEMORY_SIZE.GB_2;
                        case 4:
                            return MEMORY_SIZE.GB_4;
                        case 5:
                            return MEMORY_SIZE.GB_8;
                        default:
                            return MEMORY_SIZE.UNKNOWN;
                        }
                    }
                }

                @Override
                public TYPE getType() {
                    if (revisionLong >= 2 && revisionLong <= 21) {
                        switch (revisionLong.intValue()) {
                        case 2:
                        case 3:
                        case 4:
                        case 5:
                        case 6:
                            return TYPE.B;
                        case 7:
                        case 8:
                        case 9:
                            return TYPE.A;
                        case 13:
                        case 14:
                        case 15:
                            return TYPE.B;
                        case 16:
                            return TYPE.B_PLUS;
                        case 17:
                            return TYPE.CM1;
                        case 18:
                            return TYPE.A_PLUS;
                        case 19:
                            return TYPE.B_PLUS;
                        case 20:
                            return TYPE.CM1;
                        case 21:
                            return TYPE.A_PLUS;
                        default:
                            return TYPE.UNKNOWN;
                        }
                    } else {
                        final int value = (int) (revisionLong >>> 4) & 255;
                        switch (value) {
                        case 0:
                            return TYPE.A;
                        case 1:
                            return TYPE.B;
                        case 2:
                            return TYPE.A_PLUS;
                        case 3:
                            return TYPE.B_PLUS;
                        case 4:
                            return TYPE.TWO_B;
                        case 5:
                            return TYPE.ALPHA;
                        case 6:
                            return TYPE.CM1;
                        case 7:
                            return TYPE.UNKNOWN;
                        case 8:
                            return TYPE.THREE_B;
                        case 9:
                            return TYPE.ZERO;
                        case 0x0a:
                            return TYPE.CM3;
                        case 0x0b:
                            return TYPE.UNKNOWN;
                        case 0x0c:
                            return TYPE.ZERO_W;
                        case 0x0d:
                            return TYPE.THREE_B_PLUS;
                        case 0x0e:
                            return TYPE.THREE_A_PLUS;
                        case 0x0f:
                            return TYPE.INTERNAL_USE_ONLY;
                        case 0x10:
                            return TYPE.CM3_PLUS;
                        case 0x11:
                            return TYPE.FOUR_B;
                        case 0x12:
                            return TYPE.ZERO_2_W;
                        case 0x13:
                            return TYPE.FOUR_HUNDRET;
                        case 0x14:
                            return TYPE.CM4;
                        case 0x15:
                            return TYPE.CM4_S;
                        case 0x16:
                            return TYPE.UNKNOWN;
                        case 0x17:
                            return TYPE.FIVE_B;
                        default:
                            return TYPE.UNKNOWN;
                        }
                    }
                }
            };
        } else {
            return null;
        }
    }

    public String toString() {
        return "RaspberryPI|Type:" + getType() + "|MemorySize:" + getMemorySize() + "|Manufacturer:" + getManufaturer() + "|Processor:" + getProcessor() + "|Raw:" + getRaw();
    }

    @Override
    public ID getHardwareType() {
        return ID.RASPBERRYPI;
    }

    public abstract String getRaw();

    public abstract boolean isNewStyleRevision();

    public abstract MANUFACTURER getManufaturer();

    public abstract PROCESSOR getProcessor();

    public abstract MEMORY_SIZE getMemorySize();

    public abstract TYPE getType();
}
