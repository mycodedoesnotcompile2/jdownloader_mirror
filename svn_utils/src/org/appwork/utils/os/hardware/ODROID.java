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

/**
 * @author daniel
 * @date Sep 9, 2020
 *
 */
public abstract class ODROID implements HardwareTypeInterface {

    public static enum TYPE {
        ODROID,
        U2,
        X2,
        U3,
        XU,
        XU3,
        XU3_LITE("XU3-Lite"),
        W,
        C1,
        C1_PLUS("C1+"),
        C2,
        H2,
        N2,
        C4,
        XU4,
        XU4Q,
        GO,
        HC1,
        HC2,
        MC1,
        N2_PLUS("N2+"),
        H2_PLUS("H2+"),
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

    @Override
    public ID getHardwareType() {
        return HardwareTypeInterface.ID.ODROID;
    }

    public static ODROID getODROIDDetails() {
        if (CrossSystem.isUnix()) {
            for (final String infoSource : new String[] { "/proc/cpuinfo", "/proc/device-tree/mode" }) {
                final File cpuInfo = new File(infoSource);
                if (cpuInfo.isFile()) {
                    FileInputStream fis = null;
                    try {
                        fis = new FileInputStream(cpuInfo);
                        final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                        String odroidHardware = null;
                        String line = null;
                        while ((line = is.readLine()) != null) {
                            if (StringUtils.startsWithCaseInsensitive(line, "Hardware")) {
                                odroidHardware = new Regex(line, "(?i)^\\s*Hardware\\s*:\\s*(.*?ODROID.*?)$").getMatch(0);
                            } else if (StringUtils.contains(line, "Hardkernel") || StringUtils.contains(line, "Odroid")) {
                                odroidHardware = line;
                            }
                            if (odroidHardware != null) {
                                break;
                            }
                        }
                        is.close();
                        if (odroidHardware != null) {
                            return parse(odroidHardware);
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
        }
        return null;
    }

    private static ODROID parse(final String hardware) {
        if (StringUtils.containsIgnoreCase(hardware, "ODROID")) {
            return new ODROID() {

                @Override
                public TYPE getType() {
                    if (StringUtils.contains(hardware, "HC2")) {
                        return TYPE.HC2;
                    } else if (StringUtils.contains(hardware, "HC1")) {
                        return TYPE.HC1;
                    } else if (StringUtils.contains(hardware, "MC1")) {
                        return TYPE.MC1;
                    } else if (StringUtils.contains(hardware, "N2+")) {
                        return TYPE.N2_PLUS;
                    } else if (StringUtils.contains(hardware, "N2")) {
                        return TYPE.N2;
                    } else if (StringUtils.contains(hardware, "C1+") || StringUtils.contains(hardware, "C+")) {
                        return TYPE.C1_PLUS;
                    } else if (StringUtils.contains(hardware, "H2+")) {
                        return TYPE.H2_PLUS;
                    } else if (StringUtils.contains(hardware, "H2")) {
                        return TYPE.H2;
                    } else if (StringUtils.contains(hardware, "C2")) {
                        return TYPE.C2;
                    } else if (StringUtils.contains(hardware, "XU4Q")) {
                        return TYPE.XU4Q;
                    } else if (StringUtils.contains(hardware, "XU4")) {
                        return TYPE.XU4;
                    } else if (StringUtils.contains(hardware, "XU3")) {
                        return TYPE.XU3;
                    } else if (StringUtils.contains(hardware, "XU")) {
                        return TYPE.XU;
                    } else if (StringUtils.contains(hardware, "U3")) {
                        return TYPE.U3;
                    } else if (StringUtils.contains(hardware, "X2")) {
                        return TYPE.X2;
                    } else if (StringUtils.contains(hardware, "U2")) {
                        return TYPE.U2;
                    } else if (StringUtils.contains(hardware, "W")) {
                        return TYPE.W;
                    } else if (StringUtils.contains(hardware, "C4")) {
                        return TYPE.C4;
                    } else if (StringUtils.contains(hardware, "GO")) {
                        return TYPE.GO;
                    } else if (StringUtils.contains(hardware, "C")) {
                        return TYPE.C1;
                    } else {
                        return TYPE.UNKNOWN;
                    }
                }

                @Override
                public String getRaw() {
                    return hardware;
                }
            };
        } else {
            return null;
        }
    }

    public String toString() {
        return "ODROID|Type:" + getType() + "|RAW:" + getRaw();
    }

    private ODROID() {
    }

    public abstract TYPE getType();

    public abstract String getRaw();
}
