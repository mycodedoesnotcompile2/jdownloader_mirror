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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os;

import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import javax.swing.KeyStroke;

import org.appwork.JNAHelper;
import org.appwork.builddecision.BuildDecisions;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.StorableDoc;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.mime.Mime;
import org.appwork.utils.os.mime.MimeFactory;
import org.appwork.utils.processes.ProcessBuilderFactory;

/**
 * This class provides a few native features.
 *
 * @author $Author: unknown$
 */
public class CrossSystem {
    @StorableDoc("Enumeration of supported operating systems and their specific versions. Used for OS detection and platform-specific functionality.")
    public static enum OperatingSystem {
        @StorableDoc("NetBSD operating system")
        NETBSD(OSFamily.BSD),
        @StorableDoc("OpenBSD operating system")
        OPENBSD(OSFamily.BSD),
        @StorableDoc("kFreeBSD (Debian GNU/kFreeBSD)")
        KFREEBSD(OSFamily.BSD),
        @StorableDoc("FreeBSD operating system")
        FREEBSD(OSFamily.BSD),
        @StorableDoc("DragonFlyBSD operating system")
        DRAGONFLYBSD(OSFamily.BSD),
        @StorableDoc("Generic BSD operating system")
        BSD(OSFamily.BSD),
        @StorableDoc("Generic Linux operating system")
        LINUX(OSFamily.LINUX),
        /*
         * Fedora: List must be sorted by release Date!!
         */
        @StorableDoc("Fedora Linux distribution")
        FEDORA(OSFamily.LINUX),
        @StorableDoc("Bazzite Linux distribution (Steam Deck optimized)")
        BAZZITE(OSFamily.LINUX),
        /*
         * CentOS: List must be sorted by release Date!!
         */
        @StorableDoc("CentOS Linux distribution")
        CENTOS(OSFamily.LINUX),
        @StorableDoc("CentOS Stream Linux distribution")
        CENTOS_STREAM(OSFamily.LINUX),
        /*
         * openSUSE: List must be sorted by release Date!!
         */
        @StorableDoc("openSUSE Linux distribution")
        OPENSUSE(OSFamily.LINUX),
        @StorableDoc("openSUSE Leap Linux distribution")
        OPENSUSE_LEAP(OSFamily.LINUX, "opensuse-leap"),
        @StorableDoc("openSUSE Tumbleweed Linux distribution (rolling release)")
        OPENSUSE_TUMBLEWEED(OSFamily.LINUX, "opensuse-tumbleweed"),
        /*
         * Slackware: List must be sorted by release Date!!
         */
        @StorableDoc("Slackware Linux distribution")
        SLACKWARE(OSFamily.LINUX),
        /*
         * Arch: List must be sorted by release Date!!
         */
        @StorableDoc("Arch Linux distribution")
        ARCH(OSFamily.LINUX),
        /*
         * Gentoo: List must be sorted by release Date!!
         */
        @StorableDoc("Gentoo Linux distribution")
        GENTOO(OSFamily.LINUX),
        /*
         * Red Hat: List must be sorted by release Date!!
         */
        @StorableDoc("Red Hat Enterprise Linux (RHEL)")
        REDHAT(OSFamily.LINUX),
        /*
         * SUSE Linux Enterprise Server: List must be sorted by release Date!!
         */
        @StorableDoc("SUSE Linux Enterprise Server (SLES)")
        SLES(OSFamily.LINUX),
        /*
         * Alpine Linux
         */
        @StorableDoc("Alpine Linux distribution")
        ALPINE(OSFamily.LINUX),
        /*
         * Manjaro
         */
        @StorableDoc("Manjaro Linux distribution (Arch-based)")
        MANJARO(OSFamily.LINUX),
        /*
         * EndeavourOS(Arch Linux)
         */
        @StorableDoc("EndeavourOS Linux distribution (Arch-based)")
        ENDEAVOUROS(OSFamily.LINUX),
        /*
         * Elementary OS
         */
        @StorableDoc("Elementary OS Linux distribution")
        ELEMENTARYOS(OSFamily.LINUX),
        /*
         * NixOS
         */
        @StorableDoc("NixOS Linux distribution")
        NIXOS(OSFamily.LINUX),
        /**
         * Mageia
         */
        @StorableDoc("Mageia Linux distribution")
        MAGEIA(OSFamily.LINUX),
        /**
         * AlmaLinux
         */
        @StorableDoc("AlmaLinux Linux distribution (RHEL-compatible)")
        ALMALINUX(OSFamily.LINUX),
        /**
         * Rocky Linux
         */
        @StorableDoc("Rocky Linux distribution (RHEL-compatible)")
        ROCKYLINUX(OSFamily.LINUX),
        /**
         * https://www.kali.org/releases/
         *
         * Kali Linux: List must be sorted by release Date!!
         */
        @StorableDoc("Kali Linux distribution (penetration testing)")
        KALILINUX(OSFamily.LINUX),
        @StorableDoc("Kali Linux 2022.1 release")
        KALILINUX_2022_1(OSFamily.LINUX, "2022\\.1"),
        @StorableDoc("Kali Linux 2022.2 release")
        KALILINUX_2022_2(OSFamily.LINUX, "2022\\.2"),
        @StorableDoc("Kali Linux 2022.3 release")
        KALILINUX_2022_3(OSFamily.LINUX, "2022\\.3"),
        @StorableDoc("Kali Linux 2022.4 release")
        KALILINUX_2022_4(OSFamily.LINUX, "2022\\.4"),
        @StorableDoc("Kali Linux 2023.1 release")
        KALILINUX_2023_1(OSFamily.LINUX, "2023\\.1"),
        @StorableDoc("Kali Linux 2023.2 release")
        KALILINUX_2023_2(OSFamily.LINUX, "2023\\.2"),
        @StorableDoc("Kali Linux 2023.3 release")
        KALILINUX_2023_3(OSFamily.LINUX, "2023\\.3"),
        @StorableDoc("Kali Linux 2023.4 release")
        KALILINUX_2023_4(OSFamily.LINUX, "2023\\.4"),
        @StorableDoc("Kali Linux 2024.1 release")
        KALILINUX_2024_1(OSFamily.LINUX, "2024\\.1"),
        @StorableDoc("Kali Linux 2024.2 release")
        KALILINUX_2024_2(OSFamily.LINUX, "2024\\.2"),
        @StorableDoc("Kali Linux 2024.3 release")
        KALILINUX_2024_3(OSFamily.LINUX, "2024\\.3"),
        @StorableDoc("Kali Linux 2024.4 release")
        KALILINUX_2024_4(OSFamily.LINUX, "2024\\.4"),
        @StorableDoc("Kali Linux 2025.1 release")
        KALILINUX_2025_1(OSFamily.LINUX, "2025\\.1"),
        @StorableDoc("Kali Linux 2025.2 release")
        KALILINUX_2025_2(OSFamily.LINUX, "2025\\.2"),
        @StorableDoc("Kali Linux 2025.3 release")
        KALILINUX_2025_3(OSFamily.LINUX, "2025\\.3"),
        @StorableDoc("Kali Linux 2025.4 release")
        KALILINUX_2025_4(OSFamily.LINUX, "2025\\.4"),
        /*
         * https://www.debian.org/releases/
         *
         * Debian: List must be sorted by release Date!!
         */
        @StorableDoc("Debian Linux distribution")
        DEBIAN(OSFamily.LINUX),
        @StorableDoc("Debian 5.0 (Lenny)")
        DEBIAN_LENNY(OSFamily.LINUX, "lenny"),
        @StorableDoc("Debian 6.0 (Squeeze)")
        DEBIAN_SQUEEZE(OSFamily.LINUX, "squeeze"), // 6
        @StorableDoc("Debian 7.0 (Wheezy)")
        DEBIAN_WHEEZY(OSFamily.LINUX, "wheezy"), // 7
        @StorableDoc("Debian 8.0 (Jessie)")
        DEBIAN_JESSIE(OSFamily.LINUX, "jessie"), // 8
        @StorableDoc("Debian 9.0 (Stretch)")
        DEBIAN_STRETCH(OSFamily.LINUX, "stretch"), // 9
        @StorableDoc("Debian 10.0 (Buster)")
        DEBIAN_BUSTER(OSFamily.LINUX, "buster"), // 10
        @StorableDoc("Debian 11.0 (Bullseye)")
        DEBIAN_BULLSEYE(OSFamily.LINUX, "bull"), // 11
        @StorableDoc("Debian 12.0 (Bookworm)")
        DEBIAN_BOOKWORM(OSFamily.LINUX, "bookworm"), // 12
        @StorableDoc("Debian 13.0 (Trixie)")
        DEBIAN_TRIXIE(OSFamily.LINUX, "trixie"), // 13
        @StorableDoc("Debian 14.0 (Forky)")
        DEBIAN_FORKY(OSFamily.LINUX, "forky"), // 14
        @StorableDoc("Debian Unstable (Sid)")
        DEBIAN_SID(OSFamily.LINUX, "sid"), // unstable
        /*
         * RASPBIAN
         *
         * RASPBIAN: List must be sorted by release Date!!
         */
        @StorableDoc("Raspbian Linux distribution (Raspberry Pi OS)")
        RASPBIAN(OSFamily.LINUX),
        @StorableDoc("Raspbian based on Debian Wheezy")
        RASPBIAN_WHEEZY(OSFamily.LINUX, "wheezy"),
        @StorableDoc("Raspbian based on Debian Jessie")
        RASPBIAN_JESSIE(OSFamily.LINUX, "jessie"),
        @StorableDoc("Raspbian based on Debian Stretch")
        RASPBIAN_STRETCH(OSFamily.LINUX, "stretch"),
        @StorableDoc("Raspbian based on Debian Buster")
        RASPBIAN_BUSTER(OSFamily.LINUX, "buster"),
        @StorableDoc("Raspbian based on Debian Bullseye")
        RASPBIAN_BULLSEYE(OSFamily.LINUX, "bull"),
        @StorableDoc("Raspbian based on Debian Bookworm")
        RASPBIAN_BOOKWORM(OSFamily.LINUX, "bookworm"),
        @StorableDoc("Raspbian based on Debian Trixie")
        RASPBIAN_TRIXIE(OSFamily.LINUX, "trixie"),
        /*
         * https://en.wikipedia.org/wiki/Ubuntu_version_history
         *
         * https://wiki.ubuntu.com/Releases
         *
         * Ubuntu: List must be sorted by release Date!!
         */
        @StorableDoc("Ubuntu Linux distribution")
        UBUNTU(OSFamily.LINUX),
        @StorableDoc("Ubuntu 12.04 LTS (Precise Pangolin)")
        UBUNTU_PRECISE(OSFamily.LINUX, "12\\.04"), // 12.04
        @StorableDoc("Ubuntu 12.10 (Quantal Quetzal)")
        UBUNTU_QUANTAL(OSFamily.LINUX, "12\\.10"), // 12.10
        @StorableDoc("Ubuntu 13.04 (Raring Ringtail)")
        UBUNTU_RARING(OSFamily.LINUX, "13\\.04"), // 13.04
        @StorableDoc("Ubuntu 13.10 (Saucy Salamander)")
        UBUNTU_SAUCY(OSFamily.LINUX, "13\\.10"), // 13.10
        @StorableDoc("Ubuntu 14.04 LTS (Trusty Tahr)")
        UBUNTU_TRUSTY(OSFamily.LINUX, "14\\.04"), // 14.04
        @StorableDoc("Ubuntu 14.10 (Utopic Unicorn)")
        UBUNTU_UTOPIC(OSFamily.LINUX, "14\\.10"), // 14.10
        @StorableDoc("Ubuntu 15.04 (Vivid Vervet)")
        UBUNTU_VIVID(OSFamily.LINUX, "15\\.04"), // 15.04
        @StorableDoc("Ubuntu 15.10 (Wily Werewolf)")
        UBUNTU_WILY(OSFamily.LINUX, "15\\.10"), // 15.10
        @StorableDoc("Ubuntu 16.04 LTS (Xenial Xerus)")
        UBUNTU_XENIAL(OSFamily.LINUX, "16\\.04"), // 16.04
        @StorableDoc("Ubuntu 16.10 (Yakkety Yak)")
        UBUNTU_YAKKETY(OSFamily.LINUX, "16\\.10"), // 16.10
        @StorableDoc("Ubuntu 17.04 (Zesty Zapus)")
        UBUNTU_ZESTY(OSFamily.LINUX, "17\\.04"), // 17.04
        @StorableDoc("Ubuntu 17.10 (Artful Aardvark)")
        UBUNTU_ARTFUL(OSFamily.LINUX, "17\\.10"), // 17.10
        @StorableDoc("Ubuntu 18.04 LTS (Bionic Beaver)")
        UBUNTU_BIONIC(OSFamily.LINUX, "18\\.04"), // 18.04
        @StorableDoc("Ubuntu 18.10 (Cosmic Cuttlefish)")
        UBUNTU_COSMIC(OSFamily.LINUX, "18\\.10"), // 18.10
        @StorableDoc("Ubuntu 19.04 (Disco Dingo)")
        UBUNTU_DISCO(OSFamily.LINUX, "19\\.04"), // 19.04
        @StorableDoc("Ubuntu 19.10 (Eoan Ermine)")
        UBUNTU_EOAN(OSFamily.LINUX, "19\\.10"), // 19.10
        @StorableDoc("Ubuntu 20.04 LTS (Focal Fossa)")
        UBUNTU_FOCAL(OSFamily.LINUX, "20\\.04"), // 20.04
        @StorableDoc("Ubuntu 20.10 (Groovy Gorilla)")
        UBUNTU_GROOVY(OSFamily.LINUX, "20\\.10"), // 20.10
        @StorableDoc("Ubuntu 21.04 (Hirsute Hippo)")
        UBUNTU_HIRSUTE(OSFamily.LINUX, "21\\.04"), // 21.04
        @StorableDoc("Ubuntu 21.10 (Impish Indri)")
        UBUNTU_IMPISH(OSFamily.LINUX, "21\\.10"), // 21.10
        @StorableDoc("Ubuntu 22.04 LTS (Jammy Jellyfish)")
        UBUNTU_JAMMY(OSFamily.LINUX, "22\\.04"), // 22.04
        @StorableDoc("Ubuntu 22.10 (Kinetic Kudu)")
        UBUNTU_KINETIC(OSFamily.LINUX, "22\\.10"), // 22.10
        @StorableDoc("Ubuntu 23.04 (Lunar Lobster)")
        UBUNTU_LUNAR(OSFamily.LINUX, "23\\.04"), // 23.04
        @StorableDoc("Ubuntu 23.10 (Mantic Minotaur)")
        UBUNTU_MANTIC(OSFamily.LINUX, "23\\.10"), // 23.10
        @StorableDoc("Ubuntu 24.04 LTS (Noble Numbat)")
        UBUNTU_NOBLE(OSFamily.LINUX, "24\\.04"), // 24.04
        @StorableDoc("Ubuntu 24.10 (Oracular Oriole)")
        UBUNTU_ORACULAR(OSFamily.LINUX, "24\\.10"), // 24.10
        @StorableDoc("Ubuntu 25.04 (Plucky Pangolin)")
        UBUNTU_PLUCKY(OSFamily.LINUX, "25\\.04"), // 25.04
        @StorableDoc("Ubuntu 25.10 (Questing Quail)")
        UBUNTU_QUESTING(OSFamily.LINUX, "25\\.10"), // 25.10
        @StorableDoc("Ubuntu 26.04 LTS (Resolute Rhino)")
        UBUNTU_RESOLUTE(OSFamily.LINUX, "26\\.04"), // 26.04
        /*
         * MAC: List must be sorted by release Date!!
         */
        @StorableDoc("Generic macOS operating system")
        MAC(OSFamily.MAC),
        @StorableDoc("macOS 10.0 (Cheetah)")
        MAC_CHEETAH(OSFamily.MAC), // 10.0
        @StorableDoc("macOS 10.1 (Puma)")
        MAC_PUMA(OSFamily.MAC), // 10.1
        @StorableDoc("macOS 10.2 (Jaguar)")
        MAC_JAGUAR(OSFamily.MAC), // 10.2
        @StorableDoc("macOS 10.3 (Panther)")
        MAC_PANTHER(OSFamily.MAC), // 10.3
        @StorableDoc("macOS 10.4 (Tiger)")
        MAC_TIGER(OSFamily.MAC), // 10.4
        @StorableDoc("macOS 10.5 (Leopard)")
        MAC_LEOPOARD(OSFamily.MAC), // 10.5
        @StorableDoc("macOS 10.6 (Snow Leopard)")
        MAC_SNOW_LEOPOARD(OSFamily.MAC), // 10.6
        @StorableDoc("macOS 10.7 (Lion)")
        MAC_LION(OSFamily.MAC), // 10.7
        @StorableDoc("macOS 10.8 (Mountain Lion)")
        MAC_MOUNTAIN_LION(OSFamily.MAC), // 10.8
        @StorableDoc("macOS 10.9 (Mavericks)")
        MAC_MAVERICKS(OSFamily.MAC), // 10.9
        @StorableDoc("macOS 10.10 (Yosemite)")
        MAC_YOSEMITE(OSFamily.MAC), // 10.10
        @StorableDoc("macOS 10.11 (El Capitan)")
        MAC_EL_CAPITAN(OSFamily.MAC), // 10.11
        @StorableDoc("macOS 10.12 (Sierra)")
        MAC_SIERRA(OSFamily.MAC), // 10.12
        @StorableDoc("macOS 10.13 (High Sierra)")
        MAC_HIGH_SIERRA(OSFamily.MAC), // 10.13
        @StorableDoc("macOS 10.14 (Mojave)")
        MAC_MOJAVE(OSFamily.MAC), // 10.14
        @StorableDoc("macOS 10.15 (Catalina)")
        MAC_CATALINA(OSFamily.MAC), // 10.15
        @StorableDoc("macOS 11.0 (Big Sur)")
        MAC_BIG_SUR(OSFamily.MAC), // 10.16/11.00
        @StorableDoc("macOS 12.0 (Monterey)")
        MAC_MONTEREY(OSFamily.MAC), // 10.17/12.00
        @StorableDoc("macOS 13.0 (Ventura)")
        MAC_VENTURA(OSFamily.MAC), // 10.18/13.00
        @StorableDoc("macOS 14.0 (Sonoma)")
        MAC_SONOMA(OSFamily.MAC), // 10.19/14.00
        @StorableDoc("macOS 15.0 (Sequoia)")
        MAC_SEQUOIA(OSFamily.MAC), // 10.20/15.00
        @StorableDoc("macOS 26.0 (Tahoe)")
        MAC_TAHOE(OSFamily.MAC), // 10.xx/26.00
        /*
         * OS2
         */
        @StorableDoc("OS/2 operating system")
        OS2(OSFamily.OS2),
        /*
         * Windows: List must be sorted by release Date!!
         */
        @StorableDoc("Other or unknown Windows version")
        WINDOWS_OTHERS(OSFamily.WINDOWS),
        @StorableDoc("Windows NT")
        WINDOWS_NT(OSFamily.WINDOWS),
        @StorableDoc("Windows 2000")
        WINDOWS_2000(OSFamily.WINDOWS),
        @StorableDoc("Windows XP")
        WINDOWS_XP(OSFamily.WINDOWS),
        @StorableDoc("Windows 2003")
        WINDOWS_2003(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2003")
        WINDOWS_SERVER_2003(OSFamily.WINDOWS),
        @StorableDoc("Windows Vista")
        WINDOWS_VISTA(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2008")
        WINDOWS_SERVER_2008(OSFamily.WINDOWS),
        @StorableDoc("Windows 7")
        WINDOWS_7(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2008 R2")
        WINDOWS_SERVER_2008_R2(OSFamily.WINDOWS),
        @StorableDoc("Windows 8")
        WINDOWS_8(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2012")
        WINDOWS_SERVER_2012(OSFamily.WINDOWS),
        @StorableDoc("Windows 8.1")
        WINDOWS_8_1(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2012 R2")
        WINDOWS_SERVER_2012_R2(OSFamily.WINDOWS),
        @StorableDoc("Windows 10")
        WINDOWS_10(OSFamily.WINDOWS),
        @StorableDoc("Windows 10 version 20H2 (October 2020 Update)")
        WINDOWS_10_20H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 10 version 21H1 (May 2021 Update)")
        WINDOWS_10_21H1(OSFamily.WINDOWS),
        @StorableDoc("Windows 10 version 21H2 (November 2021 Update)")
        WINDOWS_10_21H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 10 version 22H2 (October 2022 Update)")
        WINDOWS_10_22H2(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2016")
        WINDOWS_SERVER_2016(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2019")
        WINDOWS_SERVER_2019(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2020")
        WINDOWS_SERVER_2020(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2022")
        WINDOWS_SERVER_2022(OSFamily.WINDOWS),
        @StorableDoc("Windows Server 2025")
        WINDOWS_SERVER_2025(OSFamily.WINDOWS),
        @StorableDoc("Windows 11")
        WINDOWS_11(OSFamily.WINDOWS), // WINDOWS_11_21H2
        @StorableDoc("Windows 11 version 21H2 (original release)")
        WINDOWS_11_21H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 11 version 22H2 (2022 Update)")
        WINDOWS_11_22H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 11 version 23H2 (2023 Update)")
        WINDOWS_11_23H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 11 version 24H2 (2024 Update)")
        WINDOWS_11_24H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 11 version 25H2 (2025 Update)")
        WINDOWS_11_25H2(OSFamily.WINDOWS),
        @StorableDoc("Windows 11 version 26H1 (2026 Update)")
        WINDOWS_11_26H1(OSFamily.WINDOWS);

        private final OSFamily family;
        private final Pattern  releasePattern;

        private OperatingSystem(final OSFamily family) {
            this(family, (Pattern) null);
        }

        private OperatingSystem(final OSFamily family, final Pattern releasePattern) {
            this.family = family;
            this.releasePattern = releasePattern;
        }

        private OperatingSystem(final OSFamily family, final String releasePattern) {
            this(family, releasePattern != null ? Pattern.compile(releasePattern) : null);
        }

        public final OSFamily getFamily() {
            return this.family;
        }

        private boolean isRelease(final String line) {
            return this.releasePattern != null && line != null && this.releasePattern.matcher(line).find();
        }

        public static boolean sameOSFamily(final OperatingSystem x, final OperatingSystem y) {
            if (x.getFamily().equals(y.getFamily())) {
                final String prefixX = x.name().replaceFirst("(_.+)", "");
                final String prefixY = y.name().replaceFirst("(_.+)", "");
                return prefixX.equals(prefixY);
            } else {
                return false;
            }
        }

        public boolean sameOSFamily(final OperatingSystem x) {
            return sameOSFamily(x, this);
        }

        public final boolean isMaximum(final OperatingSystem os) {
            if (this.sameOSFamily(os)) {
                final int maximum = os.ordinal();
                return this.ordinal() <= maximum;
            } else {
                return false;
            }
        }

        public final boolean isMinimum(final OperatingSystem os) {
            if (this.sameOSFamily(os)) {
                final int minimum = os.ordinal();
                return this.ordinal() >= minimum;
            } else {
                return false;
            }
        }
    }

    private final static EnumSet<OperatingSystem> OperatingSystems = EnumSet.allOf(OperatingSystem.class);

    @StorableDoc("Enumeration of operating system families. Groups operating systems by their base platform (BSD, Linux, Mac, Windows, etc.).")
    public static enum OSFamily {
        @StorableDoc("BSD family of operating systems (FreeBSD, OpenBSD, NetBSD, etc.)")
        BSD,
        @StorableDoc("Linux family of operating systems (Ubuntu, Debian, Fedora, etc.)")
        LINUX,
        @StorableDoc("macOS family (Apple Macintosh operating systems)")
        MAC,
        @StorableDoc("OS/2 operating system")
        OS2,
        @StorableDoc("Other or unknown operating system families")
        OTHERS,
        @StorableDoc("Windows family of operating systems (Windows 7, 10, 11, Server editions, etc.)")
        WINDOWS;

        public static OSFamily get(final OperatingSystem os) {
            return os != null ? os.getFamily() : null;
        }

        public static OSFamily get(final String os) {
            if (os != null && os.length() > 2) {
                try {
                    return OperatingSystem.valueOf(os).getFamily();
                } catch (final IllegalArgumentException ignore) {
                }
                try {
                    return OSFamily.valueOf(os.replaceFirst("(_.+)", ""));
                } catch (final IllegalArgumentException ignore) {
                }
            }
            return null;
        }
    }

    @StorableDoc("Enumeration of processor architecture families. Used to identify the CPU architecture of the system.")
    public static enum ARCHFamily {
        @StorableDoc("Not available or unknown architecture")
        NA,
        @StorableDoc("x86 architecture family (Intel/AMD 32-bit and 64-bit processors: i386, i486, i586, i686, x86, amd64, x86_64)")
        X86,
        @StorableDoc("ARM architecture family (ARM processors: arm, aarch64, arm64)")
        ARM,
        @StorableDoc("PowerPC architecture family (PPC processors: ppc, powerpc, ppc64)")
        PPC,
        @StorableDoc("MIPS architecture family (MIPS processors: mips, mipsel)")
        MIPS,
        @StorableDoc("SPARC architecture family (SPARC processors: sparc, sparcv9)")
        SPARC,
        @StorableDoc("Intel Itanium 64-bit architecture (IA-64 processors)")
        IA64,
        @StorableDoc("RISC-V architecture family (RISC-V processors: riscv32, riscv64)")
        RISCV,
        @StorableDoc("LoongArch architecture family (LoongArch processors: loongarch64, loong64)")
        LOONGARCH
    }

    public static boolean isUnix() {
        return CrossSystem.isBSD() || CrossSystem.isLinux();
    }

    private static volatile String[]                     BROWSER_COMMANDLINE = null;
    private static final AtomicReference<DesktopSupport> DESKTOP_SUPPORT     = new AtomicReference<DesktopSupport>();
    private static String[]                              FILE_COMMANDLINE    = null;
    private static String                                JAVAINT             = null;
    /**
     * Cache to store the Mime Class in
     */
    private static final AtomicReference<Mime>           MIME                = new AtomicReference<Mime>();
    public static final OperatingSystem                  OS;
    public static final ARCHFamily                       ARCH;
    /**
     * Cache to store the OS string in
     */
    private final static String                          OS_STRING;
    private final static String                          ARCH_STRING;
    private static Boolean                               OS64BIT             = null;
    static {
        /* Init OS_ID */
        OS_STRING = System.getProperty("os.name");
        ARCH_STRING = System.getProperty("os.arch");
        OS = CrossSystem.getOSByString(CrossSystem.OS_STRING);
        ARCH = CrossSystem.getARCHByString(CrossSystem.ARCH_STRING);
    }

    public static void setDesktopSupportInstance(final DesktopSupport desktopSupport) {
        if (desktopSupport == null) {
            throw new IllegalArgumentException();
        }
        DESKTOP_SUPPORT.set(desktopSupport);
    }

    public static String getDefaultDownloadDirectory() {
        try {
            final String defaultDownloadDirectory = CrossSystem.getDesktopSupport().getDefaultDownloadDirectory();
            if (StringUtils.isNotEmpty(defaultDownloadDirectory)) {
                //
                return defaultDownloadDirectory;
            }
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        final String userHome = System.getProperty("user.home");
        if (userHome != null && new File(userHome).isDirectory()) {
            return new File(userHome, "Downloads").getAbsolutePath();
        } else {
            return Application.getResource("Downloads").getAbsolutePath();
        }
    }

    /**
     * internal function to open a file/folder
     *
     * @param file
     * @param trySingleInstance
     * @throws IOException
     */
    private static void _openFILE(final File file, boolean trySingleInstance) throws IOException {
        try {
            if (CrossSystem.openCustom(CrossSystem.FILE_COMMANDLINE, file.getAbsolutePath())) {
                return;
            } else if (CrossSystem.isOpenFileSupported()) {
                CrossSystem.getDesktopSupport().openFile(file, trySingleInstance);
            }
        } catch (final IOException e) {
            if (CrossSystem.isOpenFileSupported()) {
                CrossSystem.getDesktopSupport().openFile(file, trySingleInstance);
            } else {
                throw e;
            }
        }
    }

    /**
     * internal function to open an URL in a browser
     *
     * @param _url
     * @throws IOException
     * @throws URISyntaxException
     */
    public static void openUrlOrThrowException(final String _url) throws IOException, URISyntaxException {
        try {
            if (CrossSystem.openCustom(CrossSystem.BROWSER_COMMANDLINE, _url)) {
                return;
            } else if (CrossSystem.isOpenBrowserSupported()) {
                CrossSystem.getDesktopSupport().browseURL(new URL(_url));
            } else {
                throw new IOException("Unsupported OpenBrowser:" + _url);
            }
        } catch (final IOException e) {
            if (CrossSystem.isOpenBrowserSupported()) {
                CrossSystem.getDesktopSupport().browseURL(new URL(_url));
            } else {
                throw e;
            }
        }
    }

    /**
     * use this method to make pathPart safe to use in a full absoluePath.
     *
     * it will remove driveletters/path separators and all known chars that are forbidden in a path
     *
     * @param pathPart
     * @return
     */
    public static String alleviatePathParts(final String pathPart) {
        return alleviatePathParts(pathPart, true);
    }

    /**
     * use this method to make pathPart safe to use in a full absoluePath.
     *
     * it will remove driveletters/path separators and all known chars that are forbidden in a path
     *
     * @param pathPart
     *            {@link String}
     * @param removeLeadingHidingDot
     *            {@link Boolean} remove leading/hiding(unix) dot
     * @return
     */
    public static String alleviatePathParts(String pathPart, final boolean removeLeadingHidingDot) {
        if (StringUtils.isEmpty(pathPart)) {
            if (pathPart != null) {
                return pathPart;
            }
            return null;
        }
        pathPart = pathPart.trim();
        /* remove invalid chars */
        /**
         * Integer value zero, sometimes referred to as the ASCII NUL character.
         *
         * Characters whose integer representations are in the range from 1 through 31->\\x00-\\x1f
         *
         * < (less than), * > (greater than), : (colon), " (double quote), / (forward slash), \ (backslash), | (vertical bar or pipe), ?
         * (question* mark) (asterisk)
         *
         * Volume designators (drive letters) are similarly case-insensitive. For example, "D:\" and "d:\" refer to the same volume.
         */
        pathPart = pathPart.replaceAll("([\\\\|<|>|\\||\r|\n|\t|\"|:|\\*|\\?|/|\\x00-\\x1f])+", "_");
        if (CrossSystem.isWindows() || CrossSystem.isOS2()) {
            /**
             * http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
             */
            if (CrossSystem.isForbiddenFilename(pathPart)) {
                pathPart = "_" + pathPart;
            }
        }
        /*
         * replace starting dots by single dot (prevents directory traversal)
         */
        if (removeLeadingHidingDot) {
            pathPart = pathPart.replaceFirst("^\\.+", "");
        } else {
            pathPart = pathPart.replaceFirst("^\\.+", ".");
        }
        /*
         * remove ending dots, not allowed under windows and others os maybe too
         *
         * Do not end a file or directory name with a space or a period.
         */
        pathPart = pathPart.replaceFirst("\\.+$", "");
        pathPart = pathPart.trim();
        if (StringUtils.isEmpty(pathPart)) {
            return "_";
        } else {
            return pathPart;
        }
    }

    public static boolean isForbiddenFilename(final String name) {
        if (CrossSystem.isWindows() || CrossSystem.isOS2()) {
            /**
             * http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
             *
             * Do not use the following reserved names for the name of a file:
             *
             * CON, PRN, AUX, NUL, COM1, COM2, COM3, COM4, COM5, COM6, COM7, COM8, COM9, LPT1, LPT2, LPT3, LPT4, LPT5, LPT6, LPT7, LPT8, and
             * LPT9.
             */
            return new Regex(name, "^(CON|PRN|AUX|NUL|COM\\d+|LPT\\d+|CLOCK)\\s*?(\\.|$)").matches();
        }
        return false;
    }

    public static String fixPathSeparators(String path) {
        if (StringUtils.isEmpty(path)) {
            if (path != null) {
                return path;
            }
            return null;
        }
        if (CrossSystem.isWindows()) {
            /* windows uses \ as path separator */
            final boolean network = path.startsWith("\\\\");
            path = path.replaceAll("(/+)", "\\\\");
            path = path.replaceAll("(\\\\+)", "\\\\");
            if (network) {
                path = "\\" + path;
            }
        } else {
            /* mac/linux uses / as path separator */
            path = path.replaceAll("(\\\\+)", "/");
            path = path.replaceAll("(/+)", "/");
        }
        return path;
    }

    public static String[] getBrowserCommandLine() {
        return CrossSystem.BROWSER_COMMANDLINE;
    }

    /**
     * @return
     */
    public static KeyStroke getDeleteShortcut() {
        if (GraphicsEnvironment.isHeadless()) {
            return null;
        } else {
            if (CrossSystem.isMac()) {
                return KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
            } else {
                return KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0);
            }
        }
    }

    public static String[] getFileCommandLine() {
        return CrossSystem.FILE_COMMANDLINE;
    }

    public static String getJavaBinary() {
        if (CrossSystem.JAVAINT != null) {
            return CrossSystem.JAVAINT;
        }
        String javaBinary = "java";
        if (CrossSystem.isWindows() || CrossSystem.isOS2()) {
            javaBinary = "javaw.exe";
        }
        final String javaHome = System.getProperty("java.home");
        if (javaHome != null) {
            /* get path from system property */
            final File java = new File(new File(javaHome), "/bin/" + javaBinary);
            if (java.exists() && java.isFile()) {
                CrossSystem.JAVAINT = java.getAbsolutePath();
            }
        } else {
            CrossSystem.JAVAINT = javaBinary;
        }
        return CrossSystem.JAVAINT;
    }

    private static long parseMacOSVersion(final String osVersionProperty) {
        if (osVersionProperty != null) {
            try {
                long ret = 0;
                long faktor = 1000000;
                for (final String s : osVersionProperty.trim().split("\\.")) {
                    ret += Integer.parseInt(s) * faktor;
                    faktor /= 1000;
                }
                return ret;
            } catch (final Throwable ignore) {
            }
        }
        return -1;
    }

    @Deprecated
    public static boolean caseSensitiveFileExists(final File file) {
        if (file != null) {
            if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
                try {
                    /**
                     * this is very fast
                     */
                    return CrossSystem17.caseSensitiveFileExists(file);
                } catch (final Throwable e) {
                    LogV3.defaultLogger().log(e);
                }
            }
            if (file.exists()) {
                /** this can be slow **/
                File current = file;
                String currentName = current.getName();
                loop: while ((current = current.getParentFile()) != null) {
                    final String[] list = current.list();
                    if (list != null) {
                        for (final String listItem : list) {
                            if (currentName.equals(listItem)) {
                                currentName = current.getName();
                                continue loop;
                            }
                        }
                    }
                    return false;
                }
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    /**
     * Returns the Mime Class for the current OS
     *
     * @return
     * @see Mime
     */
    public static Mime getMime() {
        Mime ret = CrossSystem.MIME.get();
        if (ret != null) {
            return ret;
        }
        synchronized (MIME) {
            ret = CrossSystem.MIME.get();
            if (ret == null) {
                ret = MimeFactory.getInstance();
                setMime(ret);
            }
        }
        return ret;
    }

    public static Mime setMime(Mime mime) {
        if (mime == null) {
            throw new IllegalArgumentException();
        }
        return MIME.getAndSet(mime);
    }

    public static DesktopSupport getDesktopSupport() {
        DesktopSupport ret = CrossSystem.DESKTOP_SUPPORT.get();
        if (ret != null) {
            return ret;
        }
        synchronized (DESKTOP_SUPPORT) {
            ret = CrossSystem.DESKTOP_SUPPORT.get();
            if (ret == null) {
                switch (getOSFamily()) {
                case WINDOWS:
                    try {
                        if (JNAHelper.isJNAAvailable() && !BuildDecisions.contains(DesktopSupportWindowsViaJNA.DESKTOP_SUPPORT_WINDOWS_VIA_JNA_NO)) {
                            ret = new org.appwork.utils.os.DesktopSupportWindowsViaJNA();
                        } else {
                            ret = new DesktopSupportWindows();
                        }
                    } catch (final Exception e) {
                        DebugMode.debugger(e);
                        ret = new DesktopSupportWindows();
                    }
                    break;
                case LINUX:
                    ret = new DesktopSupportLinux();
                    break;
                case MAC:
                    ret = new DesktopSupportMac();
                    break;
                default:
                    ret = new DesktopSupportJavaDesktop();
                    break;
                }
                setDesktopSupportInstance(ret);
            }
        }
        return ret;
    }

    /**
     * @return
     */
    public static OperatingSystem getOS() {
        return CrossSystem.OS;
    }

    private static OperatingSystem getWindowsReleaseCMD(final String osName) {
        final Object initialValue = new Object();
        final AtomicReference<Object> reference = new AtomicReference<Object>(initialValue);
        final Thread thread = new Thread("getWindowsReleaseCMD: cmd -c ver") {
            private void set(final OperatingSystem operatingSystem) {
                synchronized (reference) {
                    reference.compareAndSet(initialValue, operatingSystem);
                    reference.notify();
                }
            }

            @Override
            public void run() {
                Process process = null;
                try {
                    process = new ProcessBuilder("cmd", "/c", "ver").start();
                    String buildNumberString = null;
                    try {
                        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
                        while (true) {
                            final String line = reader.readLine();
                            if (line == null) {
                                break;
                            }
                            buildNumberString = new Regex(line, "Microsoft\\s*Windows\\s*\\[Versi(?:ó|o)n\\s*\\d+\\.\\d+\\.(\\d+)").getMatch(0);
                            if (buildNumberString == null) {
                                buildNumberString = new Regex(line, "\\d+\\.\\d+\\.(\\d+)").getMatch(0);
                            }
                            if (buildNumberString != null) {
                                break;
                            }
                        }
                    } finally {
                        process.destroy();
                        process = null;
                    }
                    final int buildNumber = Integer.parseInt(buildNumberString);
                    // TODO: query is workstation
                    final boolean isServer = osName != null && osName.toLowerCase(Locale.ENGLISH).contains("server");
                    if (isServer) {
                        // https://learn.microsoft.com/en-us/windows/release-health/windows-server-release-info
                        if (buildNumber >= 26040 /* Preview */ || buildNumber >= 26100 /* GA */) {
                            this.set(OperatingSystem.WINDOWS_SERVER_2025);
                        } else if (buildNumber >= 20348) {
                            this.set(OperatingSystem.WINDOWS_SERVER_2022);
                        } else if (buildNumber >= 17763) {
                            this.set(OperatingSystem.WINDOWS_SERVER_2019);
                        } else if (buildNumber >= 14393) {
                            this.set(OperatingSystem.WINDOWS_SERVER_2016);
                        }
                        return;
                    }
                    // https://en.wikipedia.org/wiki/List_of_Microsoft_Windows_versions
                    // https://en.wikipedia.org/wiki/Windows_11_version_history
                    // https://betawiki.net/wiki/Windows_as_a_service
                    // https://ss64.com/nt/ver.html
                    if (buildNumber >= 28000) {
                        // https://blogs.windows.com/windows-insider/2025/11/07/announcing-windows-11-insider-preview-build-28000-canary-channel/
                        this.set(OperatingSystem.WINDOWS_11_26H1);
                    } else if (buildNumber >= 26200) {
                        this.set(OperatingSystem.WINDOWS_11_25H2);
                    } else if (buildNumber >= 26052 || buildNumber >= 26080 || buildNumber >= 26100) {
                        // https://blogs.windows.com/windows-insider/2024/02/08/announcing-windows-11-insider-preview-build-26052-canary-and-dev-channels/
                        // TODO: update buildNumber
                        this.set(OperatingSystem.WINDOWS_11_24H2);
                    } else if (buildNumber >= 22631) {
                        this.set(OperatingSystem.WINDOWS_11_23H2);
                    } else if (buildNumber >= 22621) {
                        this.set(OperatingSystem.WINDOWS_11_22H2);
                    } else if (buildNumber >= 22000) {
                        // return OperatingSystem.WINDOWS_11_21H2;
                        this.set(OperatingSystem.WINDOWS_11);
                    } else if (buildNumber >= 19045) {
                        this.set(OperatingSystem.WINDOWS_10_22H2);
                    } else if (buildNumber >= 19044) {
                        this.set(OperatingSystem.WINDOWS_10_21H2);
                    } else if (buildNumber >= 19043) {
                        this.set(OperatingSystem.WINDOWS_10_21H1);
                    } else if (buildNumber >= 19042) {
                        this.set(OperatingSystem.WINDOWS_10_20H2);
                    } else if (buildNumber >= 10240) {
                        this.set(OperatingSystem.WINDOWS_10);
                    } else if (buildNumber >= 9600) {
                        this.set(OperatingSystem.WINDOWS_8_1);
                    } else if (buildNumber >= 9200) {
                        this.set(OperatingSystem.WINDOWS_8);
                    } else if (buildNumber >= 7601) {
                        this.set(OperatingSystem.WINDOWS_7);
                    } else if (buildNumber >= 6002) {
                        this.set(OperatingSystem.WINDOWS_VISTA);
                    } else if (buildNumber >= 2600) {
                        this.set(OperatingSystem.WINDOWS_XP);
                    }
                } catch (final Throwable ignore) {
                    ignore.printStackTrace();
                } finally {
                    this.set(null);
                    if (process != null) {
                        try {
                            process.destroy();
                        } catch (final Throwable ignore2) {
                        }
                    }
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
        try {
            synchronized (reference) {
                if (reference.get() == initialValue && thread.isAlive()) {
                    reference.wait(1000);
                }
            }
        } catch (final InterruptedException ignore) {
        }
        final Object resultValue = reference.get();
        if (resultValue instanceof OperatingSystem) {
            return (OperatingSystem) resultValue;
        } else {
            return null;
        }
    }

    public static OperatingSystem getWindowsRelease(final String osName) {
        if (osName != null) {
            final boolean forceProbeCMD = true;
            final String os = osName.toLowerCase(Locale.ENGLISH);
            if (os.contains("windows 11")) {
                if (forceProbeCMD) {
                    final OperatingSystem ret = getWindowsReleaseCMD(os);
                    if (ret != null) {
                        return ret;
                    }
                }
                return OperatingSystem.WINDOWS_11;
            } else if (os.contains("windows 10")) {
                try {// see https://bugs.openjdk.org/browse/JDK-8274840
                    final long jvmVersion = JVMVersion.get();
                    final boolean trustFlag;
                    if (forceProbeCMD) {
                        trustFlag = false;
                    } else if (jvmVersion >= JVMVersion.JAVA_18) {
                        trustFlag = true;
                    } else if (jvmVersion >= JVMVersion.JAVA_17) {
                        trustFlag = JVMVersion.isMinimum(JVMVersion.parseJavaVersionString("17.0.2"));
                    } else if (jvmVersion >= JVMVersion.JAVA_11 && jvmVersion < JVMVersion.JAVA_12) {
                        trustFlag = JVMVersion.isMinimum(JVMVersion.parseJavaVersionString("11.0.14"));
                    } else if (jvmVersion >= JVMVersion.JAVA_1_8 && jvmVersion < JVMVersion.JAVA_9) {
                        trustFlag = JVMVersion.isMinimum(JVMVersion.parseJavaVersionString("8u331"));
                    } else if (jvmVersion >= JVMVersion.JAVA_1_7 && jvmVersion < JVMVersion.JAVA_1_8) {
                        trustFlag = JVMVersion.isMinimum(JVMVersion.parseJavaVersionString("7u331"));
                    } else {
                        trustFlag = false;
                    }
                    if (!trustFlag) {
                        final OperatingSystem ret = getWindowsReleaseCMD(os);
                        if (ret != null) {
                            return ret;
                        }
                    }
                } catch (final Throwable ignore) {
                    ignore.printStackTrace();
                }
                return OperatingSystem.WINDOWS_10;
            } else if (os.contains("windows 8")) {
                if (forceProbeCMD) {
                    final OperatingSystem ret = getWindowsReleaseCMD(os);
                    if (ret != null) {
                        return ret;
                    }
                }
                if (os.contains("8.1")) {
                    return OperatingSystem.WINDOWS_8_1;
                } else {
                    return OperatingSystem.WINDOWS_8;
                }
            } else if (os.contains("windows 7")) {
                return OperatingSystem.WINDOWS_7;
            } else if (os.contains("windows xp")) {
                return OperatingSystem.WINDOWS_XP;
            } else if (os.contains("windows vista")) {
                return OperatingSystem.WINDOWS_VISTA;
            } else if (os.contains("windows 2000")) {
                return OperatingSystem.WINDOWS_2000;
            } else if (os.contains("windows 2003")) {
                return OperatingSystem.WINDOWS_2003;
            } else if (os.contains("windows server 2003")) {
                return OperatingSystem.WINDOWS_SERVER_2003;
            } else if (os.contains("windows server 2008")) {
                if (os.contains("r2")) {
                    return OperatingSystem.WINDOWS_SERVER_2008_R2;
                } else {
                    return OperatingSystem.WINDOWS_SERVER_2008;
                }
            } else if (os.contains("windows server 2012")) {
                if (os.contains("r2")) {
                    return OperatingSystem.WINDOWS_SERVER_2012_R2;
                } else {
                    return OperatingSystem.WINDOWS_SERVER_2012;
                }
            } else if (os.contains("windows server 2016")) {
                return OperatingSystem.WINDOWS_SERVER_2016;
            } else if (os.contains("windows server 2019")) {
                return OperatingSystem.WINDOWS_SERVER_2019;
            } else if (os.contains("windows server 2020")) {
                return OperatingSystem.WINDOWS_SERVER_2020;
            } else if (os.contains("windows server 2022")) {
                return OperatingSystem.WINDOWS_SERVER_2022;
            } else if (os.contains("windows server 2025")) {
                return OperatingSystem.WINDOWS_SERVER_2025;
            } else if (os.contains("nt")) {
                return OperatingSystem.WINDOWS_NT;
            } else if (os.contains("windows")) {
                return OperatingSystem.WINDOWS_OTHERS;
            }
        }
        return null;
    }

    public static OperatingSystem getMacOSRelease(final String osName, final String osVersion) {
        if (osName != null) {
            final String os = osName.toLowerCase(Locale.ENGLISH);
            if (os.contains("mac") || os.contains("darwin")) {
                final AtomicReference<String> sw_ver = new AtomicReference<String>();
                final Thread thread = new Thread("getMacOSRelease:sw_ver") {
                    @Override
                    public void run() {
                        final File binary = new File("/usr/bin/sw_vers");
                        if (binary.exists()) {
                            Process p = null;
                            try {
                                final Runtime r = Runtime.getRuntime();
                                p = r.exec(new String[] { binary.getAbsolutePath(), "-productVersion" });
                                p.waitFor();
                                final BufferedReader b = new BufferedReader(new InputStreamReader(p.getInputStream()));
                                while (true) {
                                    final String line = b.readLine();
                                    if (line == null) {
                                        break;
                                    } else if (line.trim().matches("^[0-9\\.]+$")) {
                                        System.out.println("MacOS: detected sw_vers:" + line);
                                        sw_ver.set(line.trim());
                                    }
                                }
                            } catch (final Throwable e) {
                                e.printStackTrace();
                            } finally {
                                try {
                                    if (p != null) {
                                        p.destroy();
                                    }
                                } catch (final Throwable e2) {
                                }
                            }
                        }
                    }
                };
                thread.start();
                try {
                    thread.join(5000);
                } catch (final InterruptedException ignore) {
                }
                final long version = Math.max(parseMacOSVersion(osVersion), parseMacOSVersion(sw_ver.get()));
                // new version scheme
                if (version >= 26000000) {
                    return OperatingSystem.MAC_TAHOE;
                } else if (version >= 15000000) {
                    return OperatingSystem.MAC_SEQUOIA;
                } else if (version >= 14000000) {
                    return OperatingSystem.MAC_SONOMA;
                } else if (version >= 13000000) {
                    return OperatingSystem.MAC_VENTURA;
                } else if (version >= 12000000) {
                    return OperatingSystem.MAC_MONTEREY;
                } else if (version >= 11000000) {
                    return OperatingSystem.MAC_BIG_SUR;
                } else {
                    // old version scheme
                    if (version >= 10021000 || version >= 10026000) {
                        // TODO: unconfirmed
                        return OperatingSystem.MAC_TAHOE;
                    } else if (version >= 10020000) {
                        // os.version=10.20
                        return OperatingSystem.MAC_SEQUOIA;
                    } else if (version >= 10019000) {
                        // os.version=10.19
                        return OperatingSystem.MAC_SONOMA;
                    } else if (version >= 10018000) {
                        // os.version=10.18
                        return OperatingSystem.MAC_VENTURA;
                    } else if (version >= 10017000) {
                        // os.version=10.17
                        return OperatingSystem.MAC_MONTEREY;
                    } else if (version >= 10016000) {
                        // os.version=10.16
                        return OperatingSystem.MAC_BIG_SUR;
                    } else if (version >= 10015000) {
                        return OperatingSystem.MAC_CATALINA;
                    } else if (version >= 10014000) {
                        return OperatingSystem.MAC_MOJAVE;
                    } else if (version >= 10013000) {
                        return OperatingSystem.MAC_HIGH_SIERRA;
                    } else if (version >= 10012000) {
                        return OperatingSystem.MAC_SIERRA;
                    } else if (version >= 10011000) {
                        return OperatingSystem.MAC_EL_CAPITAN;
                    } else if (version >= 10010000) {
                        return OperatingSystem.MAC_YOSEMITE;
                    } else if (version >= 10009000) {
                        return OperatingSystem.MAC_MAVERICKS;
                    } else if (version >= 10008000) {
                        return OperatingSystem.MAC_MOUNTAIN_LION;
                    } else if (version >= 10007000) {
                        return OperatingSystem.MAC_LION;
                    } else if (version >= 10006000) {
                        return OperatingSystem.MAC_SNOW_LEOPOARD;
                    } else if (version >= 10005000) {
                        return OperatingSystem.MAC_LEOPOARD;
                    } else if (version >= 10004000) {
                        return OperatingSystem.MAC_TIGER;
                    } else if (version >= 10003000) {
                        return OperatingSystem.MAC_PANTHER;
                    } else if (version >= 10002000) {
                        return OperatingSystem.MAC_JAGUAR;
                    } else if (version >= 10001000) {
                        return OperatingSystem.MAC_PUMA;
                    } else if (version >= 10000000) {
                        return OperatingSystem.MAC_CHEETAH;
                    } else {
                        return OperatingSystem.MAC;
                    }
                }
            }
        }
        return null;
    }

    public static OperatingSystem getBSDRelease(final String osName) {
        if (osName != null) {
            final String os = osName.toLowerCase(Locale.ENGLISH);
            if (os.contains("bsd")) {
                if (os.contains("kfreebsd")) {
                    return OperatingSystem.KFREEBSD;
                } else if (os.contains("freebsd")) {
                    return OperatingSystem.FREEBSD;
                } else if (os.contains("netbsd")) {
                    return OperatingSystem.NETBSD;
                } else if (os.contains("openbsd")) {
                    return OperatingSystem.OPENBSD;
                } else if (os.contains("dragonflybsd")) {
                    return OperatingSystem.DRAGONFLYBSD;
                } else {
                    return OperatingSystem.BSD;
                }
            }
        }
        return null;
    }

    private static OperatingSystem getLinuxRelease(final OperatingSystem base, final String release) {
        OperatingSystem best = null;
        for (final OperatingSystem os : OperatingSystems) {
            if (base.sameOSFamily(os) && os.isRelease(release)) {
                best = os;
            }
        }
        return best;
    }

    public static OperatingSystem getLinuxReleaseByProcVersion() {
        final File procVersion = new File("/proc/version");
        if (!procVersion.exists()) {
            return null;
        }
        try {
            final FileInputStream fis = new FileInputStream(procVersion);
            try {
                final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                try {
                    String line = null;
                    while ((line = is.readLine()) != null) {
                        final OperatingSystem ret = parseReleaseByKernelString(line);
                        if (ret != null) {
                            return ret;
                        }
                    }
                } finally {
                    is.close();
                }
            } finally {
                fis.close();
            }
        } catch (final Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        return null;
    }

    public static OperatingSystem parseReleaseByKernelString(String line) {
        if (line == null) {
            return null;
        }
        final class debianReleaseName {
            private String getReleaseName(int release) {
                switch (release) {
                case 6:
                    return "squeeze";
                case 7:
                    return "wheezy";
                case 8:
                    return "jessie";
                case 9:
                    return "stretch";
                case 10:
                    return "buster";
                case 11:
                    return "bull";
                case 12:
                    return "bookworm";
                case 13:
                    return "trixie";
                case 14:
                    return "forky";
                default:
                    return null;
                }
            }
        }
        line = line.toLowerCase(Locale.ENGLISH);
        OperatingSystem ret = null;
        {
            if (line.contains("freebsd")) {
                return OperatingSystem.FREEBSD;
            } else if (line.contains("bazzite")) {
                return OperatingSystem.BAZZITE;
            } else if (line.contains("alpine")) {
                return OperatingSystem.ALPINE;
            }
        }
        UBUNTU: {
            if (line.contains("ubuntu")) {
                ret = OperatingSystem.UBUNTU;
                final String version = new Regex(line, "ubuntu[^~]*~([0-9\\.]+)").getMatch(0);
                final OperatingSystem release = getLinuxRelease(ret, version);
                if (release != null) {
                    return release;
                }
            }
        }
        FEDORA: {
            if (line.contains(".fc") && line.matches(".*\\.fc\\d+.*")) {
                return OperatingSystem.FEDORA;
            } else if (line.contains("fedoraproject.org")) {
                return OperatingSystem.FEDORA;
            } else if (line.contains("fedora")) {
                return OperatingSystem.FEDORA;
            }
        }
        REDHAT_CENTOS: {
            if (line.contains("redhat.com")) {
                return OperatingSystem.REDHAT;
            } else if (line.contains("centos.org") || line.contains("centos")) {
                return OperatingSystem.CENTOS;
            } else if (line.contains(".el") && line.matches(".*\\.el\\d+.*")) {
                // can also be OperatingSystem.CENTOS;
                return OperatingSystem.REDHAT;
            }
        }
        RASPBIAN: {
            if (line.contains("rpt")) {
                ret = OperatingSystem.RASPBIAN;
                {
                    final String version = new Regex(line, "rpt[^~]*~([a-z]+)").getMatch(0);
                    final OperatingSystem release = getLinuxRelease(ret, version);
                    if (release != null) {
                        return release;
                    }
                }
                final String debian = new Regex(line, "Debian\\s*(\\d+)").getMatch(0);
                if (debian != null) {
                    final String version = new debianReleaseName().getReleaseName(Integer.parseInt(debian));
                    final OperatingSystem release = getLinuxRelease(ret, version);
                    if (release != null) {
                        return release;
                    }
                }
                if (line.contains("rpt-rpi")) {
                    final OperatingSystem release = getLinuxRelease(ret, line);
                    if (release != null) {
                        return release;
                    }
                    return ret;
                }
            }
            if (line.contains("raspberrypi.com")) {
                return OperatingSystem.RASPBIAN;
            } else if (line.contains("raspberrypi")) {
                return OperatingSystem.RASPBIAN;
            }
        }
        ARCH: {
            if (line.contains("arch1-")) {
                return OperatingSystem.ARCH;
            } else if (line.contains("archlinux")) {
                return OperatingSystem.ARCH;
            }
        }
        debian: {
            if (ret == null && (line.contains("debian") || line.contains("debian.org"))) {
                ret = OperatingSystem.DEBIAN;
                final OperatingSystem release = getLinuxRelease(ret, line);
                if (release != null) {
                    return release;
                }
            }
            final OperatingSystem release = getLinuxRelease(OperatingSystem.DEBIAN, line);
            if (release != null) {
                return release;
            }
        }
        return ret;
    }

    /*
     * https://gitlab.com/zygoon/os-release-zoo
     */
    public static OperatingSystem getLinuxRelease(final String osName) {
        OperatingSystem ret = null;
        if (osName != null && osName.toLowerCase(Locale.ENGLISH).contains("linux")) {
            final List<String> sources = new ArrayList<String>(Arrays.asList("/etc/os-release", "/etc/issue", "/usr/lib/os-release"));
            if (Flatpak.isInsideFlatpak()) {
                // https://docs.flatpak.org/en/latest/flatpak-command-reference.html
                // Flatpak also bind-mounts as read-only the host's /etc/os-release (if available, or /usr/lib/os-release as a fallback) to
                // /run/host/os-release
                sources.add(0, "/run/host/os-release");
            }
            for (final String source : sources) {
                final File issue = new File(source);
                if (issue.isFile()) {
                    try {
                        final FileInputStream fis = new FileInputStream(issue);
                        try {
                            final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                            try {
                                String line = null;
                                while ((line = is.readLine()) != null) {
                                    line = line.toLowerCase(Locale.ENGLISH);
                                    if (line.contains("debian") || OperatingSystem.DEBIAN.equals(ret)) {
                                        ret = OperatingSystem.DEBIAN;
                                        final OperatingSystem release = getLinuxRelease(ret, line);
                                        if (release != null) {
                                            return release;
                                        }
                                    } else if (line.contains("raspbian") || OperatingSystem.RASPBIAN.equals(ret)) {
                                        ret = OperatingSystem.RASPBIAN;
                                        final OperatingSystem release = getLinuxRelease(ret, line);
                                        if (release != null) {
                                            return release;
                                        }
                                    } else if (line.contains("ubuntu") || OperatingSystem.UBUNTU.equals(ret)) {
                                        ret = OperatingSystem.UBUNTU;
                                        final OperatingSystem release = getLinuxRelease(ret, line);
                                        if (release != null) {
                                            return release;
                                        }
                                    } else if (line.contains("kali") || line.contains("kali gnu/linux") || OperatingSystem.KALILINUX.equals(ret)) {
                                        ret = OperatingSystem.KALILINUX;
                                        final OperatingSystem release = getLinuxRelease(ret, line);
                                        if (release != null) {
                                            return release;
                                        }
                                    } else if (ret == null) {
                                        if (line.contains("rocky linux") || line.contains("rocky-linux")) {
                                            return OperatingSystem.ROCKYLINUX;
                                        } else if (line.contains("almalinux")) {
                                            return OperatingSystem.ALMALINUX;
                                        } else if (line.contains("mageia")) {
                                            return OperatingSystem.MAGEIA;
                                        } else if (line.contains("nixos")) {
                                            return OperatingSystem.NIXOS;
                                        } else if (line.contains("elementary")) {
                                            return OperatingSystem.ELEMENTARYOS;
                                        } else if (line.contains("endeavour")) {
                                            return OperatingSystem.ENDEAVOUROS;
                                        } else if (line.contains("manjaro")) {
                                            return OperatingSystem.MANJARO;
                                        } else if (line.contains("alpine")) {
                                            return OperatingSystem.ALPINE;
                                        } else if (line.contains("sles")) {
                                            return OperatingSystem.SLES;
                                        } else if (line.contains("red hat") || line.contains("rhel")) {
                                            return OperatingSystem.REDHAT;
                                        } else if (line.contains("gentoo")) {
                                            return OperatingSystem.GENTOO;
                                        } else if (line.contains("arch linux")) {
                                            return OperatingSystem.ARCH;
                                        } else if (line.contains("slackware")) {
                                            return OperatingSystem.SLACKWARE;
                                        } else if (line.contains("opensuse")) {
                                            ret = OperatingSystem.OPENSUSE;
                                            final OperatingSystem release = getLinuxRelease(ret, line);
                                            if (release != null) {
                                                return release;
                                            }
                                        } else if (line.contains("centos")) {
                                            if (line.contains("centos stream")) {
                                                return OperatingSystem.CENTOS_STREAM;
                                            } else {
                                                return OperatingSystem.CENTOS;
                                            }
                                        } else if (line.contains("fedora")) {
                                            return OperatingSystem.FEDORA;
                                        } else if (line.contains("bazzite")) {
                                            return OperatingSystem.BAZZITE;
                                        }
                                    }
                                }
                            } finally {
                                is.close();
                            }
                        } finally {
                            fis.close();
                        }
                    } catch (final Throwable e) {
                        org.appwork.loggingv3.LogV3.log(e);
                    }
                }
            }
        }
        return ret;
    }

    /**
     * @param osString
     * @return
     */
    public static OperatingSystem getOSByString(final String osString) {
        if (osString != null) {
            final String os = osString.toLowerCase(Locale.ENGLISH);
            OperatingSystem ret = null;
            if (ret == null && (os.contains("windows") || os.contains("nt"))) {
                ret = getWindowsRelease(os);
            }
            if (ret == null && (os.contains("mac") || os.contains("darwin"))) {
                ret = getMacOSRelease(os, System.getProperty("os.version"));
            }
            if (ret == null && os.contains("bsd")) {
                ret = getBSDRelease(os);
            }
            if (ret == null && os.contains("os/2")) {
                ret = OperatingSystem.OS2;
            }
            if (ret == null) {
                ret = getLinuxRelease(os);
                if (ret == null || ret == OperatingSystem.LINUX) {
                    final OperatingSystem byOsVersion = parseReleaseByKernelString(System.getProperty("os.version"));
                    final OperatingSystem byProcVersion = getLinuxReleaseByProcVersion();
                    if (byOsVersion != null && byProcVersion != null) {
                        if (byOsVersion.name().length() > byProcVersion.name().length()) {
                            return byOsVersion;
                        }
                        return byProcVersion;
                    } else if (byOsVersion != null) {
                        return byOsVersion;
                    } else if (byProcVersion != null) {
                        return byProcVersion;
                    }
                }
            }
            if (ret != null) {
                return ret;
            } else {
                // Fallback
                return OperatingSystem.LINUX;
            }
        }
        return OperatingSystem.WINDOWS_8;
    }

    private static ARCHFamily getARCHByString(final String archString) {
        if (archString != null) {
            final String arch = archString.toLowerCase(Locale.ENGLISH);
            if (arch.contains("i386") || arch.contains("i486") || arch.contains("i586") || arch.contains("i686") || arch.contains("x86") || arch.contains("amd64")) {
                return ARCHFamily.X86;
            } else if (arch.contains("ppc") || arch.contains("powerpc") || arch.contains("ppc64") || arch.contains("ppc64le")) {
                return ARCHFamily.PPC;
            } else if (arch.contains("mips")) {
                return ARCHFamily.MIPS;
            } else if (arch.contains("sparc")) {
                return ARCHFamily.SPARC;
            } else if (arch.contains("arm") || arch.contains("aarch") || arch.contains("aarch64")) {
                return ARCHFamily.ARM;
            } else if (arch.contains("ia64")) {
                return ARCHFamily.IA64;
            } else if (arch.contains("riscv") || arch.contains("riscv64")) {
                return ARCHFamily.RISCV;
            } else if (arch.contains("loongarch") || arch.contains("loong") || arch.contains("loongarch64")) {
                // loongarch64 and loong64
                return ARCHFamily.LOONGARCH;
            }
        }
        return ARCHFamily.NA;
    }

    /**
     * Returns true if the OS is a linux system
     *
     * @return
     */
    public static OSFamily getOSFamily() {
        return CrossSystem.OS.getFamily();
    }

    public static ARCHFamily getARCHFamily() {
        return CrossSystem.ARCH;
    }

    public static String getARCHString() {
        return CrossSystem.ARCH_STRING;
    }

    public static String getOSString() {
        return CrossSystem.OS_STRING;
    }

    public static String[] getPathComponents(File path) throws IOException {
        final LinkedList<String> ret = new LinkedList<String>();
        if (path != null) {
            /*
             * getCanonicalFile once, so we are sure all .././symlinks are evaluated
             */
            try {
                if (!CrossSystem.isForbiddenFilename(path.getName())) {
                    path = path.getCanonicalFile();
                }
            } catch (final IOException e) {
                /**
                 * can happen when drive is not mounted, no cd in drive...
                 */
                e.printStackTrace();
            }
            final String separator = File.separatorChar + "";
            while (path != null) {
                if (path.getPath().endsWith(separator)) {
                    // for example c:\ file.getName() would be "" in this case.
                    ret.add(0, path.getPath());
                    break;
                } else {
                    ret.add(0, path.getName());
                }
                path = path.getParentFile();
            }
        }
        return ret.toArray(new String[] {});
    }

    public static double getSystemCPUUsage() {
        try {
            final java.lang.management.OperatingSystemMXBean operatingSystemMXBean = java.lang.management.ManagementFactory.getOperatingSystemMXBean();
            double sysload = operatingSystemMXBean.getSystemLoadAverage();
            if (sysload < 0) {
                final java.lang.reflect.Method method = operatingSystemMXBean.getClass().getDeclaredMethod("getSystemCpuLoad", new Class[] {});
                method.setAccessible(true);
                sysload = (Double) method.invoke(operatingSystemMXBean, new Object[] {});
            }
            return sysload;
        } catch (final Throwable e) {
            return -1;
        }
    }

    public static long getPID() {
        final RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
        try {
            final String jvmName = runtimeMXBean.getName();
            final int index = jvmName.indexOf('@');
            /**
             * http://www.golesny.de/p/code/javagetpid
             *
             * @return
             */
            if (index >= 1) {
                return Long.parseLong(jvmName.substring(0, index), 10);
            }
        } catch (final Throwable e) {
        }
        try {
            /**
             * http://blog.philippheckel.com/2014/06/14/getting-the-java-process -pid-and-managing-pid-files-linux-windows/
             */
            final Field jvmField = runtimeMXBean.getClass().getDeclaredField("jvm");
            jvmField.setAccessible(true);
            final Object vmManagement = jvmField.get(runtimeMXBean);
            final Method getProcessIdMethod = vmManagement.getClass().getDeclaredMethod("getProcessId");
            getProcessIdMethod.setAccessible(true);
            return ((Number) getProcessIdMethod.invoke(vmManagement)).longValue();
        } catch (final Throwable e) {
        }
        return -1;
    }

    public static String NEWLINE = null;

    public static String getNewLine() {
        if (NEWLINE == null) {
            String newLine = null;
            try {
                if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
                    newLine = System.lineSeparator();
                }
            } catch (final Throwable e) {
            }
            if (StringUtils.isEmpty(newLine)) {
                newLine = System.getProperty("line.separator");
            }
            if (StringUtils.isEmpty(newLine)) {
                switch (CrossSystem.getOSFamily()) {
                case WINDOWS:
                    newLine = "\r\n";
                    break;
                default:
                    newLine = "\n";
                    break;
                }
            }
            NEWLINE = newLine;
            return newLine;
        }
        return NEWLINE;
    }

    @Deprecated
    // this should not be used from extern - it's a helper method to find correct values for CrossSystem.is64BitOperatingSystem and
    // Application.is64BitJVM...
    public static boolean is64BitArch() {
        if (CrossSystem.isWindows()) {
            final String wow64Arch = System.getenv("PROCESSOR_ARCHITEW6432");
            if (wow64Arch != null) {
                // cpu architecture
                // null = 32Bit
                // AMD64 = 64Bit
                return wow64Arch.trim().endsWith("64");
            }
            final String arch = System.getenv("PROCESSOR_ARCHITECTURE");
            if (arch != null) {
                // process architecture
                // x86 = 32Bit
                // AMD64 = 64Bit
                return arch.trim().endsWith("64");
            }
        }
        final String osArch = System.getProperty("os.arch");
        if (osArch != null) {
            // 32Bit JVM on 64Bit OS will still return 32Bit
            final Boolean result = is64BitARCH(osArch, false);
            if (result != null) {
                return result.booleanValue();
            } else {
                return false;
            }
        } else {
            // no os.arch?!
            return false;
        }
    }

    public static Boolean is64BitARCH(String arch, final boolean relaxed) {
        if (arch != null) {
            arch = arch.toLowerCase(Locale.ENGLISH);
            if ((relaxed && (arch.contains("amd64") || arch.contains("amd_64") || arch.contains("x86_64"))) || (arch.equals("amd64") || arch.equals("amd_64") || arch.equals("x86_64"))) {
                // ARCHFamily.X86
                return true;
            } else if ((relaxed && (arch.contains("aarch64") || arch.contains("arm64"))) || (arch.equals("aarch64") || arch.equals("arm64"))) {
                // ARCHFamily.ARM
                return true;
            } else if ((relaxed && (arch.contains("riscv64"))) || (arch.equals("riscv64"))) {
                // ARCHFamily.RISCV
                return true;
            } else if ((relaxed && (arch.contains("sparcv9"))) || (arch.equals("sparcv9"))) {
                // ARCHFamily.SPARC
                return true;
            } else if ((relaxed && (arch.contains("mipsel"))) || (arch.equals("mipsel"))) {
                // ARCHFamily.MIPS
                return true;
            } else if ((relaxed && (arch.contains("ppc64"))) || (arch.equals("ppc64"))) {
                // ARCHFamily.PPC
                return true;
            } else if ((relaxed && (arch.contains("ia64"))) || (arch.equals("ia64"))) {
                // ARCHFamily.IA64
                return true;
            } else if ((relaxed && (arch.contains("loongarch64") || arch.contains("loong64"))) || (arch.equals("loongarch64") || arch.equals("loong64"))) {
                // ARCHFamily.LOONGARCH
                return true;
            }
            if ((relaxed && (arch.contains("riscv32"))) || (arch.equals("riscv32"))) {
                // ARCHFamily.RISCV
                return false;
            } else if (!relaxed && ("i386".equals(arch) || arch.equals("i486") || arch.equals("i586") || arch.equals("i686") || arch.equals("x86"))) {
                // ARCHFamily.X86
                return false;
            } else if (!relaxed && "sparc".equals(arch)) {
                // ARCHFamily.SPARC
                return false;
            }
        }
        return null;
    }

    public static boolean is64BitOperatingSystem() {
        if (CrossSystem.OS64BIT != null) {
            return CrossSystem.OS64BIT;
        }
        if (org.appwork.utils.Application.is64BitJvm()) {
            /*
             * we are running a 64bit jvm, so the underlying os must be 64bit too
             */
            CrossSystem.OS64BIT = true;
            return true;
        } else {
            switch (CrossSystem.getOSFamily()) {
            case BSD:
            case LINUX:
                if (CrossSystem.is64BitArch()) {
                    CrossSystem.OS64BIT = true;
                    return true;
                }
                final String hostType = System.getenv("HOSTTYPE");
                if (hostType != null && Boolean.TRUE.equals(is64BitARCH(hostType, false))) {
                    CrossSystem.OS64BIT = true;
                    return true;
                }
                Process p = null;
                try {
                    final Runtime r = Runtime.getRuntime();
                    p = r.exec("uname -m");
                    p.waitFor();
                    final BufferedReader b = new BufferedReader(new InputStreamReader(p.getInputStream()));
                    final String line = b.readLine();
                    if (line != null && Boolean.TRUE.equals(is64BitARCH(line, false))) {
                        CrossSystem.OS64BIT = true;
                        return true;
                    }
                } catch (final Throwable e) {
                } finally {
                    try {
                        if (p != null) {
                            p.destroy();
                        }
                    } catch (final Throwable e2) {
                    }
                }
                break;
            case WINDOWS:
                if (System.getenv("ProgramFiles(x86)") != null || System.getenv("ProgramW6432") != null) {
                    /* those folders also exist on newer 32bit os */
                    if (CrossSystem.is64BitArch()) {
                        CrossSystem.OS64BIT = true;
                        return true;
                    }
                }
                break;
            default:
                if (CrossSystem.is64BitArch()) {
                    CrossSystem.OS64BIT = true;
                    return true;
                }
                break;
            }
        }
        CrossSystem.OS64BIT = false;
        return false;
    }

    /**
     * checks if given path is absolute or relative
     *
     * @param path
     * @return
     */
    public static boolean isAbsolutePath(final String path) {
        if (StringUtils.isEmpty(path)) {
            return false;
        } else if ((CrossSystem.isWindows() || CrossSystem.isOS2()) && path.matches("\\\\\\\\.+\\\\.+")) {
            return true;
        } else if ((CrossSystem.isWindows() || CrossSystem.isOS2()) && path.matches("[a-zA-Z]:/.*")) {
            return true;
        } else if ((CrossSystem.isWindows() || CrossSystem.isOS2()) && path.matches("[a-zA-Z]:\\\\.*")) {
            return true;
        } else if (!CrossSystem.isWindows() && !CrossSystem.isOS2() && path.startsWith("/")) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean isClearSelectionTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        }
    }

    public static boolean isContextMenuTrigger(final MouseEvent e) {
        if (CrossSystem.isMac()) {
            if (e.getButton() == MouseEvent.BUTTON1 && e.isControlDown()) {
                return true;
            }
        }
        return e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3;
    }

    public static boolean isDeleteFinalSelectionTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else if (CrossSystem.isMac()) {
            if (ks == KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask() | ActionEvent.SHIFT_MASK)) {
                return true;
            }
        }
        return ks == KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, ActionEvent.SHIFT_MASK);
    }

    public static boolean isDeleteSelectionTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else if (CrossSystem.isMac()) {
            if (ks == KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())) {
                return true;
            }
        }
        return ks == KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0);
    }

    public static boolean isDeleteSelectionTrigger(final KeyEvent e) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return CrossSystem.isDeleteSelectionTrigger(KeyStroke.getKeyStroke(e.getKeyCode(), e.getModifiers()));
        }
    }

    public static boolean isLinux() {
        final OperatingSystem os = CrossSystem.OS;
        if (os != null) {
            return OSFamily.LINUX.equals(os.getFamily());
        }
        return OS_STRING != null && OS_STRING.toLowerCase(Locale.ROOT).contains("linux");
    }

    public static boolean isBSD() {
        final OperatingSystem os = CrossSystem.OS;
        if (os != null) {
            return OSFamily.BSD.equals(os.getFamily());
        }
        return OS_STRING != null && OS_STRING.toLowerCase(Locale.ROOT).contains("bsd");
    }

    /**
     * Returns true if the OS is a MAC System
     *
     * @return
     */
    public static boolean isMac() {
        final OperatingSystem os = CrossSystem.OS;
        if (os != null) {
            return OSFamily.MAC.equals(os.getFamily());
        }
        return OS_STRING != null && (OS_STRING.toLowerCase(Locale.ROOT).contains("mac") || OS_STRING.toLowerCase(Locale.ROOT).contains("darwin"));
    }

    /**
     * returns true in case of "open an URL in a browser" is supported
     *
     * @return
     */
    public static boolean isOpenBrowserSupported() {
        return CrossSystem.getDesktopSupport().isBrowseURLSupported() || (CrossSystem.getBrowserCommandLine() != null && CrossSystem.getBrowserCommandLine().length > 0);
    }

    /**
     * returns true in case of "open a File" is supported
     *
     * @return
     */
    public static boolean isOpenFileSupported() {
        return CrossSystem.getDesktopSupport().isOpenFileSupported();
    }

    public static boolean isOS2() {
        final OperatingSystem os = CrossSystem.OS;
        if (os != null) {
            return OSFamily.OS2.equals(os.getFamily());
        }
        return OS_STRING != null && OS_STRING.toLowerCase(Locale.ROOT).contains("os/2");
    }

    public static boolean isCopySelectionTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
        }
    }

    public static boolean isCutSelectionTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_X, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
        }
    }

    public static boolean isPasteSelectionTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_V, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
        }
    }

    public static boolean isSearchTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
        }
    }

    public static boolean isSelectionAllTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_A, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
        }
    }

    public static boolean isSelectionDownTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0);
        }
    }

    public static boolean isSelectionUpTrigger(final KeyStroke ks) {
        if (GraphicsEnvironment.isHeadless()) {
            return false;
        } else {
            return ks == KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0);
        }
    }

    /**
     * Returns true if the OS is a Windows System
     *
     * @return
     */
    public static boolean isWindows() {
        final OperatingSystem os = CrossSystem.OS;
        if (os != null) {
            return OSFamily.WINDOWS.equals(os.getFamily());
        }
        return OS_STRING != null && (OS_STRING.toLowerCase(Locale.ROOT).contains("windows") || OS_STRING.toLowerCase(Locale.ROOT).contains("nt"));
    }

    protected static boolean openCustom(String[] commandLine, final String url) throws IOException {
        commandLine = buildBrowserCommandline(commandLine, url);
        if (commandLine != null && commandLine.length > 0) {
            Runtime.getRuntime().exec(commandLine);
            return true;
        } else {
            return false;
        }
    }

    public static String[] buildBrowserCommandline(final String[] commandLine, final String url) {
        if (commandLine == null || commandLine.length == 0 || url == null || url.trim().length() == 0) {
            return null;
        } else {
            boolean urlParam = false;
            final List<String> ret = new ArrayList<String>();
            for (final String arg : commandLine) {
                if (arg != null) {
                    if (arg.contains("%s")) {
                        urlParam = true;
                        ret.add(arg.replace("%s", url));
                    } else {
                        ret.add(arg);
                    }
                }
            }
            if (ret.size() > 0) {
                if (urlParam == false) {
                    ret.add(url);
                }
                return ret.toArray(new String[0]);
            } else {
                return null;
            }
        }
    }

    /**
     * Opens a file or directory
     *
     * @see java.awt.Desktop#open(File)
     * @param file
     * @throws IOException
     */
    public static void openFile(final File file) {
        openFile(file, false);
    }

    public static void openFile(final File file, final boolean trySingleInstance) {
        // I noticed a bug: desktop.open freezes under win7 java 1.7u25 in some
        // cases... we should at least avoid a gui freeze in such cases..
        final Runnable runnable = new Runnable() {
            @Override
            public void run() {
                try {
                    CrossSystem._openFILE(file, trySingleInstance);
                } catch (final IOException e) {
                    org.appwork.loggingv3.LogV3.log(e);
                }
            }
        };
        if (CrossSystem.isWindows()) {
            new Thread(runnable, "Open Folder").start();
        } else {
            runnable.run();
        }
    }

    /**
     * Open an url in the systems default browser
     *
     * @param url
     */
    public static Throwable openURL(final String url) {
        try {
            CrossSystem.openUrlOrThrowException(url);
            return null;
        } catch (final Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
            return e;
        }
    }

    public static void openURL(final URL url) {
        CrossSystem.openURL(url.toString());
    }

    /**
     * Set commandline to open the browser use %s as wildcard for the url
     *
     * @param commands
     */
    public static void setBrowserCommandLine(final String[] commands) {
        CrossSystem.BROWSER_COMMANDLINE = commands;
    }

    public static void setFileCommandLine(final String[] fILE_COMMANDLINE) {
        CrossSystem.FILE_COMMANDLINE = fILE_COMMANDLINE;
    }

    @Deprecated
    public static void showInExplorer(final File saveTo) {
        showInExplorer(saveTo, false);
    }

    /**
     * @param saveTo
     */
    public static void showInExplorer(final File saveTo, boolean useExitingWindow) {
        if (saveTo.exists()) {
            if (CrossSystem.isWindows()) {
                if (useExitingWindow) {
                    File openFolder = saveTo.getParentFile();
                    if (WindowsUtils.explorerToFront(openFolder)) {
                        return;
                    }
                }
                try {
                    // we need to go this cmd /c way, because explorer.exe seems to
                    // do some strange parameter parsing.
                    new ProcessBuilder("cmd", "/c", "explorer /select,\"" + saveTo.getAbsolutePath() + "\"").start();
                    return;
                } catch (final IOException e) {
                    e.printStackTrace();
                }
            } else if (CrossSystem.isMac()) {
                try {
                    ProcessBuilderFactory.create("open", "-R", saveTo.getAbsolutePath()).start();
                } catch (final IOException e) {
                    e.printStackTrace();
                }
            }
        }
        if (saveTo.isDirectory()) {
            CrossSystem.openFile(saveTo);
        } else {
            CrossSystem.openFile(saveTo.getParentFile());
        }
    }

    /**
     * splits filename into name,extension
     *
     * @param filename
     * @return
     */
    public static String[] splitFileName(final String filename) {
        final String extension = new Regex(filename, "\\.+([^\\.]*$)").getMatch(0);
        final String name = new Regex(filename, "(.*?)(\\.+[^\\.]*$|$)").getMatch(0);
        return new String[] { name, extension };
    }

    public static void standbySystem() throws InterruptedException {
        CrossSystem.getDesktopSupport().standby();
    }

    public static void hibernateSystem() throws InterruptedException {
        CrossSystem.getDesktopSupport().hibernate();
    }

    public static void shutdownSystem(final boolean force) throws InterruptedException {
        CrossSystem.getDesktopSupport().shutdown(force);
    }

    /**
     *
     */
    public static void playErrorSound() {
        if (getOSFamily() == OSFamily.WINDOWS) {
            final Object runnable = Toolkit.getDefaultToolkit().getDesktopProperty("win.sound.exclamation");
            if (runnable != null && runnable instanceof Runnable) {
                ((Runnable) runnable).run();
                return;
            }
        }
        Toolkit.getDefaultToolkit().beep();
    }
}