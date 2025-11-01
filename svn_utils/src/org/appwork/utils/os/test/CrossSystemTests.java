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
package org.appwork.utils.os.test;

import org.appwork.testframework.AWTest;
import org.appwork.utils.os.CrossSystem;

/**
 * @author daniel
 * @date Mar 31, 2022
 *
 */
public class CrossSystemTests extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        final String[] true64BitValues = new String[] { "amd64", "amd_64", "x86_64", "aarch64", "arm64", "riscv64", "sparcv9", "ppc64", "ia64" };
        for (final String true64BitValue : true64BitValues) {
            assertTrue(CrossSystem.is64BitARCH(true64BitValue, false));
            assertTrue(CrossSystem.is64BitARCH(true64BitValue, true));
        }
        assertFalse(CrossSystem.is64BitARCH("riscv32", false));
        assertFalse(CrossSystem.is64BitARCH("riscv32", true));
        final String[] true32BitValues = new String[] { "i386", "i486", "i586", "i686", "x86", "sparc" };
        for (final String true32BitValue : true32BitValues) {
            assertFalse(CrossSystem.is64BitARCH(true32BitValue, false));
            assertEquals(null, CrossSystem.is64BitARCH(true32BitValue, true));
        }
        assertEquals(CrossSystem.OperatingSystem.RASPBIAN_BOOKWORM, CrossSystem.parseReleaseByKernelString("Linux version 6.12.34+rpt-rpi-v8 (serge@raspberrypi.com) (aarch64-linux-gnu-gcc-12 (Debian 12.2.0-14+deb12u1) 12.2.0, GNU ld (GNU Binutils for Debian) 2.40) #1 SMP PREEMPT Debian 1:6.12.34-1+rpt1~bookworm (2025-06-26)"));
        {
            assertEquals(CrossSystem.OperatingSystem.FEDORA, CrossSystem.parseReleaseByKernelString("Linux version 2.6.32.12-115.fc12.i686 (mockbuild@x86-03.phx2.fedoraproject.org) (gcc version 4.4.3 20100127 (Red Hat 4.4.3-4) (GCC) ) #1 SMP Fri Apr 30 20:34:53 UTC 2010"));
            assertEquals(CrossSystem.OperatingSystem.FEDORA, CrossSystem.parseReleaseByKernelString("Linux version 2.6.32.12-115.fc12.i686 (gcc version 4.4.3 20100127 (Red Hat 4.4.3-4) (GCC) ) #1 SMP Fri Apr 30 20:34:53 UTC 2010"));
        }
        {
            assertEquals(CrossSystem.OperatingSystem.UBUNTU, CrossSystem.parseReleaseByKernelString("Linux version 6.1.21-v8+ (dom@buildbot) (aarch64-linux-gnu-gcc-8 (Ubuntu/Linaro 8.4.0-3ubuntu1) 8.4.0, GNU ld (GNU Binutils for Ubuntu) 2.34) #1642 SMP PREEMPT Mon Apr  3 17:24:16 BST 2023"));
            assertEquals(CrossSystem.OperatingSystem.UBUNTU_JAMMY, CrossSystem.parseReleaseByKernelString("Linux version 5.15.0-157-generic (buildd@lcy02-amd64-004) (gcc (Ubuntu 11.4.0-1ubuntu1~22.04.2) 11.4.0, GNU ld (GNU Binutils for Ubuntu) 2.38) #167-Ubuntu SMP Wed Sep 17 21:35:53 UTC 2025"));
            assertEquals(CrossSystem.OperatingSystem.UBUNTU_FOCAL, CrossSystem.parseReleaseByKernelString("Linux version 5.4.0-65-generic (buildd@lgw01-amd64-039) (gcc version 9.3.0 (Ubuntu 9.3.0-17ubuntu1~20.04)) #73-Ubuntu SMP Mon Jan 18 17:25:17 UTC 2021"));
        }
        {
            assertEquals(CrossSystem.OperatingSystem.RASPBIAN_BOOKWORM, CrossSystem.parseReleaseByKernelString("Linux zero2w 6.12.34+rpt-rpi-v8 #1 SMP PREEMPT Debian 1:6.12.34-1+rpt1~bookworm (2025-06-26) aarch64 GNU/Linux"));
            assertEquals(CrossSystem.OperatingSystem.RASPBIAN_BOOKWORM, CrossSystem.parseReleaseByKernelString("Linux version 6.12.34+rpt-rpi-v8 (serge@raspberrypi.com) (aarch64-linux-gnu-gcc-12 (Debian 12.2.0-14+deb12u1) 12.2.0, GNU ld (GNU Binutils for Debian) 2.40) #1 SMP PREEMPT Debian 1:6.12.34-1+rpt1~UNKNOWN (2025-06-26)"));
            assertEquals(CrossSystem.OperatingSystem.RASPBIAN_BOOKWORM, CrossSystem.parseReleaseByKernelString("Linux version 6.12.34+rpt-rpi-v8 (serge@raspberrypi.com) (aarch64-linux-gnu-gcc-12 (Debian 12.2.0-14+deb12u1) 12.2.0, GNU ld (GNU Binutils for Debian) 2.40) #1 SMP PREEMPT Debian 1:6.12.34-1+rpt1~UNKNOWN (2025-06-26)"));
            assertEquals(CrossSystem.OperatingSystem.RASPBIAN_TRIXIE, CrossSystem.parseReleaseByKernelString("Linux Trixie 6.12.47+rpt-rpi-2712 #1 SMP PREEMPT Debian 1:6.12.47-1+rpt1 (2025-0 9-16) aarch64"));
            assertEquals(CrossSystem.OperatingSystem.RASPBIAN, CrossSystem.parseReleaseByKernelString("Linux zero2w 6.12.34+rpt-rpi-v8 #1 SMP PREEMPT Debian 1:6.12.34-1+rpt1~UNKNOWN (2025-06-26) aarch64 GNU/Linux"));
        }
        {
            assertEquals(CrossSystem.OperatingSystem.DEBIAN_SQUEEZE, CrossSystem.parseReleaseByKernelString("Linux version 2.6.32-5-686 (Debian 2.6.32-48squeeze6) (gcc version 4.3.5 (Debian 4.3.5-4) ) #1 SMP Mon Feb 25 00:26:36 UTC 2013"));
            assertEquals(CrossSystem.OperatingSystem.DEBIAN, CrossSystem.parseReleaseByKernelString("Linux version 4.9.0-8-amd64 (debian-kernel@lists.debian.org) (gcc version 6.3.0 20170516 (Debian 6.3.0-18+deb9u1) ) #1 SMP Debian 4.9.110-3+deb9u4 (2018-08-21)"));
            assertEquals(CrossSystem.OperatingSystem.DEBIAN_TRIXIE, CrossSystem.parseReleaseByKernelString("Linux Trixie 6.12.47"));
        }
        {
            assertEquals(CrossSystem.OperatingSystem.CENTOS, CrossSystem.parseReleaseByKernelString("Linux centos7 3.10.0-1062.el7.x86_64 #1 SMP Wed Aug "));
            assertEquals(CrossSystem.OperatingSystem.CENTOS, CrossSystem.parseReleaseByKernelString("Linux version 3.10.0-1062.el7.x86_64 (mockbuild@kbuilder.bsys.centos.org) (gcc version 4.8.5 20150623 (Red Hat 4.8.5-36) (GCC) ) #1 SMP Wed Aug 7 18:08:02 UTC 2019"));
        }
        {
            assertEquals(CrossSystem.OperatingSystem.REDHAT, CrossSystem.parseReleaseByKernelString(" 3.10.0-327.el7.x86_64 (mockbuild@x86-034.build.eng.bos.redhat.com) (gcc version 4.8.3 20140911 (Red Hat 4.8.3-9) (GCC) ) #1 SMP Thu Oct 29 17:29:29 EDT 2015 "));
            assertEquals(CrossSystem.OperatingSystem.REDHAT, CrossSystem.parseReleaseByKernelString("Linux 3.10.0-1062.el7.x86_64 #1 SMP Wed Aug "));
            assertEquals(CrossSystem.OperatingSystem.REDHAT, CrossSystem.parseReleaseByKernelString("Linux 2.6.18-8.1.14.el5 x86_64 "));
        }
        assertEquals(CrossSystem.OperatingSystem.BAZZITE, CrossSystem.parseReleaseByKernelString("6.16.4-116.bazzite.fc42.x86_64"));
        assertEquals(CrossSystem.OperatingSystem.FREEBSD, CrossSystem.parseReleaseByKernelString("FreeBSD freepi4 13.5-RELEASE FreeBSD 13.5-RELEASE releng/13.5-n259162-882b9f3f2218 GENERIC arm64"));
        assertEquals(CrossSystem.OperatingSystem.ALPINE, CrossSystem.parseReleaseByKernelString("Linux version 4.19.76-linuxkit (root@d203b39a3d78) (gcc version 8.3.0 (Alpine 8.3.0)) #1 SMP Thu Oct 17 19:31:58 UTC 2019"));
    }
}
