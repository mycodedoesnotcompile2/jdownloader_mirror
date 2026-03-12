/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.os.tests;

import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import org.appwork.exceptions.NotSupportedException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.DesktopSupport;
import org.appwork.utils.os.DesktopSupportWindows;
import org.appwork.utils.processes.command.Command;

/**
 * AWTest for getPIDForRemoteAddress on all platforms: Windows ({@link DesktopSupportWindows} / {@link DesktopSupportWindowsViaJNA}),
 * Linux ({@link org.appwork.utils.os.DesktopSupportLinux}), Mac ({@link org.appwork.utils.os.DesktopSupportMac}).
 * <p>
 * On all OS: tests unknown address (-1), IPv4 localhost (client PID), IPv6 localhost (client PID) using
 * {@link CrossSystem#getDesktopSupport()}. On Windows only: additionally tests netstat and JNA variant and that both return the same PID.
 * <p>
 * The getPID tests (IPv4/IPv6 localhost) run with the <b>client in a separate JVM process</b> (started via {@link Command}), so that server
 * and client are different processes and getPIDForRemoteAddress(remote) correctly resolves the client process PID.
 *
 * @author thomas
 * @date 20.11.2024
 */
public class DesktopSupportWindowsTest extends AWTest {

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        final DesktopSupport ds = CrossSystem.getDesktopSupport();
        final String osName = CrossSystem.isWindows() ? "Windows" : (CrossSystem.isLinux() ? "Linux" : (CrossSystem.isMac() ? "Mac" : "current OS"));
        logInfoAnyway("=== Testing getPIDForRemoteAddress (" + osName + ") ===");
        testGetPIDForRemoteAddressUnknownAddress(ds);
        testGetPIDForRemoteAddressIPv4Localhost(ds);
        testGetPIDForRemoteAddressIPv6Localhost(ds);
        if (!CrossSystem.isWindows()) {
            return;
        }
        // Windows-only: test netstat and JNA variants separately, then same-result comparison
        logInfoAnyway("=== Testing DesktopSupportWindows (netstat) ===");
        final DesktopSupportWindows base = new DesktopSupportWindows();
        testGetPIDForRemoteAddressUnknownAddress(base);
        testGetPIDForRemoteAddressIPv4Localhost(base);
        testGetPIDForRemoteAddressIPv6Localhost(base);
        try {
            final Class<?> jnaClass = Class.forName("org.appwork.utils.os.DesktopSupportWindowsViaJNA");
            final DesktopSupportWindows viaJna = (DesktopSupportWindows) jnaClass.getDeclaredConstructor().newInstance();
            logInfoAnyway("=== Testing DesktopSupportWindowsViaJNA ===");
            testGetPIDForRemoteAddressUnknownAddress(viaJna);
            testGetPIDForRemoteAddressIPv4Localhost(viaJna);
            testGetPIDForRemoteAddressIPv6Localhost(viaJna);
            logInfoAnyway("=== Testing netstat vs JNA return same PID ===");
            testGetPIDForRemoteAddressIPv4LocalhostSameResult(base, viaJna);
            testGetPIDForRemoteAddressIPv6LocalhostSameResult(base, viaJna);
        } catch (ClassNotFoundException e) {
            logInfoAnyway("DesktopSupportWindowsViaJNA not on classpath, skip JNA variant tests");
        } catch (Throwable e) {
            logInfoAnyway("DesktopSupportWindowsViaJNA not available: " + e.getMessage() + ", skip JNA variant tests");
        }
    }

    /**
     * Unknown/non-existing address must return -1.
     */
    private void testGetPIDForRemoteAddressUnknownAddress(final DesktopSupport ds) throws Exception {
        logInfoAnyway("Test: getPIDForRemoteAddress(unknown address) should return -1");
        final SocketAddress unknown = new InetSocketAddress("1.2.3.4", 65535);
        final int pid = ds.getPIDForRemoteAddress(unknown);
        assertTrue(pid == -1, "getPIDForRemoteAddress(unknown) should return -1, got: " + pid);
        logInfoAnyway("  OK: got -1 as expected");
    }

    /**
     * IPv4 localhost: server (this process) accepts client from a separate process; getPIDForRemoteAddress(remote) must return the client
     * process PID. Client is started via Command in another JVM process so that sender and receiver are different processes.
     * On Linux/Mac the experimental implementation may return -1 when run from JAR (IDE-only); then test is skipped if PID is -1 and
     * Command.getPID() is supported.
     */
    private void testGetPIDForRemoteAddressIPv4Localhost(final DesktopSupport ds) throws Exception {
        logInfoAnyway("Test: getPIDForRemoteAddress(IPv4 localhost) with client in separate process -> PID = client PID");
        final ServerSocket server = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"));
        final int port = server.getLocalPort();
        final String classpath = ManagementFactory.getRuntimeMXBean().getClassPath();
        final Command cmd = new Command(CrossSystem.getJavaBinary(), "-cp", classpath, DesktopSupportWindowsTestClient.class.getName(), String.valueOf(port));
        try {
            cmd.start(false);
            long clientPid;
            try {
                clientPid = cmd.getPID();
            } catch (NotSupportedException e) {
                logInfoAnyway("  SKIP: Command.getPID() not supported: " + e.getMessage());
                return;
            }
            long mePid = CrossSystem.getPID();
            final Socket accepted = server.accept();
            try {
                final SocketAddress remoteAdr = accepted.getRemoteSocketAddress();
                assertTrue(remoteAdr instanceof InetSocketAddress, "remote should be InetSocketAddress");
                final int pid = ds.getPIDForRemoteAddress(remoteAdr);
                if (pid < 0) {
                    logInfoAnyway("  SKIP: getPIDForRemoteAddress returned -1 (e.g. not supported or not running in IDE on Linux/Mac)");
                    return;
                }
                assertTrue(pid != mePid, "The found PID must not be my own PID");
                assertTrue(pid >= 0, "getPIDForRemoteAddress(IPv4 client) should return non-negative PID, got: " + pid);
                assertThat(pid).isNumber((int) clientPid);
                logInfoAnyway("  OK: got PID " + pid + " (client process " + clientPid + ")");
            } finally {
                accepted.close();
            }
        } finally {
            server.close();
            try {
                cmd.destroy();
            } catch (Throwable t) {
                // ignore
            }
        }
    }

    /**
     * When both netstat and JNA implementations are available, both must return the same PID for the same
     * remote address (IPv4 localhost client).
     */
    private void testGetPIDForRemoteAddressIPv4LocalhostSameResult(final DesktopSupportWindows netstatImpl, final DesktopSupportWindows jnaImpl) throws Exception {
        logInfoAnyway("Test: netstat and JNA getPIDForRemoteAddress(IPv4 localhost) return same PID");
        final ServerSocket server = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"));
        final int port = server.getLocalPort();
        final String classpath = ManagementFactory.getRuntimeMXBean().getClassPath();
        final Command cmd = new Command(CrossSystem.getJavaBinary(), "-cp", classpath, DesktopSupportWindowsTestClient.class.getName(), String.valueOf(port));
        try {
            cmd.start(false);
            long clientPid;
            try {
                clientPid = cmd.getPID();
            } catch (NotSupportedException e) {
                logInfoAnyway("  SKIP: Command.getPID() not supported: " + e.getMessage());
                return;
            }
            final Socket accepted = server.accept();
            try {
                final SocketAddress remoteAdr = accepted.getRemoteSocketAddress();
                assertTrue(remoteAdr instanceof InetSocketAddress, "remote should be InetSocketAddress");
                final int pidNetstat = netstatImpl.getPIDForRemoteAddress(remoteAdr);
                final int pidJna = jnaImpl.getPIDForRemoteAddress(remoteAdr);
                assertTrue(pidNetstat >= 0, "netstat getPIDForRemoteAddress should return non-negative, got: " + pidNetstat);
                assertTrue(pidJna >= 0, "JNA getPIDForRemoteAddress should return non-negative, got: " + pidJna);
                assertTrue(pidNetstat == pidJna, "netstat and JNA must return same PID; netstat=" + pidNetstat + ", JNA=" + pidJna);
                assertThat(pidNetstat).isNumber((int) clientPid);
                logInfoAnyway("  OK: netstat and JNA both returned PID " + pidNetstat + " (client " + clientPid + ")");
            } finally {
                accepted.close();
            }
        } finally {
            server.close();
            try {
                cmd.destroy();
            } catch (Throwable t) {
                // ignore
            }
        }
    }

    /**
     * IPv6 localhost: same as IPv4 but client connects to ::1. Client runs in separate process via Command. Skips if IPv6 not available.
     * On Linux/Mac the experimental implementation may return -1 when run from JAR (IDE-only); then test is skipped if PID is -1.
     */
    private void testGetPIDForRemoteAddressIPv6Localhost(final DesktopSupport ds) throws Exception {
        logInfoAnyway("Test: getPIDForRemoteAddress(IPv6 localhost) with client in separate process -> PID = client PID");
        ServerSocket server = null;
        try {
            server = new ServerSocket(0, 1, InetAddress.getByName("::1"));
        } catch (Throwable e) {
            logInfoAnyway("  SKIP: IPv6 listen not available: " + e.getMessage());
            return;
        }
        try {
            final int port = server.getLocalPort();
            final String classpath = ManagementFactory.getRuntimeMXBean().getClassPath();
            final Command cmd = new Command(CrossSystem.getJavaBinary(), "-cp", classpath, DesktopSupportWindowsTestClient.class.getName(), "::1", String.valueOf(port));
            try {
                cmd.start(false);
                long clientPid;
                try {
                    clientPid = cmd.getPID();
                } catch (NotSupportedException e) {
                    logInfoAnyway("  SKIP: Command.getPID() not supported: " + e.getMessage());
                    return;
                }
                final Socket accepted = server.accept();
                try {
                    final SocketAddress remoteAdr = accepted.getRemoteSocketAddress();
                    assertTrue(remoteAdr instanceof InetSocketAddress, "remote should be InetSocketAddress");
                    final int pid = ds.getPIDForRemoteAddress(remoteAdr);
                    if (pid < 0) {
                        logInfoAnyway("  SKIP: getPIDForRemoteAddress returned -1 (e.g. not supported or not running in IDE on Linux/Mac)");
                        return;
                    }
                    assertTrue(pid != CrossSystem.getPID(), "Found PID must not be my own PID");
                    assertTrue(pid >= 0, "getPIDForRemoteAddress(IPv6 client) should return non-negative PID, got: " + pid);
                    assertThat(pid).isNumber((int) clientPid);
                    logInfoAnyway("  OK: got PID " + pid + " (client process " + clientPid + ")");
                } finally {
                    accepted.close();
                }
            } finally {
                try {
                    cmd.destroy();
                } catch (Throwable t) {
                    // ignore
                }
            }
        } finally {
            server.close();
        }
    }

    /**
     * When both netstat and JNA implementations are available, both must return the same PID for the same
     * remote address (IPv6 localhost client). Skips if IPv6 listen is not available.
     */
    private void testGetPIDForRemoteAddressIPv6LocalhostSameResult(final DesktopSupportWindows netstatImpl, final DesktopSupportWindows jnaImpl) throws Exception {
        logInfoAnyway("Test: netstat and JNA getPIDForRemoteAddress(IPv6 localhost) return same PID");
        ServerSocket server = null;
        try {
            server = new ServerSocket(0, 1, InetAddress.getByName("::1"));
        } catch (Throwable e) {
            logInfoAnyway("  SKIP: IPv6 listen not available: " + e.getMessage());
            return;
        }
        try {
            final int port = server.getLocalPort();
            final String classpath = ManagementFactory.getRuntimeMXBean().getClassPath();
            final Command cmd = new Command(CrossSystem.getJavaBinary(), "-cp", classpath, DesktopSupportWindowsTestClient.class.getName(), "::1", String.valueOf(port));
            try {
                cmd.start(false);
                long clientPid;
                try {
                    clientPid = cmd.getPID();
                } catch (NotSupportedException e) {
                    logInfoAnyway("  SKIP: Command.getPID() not supported: " + e.getMessage());
                    return;
                }
                final Socket accepted = server.accept();
                try {
                    final SocketAddress remoteAdr = accepted.getRemoteSocketAddress();
                    assertTrue(remoteAdr instanceof InetSocketAddress, "remote should be InetSocketAddress");
                    final int pidNetstat = netstatImpl.getPIDForRemoteAddress(remoteAdr);
                    final int pidJna = jnaImpl.getPIDForRemoteAddress(remoteAdr);
                    assertTrue(pidNetstat >= 0, "netstat getPIDForRemoteAddress should return non-negative, got: " + pidNetstat);
                    assertTrue(pidJna >= 0, "JNA getPIDForRemoteAddress should return non-negative, got: " + pidJna);
                    assertTrue(pidNetstat == pidJna, "netstat and JNA must return same PID; netstat=" + pidNetstat + ", JNA=" + pidJna);
                    assertThat(pidNetstat).isNumber((int) clientPid);
                    logInfoAnyway("  OK: netstat and JNA both returned PID " + pidNetstat + " (client " + clientPid + ")");
                } finally {
                    accepted.close();
                }
            } finally {
                try {
                    cmd.destroy();
                } catch (Throwable t) {
                    // ignore
                }
            }
        } finally {
            server.close();
        }
    }

    public static void main(String[] args) {
        run();
    }
}
