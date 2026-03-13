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
package org.appwork.processes.windows.jna;

import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Memory;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.ptr.IntByReference;

/**
 * Windows process utilities via JNA (IP Helper API). Resolves PID for a remote socket address from the TCP connection table.
 *
 * @author thomas
 * @date 12.03.2026
 */
public class WindowsJNAProcessUtils {

    /**
     * Resolves the owning process ID for the given remote socket address using GetTcpTable2/GetTcp6Table2. Returns -1 if not found or on error
     * (caller may fall back to netstat or other implementation).
     *
     * @param adr remote socket address (client side of the connection from server perspective)
     * @return owning PID, or -1 if not found or on error
     */
    public static int getPIDForRemoteAddress(final SocketAddress adr) {
        final InetSocketAddress inetAdr = (InetSocketAddress) adr;
        final InetAddress requestingAddress = inetAdr.getAddress();
        final int port = inetAdr.getPort();
        if (requestingAddress instanceof Inet6Address) {
            final int pid = findPidFromTcpTableIPv6(requestingAddress, port);
            if (pid >= 0) {
                return pid;
            }
            LogV3.fine("Get PID By socket(JNA IPv6): " + adr + " -> NONE. try fallback.");
            return -1;
        }
        final int pid = findPidFromTcpTableIPv4(requestingAddress, port);
        if (pid >= 0) {
            return pid;
        }
        LogV3.fine("Get PID By socket(JNA): " + adr + " -> NONE. try fallback.");
        return -1;
    }

    public static byte[] int32toBytes(final int hex) {
        final byte[] b = new byte[4];
        b[3] = (byte) ((hex & 0xFF000000) >> 24);
        b[2] = (byte) ((hex & 0x00FF0000) >> 16);
        b[1] = (byte) ((hex & 0x0000FF00) >> 8);
        b[0] = (byte) (hex & 0x000000FF);
        return b;
    }

    public static int switchbytes(final DWORD value) {
        final int b1 = (value.intValue() & 0x0000FF00) >> 8;
        final int b2 = value.intValue() & 0x000000FF;
        final int port = (b2 << 8) + b1;
        return port;
    }

    public static InetAddress getInetAdress(final DWORD d) {
        final int dwLocalAddr = d.intValue();
        try {
            if (dwLocalAddr == 0) {
                return null;
            }
            final byte[] bytes = int32toBytes(dwLocalAddr);
            final InetAddress address = InetAddress.getByAddress(bytes);
            return address;
        } catch (final UnknownHostException e) {
            LogV3.log(e);
        }
        return null;
    }

    /**
     * Converts 16-byte IPv6 address (e.g. from {@link MIB_TCP6ROW2}) to InetAddress.
     */
    public static InetAddress getInetAddressFromIPv6Bytes(final byte[] bytes) {
        if (bytes == null || bytes.length != 16) {
            return null;
        }
        try {
            return InetAddress.getByAddress(bytes);
        } catch (final UnknownHostException e) {
            LogV3.log(e);
        }
        return null;
    }

    /**
     * Finds the owning PID for the connection row where local address = peer (IPv4). Returns -1 if not found or on error.
     */
    public static int findPidFromTcpTableIPv4(final InetAddress peerAddress, final int peerPort) {
        try {
            final IntByReference psize = new IntByReference(0);
            psize.setValue(0);
            final boolean sort = true;
            if (WindowsIphlpapi.INSTANCE.GetTcpTable2(null, psize, sort) != WinError.ERROR_INSUFFICIENT_BUFFER) {
                return -1;
            }
            final Memory mem = new Memory(psize.getValue());
            if (WindowsIphlpapi.INSTANCE.GetTcpTable2(mem, psize, sort) != WinError.NO_ERROR) {
                return -1;
            }
            final MIB_TCPTABLE2 table = new MIB_TCPTABLE2(mem);
            for (int i = 0; i < table.dwNumEntries.intValue(); i++) {
                final MIB_TCPROW2 row = table.table[i];
                if (row == null) {
                    continue;
                }
                if (!peerAddress.equals(getInetAdress(row.dwLocalAddr))) {
                    continue;
                }
                if (switchbytes(row.dwLocalPort) != peerPort) {
                    continue;
                }
                final long ret = row.dwOwningPid.longValue();
                if (ret > 0) {
                    return (int) ret;
                }
                LogV3.fine("GetTcpTable2 returned PID 0 for matching row (unknown owner), try fallback");
                return -1;
            }
        } catch (final Throwable e) {
            LogV3.log(e);
        }
        return -1;
    }

    /**
     * Finds the owning PID for the connection row where local address = peer (IPv6). Returns -1 if not found or on error. GetTcp6Table2 is
     * only available on Windows Vista and later; on older Windows we return -1 so the caller can fall back to netstat.
     */
    public static int findPidFromTcpTableIPv6(final InetAddress peerAddress, final int peerPort) {
        if (!CrossSystem.OS.isMinimum(CrossSystem.OperatingSystem.WINDOWS_VISTA)) {
            LogV3.fine("GetTcp6Table2 requires Windows Vista or later, skipping JNA IPv6 (fallback to netstat)");
            return -1;
        }
        try {
            final IntByReference psize = new IntByReference(0);
            psize.setValue(0);
            final boolean sort = true;
            if (WindowsIphlpapi.INSTANCE.GetTcp6Table2(null, psize, sort) != WinError.ERROR_INSUFFICIENT_BUFFER) {
                return -1;
            }
            final Memory mem = new Memory(psize.getValue());
            if (WindowsIphlpapi.INSTANCE.GetTcp6Table2(mem, psize, sort) != WinError.NO_ERROR) {
                return -1;
            }
            final MIB_TCP6TABLE2 table = new MIB_TCP6TABLE2(mem);
            for (int i = 0; i < table.dwNumEntries.intValue(); i++) {
                final MIB_TCP6ROW2 row = table.table[i];
                if (row == null) {
                    continue;
                }
                final InetAddress rowLocal = getInetAddressFromIPv6Bytes(row.LocalAddr);
                if (!peerAddress.equals(rowLocal)) {
                    continue;
                }
                if (switchbytes(row.dwLocalPort) != peerPort) {
                    continue;
                }
                final long pid = row.dwOwningPid.longValue();
                if (pid > 0) {
                    return (int) pid;
                }
                LogV3.fine("GetTcp6Table2 returned PID 0 for matching row (unknown owner), try fallback");
                return -1;
            }
        } catch (final Throwable e) {
            LogV3.log(e);
        }
        return -1;
    }
}
