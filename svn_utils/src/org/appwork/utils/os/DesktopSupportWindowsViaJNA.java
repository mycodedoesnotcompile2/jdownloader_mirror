package org.appwork.utils.os;

import java.io.File;
import java.io.IOException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;

import org.appwork.builddecision.BuildDecisionRequired;
import org.appwork.utils.os.DesktopSupportForWindowsIphlpapi;
import org.appwork.utils.os.DesktopSupportForWindowsMIB_TCP6ROW2;
import org.appwork.utils.os.DesktopSupportForWindowsMIB_TCP6TABLE2;
import org.appwork.jna.windows.MIB_TCPROW2;
import org.appwork.jna.windows.MIB_TCPTABLE2;
import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTestValidateClassReference;

import com.sun.jna.Memory;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.ptr.IntByReference;

@BuildDecisionRequired(tags = { DesktopSupportWindowsViaJNA.DESKTOP_SUPPORT_WINDOWS_VIA_JNA_YES, DesktopSupportWindowsViaJNA.DESKTOP_SUPPORT_WINDOWS_VIA_JNA_NO }, imports = { DesktopSupportWindowsViaJNA.ORG_APPWORK_UTILS_OS_DESKTOP_SUPPORT_WINDOWS_VIA_JNA, "" }, dependsOn = { org.appwork.JNAHelper.JNA_HELPER_USE_JNA, "" })
public class DesktopSupportWindowsViaJNA extends DesktopSupportWindows {
    @AWTestValidateClassReference
    static final String        ORG_APPWORK_UTILS_OS_DESKTOP_SUPPORT_WINDOWS_VIA_JNA = "org.appwork.utils.os.DesktopSupportWindowsViaJNA";
    public static final String DESKTOP_SUPPORT_WINDOWS_VIA_JNA_NO                   = "DesktopSupportWindowsViaJNA.no";
    public static final String DESKTOP_SUPPORT_WINDOWS_VIA_JNA_YES                  = "DesktopSupportWindowsViaJNA.yes";

    public DesktopSupportWindowsViaJNA() {
        // TODO Auto-generated constructor stub
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
     * Converts 16-byte IPv6 address (e.g. from DesktopSupportForWindowsMIB_TCP6ROW2) to InetAddress.
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
     * @see org.appwork.utils.os.DesktopSupportWindows#openFile(java.io.File, boolean)
     */
    @Override
    public void openFile(File file, boolean tryToReuseWindows) throws IOException {
        if (tryToReuseWindows) {
            if (file.isDirectory()) {
                if (WindowsUtils.explorerToFront(file)) {
                    return;
                }
            }
        }
        super.openFile(file, tryToReuseWindows);
    }

    @Override
    public int getPIDForRemoteAddress(final SocketAddress adr) throws InterruptedException {
        final InetSocketAddress inetAdr = (InetSocketAddress) adr;
        final InetAddress requestingAddress = inetAdr.getAddress();
        final int port = inetAdr.getPort();
        if (requestingAddress instanceof Inet6Address) {
            final int pid = findPidFromTcpTableIPv6(requestingAddress, port);
            if (pid >= 0) {
                return pid;
            }
            LogV3.fine("Get PID By socket(JNA IPv6): " + adr + " -> NONE. try fallback.");
            return super.getPIDForRemoteAddress(adr);
        }
        final int pid = findPidFromTcpTableIPv4(requestingAddress, port);
        if (pid >= 0) {
            return pid;
        }
        LogV3.fine("Get PID By socket(JNA): " + adr + " -> NONE. try fallback.");
        return super.getPIDForRemoteAddress(adr);
    }

    /**
     * Finds the owning PID for the connection row where local address = peer (IPv4). Returns -1 if not found or on error.
     */
    private int findPidFromTcpTableIPv4(final InetAddress peerAddress, final int peerPort) {
        try {
            final IntByReference psize = new IntByReference(0);
            psize.setValue(0);
            final boolean sort = true;
            if (DesktopSupportForWindowsIphlpapi.INSTANCE.GetTcpTable2(null, psize, sort) != WinError.ERROR_INSUFFICIENT_BUFFER) {
                return -1;
            }
            final Memory mem = new Memory(psize.getValue());
            if (DesktopSupportForWindowsIphlpapi.INSTANCE.GetTcpTable2(mem, psize, sort) != WinError.NO_ERROR) {
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
     * Finds the owning PID for the connection row where local address = peer (IPv6). Returns -1 if not found or on error.
     * GetTcp6Table2 is only available on Windows Vista and later; on older Windows we return -1 so the caller can fall back to netstat.
     */
    private int findPidFromTcpTableIPv6(final InetAddress peerAddress, final int peerPort) {
        if (!CrossSystem.OS.isMinimum(CrossSystem.OperatingSystem.WINDOWS_VISTA)) {
            LogV3.fine("GetTcp6Table2 requires Windows Vista or later, skipping JNA IPv6 (fallback to netstat)");
            return -1;
        }
        try {
            final IntByReference psize = new IntByReference(0);
            psize.setValue(0);
            final boolean sort = true;
            if (DesktopSupportForWindowsIphlpapi.INSTANCE.GetTcp6Table2(null, psize, sort) != WinError.ERROR_INSUFFICIENT_BUFFER) {
                return -1;
            }
            final Memory mem = new Memory(psize.getValue());
            if (DesktopSupportForWindowsIphlpapi.INSTANCE.GetTcp6Table2(mem, psize, sort) != WinError.NO_ERROR) {
                return -1;
            }
            final DesktopSupportForWindowsMIB_TCP6TABLE2 table = new DesktopSupportForWindowsMIB_TCP6TABLE2(mem);
            for (int i = 0; i < table.dwNumEntries.intValue(); i++) {
                final DesktopSupportForWindowsMIB_TCP6ROW2 row = table.table[i];
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
