package org.appwork.utils.os;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;

import org.appwork.builddecision.BuildDecisionRequired;
import org.appwork.exceptions.WTFException;
import org.appwork.jna.windows.Iphlpapi;
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

    @Override
    public int getPIDForRemoteAddress(final SocketAddress adr) throws InterruptedException {
        try {
            final InetAddress requestingAddress = ((InetSocketAddress) adr).getAddress();
            final int port = ((InetSocketAddress) adr).getPort();
            final IntByReference psize = new IntByReference(0);
            psize.setValue(0);
            final boolean sort = true;
            MIB_TCPTABLE2 table = null;
            if (Iphlpapi.INSTANCE.GetTcpTable2(null, psize, sort) == WinError.ERROR_INSUFFICIENT_BUFFER) {
                Memory mem;
                if (Iphlpapi.INSTANCE.GetTcpTable2(mem = new Memory(psize.getValue()), psize, sort) != WinError.NO_ERROR) {
                    throw new WTFException("ERROR");
                }
                table = new MIB_TCPTABLE2(mem);
            } else {
                throw new WTFException();
            }
            LogV3.info("Table Size: " + table.table.length + "/" + table.dwNumEntries.intValue());
            for (int i = 0; i < table.dwNumEntries.intValue(); i++) {
                final MIB_TCPROW2 e = table.table[i];
                if (e != null) {
                    if (requestingAddress.equals(getInetAdress(e.dwLocalAddr)) && switchbytes(e.dwLocalPort) == port) {
                        final long ret = e.dwOwningPid.longValue();
                        LogV3.fine("Get PID By socket(JNA): " + adr + " -> " + ret);
                        return (int) ret;
                    }
                }
            }
        } catch (final Throwable e) {
            LogV3.log(e);
        }
        LogV3.fine("Get PID By socket(JNA): " + adr + " -> NONE. try fallback.");
        return super.getPIDForRemoteAddress(adr);
    }
}
