package org.appwork.jna.windows;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.LastErrorException;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.Wtsapi32;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

public interface Wtsapi32Ext extends Wtsapi32 {
    // Method declarations, constant and structure definitions go here
    /** The INSTANCE. */
    public final Wtsapi32Ext INSTANCE = Native.loadLibrary("wtsapi32", Wtsapi32Ext.class, W32APIOptions.DEFAULT_OPTIONS);

    public boolean WTSQueryUserToken(int sessionID, PointerByReference phToken);
    /*
     * BOOL WTSQueryUserToken( __in ULONG SessionId, __out PHANDLE phToken);
     */

    public boolean WTSSendMessageW(HANDLE server, int sessionID, char[] title, int titleLength, char[] message, int messageLength, int style, int timeout, IntByReference response, boolean wait);

    /** The message box contains three push buttons: Abort, Retry, and Ignore. **/
    public static final int MB_ABORTRETRYIGNORE = 2;
    public static final int MB_OKCANCEL         = 1;

    boolean WTSEnumerateSessions(Pointer hServer, int Reserved, int Version, PointerByReference ppSessionInfo, IntByReference pCount) throws LastErrorException;

    void WTSFreeMemory(Pointer pMemory);

    public static enum WTS_CONNECTSTATE_CLASS {
        WTSActive,
        WTSConnected,
        WTSConnectQuery,
        WTSShadow,
        WTSDisconnected,
        WTSIdle,
        WTSListen,
        WTSReset,
        WTSDown,
        WTSInit
    }

    public static class WTS_SESSION_INFO extends Structure {
        public static class ByReference extends WTS_SESSION_INFO implements Structure.ByReference {
        }

        public int    sessionId;
        public String pWinStationName;
        // typedef enum _WTS_CONNECTSTATE_CLASS {
        // WTSActive,
        // WTSConnected,
        // WTSConnectQuery,
        // WTSShadow,
        // WTSDisconnected,
        // WTSIdle,
        // WTSListen,
        // WTSReset,
        // WTSDown,
        // WTSInit
        // } WTS_CONNECTSTATE_CLASS;
        public int    state;

        public WTS_SESSION_INFO() {
        }

        public WTS_SESSION_INFO(final Pointer p) {
            super(p);
        }

        @Override
        protected List getFieldOrder() {
            return Arrays.asList("sessionId", "pWinStationName", "state");
        }
    }
}