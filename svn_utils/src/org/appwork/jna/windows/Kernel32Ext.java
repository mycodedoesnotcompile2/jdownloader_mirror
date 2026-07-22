package org.appwork.jna.windows;

import org.appwork.jna.windows.interfaces.ByHandleFileInformation;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Kernel32;

public interface Kernel32Ext extends Kernel32 {
    final static Kernel32Ext INSTANCE = Native.load("kernel32", Kernel32Ext.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

    boolean SetConsoleCtrlHandler(HANDLER_ROUTINE HandlerRoutine, boolean Add);

    boolean GetFileInformationByHandle(HANDLE hFile, ByHandleFileInformation lpFileInformation);

    /**
     * Retrieves the session ID of the console session.
     *
     * @return session id, or 0xFFFFFFFF if none
     */
    int WTSGetActiveConsoleSessionId();
}
