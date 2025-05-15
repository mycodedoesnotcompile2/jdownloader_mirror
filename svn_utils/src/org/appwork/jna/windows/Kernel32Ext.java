package org.appwork.jna.windows;

import org.appwork.jna.windows.interfaces.ByHandleFileInformation;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.win32.W32APIOptions;

public interface Kernel32Ext extends Kernel32 {
    final static Kernel32Ext INSTANCE = Native.load("kernel32", Kernel32Ext.class, W32APIOptions.DEFAULT_OPTIONS);

    boolean SetConsoleCtrlHandler(HANDLER_ROUTINE HandlerRoutine, boolean Add);

    boolean GetFileInformationByHandle(HANDLE hFile, ByHandleFileInformation lpFileInformation);
}
