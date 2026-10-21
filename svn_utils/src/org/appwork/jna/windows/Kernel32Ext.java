package org.appwork.jna.windows;

import org.appwork.jna.windows.interfaces.ByHandleFileInformation;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinBase.SECURITY_ATTRIBUTES;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.ptr.IntByReference;

public interface Kernel32Ext extends Kernel32 {
    final static Kernel32Ext INSTANCE = Native.load("kernel32", Kernel32Ext.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

    /** {@code JobObjectBasicProcessIdList} for {@link #QueryInformationJobObject} */
    int JobObjectBasicProcessIdList = 3;

    boolean SetConsoleCtrlHandler(HANDLER_ROUTINE HandlerRoutine, boolean Add);

    boolean GetFileInformationByHandle(HANDLE hFile, ByHandleFileInformation lpFileInformation);

    /**
     * Retrieves the session ID of the console session.
     *
     * @return session id, or 0xFFFFFFFF if none
     */
    int WTSGetActiveConsoleSessionId();

    HANDLE CreateJobObject(SECURITY_ATTRIBUTES lpJobAttributes, String lpName);

    boolean AssignProcessToJobObject(HANDLE hJob, HANDLE hProcess);

    boolean QueryInformationJobObject(HANDLE hJob, int JobObjectInformationClass, Pointer lpJobObjectInformation, int cbJobObjectInformationLength, IntByReference lpReturnLength);

    boolean SetInformationJobObject(HANDLE hJob, int JobObjectInformationClass, Pointer lpJobObjectInformation, int cbJobObjectInformationLength);

    DWORD ResumeThread(HANDLE hThread);
}
