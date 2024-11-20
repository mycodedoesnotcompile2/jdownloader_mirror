package org.appwork.jna.windows;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

public interface NtDll extends StdCallLibrary, WinNT {
    public final static NtDll INSTANCE = Native.load("ntdll", NtDll.class, W32APIOptions.UNICODE_OPTIONS);

    public BOOL RtlSetCurrentTransaction(HANDLE transaction);

    int NtQueryInformationProcess(WinNT.HANDLE processHandle, int processInformationClass, Pointer processInformation, int processInformationLength, IntByReference returnLength);

    public HANDLE RtlGetCurrentTransaction();
}
