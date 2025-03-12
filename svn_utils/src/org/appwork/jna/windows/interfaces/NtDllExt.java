package org.appwork.jna.windows.interfaces;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

public interface NtDllExt extends StdCallLibrary, WinNT {
    public final static NtDllExt INSTANCE = Native.load("ntdll", NtDllExt.class, W32APIOptions.UNICODE_OPTIONS);

    public BOOL RtlSetCurrentTransaction(HANDLE transaction);

    int NtQueryInformationProcess(WinNT.HANDLE processHandle, int processInformationClass, Pointer processInformation, int processInformationLength, IntByReference returnLength);

    public HANDLE RtlGetCurrentTransaction();
}
