/**
 * JNA interface for NT API used by Windows handle/file-lock scanning only.
 * Uses ntdll: NtQuerySystemInformation (handle list) and NtDuplicateObject (to resolve handle paths).
 * Do not extend; keep separate from other NtDll usages (e.g. TxF).
 */
package org.appwork.utils.os.windows.jna;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

public interface NtDllForHandleScan extends StdCallLibrary, WinNT {
    NtDllForHandleScan INSTANCE = Native.load("ntdll", NtDllForHandleScan.class, W32APIOptions.UNICODE_OPTIONS);

    /** SystemHandleInformation; buffer: ULONG HandleCount then entries. HandleValue is 16-bit in entries. */
    int SystemHandleInformation = 16;
    /** SystemExtendedHandleInformation (Vista+); full HandleValue (32/64-bit). Buffer: count, reserved, then entries. */
    int SystemExtendedHandleInformation = 0x40;

    int STATUS_SUCCESS = 0;
    int STATUS_INFO_LENGTH_MISMATCH = 0xC0000004;
    int STATUS_NO_MEMORY = 0xC0000017;
    int STATUS_ACCESS_DENIED = 0xC0000022;

    int DUPLICATE_SAME_ACCESS = 2;

    int NtQuerySystemInformation(int systemInformationClass, Pointer systemInformation, int systemInformationLength, IntByReference returnLength);

    int NtDuplicateObject(HANDLE sourceProcessHandle, HANDLE sourceHandle, HANDLE targetProcessHandle, WinNT.HANDLEByReference targetHandle, int desiredAccess, int handleAttributes, int options);
}
