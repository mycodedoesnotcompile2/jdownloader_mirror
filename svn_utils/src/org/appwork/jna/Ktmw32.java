package org.appwork.jna;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

public interface Ktmw32 extends StdCallLibrary, WinNT {
    public final static Ktmw32 INSTANCE = Native.load("ktmw32", Ktmw32.class, W32APIOptions.UNICODE_OPTIONS);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-createtransaction
    public HANDLE CreateTransaction(SECURITY_ATTRIBUTES lpSecurityAttribute, Pointer UOW, int CreateOptions, int IsolationLevel, int IsolationFlags, int Timeout, String description);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-committransaction
    public BOOL CommitTransaction(HANDLE TransactionHandle);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-rollbacktransaction
    public BOOL RollbackTransaction(HANDLE TransactionHandle);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-gettransactioninformation
    public BOOL GetTransactionInformation(HANDLE TransactionHandle, DWORDByReference Outcome, DWORDByReference IsolationLevel, DWORDByReference IsolationFlags, DWORDByReference Timeout, int BufferLength, Pointer Description);
}
