package org.appwork.jna.windows.interfaces;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.win32.StdCallLibrary;
import org.appwork.jna.windows.JNAOptions;

public interface Ktmw32Ext extends StdCallLibrary, WinNT {
    public final static Ktmw32Ext INSTANCE = Native.load("ktmw32", Ktmw32Ext.class, JNAOptions.UNICODE_SYSTEM_DLLS_ONLY);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-createtransaction
    public HANDLE CreateTransaction(SECURITY_ATTRIBUTES lpSecurityAttribute, Pointer UOW, int CreateOptions, int IsolationLevel, int IsolationFlags, int Timeout, String description);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-committransaction
    public BOOL CommitTransaction(HANDLE TransactionHandle);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-rollbacktransaction
    public BOOL RollbackTransaction(HANDLE TransactionHandle);

    // https://docs.microsoft.com/en-us/windows/win32/api/ktmw32/nf-ktmw32-gettransactioninformation
    public BOOL GetTransactionInformation(HANDLE TransactionHandle, DWORDByReference Outcome, DWORDByReference IsolationLevel, DWORDByReference IsolationFlags, DWORDByReference Timeout, int BufferLength, Pointer Description);
}
