package org.appwork.jna.windows.interfaces;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.win32.W32APIOptions;

public interface Advapi32Ext extends Advapi32 {
    public final static Advapi32Ext INSTANCE = Native.load("advapi32", Advapi32Ext.class, W32APIOptions.UNICODE_OPTIONS);
    // boolean ConvertStringSidToSid(String StringSid, WinNT.PSID Sid);
    // int SetNamedSecurityInfo(String pObjectName, int ObjectType, int SecurityInfo, Pointer pointer, WinNT.PSID psidGroup, WinNT.ACL
    // pDacl, WinNT.ACL pSacl);
}