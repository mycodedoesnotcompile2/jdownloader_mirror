package org.appwork.jna.windows;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.StringArray;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.win32.W32APIOptions;

public interface Rm extends Library {
    final static Rm INSTANCE = Native.load("rstrtmgr", Rm.class, W32APIOptions.UNICODE_OPTIONS);

    int RmGetList(int dwSessionHandle, IntByReference pnProcInfoNeeded, IntByReference pnProcInfo, RmProcessInfo[] rgAffectedApps, LongByReference lpdwRebootReasons);

    int RmStartSession(IntByReference pSessionHandle, int dwSessionFlags, char[] strSessionKey);

    int RmRegisterResources(int dwSessionHandle, int nFiles, StringArray rgsFilenames, int nApplications, Pointer rgApplications, int nServices, StringArray rgsServiceNames);

    int RmEndSession(int sessionHandle);
}
