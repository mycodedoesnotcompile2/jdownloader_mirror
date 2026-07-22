package org.appwork.jna.windows;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.ptr.PointerByReference;

public interface WLANApi extends Library {
    public final static WLANApi INSTANCE = Native.load("wlanapi", WLANApi.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

    int WlanOpenHandle(int dwClientVersion, Pointer pReserved, PointerByReference pHandle);

    int WlanEnumInterfaces(Pointer hClientHandle, Pointer pReserved, PointerByReference ppInterfaceList);

    int WlanQueryInterface(Pointer hClientHandle, Guid pInterfaceGuid, int OpCode, Pointer pReserved, PointerByReference pDataSize, PointerByReference ppData);
}