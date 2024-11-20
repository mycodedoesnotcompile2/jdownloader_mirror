package org.appwork.jna.windows;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

public interface WLANApi extends Library {
    public final static WLANApi INSTANCE = Native.load("wlanapi", WLANApi.class, W32APIOptions.DEFAULT_OPTIONS);

    int WlanOpenHandle(int dwClientVersion, Pointer pReserved, PointerByReference pHandle);

    int WlanEnumInterfaces(Pointer hClientHandle, Pointer pReserved, PointerByReference ppInterfaceList);

    int WlanQueryInterface(Pointer hClientHandle, Guid pInterfaceGuid, int OpCode, Pointer pReserved, PointerByReference pDataSize, PointerByReference ppData);
}