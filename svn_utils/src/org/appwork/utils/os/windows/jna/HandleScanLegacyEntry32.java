/**
 * 32-bit layout of SYSTEM_HANDLE_TABLE_ENTRY_INFO for
 * NtQuerySystemInformation(SystemHandleInformation) used by handle scan (legacy fallback).
 * Total 0x10 (16) bytes. HandleValue is 16-bit; path resolution may fail on 32-bit JVM.
 */
package org.appwork.utils.os.windows.jna;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class HandleScanLegacyEntry32 extends Structure {
    public int UniqueProcessId;
    public Pointer Object;
    public short HandleValue;
    public byte ObjectTypeIndex;
    public byte HandleAttributes;
    public int GrantedAccess;

    public HandleScanLegacyEntry32() {
        super();
    }

    public HandleScanLegacyEntry32(final Pointer p) {
        super(p);
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("UniqueProcessId", "Object", "HandleValue", "ObjectTypeIndex", "HandleAttributes", "GrantedAccess");
    }

    public int getProcessId() {
        return UniqueProcessId;
    }

    public int getHandleValue() {
        return HandleValue & 0xFFFF;
    }

    public int getObjectTypeNumber() {
        return ObjectTypeIndex & 0xFF;
    }
}
