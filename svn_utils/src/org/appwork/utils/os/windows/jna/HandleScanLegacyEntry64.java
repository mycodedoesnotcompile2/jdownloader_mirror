/**
 * 64-bit layout of SYSTEM_HANDLE_TABLE_ENTRY_INFO for
 * NtQuerySystemInformation(SystemHandleInformation) used by handle scan (legacy fallback).
 * Structure 0x18 (24) bytes. HandleValue is 16-bit in structure.
 */
package org.appwork.utils.os.windows.jna;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class HandleScanLegacyEntry64 extends Structure {
    public short UniqueProcessId;
    public short CreatorBackTraceIndex;
    public byte ObjectTypeIndex;
    public byte HandleAttributes;
    public short HandleValue;
    public Pointer Object;
    public int GrantedAccess;
    public byte[] padding = new byte[4];

    public HandleScanLegacyEntry64() {
        super();
    }

    public HandleScanLegacyEntry64(final Pointer p) {
        super(p);
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("UniqueProcessId", "CreatorBackTraceIndex", "ObjectTypeIndex", "HandleAttributes", "HandleValue", "Object", "GrantedAccess", "padding");
    }

    public int getProcessId() {
        return UniqueProcessId & 0xFFFF;
    }

    public int getHandleValue() {
        return HandleValue & 0xFFFF;
    }

    public int getObjectTypeNumber() {
        return ObjectTypeIndex & 0xFF;
    }
}
