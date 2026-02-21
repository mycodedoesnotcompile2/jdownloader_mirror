/**
 * 32-bit layout of SYSTEM_HANDLE_TABLE_ENTRY_INFO_EX for
 * NtQuerySystemInformation(SystemExtendedHandleInformation) used by handle scan.
 * Total 0x1C (28) bytes. Full 32-bit HandleValue (unlike legacy 16-bit).
 */
package org.appwork.utils.os.windows.jna;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class HandleScanExEntry32 extends Structure {
    public Pointer Object;
    public int UniqueProcessId;
    public int HandleValue;
    public int GrantedAccess;
    public short CreatorBackTraceIndex;
    public short ObjectTypeIndex;
    public int HandleAttributes;
    public int Reserved;

    public HandleScanExEntry32() {
        super();
    }

    public HandleScanExEntry32(final Pointer p) {
        super(p);
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("Object", "UniqueProcessId", "HandleValue", "GrantedAccess", "CreatorBackTraceIndex", "ObjectTypeIndex", "HandleAttributes", "Reserved");
    }

    public int getProcessId() {
        return UniqueProcessId;
    }

    public int getHandleValue() {
        return HandleValue;
    }

    public int getObjectTypeNumber() {
        return ObjectTypeIndex & 0xFFFF;
    }
}
