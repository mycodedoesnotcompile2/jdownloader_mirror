/**
 * 64-bit layout of SYSTEM_HANDLE_TABLE_ENTRY_INFO_EX for
 * NtQuerySystemInformation(SystemExtendedHandleInformation) used by handle scan.
 * Total 0x28 (40) bytes. Full pointer-sized HandleValue.
 */
package org.appwork.utils.os.windows.jna;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class HandleScanExEntry64 extends Structure {
    /** Pointer to kernel object (8 bytes on 64-bit). */
    public Pointer Object;
    /** ULONG_PTR: Process ID (8 bytes on 64-bit, pointer-sized). */
    public long UniqueProcessId;
    /** ULONG_PTR: Handle value (8 bytes on 64-bit, pointer-sized). */
    public long HandleValue;
    /** ULONG: Access mask (4 bytes). */
    public int GrantedAccess;
    /** USHORT (2 bytes). */
    public short CreatorBackTraceIndex;
    /** USHORT: Object type index (2 bytes). */
    public short ObjectTypeIndex;
    /** ULONG (4 bytes). */
    public int HandleAttributes;
    /** ULONG (4 bytes). */
    public int Reserved;

    public HandleScanExEntry64() {
        super();
    }

    public HandleScanExEntry64(final Pointer p) {
        super(p);
    }

    @Override
    protected List<String> getFieldOrder() {
        return Arrays.asList("Object", "UniqueProcessId", "HandleValue", "GrantedAccess", "CreatorBackTraceIndex", "ObjectTypeIndex", "HandleAttributes", "Reserved");
    }

    public int getProcessId() {
        return (int) UniqueProcessId;
    }

    public int getHandleValue() {
        return (int) HandleValue;
    }

    public int getObjectTypeNumber() {
        return ObjectTypeIndex & 0xFFFF;
    }
}
