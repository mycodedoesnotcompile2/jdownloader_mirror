package org.appwork.jna.tests;

import java.io.File;

import org.appwork.jna.windows.Ktmw32;
import org.appwork.jna.windows.NtDll;
import org.appwork.utils.IO;

import com.sun.jna.Memory;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WTypes.LPWSTR;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORDByReference;
import com.sun.jna.platform.win32.WinNT.HANDLE;

public class TxFTest {

    public static void main(String[] args) throws Exception {
        File file = new File("E:\\Test.txt");
        File file2 = new File("C:\\Users\\daniel\\Downloads\\Test.txt");
        // FileInputStream fis = new FileInputStream(file);
        final HANDLE handle = Ktmw32.INSTANCE.CreateTransaction(null, null, 0, 0, 0, 0, "Test" + System.currentTimeMillis());
        if (handle == null || WinBase.INVALID_HANDLE_VALUE.equals(handle)) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        final int memorySize = 100;
        LPWSTR Description = new LPWSTR(new Memory(memorySize));
        System.out.println(Ktmw32.INSTANCE.GetTransactionInformation(handle, new DWORDByReference(), new DWORDByReference(), new DWORDByReference(), new DWORDByReference(), memorySize, Description.getPointer()));
        System.out.println(Description.toString());
        try {
            HANDLE currentTransaction = NtDll.INSTANCE.RtlGetCurrentTransaction();
            try {
                System.out.println("get:" + currentTransaction);
                System.out.println("set:" + NtDll.INSTANCE.RtlSetCurrentTransaction(handle));
                System.out.println("exists:" + file.isFile());
                System.out.println("delete:" + file.delete());
                String text = "Sichtbar" + System.currentTimeMillis();
                System.out.println("Text:" + text);
                IO.writeStringToFile(file, text);
                IO.writeStringToFile(file2, text);
                System.out.println("Inhalt:" + IO.readFileToString(file));
                if (true) {
                    System.out.println("Commit:" + Ktmw32.INSTANCE.CommitTransaction(handle));
                } else {
                    System.out.println("Rollback:" + Ktmw32.INSTANCE.RollbackTransaction(handle));
                }
            } finally {
                System.out.println("Set:" + NtDll.INSTANCE.RtlSetCurrentTransaction(currentTransaction));
            }
        } finally {
            Kernel32.INSTANCE.CloseHandle(handle);
        }
        System.out.println("Inhalt:" + IO.readFileToString(file));
    }
}
