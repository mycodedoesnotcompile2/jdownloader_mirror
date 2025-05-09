/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os;

import static com.sun.jna.platform.win32.WinNT.TOKEN_DUPLICATE;
import static com.sun.jna.platform.win32.WinNT.TOKEN_QUERY;
import static com.sun.jna.platform.win32.WinUser.SW_HIDE;
import static com.sun.jna.platform.win32.WinUser.SW_SHOW;

import java.util.HashSet;
import java.util.Set;

import org.appwork.utils.Exceptions;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Advapi32Util.Account;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.Shell32;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.INT_PTR;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.WinNT.TOKEN_ELEVATION;
import com.sun.jna.ptr.IntByReference;

/**
 * @author Thomas
 * @date 14.10.2018
 *
 */
public class WindowsUtils {
    // stay
    /**
     * @deprecated user WindowsUtils.SID enum instead we keep this field, because we would have to rebuild all Connect Service
     *             SelfTestPackages if we remove it.
     */
    public static final String SID_LOCAL_SYSTEM = "S-1-5-18";

    // public static final String SID_USER = "S-1-5-32-545";
    // public static final String SID_EVERYBODYY = "S-1-1-0";
    // public static final String SID_ADMINISTRATOR = "S-1-5-32-544";
    public static Account getCurrentUserAccount() {
        HANDLEByReference phToken = new HANDLEByReference();
        Win32Exception err = null;
        try {
            // open thread or process token
            HANDLE threadHandle = Kernel32.INSTANCE.GetCurrentThread();
            if (!Advapi32.INSTANCE.OpenThreadToken(threadHandle, TOKEN_DUPLICATE | TOKEN_QUERY, true, phToken)) {
                int rc = Kernel32.INSTANCE.GetLastError();
                if (rc != W32Errors.ERROR_NO_TOKEN) {
                    throw new Win32Exception(rc);
                }
                HANDLE processHandle = Kernel32.INSTANCE.GetCurrentProcess();
                if (!Advapi32.INSTANCE.OpenProcessToken(processHandle, TOKEN_DUPLICATE | TOKEN_QUERY, phToken)) {
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
            }
            Advapi32Util.Account userAcct = Advapi32Util.getTokenAccount(phToken.getValue());
            return userAcct;
        } catch (Win32Exception e) {
            err = e;
            throw err; // re-throw in order to invoke finally block
        } finally {
            HANDLE hToken = phToken.getValue();
            if (!WinBase.INVALID_HANDLE_VALUE.equals(hToken)) {
                try {
                    Kernel32Util.closeHandle(hToken);
                } catch (Win32Exception e) {
                    if (err == null) {
                        err = e;
                    } else {
                        Exceptions.addSuppressed(err, e);
                    }
                }
            }
            if (err != null) {
                throw err;
            }
        }
    }

    /**
     * Returns the current user SID. WARNING: Uses Reflection and com.sun packages.
     *
     * @return
     */
    public static String getCurrentUserSID() throws Exception {
        return getCurrentUserAccount().sidString;
    }

    /**
     * Tests if the current user is part of a SID group (e.g. {@link #SID_EVERYBODYY}) WARNING: Does not work for Local System. use
     * getCurrentUserSID() instead; WARNING: Uses Reflection and com.sun packages.
     *
     * @param sid
     * @return
     */
    public static boolean isCurrentUserPartOfGroup(String sid) {
        for (Account a : getCurrentUsersAccounts()) {
            if (a.sidString.equals(sid)) {
                return true;
            }
        }
        return false;
    }

    public static boolean isAdministrator() {
        return isCurrentUserPartOfGroup(SID.SID_BUILTIN_ADMINISTRATORS.sid);
    }

    public static boolean isElevated() {
        return Advapi32Util.isCurrentProcessElevated();
    }

    public static enum SID {
        SID_NULL("S-1-0-0"),
        SID_EVERYONE("S-1-1-0"),
        SID_LOCAL("S-1-2-0"),
        SID_CONSOLE_LOGON("S-1-2-1"),
        SID_CREATOR_OWNER("S-1-3-0"),
        SID_CREATOR_GROUP("S-1-3-1"),
        SID_OWNER_SERVER("S-1-3-2"),
        SID_GROUP_SERVER("S-1-3-3"),
        SID_OWNER_RIGHTS("S-1-3-4"),
        SID_NT_AUTHORITY("S-1-5"),
        SID_DIALUP("S-1-5-1"),
        SID_NETWORK("S-1-5-2"),
        SID_BATCH("S-1-5-3"),
        SID_INTERACTIVE("S-1-5-4"),
        SID_SERVICE("S-1-5-6"),
        SID_ANONYMOUS("S-1-5-7"),
        SID_PROXY("S-1-5-8"),
        SID_ENTERPRISE_DOMAIN_CONTROLLERS("S-1-5-9"),
        SID_PRINCIPAL_SELF("S-1-5-10"),
        SID_AUTHENTICATED_USERS("S-1-5-11"),
        SID_RESTRICTED_CODE("S-1-5-12"),
        SID_TERMINAL_SERVER_USER("S-1-5-13"),
        SID_REMOTE_INTERACTIVE_LOGON("S-1-5-14"),
        SID_THIS_ORGANIZATION("S-1-5-15"),
        SID_IUSR("S-1-5-17"),
        SID_LOCAL_SYSTEM("S-1-5-18"),
        SID_LOCAL_SERVICE("S-1-5-19"),
        SID_NETWORK_SERVICE("S-1-5-20"),
        SID_COMPOUNDED_AUTHENTICATION("S-1-5-21-0-0-0-496"),
        SID_CLAIMS_VALID("S-1-5-21-0-0-0-497"),
        SID_BUILTIN_ADMINISTRATORS("S-1-5-32-544"),
        SID_BUILTIN_USERS("S-1-5-32-545"),
        SID_BUILTIN_GUESTS("S-1-5-32-546"),
        SID_POWER_USERS("S-1-5-32-547"),
        SID_ACCOUNT_OPERATORS("S-1-5-32-548"),
        SID_SERVER_OPERATORS("S-1-5-32-549"),
        SID_PRINTER_OPERATORS("S-1-5-32-550"),
        SID_BACKUP_OPERATORS("S-1-5-32-551"),
        SID_REPLICATOR("S-1-5-32-552"),
        SID_ALIAS_PREW2KCOMPACC("S-1-5-32-554"),
        SID_REMOTE_DESKTOP("S-1-5-32-555"),
        SID_NETWORK_CONFIGURATION_OPS("S-1-5-32-556"),
        SID_INCOMING_FOREST_TRUST_BUILDERS("S-1-5-32-557"),
        SID_PERFMON_USERS("S-1-5-32-558"),
        SID_PERFLOG_USERS("S-1-5-32-559"),
        SID_WINDOWS_AUTHORIZATION_ACCESS_GROUP("S-1-5-32-560"),
        SID_TERMINAL_SERVER_LICENSE_SERVERS("S-1-5-32-561"),
        SID_DISTRIBUTED_COM_USERS("S-1-5-32-562"),
        SID_IIS_IUSRS("S-1-5-32-568"),
        SID_CRYPTOGRAPHIC_OPERATORS("S-1-5-32-569"),
        SID_EVENT_LOG_READERS("S-1-5-32-573"),
        SID_CERTIFICATE_SERVICE_DCOM_ACCESS("S-1-5-32-574"),
        SID_RDS_REMOTE_ACCESS_SERVERS("S-1-5-32-575"),
        SID_RDS_ENDPOINT_SERVERS("S-1-5-32-576"),
        SID_RDS_MANAGEMENT_SERVERS("S-1-5-32-577"),
        SID_HYPER_V_ADMINS("S-1-5-32-578"),
        SID_ACCESS_CONTROL_ASSISTANCE_OPS("S-1-5-32-579"),
        SID_REMOTE_MANAGEMENT_USERS("S-1-5-32-580"),
        SID_WRITE_RESTRICTED_CODE("S-1-5-33"),
        SID_NTLM_AUTHENTICATION("S-1-5-64-10"),
        SID_SCHANNEL_AUTHENTICATION("S-1-5-64-14"),
        SID_DIGEST_AUTHENTICATION("S-1-5-64-21"),
        SID_THIS_ORGANIZATION_CERTIFICATE("S-1-5-65-1"),
        SID_NT_SERVICE("S-1-5-80"),
        SID_USER_MODE_DRIVERS("S-1-5-84-0-0-0-0-0"),
        SID_LOCAL_ACCOUNT("S-1-5-113"),
        SID_LOCAL_ACCOUNT_AND_MEMBER_OF_ADMINISTRATORS_GROUP("S-1-5-114"),
        SID_OTHER_ORGANIZATION("S-1-5-1000"),
        SID_ALL_APP_PACKAGES("S-1-15-2-1"),
        SID_ML_UNTRUSTED("S-1-16-0"),
        SID_ML_LOW("S-1-16-4096"),
        SID_ML_MEDIUM("S-1-16-8192"),
        SID_ML_MEDIUM_PLUS("S-1-16-8448"),
        SID_ML_HIGH("S-1-16-12288"),
        SID_ML_SYSTEM("S-1-16-16384"),
        SID_ML_PROTECTED_PROCESS("S-1-16-20480"),
        SID_AUTHENTICATION_AUTHORITY_ASSERTED_IDENTITY("S-1-18-1"),
        SID_SERVICE_ASSERTED_IDENTITY("S-1-18-2"),
        SID_FRESH_PUBLIC_KEY_IDENTITY("S-1-18-3"),
        SID_KEY_TRUST_IDENTITY("S-1-18-4"),
        SID_KEY_PROPERTY_MFA("S-1-18-5"),
        SID_KEY_PROPERTY_ATTESTATION("S-1-18-6");
        public final String sid;

        private SID(String sid) {
            this.sid = sid;
        }
    }

    public static Set<Account> getCurrentUsersAccounts() {
        Set<Account> accounts = new HashSet<Account>();
        HANDLEByReference phToken = new HANDLEByReference();
        Win32Exception err = null;
        try {
            // open thread or process token
            HANDLE threadHandle = Kernel32.INSTANCE.GetCurrentThread();
            if (!Advapi32.INSTANCE.OpenThreadToken(threadHandle, TOKEN_DUPLICATE | TOKEN_QUERY, true, phToken)) {
                int rc = Kernel32.INSTANCE.GetLastError();
                if (rc != W32Errors.ERROR_NO_TOKEN) {
                    throw new Win32Exception(rc);
                }
                HANDLE processHandle = Kernel32.INSTANCE.GetCurrentProcess();
                if (!Advapi32.INSTANCE.OpenProcessToken(processHandle, TOKEN_DUPLICATE | TOKEN_QUERY, phToken)) {
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
            }
            Account[] tg = Advapi32Util.getTokenGroups(phToken.getValue());
            accounts.add(Advapi32Util.getTokenAccount(phToken.getValue()));
            for (Account acct : tg) {
                accounts.add(acct);
            }
            return accounts;
        } catch (Win32Exception e) {
            err = e;
            throw err; // re-throw in order to invoke finally block
        } finally {
            HANDLE hToken = phToken.getValue();
            if (!WinBase.INVALID_HANDLE_VALUE.equals(hToken)) {
                try {
                    Kernel32Util.closeHandle(hToken);
                } catch (Win32Exception e) {
                    if (err == null) {
                        err = e;
                    } else {
                        Exceptions.addSuppressed(err, e);
                    }
                }
            }
            if (err != null) {
                throw err;
            }
        }
    }

    /**
     * @return
     */
    public static Set<String> getMyPrincipalNames() {
        Set<String> myPrincipals = new HashSet<String>();
        for (Account a : getCurrentUsersAccounts()) {
            myPrincipals.add(a.domain + "\\" + a.name);
        }
        return myPrincipals;
    }

    /**
     * Starts a process with elevated privileges (UAC prompt will be shown)
     *
     * @param command
     *            The command to execute
     * @param workingDir
     *            The working directory (can be null)
     * @param showWindow
     *            Whether to show the window (true) or hide it (false)
     * @return The process handle if successful, null otherwise
     * @throws Win32Exception
     *             if the process cannot be started
     * @throws IllegalArgumentException
     *             if the command is invalid
     */
    public static INT_PTR startElevatedProcess(String[] command, String workingDir, boolean showWindow) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        if (command == null || command.length == 0) {
            throw new IllegalArgumentException("Command cannot be null or empty");
        }
        // Convert command to absolute path if it's a file
        // Build the final command
        String binary = command[0];
        String[] params = new String[command.length - 1];
        System.arraycopy(command, 1, params, 0, params.length);
        String finalCommand = ShellParser.createCommandLine(Style.WINDOWS, binary);
        String args = ShellParser.createCommandLine(Style.WINDOWS, params);
        System.out.println(finalCommand);
        // Set up ShellExecuteEx parameters
        Shell32.SHELLEXECUTEINFO sei = new Shell32.SHELLEXECUTEINFO();
        sei.cbSize = sei.size();
        sei.lpVerb = "runas"; // Request elevation
        sei.lpFile = finalCommand;
        sei.lpParameters = args;
        sei.lpDirectory = workingDir;
        sei.nShow = showWindow ? SW_SHOW : SW_HIDE;
        sei.fMask = Shell32.SEE_MASK_NOCLOSEPROCESS; // Get process handle
        // Execute the command
        if (!Shell32.INSTANCE.ShellExecuteEx(sei)) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        return new INT_PTR(Pointer.nativeValue(sei.hProcess.getPointer()));
    }

    /**
     * Gets the process ID from a process handle
     *
     * @param processHandle
     *            The handle of the process
     * @return The process ID
     * @throws Win32Exception
     *             if the operation fails
     */
    public static int getProcessId(INT_PTR processHandle) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        if (processHandle == null) {
            throw new IllegalArgumentException("Process handle cannot be null");
        }
        HANDLE handle = new HANDLE(Pointer.createConstant(processHandle.longValue()));
        return Kernel32.INSTANCE.GetProcessId(handle);
    }

    /**
     * Terminates a process using its handle
     *
     * @param processHandle
     *            The handle of the process to terminate
     * @param exitCode
     *            The exit code to set (typically 0 for normal termination)
     * @return true if the process was terminated successfully, false otherwise
     * @throws Win32Exception
     *             if the termination fails
     */
    public static boolean terminateProcess(INT_PTR processHandle, int exitCode) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        if (processHandle == null) {
            throw new IllegalArgumentException("Process handle cannot be null");
        }
        HANDLE handle = new HANDLE(Pointer.createConstant(processHandle.longValue()));
        // First try to get the process ID to verify the handle is valid
        int pid = getProcessId(processHandle);
        System.out.println("Terminating process with PID: " + pid);
        boolean result = Kernel32.INSTANCE.TerminateProcess(handle, exitCode);
        if (!result) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        return result;
    }

    /**
     * Checks if a process is running with elevated privileges
     *
     * @param pid
     *            The process ID to check
     * @return true if the process is running with elevated privileges, false otherwise
     * @throws Win32Exception
     *             if the operation fails
     */
    public static boolean isProcessElevated(int pid) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        HANDLEByReference phToken = new HANDLEByReference();
        try {
            // Open process token
            HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
            if (processHandle == null) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            try {
                // Get process token
                if (!Advapi32.INSTANCE.OpenProcessToken(processHandle, WinNT.TOKEN_QUERY, phToken)) {
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
                // Get token elevation information
                IntByReference pReturnLength = new IntByReference();
                TOKEN_ELEVATION elevation = new TOKEN_ELEVATION();
                if (!Advapi32.INSTANCE.GetTokenInformation(phToken.getValue(), WinNT.TOKEN_INFORMATION_CLASS.TokenElevation, elevation, elevation.size(), pReturnLength)) {
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
                return elevation.TokenIsElevated != 0;
            } finally {
                if (processHandle != null) {
                    Kernel32Util.closeHandle(processHandle);
                }
            }
        } finally {
            if (phToken.getValue() != null) {
                Kernel32Util.closeHandle(phToken.getValue());
            }
        }
    }
}
